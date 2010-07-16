/*
 *  SearchResultFrame.scala
 *  (ScalaFreesound-Swing)
 *
 *  Copyright (c) 2010 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 *
 *
 *  Changelog:
 */

package de.sciss.freesound.swing

import javax.swing._
import event.TableModelEvent
import java.awt.event.{ActionEvent, ActionListener}
import de.sciss.freesound.swing.SearchQueryFrame.NewSearch
import collection.breakOut
import collection.immutable.{ IndexedSeq => IIdxSeq, Queue => IQueue, Set => ISet }
import de.sciss.freesound._
import Search._
import actors.{TIMEOUT, DaemonActor, Actor}
import table._
import java.text.SimpleDateFormat
import java.util.{Locale, Date, Comparator}
import java.awt.{Component, Graphics, BorderLayout, EventQueue}

object SearchResultFrame {
   var maxConcurrentInfoQueries  = 20

   private case class Column( idx: Int, name: String, minWidth: Int, maxWidth: Int, extract: SampleInfo => Any,
                              renderer: Option[ TableCellRenderer ], sorter: Option[ Comparator[ _ ]])

   private object ColumnEnum {
      private var allVar = Vector.empty[ Column ]
      lazy val COLUMNS = allVar.toArray 

      private def column( name: String, minWidth: Int, maxWidth: Int, extract: SampleInfo => Any,
                          renderer: Option[ TableCellRenderer ] = None,
                          sorter: Option[ Comparator[ _ ]] = None ) : Column = {
         val c = Column( allVar.size, name, minWidth, maxWidth, extract, renderer, sorter )
         allVar :+= c
         c
      }

      val COL_ID     = column( "ID",   56, 56, _.id, Some( RightAlignedRenderer ), Some( LongComparator ))
      val COL_NAME   = column( "Name", 96, 192, _.fileName )
      val COL_FORM   = column( "Form", 36, 36, _.extension )
      val COL_CHAN   = column( "Ch", 24, 24, _.numChannels, Some( RightAlignedRenderer ), Some( IntComparator ))
      val COL_BITS   = column( "Bit", 24, 24, _.bitDepth, Some( RightAlignedRenderer ), Some( IntComparator ))
      val COL_SR     = column( "kHz",  48, 48, _.sampleRate / 1000, Some( RightAlignedRenderer ), Some( DoubleComparator ))
      val COL_DUR    = column( "Duration", 46, 46, _.duration, Some( DurationRenderer ), Some( DoubleComparator ))
      val COL_DESCR  = column( "Description", 96, 384, _.descriptions.headOption.map( _.text ).getOrElse( "" ))
      val COL_USER   = column( "User", 56, 84, _.user.name )
      val COL_DATE   = column( "Date", 78, 78, _.date, Some( DateRenderer ), Some( DateComparator ))
      val COL_RATING = column( "\u2605", 29, 29, _.statistics.rating, Some( RatingRenderer ), Some( IntComparator ))
   }
   val NUM_COLUMNS   = ColumnEnum.COLUMNS.size

   private case class IGetInfo( smp: Sample )
   private case class IInfoDone( smp: Sample )

   private object IntComparator extends Comparator[ Int ] {
      def compare( a: Int, b: Int ) = a.compare( b )
   }
   private object LongComparator extends Comparator[ Long ] {
      def compare( a: Long, b: Long ) = a.compare( b )
   }
   private object DoubleComparator extends Comparator[ Double ] {
      def compare( a: Double, b: Double ) = a.compare( b )
   }
   private object DateComparator extends Comparator[ Date ] {
      def compare( a: Date, b: Date ) = a.compareTo( b )
   }
   private object RightAlignedRenderer extends DefaultTableCellRenderer {
      setHorizontalAlignment( SwingConstants.TRAILING )
   }
   private object DateRenderer extends DefaultTableCellRenderer {
      private val df = new SimpleDateFormat( "yy-MMM-dd", Locale.US )
      override def setValue( value: AnyRef ) {
         super.setValue( if( value == null ) null else df.format( value ))
      }
   }
   private object DurationRenderer extends DefaultTableCellRenderer {
      override def setValue( value: AnyRef ) {
         super.setValue( if( value == null ) null else value match {
            case d: java.lang.Double => {
               val secsP   = (d.doubleValue() + 0.5).toInt
               val mins    = secsP / 60
               val secs    = secsP % 60
               (mins + 100).toString.substring( 1 ) + ":" +
               (secs + 100).toString.substring( 1 ) 
            }
            case _ => value
         })
      }
   }
   private object RatingRenderer extends DefaultTableCellRenderer with Icon {
      setIcon( this )
      private var rating   = 0
//      private var selected = false
      override def setValue( value: AnyRef ) {
         rating = if( value == null ) 0 else value match {
            case i: Integer => i.intValue()
            case _ => 0
         }
      }
//      override def getTableCellRendererComponent( table: JTable, value: AnyRef, isSelected: Boolean,
//                                                  hasFocus: Boolean, rowIdx: Int, colIdx: Int ) : Component = {
//         selected = isSelected
//         super.getTableCellRendererComponent( table, value, isSelected, hasFocus, rowIdx, colIdx )
//      }
      def getIconHeight = 16
      def getIconWidth  = 21
      def paintIcon( c: Component, g: Graphics, x: Int, y: Int ) {
         g.setColor( getForeground() )
         var xi = x + 1
         val xn = x + rating * 2
         while( xi < xn ) {
            g.drawLine( xi, y, xi, y + 15 )
            xi += 2
         }
      }
   }
}

class SearchResultFrame( queryFrame: SearchQueryFrame, search: Search, title: String, sizeVariant: String = "small" )
extends JFrame( title )
with Model {
   import SearchResultFrame._
   import ColumnEnum._

   private val frameListener: Model.Listener = { case LoginFrame.LoggedOut => loggedOut }
   private val ggTable                       = new JTable()
   private var mapToRowIndices               = Map.empty[ Sample, Int ]
   private var tableModel : SampleTableModel = _
   private var samples                       = Array.empty[ Sample ]
   private var infoQuerySet                  = ISet.empty[ Sample ]
   private val searchListener: Model.Listener= {
      case SearchDone( samples ) => defer( searchDone( samples ))
   }
   private def sampleListener( smp: Sample ) : Model.Listener = {
      case r: Sample.InfoResult => defer( infoDone( smp ))
   }

   private lazy val infoQueryActor = {
      val res = new DaemonActor {
         myself =>
         def act {
            var queue = ISet.empty[ Sample ]
            loop {
               loopWhile( queue.size >= maxConcurrentInfoQueries ) {
                  react {
                     case IInfoDone( smp ) => queue -= smp
                  }
               } andThen {
                  reactWithin( if( infoQuerySet.isEmpty ) 0x3FFFFFFFFFFFFFFFL else 1L ) {
                     case IGetInfo( smp ) => {
                        if( smp.infoResult.isEmpty ) {
                           queue += smp
                           smp.addListener( sampleListener( smp ))
                           smp.performInfo
                        } else {
                           defer( infoDone( smp ))
                        }
                     }
                     case TIMEOUT => defer( queueNextInfo )
                  }
               }
            }
         }
      }
      res.start
      res
   }
   
   // ---- constructor ----
   {
      val panel = getContentPane()
      ggTable.putClientProperty( "JComponent.sizeVariant", sizeVariant )
//      ggTable.setAutoCreateRowSorter( true )
      val ggScroll = new JScrollPane( ggTable,
         ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS )
      panel.add( ggScroll, BorderLayout.CENTER )
      queryFrame.addListener( frameListener )
      search.addListener( searchListener )
//      pack()
      setSize( 400, 400 )
   }

   private def loggedOut {
//      dispose
//      dispatch( LoggedOut )
   }

   private def searchDone( smps: IIdxSeq[ Sample ]) {
      mapToRowIndices   = smps.zipWithIndex[ Sample, Map[ Sample, Int ]]( breakOut )
      samples           = smps.toArray
      infoQuerySet      = smps.toSet
      tableModel        = new SampleTableModel( samples )
      val rowSorter     = new TableRowSorter( tableModel )
      ggTable.setModel( tableModel )
      val colModel      = ggTable.getColumnModel()
      COLUMNS foreach { case col =>
         col.sorter.foreach( rowSorter.setComparator( col.idx, _ ))
         val tc = colModel.getColumn( col.idx )
         tc.setMinWidth( col.minWidth )
         tc.setMaxWidth( col.maxWidth )
//         tc.setPreferredWidth( col.width )
         col.renderer.foreach( tc.setCellRenderer( _ ))
      }
      colModel.setColumnMargin( 6 )
      ggTable.setRowSorter( rowSorter )
   }

   private def infoDone( smp: Sample ) {
//      smp.removeListener( sampleListener )
      infoQueryActor ! IInfoDone( smp )
      val rowIdx = mapToRowIndices( smp )
      tableModel.fireTableCellUpdated( rowIdx, TableModelEvent.ALL_COLUMNS )
   }

   private def defer( thunk: => Unit ) {
      EventQueue.invokeLater( new Runnable { def run = thunk })
   }

   private class SampleTableModel( samples: Array[ Sample ]) extends AbstractTableModel {
      def getRowCount = samples.size
      def getColumnCount = NUM_COLUMNS
      override def getColumnName( colIdx: Int ) = COLUMNS( colIdx ).name

      def getValueAt( rowIdx: Int, colIdx: Int ) : AnyRef = {
         val smp = samples( rowIdx )
         if( colIdx == COL_ID.idx ) return smp.id.asInstanceOf[ AnyRef ]
         smp.info.map( COLUMNS( colIdx ).extract( _ ).asInstanceOf[ AnyRef ]).getOrElse({
            addInfoQuery( smp )
            null
         })
      }
   }

   private def addInfoQuery( smp: Sample ) {
      if( !infoQuerySet.contains( smp )) return
      infoQuerySet   -= smp
      infoQueryActor ! IGetInfo( smp )
   }

   private def queueNextInfo {
      samples.find( smp => infoQuerySet.contains( smp )).foreach( addInfoQuery( _ ))
   }
}