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
import java.awt.{BorderLayout, EventQueue}
import actors.{TIMEOUT, DaemonActor, Actor}
import table.{TableRowSorter, AbstractTableModel, TableModel}
import java.util.{Date, Comparator}

object SearchResultFrame {
   var maxConcurrentInfoQueries  = 20

   private case class Column( idx: Int, name: String, width: Int, extract: SampleInfo => Any, sorter: Option[ Comparator[ _ ]])

   private object ColumnEnum {
      private var allVar = Vector.empty[ Column ]
      lazy val COLUMNS = allVar.toArray 

      private def column( name: String, width: Int, fun: SampleInfo => Any,
                          sorter: Option[ Comparator[ _ ]] = None ) : Column = {
         val c = Column( allVar.size, name, width, fun, sorter )
         allVar :+= c
         c
      }

      val COL_ID     = column( "ID",   48, _.id, Some( LongComparator ))
      val COL_NAME   = column( "Name", 96, _.fileName )
      val COL_FORM   = column( "Form", 32, _.extension )
      val COL_CHAN   = column( "Chan", 16, _.numChannels, Some( IntComparator ))
      val COL_BITS   = column( "Bits", 24, _.bitDepth, Some( IntComparator ))
      val COL_SR     = column( "kHz",  32, _.sampleRate / 1000, Some( DoubleComparator ))
      val COL_DUR    = column( "Duration", 48, _.duration, Some( DoubleComparator ))
      val COL_DESCR  = column( "Description", 96, _.descriptions.headOption.map( _.text ).getOrElse( "" ))
      val COL_USER   = column( "User", 32, _.user.name )
      val COL_DATE   = column( "Date", 48, _.date )
      val COL_RATING = column( "Rating", 16, _.statistics.rating, Some( IntComparator ))
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
         col.sorter foreach { case sort => rowSorter.setComparator( col.idx, sort )}
         colModel.getColumn( col.idx ).setPreferredWidth( col.width )
      }
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
         smp.info.map( COLUMNS( colIdx ).extract( _ ).asInstanceOf[ AnyRef ])
//         map { info =>
//            colIdx match {
//               case COL_NAME    => info.fileName
//               case COL_FORM    => info.extension
//               case COL_CHAN    => info.numChannels.asInstanceOf[ AnyRef ]
//               case COL_BITS    => info.bitDepth.asInstanceOf[ AnyRef ]
//               case COL_SR      => (info.sampleRate / 1000).asInstanceOf[ AnyRef ]
//               case COL_DUR     => info.duration.asInstanceOf[ AnyRef ]
//               case COL_DESCR   => info.descriptions.headOption.map( _.text ).getOrElse( "" )
//               case COL_USER    => info.user.name
//               case COL_DATE    => info.date
//               case COL_RATING  => info.statistics.rating.asInstanceOf[ AnyRef ]
//            }
         .getOrElse({
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