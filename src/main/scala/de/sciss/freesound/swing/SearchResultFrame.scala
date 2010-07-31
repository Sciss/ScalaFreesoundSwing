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
import java.awt.event._
import de.sciss.freesound.swing.SearchQueryFrame.NewSearch
import collection.breakOut
import collection.immutable.{ IndexedSeq => IIdxSeq, Queue => IQueue, Set => ISet }
import de.sciss.freesound._
import Search._
import actors.{TIMEOUT, DaemonActor, Actor}
import table._
import java.text.SimpleDateFormat
import java.util.{Locale, Date, Comparator}
import java.awt._
import java.io.File
import javax.swing.event.{ListSelectionListener, ListSelectionEvent, TableModelEvent}

/**
 *    @version 0.11, 17-Jul-10
 */
object SearchResultFrame {
   var maxConcurrentInfoQueries  = 20

   case class SelectionChanged( selection: Sample* )

   private case class Column( idx: Int, name: String, minWidth: Int, maxWidth: Int, extract: SampleRepr => Any,
                              renderer: Option[ TableCellRenderer ], sorter: Option[ Comparator[ _ ]])
   private val UnknownColumn = Column( -1, null, 0, 0, null, None, None )

   private case class IGetInfo( repr: SampleRepr )
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
   private object DownloadComparator extends Comparator[ AnyRef ] {
      import Sample._

      def compare( a: AnyRef, b: AnyRef ) = {
         rank( a ).compare( rank( b ))
      }

      private def rank( d: AnyRef ) = d match {
         case DownloadBegin         => 1
         case DownloadDone( _ )     => 102
         case _: DownloadFailed     => 101
         case DownloadProgress( p ) => p
         case _                     => 0
      }
   }

//   private object IDRenderer extends DefaultTableCellRenderer {
//      setHorizontalAlignment( SwingConstants.TRAILING )
//      private val colrCached = Color.green
//      override def getTableCellRendererComponent( table: JTable, value: AnyRef, isSelected: Boolean,
//                                                  hasFocus: Boolean, rowIdx: Int, colIdx: Int ) : Component = {
//         val res = super.getTableCellRendererComponent( table, value, isSelected, hasFocus, rowIdx, colIdx )
//         value match {
//            case SampleRepr( _, cached ) if( cached ) => {
//               if( isSelected ) res.setForeground( colrCached ) else res.setBackground( colrCached )
//            }
//            case _ =>
//         }
//         res
//      }
//   }
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
   private object DownloadRenderer extends DefaultTableCellRenderer with Icon {
      import Sample._

      private var barWidth = 0
      private val colrDone = new Color( 0x00, 0xC0, 0x00 )
      private val colrProg = new Color( 0xFF, 0xC0, 0x00 )
      private val colrFail = new Color( 0xFF, 0x00, 0x00 )
      private var color    = Color.white

      setIcon( this )

      override def setValue( value: AnyRef ) {
         val (w, c) = value match {
            case DownloadDone( _ )     => (57, colrDone)
            case _: DownloadFailed     => (57, colrFail)
            case DownloadProgress( p ) => (p * 57/100, colrProg)
            case _                     => (0, Color.white)
         }
         barWidth = w
         color    = c
      }
      def getIconHeight = 16
      def getIconWidth  = 60
      def paintIcon( c: Component, g: Graphics, x: Int, y: Int ) {
         g.setColor( color )
         g.fillRect( x, y + 2, barWidth, 13 )
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
      def getIconHeight = 16
      def getIconWidth  = 21
      def paintIcon( c: Component, g: Graphics, x: Int, y: Int ) {
         g.setColor( getForeground() )
         var xi = x + 1
         val y0 = y + 2
         val y1 = y + 14
         val xn = x + rating * 2
         while( xi < xn ) {
            g.drawLine( xi, y0, xi, y1 )
            xi += 2
         }
      }
   }

   private case class SampleRepr( sample: Sample, download: AnyRef )
}

class SearchResultFrame( samples: IIdxSeq[ Sample ], login: Option[ Login ], title: String,
                         icache: Option[ SampleInfoCache ], downloadPath : Option[ String ],
                         sizeVariant: String = "small" )
extends JFrame( title )
with Model {
   import SearchResultFrame._

   private object ColumnEnum {
      private var allVar = Vector.empty[ Column ]
      lazy val COLUMNS = allVar.toArray

      private def column( name: String, minWidth: Int, maxWidth: Int, extract: SampleInfo => Any,
                          renderer: Option[ TableCellRenderer ] = None,
                          sorter: Option[ Comparator[ _ ]] = None ) : Column = {
         val c = Column( allVar.size, name, minWidth, maxWidth, r => extract( r.sample.info.get ), renderer, sorter )
         allVar :+= c
         c
      }

      private def columnR( name: String, minWidth: Int, maxWidth: Int, extract: SampleRepr => Any,
                          renderer: Option[ TableCellRenderer ] = None,
                          sorter: Option[ Comparator[ _ ]] = None ) : Column = {
         val c = Column( allVar.size, name, minWidth, maxWidth, extract, renderer, sorter )
         allVar :+= c
         c
      }

      val COL_ID     = column( "ID",   56, 56, _.id, Some( RightAlignedRenderer ), Some( LongComparator ))
      val COL_DOWN   = if( downloadPath.isDefined ) {
         columnR( "Download", 64, 64, _.download, Some( DownloadRenderer ), Some( DownloadComparator ))
      } else UnknownColumn
      
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
   import ColumnEnum._

   private val ggTable                       = new JTable( SampleTableModel )
   private val mapToRowIndices: Map[ Sample, Int ] = samples.zipWithIndex[ Sample, Map[ Sample, Int ]]( breakOut )
//   private var tableModel : SampleTableModel = _
   private val reprs: Array[ SampleRepr ] = samples.map( SampleRepr( _, None ))( breakOut )

   private var infoQuerySet: ISet[ Sample ] = samples.toSet

   private def sampleInfoListener( repr: SampleRepr ) : Model.Listener = {
      case f: Sample.InfoFailed => {
         defer( infoDone( repr ))
      }
      case Sample.InfoDone( i ) => {
         icache.foreach( c =>
            if( !c.contains( repr.sample.id )) try {
               c.add( i )
            } catch { case e => e.printStackTrace() }
         )
         val repr2 = if( repr.download == None ) {
            downloadPath map { path =>
               val f = new File( path, i.fileName )
//println( ": " + f + ".isFile = " + f.isFile )
               if( !f.isFile ) repr else {
                  val path = f.getCanonicalPath()
                  repr.sample.download = Some( path )
                  repr.copy( download = Sample.DownloadDone( path )) 
               }
            } getOrElse repr
         } else repr
         defer( infoDone( repr2 ))
      }
   }

   private def sampleDownloadListener( repr: SampleRepr ) = new PartialFunction[ AnyRef, Unit ] {
      listener =>
      val fun: Model.Listener = {
         case p: Sample.DownloadProgress => {
            val newRepr = repr.copy( download = p )
            defer( downloadUpdate( newRepr ))
         }
         case Sample.DownloadBegin =>
         case r: Sample.DownloadResult => {
            val r2 = r match {
               case Sample.DownloadDone( path ) => {
                  repr.sample.info.map( i => {
                     val f1 = new File( path )
                     val f2 = new File( f1.getParentFile(), i.fileName )
                     if( f1.renameTo( f2 )) {
                        val path = f2.getCanonicalPath()
                        repr.sample.removeListener( listener ) // avoid loop!
                        repr.sample.download = Some( path )
                        Sample.DownloadDone( path )
                     } else r
                  }).getOrElse( r )
               }
               case _ => r
            }
            val newRepr = repr.copy( download = r2 )
            defer( downloadUpdate( newRepr ))
         }
      }
      def isDefinedAt( x: AnyRef ) = fun.isDefinedAt( x )
      def apply( x: AnyRef ) = fun.apply( x )
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
                     case IGetInfo( repr ) => {
                        val smp     = repr.sample
//                        val repr2   = if( repr.download.isEmpty ) {
//                           downloadPath map { path =>
//
//                           } getOrElse repr
//                        } else repr
                        if( smp.infoResult.isEmpty ) {
                           queue += smp
                           smp.addListener( sampleInfoListener( repr ))
                           icache.flatMap( _.get( smp.id )).map( i => smp.info = Some( i ))
                              .getOrElse( login.foreach( smp.performInfo( _ )))
                        } else {
//                           defer( infoDone( SampleRepr( smp, icache.map( _.contains( smp.id )).getOrElse( false ))))
                           defer( infoDone( repr ))
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
      val rowSorter     = new TableRowSorter( SampleTableModel )
      val colModel      = ggTable.getColumnModel()
      COLUMNS foreach { case col =>
         col.sorter.foreach( rowSorter.setComparator( col.idx, _ ))
         val tc = colModel.getColumn( col.idx )
         tc.setMinWidth( col.minWidth )
         tc.setMaxWidth( col.maxWidth )
         col.renderer.foreach( tc.setCellRenderer( _ ))
      }
      colModel.setColumnMargin( 6 )
      ggTable.setRowSorter( rowSorter )
      downloadPath.foreach( path => {
         ggTable.addMouseListener( new MouseAdapter {
            override def mouseClicked( e: MouseEvent ) {
               if( e.getClickCount() != 2 ) return
               val rowIdx = ggTable.convertRowIndexToModel( ggTable.getSelectedRow() )
               val colIdx = ggTable.convertColumnIndexToModel( ggTable.getSelectedColumn() )
               if( rowIdx >= 0 && colIdx == COL_DOWN.idx ) {
                  val repr = reprs( rowIdx )
                  if( repr.sample.downloadResult.isEmpty ) {
                     download( path, repr )
                  }
               }
            }
         })
      })
      ggTable.getSelectionModel().addListSelectionListener( new ListSelectionListener {
         def valueChanged( e: ListSelectionEvent ) {
            val smps = ggTable.getSelectedRows().map( idx =>
               reprs( ggTable.convertRowIndexToModel( idx )).sample )
            dispatch( SelectionChanged( smps: _* ))
         }
      })
      val ggScroll = new JScrollPane( ggTable,
         ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS )
      panel.add( ggScroll, BorderLayout.CENTER )
//      queryFrame.addListener( frameListener )
//      search.addListener( searchListener )
//      pack()
      setSize( 640, 480 )
   }

   private def download( folder: String, repr: SampleRepr ) {
      login.foreach( l => {
         val smp = repr.sample
         smp.addListener( sampleDownloadListener( repr ))
         val tmpFile = {
            var i    = smp.id
            val ext  = smp.info.get.extension
            var f: File = null
            do {
               f = new File( folder, "__tmp" + i + "." + ext )
               i += 1
            } while( f.exists() )
            f.createNewFile()
            f.deleteOnExit()
            f.getCanonicalPath()
         }
         smp.performDownload( tmpFile )( l )
      })
   }

   private def infoDone( newRepr: SampleRepr ) {
//      smp.removeListener( sampleListener )
      infoQueryActor ! IInfoDone( newRepr.sample )
      val rowIdx = mapToRowIndices( newRepr.sample )
      reprs( rowIdx ) = newRepr
      SampleTableModel.fireTableCellUpdated( rowIdx, TableModelEvent.ALL_COLUMNS )
   }

   private def downloadUpdate( newRepr: SampleRepr ) {
//println( "DOWN = " + newRepr.download )
      val rowIdx = mapToRowIndices( newRepr.sample )
      reprs( rowIdx ) = newRepr
      SampleTableModel.fireTableCellUpdated( rowIdx, COL_DOWN.idx )
   }

   private def defer( thunk: => Unit ) {
      EventQueue.invokeLater( new Runnable { def run = thunk })
   }

   private object SampleTableModel extends AbstractTableModel {
      def getRowCount = reprs.size
      def getColumnCount = NUM_COLUMNS
      override def getColumnName( colIdx: Int ) = COLUMNS( colIdx ).name

      def getValueAt( rowIdx: Int, colIdx: Int ) : AnyRef = {
         val repr = reprs( rowIdx )
         val smp  = repr.sample
         if( colIdx == COL_ID.idx ) return smp.id.asInstanceOf[ AnyRef ]
         if( smp.info.isDefined ) {
            COLUMNS( colIdx ).extract( repr ).asInstanceOf[ AnyRef ]
         } else {
            addInfoQuery( repr )
            null
         }
      }
   }

   private def addInfoQuery( repr: SampleRepr ) {
      if( !infoQuerySet.contains( repr.sample )) return
      infoQuerySet   -= repr.sample
      infoQueryActor ! IGetInfo( repr )
   }

   private def queueNextInfo {
      reprs.find( repr => infoQuerySet.contains( repr.sample )).foreach( addInfoQuery( _ ))
   }
}