/*
 *  SearchProgressFrame.scala
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
class SearchProgressFrame( queryFrame: SearchQueryFrame, search: Search, title: String,
                           sizeVariant: String = "small" )
extends JFrame( title )
with Model {
   private val frameListener: Model.Listener = { case LoginFrame.LoggedOut => loggedOut }
   private val searchListener: Model.Listener= {
      case SearchDone( samples ) => defer( searchDone( samples ))
      case f: SearchFailed => defer( searchFailed( f ))
   }
   private val lbStatus = new JLabel( "<html><body><b>Searching...</b></body></html>" )
   private val ggProgress = new JProgressBar()

   // ---- constructor ----
   {
      val panel = getContentPane()
      panel.setLayout( new BoxLayout( panel, BoxLayout.Y_AXIS ))
      val p2 = new JPanel( new FlowLayout() )
      lbStatus.putClientProperty( "JComponent.sizeVariant", sizeVariant )
      p2.add( lbStatus )
      ggProgress.setIndeterminate( true )
      val dimProgress   = new Dimension( 24, 24 )
      ggProgress.setPreferredSize( dimProgress )
      ggProgress.putClientProperty( "JProgressBar.style", "circular" )
      p2.add( ggProgress )
      panel.add( p2 )
      val lbSearch = new JLabel( search.toString )
      lbSearch.putClientProperty( "JComponent.sizeVariant", sizeVariant )
      panel.add( lbSearch )
      queryFrame.addListener( frameListener )
      search.addListener( searchListener )
      setResizable( false )
      pack()
   }

   private def loggedOut {
      dispose
//      dispatch( LoggedOut )
   }

   override def dispose {
      queryFrame.removeListener( frameListener )
      search.removeListener( searchListener )
      super.dispose
   }

   private def searchDone( smps: IIdxSeq[ Sample ]) {
      dispatch( SearchDone( smps ))
      dispose
   }

   private def searchFailed( failure: SearchFailed ) {
      lbStatus.setText( "FAILED : " + failure )
      ggProgress.setVisible( false )
      dispatch( failure )
   }

   private def defer( thunk: => Unit ) {
      EventQueue.invokeLater( new Runnable { def run = thunk })
   }
}