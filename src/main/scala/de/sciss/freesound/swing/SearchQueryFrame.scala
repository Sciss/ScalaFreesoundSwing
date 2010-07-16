/*
 *  SearchQueryFrame.scala
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
import java.awt.event.{ActionEvent, ActionListener}
import de.sciss.freesound.{SearchOptions, Search, Model, Login}
import de.sciss.freesound.swing.SearchQueryFrame.NewSearch
import table.TableModel

object SearchQueryFrame {
   case class NewSearch( idx: Int, search: Search )
}

class SearchQueryFrame( loginFrame: LoginFrame, login: Login, sizeVariant: String = "small" )
extends JFrame( "New Freesound Search" ) with Model {

   private val frameListener: Model.Listener = { case LoginFrame.LoggedOut => loggedOut }
   private val ggKeyword  = new JTextField( 32 )

   // ---- constructor ----
   {
      val panel      = getContentPane()
      val lay        = new GroupLayout( panel )
      lay.setAutoCreateContainerGaps( true )
      panel.setLayout( lay )
      val lbKeyword  = new JLabel( "Keyword(s):", SwingConstants.TRAILING )
      lbKeyword.putClientProperty( "JComponent.sizeVariant", sizeVariant )
      ggKeyword.putClientProperty( "JComponent.sizeVariant", sizeVariant )
      val gapKeyword = Box.createHorizontalStrut( 4 )
      val ggSearch   = new JButton( "Search" )
      ggSearch.putClientProperty( "JComponent.sizeVariant", sizeVariant )
      ggSearch.putClientProperty( "JButton.buttonType", "bevel" )

      ggSearch.addActionListener( new ActionListener { def actionPerformed( e: ActionEvent ) { searchAction }})

      lay.setHorizontalGroup( lay.createParallelGroup()
         .addGroup( lay.createSequentialGroup()
            .addComponent( lbKeyword )
            .addComponent( gapKeyword )
            .addComponent( ggKeyword )
         )
         .addGroup( lay.createSequentialGroup()
            .addComponent( ggSearch )
         )
      )

      lay.setVerticalGroup( lay.createSequentialGroup()
         .addGroup( lay.createParallelGroup( GroupLayout.Alignment.BASELINE )
            .addComponent( lbKeyword )
            .addComponent( gapKeyword )
            .addComponent( ggKeyword )
         )
         .addGroup( lay.createParallelGroup( GroupLayout.Alignment.BASELINE )
            .addComponent( ggSearch )
         )
      )

      loginFrame.addListener( frameListener )
      setResizable( false )
      pack()
   }

   private def loggedOut {
      dispose
      dispatch( LoginFrame.LoggedOut )
   }

   private var idxCount = 0

   private def searchAction {
      val keyword = ggKeyword.getText()
      val options = SearchOptions( keyword )
      val search  = login.search( options )
      idxCount += 1
      dispatch( NewSearch( idxCount, search ))
      search.perform
   }
}