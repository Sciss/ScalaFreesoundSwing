/*
 *  ScalaFreesoundSwing.scala
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

import javax.swing.WindowConstants
import java.awt.{Point, EventQueue}
import java.io.File
import de.sciss.freesound.{Search, SampleInfoCache, Freesound}

object ScalaFreesoundSwing {
   val name          = "ScalaFreesound-Swing"
   val version       = 0.12
   val copyright     = "(C)opyright 2010-2011 Hanns Holger Rutz"
   def versionString = (version + 0.001).toString.substring( 0, 4 )

   def main( args: Array[ String ]) {
      args.headOption match {
         case Some( "--test" ) =>
            val icachePath    = if( args.size > 1 ) Some( args( 1 )) else None
            val downloadPath  = if( args.size > 2 ) Some( args( 2 )) else None
            EventQueue.invokeLater( new Runnable {
               def run = test( icachePath, downloadPath )
            })
         case _ => {
            printInfo
            System.exit( 1 )
         }
      }
   }

   def printInfo {
      println( "\n" + name + " v" + versionString + "\n" + copyright +
""". All rights reserved.

   --test [<cacheFolder>] [<downloadFolder>]  to run the test application
""" )
   }

   def test( icachePath: Option[ String ], downloadPath: Option[ String ]) {
      // prevent actor starvation!!!
      // --> http://scala-programming-language.1934581.n4.nabble.com/Scala-Actors-Starvation-td2281657.html
      System.setProperty( "actors.enableForkJoin", "false" )

      val icache = icachePath.map( SampleInfoCache.persistent( _ )) 
      val f = new LoginFrame()
      f.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE )
      f.setLocation( 40, 40 )
      f.setVisible( true )
      f.addListener {
         case LoginFrame.LoggedIn( login ) => {
            val sqf = new SearchQueryFrame( f, login )
            sqf.setLocationRelativeTo( null )
            sqf.setLocation( sqf.getX(), 40 )
            sqf.setVisible( true )
            sqf.addListener {
               case SearchQueryFrame.NewSearch( idx, search ) => {
                  val title = "Freesound Search #" + idx + " (" + {
                        val kw = search.options.keyword
                        if( kw.size < 24 ) kw else kw.take( 23 ) + "…"
                     } + ")"
                  val spf = new SearchProgressFrame( sqf, search, title )
                  spf.setLocationRelativeTo( null )
                  spf.setVisible( true )
                  spf.addListener {
                     case Search.SearchDone( samples ) => {
                        val srf = new SearchResultFrame( samples, Some( login ), title, icache, downloadPath )
                        srf.setLocationRelativeTo( null )
                        srf.setVisible( true )
                     }
                  }
               }
            }
         }
      }
   }
}