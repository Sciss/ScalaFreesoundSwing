/*
 *  LoginFrame.scala
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
import event.{DocumentListener, DocumentEvent}
import java.awt.event.{InputMethodListener, InputMethodEvent, ActionListener, ActionEvent}
import java.awt.{EventQueue, BorderLayout, Dimension}
import de.sciss.freesound.{Login, Model, Freesound, LoginProcess}
import LoginProcess._

/**
 *    @version 0.11, 17-Jul-10
 */
object LoginFrame {
   case class LoggedIn( login: Login )
   case object LoggedOut
}

class LoginFrame( sizeVariant: String = "small" )
extends JFrame( "Freesound Login" ) with Model {
   import LoginFrame._
   
   private var loginProc : Option[ LoginProcess ] = None
   private val ggProgress = new JProgressBar()
   private val lbStatus   = new JLabel()
   private val ggLogin    = new JButton()
   private val ggUsername = new JTextField( 12 )
   private val ggPassword = new JPasswordField( 12 )

   private val loginProcListener: Model.Listener = {
      case LoginProcess.LoginBegin     => defer( loginBegin )
      case r: LoginProcess.LoginResult => defer( loginResult( r ))
   }

   // ---- constructor -----
   {
      val panel = getContentPane()
      val lay  = new GroupLayout( panel )
      lay.setAutoCreateContainerGaps( true )
      panel.setLayout( lay )
      val lbUsername = new JLabel( "Username:", SwingConstants.TRAILING )
      val gapUsername = Box.createHorizontalStrut( 4 )
      lbUsername.putClientProperty( "JComponent.sizeVariant", sizeVariant )
      val lbPassword = new JLabel( "Password:", SwingConstants.TRAILING )
      val gapPassword = Box.createHorizontalStrut( 4 )
      lbPassword.putClientProperty( "JComponent.sizeVariant", sizeVariant )
      ggUsername.putClientProperty( "JComponent.sizeVariant", sizeVariant )
      ggPassword.putClientProperty( "JComponent.sizeVariant", sizeVariant )
      ggLogin.putClientProperty( "JComponent.sizeVariant", sizeVariant )
      ggLogin.putClientProperty( "JButton.buttonType", "bevel" )
      ggProgress.setIndeterminate( true )
      val dimProgress   = new Dimension( 24, 24 )
      ggProgress.setPreferredSize( dimProgress )
      ggProgress.putClientProperty( "JProgressBar.style", "circular" )
      val boxProgress = new JPanel()
      boxProgress.setLayout( new OverlayLayout  ( boxProgress ))
      boxProgress.add( Box.createRigidArea( dimProgress ))
      boxProgress.add( ggProgress )
      lbStatus.putClientProperty( "JComponent.sizeVariant", sizeVariant )

      val enableListener = new DocumentListener {
         def insertUpdate( e: DocumentEvent )  { checkLoginEnabled }
         def removeUpdate( e: DocumentEvent )  { checkLoginEnabled }
         def changedUpdate( e: DocumentEvent ) { checkLoginEnabled }
      }

      val actionButton = new ActionListener {
         def actionPerformed( e: ActionEvent ) {
            if( loginProc.isEmpty && ggLogin.isEnabled() ) {
               ggLogin.doClick( 200 )
            }
         }
      }

      ggUsername.getDocument().addDocumentListener( enableListener )
      ggPassword.getDocument().addDocumentListener( enableListener )
      ggUsername.addActionListener( actionButton )
      ggPassword.addActionListener( actionButton )

      ggLogin.addActionListener( new ActionListener { def actionPerformed( e: ActionEvent ) { performLogin }})

      lay.setHorizontalGroup( lay.createParallelGroup()
         .addGroup( lay.createSequentialGroup()
            .addGroup( lay.createParallelGroup()
               .addComponent( lbUsername )
               .addComponent( lbPassword )
            )
            .addGroup( lay.createParallelGroup()
               .addComponent( gapUsername )
               .addComponent( gapPassword )
            )
            .addGroup( lay.createParallelGroup()
               .addComponent( ggUsername )
               .addComponent( ggPassword )
            )
         )
         .addGroup( lay.createSequentialGroup()
            .addComponent( ggLogin )
            .addComponent( boxProgress )
            .addComponent( lbStatus )
         )
      )

      lay.setVerticalGroup( lay.createSequentialGroup()
         .addGroup( lay.createParallelGroup( GroupLayout.Alignment.BASELINE )
            .addComponent( lbUsername )
            .addComponent( gapUsername )
            .addComponent( ggUsername )
         )
         .addGroup( lay.createParallelGroup( GroupLayout.Alignment.BASELINE )
            .addComponent( lbPassword )
            .addComponent( gapPassword )
            .addComponent( ggPassword )
         )
         .addGroup( lay.createParallelGroup( GroupLayout.Alignment.CENTER ) // screw baseline, ggProgress needs center
            .addComponent( ggLogin )
            .addComponent( boxProgress )
            .addComponent( lbStatus )
         )
      )

      clearLoginProc

      setResizable( false )
      pack()
   }

   def username : String = ggUsername.getText()
   def username_=( value: String ) { ggUsername.setText( value )}
   def password_=( value: String ) { ggPassword.setText( value )}

   private def defer( thunk: => Unit ) {
      EventQueue.invokeLater( new Runnable { def run = thunk })
   }

   private def loginBegin {
      ggLogin.setEnabled( false )
      ggProgress.setVisible( true )
      ggUsername.setEnabled( false )
      ggPassword.setEnabled( false )
      lbStatus.setText( "Connecting..." )
   }

   private def loginResult( r: LoginResult ) {
      ggProgress.setVisible( false )
      ggLogin.setEnabled( true )
      r match {
         case LoginDone( login ) => {
            lbStatus.setText( "Connected." )
            ggLogin.setText( "Logout" )
            dispatch( LoggedIn( login ))
         }
         case LoginFailedCurl => {
            lbStatus.setText( "Site unavailable!" )
            clearLoginProc
         }
         case LoginFailedCredentials => {
            lbStatus.setText( "Invalid credentials!" )
            clearLoginProc
         }
         case LoginFailedTimeout => {
            lbStatus.setText( "Timeout!" )
            clearLoginProc
         }
      }
   }

   private def checkLoginEnabled {
      if( loginProc.isDefined ) return
      val enable = !ggUsername.getText().isEmpty && !ggPassword.getPassword().isEmpty
      if( ggLogin.isEnabled() != enable ) {
         ggLogin.setEnabled( enable )
      }
   }

   def performLogin {
      loginProc match {
         case Some( proc ) => {
            if( proc.result.isDefined ) {
               clearLoginProc
               lbStatus.setText( null )
            }
         }
         case None => {
            val username   = ggUsername.getText()
            val passwordA  = ggPassword.getPassword()
            if( username.isEmpty || passwordA.isEmpty ) return
            val password   = new String( passwordA )
            val proc       = Freesound.login( username, password )
            proc.addListener( loginProcListener )
            proc.perform
            loginProc      = Some( proc )
         }
      }
   }

   private def clearLoginProc {
      val wasIn = loginProc.isDefined
      loginProc.foreach( _.removeListener( loginProcListener ))
      loginProc   = None
      ggLogin.setText( "Login" )
      ggUsername.setEnabled( true )
      ggPassword.setEnabled( true )
      ggProgress.setVisible( false )
      checkLoginEnabled
      if( wasIn ) dispatch( LoggedOut )
   }
}