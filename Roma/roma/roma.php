<?php
// ######################################################################
//
// ######################################################################

// ######################################################################
// --- LICENSE
// ######################################################################

//This is the Roma processor
//
// This software is dual-licensed:
// 
// 1. Distributed under a Creative Commons Attribution-ShareAlike 3.0
// Unported License http://creativecommons.org/licenses/by-sa/3.0/ 
// 
// 2. http://www.opensource.org/licenses/BSD-2-Clause
// 		
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
// 
// * Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
// 
// * Redistributions in binary form must reproduce the above copyright
// notice, this list of conditions and the following disclaimer in the
// documentation and/or other materials provided with the distribution.
// 
// This software is provided by the copyright holders and contributors
// "as is" and any express or implied warranties, including, but not
// limited to, the implied warranties of merchantability and fitness for
// a particular purpose are disclaimed. In no event shall the copyright
// holder or contributors be liable for any direct, indirect, incidental,
// special, exemplary, or consequential damages (including, but not
// limited to, procurement of substitute goods or services; loss of use,
// data, or profits; or business interruption) however caused and on any
// theory of liability, whether in contract, strict liability, or tort
// (including negligence or otherwise) arising in any way out of the use
// of this software, even if advised of the possibility of such damage.
// 
// ######################################################################
// --- The Roma PACKAGE
// ######################################################################

// ######################################################################
// --- Installation
// ######################################################################

// copy the files index.html and startroma.php into your web-directory.
// copy the directories roma, parser to your include directory
// make sure you eXist is running on the same Server and can be accessed on Port 8080
//
// Setting up a directory for temporary files. Specify that directory in the constants.
// Your webserver needs to have writing access to that directory!
//
// You need to have trang installed ( http://www.thaiopensource.com/relaxng/trang.html )
// 

// ######################################################################
// --- DESCRIPTION
// ######################################################################


// ######################################################################
// --- REQUIRES
// ######################################################################

require_once( 'config.php' );

// The parser is used to do some simple replacements for which
// XSLT would have been much too powerful

require_once( 'parser/parser.php' );

// Romadom extends the domDocument class. It is used to hold the
// Customization file.
require_once( 'roma/romadom.php' );

//sanity cherker files
require_once( 'roma/sanitychecker_errorhandler.php' );
require_once( 'roma/sanitychecker.php' );

// Handles Notams
require_once( 'notam/notamHandler.php' );

//Exceptions
require_once( 'roma/exception.php' );

//Resources
require_once( 'resource/resource.php' );

// ######################################################################
// --- CONSTANTS
// ######################################################################

//#########################
// Startup Options
//
// These Options shouldn't be changed. But the only important thing
// is, that they do not have the same value
define( 'roma_startupOption_all', 'all' );
define( 'roma_startupOption_minimum', 'minimum' );
define( 'roma_startupOption_other', 'other' );
define( 'roma_startupOption_upload', 'upload' );
define( 'roma_startupOption_template', 'template' );

define( 'roma_validate_schema', false );


//#########################
// The different Modes
//
// These are the different modes which are used by Roma (GET parameters).
// You do not need to change them.
define( 'roma_mode_main', 'main' );
define( 'roma_mode_changeModule', 'changeModule' );
define( 'roma_mode_addElements', 'addElements' );
define( 'roma_mode_addModule', 'addModule' );
define( 'roma_mode_removeModule', 'removeModule' );
define( 'roma_mode_moduleChanged', 'moduleChanged' );
define( 'roma_mode_saveCustomization', 'saveCustomization' );
define( 'roma_mode_createSchema', 'createSchema' );
define( 'roma_mode_processCreateSchema', 'processCreateSchema' );
define( 'roma_mode_elementAdded', 'elementAdded' );
define( 'roma_mode_customizeLanguage', 'customizeLanguage' );
define( 'roma_mode_processCustomizeLanguage', 'processCustomizeLanguage' );
define( 'roma_mode_processChangeElement', 'changeElement' );
define( 'roma_mode_elementChanged', 'elementChanged' );
define( 'roma_mode_processAddAttribute', 'addAttribute' );
define( 'roma_mode_attributeAdded', 'attributeAdded' );
define( 'roma_mode_newCustomization', 'newCustomization' );
define( 'roma_mode_listAddedElements', 'listAddedElements' );
define( 'roma_mode_listAddedAttributes', 'listAddedAttributes' );
define( 'roma_mode_changeClasses', 'changeClasses' );
define( 'roma_mode_changeListAddedAttributes', 'changeListAddedAttributes' );
define( 'roma_mode_deleteAttribute', 'deleteAttribute' );
define( 'roma_mode_deleteAddedElement', 'deleteAddedElement' );
define( 'roma_mode_changeListAddedElements', 'changeListAddedElements' );
define( 'roma_mode_createDocumentation', 'createDocumentation' );
define( 'roma_mode_generateDoc', 'generateDoc' );
define( 'roma_mode_downloadFile', 'download' );
define( 'roma_mode_customizeCustomization', 'customizeCustomization' );
define( 'roma_mode_processCustomizeCustomization', 'processCustomizeCustomization' );


//#########################
// Roma messages
//
// Sometimes Roma feels the need to notify the user
define( 'roma_message_elmentAdded', 'Element was added' );
define( 'roma_message_headline_changeElement', 'Change element' );
define( 'roma_message_loadContentError', 'Your content seems to be no valid XML' );
define( 'roma_message_elementNameError', 'Element names have to be alphanumeric and have to contain at least one character.' );
define( 'roma_message_attributeNameError', 'Attribute names have to be alphanumeric and have to contain at least one character.' );
define( 'roma_message_attributeAdded', 'The attribute was added' );
define( 'roma_message_changedElement', 'Successfully changed Element' );
define( 'roma_message_elementContentError', 'Content has to be valid XML.' );
define( 'roma_message_elementExistsError', 'Element already exists' );
define( 'roma_message_attributeExistsError', 'Attribute already exists' );

//#########################
// Other stuff
//
//
define( 'roma_template_append', '</body></html>' );

/**
 * ROMA.
 *
 ***
 * PASS BY REFERENCE
 * I decided to use the functions return parameter as Error indicator.
 * Therefor, every function should return false on success. All other
 * return values are handled by parameters which are passed by reference.
 * 
 * @author: Arno Mittelbach <arno@mittelbach-online.de>
 * @version: 0.9
 * @access:  public
 * @package: roma
 */
class roma
  {
    /**
     * This member variable holds the current Customization
     */     
    private $m_oRomaDom;
    
    private $m_szLanguage;

    private $m_szOddLanguage;

    private $m_szDocLanguage;

    /**
     * The Constructor adds the mandatory modules
     * and reloads the customization from the session.
     * If no customization is given, it creates a new
     * one.
     */
    function __construct( $nOption = '')
      {

	// Sessions have to be started
	session_start();

        switch( $nOption )
          {
            case roma_startupOption_upload:
	      $this->m_oRomaDom = new romaDom( join( '', file(  $_FILES[ 'customization' ][ 'tmp_name' ] ) ) );
              break;
            case roma_startupOption_minimum:
               $this->m_oRomaDom = new romaDom( '' ); 
               $this->m_oRomaDom->addModule( 'core' );
               $this->m_oRomaDom->addModule( 'tei' ); 
               $this->m_oRomaDom->addModule( 'header' ); 
               $this->m_oRomaDom->addModule( 'textstructure' ); 
	       break;
	    case roma_startupOption_all:
	      $this->m_oRomaDom = new romaDom( join( '',
          file('/usr/share/xml/tei/custom/odd/tei_all.odd'   )));
	       break;
	    case roma_startupOption_other:
	      $this->m_oRomaDom = new romaDom( join( '',
          file('/usr/share/xml/tei/custom/odd/' . $_REQUEST ['tei_other' ] )));
	       break;
	    case roma_startupOption_template:
	      $this->m_oRomaDom = new romaDom( join( '',
          file('/usr/share/xml/tei/custom/odd/' . $_REQUEST ['tei_template' ] )));
	       break;
	    default:
               $this->m_oRomaDom = new romaDom( $_SESSION[ 'romaDom' ] );
              break;
            }          

	//set Language
	$this->m_oRomaDom->getCustomizationLanguage( $this->m_szLanguage );
	$this->m_oRomaDom->getOddLanguage( $this->m_szOddLanguage );
	$this->m_oRomaDom->getDocLanguage( $this->m_szDocLanguage );

      }

    // #####################################################################
    // --- Decide which mode to use
    // #####################################################################

    /**
     * This function is called from the outside script
     * It actually starts the whole roma programm.
     * Commands are executed here. After every command that changes something
     * in the customization, the webserver delivers a redirect.
     * At the end of this function the customization is saved inside the session.
     * The actual output is printed on the screen at the end of this function.
     */
    public function run()
      {
	//Check for Help
	if ( $_REQUEST[ 'help' ] == 'switch' )
	  {
	    $_SESSION[ 'help' ] = ( $_SESSION[ 'help' ] == 'on' ) ? 'off' : 'on';
	    $this->redirectBrowserHeader( $_SESSION[ 'lastQuery' ] );
	  }
	else
	  {
	    $_SESSION[ 'lastQuery' ] = $_SERVER[ 'QUERY_STRING' ];
	  }
	
        switch( $_REQUEST[ 'mode' ] )
          {
	    case roma_mode_processCustomizeCustomization:
	      $this->customizedCustomization();
	      $this->redirectBrowserHeader( 'mode=' . roma_mode_customizeCustomization );
	      break;
 	    case roma_mode_downloadFile:
	      $this->downloadFile( $szOutput );
	      break;
 	    case roma_mode_generateDoc:
	      $this->generateDocumentation( $szOutput );
	      break;
 	    case roma_mode_createDocumentation:
	      $this->processCreateDocumentation( $szOutput );
	      break;
 	    case roma_mode_changeListAddedElements:
	      $this->changeListAddedElements();
	      $this->redirectBrowserHeader( 'mode=' . roma_mode_listAddedElements );
	      break;
 	    case roma_mode_deleteAddedElement:
	      if( $this->m_oRomaDom->deleteAddedElement( $_REQUEST[ 'element' ] ) === false )
		{
		  //notam
		  $oNotam = new notam();
		  $oNotam->setHeadline( 'Delete element' );
		  $oNotam->setMessage( 'Element ' . $_REQUEST[ 'name' ] . ' successfully deleted' );
		  $oNotam->setStatus( notam_status_success );
		  $oNotam->addNotam();
		}
	      else
		{
		  //notam
		  $oNotam = new notam();
		  $oNotam->setHeadline( 'Delete element' );
		  $oNotam->setMessage( 'Could not delete Element ' . $_REQUEST[ 'name' ] );
		  $oNotam->setStatus( notam_status_error );
		  $oNotam->addNotam();
		}
	      $this->redirectBrowserHeader( 'mode=' . roma_mode_listAddedElements );
	      break;
 	    case roma_mode_deleteAttribute:
	      $this->m_oRomaDom->deleteAttribute( $_REQUEST[ 'attribute' ], $_REQUEST[ 'class' ], $_REQUEST[ 'module' ], $_REQUEST[ 'element' ] );
	      $this->redirectBrowserHeader( 'mode=' . roma_mode_listAddedAttributes . '&element=' . $_REQUEST[ 'element' ] . '&module=' . $_REQUEST[ 'module' ] . '&class=' . $_REQUEST[ 'class' ] );
	      break;
 	    case roma_mode_changeListAddedAttributes:
	      $this->changeListAddedAttributes();
	      $this->redirectBrowserHeader( 'mode=' . roma_mode_listAddedAttributes . '&element=' . $_REQUEST[ 'element' ] . '&module=' . $_REQUEST[ 'module' ] . '&class=' . $_REQUEST[ 'class' ] );
	      break;
            case roma_mode_changeClasses:
              $this->processChangeClasses( $szOutput );
              break;
            case roma_mode_listAddedElements:
              $this->processListAddedElements( $szOutput );
              break;
            case roma_mode_listAddedAttributes:
              $this->processListAddedAttributes( $szOutput );
              break;
	    case roma_mode_newCustomization:
	      unset( $this->m_oRomaDom );
	      unset( $_SESSION[ 'romaDom' ] );
	      $this->redirectToStart();
	      break;
	    case roma_mode_elementChanged:
	      if ( $this->elementChanged() === false )
		{
		  $this->redirectBrowserHeader( 'mode=' . roma_mode_changeModule . '&module=' . $_REQUEST[ 'module' ] );
		}
	      else
		{
		  $this->redirectBrowserHeader( 'mode=' . roma_mode_processChangeElement . '&module=' . $_REQUEST[ 'module' ] . '&element=' . $_REQUEST[ 'name' ] );
		}
	      break; 
  	    case roma_mode_processAddAttribute:
              $this->processAddAttribute( $szOutput );
	      break;
            case roma_mode_attributeAdded:
	      $aszOptions = array( 'name' => $_REQUEST[ 'name' ],
				   'class' => $_REQUEST[ 'class' ],
				   'altname' => $_REQUEST[ 'altname' ],
				   'module' => $_REQUEST[ 'module' ],
				   'changedDesc' => $_REQUEST[ 'changedDesc'],
				   'changedName' => $_REQUEST[ 'changedName'],
				   'changedUsage' => $_REQUEST[ 'changedUsage'],
				   'changedContent' => $_REQUEST[ 'changedContent'],
				   'element' => $_REQUEST[ 'element' ],
				   'valList' => $_REQUEST[ 'valList' ],
				   'added' => $_REQUEST[ 'added' ],
				   'optional' => $_REQUEST[ 'optional' ],
				   'maxOccurs' => $_REQUEST[ 'maxOccurs' ],
				   'minOccurs' => $_REQUEST[ 'minOccurs' ],
				   'closed' => $_REQUEST[ 'closed' ],
				   'content' => $_REQUEST[ 'content' ],
				   'defaultValue' => $_REQUEST[ 'defaultValue' ],
				   'description' => $_REQUEST[ 'description' ] );
	      try 
		{
		  $this->m_oRomaDom->addAttribute( $aszOptions );

		  //notam
		  $oNotam = new notam();
		  if ( $_REQUEST[ 'added' ] == 'true' )
		    {
		      $oNotam->setHeadline( 'Add Attribute' );
		      $oNotam->setMessage( 'Attribute ' . $_REQUEST[ 'name' ] . ' was successfully added' );
		    }
		  else
		    {
		      $oNotam->setHeadline( 'Change Attribute' );
		      $oNotam->setMessage( 'Attribute ' . $_REQUEST[ 'name' ] . ' was successfully changed' );
		    }
		  $oNotam->setStatus( notam_status_success );
		  $oNotam->addNotam();

		  $this->redirectBrowserHeader( 'mode=' . roma_mode_listAddedAttributes . '&module=' . $_REQUEST[ 'module' ] . '&element=' . $_REQUEST[ 'element' ] . '&class=' . $_REQUEST[ 'class' ]);
		}
	      catch( falseTagnameException $e )
		{
		  $e->addError( 'addAttribute', 'name' );
		  
		  //notam
		  $oNotam = new notam();
		  $oNotam->setHeadline( 'Add Attribute' );
		  $oNotam->setMessage( roma_message_attributeNameError );
		  $oNotam->setStatus( notam_status_error );
		  $oNotam->addNotam();
		  
		  $this->redirectBrowserHeader( 'mode=' . roma_mode_processAddAttribute . '&module=' . $_REQUEST[ 'module' ] . '&element=' . $_REQUEST[ 'element' ] . '&class=' . $_REQUEST[ 'class' ]);
		}
	      catch( attributeExistsException $e )
		{
		  $e->addError( 'addAttribute', 'name' );
		  
		  //notam
		  $oNotam = new notam();
		  $oNotam->setHeadline( 'Add Attribute' );
		  $oNotam->setMessage( roma_message_attributeExistsError );
		  $oNotam->setStatus( notam_status_error );
		  $oNotam->addNotam();
		  
		  $this->redirectBrowserHeader( 'mode=' . roma_mode_processAddAttribute . '&module=' . $_REQUEST[ 'module' ] . '&element=' . $_REQUEST[ 'element' ] . '&class=' . $_REQUEST[ 'class' ]);
		}
		  
              break;
            case roma_mode_changeModule:
              $this->processChangeModule( $szOutput );
              break;
            case roma_mode_addElements:
	      $this->processAddElements( $szOutput );
              break;
            case roma_mode_createSchema:
	      $this->processCreateSchema( $szOutput );
              break;
            case roma_mode_processCreateSchema:
	      $this->MakeSchema();
              break;
            case roma_mode_processChangeElement:
	      $this->processChangeElement( $szOutput );
              break;
            case roma_mode_processCustomizeLanguage:
	    	$this->getParser( $oParser );
		$this->m_oRomaDom->getDocLanguage( $this->m_szDocLanguage );
		$this->m_oRomaDom->getCustomizationLanguage( $szLanguage );
	      //notam
	      $this->m_oRomaDom->setDocLanguage( $_REQUEST[ 'doclanguage' ] );
	      $oNotam = new notam();
	      $oNotam->setHeadline( 'Language customization' );
	      $oNotam->setMessage( 'Translate to ' .
	      $_REQUEST[ 'doclanguage' ] . 
	      ' for documentation: session lang is ' . 
	      $_SESSION['docLang'] . 
	      ' and customization language is ' . $szLanguage);
	      $oNotam->setStatus( notam_status_success );
	      $oNotam->addNotam();
	      $this->redirectBrowserHeader( 'mode=' . roma_mode_customizeLanguage );
              break;
            case roma_mode_customizeLanguage:
	      $this->processCustomizeLanguage( $szOutput );
              break;
            case roma_mode_saveCustomization:
              $this->saveCustomization( $szOutput );
              break;
            case roma_mode_elementAdded:
	      $aszClasses = array();
	      foreach( $_REQUEST as $key => $value )
		{
		  if ( substr( $key, 0, 6 ) == 'class|' )
		    {
		      $aszClasses[] = $value;
		    }
		}
	      $aszConfig = array( 'name' => $_REQUEST[ 'name' ],
				  'added' => $_REQUEST[ 'added' ],
				  'namespace' => $_REQUEST[ 'elementNamespace' ],
				  'content' => $_REQUEST[ 'content' ],
				  'contentmodel' => $_REQUEST[ 'contentmodel' ],
				  'userContent' => $_REQUEST['userContent'],
				  'classes' => $aszClasses,
				  'description' => $_REQUEST[ 'description' ]);
	      try {
		$this->m_oRomaDom->addElement( $aszConfig );

		//notam
		$oNotam = new notam();
		$oNotam->setHeadline( 'Create new element' );
		$oNotam->setMessage( 'New element ' . $_REQUEST[ 'name' ] . ' successfully created' );
		$oNotam->setStatus( notam_status_success );
		$oNotam->addNotam();

		$this->redirectBrowserHeader( 'mode=' . roma_mode_listAddedElements );
	      }
	      catch( falseTagnameException $e )
		{
		  $e->addError( 'addElement', 'name' );

		  $_SESSION[ 'addElements' ][ 'ERROR' ][ 'classes' ] = $aszClasses;
		  $_SESSION[ 'addElements' ][ 'ERROR' ][ 'content' ] = $_REQUEST[ 'content' ];
		  $_SESSION[ 'addElements' ][ 'ERROR' ][ 'description' ] = $_REQUEST[ 'description' ];

		  //notam
		  $oNotam = new notam();
		  $oNotam->setHeadline( 'Create new element' );
		  $oNotam->setMessage( roma_message_elementNameError );
		  $oNotam->setStatus( notam_status_error );
		  $oNotam->addNotam();

		  $this->redirectBrowserHeader( 'mode=' . roma_mode_addElements );
		}
	      catch( elementExistsException $e )
		{
		  $e->addError( 'addElement', 'name' );

		  $_SESSION[ 'addElements' ][ 'ERROR' ][ 'classes' ] = $aszClasses;
		  $_SESSION[ 'addElements' ][ 'ERROR' ][ 'content' ] = $_REQUEST[ 'content' ];
		  $_SESSION[ 'addElements' ][ 'ERROR' ][ 'description' ] = $_REQUEST[ 'description' ];

		  //notam
		  $oNotam = new notam();
		  $oNotam->setHeadline( 'Create new element' );
		  $oNotam->setMessage( roma_message_elementExistsError );
		  $oNotam->setStatus( notam_status_error );
		  $oNotam->addNotam();

		  $this->redirectBrowserHeader( 'mode=' . roma_mode_addElements );
		}

              break;
            case roma_mode_moduleChanged:
	      $this->moduleChanged();
	      $this->redirectBrowserHeader( 'mode=changeModule&module=' . $_REQUEST[ 'module' ] );
	      break;
            case roma_mode_addModule:
	      $this->addModule();
	      $this->redirectBrowserHeader( 'mode=' . roma_mode_main );
	      break;
            case roma_mode_removeModule:
	      $this->removeModule();
	      $this->redirectBrowserHeader( 'mode=' . roma_mode_main );
	      break;
            case roma_mode_main:
              $this->processMain( $szOutput );
	      break;
            case roma_sanity_check:
              $this->processSanityCheck( $szOutput );
	      break;
	      	case 'roma_sc_model': {
		      	header('Content-Type: text/xml');
		      	echo '<?xml version="1.0" encoding="UTF-8"?>';
		     		echo $_SESSION['model_document'];
	      		break;
	      	}
	    case 'roma_sc_model_html': {
	    			echo '<pre>';
	    			echo htmlspecialchars('<?xml version="1.0" encoding="UTF-8"?>');
	    			echo htmlspecialchars(str_replace("><", ">\n<", $_SESSION['model_document']));
	    			echo '</pre>';
	      		break;
	      	}
	    case roma_mode_customizeCustomization:
	    default;
	      $this->processCustomizeCustomization( $szOutput );
	      break;
          }

	// The current customization is saved inside the session
	$_SESSION[ 'romaDom' ] = $this->m_oRomaDom->SaveXML();
	
	// The output is printed on the screen.
	echo $szOutput;
      }


    // #####################################################################
    // --- Little Helpers
    // #####################################################################

    private function getParser( &$oParser )
      {
	$oParser = new parser();
	//check for notams
	notamHandler::outputHTML( $szHTML );
	notamHandler::deleteNotams();
	$oParser->addReplacement( 'notams', $szHTML );
	$oParser->addReplacement( 'help', $_SESSION[ 'help' ] );

	$oParser->addReplacement( 'lang', $this->m_szLanguage );
      }

    private function addErrorsDom( &$oDom, $aoErrors )
      {
	if ( is_object( $aoErrors ) )
	  {
	    $oErrorRoot = $oDom->appendChild( new domElement( 'errorList' ) );
	    foreach( $aoErrors as $oError )
	      {
		$oError = $oDom->importNode( $oError, true );
		$oErrorRoot->appendChild( $oError );
	      }
	  }
      }
    
    private function getListDom( $szQuery, &$oListDom )
      {
	$oListDom = new domDocument();
	$oListDom->appendChild( new domElement( 'list' ) );

	$oTmpDom = new domDocument();
	$oTmpDom->loadXML( join( '', file( $szQuery ) ) );
	$oElemsByMod = $oListDom->importNode( $oTmpDom->documentElement, true );
	$oListDom->documentElement->appendChild( $oElemsByMod );
      }

    private function createChangeInListDom( $oListDom, $aszDom )
      {
	if ( is_array( $aszDom ) )
	  {
	    $oChanges = $oListDom->documentElement->appendChild( new domElement( 'changes' ) );
	    foreach( $aszDom as $oDom )
	      {
		$oRoot = $oListDom->importNode( $oDom->documentElement, true );
		$oChanges->appendChild( $oRoot );
	      }
	  }
      }

    private function appendOutput( &$szOutput )
      {
	$szOutput .= roma_template_append;
      }

    private function redirectBrowserMeta( $szLocation )
      {
	echo '<meta http-equiv="refresh" content="0; url=' . $szLocation . '">';
	echo '<br><a href="' . $szLocation . '">If your browser did not redirect you, please click here</a>';
	flush();
      }

    private function redirectBrowserHeader( $szLocation )
      {
	header( "Location: http://" . $_SERVER[ 'HTTP_HOST' ] . $_SERVER[ 'PHP_SELF' ] . '?' . $szLocation );
      }
    
    private function redirectToStart()
      {
	header( "Location: http://" . $_SERVER[ 'HTTP_HOST' ] . '/Roma/' );
      }
    

    // #####################################################################
    // --- Different process Functions
    // each process function has one parameter
    // this parameter is called szOutput. The functions save their
    // Output in that variable which is then echoed by the run function.
    // #####################################################################
 
    /**
     * The main view is created here
     */
    private function processMain( &$szOutput )
      {
        $szTemplate = join( '', file(  roma_templateDir . '/main.tem' ) );
	$this->getParser( $oParser );
	$this->m_oRomaDom->getCustomizationLanguage( $szLanguage );
	$this->m_oRomaDom->getDocLanguage( $szDocLanguage );	
	$this->getListDom( roma_xquery_server . 'modules.xql?lang='
	. $szDocLanguage , $oListDom );
	$this->m_oRomaDom->getCustomizationTitle( $szTitle );

	// build param list
	$this->m_oRomaDom->getSelectedModulesDom( $oModules );
	$this->m_oRomaDom->getChangedModulesDom( $oChanged );

	$this->createChangeInListDom( $oListDom, array ( $oModules, $oChanged ) );

	$this->applyStylesheet( $oListDom, 'modules.xsl', $oNewDom, array('lang' => $szDocLanguage ), 'modules' );
	$oParser->addReplacement( 'lang', $szLanguage );
	$oParser->addReplacement( 'doclang', $szDocLanguage);
	$oParser->addReplacement ('Version', $_SESSION['Version']);
	$oParser->addReplacement( 'mode', 'modules' );
	$oParser->addReplacement( 'view', 'main' );
	$oParser->addReplacement( 'title', $szTitle );
	$oParser->addReplacement( 'template', $oNewDom->SaveHTML() );
        $oParser->Parse( $szTemplate, $szOutput );

	$this->appendOutput( $szOutput );
      }

    /**
     * The sanity check is provided here
    */
    private function processSanityCheck( &$szOutput )
      {
	$szTemplate = join( '', file(  roma_templateDir . '/main.tem' ) );
	$szSchemTem = join( '', file(  roma_templateDir . '/sanityCheck.tem' ) );
	$this->getParser( $oParser );
	
	$oSchemaParser = new parser();
	$this->m_oRomaDom->getCustomizationLanguage( $szLanguage );
	$this->m_oRomaDom->getDocLanguage( $szDocLanguage );	
	$this->m_oRomaDom->getCustomizationTitle( $szTitle );
	$oSchemaParser->addReplacement( 'lang', $szLanguage );
	$oSchemaParser->addReplacement( 'doclang',  $szDocLanguage );
	$oParser->addReplacement ('Version', $_SESSION['Version']);
	$oSchemaParser->addReplacement( 'output', $_REQUEST[ 'output' ] );
	$oSchemaParser->Parse( $szSchemTem, $szSchema );
	$oParser->addReplacement( 'lang', $szLanguage );
	$oParser->addReplacement( 'doclang', $szDocLanguage );
	$oParser->addReplacement ('Version', $_SESSION['Version']);
	$oParser->addReplacement( 'mode', 'main' );
	$oParser->addReplacement( 'view', 'sanitycheck' );
	$oParser->addReplacement( 'title', $szTitle );
	$oParser->addReplacement( 'template', $szSchema );
	$oParser->Parse( $szTemplate, $szOutput );

	//$this->appendOutput( $szOutput );
	echo($szOutput);
	$this->m_oRomaDom->processSanityCheck();
	exit(0);
      }


    /**
     * The list of Elements inside a Module with the options to exclude
     * or include or change their names is created here
     */
    private function processChangeModule( &$szOutput )
      {
	$this->m_oRomaDom->getDocLanguage( $szDocLanguage );	
        $szTemplate = join( '', file(  roma_templateDir . '/main.tem' ) );
	$this->getParser( $oParser );
	$this->getListDom( roma_xquery_server . 'elemsbymod.xql?lang='
	. $szDocLanguage . '&module=' . $_REQUEST[ 'module' ], $oListDom );
	notamHandler::getError( 'moduleChanged', $aoErrors );
	notamHandler::deleteError( 'moduleChanged' );
	$this->addErrorsDom( $oListDom, $aoErrors );
	$this->m_oRomaDom->getExcludedElementsInModule( $_REQUEST[ 'module' ], $oExcludedElements );
	$this->m_oRomaDom->getIncludedElementsInModule( $_REQUEST[ 'module' ], $oIncludedElements );
	$this->m_oRomaDom->getElementsWithChangedNameInModuleDom( $_REQUEST[ 'module' ], $oChanged );
	$this->m_oRomaDom->getCustomizationTitle( $szTitle );
	$this->createChangeInListDom( $oListDom, array ( $oChanged, $oExcludedElements, $oIncludedElements ) );
	$this->m_oRomaDom->getCustomizationLanguage( $szLanguage );

	$aszParam =  array( 
          'module' => $_REQUEST[ 'module' ],
	  'lang' => $szLanguage ,
	  'doclang' => $szDocLanguage ,
	  'TEISERVER' => roma_xquery_server,
	  'TEIWEB' => roma_teiweb_server
	);
	$this->applyStylesheet( $oListDom, 'changeModules.xsl', $oNewDom, $aszParam , 'changeModule');
	$oParser->addReplacement( 'lang', $szLanguage );
	$oParser->addReplacement( 'doclang', $szDocLanguage );
	$oParser->addReplacement( 'mode', 'changeModule' );
	$oParser->addReplacement( 'view', 'main' );
	$oParser->addReplacement ('Version', $_SESSION['Version']);
	$oParser->addReplacement( 'title', $szTitle );
	$oParser->addReplacement( 'template', $oNewDom->SaveHTML() );
        $oParser->Parse( $szTemplate, $szOutput );

	$this->appendOutput( $szOutput );
      }

    /**
     * The nice little Screen with all those nice divs for
     * creating a new Element is done in this function
     */
    private function processAddElements( &$szOutput )
      {
        $szTemplate = join( '', file(  roma_templateDir . '/main.tem' ) );
	$this->getParser( $oParser );
	$this->m_oRomaDom->getCustomizationLanguage( $szLanguage );
	$this->m_oRomaDom->getDocLanguage( $szDocLanguage );	
	$oModelClassDom = new domDocument();
	$oModelClassDom->loadXML( join( '', file( roma_xquery_server  . 'classes.xql?lang=' . $szDocLanguage ) ) );
	$oRootClass = $oModelClassDom->documentElement;
	$this->m_oRomaDom->getCustomizationTitle( $szTitle );

	$oAttributeDom = new domDocument();
	$oAttributeDom->loadXML( join( '', 
	file( roma_xquery_server . 'attclasses.xql?lang=' . $szDocLanguage) 
	) );
	$oRootAtt = $oAttributeDom->documentElement;

	$oDatatypeDom = new domDocument();
	$oDatatypeDom->loadXML( join( '', file( roma_xquery_server . 'datatypes.xql' ) ) ) ;
	$oRootDat = $oDatatypeDom->documentElement;

	$oMacroDom = new domDocument();
	$oMacroDom->loadXML( join( '', file( roma_xquery_server . 'macros.xql' ) ) );
	$oRootMac = $oMacroDom->documentElement;

        //Get created Elements
        $this->m_oRomaDom->getAddedElements( $oElementsDom );
        $oRootElement = $oElementsDom->documentElement;


	$oListDom = new domDocument();
	$oRoot = $oListDom->appendChild( new domElement( 'addElement' ) );
	$oRootClass = $oListDom->importNode( $oRootClass, true );
	$oRootAtt = $oListDom->importNode( $oRootAtt, true );
	$oRootDat = $oListDom->importNode( $oRootDat, true );
	$oRootMac = $oListDom->importNode( $oRootMac, true );
	$oRootElement = $oListDom->importNode( $oRootElement, true );
	
	$oRoot->appendChild( $oRootClass );
	$oRoot->appendChild( $oRootAtt );
	$oRoot->appendChild( $oRootDat );
	$oRoot->appendChild( $oRootMac );
	$oRoot->appendChild( $oRootElement );


	notamHandler::getError( 'addElement', $aoErrors );
	notamHandler::deleteError( 'addElement' );
	$this->addErrorsDom( $oListDom, $aoErrors );


        $aszParam = array( 'host' => $_SERVER[ 'HTTP_HOST' ], 
		  'MESSAGE' => $szMessage, 
		  'selectedMode' => 'addElement' );
        $E =  $_REQUEST[ 'element' ];
        if ( $E != '' )
          {
            //get Element's Classes
            $this->m_oRomaDom->getAddedElementsClasses($E, $aszClasses );
            //get Element's description
            $this->m_oRomaDom->getAddedElementsDescription($E, $szDesc );
            //get Element's description
            $this->m_oRomaDom->getAddedElementsContents($E, $szContents );
            $this->m_oRomaDom->getAddedElementsFullContents($E, $szFullContents );
            $this->m_oRomaDom->getAddedElementsNamespace($E, $szNamespace );
          }
	else
	  {
        	$this->m_oRomaDom->getCustomizationNamespace( $szNamespace );	
	  }
	
	if( is_array( $_SESSION[ 'addElements' ][ 'ERROR' ] ) )
	  {
	    $aszClasses = $_SESSION[ 'addElements' ][ 'ERROR' ][ 'classes' ];
	    $szDesc = $_SESSION[ 'addElements' ][ 'ERROR' ][ 'description' ];
	    $szContents = $_SESSION[ 'addElements' ][ 'ERROR' ][ 'content' ];
	    unset( $_SESSION[ 'addElements' ] );
	  }

	$aszParam[ 'elementName' ] = $E;
	$aszParam[ 'elementDesc' ] = $szDesc;
	if (is_array( $aszClasses) )
	  $aszParam[ 'elementClasses' ] = join( ';', $aszClasses );
	$aszParam[ 'TEIWEB'] =roma_teiweb_server;
	$aszParam[ 'elementContents' ] = $szContents;
	$aszParam[ 'elementFullContents' ] = $szFullContents;
	$aszParam[ 'elementNamespace' ] = $szNamespace;

	$this->applyStylesheet( $oListDom, 'addElements.xsl', $oNewDom, $aszParam, 'addElements'  );

	$oParser->addReplacement( 'lang', $szLanguage );
	$oParser->addReplacement( 'doclang', $szDocLanguage );
	$oParser->addReplacement( 'mode', 'addElements' );
	$oParser->addReplacement ('Version', $_SESSION['Version']);
	$oParser->addReplacement( 'view', 'listElements' );
	$oParser->addReplacement( 'title', $szTitle );
	$oParser->addReplacement( 'template', $oNewDom->SaveHTML() );
        $oParser->Parse( $szTemplate, $szOutput );

	$this->appendOutput( $szOutput );
      }

    /**
     * The "Time to give you a schema" view is done
     * in this function
     */
    private function processCreateSchema( &$szOutput )
      {
	$szTemplate = join( '', file(  roma_templateDir . '/main.tem' ) );
	$szSchemTem = join( '', file(  roma_templateDir . '/createSchema.tem' ) );
	$this->getParser( $oParser );
	
	$oSchemaParser = new parser();
	$this->m_oRomaDom->getCustomizationFilename( $szFilename );
	$this->m_oRomaDom->getCustomizationLanguage( $szLanguage );
	$this->m_oRomaDom->getDocLanguage( $szDocLanguage );	
	$this->m_oRomaDom->getCustomizationTitle( $szTitle );
	$oSchemaParser->addReplacement( 'lang', $szLanguage );
	$oParser->addReplacement( 'doclang', $szDocLanguage );
	$oSchemaParser->addReplacement( 'output', $_REQUEST[ 'output' ] );
	//validate file
	if ( roma_validate_schema && $szFilename !='teilite' && $szFilename != 'tei_tite' )
	  {
	    set_error_handler( array($this, 'schemaValidatorErrorHandler' ) );
	    if ( $this->m_oRomaDom->relaxNGValidate( roma_customization_validator ) )
	      {
		$oSchemaParser->addReplacement( 'validated', 'true' );
	      }
	    else
	      {
		$oSchemaParser->addReplacement( 'validated', 'false' );
		$oSchemaParser->addReplacement( 'validatorMessages', join( '<br>', $this->m_aszErrors ) );
	      }
	    restore_error_handler();
	  }
	else
	  {
	    $oSchemaParser->addReplacement( 'validated', 'true' );
	  }

	$oSchemaParser->Parse( $szSchemTem, $szSchema );
	$oParser->addReplacement( 'lang', $szLanguage );
	$oParser->addReplacement( 'doclang', $szDocLanguage );
	$oParser->addReplacement( 'mode', 'createSchema' );
	$oParser->addReplacement ('Version', $_SESSION['Version']);
	$oParser->addReplacement( 'view', 'createSchema' );
	$oParser->addReplacement( 'title', $szTitle );
	$oParser->addReplacement( 'template', $szSchema );
	$oParser->Parse( $szTemplate, $szOutput );


	if ( $_REQUEST[ 'task' ] != 'create' )
	  $this->appendOutput( $szOutput );
      }

    /**
     * The language customization view is created
     * with this function
     */
    private function processCustomizeLanguage( &$szOutput )
      {
        $szTemplate = join( '', file(  roma_templateDir . '/main.tem' ) );
        $szLangTem = join( '', file(  roma_templateDir . '/customizeLanguage.tem' ) );
	$oLanguageParser = new parser();
	$this->getParser( $oParser );
	$this->m_oRomaDom->getCustomizationLanguage( $szLanguage );
	$this->m_oRomaDom->getDocLanguage( $szDocLanguage );	
	$this->m_oRomaDom->getCustomizationTitle( $szTitle );
	$this->m_oRomaDom->getOddLanguage( $szOddLanguage );
	$oLanguageParser->addReplacement( 'doclang', $szDocLanguage );
	$oLanguageParser->addReplacement( 'oddlang', $szOddLanguage );
	$oLanguageParser->Parse( $szLangTem, $szLango );
	$oParser->addReplacement( 'lang', $szLanguage );
	$oParser->addReplacement( 'doclang', $szDocLanguage);
	$oParser->addReplacement( 'mode', 'customizeLanguage' );
	$oParser->addReplacement ('Version', $_SESSION['Version']);
	$oParser->addReplacement( 'view', 'customizeLanguage' );
	$oParser->addReplacement( 'title', $szTitle );
	$oParser->addReplacement( 'template', $szLango );
        $oParser->Parse( $szTemplate, $szOutput );

	$this->appendOutput( $szOutput );
      }

    /**
     * is responsible for the screen, where you can change an Element
     * inside a module. This function is very similar to the processAddElements
     * function.
     */
    private function processChangeElement( &$szOutput )
      {
        $szTemplate = join( '', file(  roma_templateDir . '/main.tem' ) );
	$this->getParser( $oParser );
	$this->m_oRomaDom->getCustomizationLanguage( $szLanguage );
	$this->m_oRomaDom->getDocLanguage( $szDocLanguage );	
	$this->m_oRomaDom->getCustomizationTitle( $szTitle );

	$oModelClassDom = new domDocument();
	$oModelClassDom->loadXML( join( '', file( roma_xquery_server . 'classes.xql?lang=' . $szDocLanguage ) ) );
	$oRootClass = $oModelClassDom->documentElement;

	$oAttributeDom = new domDocument();
	$oAttributeDom->loadXML( join( '', file( roma_xquery_server . 'attclasses.xql?lang=' . $szDocLanguage ) ) );
	$oRootAtt = $oAttributeDom->documentElement;

	$oDatatypeDom = new domDocument();
	$oDatatypeDom->loadXML( join( '', file(  roma_xquery_server . 'datatypes.xql' ) ) ) ;
	$oRootDat = $oDatatypeDom->documentElement;

	$oMacroDom = new domDocument();
	$oMacroDom->loadXML( join( '', file( roma_xquery_server . 'macros.xql' ) ) );
	$oRootMac = $oMacroDom->documentElement;

	$oListDom = new domDocument();
	$oRoot = $oListDom->appendChild( new domElement( 'addElement' ) );
	$oRootClass = $oListDom->importNode( $oRootClass, true );
	$oRootAtt = $oListDom->importNode( $oRootAtt, true );
	$oRootDat = $oListDom->importNode( $oRootDat, true );
	$oRootMac = $oListDom->importNode( $oRootMac, true );
	
	$oRoot->appendChild( $oRootClass );
	$oRoot->appendChild( $oRootAtt );
	$oRoot->appendChild( $oRootDat );
	$oRoot->appendChild( $oRootMac );

        //
        $this->m_oRomaDom->getClassesByElementNameInModule( $_REQUEST[ 'element' ], $_REQUEST[ 'module' ], $aszClasses );
        $this->m_oRomaDom->getContentsByElementNameInModule( $_REQUEST[ 'element' ], $_REQUEST[ 'module' ], $szContent );
        $this->m_oRomaDom->getDescriptionByElementNameInModule(
        $_REQUEST[ 'element' ], $_REQUEST[ 'module' ], $szDesc );    
	$this->m_oRomaDom->getElementsChangedNameInModule( $_REQUEST[ 'module' ], $_REQUEST[ 'element' ], $szChangedName );


	notamHandler::getError( 'elementChanged', $aoErrors );
	notamHandler::deleteError( 'elementChanged' );
	$this->addErrorsDom( $oListDom, $aoErrors );

        // Tell the stylesheet something about the Element
        $aszParam = array( 'selectedMode' => 'changeElement',
        'headline' => roma_message_headline_changeElement );

        $aszParam[ 'elementName' ] = $_REQUEST[ 'element' ];
        $aszParam[ 'elementClasses' ] = join( ';', $aszClasses );  
        $aszParam[ 'elementContent' ] = $szContent;
        $aszParam[ 'elementDesc' ] = $szDesc;
	$aszParam[ 'TEIWEB'] =roma_teiweb_server;
        $aszParam[ 'elementsModule' ] = $_REQUEST[ 'module' ];
	$aszParam[ 'elementChangedName' ] = $szChangedName;
	$aszParam[ 'module' ] = $_REQUEST[ 'module' ];

	$this->applyStylesheet( $oListDom, 'addElements.xsl', $oNewDom, $aszParam, 'changeElement'  );

	$oParser->addReplacement( 'lang', $szLanguage );
	$oParser->addReplacement( 'doclang', $szDocLanguage );
	$oParser->addReplacement( 'mode', 'changeElement' );
	$oParser->addReplacement ('Version', $_SESSION['Version']);
	$oParser->addReplacement( 'view', 'main' );
	$oParser->addReplacement( 'title', $szTitle );
	$oParser->addReplacement( 'template', $oNewDom->SaveHTML() );
        $oParser->Parse( $szTemplate, $szOutput );

	$this->appendOutput( $szOutput );
      }

    /**
     * This function is responsible for the screen where you can add 
     * a new or change an existing attribute.
     */
    private function processAddAttribute( &$szOutput )
      {
        $szTemplate = join( '', file(  roma_templateDir . '/main.tem' ) );
	$this->getParser( $oParser );
        $this->m_oRomaDom->getDocLanguage($szDocLanguage );

       	$oListDom = new domDocument();
	$this->m_oRomaDom->getCustomizationTitle( $szTitle );
	$oAddAttribute = $oListDom->appendChild ( new domElement( 'addAttribute' ) );
	
	$oDatatypeDom = new domDocument();
	$oDatatypeDom->loadXML( join( '', file( roma_xquery_server . 'datatypes.xql' ) ) ) ;
	$oRootDat = $oDatatypeDom->documentElement;
	$oRootDat = $oListDom->importNode( $oRootDat, true );
	$oAddAttribute->appendChild( $oRootDat );
	
	//attributes
	$this->m_oRomaDom->getAttributeDomByElementInModule( $_REQUEST[ 'element' ], $_REQUEST[ 'module' ], $_REQUEST[ 'class' ],$oAtts );
	$oRootAtts = $oAtts->documentElement;
	$oRootAtts = $oListDom->importNode( $oRootAtts, true );
	$oAddAttribute->appendChild( $oRootAtts );

	//Parameters
	$aszParam = array( 
		  'element' => $_REQUEST[ 'element' ], 
		  'module' => $_REQUEST[ 'module' ], 
		  'class' => $_REQUEST[ 'class' ], 
		  'type' => $_REQUEST[ 'type' ], 
		  'added' => $_REQUEST[ 'added' ] );
	//if attribute specified
	if( $_REQUEST[ 'attribute' ] != '' )
	  {
	    $this->m_oRomaDom->getAttributeDefinition( $_REQUEST[ 'attribute' ], $_REQUEST[ 'element' ], $_REQUEST[ 'module' ], $_REQUEST[ 'class' ], $oAttDom );
	    $oCurrent = $oAddAttribute->appendChild( new domElement( 'currentAttribute' ) );
	    $oTheAttribute = $oListDom->importNode( $oAttDom->documentElement, true );
	    $oCurrent->appendChild( $oTheAttribute );
	  }

	notamHandler::getError( 'addAttribute', $aoErrors );
	notamHandler::deleteError( 'addAttribute' );
	$this->addErrorsDom( $oListDom, $aoErrors );


	$this->applyStylesheet( $oListDom, 'addAttribute.xsl', $oNewDom, $aszParam, 'addAttributes' );

	if ( $_REQUEST[ 'class' ] != '' )
	  {
	    $oParser->addReplacement( 'view', 'changeClasses' );
	  }
	elseif( $_REQUEST[ 'module' ] != '' )
	  {
	    $oParser->addReplacement( 'view', 'main' );
	  }
	else
	  {
	    $oParser->addReplacement( 'view', 'listElements' );
	  }
	$oParser->addReplacement( 'title', $szTitle );
	$oParser->addReplacement( 'lang', $szLanguage );
	$oParser->addReplacement( 'doclang', $szDocLanguage );
	$oParser->addReplacement( 'mode', 'addAttributes' );
	$oParser->addReplacement ('Version', $_SESSION['Version']);
	$oParser->addReplacement( 'template', $oNewDom->SaveHTML() );
        $oParser->Parse( $szTemplate, $szOutput );

	$this->appendOutput( $szOutput );
      }

    /**
     * this function decides whether to print out a list of existing
     * elements (that is, it prints out the list, if there are any) or
     * to get you directly to the processAddElements function
     */
    private function processListAddedElements( &$szOutput )
      {
        //Get created Elements
        if ( $this->m_oRomaDom->getAddedElements( $oElementsDom ) === false )
	  {
	    $szTemplate = join( '', file(  roma_templateDir . '/main.tem' ) );
	    $this->getParser( $oParser );
	    $this->m_oRomaDom->getCustomizationLanguage( $szLanguage );	
	    $this->m_oRomaDom->getDocLanguage( $szDocLanguage );		
	    $this->m_oRomaDom->getCustomizationTitle( $szTitle );

	    $this->applyStylesheet( $oElementsDom, 'listAddedElements.xsl', $oNewDom, array(), 'listElements' );

	    $oParser->addReplacement ('Version', $_SESSION['Version']);
	    $oParser->addReplacement( 'lang', $szLanguage );
	    $oParser->addReplacement( 'doclang', $szDocLanguage );
	    $oParser->addReplacement( 'mode', 'listElements' );
	    $oParser->addReplacement( 'view', 'listElements' );
	    $oParser->addReplacement( 'title', $szTitle );
	    $oParser->addReplacement( 'template', $oNewDom->SaveHTML() );
	    $oParser->Parse( $szTemplate, $szOutput );
	  }
	else
	 {
	   $this->processAddElements( $szOutput );
	 }

	$this->appendOutput( $szOutput );
      }

    /**
     * Does the same thing as processListAddedElements but
     * with Attributes.
     */
    private function processListAddedAttributes( &$szOutput )
      {
	//Get added Attributes
        if ( $this->m_oRomaDom->getAttributeDomByElementInModule(
    $_REQUEST[ 'element' ], $_REQUEST[ 'module' ], $_REQUEST[ 'class'], $oAtts ) === false )
	  {
	    $szTemplate = join( '', file(  roma_templateDir . '/main.tem' ) );
	    $this->getParser( $oParser );

	    $this->m_oRomaDom->getCustomizationTitle( $szTitle );
	    $aszParam = array( 'element' => $_REQUEST[ 'element' ], 'module' => $_REQUEST[ 'module' ], 'class' => $_REQUEST[ 'class' ] );

	    //messages
	    if ( $_SESSION[ 'addAttribute_SUCCESS' ] != '' )
	      {
		$aszParam[ 'MESSAGE' ] = $_SESSION[ 'addAttribute_SUCCESS' ];
		$_SESSION[ 'addAttribute_SUCCESS' ] = '';
	      }

	    //Error
	    if ( $_SESSION[ 'addAttribute_ERROR' ] != '' )
	      {
		$aszParam[ 'ERRORS' ] = $_SESSION[ 'addAttribute_ERROR' ];
		$_SESSION[ 'addAttribute_ERROR' ] = ''; 
	      }

	    $this->applyStylesheet( $oAtts, 'listAddedAttributes.xsl', $oNewDom, $aszParam, 'listAttributes' );


	    if ( $_REQUEST[ 'class' ] != '' )
	      {
		$oParser->addReplacement( 'view', 'changeClasses' );
	      }
	    elseif( $_REQUEST[ 'module' ] != '' )
	      {
		$oParser->addReplacement( 'view', 'main' );
	      }
	    else
	      {
		$oParser->addReplacement( 'view', 'listElements' );
	      }
	    $oParser->addReplacement( 'title', $szTitle );
	    $oParser->addReplacement( 'mode', 'listAttributes' );
	    $oParser->addReplacement ('Version', $_SESSION['Version']);
	    $oParser->addReplacement( 'template', $oNewDom->SaveHTML() );
	    $oParser->Parse( $szTemplate, $szOutput );
	  }
	else
	  {
	    $this->processAddAttribute( $szOutput );
	  }

	$this->appendOutput( $szOutput );
      }

    /**
     * Is responsible for the List of Attributeclasses.
     */
    private function processChangeClasses( &$szOutput )
      {
        $szTemplate = join( '', file(  roma_templateDir . '/main.tem' ) );
	$this->getParser( $oParser );
	$this->m_oRomaDom->getCustomizationLanguage( $szLanguage );
	$this->m_oRomaDom->getDocLanguage( $szDocLanguage );
	$this->m_oRomaDom->getCustomizationTitle( $szTitle );
	$oListDom = new domDocument();
	$oListDom->loadXML( join ( '', file( roma_xquery_server . 'attclasses.xql?lang=' . $szDocLanguage ) ) );

	$this->applyStylesheet( $oListDom, 'changeClasses.xsl',
				$oNewDom, array( 'TEIWEB' => roma_teiweb_server,
						 'doclang' => $szDocLanguage,
						 'class' => $_REQUEST[ 'class' ], 
						 'module' => $_REQUEST[ 'module' ] ), 'changeClasses' );
	$oParser->addReplacement( 'lang', $szLanguage );
	$oParser->addReplacement( 'doclang', $szDocLanguage );
	$oParser->addReplacement( 'mode', 'changeClasses' );
	$oParser->addReplacement ('Version', $_SESSION['Version']);
	$oParser->addReplacement( 'view', 'changeClasses' );
	$oParser->addReplacement( 'title', $szTitle );
	$oParser->addReplacement( 'template', $oNewDom->SaveHTML() );
        $oParser->Parse( $szTemplate, $szOutput );

	$this->appendOutput( $szOutput );
      }

    /**
     * This creates the Documentation view
     */
    private function processCreateDocumentation( &$szOutput )
      {
        $szTemplate = join( '', file(  roma_templateDir . '/main.tem' ) );
        $szDocTem = join( '', file(  roma_templateDir . '/createDocumentation.tem' ) );
	$this->getParser( $oParser );

	$oDocParser = new parser();
	$this->m_oRomaDom->getCustomizationLanguage( $szLanguage );
	$this->m_oRomaDom->getDocLanguage( $szDocLanguage );	
	$this->m_oRomaDom->getCustomizationTitle( $szTitle );
	$oDocParser->addReplacement( 'lang', $szLanguage );
	$oDocParser->addReplacement( 'doclang', $szDocLanguage );
	$oDocParser->addReplacement( 'format', $_REQUEST[ 'format' ] );
	$oDocParser->Parse( $szDocTem, $szDoc );

	$oParser->addReplacement( 'lang', $szLanguage );
	$oParser->addReplacement( 'doclang', $szDocLanguage );
	$oParser->addReplacement ('Version', $_SESSION['Version']);
	$oParser->addReplacement( 'mode', 'createDocumentation' );
	$oParser->addReplacement( 'view', 'createDocumentation' );
	$oParser->addReplacement( 'title', $szTitle );
	$oParser->addReplacement( 'template', $szDoc );
        $oParser->Parse( $szTemplate, $szOutput );

	if ( $_REQUEST[ 'task' ] != 'create' )
	  $this->appendOutput( $szOutput );
      }

    private function processCustomizeCustomization( &$szOutput )
      {
	$szTemplate = join( '', file(  roma_templateDir . '/main.tem' ) );
	$szSchemTem = join( '', file(  roma_templateDir . '/customizeCustomization.tem' ) );
	$this->getParser( $oParser );
	
	$oSchemaParser = new parser();

	$this->m_oRomaDom->getCustomizationTitle( $szTitle );
	$this->m_oRomaDom->getCustomizationAuthor( $szAuthor );
	$this->m_oRomaDom->getCustomizationFilename( $szFilename );
	$this->m_oRomaDom->getCustomizationPrefix( $szPrefix );
	$this->m_oRomaDom->getCustomizationNamespace( $szNamespace );
	$this->m_oRomaDom->getCustomizationLanguage( $szLanguage );
	$this->m_oRomaDom->getCustomizationDescription( $szDesc );
	$this->m_oRomaDom->getDocLanguage( $szDocLanguage );	
	$oSchemaParser->addReplacement( 'lang', $szLanguage );
	$oSchemaParser->addReplacement( 'doclang', $szDocLanguage );
	$oSchemaParser->addReplacement( 'oddlang', $szOddLanguage );
	$oSchemaParser->addReplacement( 'title', $szTitle );
	$oSchemaParser->addReplacement( 'author', $szAuthor );
	$oSchemaParser->addReplacement( 'filename', $szFilename );
	$oSchemaParser->addReplacement( 'prefix', $szPrefix );
	$oSchemaParser->addReplacement( 'language', $szLanguage );
	$oSchemaParser->addReplacement( 'description', $szDesc );
	$oSchemaParser->addReplacement( 'namespace', $szNamespace );
	$oSchemaParser->Parse( $szSchemTem, $szSchema );
	$oParser->addReplacement ('Version', $_SESSION['Version']);
	$oParser->addReplacement( 'lang', $szLanguage );
	$oParser->addReplacement( 'doclang', $szDocLanguage );
	$oParser->addReplacement( 'mode', 'customizeCustomization' );
	$oParser->addReplacement( 'view', 'customizeCustomization' );
	$oParser->addReplacement( 'template', $szSchema );
	$oParser->addReplacement( 'title', $szTitle );
	$oParser->Parse( $szTemplate, $szOutput );

	$this->appendOutput( $szOutput );
      }


    // #####################################################################
    // --- Giving the User something to Save
    // #####################################################################


    private function downloadFile( &$szOutput )
      {
	$this->outputDownloadHeaders();
	header( "Content-Disposition: attachment;  filename=" . $_REQUEST[ 'file' ] );
	
	$szOutput = $_SESSION[ 'download' ];
      }

    /**
     * The customization file is send to the user so he/she can
     * download it and use it as an input file for roma later.
     */
    private function saveCustomization( &$szOutput )
      {
	$this->outputDownloadHeaders();
	$this->m_oRomaDom->getCustomizationFilename( $szFilename );
	$szFilename = ( $szFilename ) ? $szFilename : 'mytei';

	header( "Content-Disposition: attachment;  filename={$szFilename}.xml" );

	//tidy up output
//	$oTidy = new tidy();
//	$aszOptions = array( 'indent' => true,
//			     'indent-spaces' => 4,
//			     'wrap' => 72,
//			     'input-xml' => true,
//			     'output-xml' => true
//			     );
/*     $oTidy->parseString( $this->m_oRomaDom->SaveXML(), $aszOptions );
	$oTidy->cleanRepair();  
	$szOutput = $oTidy;*/
	$szOutput = $this->m_oRomaDom->SaveXML();
      }

    /**
     * Roma creates a customized schema in for the user to download
     * in this function.
     * There are 4 different schema types available.
     * rng
     * xsd
     * dtd
     * rnc
     *
     * Often trang has to be called to convert between schemas.
     * Since trang sometimes crashes without doing anything, there have
     * to be some error messages included in that function.
     */
    private function makeSchema()
      {
	$this->processCreateSchema( $szOut );
	echo $szOut;
	
        $this->m_oRomaDom->getCustomizationFilename( $szFilename );
	$szFilename = ( $szFilename ) ? $szFilename : 'myTei';

	$postFix=  $_REQUEST[ 'output' ] ;
	switch( $_REQUEST[ 'output' ] )
	  {
	  case 'rnc':
	    $szError = $this->m_oRomaDom->createSchemaRNC( $szSchema);
	    break;
	  case 'xsd':
	    $szError = $this->m_oRomaDom->createSchemaXSD( $szSchema,  $szFilename );
	    $postFix = "zip";
	    break;
	  case 'dtd':
	    $szError = $this->m_oRomaDom->createSchemaDTD( $szSchema );
	    break;
	  case 'sch':
	    $szError = $this->m_oRomaDom->createSchemaSCH( $szSchema );
	    break;
	  case 'isosch':
	    $szError = $this->m_oRomaDom->createSchemaISOSCH( $szSchema );
	    break;
	  case 'rng':
	    $szError = $this->m_oRomaDom->createSchemaRNG( $szSchema );
	    break;
	  }
	
	if ( $szSchema != '' )
	  {
	    echo '<pre>' . $szError . '</pre>';

	    $this->redirectBrowserMeta( "http://" . $_SERVER[
	    'HTTP_HOST' ] . $_SERVER[ 'PHP_SELF' ] . '?mode='
	    . roma_mode_downloadFile . '&prefix=' . $szPrefix . '&file=' . $szFilename . '.' . $postFix . '&nextpage=' .  roma_mode_createSchema );	    

	    $_SESSION[ 'download' ] = $szSchema;
	  }
	else
	  {
	    //notam
	    $oNotam = new notam();
	    $oNotam->setHeadline( 'MESSAGES' );
	    $oNotam->setMessage( $szError );
	    $oNotam->setStatus( notam_status_error );
	    $oNotam->addNotam();

	    //notam
	    $oNotam = new notam();
	    $oNotam->setHeadline( 'Schema' );
	    $oNotam->setMessage( 'Could not create schema.' );
	    $oNotam->setStatus( notam_status_error );
	    $oNotam->addNotam();
	    
	    $this->redirectBrowserMeta( "http://" . $_SERVER[ 'HTTP_HOST' ] . $_SERVER[ 'PHP_SELF' ] . '?mode=' . roma_mode_createSchema );	    
	  }
      }

    // #####################################################################
    // --- Get some Documentation

    /**
     * This Function creates a customized Documentation and offers it
     * to the user for download.
     */
    private function generateDocumentation( )
      {
	$this->processCreateDocumentation( $szOut );
	echo $szOut;
	flush();

	// get docDom
//	$oDomDoc = new docDom( $this->m_oRomaDom, true );
	$szEnding = '';

	switch( $_REQUEST[ 'format' ] )
	  {
//	  case 'dvi':
//	    $szError = $this->m_oRomaDom->outputDVI( $szDoc );
//	    $szEnding = 'dvi';
//	    break;
	  case 'pdf':
	    $szError = $this->m_oRomaDom->outputPdfLatex( $szDoc );
	    $szEnding = 'pdf';
	    break;
	  case 'latex':
	    $szError = $this->m_oRomaDom->outputLatex( $szDoc );
	    $szEnding = 'tex';
	    break;
//	  case 'pdf':
//	    $szError = $this->m_oRomaDom->outputPDF( $szDoc );
//	    $szEnding = 'pdf';
//	    break;
	  case 'teiLite':
	    $szError = $this->m_oRomaDom->outputTeiLite( $szDoc );
	    $szEnding = 'xml';
	    break;
	  case 'plain':
	    $szError = $this->m_oRomaDom->outputPlain( $szDoc );
	    $szEnding = 'xml';
	    break;
	  case 'html':
	  default:
	    $szError = $this->m_oRomaDom->outputHTML( $szDoc );
	    $szEnding = 'html';
	    break;	    
	  }

	if ( $szError == '' )
	  {
	    $this->m_oRomaDom->getCustomizationFilename( $szFilename );
	    $szFilename = ( ( $szFilename ) ? $szFilename : 'mytei' ) . '_doc';

	    $this->redirectBrowserMeta( "http://" . $_SERVER[ 'HTTP_HOST' ] . $_SERVER[ 'PHP_SELF' ] . '?mode=' . roma_mode_downloadFile . '&file=' . $szFilename . '.' . $szEnding );	    

	    $_SESSION[ 'download' ] = $szDoc;
	  }
      }

    private function outputDownloadHeaders()
      {
	header( "Pragma: public");
	header( "Expires: Mon, 26 Jul 1997 05:00:00 GMT" );
	header( "Last-Modified: " . gmdate("D, d M Y H:i:s") . " GMT" );

	header( 'Cache-Control: must-revalidate, post-check=0, pre-check=0');
	header( "Cache-Control: post-check=0, pre-check=0", false );

	header( "Content-Type: application/octet-stream" );
      }
    

    // #####################################################################
    // --- Stylesheets and formatting
    // #####################################################################

    /**
     * this function gets a domDocument object, a stylesheet name, a parameter where to
     * put the new domDocument object in, and an associative array with paramters for
     * the xslt processor.
     * It then loads the stylesheet which has to be inside the 'roma_localStylesheetDir' and
     * creates an XSLT-Processor. Then the XSLT-Processor is fed with the paramters specified
     * and finally the stylesheet is parsed, returning the output to the third parameter.
     */
    private function applyStylesheet( $oDom, $szStylesheet, &$oNewDom, $aszParams = array(), $szRessource = '' )
      {
        $oXSL = new domDocument();
 	$oXSL->loadXML( join( '', file(  roma_localStylesheetDir . '/' . $szStylesheet ) ) );

	$oProc = new XsltProcessor();
	$oProc->importStylesheet( $oXSL );
	
	if ( $szRessource != '' )
	  {
	    //reading resource File
	    $oRessource = new resource( roma_resource_path . '/' . $szRessource );
	    $oRessource->getStringArray( $this->m_szLanguage, $aszRessources );
	    $aszParams = array_merge( $aszParams, $aszRessources );
	  }

        if ( is_array( $aszParams ) )
          {
  	    foreach( $aszParams as $key => $value )
              {
   	        $oProc->setParameter( null, $key, $value );
  	      }
          }
	$oNewDom = $oProc->transformToDoc( $oDom );
      }



    // #####################################################################
    // --- Changing Stuff
    // #####################################################################

    public function changeListAddedElements()
      {
	foreach( $_REQUEST as $key => $value )
	  {
	    if( substr( $key, 0, 8 ) == 'include_' )
	      {
		switch( $value )
		  {
		  case 'yes':
		    $this->m_oRomaDom->includeAddedElement( substr( $key, 8 ) );
		    break;
		  case 'no':
		    $this->m_oRomaDom->excludeAddedElement( substr( $key, 8 ) );
		    break;
		  }
	      }
	  }
      }  

    public function changeListAddedAttributes()
      {
	foreach( $_REQUEST as $key => $value )
	  {
	    if( substr( $key, 0, 8 ) == 'include_' )
	      {
		switch( $value )
		  {
		  case 'yes':
		    $this->m_oRomaDom->includeAttributeInElement( substr( $key, 8 ), $_REQUEST[ 'class' ], $_REQUEST[ 'module' ], $_REQUEST[ 'element' ] );
		    break;
		  case 'no':
		    $this->m_oRomaDom->excludeAttributeInElement( substr( $key, 8 ), $_REQUEST[ 'class' ], $_REQUEST[ 'module' ], $_REQUEST[ 'element' ] );
		    break;
		  }
	      }
	    if( substr( $key, 0, 5 ) == 'name_' )
	      {
	      $ATTNAME = substr( $key, 5 );
	      if ($ATTNAME != $value) {
		$this->m_oRomaDom->changeAttributesName($ATTNAME,
      $value, $_REQUEST[ 'class' ], $_REQUEST[ 'module' ], $_REQUEST[
      'element' ] );
           }
	      }
	  }
	$this->m_oRomaDom->checkForEmpty ($_REQUEST['element'],$_REQUEST['class']);

      }

    public function elementChanged() {
      //print_r($_REQUEST);
		$errResult = false;
		// element name
        if ( $_REQUEST[ 'name' ] != $_REQUEST[ 'newName' ] )
          {
	    try {
	      $this->m_oRomaDom->changeElementNameInModule( $_REQUEST[ 'name' ], $_REQUEST[ 'newName' ], $_REQUEST[ 'module' ] );
	    }
	    catch( falseTagnameException $e )
	      {
		$e->addError( 'elementChanged', 'name' );
		//notam
		$oNotam = new notam();
		$oNotam->setHeadline( 'Illegal name' );
		$oNotam->setMessage(  roma_message_elementNameError );
		$oNotam->setStatus( notam_status_error );
		$oNotam->addNotam();

		$errResult = true;
	      }
          }

// namespace
	$this->m_oRomaDom->changeElementNamespaceInModule( $_REQUEST[ 'name' ], $_REQUEST[ 'elementNamespace' ] );
	

// description
        if ($_REQUEST[ 'changedDesc' ] == 'true')  {
	$this->m_oRomaDom->changeElementDescInModule( $_REQUEST[ 'name' ], $_REQUEST[ 'module' ], $_REQUEST[ 'description' ] );
	
	 }

// class membership
   if ($_REQUEST[ 'changedClasses' ] == 'true' ) {
	$aszClasses = array();
	foreach( $_REQUEST as $key => $value )
	  {
	    if ( substr( $key, 0, 6 ) == 'class|' )
	      {
		$aszClasses[$value] = "add";
	      }
	  }
	$originals = explode(" ", $_REQUEST ['originalClasses' ]);
	foreach ($originals as $oclass) {
	
	if ($oclass == '') { }
	elseif (array_key_exists($oclass,$aszClasses)) {

     	     if ($aszClasses[$oclass] == "add") {
		$aszClasses[$oclass] = "replace";
	    }
        }	    
	else {
		$aszClasses[$oclass] = "delete";
	     }
         }
	//print_r($aszClasses);
	$this->m_oRomaDom->replaceElementsClassesInModule( 
	$_REQUEST['name' ], 
	$_REQUEST[ 'module' ], 
	$aszClasses);

}

// content model
        if ($_REQUEST[ 'changedContent' ] == 'true')  {

	try {
	  $this->m_oRomaDom->changeElementContentsInModule( $_REQUEST[ 'name' ], $_REQUEST[ 'module' ], $_REQUEST[ 'content' ] );
	}
	catch( falseContentsException $e )
	  {
	    $e->addError( 'elementChanged', 'contents' );

	    //notam
	    $oNotam = new notam();
	    $oNotam->setHeadline( 'Illegal Content' );
	    $oNotam->setMessage(  roma_message_elementContentError );
	    $oNotam->setStatus( notam_status_error );
	    $oNotam->addNotam();

	    $errResult = true;
	  }

	  }

	if( !  $errResult )
	  {
	    //notam
	    $oNotam = new notam();
	    $oNotam->setHeadline( 'Element ' . $_REQUEST[ 'name' ] . ' changed' );
	    $oNotam->setMessage( roma_message_changedElement );
	    $oNotam->setStatus( notam_status_success );
	    $oNotam->addNotam();
	  }

	return $errResult;

      }


    private function moduleChanged()
      {
        $errResult = false;
	$module = $_REQUEST[ 'module' ];
        $excludeList="";
        $includeList="";
	foreach( $_REQUEST as $key => $value )
	  {
	   if ( substr( $key, 0, 8 ) == 'element_' && $value == 'exclude' )
	      {
	       $excludeList = $excludeList . substr($key,8) . " ";
	      }
	   elseif ( substr( $key, 0, 8 ) == 'element_' && $value == 'include' )
	      {
	       $includeList = $includeList . substr($key,8) . " ";
	      }
	   elseif( substr( $key,0, 12 ) == 'elementName_' && ! isset( $_REQUEST[ 'element_' . $value ] ) )
	     {
	       try {
		 $this->m_oRomaDom->changeElementNameInModule( substr( $key, 12 ), $value, $_REQUEST[ 'module' ] );
	       }
	       catch ( falseTagnameException $e )
		 {
		   $e->addError( 'moduleChanged', 'name' );
		   $errResult = true;
		 }
	     }
	   else
	     {    }
	  }

	$this->m_oRomaDom->setElementsInModule($module,trim($includeList),trim($excludeList));
	
	if (! $errResult )
	  {
 	    //notam
	    $oNotam = new notam();
	    $oNotam->setHeadline( 'Module changed' );
	    $oNotam->setMessage( $_REQUEST[ 'module' ] . ' was successfully changed.' );
	    $oNotam->setStatus( notam_status_success );
	    $oNotam->addNotam();
          }
	else
	  {
 	    //notam
	    $oNotam = new notam();
	    $oNotam->setHeadline( 'Module changed' );
	    $oNotam->setMessage( 'There are errors in your customization.' );
	    $oNotam->setStatus( notam_status_error );
	    $oNotam->addNotam();
          }

	return $errResult;
      }


    private function removeModule()
      {
	if ( $this->m_oRomaDom->removeModule( $_REQUEST[ 'module' ] ) === false )
	  {
 	    //notam
	    $oNotam = new notam();
	    $oNotam->setHeadline( 'Module removed' );
	    $oNotam->setMessage( $_REQUEST[ 'module' ] . ' was successfully removed.' );
	    $oNotam->setStatus( notam_status_success );
	    $oNotam->addNotam();
	  }
      }


    private function addModule()
      {
	if ( $this->m_oRomaDom->addModule( $_REQUEST[ 'module' ] ) === false )
	  {
	    //notam
	    $oNotam = new notam();
	    $oNotam->setHeadline( 'Module added' );
	    $oNotam->setMessage(  $_REQUEST[ 'module' ] . ' was successfully added.' );
	    $oNotam->setStatus( notam_status_success );
	    $oNotam->addNotam();
	  }
      }


    private function customizedCustomization()
      {
	$this->m_oRomaDom->setCustomizationTitle( $_REQUEST[ 'title' ] );
	$this->m_oRomaDom->setCustomizationNamespace( $_REQUEST[ 'namespace' ] );
	$this->m_oRomaDom->setCustomizationFilename( $_REQUEST[ 'filename' ] );
	$this->m_oRomaDom->setCustomizationPrefix( $_REQUEST[ 'prefix' ] );
	$this->m_oRomaDom->setCustomizationLanguage( $_REQUEST[ 'lang' ] );
	$this->m_oRomaDom->setCustomizationAuthor( $_REQUEST[ 'author' ] );
	$this->m_oRomaDom->setCustomizationDescription( $_REQUEST[ 'description' ] );
      }


    // #########################################################################
    // --- get Error messages
    // #########################################################################

    function schemaValidatorErrorHandler($errno, $errmsg, $filename, $linenum, $vars) 
    {
      echo $errmsg . '<br>';
      $this->m_aszErrors[] = $errmsg;
    }

}
?>
