<script language="php">
// ######################################################################
//
// Path: /usr/local/php/includes/notam/notamHandler.php
//
// ######################################################################

// ######################################################################
// --- REQUIRES
// ######################################################################

// ######################################################################
// --- CONSTANTS
// ######################################################################

define( 'notam_stylesheetDir', 'notam/stylesheets' );
define( 'notam_stylesheet_notam2html', 'notam2html.xsl' );


define( 'notam_status_success', 'success' );
define( 'notam_status_error', 'error' );

/**
 * @author: Arno Mittelbach <arno@mittelbach-online.de>
 * @version: 0.1
 * @access:  public
 * @package: notam
 */
class notamHandler extends domDocument
  {
    private $m_oNotamList;
    private $m_oErrorList;

    function __construct( $szXML = '' )
      {
	parent::__construct();
	if ( $szXML != '' )
	  {
	    $this->loadXML( $szXML );
	    $this->m_oNotamList = $this->getElementsByTagname( 'notamList' )->item(0);
	    if ( ! is_object( $this->m_oNotamList ) )
	      {
		$this->m_oNotamList = $this->documentElement->appendChild( new domElement( 'notamList' ) );
	      }
	    $this->m_oErrorList = $this->getElementsByTagname( 'errorList' )->item(0);
	    if ( ! is_object( $this->m_oErrorList ) )
	      {
		$this->m_oErrorList = $this->documentElement->appendChild( new domElement( 'errorList' ) );
	      }
	  }
	else
	  {
	    $oNotamHandler = $this->appendChild( new domElement( 'notamHandler' ) );
	    $this->m_oNotamList = $oNotamHandler->appendChild( new domElement( 'notamList' ) );
	    $this->m_oErrorList = $oNotamHandler->appendChild( new domElement( 'errorList' ) );
	  }
      }


    // ########################################################
    // --- static functions to operate NotamHandler
    // ########################################################
    /**
     * Function to create Notamobject
     */
    static function getNotamObject( &$oObject )
    {
	$oObject = new notamHandler( $_SESSION[ 'NOTAM' ] );
      }

    /**
     * Function called to add a notam
     * is called from within the notam class
     */
    static function addNotam( $oNotam )
      { 
	notamHandler::getNotamObject( $oNotamHandler );
	$oNotam = $oNotamHandler->importNode( $oNotam, true );
	$oNotamHandler->m_oNotamList->appendChild( $oNotam );

	$_SESSION[ 'NOTAM' ] = $oNotamHandler->SaveXML();
      }

    /**
     * returns an Array with Notams
     */
    static function getNotams( &$aoNotam )
      {
	notamHandler::getNotamObject( $oNotamHandler );

	$aoNotam = array();
	foreach( $oNotamHandler->m_oNotamList->childNodes as $oNotam )
	  { 
	    $oTmpDom = new domDocument;
	    $oNotam = $oTmpDom->importNode( $oNotam, true );
	    $oTmpDom->appendChild( $oNotam );
	    $aoNotam[] = new notam( $oTmpDom->SaveXML() );
	  }
      }

    static function deleteNotams( )
      {
	notamHandler::getNotamObject( $oNotamHandler );
	$oNotamHandler->documentElement->removeChild( $oNotamHandler->m_oNotamList );
	$_SESSION[ 'NOTAM' ] = $oNotamHandler->SaveXML();
      }


    // ########################################################
    // --- errorHandling
    // ########################################################

    static function addError( $oError )
      {
	notamHandler::getNotamObject( $oNotamHandler );
	$oError = $oNotamHandler->importNode( $oError, true );
	$oNotamHandler->m_oErrorList->appendChild( $oError );
	
	$_SESSION[ 'NOTAM' ] = $oNotamHandler->SaveXML();
      }

    static function getError( $szError, &$aoErrors )
      {
	notamHandler::getNotamObject( $oNotamHandler );
        $oXPath = new domxpath( $oNotamHandler );
	$aoErrors = $oXPath->query( "//notamHandler/errorList/error[child::name[node()='{$szError}']]" );
      }

    static function deleteError( $szError )
      {
	notamHandler::getNotamObject( $oNotamHandler );
        $oXPath = new domxpath( $oNotamHandler );
	$aoErrors = $oXPath->query( "//notamHandler/errorList/error[child::name[node()='{$szError}']]" );

	foreach( $aoErrors as $oError )
	  {
	    $oNotamHandler->m_oErrorList->removeChild( $oError );
	  }

	$_SESSION[ 'NOTAM' ] = $oNotamHandler->SaveXML();
      }



    // ########################################################
    // --- Output
    // ########################################################

    static function outputHTML( &$szHTML )
      {
	notamHandler::getNotamObject( $oNotamHandler );

	$oXSL = new domDocument();
 	$oXSL->loadXML( join( '', file(  notam_stylesheetDir . '/' . notam_stylesheet_notam2html ) ) );

	$oProc = new XsltProcessor();
	$oProc->importStylesheet( $oXSL );

	$szHTML = chop( $oProc->transformToDoc( $oNotamHandler )->saveHTML() );
      }

  }


class error extends domDocument
  {
    
    private $m_oError;
    
    function __construct( $szXML = '' )
      {
	parent::__construct();
	if ( $szXML == '' )
	  $this->m_oError = $this->appendChild ( new domElement( 'error' ) );
	else
	  {
	    $this->loadXML( $szXML );
	    $this->m_oError = $this->documentElement;
	  }

      }

    public function addError()
      {
	notamHandler::addError( $this->m_oError );
      }

    // ########################################################
    // --- add Properties to Notam
    // ########################################################

    public function setName( $szName )
      {
	$this->m_oError->appendChild( new domElement( 'name', $szName ) );
      }

    public function setLocation( $szLocation )
      {
	$this->m_oError->appendChild( new domElement( 'location', $szLocation ) );
      }

    public function setOldValue( $szOldValue )
      {
	$this->m_oError->appendChild( new domElement( 'oldValue', $szOldValue ) );
      }

    public function setValue( $szValue )
      {
	$this->m_oError->appendChild( new domElement( 'value', $szValue ) );
      }
  }  


class notam extends domDocument
  {
    
    private $m_oNotam;
    
    function __construct( $szXML = '')
      {
	parent::__construct();
	if ( $szXML == '' )
	  $this->m_oNotam = $this->appendChild ( new domElement( 'notam' ) );
	else
	  {
	    $this->loadXML( $szXML );
	    $this->m_oNotam = $this->documentElement;
	  }

      }

    public function addNotam()
      {
	notamHandler::addNotam( $this->m_oNotam );
      }
    


    // ########################################################
    // --- add Properties to Notam
    // ########################################################


    function setHeadline( $szHeadline )
      {
	$this->m_oNotam->appendChild( new domElement( 'headline', $szHeadline ) );
      }

    function setMessage( $szMessage )
      {
	$this->m_oNotam->appendChild( new domElement( 'message', $szMessage ) );
      }

    function setStatus( $szStatus )
      {
	$this->m_oNotam->appendChild( new domElement( 'status', $szStatus ) );
      }



    // ########################################################
    // --- get Properties out of Notam
    // ########################################################

    function getHeadline( &$szHeadline )
      {
	$szHeadline = $this->getElementsByTagname( 'headline' )->item(0)->nodeValue;
      }

    function getMessage( &$szHeadline )
      {
	$szHeadline = $this->getElementsByTagname( 'message' )->item(0)->nodeValue;
      }
  }  

</script>
