<script language="php">
// ######################################################################
//
// Path: TEIARNO/Roma/roma.php
//
// ######################################################################


// ######################################################################
// --- REQUIRES
// ######################################################################

require_once( 'roma/romadom.php' );

// ######################################################################
// --- CONSTANTS
// ######################################################################

define( 'roma_stylesheetDirectory', '/usr/local/php/includes/roma/stylesheets' );

/**
 * @author: Arno Mittelbach <arno@mittelbach-online.de>
 * @version: 0.1
 * @access:  public
 * @package: roma
 */
class roma
  {
    
    var $m_oRomaDom;

    //Constructor
    function __construct()
      {
        //Load romaDom
        $this->m_oRomaDom = new romaDom();
	$this->m_oRomaDom->Load( 'http://www.tei-c.org.uk/tei-bin/files.pl?name=tags.xml' );
      }

    // #####################################################################
    // --- Decide which mode to use
    // #####################################################################

    public function process()
      {
        //Check which mode I am in
 	switch( $_REQUEST[ 'mode' ] )
	  {
	    case 'evaluateMain':
              $this->processEvaluateMain( $szOutput );
 	      break;
	    case 'main':
	    default:
	      $this->processMain( $szOutput );
          }

	echo $szOutput;	
      }


    // #####################################################################
    // --- Different process Functions
    // #####################################################################
    
    protected function processMain( &$szOutput )
      {
	// Get a List of all the Modules
	$this->m_oRomaDom->GetAllModules( $oDomModules );
	$this->applyStylesheet( $oDomModules, 'main.xsl', $oNewDom );
	$szOutput = $oNewDom->SaveHTML();
      }

    protected function processEvaluateMain( &$szOutput )
      {
	// parse arguments
	if ( $_REQUEST[ 'changeTagset' ] )
          $this->changeTagset( $szOutput );
      }


    // #####################################################################
    // --- Process' Helper
    // #####################################################################

    protected function changeTagset( &$szOutput )
      {
	$oChange = new domDocument();
	$oChange->appendChild( new domElement( 'root' ) );
        $oRoot = $oChange->documentElement;

	foreach( $_REQUEST as $key => $value )
          {
	    if( substr( $key, 0, 7 ) == 'module_' )
              {
	        $this->m_oRomaDom->GetElementsOfModule( substr( $key, 7 ), $oElements );
		$oE = $oElements->documentElement;
		$oE = $oChange->importNode( $oE, true );
		$oRoot->appendChild( $oE );
              }
          }
	$this->applyStylesheet( $oChange, 'changeTagset.xsl', $oNewDom );
	$szOutput = $oNewDom->SaveHTML();
      }
  

    // #####################################################################
    // --- Apply Stylesheet
    // #####################################################################

 
    protected function applyStylesheet( $oDom, $szStylesheet, &$oNewDom )
      {
        $oXSL = new domDocument();
 	$oXSL->load( roma_stylesheetDirectory . '/' . $szStylesheet );
	
	$oProc = new XsltProcessor();
	$oProc->importStylesheet( $oXSL );
        $oProc->setParameter( null, 'action', $_SERVER[ 'SCRIPT_NAME' ] );

	$oNewDom = $oProc->transformToDoc( $oDom );
      }

  }
</script>