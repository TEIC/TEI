<script language="php">
// ######################################################################
//
// Path: TEIARNO/Roma/romaDom.php
//
// ######################################################################


// ######################################################################
// --- REQUIRES
// ######################################################################


// ######################################################################
// --- CONSTANTS
// ######################################################################


/**
 * @author: Arno Mittelbach <arno@mittelbach-online.de>
 * @version: 0.1
 * @access:  public
 * @package: roma
 */
class romaDom extends domDocument 
  {
     


    //Constructor
    function __construct()
      {
        parent::__construct();
      }

    

    public function GetAllModules( &$oDomModules )
      {
        $aszModules = array();
        $aszModulesKeys = array();

        $aoModules = $this->getElementsByTagName( 'Tagset' );

	$oDomModules = new domDocument();
        $oDomModules->appendChild( new domElement( 'teiModulesList' ) );

	$oRoot = $oDomModules->documentElement;

        foreach( $aoModules as $oModule )
          {
	    //Get description

            $aszModulesKeys[ $oModule->nodeValue ] = 'empty description'; 
          }

        foreach( $aszModulesKeys as $szModule => $szDesc )
          {
            $oM = $oRoot->appendChild( new domElement( 'teiModule' ) );
	    $oMName = $oM->appendChild( new domElement( 'moduleName' ) );
            $oMName->appendChild( new domText( $szModule ) );
            $oMDesc = $oM->appendChild( new domElement( 'moduleDesc' ) );
            $oMDesc->appendChild( new domText( $szDesc ) );
          }
      }

    public function GetElementsOfModule( $szModule, &$oDomElements )
      {
	$oDomElements = new domDocument();
        $oDomElements->appendChild( new domElement( 'teiElementList' ) );
	$oRoot = $oDomElements->documentElement;
	$oRoot->setAttribute(  'module', $szModule  );

        $oXPath = new domxpath( $this );
        $aoElements = $oXPath->query( "/descendant::Tagset[child::text()='$szModule']/parent::Tag" );

	foreach( $aoElements as $oElement )
          { 
	    $oIdent = $oElement->getElementsByTagName( 'ident' );
	    $oDesc = $oElement->getElementsByTagName( 'Desc' );

	    $oE = $oRoot->appendChild( new domElement( 'teiElement' ) );
            $oEName = $oE->appendChild( new domElement( 'elementName' ) );
            $oEName->appendChild( new domText( $oIdent->item(0)->nodeValue ) );
            $oEDesc = $oE->appendChild( new domElement( 'elementDesc' ) );
            $oEDesc->appendChild( new domText( $oDesc->item(0)->nodeValue ) );
          }

      }
  }

</script>