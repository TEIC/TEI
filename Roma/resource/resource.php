<script language="php">
// ######################################################################
//
// Path: /usr/local/php/includes/resource/resource.php
//
// ######################################################################

// ######################################################################
// --- REQUIRES
// ######################################################################

// ######################################################################
// --- CONSTANTS
// ######################################################################


/**
 *
 * @author: Arno Mittelbach <arno@mittelbach-online.de>
 * @version: 0.1
 * @access:  public
 * @package: resource
 */
class resource
  {
    
    private $m_aszRessource;

    function __construct( $szPath )
      {
	$this->readFile( $szPath );
      }

    private function readFile( $szPath )
      {
	$aszFile = file( $szPath . '.res' );
	
	$szLang = '';

	foreach( $aszFile as $szLine )
	  {
	    switch( substr( $szLine, 0, 1 ) )
	      {
	      case '#':
		next;
		break;
	      case '[':
		$szLang = substr( $szLine, 1, strpos( $szLine, ']' ) - 1);
		break;
	      default:
		list( $key, $value ) = explode( '=', $szLine, 2 );
		$this->m_aszRessource[ $szLang ][ $key ] = $value;
		break;
	      }
	  }
      }

    public function getString( $szLang, $szString, &$szReturn )
      {
	$szReturn = $this->m_aszRessource[ $szLang ][ $szString ];
	if ( $szReturn == '' )
 	  $szReturn = $this->m_aszRessource[ 'en' ][ $szString ];
      }

    public function getStringArray( $szLang, &$aszArray )
      {
	$aszArray = array();

	$aszTempArray = $this->m_aszRessource[ $szLang ];
		 
	if ( is_array( $aszTempArray ) )
	  {
	    foreach( $aszTempArray as $key => $value )
	      {
		$aszArray[ 'res_' . $key ] = $value;
	      }
	  }
      }
  }
