<script language="php">
// ######################################################################
//
// Path: /usr/local/php/includes/parser/parser.php
//
// ######################################################################

// ######################################################################
// --- REQUIRES
// ######################################################################

require_once( 'resource/resource.php' );

// ######################################################################
// --- CONSTANTS
// ######################################################################


/**
 * This is a very simple Parser. You can specifiy Replacements by calling
 * addReplacement. If you are done you call the Parse function. The Parser
 * looks for the specified replacements inside every Tag ({TAG}). 
 * If it matches something. It replaces it.
 *
 * Parser now supports IF and RES statements
 * You can now have variables in Parserstatements
 * 
 *
 * @author: Arno Mittelbach <arno@mittelbach-online.de>
 * @version: 0.1
 * @access:  public
 * @package: parser
 */
class parser
  {
    /**
     * the replacements are stored inside that array
     */
    private $m_aszReplacement;

    /**
     * The Parser does not need a constructor yet 
     */
     function __construct()
       {}

     // ######################################################################
     // --- Replacements
     // ######################################################################
  
    /**
     * You can give the parser replacements by calling this function.
     * 'false' is returned on success.
     */
     public function addReplacement( $szReplacement, $szValue )
       {
	 $errResult = false;

         $this->m_aszReplacement[ $szReplacement ] = $szValue;
	 
	 return $errResult;
       }

    public function addReplacementArray( $aszReplacement )
      {
	if ( is_array( $aszReplacement ) ) 
	  {
	    foreach ( $aszReplacement as $key => $value )
	      {
		$this->addReplacement( $key , $value );
	      }
	  }
      }  

     // ######################################################################
     // --- Commands
     // ######################################################################

    private function executeCommand( $szCommand, $szTask, $szSubstring, &$szParsed, &$nNewPos )
      {
	switch ( $szCommand )
	  {
	    case 'if':
  	      $this->executeIf( $szTask, $szSubstring, $szParsed, $nNewPos );
	      break;    
	    case 'res':
	      $this->executeRes( $szTask, $szParsed );
 	      break;
	  }
      }
    
    private function executeRes( $szTask, &$szParsed )
      {
	list( $szPath, $szLang, $szString ) = explode( '|', $szTask );

	//reading resource File
	$oRessource = new resource( $szPath );
	$oRessource->getString( $szLang, $szString, $szParsed );
      }
   

    private function executeIf( $szIf, $szSubstring, &$szParsed, &$nNewPos )
      {
	$szParsed = '';

	//get Endif
	$nPos = strpos( $szSubstring, '{endif}' );
	if ( ! $nPos )
	  {
	    throw new Exception();
	  }
	
	if ( strstr( $szIf, '=' ) != '' )
	  {
	    list( $szRep, $szString ) = explode( '=', $szIf );
	    if ( $this->m_aszReplacement[ $szRep ] == $szString )
	      {
		$oParser = new parser();
		$oParser->addReplacementArray( $this->m_aszReplacement );
		$oParser->parse( substr( $szSubstring, 0, $nPos ), $szParsed );
	      }
	    $nNewPos += $nPos;
	  }
      }

     // ######################################################################
     // --- Parse
     // ######################################################################

    /**
     * The first parameter of this function is the string you want the parser
     * to parse. The second parameter is the parser's output (pass by reference).
     * 'false' is returned on success.
     */
     public function Parse( $szTemplate, &$szParsed )
       {
	 $errResult = false;

	 $nInputLength = strlen( $szTemplate );
	 $nParsedPos = 0;
	 $szParsed = '';
	 while ( $nParsedPos < $nInputLength )
  	   {
	     $nPos = strpos( $szTemplate, '{', $nParsedPos );
	     if ( $nPos === false )
	       {
		 $szParsed .= substr( $szTemplate, $nParsedPos );
		 $nParsedPos = $nInputLength;
	       }
	     else
	       {
		 $szParsed .= substr( $szTemplate, $nParsedPos, $nPos - $nParsedPos );
		 $nParsedPos = $nPos + 1;
		 
		 $nPos = strpos( $szTemplate, '}', $nParsedPos );
		 if ( $nPos === false )
		   {
		     $szParsed .= substr( $szTemplate, $nParsedPos );
		     $nParsedPos = $nInputLength;
		   }
		 else
		   {
		     $szReplacement = substr( $szTemplate, $nParsedPos, $nPos - $nParsedPos );

		     //replace Variables
		     preg_match_all( '/\$\[([^\]]*)\]/', $szReplacement, $aMatches );
		     for ( $i = 0; $i < count( $aMatches[ 0 ] ); $i++ )
		       {
			 $szReplacement = preg_replace( '/\$\[' .$aMatches[ 1 ][ $i ] . '\]/', $this->m_aszReplacement[ $aMatches[1][$i] ], $szReplacement );
		       }


		     list( $szCommand, $szTask ) = explode( ':', $szReplacement );
		     if ( $szCommand != '' && $szTask != '' )
		       {
			 $this->executeCommand( $szCommand, $szTask, substr( $szTemplate, $nPos + 1), $szPreParsed, $nPos );
			 $szParsed .= $szPreParsed;
		       }
		     else
		       {
			 $szParsed .= $this->m_aszReplacement[ $szReplacement ];
		       }
		     $nParsedPos = $nPos + 1;
		   }
	       }
	   }
	 
	 return $errResult;
       }
  }