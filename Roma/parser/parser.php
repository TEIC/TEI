<script language="php">
// ######################################################################
//
// Path: /usr/local/php/includes/parser/parser.php
//
// ######################################################################

// ######################################################################
// --- REQUIRES
// ######################################################################

// ######################################################################
// --- CONSTANTS
// ######################################################################


/**
 * This is a very simple Parser. You can specifiy Replacements by calling
 * addReplacement. If you are done you call the Parse function. The Parser
 * looks for the specified replacements inside every Tag ({TAG}). 
 * If it matches something. It replaces it.
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
		     $szParsed .= $this->m_aszReplacement[ substr( $szTemplate, $nParsedPos, $nPos - $nParsedPos ) ];
		     $nParsedPos = $nPos + 1;
		   }
	       }
	   }
	 
	 return $errResult;
       }


     public function ParseOld( $szTemplate, &$szParsed )
       {
	 $errResult = false;

	 while( preg_match( '/{([^}]*)}/', $szTemplate, $aMatches ) )
           {
             $szTemplate = preg_replace( '/' . $aMatches[0] . '/', $this->m_aszReplacement[ $aMatches[1] ], $szTemplate );
           }
	 $szParsed = $szTemplate;
	 
	 return $errResult;
       }
  }