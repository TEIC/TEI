<script language="php">
// ######################################################################
//
// Path: /usr/local/includes/php/roma/docdom.php
//
// ######################################################################

/**
 * This class is responsible for Romas documentation file
 *
 * @author: Arno Mittelbach <arno@mittelbach-online.de>
 * @version: 0.9
 * @access:  public
 * @package: roma
 */
class docDom extends domDocument
  {
    private $m_oRomaDom;

    private $bBar;

    function __construct( $oRomaDom, $bBar = false )
      {
	parent::__construct();

	$this->m_oRomaDom = $oRomaDom;
	$this->bBar = $bBar;
	if ( $this->bBar )
	  {
	    $this->m_oRomaDom->loadProgressBar();
	    $this->m_oRomaDom->updateProgressBar( '1' );
	  }
	//create the XSLT to expand the modules
        $oXSL = new domDocument();
 	$oXSL->load( roma_StylesheetDir . '/odds/subsetGuidelines.xsl'  );

	$oProc = new XsltProcessor();
	$oProc->importStylesheet( $oXSL );
	
	$oTei = $oProc->transformToDoc( $oRomaDom );

	  $this->m_oRomaDom->updateProgressBar( '2' );

// now run the expansion

       $this->appendChild(
            $this->importNode($oTei->documentElement, true));
	if ( $this->bBar )
//	$this->parseCustomization();
	if ( $this->bBar )
	  $this->m_oRomaDom->updateProgressBar( '60' );
      }

    private function constructDocument()
      {
	$szXML = '<TEI xmlns="http://www.tei-c.org/ns/1.0" xmlns:rng="http://relaxng.org/ns/structure/1.0" xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0">
  <text>
    <front>
      <divGen type="toc"/>
    </front>
    <body>
    </body>
    <back>
      <div0>
        <head>ODD SUBSET</head>
        <div1 id="REFCLA">
          <head>Class catalogue</head>
          <divGen type="classcat"/>
          <p/>
        </div1>
        <div1 id="REFENT">
          <head>Pattern catalogue</head>
          <divGen type="patterncat"/>
          <p/>
        </div1>
        <div1 id="REFTAG">
          <head>Element catalogue</head>
          <divGen type="tagcat"/>
        </div1>
      </div0>
    </back>
  </text>
</TEI>';

	$szXML = '<TEI xmlns="http://www.tei-c.org/ns/1.0"
	xmlns:rng="http://relaxng.org/ns/structure/1.0"
	xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"/>';
	$this->loadXML( $szXML );
      }

    /**
     * This is the heart of the domDoc class.
     * Here the customization file is parsed and the
     * documentation Document is created.
     */

    private function parseCustomization()
      {
	$oTEI = $this->getElementsByTagname( 'TEI' )->item(0);
	$oBody = $this->getElementsByTagname( 'body' )->item(0);

	$this->m_oRomaDom->getHeader( &$aszHeader );
#        $aszHeader = $oBody->appendChild( $aszHeader );

	// get selected Modules
	$this->m_oRomaDom->getSelectedModules( $aszModules );

	foreach ( $aszModules as $szModule )
	  {
            $theMod = $this->createElementNS( 'http://www.tei-c.org/ns/1.0', 'moduleSpec' );
	    $oModule = $oBody->appendChild( $theMod );
	    $oModule->setAttribute( 'ident', $szModule );
	    echo "<p>Module: ". $szModule. "<br>";
	    $this->m_oRomaDom->getIncludedElementsInModule( $szModule, $aszElements );
	    foreach ( $aszElements as $szElement )
	      {
		$nElements++;

		$oElementDom = new domDocument();
		echo $szElement . " ";
		$oElementDom->loadXML( join( '', file( roma_xquery_server . 'copytag.xq?name=' . $szElement ) ) );
		$oSpec = $oElementDom->getElementsByTagname( 'elementSpec' )->item(0);
		
		$oSpec = $this->importNode( $oSpec, true );
		$oModule->appendChild( $oSpec );
		
		$this->m_oRomaDom->getElementsChangedNameInModule( $szModule, $szElement, $szNewName );
		if ( $szNewName != $szElement ) 
		  {
		    $oSpec->setAttribute( 'mode', 'change' );
		    $oSpec->insertBefore( new domElement( 'altIdent', $szNewName, 'http://www.tei-c.org/ns/1.0' ), $oSpec->firstChild );
		  }

		$this->m_oRomaDom->getContentsByElementNameInModuleDom( $szElement, $szModule, $oNewContents );
		$oContent = $oSpec->getElementsByTagname( 'content' )->item(0);
		$oNewContent = $this->importNode( $oNewContents->documentElement, true );
		//Content has to come after classes
		$oNext = $oContent->nextSibling;
		$oSpec->removeChild( $oContent );
		$oSpec->insertBefore( $oNewContent, $oNext );

		$this->m_oRomaDom->getDescriptionByElementNameInModule( $szElement, $szModule, $szNewDesc );
		$oSpec->getElementsByTagname( 'desc' )->item(0)->removeChild( $oSpec->getElementsByTagname( 'desc' )->item(0)->firstChild );
		$oSpec->getElementsByTagname( 'desc' )->item(0)->appendChild( new domText( $szNewDesc ) );

		$this->m_oRomaDom->getAttributeDomByElementInModule( $szElement, $szModule, '', $oAttDom );
		//delete attributes
		$oAttList = $oSpec->getElementsByTagname( 'attList' )->item(0);

		$oXPath = new domxpath( $this );
		$oXPath->registerNamespace( 'tei', 'http://www.tei-c.org/ns/1.0' );

		foreach( $oAttDom->documentElement->childNodes as $oAtt )
		  {
		    $szAttribute = $oAtt->getElementsByTagname( 'name' )->item(0)->nodeValue;

		    $oAttDef = $oXPath->query(
		    "//tei:schemaSpec/tei:elementSpec[@ident='{$szElement}'
		    and @module='{$szModule}']/tei:attList/tei:attDef[@ident='{$szAttribute}']" )->item(0);

		    if ( is_object( $oAttDef ) )
		      {
			$oAttDef->setAttribute( 'usage', $oAtt->getElementsByTagname( 'name' )->item(0)->getAttribute( 'usage' ) );

			$szMode = ( $oAtt->getElementsByTagname( 'added' )->item(0)->nodeValue == 'true' ) ? 'add' : 'change';
			$szMode = ( $oAtt->getElementsByTagname( 'include' )->item(0)->nodeValue == 'delete' ) ? 'delete' : $szMode;
			
			$oAttDef->setAttribute( 'mode',  $szMode );

			$szAltName = $oAtt->getElementsByTagname( 'altName' )->item(0)->nodeValue;
			if ( $szAltName != $szAttribute && $szAltName != '')
			  {
			    $oAttDef->insertBefore( new domElement( 'altIdent', $szAltName ), $oAttDef->firstChild );
			  }
			
			//get Definition
			$this->m_oRomaDom->getAttributeDefinition( $szAttribute, $szElement, $szModule, '', $oDef );
			$oNewDatatype = $oDef->getElementsByTagname( 'datatype' )->item(0);

			//remove old Datatype
			$oDatatype = $oAttDef->getElementsByTagname( 'datatype' )->item(0);
			if ( ! is_object ( $oDatatype ) )
			  {
			    $theDatatype = $this->createElementNS( 'http://www.tei-c.org/ns/1.0', 'datatype' );
			    $oDatatype = $oAttDef->appendChild( $theDatatype );
			  }
			else
			  {
			    if ( $oDatatype->hasChildNodes() )
			      $oDatatype->removeChild( $oDatatype->firstChild );
			  }

			switch( $oNewDatatype->nodeValue )
			  {
			  case 'text':
			    $oRNG = $this->createElementNS( 'http://relaxng.org/ns/structure/1.0', 'rng:' . $oNewDatatype->nodeValue );
			    $oDatatype->appendChild( $oRNG );
			    break;
			  default:
			    if( substr( $oNewDatatype->nodeValue, 0, 9 ) == 'datatype.' )
			      {
				$oRNG = $this->createElementNS( 'http://relaxng.org/ns/structure/1.0', 'rng:ref' );
				$oRef = $oDatatype->appendChild( $oRNG );
				$oRef->setAttribute( 'name', $oNewDatatype->nodeValue );
			      }
			    else
			      {
				$oRNG = $this->createElementNS( 'http://relaxng.org/ns/structure/1.0', 'rng:data' );
				$oRef = $oDatatype->appendChild( $oRNG );
				$oRef->setAttribute( 'type', $oNewDatatype->nodeValue );
			      }
			    break;
			  }

			//desc
			$oDesc = $oAttDef->getElementsByTagname( 'desc' )->item(0);
			$oDesc->removeChild( $oDesc->firstChild );
			$oDesc->appendChild( new domText( $oAtt->getElementsByTagname( 'desc' )->item(0)->nodeValue ) );

			//defaultval
			$oNewDefault = $oAtt->getElementsByTagname( 'default' )->item(0);
			$oDefault = $oAttDef->getElementsByTagname( 'defaultVal' )->item(0);
			if ( is_object( $oDefault ) )
			  {
			    $oDefault->removeChild( $oDefault->firstChild );
			  }
			else
			  {
			    $theDefault = $this->createElementNS( 'http://www.tei-c.org/ns/1.0', 'defaultVal' );
			    $oDefault = $oAttDef->appendChild( $theDefault );
			  }
			$oDefault->appendChild( new domText( $oNewDefault->nodeValue ) );
		      }
		    else //new attribute
		      {
			$theAttDef = $this->createElementNS( 'http://www.tei-c.org/ns/1.0', 'attDef' );
			$oAttDef = $oAttList->appendChild( $theAttDef );
			
			$oAttDef->setAttribute( 'ident', $szAttribute );
			$oAttDef->setAttribute( 'mode', 'add' );

			$oAttDef->appendChild( new domElement( 'defaultVal', $oAtt->getElementsByTagname( 'default' )->item(0)->nodeValue, 'http://www.tei-c.org/ns/1.0' ) );

			$this->m_oRomaDom->getAttributeDefinition( $szAttribute, $szElement, $szModule, '', $oDef );
			$oNewDatatype = $oDef->getElementsByTagname( 'datatype' )->item(0);

			$theDatatype = $this->createElementNS( 'http://www.tei-c.org/ns/1.0', 'datatype' );
			$oDatatype = $oAttDef->appendChild( $theDatatype );

			switch( $oNewDatatype->nodeValue )
			  {
			  case 'text':
			    $oRNG = $this->createElementNS( 'http://relaxng.org/ns/structure/1.0', 'rng:' . $oNewDatatype->nodeValue );
			    $oDatatype->appendChild( $oRNG );
			    break;
			  default:
			    if( substr( $oNewDatatype->nodeValue, 0, 9 ) == 'datatype.' )
			      {
				$oRNG = $this->createElementNS( 'http://relaxng.org/ns/structure/1.0', 'rng:ref' );
				$oRef = $oDatatype->appendChild( $oRNG );
				$oRef->setAttribute( 'name', $oNewDatatype->nodeValue );
			      }
			    else
			      {
				$oRNG = $this->createElementNS( 'http://relaxng.org/ns/structure/1.0', 'rng:data' );
				$oRef = $oDatatype->appendChild( $oRNG );
				$oRef->setAttribute( 'type', $oNewDatatype->nodeValue );
			      }
			    break;
			  }

			$oAttDef->appendChild( new domElement( 'desc', $oAtt->getElementsByTagname( 'desc' )->item(0)->nodeValue, 'http://www.tei-c.org/ns/1.0' ) );
		      }
		  }
		
		if ( $nElements == 3 )
		  {
		    $nElements = 0;
		    if ( $this->bBar )
		      $this->m_oRomaDom->updateProgressBar( 1, true, 60 );
		  }
	      }
	  }

	//parse classes
	$this->m_oRomaDom->getChangedAttributeClasses( $aszClasses );

	foreach( $aszClasses as $szClass )
	  {
	    $this->m_oRomaDom->getAttributeClassChanges( $szClass, $oDom );

	    $szModule = $oDom->getElementsByTagname( 'moduleSpec' )->item(0)->nodeValue;
	    $oXPath = new domxpath( $this );
	    $oXPath->registerNamespace( 'tei', 'http://www.tei-c.org/ns/1.0' );
	    
	    $oModule = $oXPath->query( "//tei:schemaSpec/tei:moduleRef[@ident='{$szModule}']" )->item(0);
	    if ( ! is_object( $oModule ) )
	      {
		$theMod = $this->createElementNS( 'http://www.tei-c.org/ns/1.0', 'moduleRef' );
		$oBody = $this->getElementsByTagname( 'body' )->item(0);
		$oModule = $oBody->appendChild( $theMod );
		$oModule->setAttribute( 'ident', $szModule );
	      }

    
	    $theClassSpec = $this->createElementNS( 'http://www.tei-c.org/ns/1.0', 'classSpec' );
	    $oClassSpec = $oModule->appendChild( $theClassSpec );
	    $oClassSpec->setAttribute( 'ident', $szClass );
	    
	    $oAttList = $this->importNode( $oDom->getElementsByTagname( 'attList' )->item(0), true );
	    $oClassSpec->appendChild( $oAttList );	    
	  }

	if ( $this->bBar )
	    $this->m_oRomaDom->updateProgressBar( '3', true, 60 );


	//new Elements
	if ( $this->m_oRomaDom->getAddedElements( $oElementsDom ) === false )
	  {
	    foreach( $oElementsDom->documentElement->childNodes as $oElement )
	      {
		$theElementSpec = $this->createElementNS( 'http://www.tei-c.org/ns/1.0', 'elementSpec' );
		$oElementSpec = $oBody->appendChild( $theElementSpec );
		$szElement = $oElement->getElementsByTagname( 'elementName' )->item(0)->nodeValue;
		$oElementSpec->setAttribute( 'ident', $szElement );
		$oElementSpec->setAttribute( 'mode', 'add' );

		$theClasses = $this->createElementNS( 'http://www.tei-c.org/ns/1.0', 'classes' );
		$oClasses = $oElementSpec->appendChild( $theClasses );
		$theContent = $this->createElementNS( 'http://www.tei-c.org/ns/1.0', 'content' );
		$oContent = $oElementSpec->appendChild( $theContent );

		$this->m_oRomaDom->getAddedElementsClasses( $szElement, $aszClasses );
		foreach( $aszClasses as $szClass )
		  {
		    $theMemberOf = $this->createElementNS( 'http://www.tei-c.org/ns/1.0', 'memberOf' );
		    $oMember = $oClasses->appendChild( $theMemberOf );
		    $oMember->setAttribute( 'key', $szClass );
		  }

		$this->m_oRomaDom->getAddedElementsDescription( $szElement, $szDesc );
		$oElementSpec->appendChild( new domElement( 'desc', $szDesc, 'http://www.tei-c.org/ns/1.0' ) );

		$this->m_oRomaDom->getAddedElementsContents( $szElement, $szContents );
		switch( $szContents )
  	  	  {
		  case 'empty':
		  case 'text':
		    $oRNG = $this->createElementNS( 'http://relaxng.org/ns/structure/1.0', 'rng:' . $szContents );
		    $oContent->appendChild( $oRNG );
		    break; 
		  default:
		    $oRNG = $this->createElementNS( 'http://relaxng.org/ns/structure/1.0', 'rng:ref' );
		    $oRef = $oContent->appendChild( $oRNG );
		    $oRef->setAttribute( 'name', $szContents );
		    break;
		  }

		$this->m_oRomaDom->getAttributeDomByElementInModule( $szElement, $szModule, '', $oAttDom );
		//delete attributes

		$theDefault = $this->createElementNS( 'http://www.tei-c.org/ns/1.0', 'attList' );
		$oAttList = $oElementSpec->appendChild( $theDefault );

		$oAttRoot = $oAttDom->documentElement;
		
		if ( is_object( $oAttRoot ) )
		  {
		    foreach( $oAttRoot->childNodes as $oAtt )
		      {
			$szAttribute = $oAtt->getElementsByTagname( 'name' )->item(0)->nodeValue;
			
			$theAttDef = $this->createElementNS( 'http://www.tei-c.org/ns/1.0', 'attDef' );
			$oAttDef = $oAttList->appendChild( $theAttDef );
			
			$oAttDef->setAttribute( 'ident', $szAttribute );
			$oAttDef->setAttribute( 'mode', 'add' );
			
			$oAttDef->appendChild( new domElement( 'defaultVal', $oAtt->getElementsByTagname( 'default' )->item(0)->nodeValue, 'http://www.tei-c.org/ns/1.0' ) );
			
			$this->m_oRomaDom->getAttributeDefinition( $szAttribute, $szElement, $szModule, '', $oDef );
			$oNewDatatype = $oDef->getElementsByTagname( 'datatype' )->item(0);
			
			$theDatatype = $this->createElementNS( 'http://www.tei-c.org/ns/1.0', 'datatype' );
			$oDatatype = $oAttDef->appendChild( $theDatatype );
			
			switch( $oNewDatatype->nodeValue )
			  {
			  case 'text':
			    $oRNG = $this->createElementNS( 'http://relaxng.org/ns/structure/1.0', 'rng:' . $oNewDatatype->nodeValue );
			    $oDatatype->appendChild( $oRNG );
			    break;
			  default:
			    if( substr( $oNewDatatype->nodeValue, 0, 9 ) == 'datatype.' )
			      {
				$oRNG = $this->createElementNS( 'http://relaxng.org/ns/structure/1.0', 'rng:ref' );
				$oRef = $oDatatype->appendChild( $oRNG );
				$oRef->setAttribute( 'name', $oNewDatatype->nodeValue );
			      }
			    else
			      {
				$oRNG = $this->createElementNS( 'http://relaxng.org/ns/structure/1.0', 'rng:data' );
				$oRef = $oDatatype->appendChild( $oRNG );
				$oRef->setAttribute( 'type', $oNewDatatype->nodeValue );
			      }
			    break;
			  }
			
		    $oAttDef->appendChild( new domElement( 'desc', $oAtt->getElementsByTagname( 'desc' )->item(0)->nodeValue, 'http://www.tei-c.org/ns/1.0' ) );
		      }
		  }
	      }
	  } 
      }

    // #####################################################################
    // --- Little Helpers
    // #####################################################################



    public function getTeiLiteDom( &$oTeiLite )
      {
	//create the teiLite
        $oXSL = new domDocument();
 	$oXSL->load( roma_StylesheetDir . '/odds/teixml-odds.xsl'  );

	$oProc = new XsltProcessor();
	$oProc->importStylesheet( $oXSL );
	
	$oTeiLite = $oProc->transformToDoc( $this );
      }

    // #####################################################################
    // --- Create some Output
    // #####################################################################

    public function outputPlain( &$szPlain )
      {
	$oTidy = new tidy();
	$aszOptions = array( 'indent' => true,
			     'indent-spaces' => 1,
			     'wrap' => 72,
			     'input-xml' => true,
			     'output-xml' => true
			     );
	$oTidy->parseString( $this->SaveXML(), $aszOptions );

	$oTidy->cleanRepair();
	$szPlain = $oTidy->value;
	if ( $this->bBar )
	    $this->m_oRomaDom->updateProgressBar( '100' );
      }


    public function outputTeiLite( &$szTeiLite )
      {
	$this->getTeiLiteDom( $oTeiLiteDom );
	if ( $this->bBar )
	    $this->m_oRomaDom->updateProgressBar( '70' );
	
	$oTidy = new tidy();
	$aszOptions = array( 'indent' => false,
			     'input-xml' => true,
			     'output-xml' => true
			     );
	$oTidy->parseString( $oTeiLiteDom->SaveXML(), $aszOptions );
	if ( $this->bBar )
	    $this->m_oRomaDom->updateProgressBar( '80' );

	$oTidy->cleanRepair();
	$szTeiLite = $oTidy->value;

	if ( $this->bBar )
	    $this->m_oRomaDom->updateProgressBar( '100' );
      }

    public function outputLatex( &$szLatex )
      {
	$this->getTeiLiteDom( $oTeiLiteDom );
	if ( $this->bBar )
	  $this->m_oRomaDom->updateProgressBar( '70' );

	$oXSL = new domDocument();
	$oXSL->load( roma_styleheet_docLatex );
	
	$oProc = new XsltProcessor();
	$oProc->importStylesheet( $oXSL );
	
	if ( $this->bBar )
	  $this->m_oRomaDom->updateProgressBar( '85' );
	
	$szLatex = $oProc->transformToXML( $oTeiLiteDom );
	
	if ( $this->bBar )
	  $this->m_oRomaDom->updateProgressBar( '100' );
      }

    public function outputPdfLatex( &$szPdf )
      {
	$this->getTeiLiteDom( $oTeiLiteDom );
	if ( $this->bBar )
	  $this->m_oRomaDom->updateProgressBar( '70' );

	
	$oXSL = new domDocument();
	$oXSL->load( roma_styleheet_docLatex );
	
	$oProc = new XsltProcessor();
	$oProc->importStylesheet( $oXSL );

	if ( $this->bBar )
	  $this->m_oRomaDom->updateProgressBar( '75' );

	$szTmp = $oProc->transformToXML( $oTeiLiteDom );
	
	if ( $this->bBar )
	  $this->m_oRomaDom->updateProgressBar( '80' );

	//Save File
	$szID = md5( uniqid(rand(), true ) );
	
	$szInputFile = roma_temporaryFilesDir . '/' . $szID . '.tex';    
	$szOutputFile = roma_temporaryFilesDir . '/' . $szID . '.pdf';    
	
	file_put_contents( $szInputFile , $szTmp );

	if ( $this->bBar )
	  $this->m_oRomaDom->updateProgressBar( '85' );
	
	$szCurrentDir = getcwd();
	chdir( roma_temporaryFilesDir );
	exec( roma_pdflatex . ' -interaction=nonstopmode ' . $szInputFile );
	if ( $this->bBar )
	  $this->m_oRomaDom->updateProgressBar( '90' );

	exec( roma_pdflatex . ' -interaction=nonstopmode ' . $szInputFile );
	chdir( $szCurrentDir );

	if ( $this->bBar )
	  $this->m_oRomaDom->updateProgressBar( '95' );
	
	$szPdf = join( '', file( $szOutputFile ) );
	
	unlink( $szInputFile );
	unlink( $szOutputFile );

	if ( $this->bBar )
	  $this->m_oRomaDom->updateProgressBar( '100' );
      }

    public function outputPDF( &$szPdf )
      {
	$this->getTeiLiteDom( $oTeiLiteDom );

	if ( $this->bBar )
	  $this->m_oRomaDom->updateProgressBar( '60' );

	$oXSL = new domDocument();
	$oXSL->load( roma_styleheet_docPDF );
	
	$oProc = new XsltProcessor();
	$oProc->importStylesheet( $oXSL );
	$oTmpDom = $oProc->transformToDoc( $oTeiLiteDom );

	if ( $this->bBar )
	  $this->m_oRomaDom->updateProgressBar( '70' );
	
	//Save File
	$szID = md5( uniqid(rand(), true ) );
	
	$szInputFile = roma_temporaryFilesDir . '/' . $szID . '.fo';    
	$szOutputFile = roma_temporaryFilesDir . '/' . $szID . '.pdf';    
	
	file_put_contents( $szInputFile , $oTmpDom->SaveXML() );

	if ( $this->bBar )
	  $this->m_oRomaDom->updateProgressBar( '80' );

	exec( roma_fop . ' ' . $szInputFile . ' ' . $szOutputFile );

	if ( $this->bBar )
	  $this->m_oRomaDom->updateProgressBar( '90' );

	$szPdf = join( '', file( $szOutputFile ) );
	
	unlink( $szInputFile );
	unlink( $szOutputFile );

	if ( $this->bBar )
	  $this->m_oRomaDom->updateProgressBar( '100' );
      }

    public function outputDVI ( &$szDVI )
      {
	$this->getTeiLiteDom( $oTeiLiteDom );

	if ( $this->bBar )
	  $this->m_oRomaDom->updateProgressBar( '60' );

	$oXSL = new domDocument();
	$oXSL->load( roma_styleheet_docLatex );
	
	$oProc = new XsltProcessor();
	$oProc->importStylesheet( $oXSL );
	$szTmp = $oProc->transformToXML( $oTeiLiteDom );

	if ( $this->bBar )
	  $this->m_oRomaDom->updateProgressBar( '70' );
	
	//Save File
	$szID = md5( uniqid(rand(), true ) );
	
	$szInputFile = roma_temporaryFilesDir . '/' . $szID . '.tex';    
	$szOutputFile = roma_temporaryFilesDir . '/' . $szID . '.dvi';    
	
	file_put_contents( $szInputFile , $szTmp );

	if ( $this->bBar )
	  $this->m_oRomaDom->updateProgressBar( '80' );

	$szCurrentDir = getcwd();
	chdir( roma_temporaryFilesDir );
	exec( roma_latex . ' -interaction=nonstopmode ' . $szInputFile );
	if ( $this->bBar )
	  $this->m_oRomaDom->updateProgressBar( '90' );

	exec( roma_latex . ' -interaction=nonstopmode ' . $szInputFile );
	chdir( $szCurrentDir );

	if ( $this->bBar )
	  $this->m_oRomaDom->updateProgressBar( '95' );
	
	$szDVI = join( '', file( $szOutputFile ) );
	
	unlink( $szInputFile );
	unlink( $szOutputFile );

	if ( $this->bBar )
	  $this->m_oRomaDom->updateProgressBar( '100' );
      }

    public function outputHTML ( &$szHTML )
      {
	$this->getTeiLiteDom( $oTeiLiteDom );

	if ( $this->bBar )
	  $this->m_oRomaDom->updateProgressBar( '70' );

	$oXSL = new domDocument();
	$oXSL->load( roma_styleheet_docHtml );
	
	$oProc = new XsltProcessor();
	$oProc->importStylesheet( $oXSL );

	if ( $this->bBar )
	  $this->m_oRomaDom->updateProgressBar( '80' );

	$szHTML = $oProc->transformToDoc( $oTeiLiteDom )->SaveHTML();

	if ( $this->bBar )
	  $this->m_oRomaDom->updateProgressBar( '100' );
      }

  }

</script>
