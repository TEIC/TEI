<xsl:stylesheet
  exclude-result-prefixes="xlink dbk rng tei teix xhtml a edate estr html pantor xd xs xsl"
  extension-element-prefixes="exsl estr edate" 
  version="1.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xlink="http://www.w3.org/1999/xlink"
  xmlns:dbk="http://docbook.org/ns/docbook"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:xhtml="http://www.w3.org/1999/xhtml"
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:edate="http://exslt.org/dates-and-times"
  xmlns:estr="http://exslt.org/strings" xmlns:exsl="http://exslt.org/common"
  xmlns:html="http://www.w3.org/1999/xhtml"
  xmlns:pantor="http://www.pantor.com/ns/local"
  xmlns:xd="http://www.pnp-software.com/XSLTdoc"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:import href="./odd2htmlp5.xsl"/>
<xsl:output method="xml"
	      doctype-public="//W3C//DTD XHTML 1.1//EN"
	      doctype-system="http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd"
	      encoding="utf-8"
	      />  
 
<xsl:param name="autoToc">true</xsl:param>
<xsl:param name="splitLevel">-1</xsl:param>
<xsl:param name="footnoteFile">false</xsl:param>
<xsl:param name="pageLayout">Simple</xsl:param>
<xsl:param name="cssFile">guidelines.css</xsl:param>
<xsl:param name="parentWords">Text Encoding Initiative Consortium</xsl:param>
<xsl:param name="topNavigationPanel">true</xsl:param>

<xsl:template name="generateSubTitle"/>

  <xsl:template name="includeCSS">
    <link href="{$cssFile}" rel="stylesheet" type="text/css"/>
    <xsl:if test="not($cssPrintFile='')">
      <link rel="stylesheet" media="print" type="text/css" href="{$cssPrintFile}"/>
    </xsl:if>
    <style type="text/css">
body{
 background-color: white;
 color: black;
 margin: 4pt;
 }
	 
pre,div.pre {
 font-family: monospace ;
 white-space: pre ;
 background-color: #FFFFCC;
 padding: 2pt;
 margin-left: 1em;
 margin-right: 1em;
 border-style: solid;
 border-color: black;
 border-width: 1pt;
}

pre.eg,div.pre_eg {
 font-family: monospace ;
 white-space: pre ;
 background-color: #EEEEEE;
 padding: 2pt;
 margin-left: 1em;
 margin-right: 1em;
 border-style: solid;
 border-color: black;
 border-width: 1pt;
}

div.specgrp {
 margin: 1em;
 padding: 2pt;
 border-style: solid;
 border-color: red;
 border-width: 1pt;
}

div.schemaFragment {
	   	   border: 1pt solid #000000;
		   margin: 1em;
		   }

/*	**specialized phrase-level encodings.	*/
	
span.ident-pe,span.ident-PE,span.ident-pent {
	font-family: monospace;
	font-size: larger
}
span.ident-pe:before,span.ident-PE:before,span.ident-pent:before {
	content: "%"
}
span.ident-pe:after,span.ident-PE:after,span.ident-pent:after {
	content: ";"
}
	
span.ident-att,	span.ident-attr,span.ident-attrName {
	font-family: monospace;
	font-size: larger
}
span.ident-att:after,span.ident-attr:after,span.ident-attrName:after {
	content: "="
}
	
span.ident-val,span.ident-attrVal {
	font-family: monospace;
	font-size: larger
}
span.ident-val:before,span.ident-val:after,span.ident-attrVal:before,span.ident-attrVal:after {
	content: '"'
}
	
span.ident-class,span.ident-element_class {
	font-family: Arial,Helvetica,sans-serif
}
	
span.ident-datatype {
	font-family: Arial,Helvetica,sans-serif
}
	
span.ident-ent,span.ident-ge {
	font-family: monospace;
	font-size: larger
}
span.ident-ent:after,span.ident-ge:after {
	content: ";"
}
	
span.ident-file {
	font-family: monospace;
	letter-spacing: 1pt
}
	
span.ident-fpi {
	font-family: monospace;
	letter-spacing: -1pt
}
	
span.ident-kw {
	font-family: monospace;
	font-size: larger
}
	
span.ident-entset {
	font-family: monospace
}
	
span.ident-module {
	font-style: italic;
}
	
span.ident-var, span.ident-variable {
	font-family: Arial,Helvetica,sans-serif;
	color: #2F4F4F;
}
	
	
div.specgrp {
	padding: 2pt;
	margin: 1em;
	border-style: solid;
	border-color: red;
	border-width: 1pt;
}

div.schemaFragment {
		   border: 1pt solid #000000;
		   margin: 1em;
		   }

span.label {
	   background-color: white;
	   color: red;
	   }
span.val {
	 font-family: Arial,Helvetica,sans-serif;
}
span.val:before{
 content: '"';
}  
span.val:after{
 content: '"';
}  
span.att{
 font-style: italic;
}  
span.tag{
 font-family: monospace;
}
div.citbibl {
	    text-align:right;
	    font-size: smaller;
}
span.ident-schemafrag {
	font-family: monospace
}

    </style>
  </xsl:template>


  
  <xsl:template name="pageTableHeader">
    <xsl:param name="mode"/>
    <table width="100%" border="0">
      <tr>
	<td height="100" class="bgimage" onClick="window.location='{$homeURL}'" cellpadding="0">
	  <h1 class="maintitle"><xsl:call-template name="generateTitle"/></h1>
	  <h2 class="subtitle"><xsl:call-template name="generateSubTitle"/></h2>
	</td>
      </tr>
    </table>
  </xsl:template>

<xsl:template name="printLink"/>



</xsl:stylesheet>


