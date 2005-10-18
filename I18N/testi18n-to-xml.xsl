<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
     xmlns:s="http://www.ascc.net/xml/schematron" 
     xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
     xmlns:tei="http://www.tei-c.org/ns/1.0"
     xmlns:estr="http://exslt.org/strings"
     xmlns:t="http://www.thaiopensource.com/ns/annotations"
     xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
     xmlns:edate="http://exslt.org/dates-and-times"
     xmlns:exsl="http://exslt.org/common"
     xmlns:rng="http://relaxng.org/ns/structure/1.0"
     extension-element-prefixes="exsl estr edate"
     exclude-result-prefixes="exsl edate estr tei t a rng s" 
     version="1.0">


<xsl:import
    href="/usr/share/xml/tei/stylesheet/odds/odd2lite.xsl"/>

  <xsl:param name="oddmode">tei</xsl:param>

<xsl:template match="/">
  <TEI  xmlns="http://www.tei-c.org/ns/1.0">
   <teiHeader>
     <fileDesc>
       <titleStmt>
         <title>TEI I18N demo: language LANG, interface ILANG</title>
       </titleStmt>
       <editionStmt>
       <edition><date>7th September 2005</date></edition></editionStmt>
       <publicationStmt>
            <authority>The Text Encoding Initiative</authority>
	 <p>TEI Web</p>      
       </publicationStmt>
       <sourceDesc>
	 <p>No source</p>
       </sourceDesc>
     </fileDesc>
     <profileDesc>
     </profileDesc>
     <revisionDesc>
       <change>
	 <date>$Date$.</date>
	 <respStmt>
	   <name>$Author$</name>
	 </respStmt>
	 <item>$Revision$</item>
       </change>
     </revisionDesc>
   </teiHeader>
   <text>
     <body>
       <xsl:apply-templates mode="weave"/>
     </body>
   </text>
  </TEI>
</xsl:template>

</xsl:stylesheet>


