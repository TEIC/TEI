<!-- 
Text Encoding Initiative Consortium XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL stylesheet to format TEI XML documents to HTML or XSL FO

 
#include LICENSE
--> 
<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
  version="1.0">
  
<xsl:import href="../common/teicommon.xsl"/>
<xsl:import href="teihtml-param.xsl"/>

<xsl:include href="teihtml-bibl.xsl"/>
<xsl:include href="teihtml-chunk.xsl"/>
<xsl:include href="teihtml-corpus.xsl"/>
<xsl:include href="teihtml-drama.xsl"/>
<xsl:include href="teihtml-figures.xsl"/>
<xsl:include href="teihtml-frames.xsl"/>
<xsl:include href="teihtml-front.xsl"/>
<xsl:include href="teihtml-lists.xsl"/>
<xsl:include href="teihtml-main.xsl"/>
<xsl:include href="teihtml-math.xsl"/>
<xsl:include href="teihtml-misc.xsl"/>
<xsl:include href="teihtml-notes.xsl"/>
<xsl:include href="teihtml-pagetable.xsl"/>
<xsl:include href="teihtml-poetry.xsl"/>
<xsl:include href="teihtml-struct.xsl"/>
<xsl:include href="teihtml-tables.xsl"/>
<xsl:include href="teihtml-xref.xsl"/>

</xsl:stylesheet>
