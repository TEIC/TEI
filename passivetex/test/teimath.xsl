<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:m="http://www.w3.org/1998/Math/MathML" 
  version="1.0">
   <xsl:import href="http://www.tei-c.org/Stylesheets/teihtml.xsl"/>

   <xsl:param name="masterFile">teimath</xsl:param>
   <xsl:param name="splitLevel">0</xsl:param>
   <xsl:param name="autoToc">true</xsl:param>
   <xsl:param name="linkPanel">true</xsl:param>
   <xsl:param name="leftLinks"></xsl:param>
   <xsl:param name="makeFrames"></xsl:param>
   <xsl:param name="makePageTable">true</xsl:param>
   <xsl:param name="numberHeadings">true</xsl:param>
   <xsl:param name="numberParagraphs"></xsl:param>
   <xsl:param name="numberHeadingsDepth">9</xsl:param>




   <xsl:param name="subTocDepth">-1</xsl:param>
   <xsl:template name="logoFramePicture"><a class="framelogo" target="_top" href="http://www.ox.ac.uk">
      <img src="ncrest.gif"
           vspace="5" width="90" height="107" border="0"
	     alt="University Of Oxford"/></a></xsl:template>
    <xsl:template name="logoPicture"><a target="_top" href="http://www.ox.ac.uk/"><img border="0" width="78" height="94"
    src="ncrest.gif"
    alt="Oxford University"/></a></xsl:template>
   <xsl:param name="cssFile">tei-oucs.css</xsl:param>
   <xsl:param name="homeURL">http://www.oucs.ox.ac.uk/</xsl:param>
   <xsl:param name="homeWords">OUCS</xsl:param>
   <xsl:param name="institution">Oxford University Computing Services</xsl:param>
   <xsl:param name="department"/>
   <xsl:param name="parentURL">http://www.ox.ac.uk/</xsl:param>
   <xsl:param name="parentWords">Oxford University</xsl:param>
   <xsl:param name="searchURL">http://wwwsearch.ox.ac.uk/cgi-bin/oxunit?oucs</xsl:param>
   <xsl:param name="searchWords">Search</xsl:param>
   <xsl:param name="topNavigationPanel">true</xsl:param>
   <xsl:param name="bottomNavigationPanel">true</xsl:param>
   <xsl:param name="alignNavigationPanel">right</xsl:param>
   <xsl:template name="copyrightStatement"><a 
href="/documentation/copyright.html">&#169;</a> Oxford 
University Computing Services.</xsl:template>
   <xsl:param name="rightLinks"/>
   <xsl:param name="linksWidth">10%</xsl:param>
   <xsl:param name="frameCols">200,*</xsl:param>
   <xsl:param name="frameAlternateURL"/>
   <xsl:param name="sectionUpLink"/>
   <xsl:template name="topLink"><p>[<a href="#TOP">Back to top</a>]</p></xsl:template>
   <xsl:param name="appendixWords">Appendix</xsl:param>
   <xsl:param name="tocWords">Contents</xsl:param>
   <xsl:param name="upWord">Up</xsl:param>
   <xsl:param name="nextWord">Next</xsl:param>
   <xsl:param name="previousWord">Previous</xsl:param>
   <xsl:template name="contentsWord">Contents</xsl:template>
   <xsl:param name="dateWord">Date:</xsl:param>
   <xsl:param name="authorWord">Author:</xsl:param>
   <xsl:param name="divOffset">2</xsl:param>
   <xsl:param name="numberFrontHeadings"/>
   <xsl:param name="numberBackHeadings">A.1</xsl:param>
   <xsl:param name="headingNumberSuffix">. </xsl:param>
   <xsl:param name="outputEncoding">iso-8859-1</xsl:param>


   <xsl:template match="soundClip">
     <xsl:element name="a">
       <xsl:attribute name="href">
         <xsl:value-of select="@url"/>
       </xsl:attribute>
       <xsl:apply-templates/>
     </xsl:element>
   </xsl:template>

<xsl:template match="formula">
  <xsl:apply-templates/>  
</xsl:template>


<xsl:template match="m:math">
 <m:math>
  <xsl:copy-of select="@*"/>
  <xsl:apply-templates mode="math"/>
 </m:math>
</xsl:template>

<xsl:template match="m:*|@*|comment()|processing-instruction()|text()" mode="math">
 <xsl:copy>
   <xsl:apply-templates mode="math" select="*|@*|processing-instruction()|text()"/>
 </xsl:copy>
</xsl:template>

<xsl:template match="formula" mode="header"/>

</xsl:stylesheet>
