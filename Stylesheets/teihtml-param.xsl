<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:param name="teixslHome">http://www.oucs.ox.ac.uk/stylesheets/</xsl:param>
  <xsl:param name="inputName"></xsl:param>
  <xsl:param name="outputDir"></xsl:param>
  <xsl:param name="splitLevel">-1</xsl:param>
  <xsl:param name="splitBackmatter">true</xsl:param>
  <xsl:param name="splitFrontmatter">true</xsl:param>
  <xsl:param name="sectionTopLink"></xsl:param>
  <xsl:param name="useIDs">true</xsl:param>
  <xsl:param name="makingSlides"></xsl:param>
  <xsl:param name="autoToc">true</xsl:param>
  <xsl:param name="tocDepth">5</xsl:param>
  <xsl:param name="subTocDepth">-1</xsl:param>
  <xsl:param name="tocFront">true</xsl:param>
  <xsl:param name="tocBack">true</xsl:param>
  <xsl:template name="logoPicture"><a target="_top" href="http://www.ox.ac.uk/"><img border="0" width="78" height="94"
    src="http://www.oucs.ox.ac.uk/images/ncrest.gif"
    alt="Oxford University"/></a></xsl:template>
  <xsl:param name="cssFile">/stylesheets/tei-oucs.css</xsl:param>
  <xsl:param name="feedbackURL">mailto:feedback</xsl:param>
  <xsl:template name="feedbackWords">Feedback</xsl:template>
  <xsl:param name="homeURL">http://www.oucs.ox.ac.uk/</xsl:param>
  <xsl:param name="homeLabel">Home</xsl:param>
  <xsl:param name="noframeWords">No Frames</xsl:param>
  <xsl:param name="homeWords">OUCS</xsl:param>
  <xsl:param name="institution">Oxford University Computing Services</xsl:param>
  <xsl:param name="department"></xsl:param>
  <xsl:param name="parentURL">http://www.ox.ac.uk/</xsl:param>
  <xsl:param name="parentWords">Oxford University</xsl:param>
  <xsl:param name="searchURL">http://wwwsearch.ox.ac.uk/cgi-bin/oxunit?oucs</xsl:param>
  <xsl:template name="searchWords">Search</xsl:template>
  <xsl:param name="topNavigationPanel">true</xsl:param>
  <xsl:param name="bottomNavigationPanel">true</xsl:param>
  <xsl:param name="alignNavigationPanel">right</xsl:param>
  <xsl:param name="linkPanel">true</xsl:param>
  <xsl:template name="copyrightStatement"><a 
href="/documentation/copyright.html">&#169;</a> Oxford 
University Computing Services.</xsl:template>
  <xsl:param name="makePageTable"></xsl:param>
  <xsl:param name="leftLinks"></xsl:param>
  <xsl:param name="rightLinks"></xsl:param>
  <xsl:param name="linksWidth">15%</xsl:param>
  <xsl:param name="makeFrames"></xsl:param>
  <xsl:param name="frameCols">200,*</xsl:param>
  <xsl:param name="frameAlternateURL"></xsl:param>
  <xsl:template name="logoFramePicture"><a class="framelogo" target="_top" href="http://www.ox.ac.uk">
     <img src="http://www.oucs.ox.ac.uk/images/ncrest.gif"
     vspace="5" width="90" height="107" border="0"
  alt="University Of Oxford"/></a></xsl:template>
  <xsl:param name="sectionUpLink"></xsl:param>
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
  <xsl:param name="numberHeadings">true</xsl:param>
  <xsl:param name="numberFrontHeadings"></xsl:param>
  <xsl:param name="numberBackHeadings">true</xsl:param>
  <xsl:param name="prenumberedHeadings"></xsl:param>
  <xsl:param name="headingNumberSuffix">.<xsl:value-of select="$numberSpacer"/></xsl:param>
  <xsl:param name="numberSpacer"><xsl:text> </xsl:text> </xsl:param>
  <xsl:param name="numberHeadingsDepth">9</xsl:param>
  <xsl:param name="fontURL">span</xsl:param>
  <xsl:param name="tableAlign">left</xsl:param>
  <xsl:param name="cellAlign">left</xsl:param>
  <xsl:param name="showTitleAuthor"></xsl:param>
  <xsl:param name="footnoteFile"></xsl:param>
  <xsl:param name="minimalCrossRef"></xsl:param>
  <xsl:param name="graphicsPrefix"></xsl:param>
  <xsl:param name="graphicsSuffix">.jpg</xsl:param>
  <xsl:param name="showFigures">true</xsl:param>
  <xsl:template name="bodyHook"></xsl:template>
  <xsl:template name="bodyJavaScript"></xsl:template>
  <xsl:template name="headHook"></xsl:template>
  <xsl:template name="searchbox"></xsl:template>
  <xsl:template name="rendSeparator">;</xsl:template>
  <xsl:param name="verbose"></xsl:param>
  <xsl:param name="downPicture">http://www.oucs.ox.ac.uk/images/down.gif</xsl:param>
  <xsl:param name="useHeaderFrontMatter"></xsl:param>
  <xsl:param name="outputEncoding">iso-8859-1</xsl:param>
  <xsl:param name="REQUEST"></xsl:param>
  <xsl:param name="ID"></xsl:param>
  <xsl:param name="rawIE"></xsl:param>
  <xsl:template name="numberBodyDiv"><xsl:param name="minimal"/>
     <xsl:if test="not($numberHeadings='')">
         <xsl:number level="multiple"
                     count="div|div0|div1|div2|div3|div4|div5|div6"/>
     </xsl:if></xsl:template>
  <xsl:template name="numberFrontDiv"><xsl:param name="minimal"/>
     <xsl:if test="not($numberFrontHeadings='')">
         <xsl:number level="multiple" 
                     count="div|div0|div1|div2|div3|div4|div5|div6"/>
<xsl:if test="not($minimal)"><xsl:value-of select="$numberSpacer"/></xsl:if>
      </xsl:if></xsl:template>
  <xsl:template name="numberBackDiv"><xsl:param name="minimal"/>
     <xsl:if test="not($numberBackHeadings='')">
        <xsl:number format="A.1.1.1.1.1" 
         level="multiple" count="div|div0|div1|div2|div3|div4|div5|div6"/>
      <xsl:if test="not($minimal)"><xsl:value-of select="$numberSpacer"/>
</xsl:if>
    </xsl:if></xsl:template>
</xsl:stylesheet>
