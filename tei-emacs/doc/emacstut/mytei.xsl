<xsl:stylesheet 
     xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
     version="1.0">
<!-- XSLT stylesheet to generate HTML version of TEI document
Written by the TEI XSL generator (Sebastian Rahtz, sebastian.rahtz@oucs.ox.ac.uk, October 2000)
Created on 28 Nov 2000-->
<xsl:import href="/home/chris/tools/tei.old/teihtml.xsl"/>
<xsl:variable name="showTitleAuthor">true</xsl:variable>
<xsl:variable name="teixslHome">/pfiles/xml/xsl/tei/</xsl:variable>
<xsl:variable name="searchWords"></xsl:variable>
<xsl:variable name="searchURL"></xsl:variable>
<xsl:variable name="topNavigationPanel"></xsl:variable>
<xsl:variable name="bottomNavigationPanel"></xsl:variable>
<xsl:variable name="homeWords">Christian Witterns homepage</xsl:variable>
<xsl:variable name="homeURL">http://www.chibs.edu.tw/faculty/chris</xsl:variable>
<xsl:variable name="linkPanel"></xsl:variable>
<xsl:variable name="cssFile">pfiles/xml/xsl/tei/tei-oucs.css</xsl:variable>
<xsl:variable name="institution">Chung-Hwa Institute of Buddhist Studies</xsl:variable>
<xsl:variable name="useHeaderFrontMatter">false</xsl:variable>
<xsl:template name="logoPicture"></xsl:template>
<xsl:template name="copyrightStatement"></xsl:template>
<xsl:variable name="tocFront"></xsl:variable>
<xsl:variable name="sectionTopLink">true</xsl:variable>
<xsl:variable name="tocBack"></xsl:variable>
<xsl:variable name="feedbackURL">mailto:chris@ccbs.ntu.edu.tw</xsl:variable>
<xsl:variable name="parentWords"></xsl:variable>
<xsl:variable name="makingSlides">false</xsl:variable>
<xsl:variable name="parentURL">http://www.chibs.edu.tw</xsl:variable>
</xsl:stylesheet>
