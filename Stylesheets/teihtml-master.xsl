<!-- 
Text Encoding Initiative Consortium XSLT stylesheet family version 3.0
RCS: $Id$

XSL stylesheet to format TEI XML documents to HTML or XSL FO

 Copyright 1999-2003 Text Encoding Initiative Consortium  
#include LICENSE
--> 
<xsl:stylesheet
  xmlns:tei="http://www.tei-c.org/ns/1.0"

  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
  version="1.0"
  xmlns:xt="http://www.jclark.com/xt"
  xmlns:saxon="http://icl.com/saxon"
  xmlns:lxslt="http://xml.apache.org/xslt"
  xmlns:xalan="org.apache.xalan.xslt.extensions.Redirect"
  exclude-result-prefixes="saxon xt xalan lxslt"
  extension-element-prefixes="saxon xt xalan lxslt">

<xsl:import href="teihtml.xsl"/>

<xsl:output method="html"  
 doctype-public="-//W3C//DTD HTML 4.0 Transitional//EN"/>

<!-- modules -->

<!-- parameterization -->



<!-- *************** variables ************************** -->
<xsl:variable name="verbose">true</xsl:variable>
<xsl:variable name="autoToc"></xsl:variable>
<xsl:variable name="subTocDepth">-1</xsl:variable>
<!-- xsl:variable name="masterFile">index</xsl:variable -->
<xsl:variable name="numberHeadings"></xsl:variable>
<xsl:variable name="cssFile">/Stylesheets/master.css</xsl:variable>
<xsl:variable name="alignNavigationPanel">right</xsl:variable>
<xsl:variable name="topNavigationPanel"></xsl:variable>
<xsl:variable name="bottomNavigationPanel">true</xsl:variable>
<xsl:variable name="institution">MASTER Project</xsl:variable>
<xsl:variable name="homeURL">http://www.cta.dmu.ac.uk/projects/master/</xsl:variable>
<xsl:variable name="homeWords">Project Home Page</xsl:variable>
<xsl:variable name="parentURL">/Master/Reference/</xsl:variable>
<xsl:variable name="parentWords">MASTER Reference Manual</xsl:variable>
<xsl:variable name="feedbackURL">mailto:master@maillist.ox.ac.uk</xsl:variable>
<xsl:variable name="feedbackWords">Contact</xsl:variable>
<xsl:variable name="searchURL"></xsl:variable>
<xsl:variable name="searchWords"></xsl:variable>
<xsl:variable name="leftLinks">true</xsl:variable>
<xsl:template name="logoPicture">
<img src="/TEI/Pictures/master.gif" alt="" width="160" />
</xsl:template>
<!-- xsl:variable name="useIDs"></xsl:variable -->
<xsl:template name="copyrightStatement"></xsl:template>
   <xsl:variable name="dateWord"></xsl:variable>
   <xsl:variable name="authorWord"></xsl:variable>






</xsl:stylesheet>


