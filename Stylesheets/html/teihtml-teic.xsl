<!-- 
Text Encoding Initiative Consortium XSLT stylesheet family version 3.0
RCS: $Id$

XSL stylesheet to format TEI XML documents to HTML or XSL FO

 Copyright 1999-2003 Text Encoding Initiative Consortium  
#include LICENSE
--> 
<xsl:stylesheet  
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:edate="http://exslt.org/dates-and-times"
  xmlns:estr="http://exslt.org/strings"
  xmlns:exsl="http://exslt.org/common"
  xmlns:fo="http://www.w3.org/1999/XSL/Format"
  xmlns:local="http://www.pantor.com/ns/local"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  extension-element-prefixes="exsl estr edate"
  exclude-result-prefixes="exsl estr edate a fo local rng tei teix"
  xmlns:html="http://www.w3.org/1999/xhtml"
  version="1.0">

<xsl:import href="teihtml.xsl"/>

<xsl:output 
  method="html"  
  doctype-public="-//W3C//DTD HTML 4.0 Transitional//EN" 
  doctype-system="http://www.w3.org/TR/html4/loose.dtd"
  indent="yes"/>

<!-- parameterization -->

<xsl:template name="metaHook">
  <xsl:param name="title"/>
 <meta name="DC.Title" content="{$title}"/>
 <meta name="DC.Language" content="(SCHEME=iso639) en"/> 
 <meta name="DC.Creator" content="TEI,Oxford University Computing Services, 13 Banbury Road, Oxford OX2 6NN, United Kingdom"/>
 <meta name="DC.Creator.Address" content="tei@oucs.ox.ac.uk"/>
</xsl:template>

<xsl:template name="bodyHook">
  <xsl:attribute name="background">/Pictures/background.gif</xsl:attribute>
</xsl:template>

<!-- *************** params ************************** -->
<xsl:param name="oddmode">html</xsl:param>
<xsl:param name="TAG"/>
<xsl:variable name="top" select="/"/>
<xsl:param name="STDOUT">true</xsl:param>
<xsl:param name="alignNavigationPanel">left</xsl:param>
<xsl:param name="authorWord"></xsl:param>
<xsl:param name="autoToc"></xsl:param>
<xsl:param name="bottomNavigationPanel">true</xsl:param>
<xsl:param name="cssFile">Stylesheets/tei.css</xsl:param>
<xsl:param name="dateWord"></xsl:param>
<xsl:param name="feedbackURL">http://www.tei-c.org/Consortium/TEI-contact.html</xsl:param>
<xsl:param name="feedbackWords">Contact</xsl:param>
<xsl:param name="homeURL">http://www.tei-c.org/</xsl:param>
<xsl:param name="homeWords">TEI Home</xsl:param>
<xsl:param name="institution">Text Encoding Initiative</xsl:param>
<xsl:param name="linksWidth">20%</xsl:param>
<xsl:param name="parentURL"></xsl:param>
<xsl:param name="parentWords"></xsl:param>
<xsl:param name="searchURL">http://search.ox.ac.uk/web/related/natproj/tei</xsl:param>
<xsl:param name="searchWords">Search this site</xsl:param>
<xsl:param name="showTitleAuthor">1</xsl:param>
<xsl:param name="subTocDepth">-1</xsl:param>
<xsl:param name="topNavigationPanel"></xsl:param>
<xsl:param name="numberHeadings">true</xsl:param>
<xsl:template name="copyrightStatement">Copyright TEI Consortium 2004</xsl:template>
<xsl:template name="logoPicture"><img src="/Pictures/jaco001d.gif" alt="" width="180" />
</xsl:template>


<xsl:template match="html:*">
     <xsl:element name="{local-name()}">
       <xsl:copy-of select="@*"/>
       <xsl:apply-templates/>
     </xsl:element>
</xsl:template>


   <xsl:template match="tei:p/tei:name">
     <b><xsl:apply-templates/></b>
   </xsl:template>



  <xsl:template match="tei:note[@type='proposal']" >
     <p align="right">
       <b>Proposal <xsl:number level="any" count="note"/> :</b>
       <xsl:apply-templates select="text()"/>
</p>

   </xsl:template>



<!-- templates to match Syd's boffo <action> tags -->

   <xsl:template match="tei:note[@type='action']" >
     <p align="right">
       <b>Action <xsl:value-of select="tei:label"/> by <xsl:value-of select="tei:date"/> </b>
       <xsl:text>: </xsl:text>
       <xsl:apply-templates select="text()"/>
</p>

   </xsl:template>

   <xsl:template match="tei:note[@type='action']" mode="table">
     <tr>
       <td><xsl:value-of select="tei:label"/></td>
       <td><xsl:apply-templates select="text()"/></td>
       <td><xsl:value-of select="tei:date"/> </td>
     </tr>
   </xsl:template>

   <xsl:template match="tei:action" >
     <p align="right">
       <b>Action <xsl:value-of select="tei:name"/> by <xsl:value-of select="tei:dueDate"/> </b>
       <xsl:text>: </xsl:text>
       <xsl:apply-templates select="tei:resp"/>
</p>

   </xsl:template>

   <xsl:template match="tei:front">
     <xsl:apply-templates/>
     <xsl:if test="//note[@type='action']">
     <h4>Actions</h4>
     <table>
       <tr>
        <td>Resp</td><td>action</td><td>due date</td>
       </tr>
     <xsl:apply-templates select="//tei:note[@type='action']" mode="table"/>
</table>
     </xsl:if>

   </xsl:template>




</xsl:stylesheet>


