<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.w3.org/1999/xhtml"
                xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns:fo="http://www.w3.org/1999/XSL/Format"
                xmlns:html="http://www.w3.org/1999/xhtml"

                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="#default a fo rng tei teix"
                version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet dealing with elements from the corpus module,
      making HTML output. </p>
         <p> This library is free software; you can redistribute it and/or
      modify it under the terms of the GNU Lesser General Public License as
      published by the Free Software Foundation; either version 2.1 of the
      License, or (at your option) any later version. This library is
      distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
      without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
      PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
      details. You should have received a copy of the GNU Lesser General Public
      License along with this library; if not, write to the Free Software
      Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA </p>
         <p>Author: See AUTHORS</p>
         <p>Id: $Id$</p>
         <p>Copyright: 2008, TEI Consortium</p>
      </desc>
   </doc>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element catRef</desc>
   </doc>
  <xsl:template match="tei:catRef">
      <xsl:variable name="W">
         <xsl:choose>
            <xsl:when test="starts-with(@target,'#')">
               <xsl:value-of select="substring-after(@target,'#')"/>
            </xsl:when>
            <xsl:otherwise>
               <xsl:value-of select="@target"/>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>
      <xsl:if test="preceding-sibling::tei:catRef">
         <xsl:text> 
    </xsl:text>
      </xsl:if>
      <em>
         <xsl:value-of select="@scheme"/>
      </em>: <xsl:apply-templates select="key('IDS',$W)/catDesc"/>
   </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element teiCorpus</desc>
   </doc>
  <xsl:template match="tei:teiCorpus">
      <html>
         <xsl:call-template name="addLangAtt"/>
         <head>
            <title>
               <xsl:apply-templates select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title/text()"/>
            </title>
            <xsl:call-template name="includeCSS"/>
            <xsl:call-template name="cssHook"/>
         </head>
         <body class="simple">
            <xsl:call-template name="bodyHook"/>
            <xsl:call-template name="bodyJavascriptHook"/>
	    <div class="stdheader">
	      <xsl:call-template name="stdheader">
		<xsl:with-param name="title">
		  <xsl:apply-templates select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title"/>
		</xsl:with-param>
	      </xsl:call-template>
	    </div>
	    <xsl:call-template name="corpusBody"/>
            <xsl:call-template name="stdfooter"/>
            <xsl:call-template name="bodyEndHook"/>
         </body>
      </html>
  </xsl:template>
  <xsl:template match="tei:teiCorpus" mode="split">
      <xsl:variable name="BaseFile">
         <xsl:value-of select="$masterFile"/>
         <xsl:call-template name="addCorpusID"/>
      </xsl:variable>
      <xsl:if test="$verbose='true'">
         <xsl:message>TEI HTML: run start hook template teiStartHook</xsl:message>
      </xsl:if>
      <xsl:call-template name="teiStartHook"/>
      <xsl:if test="$verbose='true'">
         <xsl:message>TEI HTML in corpus splitting mode, base file is <xsl:value-of select="$BaseFile"/>
         </xsl:message>
      </xsl:if>
      <xsl:variable name="outName">
         <xsl:call-template name="outputChunkName">
	           <xsl:with-param name="ident">
	              <xsl:value-of select="$BaseFile"/>
	           </xsl:with-param>
         </xsl:call-template>
      </xsl:variable>
    
      <xsl:if test="$verbose='true'">
         <xsl:message>Opening file <xsl:value-of select="$outName"/>
         </xsl:message>
      </xsl:if>
      <xsl:result-document doctype-public="{$doctypePublic}" doctype-system="{$doctypeSystem}"
                           encoding="{$outputEncoding}"
                           href="{$outName}"
                           method="{$outputMethod}">
      
         <html>
	   <xsl:call-template name="addLangAtt"/>
	   <head>
	     <title>
	       <xsl:apply-templates select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title/text()"/>
	     </title>
	     <xsl:call-template name="includeCSS"/>
	     <xsl:call-template name="cssHook"/>
	   </head>
	   <body class="simple">
	     <xsl:call-template name="bodyHook"/>
	     <xsl:call-template name="bodyJavascriptHook"/>
	     <div class="stdheader">
	       <xsl:call-template name="stdheader">
		 <xsl:with-param name="title">
		   <xsl:apply-templates select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[1]"/>
		 </xsl:with-param>
	       </xsl:call-template>
	     </div>
	     <xsl:call-template name="corpusBody"/>
	     <xsl:call-template name="stdfooter"/>
	     <xsl:call-template name="bodyEndHook"/>
	   </body>
         </html>
      </xsl:result-document>
      <xsl:if test="$verbose='true'">
         <xsl:message>Closing file <xsl:value-of select="$outName"/>
         </xsl:message>
      </xsl:if>
      <xsl:if test="$verbose='true'">
         <xsl:message>TEI HTML: run end hook template teiEndHook</xsl:message>
      </xsl:if>
      <xsl:call-template name="teiEndHook"/>
      <xsl:apply-templates select="tei:TEI" mode="split"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[html] </desc>
   </doc>
  <xsl:template name="corpusBody">
    <xsl:call-template name="mainTOC"/>
  </xsl:template>
</xsl:stylesheet>