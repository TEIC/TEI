<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.w3.org/1999/xhtml" 
                xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns:fo="http://www.w3.org/1999/XSL/Format"
                xmlns:html="http://www.w3.org/1999/xhtml"

                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="2.0"
                exclude-result-prefixes="a fo rng tei teix">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet dealing with elements from the header module,
      making HTML output. </p>
         <p>This software is dual-licensed:

1. Distributed under a Creative Commons Attribution-ShareAlike 3.0
Unported License http://creativecommons.org/licenses/by-sa/3.0/ 

2. http://www.opensource.org/licenses/BSD-2-Clause
		
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

* Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

This software is provided by the copyright holders and contributors
"as is" and any express or implied warranties, including, but not
limited to, the implied warranties of merchantability and fitness for
a particular purpose are disclaimed. In no event shall the copyright
holder or contributors be liable for any direct, indirect, incidental,
special, exemplary, or consequential damages (including, but not
limited to, procurement of substitute goods or services; loss of use,
data, or profits; or business interruption) however caused and on any
theory of liability, whether in contract, strict liability, or tort
(including negligence or otherwise) arising in any way out of the use
of this software, even if advised of the possibility of such damage.
</p>
         <p>Author: See AUTHORS</p>
         <p>Id: $Id$</p>
         <p>Copyright: 2011, TEI Consortium</p>
      </desc>
   </doc>

  <xsl:key name="ALL-EXTRENDITION" match="@rendition[not(starts-with(.,'#'))]" use="1"/>
  <xsl:key name="EXTRENDITION"     match="@rendition[not(starts-with(.,'#'))]" use="."/>
  <xsl:key name="ALL-LOCALRENDITION" match="tei:rendition" use='1'/>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element teiHeader</desc>
   </doc>
  <xsl:template match="tei:teiHeader"/>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>make a style section from rendition elements in the header</desc>
   </doc>
  
  <xsl:template name="generateLocalCSS">
      <xsl:if test="key('ALL-LOCALRENDITION',1)">
         <style type="text/css">
	   <xsl:for-each select="key('ALL-LOCALRENDITION',1)">
	     <xsl:text>&#10;.</xsl:text>
	     <xsl:value-of select="@xml:id"/>
	     <xsl:if test="@scope">
	       <xsl:text>:</xsl:text>
	       <xsl:value-of select="@scope"/>
	     </xsl:if>
	     <xsl:text> {&#10;	</xsl:text>
	     <xsl:value-of select="."/>
	     <xsl:text>&#10;}</xsl:text>
	   </xsl:for-each>
	   <xsl:text>&#10;</xsl:text>
         </style>
      </xsl:if>
      <xsl:if test="key('ALL-EXTRENDITION',1)">
         <style type="text/css">
	   <xsl:for-each select="key('ALL-EXTRENDITION',1)">
	     <xsl:variable name="pointer">
	       <xsl:value-of select="."/>
	     </xsl:variable>
	     <xsl:for-each select="key('RENDITION',$pointer)[1]">
	       <xsl:for-each select="document($pointer)">
		 <xsl:text>&#10;.</xsl:text>
		 <xsl:value-of select="@xml:id"/>
		 <xsl:if test="@scope">
		   <xsl:text>:</xsl:text>
		   <xsl:value-of select="@scope"/>
		 </xsl:if>
		 <xsl:text> {&#10;</xsl:text>
		 <xsl:value-of select="."/>
		 <xsl:text>&#10;}</xsl:text>
	       </xsl:for-each>
	     </xsl:for-each>
	   </xsl:for-each>
         </style>
      </xsl:if>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[html] processing licence</desc>
  </doc>
  <xsl:template match="tei:availability"  mode="copyrighttext">
    <xsl:apply-templates mode="copyrighttext"/>
  </xsl:template>

  <xsl:template match="tei:licence"  mode="copyrighttext">
    <xsl:if test="@target">
      <xsl:text>[</xsl:text>
      <xsl:value-of select="@target"/>
      <xsl:text>] </xsl:text>
    </xsl:if>
    <xsl:apply-templates/>
  </xsl:template>
  

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element catRef</desc>
   </doc>
  <xsl:template match="tei:catRef">
      <xsl:variable name="W">
         <xsl:choose>
            <xsl:when test="starts-with(@target,'#')">
               <xsl:value-of select="substring(@target,2)"/>
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
      </em>: <xsl:apply-templates select="id($W)/catDesc"/>
   </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[html] create a list of authors, editors etc as a paragraph</desc>
   </doc>
  <xsl:template name="generateAuthorList">
      <xsl:variable name="realauthor">
         <xsl:call-template name="generateAuthor"/>
      </xsl:variable>
      <xsl:variable name="revauthor">
         <xsl:call-template name="generateRevAuthor"/>
      </xsl:variable>
      <xsl:variable name="editor">
        <xsl:call-template name="generateEditor"/>
      </xsl:variable>
      <xsl:if test="not($realauthor = '')">
        <p xmlns="http://www.w3.org/1999/xhtml" class="mainAuthor">
         <xsl:text> </xsl:text>
         <xsl:call-template name="i18n">
            <xsl:with-param name="word">authorWord</xsl:with-param>
         </xsl:call-template>
          <xsl:text>: </xsl:text>
         <xsl:copy-of select="$realauthor"/>
        </p>
      </xsl:if>
      <xsl:if test="not($revauthor = '')">
      <p class="mainRevAuthor" xmlns="http://www.w3.org/1999/xhtml">
         <xsl:text> (</xsl:text>
         <xsl:call-template name="i18n">
            <xsl:with-param name="word">revisedWord</xsl:with-param>
         </xsl:call-template>
         <xsl:text> </xsl:text>
         <xsl:copy-of select="$revauthor"/>
         <xsl:text>)</xsl:text>
      </p>
    </xsl:if>
    <xsl:if test="not($editor = '')">
      <p class="mainEditor" xmlns="http://www.w3.org/1999/xhtml">
         <xsl:call-template name="i18n">
            <xsl:with-param name="word">editorWord</xsl:with-param>
         </xsl:call-template>
        <xsl:text>: </xsl:text>
        <xsl:copy-of select="$editor"/>
      </p>
      </xsl:if>
  </xsl:template>

  <xsl:template name="generateEdition">
    <p xmlns="http://www.w3.org/1999/xhtml" class="editionStmt">
      <xsl:apply-templates select="/(tei:teiCorpus|tei:TEI)/tei:teiHeader/tei:fileDesc/tei:editionStmt"/>
    </p>
  </xsl:template>

</xsl:stylesheet>