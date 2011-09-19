<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.w3.org/1999/xhtml"  xmlns:xlink="http://www.w3.org/1999/xlink"
                xmlns:dbk="http://docbook.org/ns/docbook"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:xhtml="http://www.w3.org/1999/xhtml"
                xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns:fo="http://www.w3.org/1999/XSL/Format"
                xmlns:html="http://www.w3.org/1999/xhtml"

                
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="a fo dbk xlink xhtml rng tei teix"
                version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet dealing with elements from the textcrit
      module, making HTML output. </p>
         <p> This library is free software; you can redistribute it and/or
      modify it under the terms of the GNU Lesser General Public License as
      published by the Free Software Foundation; either version 2.1 of the
      License, or (at your option) any later version. This library is
      distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
      without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
      PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
      details. You should have received a copy of the GNU Lesser General Public
      License along with this library; if not, write to the Free Software
      Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA xs </p>
         <p>Author: See AUTHORS</p>
         <p>Id: $Id$</p>
         <p>Copyright: 2011, TEI Consortium</p>
      </desc>
   </doc>

   <xsl:template name="appReading">
     <xsl:param name="lemma"/>
     <xsl:param name="lemmawitness"/>
     <xsl:param name="readings"/>
     <xsl:value-of select="$lemma"/>
      <xsl:variable name="identifier">
         <xsl:text>App</xsl:text>
         <xsl:choose>
	   <xsl:when test="@xml:id">
	     <xsl:value-of select="@xml:id"/>
	   </xsl:when>
	   <xsl:when test="@n">
	     <xsl:value-of select="@n"/>
	   </xsl:when>
	   <xsl:otherwise>
	     <xsl:number count="tei:app" level="any"/>
	   </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>

      <xsl:choose>
       <xsl:when test="$footnoteFile">
	 <a class="notelink" href="{$masterFile}-notes.html#{$identifier}">
	   <sup>
	     <xsl:call-template name="appN"/>
	   </sup>
	 </a>
       </xsl:when>
       <xsl:otherwise>
	 <a class="notelink" href="#{$identifier}">
	   <sup>
	     <xsl:call-template name="appN"/>
	   </sup>
	 </a>
       </xsl:otherwise>
      </xsl:choose>

  </xsl:template>


   <xsl:template name="appN">
      <xsl:choose>
         <xsl:when test="@n">
            <xsl:value-of select="@n"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:number from="tei:text" level="any"/>
         </xsl:otherwise>
      </xsl:choose>
   </xsl:template>

   <xsl:template match="tei:app" mode="printnotes">
      <xsl:variable name="identifier">
         <xsl:text>App</xsl:text>
         <xsl:choose>
            <xsl:when test="@xml:id">
	              <xsl:value-of select="@xml:id"/>
            </xsl:when>
            <xsl:when test="@n">
	              <xsl:value-of select="@n"/>
            </xsl:when>
            <xsl:otherwise>
	              <xsl:number count="tei:app" level="any"/>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>
      <div class="note">
         <xsl:call-template name="makeAnchor">
            <xsl:with-param name="name" select="$identifier"/>
         </xsl:call-template>
         <span class="noteLabel">
            <xsl:call-template name="appN"/>
            <xsl:text>. </xsl:text>
         </span>
         <span class="noteBody">
            <xsl:apply-templates/>
         </span>
      </div>
  
   </xsl:template>

</xsl:stylesheet>