<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="a rng tei teix"
                version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet dealing with elements from the linking module,
      making LaTeX output. </p>
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
      <desc>Process element anchor</desc>
   </doc>
  <xsl:template match="tei:anchor">
      <xsl:text>\hypertarget{</xsl:text>
      <xsl:value-of select="@xml:id"/>
      <xsl:text>}{}</xsl:text>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[latex] <param name="where">where</param>
      </desc>
   </doc>
  <xsl:template name="generateEndLink">
      <xsl:param name="where"/>
      <xsl:value-of select="$where"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[latex] <param name="ptr">ptr</param>
         <param name="dest">dest</param>
      </desc>
   </doc>
  <xsl:template name="makeExternalLink">
      <xsl:param name="ptr"/>
      <xsl:param name="dest"/>
      <xsl:choose>
         <xsl:when test="$ptr='true'">
            <xsl:text>\url{</xsl:text>
            <xsl:value-of select="$dest"/>
            <xsl:text>}</xsl:text>
         </xsl:when>
         <xsl:otherwise>
            <xsl:text>\xref{</xsl:text>
            <xsl:value-of select="$dest"/>
            <xsl:text>}{</xsl:text>
            <xsl:apply-templates/>
            <xsl:text>}</xsl:text>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[latex] <param name="target">target</param>
         <param name="ptr">ptr</param>
         <param name="dest">dest</param>
         <param name="body">body</param>
      </desc>
   </doc>
  <xsl:template name="makeInternalLink">
      <xsl:param name="target"/>
      <xsl:param name="class"/>
      <xsl:param name="ptr"/>
      <xsl:param name="dest"/>
      <xsl:param name="body"/>
      <xsl:choose>
         <xsl:when test="key('IDS',$dest)">
            <xsl:choose>
               <xsl:when test="not($body='')">
		 <xsl:text>\hyperlink{</xsl:text>
		 <xsl:value-of select="$dest"/>
		 <xsl:text>}{</xsl:text>
		 <xsl:value-of select="$body"/>
		 <xsl:text>}</xsl:text>
               </xsl:when>
               <xsl:when test="$ptr='true'">
		 <xsl:for-each select="key('IDS',$dest)">
		   <xsl:choose>
		     <xsl:when test="$class='pageref'">
		       <xsl:text>\pageref{</xsl:text>
		       <xsl:value-of select="@xml:id"/>
		       <xsl:text>}</xsl:text>
		     </xsl:when>
		     <xsl:when test="self::tei:note[@xml:id]">
		       <xsl:text>\ref{</xsl:text>
		       <xsl:value-of select="@xml:id"/>
		       <xsl:text>}</xsl:text>
		     </xsl:when>
		     <xsl:when test="self::tei:figure[tei:head and @xml:id]">
		       <xsl:text>\ref{</xsl:text>
		       <xsl:value-of select="@xml:id"/>
		       <xsl:text>}</xsl:text>
		     </xsl:when>
		     <xsl:when test="self::tei:table[tei:head and @xml:id]">
		       <xsl:text>\ref{</xsl:text>
		       <xsl:value-of select="@xml:id"/>
		       <xsl:text>}</xsl:text>
		     </xsl:when>
		     <xsl:when test="starts-with(local-name(.),'div')">
		       <xsl:text>\textit{\hyperref[</xsl:text>
		       <xsl:value-of select="$dest"/>
		       <xsl:text>]{</xsl:text>
		       <xsl:apply-templates mode="xref" select=".">
			 <xsl:with-param name="minimal" select="$minimalCrossRef"/>
		       </xsl:apply-templates>
		       <xsl:text>}}</xsl:text>
		     </xsl:when>
		     <xsl:otherwise>
		       <xsl:text>\hyperlink{</xsl:text>
		       <xsl:value-of select="$dest"/>
		       <xsl:text>}{</xsl:text>
		       <xsl:value-of select="$body"/>
		       <xsl:apply-templates mode="xref" select=".">
			 <xsl:with-param name="minimal" select="$minimalCrossRef"/>
		       </xsl:apply-templates>
		       <xsl:text>}</xsl:text>
		     </xsl:otherwise>
		   </xsl:choose>
		 </xsl:for-each>
               </xsl:when>
               <xsl:otherwise>
		 <xsl:text>\hyperlink{</xsl:text>
		 <xsl:value-of select="$dest"/>
		 <xsl:text>}{</xsl:text>
		 <xsl:value-of select="$body"/>
		 <xsl:apply-templates/>
		 <xsl:text>}</xsl:text>
	       </xsl:otherwise>
            </xsl:choose>
         </xsl:when>
         <xsl:otherwise>
	   <xsl:choose>
	     <xsl:when test="not($body='')">
	       <xsl:value-of select="$body"/>
	     </xsl:when>
	     <xsl:when test="$ptr='true'">
	       <xsl:value-of select="$dest"/>
	     </xsl:when>
	     <xsl:otherwise>
	       <xsl:apply-templates/>
	     </xsl:otherwise>
	   </xsl:choose>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process cross-ref to note</desc>
   </doc>
  <xsl:template match="tei:note" mode="xref">
    <xsl:number level="any"/>
  </xsl:template>

</xsl:stylesheet>