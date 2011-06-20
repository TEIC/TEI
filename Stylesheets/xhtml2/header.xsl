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
	     <xsl:text> {&#10;	</xsl:text>
	     <xsl:value-of select="."/>
	     <xsl:text>;&#10;}</xsl:text>
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
		 <xsl:text> {&#10;</xsl:text>
		 <xsl:value-of select="."/>
		 <xsl:text>;&#10;}</xsl:text>
	       </xsl:for-each>
	     </xsl:for-each>
	   </xsl:for-each>
         </style>
      </xsl:if>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>rendition elements in the header</desc>
   </doc>
  


</xsl:stylesheet>