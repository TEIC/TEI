<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.w3.org/1999/xhtml"
  version="1.0"
  exclude-result-prefixes="exsl estr edate a fo local rng tei teix xd"
  extension-element-prefixes="exsl estr edate" 
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:edate="http://exslt.org/dates-and-times"
  xmlns:estr="http://exslt.org/strings" 
  xmlns:exsl="http://exslt.org/common"
  xmlns:fo="http://www.w3.org/1999/XSL/Format"
  xmlns:html="http://www.w3.org/1999/xhtml"
  xmlns:local="http://www.pantor.com/ns/local"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:xd="http://www.pnp-software.com/XSLTdoc"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xd:doc type="stylesheet">
    <xd:short> TEI stylesheet dealing with elements from the header module,
      making HTML output. </xd:short>
    <xd:detail> This library is free software; you can redistribute it and/or
      modify it under the terms of the GNU Lesser General Public License as
      published by the Free Software Foundation; either version 2.1 of the
      License, or (at your option) any later version. This library is
      distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
      without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
      PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
      details. You should have received a copy of the GNU Lesser General Public
      License along with this library; if not, write to the Free Software
      Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA </xd:detail>
    <xd:author>See AUTHORS</xd:author>
    <xd:cvsId>$Id: header.xsl 4801 2008-09-13 10:05:32Z rahtz $</xd:cvsId>
    <xd:copyright>2008, TEI Consortium</xd:copyright>
  </xd:doc>

  <xsl:key name="ALL-RENDITION" match="@rendition[not(starts-with(.,'#'))]" use="1"/>

  <xsl:key name="RENDITION" match="@rendition[not(starts-with(.,'#'))]" use="."/>

  <xd:doc>
    <xd:short>Process elements tei:teiHeader</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:teiHeader"/>

  <xd:doc>
    <xd:short>make a local style section from rendition elements in the header</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  
  <xsl:template name="generateLocalCSS">
    <xsl:if
	test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:encodingDesc/tei:tagsDecl/tei:rendition">
      <style type="text/css">
	<xsl:for-each
	    select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:encodingDesc/tei:tagsDecl/tei:rendition">
	  <xsl:text>&#10;.</xsl:text>
	  <xsl:value-of select="@xml:id"/>
	  <xsl:text> {&#10;	</xsl:text>
	  <xsl:value-of select="."/>
	  <xsl:text>&#10;}</xsl:text>
	</xsl:for-each>
	<xsl:text>&#10;</xsl:text>
      </style>
    </xsl:if>
    <xsl:if test="count(key('ALL-RENDITION',1))&gt;0">
      <style type="text/css">
	<xsl:for-each select="key('ALL-RENDITION',1)">
	  <xsl:variable name="pointer">
	    <xsl:value-of select="."/>
	  </xsl:variable>
	  <xsl:for-each select="key('RENDITION',$pointer)[1]">
	    <xsl:for-each select="document($pointer)">
	      <xsl:text>&#10;.</xsl:text>
	      <xsl:value-of select="@xml:id"/>
	      <xsl:text> {&#10;	</xsl:text>
	      <xsl:value-of select="."/>
	      <xsl:text>&#10;}</xsl:text>
	    </xsl:for-each>
	  </xsl:for-each>
	</xsl:for-each>
      </style>
    </xsl:if>
  </xsl:template>

  <xd:doc>
    <xd:short>rendition elements in the header</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  


</xsl:stylesheet>
