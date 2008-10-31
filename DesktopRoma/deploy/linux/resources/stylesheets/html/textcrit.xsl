<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet
  exclude-result-prefixes="exsl estr edate a fo local dbk xlink xhtml rng tei teix xd"
  extension-element-prefixes="exsl estr edate saxon7 saxon6" version="1.0"
  xmlns:xlink="http://www.w3.org/1999/xlink"
  xmlns:dbk="http://docbook.org/ns/docbook"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:xhtml="http://www.w3.org/1999/xhtml"
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:edate="http://exslt.org/dates-and-times"
  xmlns:estr="http://exslt.org/strings" xmlns:exsl="http://exslt.org/common"
  xmlns:fo="http://www.w3.org/1999/XSL/Format"
  xmlns:html="http://www.w3.org/1999/xhtml"
  xmlns:local="http://www.pantor.com/ns/local"
  xmlns:saxon6="http://icl.com/saxon" xmlns:saxon7="http://saxon.sf.net/"
  xmlns:xd="http://www.pnp-software.com/XSLTdoc"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xd:doc type="stylesheet">
    <xd:short> TEI stylesheet dealing with elements from the textcrit
      module, making HTML output. </xd:short>
    <xd:detail> This library is free software; you can redistribute it and/or
      modify it under the terms of the GNU Lesser General Public License as
      published by the Free Software Foundation; either version 2.1 of the
      License, or (at your option) any later version. This library is
      distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
      without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
      PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
      details. You should have received a copy of the GNU Lesser General Public
      License along with this library; if not, write to the Free Software
      Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA xs </xd:detail>
    <xd:author>See AUTHORS</xd:author>
    <xd:cvsId>$Id: textcrit.xsl 4801 2008-09-13 10:05:32Z rahtz $</xd:cvsId>
    <xd:copyright>2008, TEI Consortium</xd:copyright>
  </xd:doc>

 <xsl:template match="tei:app">
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
      <xsl:when test="$footnoteFile='true'">
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

<xsl:template match="tei:rdg">
  <span class="rdg">
    <xsl:apply-templates/>
  </span>
  <xsl:text> (</xsl:text>
  <a href="{@wit}">
  <xsl:choose>
    <xsl:when test="starts-with(@wit,'#')">
      <xsl:value-of select="substring-after(@wit,'#')"/>
    </xsl:when>
    <xsl:otherwise>
    <xsl:for-each select="document(@wit)">
      <xsl:value-of select="."/>
    </xsl:for-each>
    </xsl:otherwise>
  </xsl:choose>
  </a>
  <xsl:text>)</xsl:text>
</xsl:template>

</xsl:stylesheet>
