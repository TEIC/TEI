<xsl:stylesheet 
    xpath-default-namespace="http://www.tei-c.org/ns/1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xmlns:XSL="http://www.w3.org/1999/XSL/Transform" 
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xmlns:sch="http://purl.oclc.org/dsdl/schematron"
    xmlns:m="http://www.w3.org/1998/Math/MathML"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    exclude-result-prefixes="XSL xd xsi sch tei m" 
    xmlns="http://www.tei-c.org/ns/1.0"
    xmlns:xd="http://www.oxygenxml.com/ns/doc/xsl"
    version="2.0">

<doc scope="stylesheet" xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>
      <p>This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.</p>

    <p>This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.</p>

    <p>You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA</p>

    <p>$Id$</p>
    <p>Copyright 2005, TEI Consortium</p>
    </desc>
  </doc>


<xsl:key name="XDS" match="xd:doc" use="@class"/>

<xsl:output indent="yes" encoding="utf-8" omit-xml-declaration="yes"/>

<xsl:include href="../common/verbatim.xsl"/>

<xsl:template match="div">
  <xsl:copy>
    <xsl:apply-templates select="@*|text()|*"/>
    <xsl:if test="@xml:id">
      <div>
	<head>Variables</head>
	<table rend="rules" >
	  <xsl:attribute name="preamble">
	    <xsl:text>P{0.1\textwidth}|P{0.25\textwidth}|P{0.36\textwidth}|P{0.22\textwidth}|</xsl:text>
	  </xsl:attribute>
	  <row role="label">
	    <cell>Type</cell>
	    <cell>Name</cell>
	    <cell>Description</cell>
	    <cell>Default</cell>
	  </row>
	  <xsl:call-template name="listparams">
	    <xsl:with-param name="Type">common</xsl:with-param>
	  </xsl:call-template>
	  <xsl:call-template name="listparams">
	    <xsl:with-param name="Type">xhtml</xsl:with-param>
	  </xsl:call-template>
	  <xsl:call-template name="listparams">
	    <xsl:with-param name="Type">fo</xsl:with-param>
	  </xsl:call-template>
	  <xsl:call-template name="listparams">
	    <xsl:with-param name="Type">latex</xsl:with-param>
	  </xsl:call-template>
	</table>
      </div>
      
      <div>
	<head>Templates</head>
	<xsl:call-template name="listtemplates">
	  <xsl:with-param name="Type">common</xsl:with-param>
	</xsl:call-template>
	<xsl:call-template name="listtemplates">
	  <xsl:with-param name="Type">xhtml</xsl:with-param>
	</xsl:call-template>
	<xsl:call-template name="listtemplates">
	  <xsl:with-param name="Type">fo</xsl:with-param>
	</xsl:call-template>
	<xsl:call-template name="listtemplates">
	  <xsl:with-param name="Type">latex</xsl:with-param>
	</xsl:call-template>
      </div>
    </xsl:if>
  </xsl:copy>
</xsl:template>

<xsl:template name="listtemplates">
  <xsl:param name="Type"/>
  <xsl:variable name="I" select="@xml:id"/>
  <xsl:variable name="Path">
    <xsl:text>../</xsl:text>
    <xsl:value-of select="$Type"/>
    <xsl:text>2</xsl:text>
  </xsl:variable>
  <xsl:for-each select="document(concat($Path,'/tei-param.xsl'))">
    <xsl:if test="count(key('XDS',$I))&gt;0">
      <list type="gloss">
	<xsl:for-each select="key('XDS',$I)">
	  <xsl:if test="following-sibling::xsl:*[1]/self::xsl:template">
	    <label>
	      <hi>
		<xsl:value-of  select="following-sibling::xsl:*[1]/@name"/>
	      </hi>
	  </label>
	     <item>
	     (for <xsl:value-of select="$Type"/>)
	     <xsl:value-of select="xd:desc"/>
	     <xsl:for-each select="following-sibling::xsl:*[1]">
		<xsl:choose>
		  <xsl:when test="*">
		    <egXML xmlns="http://www.tei-c.org/ns/Examples">
		      <xsl:apply-templates select="*|text()"/>
		    </egXML>
		  </xsl:when>
		  <xsl:otherwise>
		    <xsl:value-of select="."/>
		  </xsl:otherwise>
		</xsl:choose>
	      </xsl:for-each>
	  </item>
	</xsl:if>
	</xsl:for-each>
      </list>
    </xsl:if>
  </xsl:for-each>
</xsl:template>

<xsl:template name="listparams">
  <xsl:param name="Type"/>
  <xsl:variable name="I" select="@xml:id"/>
  <xsl:variable name="Path">
	<xsl:text>../</xsl:text>
	<xsl:value-of select="$Type"/>
	<xsl:text>2</xsl:text>
	</xsl:variable>

    <xsl:for-each select="document(concat($Path,'/tei-param.xsl'))">
      <xsl:for-each select="key('XDS',$I)">
	<xsl:if test="not(following-sibling::xsl:*[1]/self::xsl:template)">
	  <xsl:variable name="row">
	    <cell>
	      <xsl:choose>
		<xsl:when test="$Type='common'"></xsl:when>
		<xsl:otherwise>
		  <xsl:value-of select="$Type"/>
		</xsl:otherwise>
	      </xsl:choose>
	    </cell>
	    <cell>
	      <hi>
		<xsl:value-of select="following-sibling::xsl:*[1]/@name"/>
	      </hi>
	    </cell>
	    <cell>
	      <xsl:value-of select="xd:desc"/>
	      <xsl:text> [</xsl:text>
	      <code><xsl:value-of select="@type"/></code>
	      <xsl:text>]</xsl:text>
	    </cell>
	  </xsl:variable>

	  <xsl:for-each select="following-sibling::xsl:*[1]">
	    <xsl:choose>
	      <xsl:when test="*">
		<row>
		  <xsl:copy-of select="$row"/>
		  <cell></cell>
		</row>
		<row>
		  <cell cols="4">
		    <xsl:if test="*|text()">
		      <egXML xmlns="http://www.tei-c.org/ns/Examples">
			<xsl:apply-templates select="*|text()"/>
		      </egXML>
		    </xsl:if>
		  </cell>
		</row>
	      </xsl:when>
	      <xsl:otherwise>
		<row>
		  <xsl:copy-of select="$row"/>
		  <cell>
		    <xsl:value-of select="."/>
		  </cell>
		</row>
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:for-each>
	</xsl:if>
      </xsl:for-each>
    </xsl:for-each>
</xsl:template>

<xsl:template match="*">
 <xsl:copy>
  <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
 </xsl:copy>
</xsl:template>

<xsl:template match="text()|processing-instruction()|comment()|@*">
    <xsl:copy-of select="."/>
</xsl:template>

</xsl:stylesheet>

