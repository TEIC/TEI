<xsl:stylesheet 
    xmlns:xd="http://www.pnp-software.com/XSLTdoc" 
    xmlns:tei="http://www.tei-c.org/ns/1.0" 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xmlns:XSL="http://www.w3.org/1999/XSL/Transform" 
    exclude-result-prefixes="tei xd"
    version="1.0">

<xd:doc type="stylesheet">
    <xd:short>
      TEI stylesheet for making documentation of stylesheet parameters
      </xd:short>
    <xd:detail>
    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

   
   
      </xd:detail>
    <xd:author>Sebastian Rahtz sebastian.rahtz@oucs.ox.ac.uk</xd:author>
    <xd:cvsId>$Id$</xd:cvsId>
    <xd:copyright>2005, TEI Consortium</xd:copyright>
  </xd:doc>


<xsl:key name="XDS" match="xd:doc" use="@class"/>

<xsl:output indent="yes" encoding="utf-8" omit-xml-declaration="yes"/>

<xsl:include href="verbatim.xsl"/>

<xsl:template match="div">
<xsl:copy>
<xsl:apply-templates select="@*|text()|*"/>
<xsl:if test="@id">
<div>
  <head>Variables</head>
  <table rend="rules">
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
      <xsl:with-param name="Type">html</xsl:with-param>
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
      <xsl:with-param name="Type">html</xsl:with-param>
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
  <xsl:variable name="I" select="@id"/>
    <xsl:for-each select="document(concat($Type,'/tei-param.xsl'))">
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
	      <xsl:choose>
		<xsl:when test="starts-with(xd:short,'[')">
		  <xsl:value-of select="substring-after(xd:short,']')"/>
		</xsl:when>
		<xsl:when test="xd:short">
		  <xsl:apply-templates select="xd:short/*"/>
		</xsl:when>
		<xsl:when test="starts-with(.,'[')">
		  <xsl:value-of select="substring-after(.,']')"/>
		</xsl:when>
		<xsl:when test="contains(.,'.')">
		  <xsl:value-of select="substring-before(.,'.')"/>
		</xsl:when>
		<xsl:otherwise>
		  <xsl:copy-of select="."/>
		</xsl:otherwise>
	      </xsl:choose>
	      <xsl:for-each select="following-sibling::xsl:*[1]">
		<xsl:choose>
		  <xsl:when test="*">
		    <eg>
		      <xsl:apply-templates select="*|text()"
					   mode="verbatim"/>
		    </eg>
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
  <xsl:variable name="I" select="@id"/>
    <xsl:for-each select="document(concat($Type,'/tei-param.xsl'))">
      <xsl:for-each select="key('XDS',$I)">
	<xsl:if test="not(following-sibling::xsl:*[1]/self::xsl:template)">
      <row>
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
	    <xsl:value-of	select="following-sibling::xsl:*[1]/@name"/>
	  </hi>
	</cell>
	<cell>
	  <xsl:choose>
	    <xsl:when test="starts-with(xd:short,'[')">
	      <xsl:value-of select="substring-after(xd:short,']')"/>
	    </xsl:when>
	    <xsl:when test="xd:short">
	      <xsl:apply-templates  select="xd:short"/>
	    </xsl:when>
	    <xsl:when test="starts-with(.,'[')">
	      <xsl:value-of select="substring-after(.,']')"/>
	    </xsl:when>
	    <xsl:when test="contains(.,'.')">
	      <xsl:value-of select="substring-before(.,'.')"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:apply-templates/>
	    </xsl:otherwise>
	  </xsl:choose>
	  <xsl:text> [</xsl:text>
	  <code><xsl:value-of select="@type"/></code>]
	</cell>
	<cell>
	  <xsl:for-each select="following-sibling::xsl:*[1]">
	    <xsl:choose>
	      <xsl:when test="*">
		<eg>
		<xsl:apply-templates select="*|text()"
				     mode="verbatim"/>
		</eg>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:value-of select="."/>
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:for-each>
	</cell>
      </row>
	</xsl:if>
      </xsl:for-each>
    </xsl:for-each>
</xsl:template>


<xsl:template match="xd:short|xd:doc">
  <xsl:apply-templates/>
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

