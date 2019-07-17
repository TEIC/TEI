<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns="http://www.tei-c.org/ns/1.0"
    xmlns:tei="http://www.tei-c.org/ns/1.0" exclude-result-prefixes="tei xs" version="2.0"
    xpath-default-namespace="http://www.tei-c.org/ns/1.0">

  <xsl:include href="mapatts.xsl"/>
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
        <desc>
            <p> TEI utility stylesheet for transformation from TEI P5 to TEI Simple</p>
            <p>This software is dual-licensed: 1. Distributed under a Creative Commons
                Attribution-ShareAlike 3.0 Unported License
                http://creativecommons.org/licenses/by-sa/3.0/ 2.
                http://www.opensource.org/licenses/BSD-2-Clause All rights reserved. Redistribution
                and use in source and binary forms, with or without modification, are permitted
                provided that the following conditions are met: * Redistributions of source code
                must retain the above copyright notice, this list of conditions and the following
                disclaimer. * Redistributions in binary form must reproduce the above copyright
                notice, this list of conditions and the following disclaimer in the documentation
                and/or other materials provided with the distribution. This software is provided by
                the copyright holders and contributors "as is" and any express or implied
                warranties, including, but not limited to, the implied warranties of merchantability
                and fitness for a particular purpose are disclaimed. In no event shall the copyright
                holder or contributors be liable for any direct, indirect, incidental, special,
                exemplary, or consequential damages (including, but not limited to, procurement of
                substitute goods or services; loss of use, data, or profits; or business
                interruption) however caused and on any theory of liability, whether in contract,
                strict liability, or tort (including negligence or otherwise) arising in any way out
                of the use of this software, even if advised of the possibility of such damage. </p>
            <p>Author: See AUTHORS</p>
            <p>Copyright: 2014, TEI Consortium</p>
        </desc>
    </doc>


    <xsl:variable name="transtable">
        <list xmlns="http://www.tei-c.org/ns/1.0">
            <item>
                <in>persName</in>
                <out>person</out>
            </item>
            <item>
                <in>forename</in>
                <out>forename</out>
            </item>
            <item>
                <in>surname</in>
                <out>surname</out>
            </item>
            <item>
                <in>genName</in>
                <out>personGenName</out>
            </item>
            <item>
                <in>roleName</in>
                <out>personRoleName</out>
            </item>
            <item>
                <in>addName</in>
                <out>personAddName</out>
            </item>
            <item>
                <in>nameLink</in>
                <out>nameLink</out>
            </item>
            <item>
                <in>orgName</in>
                <out>organisation</out>
            </item>
            <item>
                <in>country</in>
                <out>country</out>
            </item>
            <item>
                <in>geogName</in>
                <out>placeGeog</out>
            </item>
            <item>
                <in>placeName</in>
                <out>place</out>
            </item>
            <item>
                <in>caesura</in>
                <out>milestone</out>
                <add>unit</add>
            </item>
            <item>
                <in>code</in>
                <out>hi</out>
                <add>rendition</add>
            </item>
            <item>
                <in>emph</in>
                <out>hi</out>
                <add>rendition</add>
            </item>
            <item>
                <in>soCalled</in>
                <out>q</out>
                <add>type</add>
            </item>
            <item>
                <in>ptr</in>
                <out>ref</out>
            </item>

            <item>
                <in>term</in>
                <out>seg</out>
                <add>type</add>
            </item>

            <item>
                <in>said</in>
                <out>q</out>
                <add>type</add>
            </item>

        </list>
    </xsl:variable>

    <xsl:template match="/">
	<xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="TEI">
      <xsl:copy>
	<xsl:attribute name="rendition">tei:teisimple</xsl:attribute>
        <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
      </xsl:copy>
    </xsl:template>

    <xsl:template match="text">
      <xsl:copy>
	  <xsl:apply-templates/>
      </xsl:copy>

    </xsl:template>

    <!-- merge into name, keep attributes and add @type with translated name of original elements -->
    <xsl:template
        match="persName | orgName | addName | nameLink | roleName |
	       forename | surname | genName | country | placeName | geogName">
        <xsl:variable name="lname" select="local-name()"/>
        <xsl:element name="name">
            <xsl:attribute name="type">
                <xsl:value-of select="$transtable//item[in=$lname]/out"/>
            </xsl:attribute>
            <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
        </xsl:element>
    </xsl:template>


    <!-- merge into element named according to transl table, keep attributes and add attribute with name of original elements -->
    <xsl:template match="soCalled | code | emph | said | term[ancestor::text]">
        <xsl:variable name="lname" select="local-name()"/>
        <xsl:variable name="tname" select="$transtable//item[in=$lname]/out"/>
        <xsl:variable name="aname" select="$transtable//item[in=$lname]/add"/>

        <xsl:element name="{$tname}">
            <xsl:if test="string($aname)">
                <xsl:attribute name="{$aname}">
		  <xsl:if test="$aname='rendition'">simple:</xsl:if>
                    <xsl:value-of select="$lname"/>
                </xsl:attribute>
            </xsl:if>
            <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
        </xsl:element>
    </xsl:template>


    <!-- merge into empty element -->
    <xsl:template match="ptr | caesura">
        <xsl:variable name="lname" select="local-name()"/>
        <xsl:variable name="tname" select="$transtable//item[in=$lname]/out"/>
        <xsl:variable name="aname" select="$transtable//item[in=$lname]/add"/>

        <xsl:element name="{$tname}">
            <xsl:if test="string($aname)">
                <xsl:attribute name="{$aname}">
                    <xsl:value-of select="$lname"/>
                </xsl:attribute>
            </xsl:if>
            <xsl:apply-templates select="@*"/>
        </xsl:element>
    </xsl:template>
    <xsl:template match="@rendition">
        <xsl:choose>
	  <xsl:when test="starts-with(.,'#') and not
			  (id(substring(.,2)))">
	      <xsl:attribute name="rendition">
		<xsl:for-each select="tokenize(.,' ')">
		  <xsl:text>simple:</xsl:text>
		  <xsl:value-of select="substring(.,2)"/>
		  <xsl:text> </xsl:text>
		</xsl:for-each>
	      </xsl:attribute>
	  </xsl:when>

            <xsl:when test="not(../@rend)">
                <xsl:copy-of select="."/>
            </xsl:when>
            <xsl:otherwise>
                <!-- merge rend and rendition -->
                <xsl:attribute name="rendition">
                    <xsl:value-of select="."/>
		    <xsl:text> </xsl:text>
		    <xsl:for-each select="tokenize(.,' ')">
		      <xsl:text>simple:</xsl:text>
		      <xsl:value-of select="."/>
		      <xsl:text> </xsl:text>
		    </xsl:for-each>
                </xsl:attribute>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    

    <xsl:template match="publicationStmt">
      <publicationStmt>
	<xsl:choose>
	<xsl:when test="p">
	    <xsl:apply-templates/>
	</xsl:when>
	  <xsl:when test="publisher or authority or distributor">
	  <xsl:apply-templates select="publisher|authority|distributor" />
	  <xsl:apply-templates select="*[not(self::publisher or
				       self::distributor or
				       self::authority)]" />
	</xsl:when>
	<xsl:otherwise>
	  <p>
	    <xsl:apply-templates/>
	  </p>
	</xsl:otherwise>
      </xsl:choose>
      </publicationStmt>
    </xsl:template>

    <xsl:template match="@*|text()|comment()|processing-instruction()">
        <xsl:copy-of select="."/>
    </xsl:template>

    <xsl:template match="*">
        <xsl:copy>
            <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
        </xsl:copy>
    </xsl:template>


 <!-- meaningless or default attributes -->
  <xsl:template match="@anchored">
    <xsl:if test="not(. = 'true')">
      <xsl:attribute name="anchored">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@sample">
    <xsl:if test="not(. = 'complete')">
      <xsl:attribute name="sample">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@org">
    <xsl:if test="not(. = 'uniform')">
      <xsl:attribute name="org">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@part">
    <xsl:if test="not(. = 'N')">
      <xsl:attribute name="part">
        <xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
    <xsl:template match="note/@place[.='unspecified']"/>
    <xsl:template match="sourceDesc/@default"/>
    <xsl:template match="biblFull/@default"/>
    <xsl:template match="bibl/@default"/>
    <xsl:template match="projectDesc/@default"/>
    <xsl:template match="editorialDecl/@default"/>

<!-- common, but illegal, shorthands -->
    <xsl:template match="sup">
      <hi rendition="simple:superscript">
	    <xsl:apply-templates/>
      </hi>
    </xsl:template>
    <xsl:template match="sub">
      <hi rendition="simple:subscript">
	    <xsl:apply-templates/>
      </hi>
    </xsl:template>


</xsl:stylesheet>
