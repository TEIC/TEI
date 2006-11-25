<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet
  exclude-result-prefixes="xd exsl estr edate a fo local rng tei teix"
  extension-element-prefixes="exsl estr edate" version="1.0"
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:edate="http://exslt.org/dates-and-times"
  xmlns:estr="http://exslt.org/strings" xmlns:exsl="http://exslt.org/common"
  xmlns:fo="http://www.w3.org/1999/XSL/Format"
  xmlns:html="http://www.w3.org/1999/xhtml"
  xmlns:local="http://www.pantor.com/ns/local"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:xd="http://www.pnp-software.com/XSLTdoc"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xd:doc type="stylesheet">
    <xd:short> TEI stylesheet dealing with elements from the drama module,
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
    <xd:cvsId>$Id$</xd:cvsId>
    <xd:copyright>2005, TEI Consortium</xd:copyright>
  </xd:doc>
  <xd:doc>
    <xd:short>Process elements tei:actor</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:actor">
    <em>
      <xsl:apply-templates/>
    </em>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:camera</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:camera">
    <em>
      <xsl:apply-templates/>
    </em>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:caption</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:caption">
    <em>
      <xsl:apply-templates/>
    </em>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:castGroup</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:castGroup">
    <ul>
      <xsl:apply-templates/>
    </ul>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:castItem</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:castItem">
    <li>
      <xsl:apply-templates/>
    </li>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:castList</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:castList">
    <xsl:if test="tei:head">
      <p>
        <em>
          <xsl:for-each select="tei:head">
            <xsl:apply-templates/>
          </xsl:for-each>
        </em>
      </p>
    </xsl:if>
    <ul>
      <xsl:apply-templates/>
    </ul>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:castList/tei:head</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:castList/tei:head"/>
  <xd:doc>
    <xd:short>Process elements tei:p/tei:stage</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:p/tei:stage">
    <em>
      <xsl:apply-templates/>
    </em>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:role</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xd:doc>
    <xd:short>Process elements tei:sp/tei:stage</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:sp/tei:stage">
    <em>
      <xsl:apply-templates/>
    </em>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:role</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:role">
    <strong>
      <xsl:apply-templates/>
    </strong>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:roleDesc</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:roleDesc">
    <blockquote>
	  <xsl:attribute name="class">
	  <xsl:choose>
	    <xsl:when test="@rend">
	      <xsl:value-of select="@rend"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:text>roleDesc</xsl:text>
	    </xsl:otherwise>
	  </xsl:choose>
	  </xsl:attribute>
      <xsl:choose>
        <xsl:when test="tei:p">
          <xsl:apply-templates/>
        </xsl:when>
        <xsl:otherwise>
          <p>
            <xsl:apply-templates/>
          </p>
        </xsl:otherwise>
      </xsl:choose>
    </blockquote>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:set</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:set">
    <em>
      <xsl:apply-templates/>
    </em>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:sound</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:sound">
    <em>
      <xsl:apply-templates/>
    </em>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:sp</xd:short>
    <xd:detail>
      <p> elaborated by Nick Nicholas &lt;nicholas@uci.edu&gt;, March
        2001 </p>
    </xd:detail>
  </xd:doc>
  <xsl:template match="tei:sp">
    <dl>
      <dt>
        <xsl:if test="@xml:id">
          <a name="{@xml:id}"/>
        </xsl:if>
        <xsl:apply-templates select="tei:speaker"/>
      </dt>
      <dd>
        <xsl:apply-templates
          select="tei:p | tei:l | tei:lg | tei:seg | tei:ab | tei:stage"/>
      </dd>
    </dl>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:sp/tei:p</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:sp/tei:p">
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:stage</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:stage">
    <p>
      <em>
        <xsl:apply-templates/>
      </em>
    </p>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:tech</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:tech">
    <em>
      <xsl:apply-templates/>
    </em>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:view</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:view">
    <em>
      <xsl:apply-templates/>
    </em>
  </xsl:template>
</xsl:stylesheet>
