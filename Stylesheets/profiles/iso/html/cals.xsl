<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.w3.org/1999/xhtml" xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html" xmlns:iso="http://www.iso.org/ns/1.0" xmlns:cals="http://www.oasis-open.org/specs/tm9901" xmlns:html="http://www.w3.org/1999/xhtml" xmlns:teix="http://www.tei-c.org/ns/Examples" xmlns:s="http://www.ascc.net/xml/schematron" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:t="http://www.thaiopensource.com/ns/annotations" xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0" xmlns:rng="http://relaxng.org/ns/structure/1.0" exclude-result-prefixes="tei html t a rng s iso tbx cals teix" version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>

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

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" type="stylesheet">
    <desc> Process CALS tables, using code borrowed from Andrew Welch
      http://www.biglist.com/lists/xsl-list/archives/200202/msg00666.html</desc>
  </doc>
  <xsl:template match="cals:tgroup">
    <table>
      <xsl:if test="@align">
        <xsl:attribute name="align">
          <xsl:value-of select="@align"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:choose>
        <xsl:when test="../@frame='topbot'">
          <xsl:attribute name="style">border-top:thin solid
	black;border-bottom:thin solid black</xsl:attribute>
        </xsl:when>
        <xsl:otherwise>
          <xsl:attribute name="border">0</xsl:attribute>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:variable name="colgroup">
        <colgroup>
          <xsl:call-template name="generate.colgroup">
            <xsl:with-param name="cols" select="count(cals:colspec)"/>
          </xsl:call-template>
        </colgroup>
      </xsl:variable>
      <xsl:copy-of select="$colgroup"/>
      <xsl:apply-templates/>
    </table>
  </xsl:template>
  <xsl:template match="cals:colspec"/>
  <xsl:template match="cals:spanspec"/>
  <xsl:template match="cals:thead|cals:tfoot">
    <xsl:element name="{name(.)}">
      <xsl:if test="@align">
        <xsl:attribute name="align">
          <xsl:value-of select="@align"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:if test="@char">
        <xsl:attribute name="char">
          <xsl:value-of select="@char"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:if test="@charoff">
        <xsl:attribute name="charoff">
          <xsl:value-of select="@charoff"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:if test="@valign">
        <xsl:attribute name="valign">
          <xsl:value-of select="@valign"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>
  <xsl:template match="cals:tbody">
    <tbody>
      <xsl:if test="@align">
        <xsl:attribute name="align">
          <xsl:value-of select="@align"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:if test="@char">
        <xsl:attribute name="char">
          <xsl:value-of select="@char"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:if test="@charoff">
        <xsl:attribute name="charoff">
          <xsl:value-of select="@charoff"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:if test="@valign">
        <xsl:attribute name="valign">
          <xsl:value-of select="@valign"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:apply-templates/>
    </tbody>
  </xsl:template>
  <xsl:template match="cals:row">
    <tr>
      <xsl:if test="@align">
        <xsl:attribute name="align">
          <xsl:value-of select="@align"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:if test="@char">
        <xsl:attribute name="char">
          <xsl:value-of select="@char"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:if test="@charoff">
        <xsl:attribute name="charoff">
          <xsl:value-of select="@charoff"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:if test="@valign">
        <xsl:attribute name="valign">
          <xsl:value-of select="@valign"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:apply-templates/>
    </tr>
  </xsl:template>
  <xsl:template match="cals:thead/cals:row/cals:entry">
    <xsl:call-template name="process.cell">
      <xsl:with-param name="cellgi">th</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <xsl:template match="cals:tbody/cals:row/cals:entry">
    <xsl:call-template name="process.cell">
      <xsl:with-param name="cellgi">td</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <xsl:template match="cals:tfoot/cals:row/cals:entry">
    <xsl:call-template name="process.cell">
      <xsl:with-param name="cellgi">th</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <xsl:template name="process.cell">
    <xsl:param name="cellgi">td</xsl:param>
    <xsl:variable name="empty.cell" select="count(node()) = 0"/>
    <xsl:variable name="entry.colnum">
      <xsl:call-template name="entry.colnum"/>
    </xsl:variable>
    <xsl:if test="$entry.colnum != ''">
      <xsl:variable name="prev.entry" select="preceding-sibling::*[1]"/>
      <xsl:variable name="prev.ending.colnum">
        <xsl:choose>
          <xsl:when test="$prev.entry">
            <xsl:call-template name="entry.ending.colnum">
              <xsl:with-param name="entry" select="$prev.entry"/>
            </xsl:call-template>
          </xsl:when>
          <xsl:otherwise>0</xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:call-template name="add-empty-entries">
        <xsl:with-param name="number">
          <xsl:choose>
            <xsl:when test="$prev.ending.colnum = ''">0</xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="$entry.colnum - $prev.ending.colnum - 1"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:if>
    <xsl:element name="{$cellgi}">
      <xsl:if test="@spanname">
        <xsl:variable name="namest" select="ancestor::cals:tgroup/cals:spanspec[@spanname=./@spanname]/@namest"/>
        <xsl:variable name="nameend" select="ancestor::cals:tgroup/cals:spanspec[@spanname=./@spanname]/@nameend"/>
        <xsl:variable name="colst" select="ancestor::*[cals:colspec/@colname=$namest]/cals:colspec[@colname=$namest]/@colnum"/>
        <xsl:variable name="colend" select="ancestor::*[cals:colspec/@colname=$nameend]/cals:colspec[@colname=$nameend]/@colnum"/>
        <xsl:attribute name="colspan">
          <xsl:value-of select="number($colend) - number($colst) + 1"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:if test="@morerows">
        <xsl:attribute name="rowspan">
          <xsl:value-of select="@morerows+1"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:if test="@namest">
        <xsl:attribute name="colspan">
          <xsl:call-template name="calculate.colspan"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:if test="@align">
        <xsl:attribute name="align">
          <xsl:value-of select="@align"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:if test="@char">
        <xsl:attribute name="char">
          <xsl:value-of select="@char"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:if test="@charoff">
        <xsl:attribute name="charoff">
          <xsl:value-of select="@charoff"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:if test="@valign">
        <xsl:attribute name="valign">
          <xsl:value-of select="@valign"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:if test="@rowsep='1'">
        <xsl:attribute name="style">border-bottom:thin solid black</xsl:attribute>
      </xsl:if>
      <xsl:if test="not(preceding-sibling::*)                   and ancestor::cals:row/@id">
        <a name="{ancestor::cals:row/@id}"/>
      </xsl:if>
      <xsl:if test="@id">
        <a name="{@id}"/>
      </xsl:if>
      <xsl:choose>
        <xsl:when test="$empty.cell">
          <xsl:text>&#160;</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:element>
  </xsl:template>
  <xsl:template name="add-empty-entries">
    <xsl:param name="number" select="'0'"/>
    <xsl:choose>
      <xsl:when test="$number &lt;= 0"/>
      <xsl:otherwise>
        <td>&#160;</td>
        <xsl:call-template name="add-empty-entries">
          <xsl:with-param name="number" select="$number - 1"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="entry.colnum">
    <xsl:param name="entry" select="."/>
    <xsl:choose>
      <xsl:when test="$entry/@colname">
        <xsl:variable name="colname" select="$entry/@colname"/>
        <xsl:variable name="colspec" select="$entry/ancestor::cals:tgroup/cals:colspec[@colname=$colname]"/>
        <xsl:call-template name="colspec.colnum">
          <xsl:with-param name="colspec" select="$colspec"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="$entry/@namest">
        <xsl:variable name="namest" select="$entry/@namest"/>
        <xsl:variable name="colspec" select="$entry/ancestor::cals:tgroup/cals:colspec[@colname=$namest]"/>
        <xsl:call-template name="colspec.colnum">
          <xsl:with-param name="colspec" select="$colspec"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="count($entry/preceding-sibling::*) = 0">1</xsl:when>
      <xsl:otherwise>
        <xsl:variable name="pcol">
          <xsl:call-template name="entry.ending.colnum">
            <xsl:with-param name="entry" select="$entry/preceding-sibling::*[1]"/>
          </xsl:call-template>
        </xsl:variable>
        <xsl:value-of select="$pcol + 1"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="entry.ending.colnum">
    <xsl:param name="entry" select="."/>
    <xsl:choose>
      <xsl:when test="$entry/@colname">
        <xsl:variable name="colname" select="$entry/@colname"/>
        <xsl:variable name="colspec" select="$entry/ancestor::cals:tgroup/cals:colspec[@colname=$colname]"/>
        <xsl:call-template name="colspec.colnum">
          <xsl:with-param name="colspec" select="$colspec"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="$entry/@nameend">
        <xsl:variable name="nameend" select="$entry/@nameend"/>
        <xsl:variable name="colspec" select="$entry/ancestor::cals:tgroup/cals:colspec[@colname=$nameend]"/>
        <xsl:call-template name="colspec.colnum">
          <xsl:with-param name="colspec" select="$colspec"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="count($entry/preceding-sibling::*) = 0">1</xsl:when>
      <xsl:otherwise>
        <xsl:variable name="pcol">
          <xsl:call-template name="entry.ending.colnum">
            <xsl:with-param name="entry" select="$entry/preceding-sibling::*[1]"/>
          </xsl:call-template>
        </xsl:variable>
        <xsl:value-of select="$pcol + 1"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="colspec.colnum">
    <xsl:param name="colspec" select="."/>
    <xsl:choose>
      <xsl:when test="$colspec/@colnum">
        <xsl:value-of select="$colspec/@colnum"/>
      </xsl:when>
      <xsl:when test="$colspec/preceding-sibling::cals:colspec">
        <xsl:variable name="prec.colspec.colnum">
          <xsl:call-template name="colspec.colnum">
            <xsl:with-param name="colspec" select="$colspec/preceding-sibling::cals:colspec[1]"/>
          </xsl:call-template>
        </xsl:variable>
        <xsl:value-of select="$prec.colspec.colnum + 1"/>
      </xsl:when>
      <xsl:otherwise>1</xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="generate.colgroup">
    <xsl:param name="cols" select="1"/>
    <xsl:param name="count" select="1"/>
    <xsl:choose>
      <xsl:when test="$count&gt;$cols"/>
      <xsl:otherwise>
        <xsl:call-template name="generate.col">
          <xsl:with-param name="countcol" select="$count"/>
        </xsl:call-template>
        <xsl:call-template name="generate.colgroup">
          <xsl:with-param name="cols" select="$cols"/>
          <xsl:with-param name="count" select="$count+1"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="generate.col">
    <xsl:param name="countcol">1</xsl:param>
    <xsl:param name="colspecs" select="./cals:colspec"/>
    <xsl:param name="count">1</xsl:param>
    <xsl:param name="colnum">1</xsl:param>
    <xsl:choose>
      <xsl:when test="$count&gt;count($colspecs)">
        <col/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="colspec" select="$colspecs[$count=position()]"/>
        <xsl:variable name="colspec.colnum">
          <xsl:choose>
            <xsl:when test="$colspec/@colnum">
              <xsl:value-of select="$colspec/@colnum"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="$colnum"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
        <xsl:choose>
          <xsl:when test="$colspec.colnum=$countcol">
            <col>
              <xsl:if test="$colspec/@align">
                <xsl:attribute name="align">
                  <xsl:value-of select="$colspec/@align"/>
                </xsl:attribute>
              </xsl:if>
              <xsl:if test="$colspec/@char">
                <xsl:attribute name="char">
                  <xsl:value-of select="$colspec/@char"/>
                </xsl:attribute>
              </xsl:if>
              <xsl:if test="$colspec/@charoff">
                <xsl:attribute name="charoff">
                  <xsl:value-of select="$colspec/@charoff"/>
                </xsl:attribute>
              </xsl:if>
            </col>
          </xsl:when>
          <xsl:otherwise>
            <xsl:call-template name="generate.col">
              <xsl:with-param name="countcol" select="$countcol"/>
              <xsl:with-param name="colspecs" select="$colspecs"/>
              <xsl:with-param name="count" select="$count+1"/>
              <xsl:with-param name="colnum">
                <xsl:choose>
                  <xsl:when test="$colspec/@colnum">
                    <xsl:value-of select="$colspec/@colnum + 1"/>
                  </xsl:when>
                  <xsl:otherwise>
                    <xsl:value-of select="$colnum + 1"/>
                  </xsl:otherwise>
                </xsl:choose>
              </xsl:with-param>
            </xsl:call-template>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="colspec.colwidth">
    <!-- when this macro is called, the current context must be an entry -->
    <xsl:param name="colname"/>
    <!-- .. = row, ../.. = thead|tbody, ../../.. = cals:tgroup -->
    <xsl:param name="colspecs" select="../../../../cals:tgroup/cals:colspec"/>
    <xsl:param name="count">1</xsl:param>
    <xsl:choose>
      <xsl:when test="$count&gt;count($colspecs)"/>
      <xsl:otherwise>
        <xsl:variable name="colspec" select="$colspecs[$count=position()]"/>
        <xsl:choose>
          <xsl:when test="$colspec/@colname=$colname">
            <xsl:value-of select="$colspec/@colwidth"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:call-template name="colspec.colwidth">
              <xsl:with-param name="colname" select="$colname"/>
              <xsl:with-param name="colspecs" select="$colspecs"/>
              <xsl:with-param name="count" select="$count+1"/>
            </xsl:call-template>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="calculate.colspan">
    <xsl:param name="entry" select="."/>
    <xsl:variable name="namest" select="$entry/@namest"/>
    <xsl:variable name="nameend" select="$entry/@nameend"/>
    <xsl:variable name="scol">
      <xsl:call-template name="colspec.colnum">
        <xsl:with-param name="colspec" select="$entry/ancestor::cals:tgroup/cals:colspec[@colname=$namest]"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="ecol">
      <xsl:call-template name="colspec.colnum">
        <xsl:with-param name="colspec" select="$entry/ancestor::cals:tgroup/cals:colspec[@colname=$nameend]"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:value-of select="$ecol - $scol + 1"/>
  </xsl:template>
</xsl:stylesheet>
