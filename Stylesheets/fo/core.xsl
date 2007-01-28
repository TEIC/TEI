<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xd="http://www.pnp-software.com/XSLTdoc" xmlns:fotex="http://www.tug.org/fotex" xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0" xmlns:edate="http://exslt.org/dates-and-times" xmlns:estr="http://exslt.org/strings" xmlns:exsl="http://exslt.org/common" xmlns:fo="http://www.w3.org/1999/XSL/Format" xmlns:rng="http://relaxng.org/ns/structure/1.0" xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:teix="http://www.tei-c.org/ns/Examples" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" extension-element-prefixes="exsl estr edate" exclude-result-prefixes="xd exsl estr edate a fotex fo rng tei teix" version="1.0">
  <xd:doc type="stylesheet">
    <xd:short>
    TEI stylesheet
    dealing  with elements from the
      core module, making XSL-FO output.
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
    <xd:author>See AUTHORS</xd:author>
    <xd:cvsId>$Id$</xd:cvsId>
    <xd:copyright>2005, TEI Consortium</xd:copyright>
  </xd:doc>
  <xd:doc>
    <xd:short>Process elements  processing-instruction()[name()='xmltex']</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="processing-instruction()[name()='xmltex']">
    <xsl:message>xmltex pi <xsl:value-of select="."/></xsl:message>
    <xsl:copy-of select="."/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:ab</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:ab">
    <fo:block>
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:abbr</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:abbr">
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:add</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:add">
    <xsl:choose>
      <xsl:when test="@place='sup'">
        <fo:inline vertical-align="super">
          <xsl:apply-templates/>
        </fo:inline>
      </xsl:when>
      <xsl:when test="@place='sub'">
        <fo:inline vertical-align="sub">
          <xsl:apply-templates/>
        </fo:inline>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:bibl</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:bibl">
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:byline</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:byline">
    <fo:block text-align="center">
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:cell//tei:lb</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:cell//tei:lb">
    <xsl:choose>
      <xsl:when test="$foEngine='passivetex'"> </xsl:when>
      <xsl:otherwise>
        <fo:inline linefeed-treatment="preserve">
          <xsl:text>&#10;</xsl:text>
        </fo:inline>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:code</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:code">
    <fo:inline font-family="{$typewriterFont}">
      <xsl:apply-templates/>
    </fo:inline>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:corr</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:corr">
    <xsl:text>[</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>]</xsl:text>
    <xsl:choose>
      <xsl:when test="@sic">
        <fo:footnote>
          <fo:footnote-citation>
            <fo:inline font-size="8pt" vertical-align="super">
              <xsl:number format="a" level="any" count="tei:corr"/>
            </fo:inline>
          </fo:footnote-citation>
          <fo:list-block>
            <xsl:attribute name="provisional-distance-between-starts">
              <xsl:value-of select="$distanceBetweenStarts"/>
            </xsl:attribute>
            <xsl:attribute name="provisional-label-separation">
              <xsl:value-of select="$labelSeparation"/>
            </xsl:attribute>
            <fo:list-item>
              <fo:list-item-label end-indent="label-end()">
                <fo:block>
                  <fo:inline font-size="{$footnoteSize}" vertical-align="super">
                    <xsl:number format="a" level="any" count="tei:corr"/>
                  </fo:inline>
                </fo:block>
              </fo:list-item-label>
              <fo:list-item-body start-indent="body-start()">
                <fo:block font-size="{$footnoteSize}">
                  <xsl:value-of select="@sic"/>
                </fo:block>
              </fo:list-item-body>
            </fo:list-item>
          </fo:list-block>
        </fo:footnote>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:del</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:del">
    <fo:inline text-decoration="line-through">
      <xsl:apply-templates/>
    </fo:inline>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:eg</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:eg">
    <fo:block font-family="{$typewriterFont}" background-color="{$exampleBackgroundColor}" color="{$exampleColor}" white-space-treatment="preserve" linefeed-treatment="preserve" white-space-collapse="false" wrap-option="no-wrap" text-indent="0em" hyphenate="false" start-indent="{$exampleMargin}" text-align="start" font-size="{$exampleSize}" space-before.optimum="4pt" space-after.optimum="4pt">
      <xsl:if test="not($flowMarginLeft='')">
        <xsl:attribute name="padding-start">
          <xsl:value-of select="$exampleMargin"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:if test="parent::tei:exemplum">
        <xsl:text>&#10;</xsl:text>
      </xsl:if>
      <xsl:value-of select="translate(.,' ',' ')"/>
    </fo:block>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:eg[@rend='kwic']/lb</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:eg[@rend='kwic']/lb"/>
  <xd:doc>
    <xd:short>Process elements  tei:emph</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:emph">
    <fo:inline font-style="italic">
      <xsl:apply-templates/>
    </fo:inline>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:epigraph</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:epigraph">
    <fo:block text-align="center" space-before.optimum="4pt" space-after.optimum="4pt" start-indent="{$exampleMargin}">
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:epigraph/tei:lg</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:epigraph/tei:lg">
    <fo:block text-align="center" space-before.optimum="4pt" space-after.optimum="4pt">
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:epigraph/tei:q</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:epigraph/tei:q">
    <fo:block space-before.optimum="4pt" space-after.optimum="4pt" start-indent="{$exampleMargin}">
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:foreign</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:foreign">
    <fo:inline font-style="italic">
      <xsl:apply-templates/>
    </fo:inline>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:gap</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:gap">
    <fo:inline border-style="solid">
      <xsl:text>[</xsl:text>
      <xsl:value-of select="@reason"/>
      <xsl:text>]</xsl:text>
    </fo:inline>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:gi</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:gi">
    <fo:inline hyphenate="false" color="{$giColor}" font-family="{$typewriterFont}">
      <xsl:text>&lt;</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>&gt;</xsl:text>
    </fo:inline>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:gloss</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:gloss">
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:hi</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:hi">
    <fo:inline>
      <xsl:call-template name="rend">
        <xsl:with-param name="defaultvalue" select="string('bold')"/>
        <xsl:with-param name="defaultstyle" select="string('font-weight')"/>
        <xsl:with-param name="rend" select="@rend"/>
      </xsl:call-template>
      <xsl:apply-templates/>
    </fo:inline>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:ident</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:ident">
    <fo:inline color="{$identColor}" font-family="{$sansFont}">
      <xsl:apply-templates/>
    </fo:inline>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:index</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:index">
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:interp</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:interp">
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:interpGrp</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:interpGrp">
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:item</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:item" mode="catalogue">
    <fo:table-cell>
      <fo:block>
        <xsl:choose>
          <xsl:when test="tei:label">
            <fo:inline font-weight="bold">
              <xsl:apply-templates select="tei:label" mode="print"/>
            </fo:inline>
          </xsl:when>
          <xsl:otherwise>
            <fo:inline font-weight="bold">
              <xsl:apply-templates mode="print" select="preceding-sibling::tei:*[1]"/>
            </fo:inline>
          </xsl:otherwise>
        </xsl:choose>
      </fo:block>
    </fo:table-cell>
    <fo:table-cell>
      <fo:block>
        <xsl:apply-templates/>
      </fo:block>
    </fo:table-cell>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:item</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:item">
    <xsl:call-template name="makeItem"/>
  </xsl:template>
  <xsl:template match="tei:item" mode="xref">
    <xsl:variable name="listdepth" select="count(ancestor::tei:list)"/>
    <xsl:if test="parent::tei:list[@type='bibliography']">
      <xsl:text> [</xsl:text>
    </xsl:if>
    <xsl:variable name="listNFormat">
      <xsl:choose>
        <xsl:when test="$listdepth=1">
          <xsl:text>1</xsl:text>
        </xsl:when>
        <xsl:when test="$listdepth=2">
          <xsl:text>i</xsl:text>
        </xsl:when>
        <xsl:when test="$listdepth=3">
          <xsl:text>a</xsl:text>
        </xsl:when>
        <xsl:when test="$listdepth=4">
          <xsl:text>I</xsl:text>
        </xsl:when>
      </xsl:choose>
    </xsl:variable>
    <xsl:number format="{$listNFormat}"/>
    <xsl:if test="parent::tei:list[@type='bibliography']">
      <xsl:text>]</xsl:text>
    </xsl:if>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:kw</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:kw">
    <fo:inline font-style="italic">
      <xsl:apply-templates/>
    </fo:inline>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:l</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:l">
    <fo:block space-before.optimum="0pt" space-after.optimum="0pt">
      <xsl:choose>
        <xsl:when test="starts-with(@rend,'indent(')">
          <xsl:attribute name="text-indent">
            <xsl:value-of select="concat(substring-before(substring-after(@rend,'('),')'),'em')"/>
          </xsl:attribute>
        </xsl:when>
        <xsl:when test="starts-with(@rend,'indent')">
          <xsl:attribute name="text-indent">1em</xsl:attribute>
        </xsl:when>
      </xsl:choose>
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:label</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template mode="print" match="tei:label">
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:label</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:label"/>
  <xd:doc>
    <xd:short>Process elements  tei:lb</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:lb">
    <xsl:choose>
      <xsl:when test="$activeLinebreaks='true'">
        <xsl:choose>
<!-- this is a *visible* linebreak character 
	       PassiveTeX implements it as a real line break
	  -->
          <xsl:when test="$foEngine='passivetex'"> </xsl:when>
          <xsl:otherwise>
            <fo:inline linefeed-treatment="preserve">
              <xsl:text>&#10;</xsl:text>
            </fo:inline>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <fo:inline font-size="8pt">
          <xsl:text>❡</xsl:text>
        </fo:inline>
      </xsl:otherwise>
    </xsl:choose>
<!-- JT's suggestion:
<fo:inline
 xml:space="preserve"
 white-space-collapse="false">&#xA;</fo:inline>
-->
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:list</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:list">
    <xsl:if test="child::tei:head">
      <fo:block font-style="italic" text-align="start" space-before.optimum="4pt">
        <xsl:for-each select="tei:head">
          <xsl:apply-templates/>
        </xsl:for-each>
      </fo:block>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="@type='runin'">
        <fo:block>
          <xsl:apply-templates select="tei:item" mode="runin"/>
        </fo:block>
      </xsl:when>
      <xsl:otherwise>
        <fo:list-block>
          <xsl:call-template name="setListIndents"/>
          <xsl:apply-templates/>
        </fo:list-block>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:listBibl</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:listBibl">
    <xsl:choose>
<!-- is it in the back matter? -->
      <xsl:when test="ancestor::tei:back">
        <fo:block>
          <xsl:call-template name="listBiblSetup"/>
        </fo:block>
        <xsl:apply-templates/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:listBibl/tei:bibl</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:listBibl/tei:bibl">
    <fo:block>
      <xsl:call-template name="addID"/>
      <xsl:attribute name="space-before.optimum">
        <xsl:value-of select="$spaceBeforeBibl"/>
      </xsl:attribute>
      <xsl:attribute name="space-after.optimum">
        <xsl:value-of select="$spaceAfterBibl"/>
      </xsl:attribute>
      <xsl:attribute name="text-indent">-<xsl:value-of select="$indentBibl"/>
</xsl:attribute>
      <xsl:attribute name="start-indent">
        <xsl:value-of select="$indentBibl"/>
      </xsl:attribute>
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:list[@type='catalogue']</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:list[@type='catalogue']">
    <fo:block space-before="{$spaceAroundTable}" space-after="{$spaceAroundTable}">
      <fo:table>
        <fo:table-column column-number="1" column-width="20%">
          <xsl:if test="$foEngine='passivetex'">
            <xsl:attribute name="column-align" namespace="http://www.tug.org/fotex">p</xsl:attribute>
          </xsl:if>
        </fo:table-column>
        <fo:table-column column-number="2" column-width="80%">
        <xsl:if test="$foEngine='passivetex'">
          <xsl:attribute name="column-align" namespace="http://www.tug.org/fotex">p</xsl:attribute>
        </xsl:if>
        </fo:table-column>
        <fo:table-body>
          <xsl:for-each select="tei:item">
            <fo:table-row>
              <xsl:apply-templates select="." mode="catalogue"/>
            </fo:table-row>
          </xsl:for-each>
        </fo:table-body>
      </fo:table>
    </fo:block>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:lg</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:lg">
    <fo:block start-indent="{$exampleMargin}" text-align="start" space-before.optimum="4pt" space-after.optimum="4pt">
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:mentioned</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:mentioned">
    <fo:inline>
      <xsl:call-template name="rend">
        <xsl:with-param name="defaultvalue" select="string('italic')"/>
        <xsl:with-param name="defaultstyle" select="string('font-style')"/>
      </xsl:call-template>
      <xsl:apply-templates/>
    </fo:inline>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:milestone</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:milestone">
    <fo:block>
      <xsl:text>******************</xsl:text>
      <xsl:value-of select="@unit"/>
      <xsl:text> </xsl:text>
      <xsl:value-of select="@n"/>
      <xsl:text>******************</xsl:text>
    </fo:block>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:name</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:name">
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:note (endnote mode)</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:note" mode="endnote">
    <fo:block id="{generate-id()}">
      <xsl:call-template name="calculateEndNoteNumber"/>
      <xsl:text>. </xsl:text>
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:note</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:note">
    <xsl:choose>
      <xsl:when test="ancestor::tei:p or ancestor::tei:item">
        <xsl:apply-templates select="." mode="real"/>
      </xsl:when>
      <xsl:otherwise>
        <fo:block>
          <xsl:apply-templates select="." mode="real"/>
        </fo:block>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:note properly</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:note" mode="real">
    <xsl:choose>
      <xsl:when test="@place='end'">
        <fo:simple-link>
          <xsl:attribute name="internal-destination">
            <xsl:value-of select="generate-id()"/>
          </xsl:attribute>
          <fo:inline font-size="{$footnotenumSize}" vertical-align="super">
            <xsl:choose>
              <xsl:when test="@n">
                <xsl:value-of select="@n"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:call-template name="calculateEndNoteNumber"/>
              </xsl:otherwise>
            </xsl:choose>
          </fo:inline>
        </fo:simple-link>
      </xsl:when>
      <xsl:when test="@place='inline'">
        <fo:inline>
          <xsl:text> (</xsl:text>
          <xsl:apply-templates/>
          <xsl:text>)</xsl:text>
        </fo:inline>
      </xsl:when>
      <xsl:when test="@place='display'">
        <fo:block text-indent="0pt" end-indent="{$exampleMargin}" start-indent="{$exampleMargin}" font-size="{$exampleSize}" space-before.optimum="{$exampleBefore}" space-after.optimum="{$exampleAfter}">
          <xsl:apply-templates/>
        </fo:block>
      </xsl:when>
      <xsl:when test="@place='divtop'">
        <fo:block text-indent="0pt" end-indent="{$exampleMargin}" start-indent="{$exampleMargin}" font-style="italic" font-size="{$exampleSize}" space-before.optimum="{$exampleBefore}" space-after.optimum="{$exampleAfter}">
          <xsl:apply-templates/>
        </fo:block>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="FootID">
          <xsl:choose>
            <xsl:when test="@n">
              <xsl:value-of select="@n"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:call-template name="calculateFootnoteNumber"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
        <fo:footnote>
          <fo:inline>
            <xsl:if test="not(@target)">
              <xsl:attribute name="font-size">
                <xsl:value-of select="$footnotenumSize"/>
              </xsl:attribute>
              <xsl:attribute name="vertical-align">super</xsl:attribute>
              <xsl:value-of select="$FootID"/>
            </xsl:if>
          </fo:inline>
          <fo:footnote-body>
            <fo:block end-indent="0pt" start-indent="0pt" text-align="start" font-style="normal" text-indent="{$parIndent}" font-size="{$footnoteSize}">
              <xsl:if test="@xml:id">
                <xsl:attribute name="id">
                  <xsl:value-of select="@xml:id"/>
                </xsl:attribute>
              </xsl:if>
              <xsl:if test="not(@target)">
                <fo:inline font-size="{$footnotenumSize}" vertical-align="super">
                  <xsl:value-of select="$FootID"/>
                </fo:inline>
                <xsl:text> </xsl:text>
              </xsl:if>
              <xsl:apply-templates/>
            </fo:block>
          </fo:footnote-body>
        </fo:footnote>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process element  tei:p</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:p">
    <fo:block>
      <xsl:if test="preceding-sibling::tei:p">
        <xsl:attribute name="text-indent">
          <xsl:value-of select="$parIndent"/>
        </xsl:attribute>
        <xsl:attribute name="space-before.optimum">
          <xsl:value-of select="$parSkip"/>
        </xsl:attribute>
        <xsl:attribute name="space-before.maximum">
          <xsl:value-of select="$parSkipmax"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:if test="@xml:lang">
        <xsl:attribute name="country">
          <xsl:value-of select="substring-before(@xml:lang,'-')"/>
        </xsl:attribute>
        <xsl:attribute name="language">
          <xsl:value-of select="substring-after(@xml:lang,'-')"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:pb</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:pb">
    <xsl:choose>
      <xsl:when test="parent::tei:list"/>
      <xsl:when test="$pagebreakStyle='active'">
        <fo:block break-before="page">
          <xsl:if test="@xml:id">
            <xsl:attribute name="id">
              <xsl:value-of select="@xml:id"/>
            </xsl:attribute>
          </xsl:if>
        </fo:block>
      </xsl:when>
      <xsl:when test="$pagebreakStyle='visible'">
        <fo:inline>
          <xsl:if test="@xml:id">
            <xsl:attribute name="id">
              <xsl:value-of select="@xml:id"/>
            </xsl:attribute>
          </xsl:if>
          <xsl:text>✁[</xsl:text>
          <xsl:value-of select="@unit"/>
          <xsl:text> Page </xsl:text>
          <xsl:value-of select="@n"/>
          <xsl:text>]✁</xsl:text>
        </fo:inline>
      </xsl:when>
      <xsl:otherwise>
        <xsl:if test="@xml:id">
          <xsl:choose>
            <xsl:when test="parent::tei:p or parent::tei:item">
              <fo:inline id="{@xml:id}"/>
            </xsl:when>
            <xsl:otherwise>
              <fo:block id="{@xml:id}"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:q</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:q">
    <xsl:choose>
      <xsl:when test="tei:text">
        <xsl:apply-templates/>
      </xsl:when>
      <xsl:when test="@rend='display' or tei:p or tei:lg">
        <fo:block text-align="start" text-indent="0pt" end-indent="{$exampleMargin}" start-indent="{$exampleMargin}" font-size="{$exampleSize}" space-before.optimum="{$exampleBefore}" space-after.optimum="{$exampleAfter}">
          <xsl:apply-templates/>
        </fo:block>
      </xsl:when>
      <xsl:when test="@rend='eg'">
        <fo:block text-align="start" font-size="{$exampleSize}" space-before.optimum="4pt" text-indent="0pt" space-after.optimum="4pt" start-indent="{$exampleMargin}" font-family="{$typewriterFont}">
          <xsl:apply-templates/>
        </fo:block>
      </xsl:when>
      <xsl:when test="@rend = 'qwic'">
        <fo:block space-before="{$spaceAroundTable}" space-after="{$spaceAroundTable}">
          <fo:inline-container>
            <fo:table font-size="{$exampleSize}" font-family="{$typewriterFont}" start-indent="{$exampleMargin}">
              <fo:table-column column-number="1" column-width="">
                <xsl:if test="$foEngine='passivetex'">
                  <xsl:attribute name="column-align" namespace="http://www.tug.org/fotex">p</xsl:attribute>
                </xsl:if>
              </fo:table-column>
              <fo:table-column column-number="2" column-width="">
                <xsl:if test="$foEngine='passivetex'">
                  <xsl:attribute name="column-align" namespace="http://www.tug.org/fotex">l</xsl:attribute>
                </xsl:if>
              </fo:table-column>
              <fo:table-body>
                <xsl:for-each select="tei:q">
                  <xsl:for-each select="tei:term">
                    <fo:table-row>
                      <fo:table-cell>
                        <fo:block>
                          <xsl:apply-templates select="preceding-sibling::node()"/>
                        </fo:block>
                      </fo:table-cell>
                      <fo:table-cell>
                        <fo:block>
                          <xsl:apply-templates/>
                          <xsl:apply-templates select="following-sibling::node()"/>
                        </fo:block>
                      </fo:table-cell>
                    </fo:table-row>
                  </xsl:for-each>
                </xsl:for-each>
              </fo:table-body>
            </fo:table>
          </fo:inline-container>
        </fo:block>
      </xsl:when>
      <xsl:when test="starts-with(@rend,'kwic')">
        <fo:block space-before="{$spaceAroundTable}" space-after="{$spaceAroundTable}">
          <fo:inline-container>
            <fo:table font-size="{$exampleSize}" start-indent="{$exampleMargin}" font-family="{$typewriterFont}">
              <fo:table-column column-number="1" column-width="">
                <xsl:if test="$foEngine='passivetex'">
                  <xsl:attribute name="column-align" namespace="http://www.tug.org/fotex">r</xsl:attribute>
                </xsl:if>
              </fo:table-column>
              <fo:table-column column-number="2" column-width="">
                <xsl:if test="$foEngine='passivetex'">
                  <xsl:attribute name="column-align" namespace="http://www.tug.org/fotex">l</xsl:attribute>
                </xsl:if>
              </fo:table-column>
              <fo:table-body>
                <xsl:for-each select="tei:term">
                  <fo:table-row>
                    <fo:table-cell>
                      <fo:block>
                        <xsl:value-of select="preceding-sibling::node()[1]"/>
                      </fo:block>
                    </fo:table-cell>
                    <fo:table-cell>
                      <fo:block>
                        <xsl:apply-templates/>
                        <xsl:value-of select="following-sibling::node()[1]"/>
                      </fo:block>
                    </fo:table-cell>
                  </fo:table-row>
                </xsl:for-each>
              </fo:table-body>
            </fo:table>
          </fo:inline-container>
        </fo:block>
      </xsl:when>
      <xsl:when test="@rend='literal'">
        <fo:block white-space-collapse="false" wrap-option="no-wrap" font-size="{$exampleSize}" space-before.optimum="4pt" space-after.optimum="4pt" start-indent="{$exampleMargin}" font-family="{$typewriterFont}">
          <xsl:apply-templates/>
        </fo:block>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>“</xsl:text>
        <xsl:apply-templates/>
        <xsl:text>”</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:reg</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:reg">
    <fo:inline font-family="{$sansFont}">
      <xsl:apply-templates/>
    </fo:inline>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:rs</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:rs">
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:s</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:s">
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:salute</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:salute">
    <fo:block text-align="left">
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:seg</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:seg">
    <fo:block font-family="{$typewriterFont}" background-color="yellow" white-space-collapse="false" wrap-option="no-wrap" text-indent="0em" start-indent="{$exampleMargin}" text-align="start" font-size="{$exampleSize}" padding-before="8pt" padding-after="8pt" space-before.optimum="4pt" space-after.optimum="4pt">
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:sic</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:sic">
    <xsl:apply-templates/>
    <xsl:text> (sic)</xsl:text>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:signed</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:signed">
    <fo:block text-align="left">
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:soCalled</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:soCalled">
    <xsl:text>‘</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>’</xsl:text>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:term</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:term">
    <fo:inline font-style="italic">
      <xsl:apply-templates/>
    </fo:inline>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:title</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:title">
    <xsl:choose>
      <xsl:when test="@level='a'">
        <xsl:text>‘</xsl:text>
        <xsl:apply-templates/>
        <xsl:text>’</xsl:text>
      </xsl:when>
      <xsl:when test="@level='m'">
        <fo:inline font-style="italic">
          <xsl:apply-templates/>
        </fo:inline>
      </xsl:when>
      <xsl:when test="@level='s'">
        <fo:inline font-style="italic">
          <xsl:apply-templates/>
        </fo:inline>
      </xsl:when>
      <xsl:otherwise>
        <fo:inline font-style="italic">
          <xsl:apply-templates/>
        </fo:inline>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements  tei:unclear</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:unclear">
    <fo:inline text-decoration="blink">
      <xsl:apply-templates/>
    </fo:inline>
  </xsl:template>
  <xd:doc>
    <xd:short>[fo] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="addID">
    <xsl:attribute name="id">
      <xsl:choose>
        <xsl:when test="@xml:id">
          <xsl:value-of select="@xml:id"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="generate-id()"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:attribute>
  </xsl:template>
  <xd:doc>
    <xd:short>[fo] process "rend" attribute</xd:short>
    <xd:param name="value">value of "rend" attribute</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="applyRend">
    <xsl:param name="value"/>
    <xsl:choose>
      <xsl:when test="$value='gothic'">
        <xsl:attribute name="font-family">fantasy</xsl:attribute>
      </xsl:when>
      <xsl:when test="$value='calligraphic'">
        <xsl:attribute name="font-family">cursive</xsl:attribute>
      </xsl:when>
      <xsl:when test="$value='ital' or $value='italic' or $value='it' or $value='i' or $value='italics'">
        <xsl:attribute name="font-style">italic</xsl:attribute>
      </xsl:when>
      <xsl:when test="$value='sc'">
        <xsl:attribute name="font-variant">small-caps</xsl:attribute>
      </xsl:when>
      <xsl:when test="$value='code'">
        <xsl:attribute name="font-family">
          <xsl:value-of select="$typewriterFont"/>
        </xsl:attribute>
      </xsl:when>
      <xsl:when test="$value='bo' or $value='bold'">
        <xsl:attribute name="font-weight">bold</xsl:attribute>
      </xsl:when>
      <xsl:when test="$value='BO'">
        <xsl:attribute name="font-style">italic</xsl:attribute>
        <xsl:attribute name="text-decoration">underline</xsl:attribute>
      </xsl:when>
      <xsl:when test="$value='UL' or $value='ul'">
        <xsl:attribute name="text-decoration">underline</xsl:attribute>
      </xsl:when>
      <xsl:when test="$value='sub'">
        <xsl:attribute name="vertical-align">sub</xsl:attribute>
      </xsl:when>
      <xsl:when test="$value='small'">
        <xsl:attribute name="font-size">small</xsl:attribute>
      </xsl:when>
      <xsl:when test="$value='strike'">
        <xsl:attribute name="text-decoration">line-through</xsl:attribute>
      </xsl:when>
      <xsl:when test="$value='sup'">
        <xsl:attribute name="vertical-align">super</xsl:attribute>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>[fo] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="calculateEndNoteNumber">
    <xsl:number level="any" format="i" count="tei:note[@place='end']"/>
  </xsl:template>
  <xd:doc>
    <xd:short>[fo] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="calculateFootnoteNumber">
    <xsl:number from="tei:text" level="any" count="tei:note"/>
  </xsl:template>
  <xd:doc>
    <xd:short>[fo] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="listBiblSetup">
    <xsl:call-template name="setupDiv0"/>
    <xsl:call-template name="addID"/>
    <xsl:call-template name="i18n">
      <xsl:with-param name="word">biblioWords</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <xd:doc>
    <xd:short>[fo] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="makeItem">
<!-- item behaviour depends on the type attribute of our parent:
simple, bullets, ordered, gloss, unordered
-->
    <xsl:variable name="listdepth" select="count(ancestor::tei:list)"/>
    <fo:list-item>
      <xsl:if test="not(parent::tei:note[@place='foot'])">
        <xsl:attribute name="space-before.optimum">
          <xsl:value-of select="$listItemsep"/>
        </xsl:attribute>
      </xsl:if>
      <fo:list-item-label end-indent="label-end()">
        <xsl:if test="@xml:id">
          <xsl:attribute name="id">
            <xsl:value-of select="@xml:id"/>
          </xsl:attribute>
        </xsl:if>
        <xsl:text>&#10;</xsl:text>
        <fo:block>
          <xsl:choose>
            <xsl:when test="@n">
              <xsl:attribute name="text-align">end</xsl:attribute>
              <xsl:value-of select="@n"/>
            </xsl:when>
            <xsl:when test="../@type='bibliography'">
              <xsl:attribute name="text-align">end</xsl:attribute>
              <xsl:apply-templates mode="xref" select="."/>
            </xsl:when>
            <xsl:when test="../@type='ordered'">
              <xsl:attribute name="text-align">end</xsl:attribute>
              <xsl:apply-templates mode="xref" select="."/>
              <xsl:text>.</xsl:text>
            </xsl:when>
            <xsl:when test="../@type='gloss'">
              <xsl:attribute name="text-align">start</xsl:attribute>
              <xsl:attribute name="font-weight">bold</xsl:attribute>
              <xsl:choose>
                <xsl:when test="tei:label">
                  <xsl:apply-templates mode="print" select="tei:label"/>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:apply-templates mode="print" select="preceding-sibling::tei:*[1]"/>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:when>
            <xsl:when test="../@type='numbered'">
<!-- numbered support added rbl 26.3.2005 -->
              <xsl:attribute name="text-align">end</xsl:attribute>
              <xsl:number/>
              <xsl:text>.</xsl:text>
            </xsl:when>
            <xsl:when test="../@type='ordered'">
<!-- numbered support added rbl 26.3.2005 -->
              <xsl:attribute name="text-align">end</xsl:attribute>
              <xsl:number/>
              <xsl:text>.</xsl:text>
            </xsl:when>
            <xsl:otherwise>
              <xsl:attribute name="text-align">end</xsl:attribute>
              <xsl:choose>
                <xsl:when test="$listdepth=0">
                  <xsl:value-of select="$bulletOne"/>
                </xsl:when>
                <xsl:when test="$listdepth=1">
                  <xsl:value-of select="$bulletOne"/>
                </xsl:when>
                <xsl:when test="$listdepth=2">
                  <xsl:value-of select="$bulletTwo"/>
                </xsl:when>
                <xsl:when test="$listdepth=3">
                  <xsl:value-of select="$bulletThree"/>
                </xsl:when>
                <xsl:when test="$listdepth=4">
                  <xsl:value-of select="$bulletFour"/>
                </xsl:when>
              </xsl:choose>
            </xsl:otherwise>
          </xsl:choose>
        </fo:block>
      </fo:list-item-label>
      <fo:list-item-body start-indent="body-start()">
        <xsl:choose>
          <xsl:when test="tei:p">
            <xsl:apply-templates/>
          </xsl:when>
          <xsl:otherwise>
            <fo:block font-weight="normal">
              <xsl:apply-templates/>
            </fo:block>
          </xsl:otherwise>
        </xsl:choose>
      </fo:list-item-body>
    </fo:list-item>
  </xsl:template>
  <xd:doc>
    <xd:short>[fo] </xd:short>
    <xd:param name="defaultvalue">defaultvalue</xd:param>
    <xd:param name="defaultstyle">defaultstyle</xd:param>
    <xd:param name="rend">rend</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="rend">
    <xsl:param name="defaultvalue"/>
    <xsl:param name="defaultstyle"/>
    <xsl:param name="rend"/>
    <xsl:choose>
      <xsl:when test="$rend=''">
        <xsl:attribute name="{$defaultstyle}">
          <xsl:value-of select="$defaultvalue"/>
        </xsl:attribute>
      </xsl:when>
      <xsl:when test="contains($rend,';')">
        <xsl:call-template name="applyRend">
          <xsl:with-param name="value" select="substring-before($rend,';')"/>
        </xsl:call-template>
        <xsl:call-template name="rend">
          <xsl:with-param name="rend" select="substring-after($rend,';')"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="applyRend">
          <xsl:with-param name="value" select="$rend"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>[fo] Spacing setup for list blocks</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="setListIndents">
    <xsl:attribute name="provisional-distance-between-starts">
      <xsl:choose>
        <xsl:when test="@type='gloss'">
          <xsl:value-of select="$distanceBetweenGlossStarts"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$distanceBetweenStarts"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:attribute>
    <xsl:attribute name="provisional-label-separation">
      <xsl:value-of select="$labelSeparation"/>
    </xsl:attribute>
    <xsl:attribute name="text-indent">0em</xsl:attribute>
    <xsl:attribute name="margin-right">
      <xsl:value-of select="$listRightMargin"/>
    </xsl:attribute>
    <xsl:variable name="listdepth" select="count(ancestor::tei:list)"/>
    <xsl:choose>
      <xsl:when test="$listdepth=0">
        <xsl:attribute name="space-before">
          <xsl:value-of select="$listAbove-1"/>
        </xsl:attribute>
        <xsl:attribute name="space-after">
          <xsl:value-of select="$listBelow-1"/>
        </xsl:attribute>
      </xsl:when>
      <xsl:when test="$listdepth=1">
        <xsl:attribute name="space-before">
          <xsl:value-of select="$listAbove-2"/>
        </xsl:attribute>
        <xsl:attribute name="space-after">
          <xsl:value-of select="$listBelow-2"/>
        </xsl:attribute>
      </xsl:when>
      <xsl:when test="$listdepth=2">
        <xsl:attribute name="space-before">
          <xsl:value-of select="$listAbove-3"/>
        </xsl:attribute>
        <xsl:attribute name="space-after">
          <xsl:value-of select="$listBelow-3"/>
        </xsl:attribute>
      </xsl:when>
      <xsl:when test="$listdepth=3">
        <xsl:attribute name="space-before">
          <xsl:value-of select="$listAbove-4"/>
        </xsl:attribute>
        <xsl:attribute name="space-after">
          <xsl:value-of select="$listBelow-4"/>
        </xsl:attribute>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
</xsl:stylesheet>
