<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet
  exclude-result-prefixes="exsl estr edate svg a fo local rng tei teix xd m"
  extension-element-prefixes="exsl estr edate" version="1.0"
  xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
  xmlns:edate="http://exslt.org/dates-and-times"
  xmlns:estr="http://exslt.org/strings" xmlns:exsl="http://exslt.org/common"
  xmlns:fo="http://www.w3.org/1999/XSL/Format"
  xmlns:svg="http://www.w3.org/2000/svg"
  xmlns:html="http://www.w3.org/1999/xhtml"
  xmlns:local="http://www.pantor.com/ns/local"
  xmlns:m="http://www.w3.org/1998/Math/MathML"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:xd="http://www.pnp-software.com/XSLTdoc"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xd:doc type="stylesheet">
    <xd:short> TEI stylesheet dealing with elements from the figures module,
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
    <xd:author>Sebastian Rahtz sebastian.rahtz@oucs.ox.ac.uk</xd:author>
    <xd:cvsId>$Id$</xd:cvsId>
    <xd:copyright>2005, TEI Consortium</xd:copyright>
  </xd:doc>
  <xd:doc>
    <xd:short>Process elements m:*|@*|comment()|processing-instruction()|text()</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="m:*|@*|comment()|processing-instruction()|text()"
    mode="math">
    <xsl:copy>
      <xsl:apply-templates mode="math"
        select="*|@*|processing-instruction()|text()"/>
    </xsl:copy>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements m:math</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="m:math">
    <m:math>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates mode="math"/>
    </m:math>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:cell</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:cell">
    <td valign="top">
      <xsl:for-each select="@*">
        <xsl:choose>
          <xsl:when
            test="name(.) = 'width'          or name(.) = 'border'          or name(.) = 'cellspacing'          or name(.) = 'cellpadding'">
            <xsl:copy-of select="."/>
          </xsl:when>
          <xsl:when test="name(.)='rend' and starts-with(.,'width:')">
            <xsl:attribute name="width">
              <xsl:value-of select="substring-after(.,'width:')"/>
            </xsl:attribute>
          </xsl:when>
          <xsl:when test="name(.)='rend' and starts-with(.,'class:')">
            <xsl:attribute name="class">
              <xsl:value-of select="substring-after(.,'class:')"/>
            </xsl:attribute>
          </xsl:when>
          <xsl:when test="name(.)='rend'">
            <xsl:attribute name="class">
              <xsl:value-of select="."/>
            </xsl:attribute>
          </xsl:when>
          <xsl:when test="name(.)='cols'">
            <xsl:attribute name="colspan">
              <xsl:value-of select="."/>
            </xsl:attribute>
          </xsl:when>
          <xsl:when test="name(.)='rows'">
            <xsl:attribute name="rowspan">
              <xsl:value-of select="."/>
            </xsl:attribute>
          </xsl:when>
          <xsl:when test="name(.)='align'">
            <xsl:attribute name="align">
              <xsl:value-of select="."/>
            </xsl:attribute>
          </xsl:when>
        </xsl:choose>
      </xsl:for-each>
      <xsl:if test="not(@align) and not($cellAlign='left')">
        <xsl:attribute name="align">
          <xsl:value-of select="$cellAlign"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:if test="@role">
        <xsl:attribute name="class">
          <xsl:value-of select="@role"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:if test="@xml:id">
        <a name="{@xml:id}"/>
      </xsl:if>
      <xsl:apply-templates/>
    </td>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:figDesc</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:figDesc"/>
  <xd:doc>
    <xd:short>Process elements tei:figure</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:figure">
    <xsl:if test="@xml:id">
      <a name="{@xml:id}" class="f">
        <xsl:comment> </xsl:comment>
      </a>
    </xsl:if>
    <xsl:if test="@file|@url|@entity">
      <xsl:call-template name="showGraphic"/>
    </xsl:if>
    <xsl:apply-templates/>
    <xsl:if test="tei:head">
      <div class="caption">
        <xsl:choose>
          <xsl:when
            test="ancestor::tei:front and      $numberFrontFigures='true'">
            <xsl:call-template name="i18n">
              <xsl:with-param name="word">figureWord</xsl:with-param>
            </xsl:call-template>
            <xsl:text> </xsl:text>
            <xsl:number count="tei:figure[tei:head]" from="tei:front"
              level="any"/>
            <xsl:text>. </xsl:text>
          </xsl:when>
          <xsl:when test="ancestor::tei:back and      $numberBackFigures='true'">
            <xsl:call-template name="i18n">
              <xsl:with-param name="word">figureWord</xsl:with-param>
            </xsl:call-template>
            <xsl:text> </xsl:text>
            <xsl:number count="tei:figure[tei:head]" from="tei:back" level="any"/>
            <xsl:text>. </xsl:text>
          </xsl:when>
          <xsl:when test="ancestor::tei:body and      $numberFigures='true'">
            <xsl:call-template name="i18n">
              <xsl:with-param name="word">figureWord</xsl:with-param>
            </xsl:call-template>
            <xsl:text> </xsl:text>
            <xsl:number count="tei:figure[tei:head]" from="tei:body" level="any"/>
            <xsl:text>. </xsl:text>
          </xsl:when>
        </xsl:choose>
        <xsl:apply-templates mode="plain" select="tei:head"/>
      </div>
    </xsl:if>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:figure/tei:head</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:figure/tei:head"/>
  <xd:doc>
    <xd:short>Process elements tei:formula</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:formula" mode="xref">
    <xsl:number/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:graphic</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:graphic">
    <xsl:if test="@xml:id and not($xhtml='true')">
	<a class="g" name="{@xml:id}">
	  <xsl:comment> </xsl:comment>
	</a>
    </xsl:if>
    <xsl:call-template name="showGraphic"/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:row</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:row">
    <tr>
      <xsl:call-template name="rendToClass"/>
      <xsl:if test="@role">
        <xsl:attribute name="class">
          <xsl:value-of select="@role"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:apply-templates/>
    </tr>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:table</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:table">
    <div>
      <xsl:attribute name="class">
        <xsl:choose>
          <xsl:when test="@align">
            <xsl:value-of select="@align"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="$tableAlign"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:attribute>
      <xsl:if test="tei:head">
        <p>
          <xsl:apply-templates mode="xref" select="."/>
        </p>
      </xsl:if>
      <table>
        <xsl:call-template name="rendToClass"/>
        <xsl:if test="@rend='frame' or @rend='rules'">
          <xsl:attribute name="rules">all</xsl:attribute>
          <xsl:attribute name="border">1</xsl:attribute>
        </xsl:if>
        <xsl:for-each select="@*">
          <xsl:if
            test="name(.)='summary' or name(.) = 'width' or name(.) = 'border' or name(.) = 'frame' or name(.) = 'rules' or name(.) = 'cellspacing' or name(.) = 'cellpadding'">
            <xsl:copy-of select="."/>
          </xsl:if>
        </xsl:for-each>
        <xsl:apply-templates/>
      </table>
    </div>
  </xsl:template>
  <xd:doc>
    <xd:short>Process elements tei:table[@rend='simple']</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:table[@rend='simple']">
    <table>
      <xsl:if test="@rend">
        <xsl:attribute name="class">
          <xsl:value-of select="@rend"/>
        </xsl:attribute>
      </xsl:if>
      <xsl:for-each select="@*">
        <xsl:if
          test="name(.)='summary'    or name(.) = 'width'    or name(.) = 'border'    or name(.) = 'frame'    or name(.) = 'rules'    or name(.) = 'cellspacing'    or name(.) = 'cellpadding'">
          <xsl:copy-of select="."/>
        </xsl:if>
      </xsl:for-each>
      <xsl:call-template name="makeAnchor"/>
      <xsl:apply-templates/>
    </table>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:param name="name">name</xd:param>
    <xd:param name="value">value</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="setDimension">
    <xsl:param name="name"/>
    <xsl:param name="value"/>
    <xsl:variable name="calcvalue">
      <xsl:choose>
        <xsl:when test="contains($value,'in')">
          <xsl:value-of select="round($dpi * substring-before($value,'in'))"/>
        </xsl:when>
        <xsl:when test="contains($value,'pt')">
          <xsl:value-of
            select="round($dpi * (substring-before($value,'pt') div 72))"/>
        </xsl:when>
        <xsl:when test="contains($value,'cm')">
          <xsl:value-of
            select="round($dpi * (          substring-before($value,'cm') div 2.54 ))"
          />
        </xsl:when>
        <xsl:when test="contains($value,'px')">
          <xsl:value-of select="substring-before($value,'px')"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$value"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:if test="$calcvalue&gt;0">
      <xsl:attribute name="{$name}">
        <xsl:value-of select="$calcvalue"/>
      </xsl:attribute>
    </xsl:if>
  </xsl:template>
  <xd:doc>
    <xd:short>[html] </xd:short>
    <xd:param name="ID">ID</xd:param>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="showGraphic">
    <xsl:variable name="File">
      <xsl:choose>
        <xsl:when test="@url">
          <xsl:value-of select="@url"/>
          <xsl:if test="not(contains(@url,'.'))">
            <xsl:value-of select="$graphicsSuffix"/>
          </xsl:if>
        </xsl:when>
        <xsl:when test="@file">
          <xsl:value-of select="@file"/>
          <xsl:if test="not(contains(@file,'.'))">
            <xsl:value-of select="$graphicsSuffix"/>
          </xsl:if>
        </xsl:when>
        <xsl:when test="@entity">
          <xsl:variable name="entity">
            <xsl:value-of select="unparsed-entity-uri(@entity)"/>
          </xsl:variable>
          <xsl:choose>
            <xsl:when test="starts-with($entity,'file:')">
              <xsl:value-of select="substring-after($entity,'file:')"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="$entity"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:when>
        <xsl:otherwise>
          <xsl:message terminate="yes">Cannot work out how to do a graphic
          </xsl:message>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="Alt">
      <xsl:choose>
        <xsl:when test="tei:figDesc">
          <xsl:value-of select="tei:figDesc//text()"/>
        </xsl:when>
        <xsl:when test="tei:head">
          <xsl:value-of select="tei:head/text()"/>
        </xsl:when>
        <xsl:when test="parent::tei:figure/tei:figDesc">
          <xsl:value-of select="parent::tei:figure/tei:figDesc//text()"/>
        </xsl:when>
        <xsl:when test="parent::tei:figure/tei:head">
          <xsl:value-of select="parent::tei:figure/tei:head/text()"/>
        </xsl:when>
      </xsl:choose>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$showFigures='true'">
        <img src="{$graphicsPrefix}{$File}">
          <xsl:if test="@xml:id and $xhtml='true'">
            <xsl:attribute name="id">
              <xsl:value-of select="@xml:id"/>
            </xsl:attribute>
          </xsl:if>
          <xsl:call-template name="rendToClass"/>
          <xsl:if test="@width">
            <xsl:call-template name="setDimension">
              <xsl:with-param name="value">
                <xsl:value-of select="@width"/>
              </xsl:with-param>
              <xsl:with-param name="name">width</xsl:with-param>
            </xsl:call-template>
          </xsl:if>
          <xsl:if test="@height">
            <xsl:call-template name="setDimension">
              <xsl:with-param name="value">
                <xsl:value-of select="@height"/>
              </xsl:with-param>
              <xsl:with-param name="name">height</xsl:with-param>
            </xsl:call-template>
          </xsl:if>
          <xsl:attribute name="alt">
            <xsl:value-of select="$Alt"/>
          </xsl:attribute>
          <xsl:call-template name="imgHook"/>
        </img>
      </xsl:when>
      <xsl:otherwise>
        <hr/>
        <p><xsl:call-template name="i18n">
            <xsl:with-param name="word">figureWord</xsl:with-param>
          </xsl:call-template><xsl:text> </xsl:text><xsl:for-each
            select="self::tei:figure|parent::tei:figure">
            <xsl:number count="tei:figure[tei:head]" level="any"/>
          </xsl:for-each> file <xsl:value-of select="$File"/> [<xsl:value-of
            select="$Alt"/>] </p>
        <hr/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:binaryObject">
    <img>
      <xsl:attribute name="src">
        <xsl:text>data:</xsl:text>
        <xsl:value-of select="@mimetype"/>
        <xsl:text>;base64,</xsl:text>
        <xsl:copy-of select="text()"/>
      </xsl:attribute>
      <xsl:if test="@width">
        <xsl:call-template name="setDimension">
          <xsl:with-param name="value">
            <xsl:value-of select="@width"/>
          </xsl:with-param>
          <xsl:with-param name="name">width</xsl:with-param>
        </xsl:call-template>
      </xsl:if>
      <xsl:if test="@height">
        <xsl:call-template name="setDimension">
          <xsl:with-param name="value">
            <xsl:value-of select="@height"/>
          </xsl:with-param>
          <xsl:with-param name="name">height</xsl:with-param>
        </xsl:call-template>
      </xsl:if>
    </img>
    <!-- also alt -->
    <!-- this is what we'll need for IE:
<style type="text/css">
   img {behavior: expression(fixBase64(this));}
  </style>
  <script type="text/javascript">
  	// a regular expression to test for Base64 data
	var BASE64_DATA = /^data:.*;base64/i;
	// path to the PHP module that will decode the encoded data
	var base64Path = "/my/base64.php";
	function fixBase64(img) {
		// stop the CSS expression from being endlessly evaluated
		img.runtimeStyle.behavior = "none";
		// check the image src
		if (BASE64_DATA.test(img.src)) {
		// pass the data to the PHP routine
		img.src = base64Path + "?" + img.src.slice(5);
		}
	};
  </script>
  Dean Edwards http://dean.edwards.name/weblog/2005/06/base64-sexy/
<?php
$data = split(";", $_SERVER["REDIRECT_QUERY_STRING"]);
$type = $data[0];
$data = split(",", $data[1]);
header("Content-type: ".$type);
echo base64_decode($data[1]);
?>
-->
  </xsl:template>

  <xsl:template match="svg:*">
    <xsl:copy-of select="."/>
  </xsl:template>

</xsl:stylesheet>
