<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet exclude-result-prefixes="tei edate xd" extension-element-prefixes="edate" version="1.0"
  xmlns:edate="http://exslt.org/dates-and-times" xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:xd="http://www.pnp-software.com/XSLTdoc" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xd:doc type="stylesheet">
    <xd:short> TEI stylesheet dealing with elements from the header module. </xd:short>
    <xd:detail> This library is free software; you can redistribute it and/or modify it under the terms of the
      GNU Lesser General Public License as published by the Free Software Foundation; either version 2.1 of
      the License, or (at your option) any later version. This library is distributed in the hope that it will
      be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
      A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details. You should have
      received a copy of the GNU Lesser General Public License along with this library; if not, write to the
      Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA </xd:detail>
    <xd:author>See AUTHORS</xd:author>
    <xd:cvsId>$Id$</xd:cvsId>
    <xd:copyright>2008, TEI Consortium</xd:copyright>
  </xd:doc>

  <xd:doc>
    <xd:short>Process elements tei:title</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:title" mode="htmlheader">
    <xsl:apply-templates/>
    <xsl:if test="following-sibling::tei:title">
	<xsl:text> &#8212; </xsl:text>
    </xsl:if>
  </xsl:template>
  <xd:doc>
    <xd:short>[common] Find a plausible main author name</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="generateAuthor">
    <xsl:choose>
      <xsl:when
        test="$useHeaderFrontMatter='true' and ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docAuthor">
        <xsl:apply-templates mode="author"
          select="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docAuthor[1]"/>
      </xsl:when>
      <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:author">
        <xsl:for-each select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:author">
          <xsl:apply-templates/>
          <xsl:choose>
            <xsl:when test="count(following-sibling::tei:author)=1"> and </xsl:when>
            <xsl:when test="following-sibling::tei:author">, </xsl:when>
          </xsl:choose>
        </xsl:for-each>
      </xsl:when>
      <xsl:when
        test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/tei:change/tei:respStmt[tei:resp='author']">
        <xsl:apply-templates
          select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/tei:change/tei:respStmt[tei:resp='author'][1]/tei:name"
        />
      </xsl:when>
      <xsl:when test="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docAuthor">
        <xsl:apply-templates mode="author"
          select="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docAuthor[1]"/>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>[common] Find a plausible name of person responsible for current revision</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="generateRevAuthor">
    <xsl:variable name="who">
      <xsl:choose>
        <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/@vcwho">
          <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/@vcwho"/>
        </xsl:when>
        <xsl:when
          test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/tei:change[1]/tei:respStmt/tei:name">
          <xsl:value-of
            select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/tei:change[1]/tei:respStmt/tei:name/text()"
          />
        </xsl:when>
      </xsl:choose>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="normalize-space($who)=concat('$Author', '$')"/>
      <xsl:when test="starts-with($who,'$Author')">
        <!-- it's RCS -->
        <xsl:value-of select="normalize-space(substring-before(substring-after($who,'Author'),'$'))"/>
      </xsl:when>
      <xsl:when test="starts-with($who,'$LastChangedBy')">
        <!-- it's Subversion -->
        <xsl:value-of select="normalize-space(substring-before(substring-after($who,'LastChangedBy:'),'$'))"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$who"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>[common] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="generateAuthorList">
    <xsl:variable name="realauthor">
      <xsl:call-template name="generateAuthor"/>
    </xsl:variable>
    <xsl:variable name="revauthor">
      <xsl:call-template name="generateRevAuthor"/>
    </xsl:variable>
    <xsl:if test="not($realauthor = '')">
      <xsl:text> </xsl:text>
      <xsl:call-template name="i18n">
        <xsl:with-param name="word">authorWord</xsl:with-param>
      </xsl:call-template>
      <xsl:text> </xsl:text>
      <xsl:copy-of select="$realauthor"/>
    </xsl:if>
    <xsl:if test="not($revauthor = '')">
      <xsl:text> (</xsl:text>
      <xsl:call-template name="i18n">
        <xsl:with-param name="word">revisedWord</xsl:with-param>
      </xsl:call-template>
      <xsl:text> </xsl:text>
      <xsl:copy-of select="$revauthor"/>
      <xsl:text>)</xsl:text>
    </xsl:if>
  </xsl:template>
  <xd:doc>
    <xd:short>[common] Work out the last revision date of the document </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="generateRevDate">
    <xsl:variable name="when">
      <xsl:choose>
        <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/@vcdate">
          <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/@vcdate"/>
        </xsl:when>
        <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/descendant::tei:date">
          <xsl:value-of
            select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/descendant::tei:date[1]"/>
        </xsl:when>
      </xsl:choose>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="starts-with($when,'$Date')">
        <!-- it's RCS -->
        <xsl:value-of select="substring($when,16,2)"/>
        <xsl:text>/</xsl:text>
        <xsl:value-of select="substring($when,13,2)"/>
        <xsl:text>/</xsl:text>
        <xsl:value-of select="substring($when,8,4)"/>
      </xsl:when>
      <xsl:when test="starts-with($when,'$LastChangedDate')">
        <!-- it's Subversion-->
        <xsl:value-of select="substring-before(substring-after($when,'('),')')"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$when"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>[common] Work out the publish date of the document </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="generateDate">
    <xsl:choose>
      <xsl:when
        test="$useHeaderFrontMatter='true' and ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docDate">
        <xsl:apply-templates mode="date" select="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docDate"/>
      </xsl:when>
      <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:date">
        <xsl:apply-templates
          select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:date"/>
      </xsl:when>
      <xsl:when
        test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/descendant::tei:date">
        <xsl:apply-templates
          select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/descendant::tei:date[1]"
        />
      </xsl:when>
      <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/tei:edition">
        <xsl:apply-templates
          select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/tei:edition"
        > </xsl:apply-templates>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>[common] Generate a title</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="generateTitle">
    <xsl:choose>
      <xsl:when
        test="$useHeaderFrontMatter='true' and ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docTitle">
        <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docTitle"/>
      </xsl:when>
      <xsl:when
        test="$useHeaderFrontMatter='true' and ancestor-or-self::tei:teiCorpus/tei:text/tei:front//tei:docTitle">
        <xsl:apply-templates select="ancestor-or-self::tei:teiCorpus/tei:text/tei:front//tei:docTitle[1]"/>
      </xsl:when>
      <xsl:when test="self::tei:teiCorpus">	
          <xsl:apply-templates mode="htmlheader"
            select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[not(@type='subordinate')]"/>
      </xsl:when>

      <xsl:otherwise>
        <xsl:for-each select="ancestor-or-self::tei:TEI">
          <xsl:apply-templates mode="htmlheader"
            select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[not(@type='subordinate')][1]"/>
        </xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>[common] Generate sub title </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="generateSubTitle">
    <xsl:choose>
      <xsl:when
        test="$useHeaderFrontMatter='true' and ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docTitle">
        <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docTitle"/>
      </xsl:when>
      <xsl:when
        test="$useHeaderFrontMatter='true' and ancestor-or-self::tei:teiCorpus/tei:text/tei:front//tei:docTitle">
        <xsl:apply-templates select="ancestor-or-self::tei:teiCorpus/tei:text/tei:front//tei:docTitle"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:for-each select="ancestor-or-self::tei:TEI|ancestor-or-self::tei:teiCorpus">
          <xsl:apply-templates mode="htmlheader"
            select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[@type='subordinate']"/>
        </xsl:for-each>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xd:doc>
    <xd:short>[common] </xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template name="whatsTheDate">
    <xsl:choose>
      <xsl:when test="function-available('edate:date-time')">
        <xsl:value-of select="edate:date-time()"/>
      </xsl:when>
      <xsl:when test="contains($processor,'SAXON')">
        <xsl:value-of select="Date:toString(Date:new())" xmlns:Date="/java.util.Date"/>
      </xsl:when>
      <xsl:otherwise> (unknown date) </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xd:doc>
    <xd:short>Process elements  tei:div/tei:docAuthor</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:div/tei:docAuthor"/>
  <xd:doc>
    <xd:short>Process elements  tei:div/tei:docDate</xd:short>
    <xd:detail>
      <p> omit if found outside front matter </p>
    </xd:detail>
  </xd:doc>
  <xsl:template match="tei:div/tei:docDate"/>
  <xd:doc>
    <xd:short>Process elements  tei:div/tei:docTitle</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:div/tei:docTitle"/>
  <xd:doc>
    <xd:short>Process elements  tei:docAuthor</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:docAuthor" mode="heading">
    <xsl:if test="preceding-sibling::tei:docAuthor">
      <xsl:choose>
        <xsl:when test="not(following-sibling::tei:docAuthor)">
          <xsl:text> and </xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>, </xsl:text>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:if>
    <xsl:apply-templates/>
  </xsl:template>

  <xd:doc>
    <xd:short>Process elements  tei:docImprint</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:docImprint"/>

</xsl:stylesheet>
