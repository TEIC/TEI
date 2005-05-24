<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
    xmlns:xd="http://www.pnp-software.com/XSLTdoc"
    xmlns:tei="http://www.tei-c.org/ns/1.0" 
    xmlns:edate="http://exslt.org/dates-and-times" 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    extension-element-prefixes="edate" 
    exclude-result-prefixes="tei edate xd" 
    version="1.0">
  <xd:doc type="stylesheet">
    <xd:short>
    TEI stylesheet
    dealing  with elements from the
      header module.
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
  <xd:doc>
    <xd:short>Process elements  tei:title</xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template match="tei:title" mode="htmlheader">
  <xsl:apply-templates/>
</xsl:template>
  <xd:doc>
    <xd:short>[common] </xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template name="generateAuthor">
 <xsl:choose>
   <xsl:when test="$useHeaderFrontMatter='true' and ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docAuthor">
     <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docAuthor[1]" mode="author"/>
  </xsl:when>
  <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:author">
  <xsl:for-each
      select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:author">
    <xsl:apply-templates/>
    <xsl:choose>
      <xsl:when test="count(following-sibling::tei:author)=1"> and </xsl:when>
      <xsl:when test="following-sibling::tei:author">, </xsl:when>
    </xsl:choose>
  </xsl:for-each>
  </xsl:when>
    <xsl:when test="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docAuthor">
      <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docAuthor[1]" mode="author"/>
  </xsl:when>
  </xsl:choose>
</xsl:template>
  <xd:doc>
    <xd:short>[common] </xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template name="generateAuthorList">
<xsl:variable name="realauthor">
  <xsl:call-template name="generateAuthor"/>
</xsl:variable>
<xsl:variable name="revauthor">
<xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/tei:change[1]/tei:respStmt/tei:name/text()"/>
</xsl:variable>
<xsl:if test="not($realauthor = '')">
  <xsl:text> </xsl:text>
  <xsl:value-of select="$authorWord"/>
  <xsl:text> </xsl:text>
  <xsl:value-of select="$realauthor"/>
</xsl:if>
<xsl:if test="not($revauthor = '') and not(normalize-space($revauthor)=concat('$Author', '$'))">
 (<xsl:value-of select="$revisedWord"/><xsl:text> </xsl:text>
 <xsl:choose>
  <xsl:when test="starts-with($revauthor,'$Author')"> <!-- it's RCS/CVS -->
    <xsl:value-of select="normalize-space(substring-before(substring-after($revauthor,'Author'),'$'))"/>
  </xsl:when>
  <xsl:when test="starts-with($revauthor,'$LastChangedBy')"> <!-- it's Subversion -->
    <xsl:value-of select="normalize-space(substring-before(substring-after($revauthor,'LastChangedBy:'),'$'))"/>
  </xsl:when>
  <xsl:otherwise>
   <xsl:value-of select="$revauthor"/>    
  </xsl:otherwise>
 </xsl:choose>
 <xsl:text>)</xsl:text>
</xsl:if>

</xsl:template>
  <xd:doc>
    <xd:short>[common] </xd:short>
    <xd:param name="showRev">whether to show revision date</xd:param>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template name="generateDate">
  <xsl:param name="showRev">true</xsl:param>
<xsl:variable name="realdate">
 <xsl:choose>
   <xsl:when test="$useHeaderFrontMatter='true' and ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docDate">
  <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docDate" mode="date"/>
  </xsl:when>
  <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/descendant::tei:date">
  <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/descendant::tei:date[1]"/>
    </xsl:when>
  </xsl:choose>
</xsl:variable>

<xsl:variable name="revdate">
<xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/descendant::tei:date[1]"/>
</xsl:variable>
<xsl:value-of select="$dateWord"/><xsl:text> </xsl:text>
<xsl:if test="not($realdate = '')">
  <xsl:value-of select="$realdate"/>
</xsl:if>


<xsl:if test="$showRev='true' and not($revdate = '') and not ($revdate=concat('$Date',$'))">
  (<xsl:value-of select="$revisedWord"/><xsl:text> </xsl:text>
  <xsl:choose>
  <xsl:when test="starts-with($revdate,'$Date')"> <!-- it's RCS -->
    <xsl:value-of select="substring($revdate,16,2)"/>
    <xsl:text>/</xsl:text>
    <xsl:value-of select="substring($revdate,13,2)"/>
    <xsl:text>/</xsl:text>
    <xsl:value-of select="substring($revdate,8,4)"/> 
  </xsl:when>
  <xsl:when test="starts-with($revdate,'$LastChangedDate')"> <!-- it's SVN -->
    <xsl:value-of select="substring-before(substring-after($revdate,'('),')')"/>
  </xsl:when>
  <xsl:otherwise>
   <xsl:value-of select="$revdate"/>    
  </xsl:otherwise>
 </xsl:choose>
 <xsl:text>) </xsl:text></xsl:if>

</xsl:template>
  <xd:doc>
    <xd:short>[common] </xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template name="generateTitle">
 <xsl:choose>
   <xsl:when test="$useHeaderFrontMatter='true' and ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docTitle">
     <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docTitle"/>
   </xsl:when>
   <xsl:when test="$useHeaderFrontMatter='true' and ancestor-or-self::tei:teiCorpus/tei:text/tei:front//tei:docTitle">
     <xsl:apply-templates select="ancestor-or-self::tei:teiCorpus/tei:text/tei:front//tei:docTitle"/>
     </xsl:when>
   <xsl:otherwise>
     <xsl:for-each select="ancestor-or-self::tei:TEI|ancestor-or-self::tei:teiCorpus">
       <xsl:apply-templates select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title" mode="htmlheader"/>
     </xsl:for-each>
   </xsl:otherwise>
</xsl:choose>
</xsl:template>
  <xd:doc>
    <xd:short>[common] </xd:short>
    <xd:detail>&#160;</xd:detail>
  </xd:doc>
  <xsl:template name="whatsTheDate">
 <xsl:choose>
 <xsl:when test="function-available('edate:date-time')">
  <xsl:value-of select="edate:date-time()"/>
 </xsl:when>
 <xsl:when test="contains($processor,'SAXON')">
   <xsl:value-of xmlns:Date="/java.util.Date" select="Date:toString(Date:new())"/>
 </xsl:when>
 <xsl:otherwise>
   (unknown)
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>
</xsl:stylesheet>
