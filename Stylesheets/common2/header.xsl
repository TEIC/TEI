<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:tei="http://www.tei-c.org/ns/1.0"
                
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="tei"
                version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet dealing with elements from the header module. </p>
         <p> This library is free software; you can redistribute it and/or modify it under the terms of the
      GNU Lesser General Public License as published by the Free Software Foundation; either version 2.1 of
      the License, or (at your option) any later version. This library is distributed in the hope that it will
      be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
      A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details. You should have
      received a copy of the GNU Lesser General Public License along with this library; if not, write to the
      Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA </p>
         <p>Author: See AUTHORS</p>
         <p>Id: $Id$</p>
         <p>Copyright: 2008, TEI Consortium</p>
      </desc>
   </doc>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[common] Find a plausible main author name</desc>
   </doc>
  <xsl:template name="generateAuthor">
      <xsl:choose>
         <xsl:when test="$useHeaderFrontMatter='true' and ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docAuthor">
            <xsl:apply-templates mode="author"
                                 select="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docAuthor"/>
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
         <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/tei:change/tei:respStmt[tei:resp='author']">
            <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/tei:change/tei:respStmt[tei:resp='author'][1]/tei:name"/>
         </xsl:when>
         <xsl:when test="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docAuthor">
            <xsl:apply-templates mode="author"
                                 select="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docAuthor"/>
         </xsl:when>
      </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[common] Find a plausible name of person responsible for current revision</desc>
   </doc>
  <xsl:template name="generateRevAuthor">
      <xsl:variable name="who">
         <xsl:choose>
            <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/@vcwho">
               <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/@vcwho"/>
            </xsl:when>
            <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/tei:change[1]/tei:respStmt/tei:name">
               <xsl:value-of select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/tei:change[1]/tei:respStmt/tei:name/text()"/>
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
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[common] </desc>
   </doc>
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
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[common] Work out the last revision date of the document </desc>
   </doc>
  <xsl:template name="generateRevDate">
      <xsl:variable name="when">
         <xsl:choose>
            <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/@vcdate">
               <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/@vcdate"/>
            </xsl:when>
            <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/descendant::tei:date">
               <xsl:value-of select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/descendant::tei:date[1]"/>
            </xsl:when>
            <xsl:when
		test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/descendant::tei:date">
	      <xsl:value-of select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/descendant::tei:date"/>
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

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[common] Work out the publish date of the document </desc>
   </doc>
  <xsl:template name="generateDate">
      <xsl:choose>
         <xsl:when test="$useHeaderFrontMatter='true' and ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docDate">
            <xsl:apply-templates mode="date" select="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docDate"/>
         </xsl:when>
         <xsl:when
	     test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/descendant::tei:date[@when]">
            <xsl:value-of select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/descendant::tei:date[@when][1]/@when"/>
         </xsl:when>
         <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/descendant::tei:date">
            <xsl:value-of select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/descendant::tei:date[1]"/>
         </xsl:when>
         <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:date">
            <xsl:value-of select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:date"/>
         </xsl:when>
         <xsl:when test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/tei:edition">
            <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/tei:edition"/>
         </xsl:when>
	 <xsl:when
	     test="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/tei:change[@when
		   or tei:date]">
            <xsl:for-each
		select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:revisionDesc/tei:change[1]">
	      <xsl:choose>
		<xsl:when test="@when">
		  <xsl:value-of select="@when"/>
		</xsl:when>
		<xsl:when test="tei:date/@when">
		  <xsl:value-of select="tei:date/@when"/>
		</xsl:when>
		<xsl:when test="tei:date">
		  <xsl:value-of select="tei:date"/>
		</xsl:when>
		<xsl:otherwise>
		  <xsl:value-of select="format-dateTime(current-dateTime(),'[Y]-[M02]-[D02]')"/>
		</xsl:otherwise>
	      </xsl:choose>
	    </xsl:for-each>
	 </xsl:when>
	 <xsl:when test="$useFixedDate='true'">1970-01-01</xsl:when>
	 <xsl:otherwise>
	   <xsl:value-of select="format-dateTime(current-dateTime(),'[Y]-[M02]-[D02]')"/>
	 </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[common] Generate a title</desc>
   </doc>
  <xsl:template name="generateTitle">
      <xsl:choose>
         <xsl:when test="$useHeaderFrontMatter='true' and ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docTitle">
            <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docTitle/tei:titlePart"/>
         </xsl:when>

         <xsl:when test="$useHeaderFrontMatter='true' and ancestor-or-self::tei:teiCorpus/tei:text/tei:front//tei:docTitle">
            <xsl:apply-templates select="ancestor-or-self::tei:teiCorpus/tei:text/tei:front//tei:docTitle/tei:titlePart"/>
         </xsl:when>

         <xsl:when test="self::tei:teiCorpus">	
            <xsl:apply-templates select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[not(@type='subordinate')]"/>
         </xsl:when>

         <xsl:otherwise>
            <xsl:for-each
		select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt">
	      <xsl:choose>
		<xsl:when test="tei:title[@type='main']">
		  <xsl:apply-templates select="tei:title"/>
		</xsl:when>
		<xsl:otherwise>
		  <xsl:apply-templates select="tei:title"/>
		</xsl:otherwise>
	      </xsl:choose>
	    </xsl:for-each>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>[common] </p>
         <p>Generate simple title with no markup</p>
      </desc>
   </doc>
  <xsl:template name="generateSimpleTitle">
      <xsl:choose>
         <xsl:when test="$useHeaderFrontMatter='true' and ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docTitle">
            <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docTitle"
                                 mode="simple"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:for-each
		select="ancestor-or-self::tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt">
	      <xsl:choose>
		<xsl:when test="tei:title[@type='main']">
		  <xsl:apply-templates
		      select="tei:title[@type='main']" mode="simple"/>
		</xsl:when>
		<xsl:otherwise>
		  <xsl:apply-templates select="tei:title[1]" mode="simple"/>
		</xsl:otherwise>
	      </xsl:choose>
	    </xsl:for-each>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[common] Generate sub title </desc>
   </doc>
  <xsl:template name="generateSubTitle">
      <xsl:choose>
         <xsl:when test="$useHeaderFrontMatter='true' and ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docTitle">
            <xsl:apply-templates select="ancestor-or-self::tei:TEI/tei:text/tei:front//tei:docTitle"/>
         </xsl:when>
         <xsl:when test="$useHeaderFrontMatter='true' and ancestor-or-self::tei:teiCorpus/tei:text/tei:front//tei:docTitle">
            <xsl:apply-templates select="ancestor-or-self::tei:teiCorpus/tei:text/tei:front//tei:docTitle"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:for-each select="ancestor-or-self::tei:TEI|ancestor-or-self::tei:teiCorpus">
               <xsl:apply-templates select="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[@type='subordinate']"/>
            </xsl:for-each>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:div/tei:docAuthor"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
    
         <p xmlns=""> Omit if found outside front matter </p>
    
      </desc>
   </doc>
  <xsl:template match="tei:div/tei:docDate"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:div/tei:docTitle"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Ignore docTitle in a div</desc>
   </doc>
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

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc/>
   </doc>
  <xsl:template match="tei:docImprint"/>

   <xsl:template match="tei:idno[@type='url']">
      <xsl:text> &lt;</xsl:text>
      <xsl:call-template name="makeExternalLink">
         <xsl:with-param name="ptr">true</xsl:with-param>
         <xsl:with-param name="dest">
            <xsl:value-of select="normalize-space(.)"/>
         </xsl:with-param>
      </xsl:call-template>
      <xsl:text>&gt;.</xsl:text>
   </xsl:template>


   <xsl:template match="tei:idno">
      <xsl:text> </xsl:text>
      <xsl:apply-templates/>
   </xsl:template>

   <xsl:template match="tei:idno[@type='doi']"/>

</xsl:stylesheet>