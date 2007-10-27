<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet exclude-result-prefixes="xd tei edate"
  extension-element-prefixes="edate" version="1.0"
  xmlns:edate="http://exslt.org/dates-and-times"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:xd="http://www.pnp-software.com/XSLTdoc"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xd:doc type="stylesheet">
    <xd:short> TEI stylesheet dealing with elements from the core module. </xd:short>
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
    <xd:copyright>2007, TEI Consortium</xd:copyright>
  </xd:doc>

  <xsl:key name="MNAMES"
   match="tei:monogr/tei:author[tei:surname]|tei:monogr/tei:editor[tei:surname]" 
   use="ancestor::tei:biblStruct/@xml:id"/>
  <xsl:key name="ANAMES"
   match="tei:analytic/tei:author[tei:surname]|tei:analytic/tei:editor[tei:surname]" 
   use ="ancestor::tei:biblStruct/@xml:id"/>
  

  <xd:doc>
    <xd:short>Process all elements in depth</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:*" mode="depth">99</xsl:template>
  <xd:doc>
    <xd:short>Process all elements in plain mode</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:*" mode="plain">
    <xsl:apply-templates mode="plain"/>
  </xsl:template>
  <xsl:template match="tei:note" mode="plain"/>
  <xsl:template match="tei:app" mode="plain"/>
  <xsl:template match="tei:pb" mode="plain"/>
  <xsl:template match="tei:lb" mode="plain"/>
  <xsl:template match="tei:ptr" mode="plain"/>
  <xd:doc>
    <xd:short>Process tei:sic</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:sic">
    <xsl:apply-templates/>
  </xsl:template>
  <xd:doc>
    <xd:short>Process tei:corr</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:corr"/>
  <xd:doc>
    <xd:short>Process tei:item in runin mode</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:item" mode="runin">
    <xsl:text> • </xsl:text>
   <xsl:apply-templates/> 
  </xsl:template>


<xd:doc>
  <xd:short>tei:author inside a bibl</xd:short>
  <xd:detail> </xd:detail>
</xd:doc>
<xsl:template match="tei:bibl/tei:author">
<xsl:apply-templates/>
<xsl:choose>
  <xsl:when test="count(following-sibling::tei:author)=1">
    <xsl:text> and </xsl:text>
  </xsl:when>
  <xsl:when test="following-sibling::tei:author">
    <xsl:text>, </xsl:text>
  </xsl:when>
</xsl:choose>
</xsl:template>

<xd:doc>
  <xd:short>tei:editor</xd:short>
  <xd:detail> </xd:detail>
</xd:doc>
<xsl:template match="tei:editor">
  <xsl:choose>
    <xsl:when test="tei:name">
      <xsl:apply-templates select="tei:name[position()=1]"/>
      <xsl:for-each select="tei:name[position()&gt;1]">
	<xsl:text>, </xsl:text>
	<xsl:apply-templates/>
      </xsl:for-each>
      <xsl:text> (ed).&#10;</xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:apply-templates/>
      <xsl:choose>
	<xsl:when test="count(following-sibling::tei:editor)=1">
	  <xsl:text> and </xsl:text>
	</xsl:when>
	<xsl:when test="following-sibling::tei:author">
	  <xsl:text>, </xsl:text>
	</xsl:when>
	<xsl:when test="preceding-sibling::tei:editor and
			not(following-sibling::tei:editor)">
	  <xsl:text> (eds.)</xsl:text>
	</xsl:when>
	<xsl:when test="not(following-sibling::tei:editor)">
	  <xsl:text> (ed.)</xsl:text>
	</xsl:when>
      </xsl:choose>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

  <xd:doc>
    <xd:short>Process elements tei:edition</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:edition">
    <xsl:apply-templates/>
    <xsl:text>.&#10;</xsl:text>
  </xsl:template>


  <xd:doc>
    <xd:short>Process elements tei:biblScope</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:biblScope">
    <xsl:text> </xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <xd:doc>
    <xd:short>Process elements tei:imprint</xd:short>
    <xd:detail> </xd:detail>
  </xd:doc>
  <xsl:template match="tei:imprint">
    <xsl:apply-templates select="tei:biblScope"/>
    <xsl:if test="tei:publisher">
      <xsl:text>, </xsl:text>
      <xsl:apply-templates select="tei:publisher"/>
    </xsl:if>
    <xsl:if test="tei:pubPlace">
      <xsl:text>, </xsl:text>
      <xsl:apply-templates select="tei:pubPlace"/>
    </xsl:if>
    <xsl:apply-templates select="tei:date"/>
  </xsl:template>

<xsl:template name="makeQuote">
  <xsl:variable name="pre">
    <xsl:choose>
      <xsl:when test="contains(@rend,'PRE')">
	<xsl:choose>
	  <xsl:when test="contains(@rend,'POST')">
	    <xsl:call-template name="getQuote">
	      <xsl:with-param name="quote"
			      select="normalize-space(substring-before(substring-after(@rend,'PRE'),'POST'))"
			      />
	    </xsl:call-template>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:call-template name="getQuote">
	      <xsl:with-param name="quote"
			      select="normalize-space(substring-after(@rend,'PRE'))"/>
	    </xsl:call-template>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="$preQuote"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:variable name="post">
    <xsl:choose>
      <xsl:when test="contains(@rend,'POST')">
	<xsl:call-template name="getQuote">
	  <xsl:with-param name="quote"
			  select="normalize-space(substring-after(@rend,'POST'))"/>
	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="$postQuote"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:value-of select="$pre"/>
  <xsl:apply-templates/>
  <xsl:value-of select="$post"/>
</xsl:template>

<!-- biblStruct -->
<xsl:template match="tei:biblStruct" mode="xref">
    <xsl:choose>
      <xsl:when test="count(key('ANAMES',@xml:id))=1">
	<xsl:value-of select="key('ANAMES',@xml:id)/tei:surname"/>
      </xsl:when>
      <xsl:when test="count(key('ANAMES',@xml:id))=2">
	<xsl:value-of
	    select="key('ANAMES',@xml:id)[1]/tei:surname"/>
	<xsl:text> and </xsl:text>
	<xsl:value-of select="key('ANAMES',@xml:id)[2]/tei:surname"/>
      </xsl:when>
      <xsl:when test="count(key('ANAMES',@xml:id))&gt;2">
	<xsl:value-of
	    select="key('ANAMES',@xml:id)[1]/tei:surname"/>
	<xsl:text> et al.</xsl:text>
      </xsl:when>
      <xsl:when test="count(key('MNAMES',@xml:id))=1">
	<xsl:value-of select="key('MNAMES',@xml:id)/tei:surname"/>
      </xsl:when>
      <xsl:when test="count(key('MNAMES',@xml:id))=2">
	<xsl:value-of
	 select="key('MNAMES',@xml:id)[1]/tei:surname"/>
	<xsl:text> and </xsl:text>
	<xsl:value-of select="key('MNAMES',@xml:id)[2]/tei:surname"/>
      </xsl:when>
      <xsl:when test="count(key('MNAMES',@xml:id))&gt;2">
	<xsl:value-of
	    select="key('MNAMES',@xml:id)[1]/tei:surname"/>
	<xsl:text> et al.</xsl:text>
      </xsl:when>
      <xsl:when test=".//tei:author[tei:surname]">
	<xsl:value-of select=".//tei:author/tei:surname[1]"/>
      </xsl:when>
      <xsl:when test=".//tei:author[tei:orgName]">
	<xsl:value-of select=".//tei:author/tei:orgName[1]"/>
      </xsl:when>
      <xsl:when test=".//tei:author">
	<xsl:value-of select=".//tei:author[1]"/>
      </xsl:when>
      <xsl:when test=".//tei:editor[tei:surname]">
	<xsl:value-of select=".//tei:editor/tei:surname[1]"/>
      </xsl:when>
      <xsl:when test=".//tei:editor">
	<xsl:value-of select=".//tei:editor[1]"/>
      </xsl:when>
      <xsl:otherwise>
	  <xsl:value-of select=".//tei:title[1]"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:choose>
      <xsl:when test="count(tei:*[1]/tei:editor)=1">
	<xsl:text> (ed.)</xsl:text>
      </xsl:when>
      <xsl:when test="count(tei:*[1]/tei:editor)&gt;1">
	<xsl:text> (eds.)</xsl:text>
      </xsl:when>
    </xsl:choose>
    <xsl:if test="tei:monogr/tei:imprint/tei:date">
      <xsl:text> (</xsl:text>
      <xsl:value-of select="tei:monogr/tei:imprint/tei:date"/>
      <xsl:text>)</xsl:text>
    </xsl:if>
</xsl:template>

<xsl:template match="tei:biblStruct">
<!-- biblStruct model is

 analytic*
 (monogr, series*)+
 (model.noteLike | idno | relatedItem)*
-->

  <xsl:apply-templates/>
</xsl:template>


<!-- authors and editors -->
<xsl:template match="tei:author|tei:editor">
  <xsl:choose>
    <!-- last name in a list -->
    <xsl:when test="ancestor::tei:bibl">
      <xsl:apply-templates/>
    </xsl:when>
    <xsl:when test="self::tei:author and not(following-sibling::tei:author)">
      <xsl:apply-templates/>
      <xsl:text>. </xsl:text>
    </xsl:when>
    <xsl:when test="self::tei:editor and not(following-sibling::tei:editor)">
      <xsl:apply-templates/>
      <xsl:text> (</xsl:text>
      <xsl:text>ed</xsl:text>
      <xsl:if test="preceding-sibling::tei:editor">s</xsl:if>
      <xsl:text>.</xsl:text>
      <xsl:text>) </xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <!-- first or middle name in a list -->
      <xsl:apply-templates/>
      <xsl:text>, </xsl:text>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template  match="tei:surname">
  <xsl:if test="../tei:forename">
    <xsl:apply-templates select="../tei:forename" mode="use"/>
    <xsl:text> </xsl:text>
  </xsl:if>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template  match="tei:forename"/>

<xsl:template  match="tei:forename" mode="use">
  <xsl:if test="preceding-sibling::tei:forename">
    <xsl:text> </xsl:text>
  </xsl:if>
  <xsl:apply-templates/>
</xsl:template>

<!-- title  -->
<xsl:template match="tei:title">
  <xsl:choose>
    <xsl:when test="@level='m' or not(@level)">
      <xsl:call-template name="emphasize">
	<xsl:with-param name="class">
	  <xsl:text>titlem</xsl:text>
	</xsl:with-param>
	<xsl:with-param name="content">
	  <xsl:apply-templates/>
	</xsl:with-param>
      </xsl:call-template>
      <xsl:if test="ancestor::tei:biblStruct">
	<xsl:text>, </xsl:text>
      </xsl:if>
    </xsl:when>
    <xsl:when test="@level='s'">
      <xsl:call-template name="emphasize">
	<xsl:with-param name="class">
	  <xsl:text>titles</xsl:text>
	</xsl:with-param>
	<xsl:with-param name="content">
	  <xsl:apply-templates/>
	</xsl:with-param>
      </xsl:call-template>
      <xsl:if test="following-sibling::*">
	<xsl:text> </xsl:text>
      </xsl:if>
    </xsl:when>
    <xsl:when test="@level='j'">
      <xsl:call-template name="emphasize">
	<xsl:with-param name="class">
	  <xsl:text>titlej</xsl:text>
	</xsl:with-param>
	<xsl:with-param name="content">
	  <xsl:apply-templates/>
	</xsl:with-param>
      </xsl:call-template>
      <xsl:text> </xsl:text>
    </xsl:when>
    <xsl:when test="@level='a'">
      <xsl:call-template name="emphasize">
	<xsl:with-param name="class">
	  <xsl:text>titlea</xsl:text>
	</xsl:with-param>
	<xsl:with-param name="content">
	  <xsl:apply-templates/>
	</xsl:with-param>
      </xsl:call-template>
      <xsl:if test="ancestor::tei:biblStruct">
	<xsl:text>. </xsl:text>
      </xsl:if>
    </xsl:when>
    <xsl:when test="@level='u'">
      <xsl:call-template name="emphasize">
	<xsl:with-param name="class">
	  <xsl:text>titleu</xsl:text>
	</xsl:with-param>
	<xsl:with-param name="content">
	  <xsl:apply-templates/>
	</xsl:with-param>
      </xsl:call-template>
      <xsl:if test="ancestor::tei:biblStruct">
	<xsl:text>. </xsl:text>
      </xsl:if>
    </xsl:when>
    <xsl:otherwise>
      <xsl:call-template name="emphasize">
	<xsl:with-param name="class">
	  <xsl:text>titleu</xsl:text>
	</xsl:with-param>
	<xsl:with-param name="content">
	  <xsl:apply-templates/>
	</xsl:with-param>
      </xsl:call-template>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


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

<xsl:template match="tei:meeting">
    <xsl:text> (</xsl:text>
      <xsl:apply-templates/>
    <xsl:text>)</xsl:text>
    <xsl:if test="following-sibling::*">
      <xsl:text> </xsl:text>
    </xsl:if>
</xsl:template>

<xsl:template match="tei:series">
  <xsl:apply-templates/>
</xsl:template>
<!-- publisher, imprint  -->
<xsl:template match="tei:imprint">
   <xsl:apply-templates select="tei:date"/>
   <xsl:apply-templates select="tei:pubPlace"/>
   <xsl:apply-templates select="tei:publisher"/>
   <xsl:apply-templates select="tei:biblScope"/>
</xsl:template>

<xsl:template match="tei:date">
 <xsl:apply-templates/>
 <xsl:if test="ancestor::tei:biblStruct">
   <xsl:text>. </xsl:text>
 </xsl:if>
</xsl:template>

<xsl:template match="tei:pubPlace">
  <xsl:choose>
    <xsl:when test="@rendition">
      <span>
	<xsl:call-template name="applyRendition"/>
	<xsl:apply-templates/>
      </span>
    </xsl:when>
    <xsl:otherwise>     
      <xsl:apply-templates/>
    </xsl:otherwise>
  </xsl:choose>
 <xsl:choose>
   <xsl:when test="ancestor::tei:bibl"/>
   <xsl:when test="following-sibling::tei:pubPlace">
     <xsl:text>, </xsl:text>
   </xsl:when>
   <xsl:when test="../tei:publisher">
     <xsl:text>: </xsl:text>
   </xsl:when>
   <xsl:otherwise>
     <xsl:text>. </xsl:text>
   </xsl:otherwise>
 </xsl:choose>
</xsl:template>

<xsl:template match="tei:publisher">
  <xsl:choose>
    <xsl:when test="@rendition">
      <span>
	<xsl:call-template name="applyRendition"/>
	<xsl:apply-templates/>
      </span>
    </xsl:when>
    <xsl:otherwise>     
      <xsl:apply-templates/>
    </xsl:otherwise>
  </xsl:choose>

   <xsl:if test="ancestor::tei:biblStruct">
     <xsl:text>. </xsl:text>
   </xsl:if>
</xsl:template>

<!-- details and notes -->
<xsl:template match="tei:biblScope">
 <xsl:choose>
   <xsl:when test="ancestor::tei:bibl">
     <xsl:apply-templates/>
   </xsl:when>
  <xsl:when test="@type='vol'">
    <xsl:call-template name="emphasize">
      <xsl:with-param name="class">
	<xsl:text>vol</xsl:text>
      </xsl:with-param>
      <xsl:with-param name="content">
	<xsl:apply-templates/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:when>
  <xsl:when test="@type='chap'">
   <xsl:text>chapter </xsl:text>
   <xsl:apply-templates/>
  </xsl:when>
  <xsl:when test="@type='issue'">
    <xsl:text> (</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>) </xsl:text>
  </xsl:when>
  <xsl:when test="@type='pp'">
    <xsl:choose>
      <xsl:when test="contains(.,'-')">
	<xsl:text>pp. </xsl:text>
      </xsl:when>
      <xsl:when test="contains(.,'ff')">
	<xsl:text>pp. </xsl:text>
      </xsl:when>
      <xsl:when test="contains(.,' ')">
	<xsl:text>pp. </xsl:text>
      </xsl:when>
      <xsl:otherwise>
	<xsl:text>p. </xsl:text>
      </xsl:otherwise>
    </xsl:choose>
   <xsl:apply-templates/>
  </xsl:when>
  <xsl:otherwise>
   <xsl:apply-templates/>
  </xsl:otherwise>
 </xsl:choose>
 
 <xsl:choose>
   <xsl:when test="@type='vol' and
		   following-sibling::tei:biblScope[@type='issue']">
     <xsl:text> </xsl:text>
   </xsl:when>
   <xsl:when test="@type='vol' and following-sibling::tei:biblScope">
     <xsl:text> </xsl:text>
   </xsl:when>
   <xsl:when test="following-sibling::tei:biblScope">
     <xsl:text> </xsl:text>
   </xsl:when>
   <xsl:when test="ancestor::tei:biblStruct">
     <xsl:text>. </xsl:text>
   </xsl:when>
 </xsl:choose>

</xsl:template>

<xsl:template match="tei:idno">
  <xsl:apply-templates/>
</xsl:template>



</xsl:stylesheet>
