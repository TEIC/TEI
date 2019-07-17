<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:tei="http://www.tei-c.org/ns/1.0"
		xmlns:tite="http://www.tei-c.org/ns/tite/1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="tei tite"
                version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet dealing with elements from the core module. </p>
         <p>This software is dual-licensed:

1. Distributed under a Creative Commons Attribution-ShareAlike 3.0
Unported License http://creativecommons.org/licenses/by-sa/3.0/ 

2. http://www.opensource.org/licenses/BSD-2-Clause
		


Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

* Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

This software is provided by the copyright holders and contributors
"as is" and any express or implied warranties, including, but not
limited to, the implied warranties of merchantability and fitness for
a particular purpose are disclaimed. In no event shall the copyright
holder or contributors be liable for any direct, indirect, incidental,
special, exemplary, or consequential damages (including, but not
limited to, procurement of substitute goods or services; loss of use,
data, or profits; or business interruption) however caused and on any
theory of liability, whether in contract, strict liability, or tort
(including negligence or otherwise) arising in any way out of the use
of this software, even if advised of the possibility of such damage.
</p>
         <p>Author: See AUTHORS</p>
         
         <p>Copyright: 2013, TEI Consortium</p>
      </desc>
   </doc>
  <xsl:output indent="no"/>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process all elements to find out their nesting depth</desc>
   </doc>
  <xsl:template match="*" mode="depth">99</xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element argument</desc>
  </doc>
  <xsl:template match="tei:argument">
    <xsl:call-template name="makeBlock">
      <xsl:with-param name="style">argument</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element salute</desc>
  </doc>
  <xsl:template match="tei:salute">
    <xsl:choose>
      <xsl:when test="parent::tei:closer">
        <xsl:apply-templates/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="makeBlock">
	  <xsl:with-param name="style">salute</xsl:with-param>
	</xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process tei:sic</desc>
   </doc>
  <xsl:template match="tei:sic">
     <xsl:choose>
       <xsl:when test="parent::tei:choice">
       </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="makeInline">
	  <xsl:with-param name="after">}</xsl:with-param>
	  <xsl:with-param name="before">{</xsl:with-param>
	</xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process tei:supplied</desc>
   </doc>
  <xsl:template match="tei:supplied">
     <xsl:choose>
       <xsl:when test="parent::tei:choice">
       </xsl:when>
       <xsl:when test="@reason='damage'">
	 <xsl:call-template name="makeInline">
	   <xsl:with-param name="style">supplied</xsl:with-param>
	   <xsl:with-param name="before">&lt;</xsl:with-param>
	   <xsl:with-param name="after">&gt;</xsl:with-param>
	 </xsl:call-template>
       </xsl:when>
       <xsl:when test="@reason='illegible' or not(@reason)">
	 <xsl:call-template name="makeInline">
	   <xsl:with-param name="style">supplied</xsl:with-param>
	   <xsl:with-param name="before">[</xsl:with-param>
	   <xsl:with-param name="after">]</xsl:with-param>
	 </xsl:call-template>
       </xsl:when>
       <xsl:when test="@reason='omitted'">
	 <xsl:call-template name="makeInline">
	   <xsl:with-param name="style">supplied</xsl:with-param>
	   <xsl:with-param name="before">⟨</xsl:with-param>
	   <xsl:with-param name="after">⟩</xsl:with-param>
	 </xsl:call-template>
       </xsl:when>
       <xsl:otherwise>
	 <xsl:call-template name="makeInline">
	   <xsl:with-param name="style">supplied</xsl:with-param>
	   <xsl:with-param name="after">}</xsl:with-param>
	   <xsl:with-param name="before">{</xsl:with-param>
	 </xsl:call-template>
       </xsl:otherwise>
     </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process tei:reg</desc>
   </doc>
   <xsl:template match="tei:reg">
     <xsl:call-template name="makeInline"/>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process tei:orig</desc>
   </doc>
   <xsl:template match="tei:orig">
     <xsl:choose>
       <xsl:when test="parent::tei:choice">
       </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="makeInline"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process tei:expan</desc>
   </doc>
  <xsl:template match="tei:expan">
    <xsl:call-template name="makeInline"/>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process tei:corr</desc>
   </doc>
  <xsl:template match="tei:corr">
    <xsl:call-template name="makeInline">
      <xsl:with-param name="before">[</xsl:with-param>
      <xsl:with-param name="after">]</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process tei:abbr (abbrevation)</desc>
   </doc>
  <xsl:template match="tei:abbr">
    <xsl:choose>
      <xsl:when test="parent::tei:choice/tei:expan">
	<xsl:call-template name="makeInline">
	  <xsl:with-param name="before"> (</xsl:with-param>
	  <xsl:with-param name="after">)</xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="makeInline"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element signed</desc>
  </doc>
  <xsl:template match="tei:signed">
    <xsl:call-template name="makeBlock">
      <xsl:with-param name="style">signed</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element item</desc>
  </doc>
  <xsl:template match="tei:item">
    <xsl:choose>
      <xsl:when test="parent::tei:list[@type='gloss'] or preceding-sibling::tei:label">
	<xsl:call-template name="makeLabelItem"/>          
      </xsl:when>
      <xsl:when test="parent::tei:list[@type='elementlist']">
        <xsl:apply-templates/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="makeItem"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process tei:item in runin mode</desc>
   </doc>
  <xsl:template match="tei:item" mode="inline">
    <xsl:if test="preceding-sibling::tei:item">, </xsl:if>
    <xsl:if test="not(following-sibling::tei:item) and preceding-sibling::tei:item">
      and </xsl:if>
    <xsl:text>• </xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&#160;</xsl:text>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element edition</desc>
   </doc>
  <xsl:template match="tei:edition">
      <xsl:apply-templates/>
      <xsl:if test="ancestor::tei:biblStruct or ancestor::tei:biblFull">
         <xsl:call-template name="makeText">
	   <xsl:with-param name="letters">. </xsl:with-param>
	 </xsl:call-template>
      </xsl:if>
  </xsl:template>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element imprint</desc>
   </doc>
  <xsl:template match="tei:imprint">
      <xsl:choose>
	<xsl:when test="ancestor::tei:biblStruct or ancestor::tei:biblFull">
	  <xsl:apply-templates select="tei:date"/>
	  <xsl:apply-templates select="tei:pubPlace"/>
	  <xsl:apply-templates select="tei:publisher"/>
	  <xsl:apply-templates select="tei:biblScope"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:apply-templates/>
	</xsl:otherwise>
      </xsl:choose>
  </xsl:template>


   <!-- authors and editors -->
   <xsl:template match="tei:editor|tei:author">
     <xsl:choose>
       <xsl:when test="ancestor::tei:bibl">
	 <xsl:apply-templates select="if (tei:surname) then * else node()"/>
       </xsl:when>
       <xsl:when test="self::tei:author and not(following-sibling::tei:author)">
	 <xsl:apply-templates select="if (tei:surname) then * else node()"/>
	 <xsl:call-template name="makeText">
	   <xsl:with-param name="letters">. </xsl:with-param>
	 </xsl:call-template>
       </xsl:when>
       <xsl:when test="self::tei:editor and not(following-sibling::tei:editor)">
	 <xsl:apply-templates select="if (tei:surname) then * else node()"/>
	 <xsl:call-template name="makeText">
	   <xsl:with-param name="letters">
	   <xsl:value-of select="if (preceding-sibling::tei:editor) then
				 ' (eds.) ' else ' (ed.) '"/>
	   </xsl:with-param>
	 </xsl:call-template>
       </xsl:when>
       <xsl:otherwise>
	 <xsl:apply-templates select="if (tei:surname) then * else node()"/>
	 <xsl:call-template name="makeText">
	   <xsl:with-param name="letters">, </xsl:with-param>
	 </xsl:call-template>
       </xsl:otherwise>
     </xsl:choose>
   </xsl:template>

   <xsl:template match="tei:author|tei:editor" mode="mla">
     <!-- <xsl:variable name="totalNbr">
	  <xsl:number select="ancestor::tei:listBibl"/>
        </xsl:variable>
        <xsl:value-of select="$totalNbr"/>. 
        <xsl:choose>
        <xsl:when test="self::tei:author[1] = parent::tei:analytic/parent::tei:biblStruct/preceding-sibling::tei:biblStruct/*/tei:author[1] or self::tei:author[1] =
        parent::tei:analytic/parent::tei:biblStruct/preceding-sibling::tei:biblStruct/tei:monogr/tei:editor[1]">
        <xsl:text>[three hyphens]</xsl:text>
        <xsl:choose>
        <xsl:when test="self::tei:author and following-sibling::tei:author"><xsl:text>, </xsl:text></xsl:when>
        <xsl:otherwise><xsl:text>. </xsl:text></xsl:otherwise>
        </xsl:choose>
        </xsl:when>
        <xsl:when test="self::tei:author[1] = parent::tei:monogr/parent::tei:biblStruct/preceding-sibling::tei:biblStruct/*/tei:author[1] and not(preceding-sibling::tei:analytic) or self::tei:author[1] =
        parent::tei:monogr/parent::tei:biblStruct/preceding-sibling::tei:biblStruct/tei:monogr/tei:editor[1] and not(preceding-sibling::tei:analytic)">
        <xsl:text>[three hyphens]</xsl:text>
        <xsl:choose>
        <xsl:when test="self::tei:author and following-sibling::tei:author"><xsl:text>, </xsl:text></xsl:when>
        <xsl:otherwise><xsl:text>. </xsl:text></xsl:otherwise>
        </xsl:choose>
        </xsl:when>
        <xsl:when test="self::tei:editor[1] = parent::tei:*/parent::tei:biblStruct/preceding-sibling::tei:biblStruct/*/tei:author[1] and
        not(preceding-sibling::tei:analytic) or self::tei:editor[1]
        = parent::tei:*/parent::tei:biblStruct/preceding-sibling::tei:biblStruct/tei:monogr/tei:editor[1] and not(preceding-sibling::tei:analytic)">
        <xsl:text>[three hyphens]</xsl:text>
        <xsl:text>, </xsl:text>
        </xsl:when> 
        TAKE OUT THE EXTRA OPEN CHOOSE BEFORE YOU ADD THIS BACK IN-->
     <xsl:choose>
       <xsl:when test="self::tei:author and not(following-sibling::tei:author)">
	 <xsl:choose>
	   <xsl:when test="ancestor::tei:biblStruct and not(preceding-sibling::tei:author)">
	     <xsl:apply-templates/>
	     <xsl:if test="not(ends-with(.,'.'))">
	       <xsl:text>. </xsl:text>
	     </xsl:if>
	   </xsl:when>
	   <xsl:otherwise>
	     <xsl:if test="not(self::tei:author[3])">
	       <xsl:text> and </xsl:text>
	     </xsl:if>
	     <xsl:choose>
	       <xsl:when test="contains(self::tei:author, ',')">
		 <xsl:value-of select="substring-after(., ',')"/>
		 <xsl:text> </xsl:text>
		 <xsl:value-of select="substring-before(., ',')"/>
	       </xsl:when>
	       <xsl:otherwise>
		 <xsl:apply-templates/>
	       </xsl:otherwise>
	     </xsl:choose>
	     <xsl:if test="not(ends-with(.,'.'))">
	       <xsl:text>. </xsl:text>
	     </xsl:if>
	   </xsl:otherwise>
	 </xsl:choose>
	 <xsl:text> </xsl:text>
       </xsl:when>
       <xsl:when test="self::tei:author and following-sibling::tei:author">
	 <xsl:choose>
	   <xsl:when test="ancestor::tei:biblStruct and not(preceding-sibling::tei:author)">
	     <xsl:apply-templates/>
	     <xsl:text>, </xsl:text>
	   </xsl:when>
	   <xsl:otherwise>
	     <xsl:choose>
	       <xsl:when test="contains(self::tei:author, ',')">
		 <xsl:value-of select="substring-after(., ',')"/>
		 <xsl:text> </xsl:text>
		 <xsl:value-of select="substring-before(., ',')"/>
		 <xsl:text>, and </xsl:text>
	       </xsl:when>
	       <xsl:otherwise>
		 <xsl:apply-templates/>
		 <xsl:text>, and </xsl:text>
	       </xsl:otherwise>
	     </xsl:choose>
	   </xsl:otherwise>
	 </xsl:choose>
       </xsl:when>
       <xsl:when test="self::tei:editor[@role='translator'] and ancestor::tei:biblStruct">
	 <xsl:choose>
	   <xsl:when test="preceding-sibling::tei:editor[@role='editor']/text() = self::tei:editor[@role='translator']/text()">
	     <xsl:text>Ed. and Trans. </xsl:text>
	   </xsl:when>
	   <xsl:when test="not(preceding-sibling::tei:editor[@role='translator'])">				
	     <xsl:text>Trans. </xsl:text>
	   </xsl:when>
	   <xsl:when test="preceding-sibling::tei:editor[@role='translator'] and following-sibling::tei:editor[@role='translator']">
	     <xsl:text>, </xsl:text>
	   </xsl:when>
	   <xsl:otherwise><xsl:text> and </xsl:text></xsl:otherwise>
	 </xsl:choose>
	 <xsl:choose>
	   <xsl:when test="contains(self::tei:editor[@role='translator'], ',')">
	     <xsl:value-of select="substring-after(., ',')"/>
	     <xsl:text> </xsl:text>
	     <xsl:value-of select="substring-before(., ',')"/>
	   </xsl:when>
	   <xsl:otherwise>
	     <xsl:apply-templates/>
	   </xsl:otherwise>
	 </xsl:choose>
	 <xsl:if test="not(following-sibling::tei:editor[@role='translator'])">
	   <xsl:text>. </xsl:text>
	 </xsl:if>
       </xsl:when>
       <xsl:when test="self::tei:editor[@role='editor'] and not(parent::tei:monogr/parent::tei:biblStruct/tei:analytic) and not(preceding-sibling::tei:author)">
	 <xsl:choose>
	   <xsl:when test="ancestor::tei:biblStruct and not(following-sibling::tei:editor[@role='editor']) and not(preceding-sibling::tei:editor[@role='editor'])">
	     <xsl:apply-templates/>
	     <xsl:text>, ed. </xsl:text>
	   </xsl:when>
	   <xsl:when test="ancestor::tei:biblStruct and following-sibling::tei:editor[@role='editor'] and not(preceding-sibling::tei:editor[@role='editor'])">
	     <xsl:apply-templates/>
	     <xsl:choose>
	       <xsl:when test="position() + 1 = last()">
		 <xsl:text> </xsl:text>
	       </xsl:when>
	       <xsl:otherwise>
		 <xsl:text>, </xsl:text>
	       </xsl:otherwise>
	     </xsl:choose>
	   </xsl:when>
	   <xsl:when test="ancestor::tei:biblStruct and following-sibling::tei:editor[@role='editor']">
	     <xsl:choose>
	       <xsl:when test="contains(self::tei:editor[@role='editor'], ',')">						
		 <xsl:value-of select="substring-after(., ',')"/>
		 <xsl:text> </xsl:text>
		 <xsl:value-of select="substring-before(., ',')"/>
	       </xsl:when>
	       <xsl:otherwise>
		 <xsl:apply-templates/>
	       </xsl:otherwise>
	     </xsl:choose>
	     <xsl:choose>
	       <xsl:when test="position() + 1 = last()">
		 <xsl:text> </xsl:text>
	       </xsl:when>
	       <xsl:otherwise>
		 <xsl:text>, </xsl:text>
	       </xsl:otherwise>
	     </xsl:choose>
	   </xsl:when>
	   <xsl:when test="ancestor::tei:biblStruct and not(following-sibling::tei:editor[@role='editor'])">
	     <xsl:choose>
	       <xsl:when test="preceding-sibling::tei:editor[@role='editor']">
		 <xsl:text>and </xsl:text>
		 <xsl:choose>
		   <xsl:when test="contains(self::tei:editor[@role='editor'], ',')">						
		     <xsl:value-of select="substring-after(., ',')"/>
		     <xsl:text> </xsl:text>
		     <xsl:value-of select="substring-before(., ',')"/>
		   </xsl:when>
		   <xsl:otherwise>
		     <xsl:apply-templates/>
		   </xsl:otherwise>
		 </xsl:choose>
	       </xsl:when>
	       <xsl:otherwise>
		 <xsl:apply-templates/>
	       </xsl:otherwise>
	     </xsl:choose>
	     <xsl:choose>
	       <xsl:when test="../tei:editor[@role='editor'][2]">
		 <xsl:text>, eds. </xsl:text>
	       </xsl:when>
	       <xsl:otherwise>
		 <xsl:text>, ed. </xsl:text>
	       </xsl:otherwise>
	     </xsl:choose>
	   </xsl:when>
	 </xsl:choose>
       </xsl:when>
       <xsl:when test="self::tei:editor[@role='editor'] and not(following-sibling::tei:editor[@role='editor'])">
	 <xsl:choose>
	   <xsl:when test="ancestor::tei:biblStruct and not(preceding-sibling::tei:editor[@role='editor'])">
	     <xsl:text>Ed. </xsl:text>
	     <xsl:choose>
	       <xsl:when test="contains(self::tei:editor[@role='editor'], ',')">						
		 <xsl:value-of select="substring-after(., ',')"/>
		 <xsl:text> </xsl:text>
		 <xsl:value-of select="substring-before(., ',')"/>
	       </xsl:when>
	       <xsl:otherwise>
		 <xsl:apply-templates/>
	       </xsl:otherwise>
	     </xsl:choose>
	     <xsl:text>. </xsl:text>
	   </xsl:when>
	   <xsl:when test="ancestor::tei:biblStruct and preceding-sibling::tei:editor[@role='editor']">
	     <xsl:text>and </xsl:text>
	     <xsl:choose>
	       <xsl:when test="contains(self::tei:editor[@role='editor'], ',')">						
		 <xsl:value-of select="substring-after(., ',')"/>
		 <xsl:text> </xsl:text>
		 <xsl:value-of select="substring-before(., ',')"/>
	       </xsl:when>
	       <xsl:otherwise>
		 <xsl:apply-templates/>
	       </xsl:otherwise>
	     </xsl:choose>
	     <xsl:text>. </xsl:text>
	   </xsl:when>
	   <xsl:otherwise>
	     <xsl:apply-templates/>
	     <xsl:text> (</xsl:text>
	     <xsl:text>ed</xsl:text>
	     <xsl:if test="preceding-sibling::tei:editor[@role='editor']">s</xsl:if>
	     <xsl:text>.</xsl:text>
	     <xsl:text>) </xsl:text>
	   </xsl:otherwise>
	 </xsl:choose>
       </xsl:when>
       <xsl:when test="self::tei:editor[@role='editor'] and following-sibling::tei:editor[@role='editor']">
	 <xsl:choose>
	   <xsl:when test="ancestor::tei:biblStruct and not(preceding-sibling::tei:editor[@role='editor'])">
	     <xsl:text>Ed. </xsl:text>
	     <xsl:choose>
	       <xsl:when test="contains(self::tei:editor[@role='editor'], ',')">						
		 <xsl:value-of select="substring-after(., ',')"/>
		 <xsl:text> </xsl:text>
		 <xsl:value-of select="substring-before(., ',')"/>
	       </xsl:when>
	       <xsl:otherwise>
		 <xsl:apply-templates/>
	       </xsl:otherwise>
	     </xsl:choose>
	     <xsl:choose>
	       <xsl:when test="position() + 1 = last()">
		 <xsl:text> </xsl:text>
	       </xsl:when>
	       <xsl:otherwise>
		 <xsl:text>, </xsl:text>
	       </xsl:otherwise>
	     </xsl:choose>
	   </xsl:when>
	   <xsl:when test="ancestor::tei:biblStruct and preceding-sibling::tei:editor[@role='editor']">
	     <xsl:choose>
	       <xsl:when test="contains(self::tei:editor[@role='editor'], ',')">						
		 <xsl:value-of select="substring-after(., ',')"/>
		 <xsl:text> </xsl:text>
		 <xsl:value-of select="substring-before(., ',')"/>
	       </xsl:when>
	       <xsl:otherwise>
		 <xsl:apply-templates/>
	       </xsl:otherwise>
	     </xsl:choose>
	     <xsl:text>, </xsl:text>
	   </xsl:when>
	   <xsl:otherwise>
	     <xsl:choose>
	       <xsl:when test="contains(self::tei:editor[@role='editor'], ',')">						
		 <xsl:value-of select="substring-after(., ',')"/>
		 <xsl:text> </xsl:text>
		 <xsl:value-of select="substring-before(., ',')"/>
	       </xsl:when>
	       <xsl:otherwise>
		 <xsl:apply-templates/>
	       </xsl:otherwise>
	     </xsl:choose>
	     <xsl:choose>
	       <xsl:when test="position() + 1 = last()">
		 <xsl:text> </xsl:text>
	       </xsl:when>
	       <xsl:otherwise>
		 <xsl:text>, </xsl:text>
	       </xsl:otherwise>
	     </xsl:choose>
	   </xsl:otherwise>
	 </xsl:choose>
       </xsl:when>
     </xsl:choose>
   </xsl:template>
   
   <xsl:template match="tei:nameLink">
</xsl:template>

   <!-- title  -->
   <xsl:template match="tei:title" mode="simple">
      <xsl:apply-templates select="text()"/>
   </xsl:template>

   <xsl:template match="tei:title">
      <xsl:choose>
         <xsl:when test="parent::tei:titleStmt/parent::tei:fileDesc">
            <xsl:if test="preceding-sibling::tei:title">
	      <xsl:call-template name="makeText">
		<xsl:with-param name="letters"> — </xsl:with-param>
	      </xsl:call-template>
            </xsl:if>
            <xsl:apply-templates/>
         </xsl:when>
         <xsl:when test="not(@level) and parent::tei:bibl">
	   <xsl:call-template name="makeInline">
	     <xsl:with-param name="style">
	       <xsl:value-of select="('titlem',@rend)" separator=" "/>
	     </xsl:with-param>
	   </xsl:call-template>
	 </xsl:when>
         <xsl:when test="@level='m' or not(@level)">
	   <xsl:call-template name="emphasize">
	     <xsl:with-param name="class">
	       <xsl:value-of select="('titlem',@rend)" separator=" "/>
	     </xsl:with-param>
	     <xsl:with-param name="content">
	       <xsl:apply-templates/>
	     </xsl:with-param>
	   </xsl:call-template>
	   <xsl:if test="ancestor::tei:biblStruct or ancestor::tei:biblFull">
	     <xsl:call-template name="makeText">
	       <xsl:with-param name="letters">, </xsl:with-param>
	     </xsl:call-template>
	   </xsl:if>
         </xsl:when>
         <xsl:when test="@level='s'">
	   <xsl:call-template name="emphasize">
	     <xsl:with-param name="class">
	       <xsl:value-of select="('titles',@rend)" separator=" "/>
	     </xsl:with-param>
	     <xsl:with-param name="content">
	       <xsl:apply-templates/>
	     </xsl:with-param>
	   </xsl:call-template>
	   <xsl:if test="following-sibling::* and
			 (ancestor::tei:biblStruct  or ancestor::tei:biblFull)">
	     <xsl:call-template name="makeText">
	       <xsl:with-param name="letters">
		 <xsl:text> </xsl:text>
	       </xsl:with-param>
	     </xsl:call-template>
	   </xsl:if>
         </xsl:when>
         <xsl:when test="@level='j'">
	   <xsl:call-template name="emphasize">
	     <xsl:with-param name="class">
	       <xsl:value-of select="('titlej',@rend)" separator=" "/>
	     </xsl:with-param>
	     <xsl:with-param name="content">
	       <xsl:apply-templates/>
	     </xsl:with-param>
	   </xsl:call-template>
	   <xsl:if test="ancestor::tei:biblStruct or ancestor::tei:biblFull">
	     <xsl:call-template name="makeText">
	     <xsl:with-param name="letters">
	     <xsl:text> </xsl:text>
	     </xsl:with-param>
	   </xsl:call-template>
	   </xsl:if>
         </xsl:when>
         <xsl:when test="@level='a'">
	   <xsl:call-template name="emphasize">
	     <xsl:with-param name="class">
	       <xsl:value-of select="('titlea',@rend)" separator=" "/>
	     </xsl:with-param>
	     <xsl:with-param name="content">
	       <xsl:apply-templates/>
	     </xsl:with-param>
	   </xsl:call-template>
	   <xsl:if test="ancestor::tei:biblStruct or ancestor::tei:biblFull">
	     <xsl:call-template name="makeText">
	       <xsl:with-param
		   name="letters">. </xsl:with-param>
	     </xsl:call-template>
	   </xsl:if>
         </xsl:when>
         <xsl:when test="@level='u'">
	   <xsl:call-template name="emphasize">
	     <xsl:with-param name="class">
	       <xsl:value-of select="('titleu',@rend)" separator=" "/>
	     </xsl:with-param>
	     <xsl:with-param name="content">
	       <xsl:apply-templates/>
	     </xsl:with-param>
	   </xsl:call-template>
	   <xsl:if test="ancestor::tei:biblStruct  or ancestor::tei:biblFull">
	     <xsl:call-template name="makeText">
	       <xsl:with-param
		   name="letters">. </xsl:with-param>
	     </xsl:call-template>
	   </xsl:if>
         </xsl:when>
<!--
         <xsl:when test="ancestor::tei:bibl">
	   <xsl:apply-templates/>
         </xsl:when>
-->
         <xsl:otherwise>
	   <xsl:call-template name="emphasize">
	     <xsl:with-param name="class">
	       <xsl:value-of select="('titlem',@rend)" separator=" "/>
	     </xsl:with-param>
	     <xsl:with-param name="content">
	       <xsl:apply-templates/>
	     </xsl:with-param>
	   </xsl:call-template>
         </xsl:otherwise>
      </xsl:choose>
   </xsl:template>
   

   <xsl:template match="tei:meeting">
      <xsl:call-template name="makeText">
	<xsl:with-param name="letters"> (</xsl:with-param>
      </xsl:call-template>
      <xsl:apply-templates/>
      <xsl:call-template name="makeText">
	<xsl:with-param name="letters">)</xsl:with-param>
      </xsl:call-template>
      <xsl:if test="following-sibling::* and (ancestor::tei:biblStruct  or ancestor::tei:biblFull)">
         <xsl:call-template name="makeText">
	   <xsl:with-param name="letters"><xsl:text> </xsl:text></xsl:with-param>
	 </xsl:call-template>
      </xsl:if>
   </xsl:template>

   <xsl:template match="tei:series">
      <xsl:apply-templates/>
   </xsl:template>

   <xsl:template match="tei:biblStruct//tei:date|tei:biblFull//tei:date">
     <!--
	 <xsl:choose>
	 <xsl:when test="starts-with(.,'$Date:')">
	 <xsl:value-of select="substring-before(substring-after(.,'$Date:'),'$')"/>
	 </xsl:when>
	 <xsl:otherwise>
	 <xsl:apply-templates/>
	 </xsl:otherwise>
	 </xsl:choose>
     -->
      <xsl:apply-templates/>
     <xsl:call-template name="makeText">
       <xsl:with-param name="letters">. </xsl:with-param>
     </xsl:call-template>
   </xsl:template>

   <xsl:template match="tei:byline">
     <xsl:call-template name="makeSpan"/>
   </xsl:template>

   <xsl:template match="tei:pubPlace">
     <xsl:call-template name="makeSpan"/>
     <xsl:choose>
         <xsl:when test="ancestor::tei:bibl"/>
         <xsl:when test="following-sibling::tei:pubPlace">
            <xsl:call-template name="makeText">
	      <xsl:with-param name="letters">, </xsl:with-param>
	    </xsl:call-template>
         </xsl:when>
         <xsl:when test="../tei:publisher">
            <xsl:call-template name="makeText">
	      <xsl:with-param name="letters">: </xsl:with-param>
	    </xsl:call-template>
         </xsl:when>
         <xsl:otherwise>
            <xsl:call-template name="makeText">
	      <xsl:with-param name="letters">. </xsl:with-param>
	    </xsl:call-template>
         </xsl:otherwise>
      </xsl:choose>
   </xsl:template>

   <xsl:template match="tei:publisher">
     <xsl:call-template name="makeSpan"/>
      <xsl:if test="ancestor::tei:biblStruct or ancestor::tei:biblFull">
         <xsl:call-template name="makeText">
	   <xsl:with-param name="letters">. </xsl:with-param>
	 </xsl:call-template>
      </xsl:if>
   </xsl:template>

   <!-- details and notes -->
   <xsl:template match="tei:biblScope">
     <xsl:variable name="Unit" select="(@unit|@type)[1]"/>
      <xsl:choose>
         <xsl:when test="ancestor::tei:bibl">
            <xsl:apply-templates/>
         </xsl:when>
         <xsl:when test="$Unit='vol' or $Unit='volume'">
            <xsl:call-template name="emphasize">
               <xsl:with-param name="class">
	                 <xsl:text>vol</xsl:text>
               </xsl:with-param>
               <xsl:with-param name="content">
	                 <xsl:apply-templates/>
               </xsl:with-param>
            </xsl:call-template>
         </xsl:when>
         <xsl:when test="$Unit='chap' or $Unit='chapter'">
            <xsl:call-template name="makeText">
	      <xsl:with-param name="letters">chapter </xsl:with-param>
	    </xsl:call-template>
            <xsl:apply-templates/>
         </xsl:when>
         <xsl:when test="$Unit='issue' or $Unit='nr'">
            <xsl:call-template name="makeText">
	      <xsl:with-param name="letters"> (</xsl:with-param>
	    </xsl:call-template>
            <xsl:apply-templates/>
            <xsl:call-template name="makeText">
	      <xsl:with-param name="letters">) </xsl:with-param>
	    </xsl:call-template>
         </xsl:when>
         <xsl:when test="$Unit='page_from'">
	   <xsl:text>pp. </xsl:text>
	   <xsl:apply-templates/>
	 </xsl:when>
         <xsl:when test="$Unit='page_to'">
	   <xsl:text>-</xsl:text>
	   <xsl:apply-templates/>
	 </xsl:when>
         <xsl:when test="$Unit='pp' or $Unit='pages' or $Unit='page'">
            <xsl:choose>
               <xsl:when test="contains(.,'-')">
	                 <xsl:call-template name="makeText">
			   <xsl:with-param
			       name="letters">pp. </xsl:with-param>
			 </xsl:call-template>
               </xsl:when>
               <xsl:when test="contains(.,'ff')">
	                 <xsl:call-template name="makeText">
			   <xsl:with-param
			       name="letters">pp. </xsl:with-param>
			 </xsl:call-template>
               </xsl:when>
               <xsl:when test="contains(.,' ')">
	                 <xsl:call-template name="makeText">
			   <xsl:with-param
			       name="letters">pp. </xsl:with-param>
			 </xsl:call-template>
               </xsl:when>
               <xsl:otherwise>
	                 <xsl:call-template name="makeText">
			   <xsl:with-param
			       name="letters">p. </xsl:with-param>
			 </xsl:call-template>
               </xsl:otherwise>
            </xsl:choose>
            <xsl:apply-templates/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:apply-templates/>
         </xsl:otherwise>
      </xsl:choose>
 
      <xsl:choose>
         <xsl:when test="$Unit='vol' and      following-sibling::tei:biblScope[$Unit='issue']">
            <xsl:call-template name="makeText">
	      <xsl:with-param name="letters"><xsl:text> </xsl:text></xsl:with-param>
	    </xsl:call-template>
         </xsl:when>
         <xsl:when test="$Unit='vol' and following-sibling::tei:biblScope">
            <xsl:call-template name="makeText">
	      <xsl:with-param name="letters"><xsl:text> </xsl:text></xsl:with-param>
	    </xsl:call-template>
         </xsl:when>
         <xsl:when test="following-sibling::tei:biblScope">
            <xsl:call-template name="makeText">
	      <xsl:with-param name="letters"><xsl:text> </xsl:text></xsl:with-param>
	    </xsl:call-template>
         </xsl:when>
         <xsl:when test="ancestor::tei:biblStruct or ancestor::tei:biblFull">
            <xsl:call-template name="makeText">
	      <xsl:with-param name="letters">. </xsl:with-param>
	    </xsl:call-template>
         </xsl:when>
      </xsl:choose>

   </xsl:template>

  <xsl:template match="tei:name|tei:persName|tei:placeName|tei:orgName">
      <xsl:choose>
         <xsl:when test="ancestor::tei:person|ancestor::tei:biblStruct">
	   <xsl:call-template name="makeSpan"/>
	   <xsl:if test="following-sibling::*[1][self::tei:name|self::tei:persName]">
	     <xsl:call-template name="makeText">
	       <xsl:with-param name="letters">, </xsl:with-param>
	     </xsl:call-template>
	   </xsl:if>
	 </xsl:when>
	 <xsl:otherwise>
	   <xsl:call-template name="makeInline">
	     <xsl:with-param name="style" select="local-name()"/>
	   </xsl:call-template>
	 </xsl:otherwise>
      </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[html] How to identify a note</desc>
  </doc>
  <xsl:template name="noteID">
    <xsl:choose>
      <xsl:when test="@xml:id">
        <xsl:value-of select="@xml:id"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>Note</xsl:text>
        <xsl:number count="tei:note" level="any"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[html] How to label a note</desc>
  </doc>
  <xsl:template name="noteN">
    <xsl:choose>
      <xsl:when test="@n">
        <xsl:value-of select="@n"/>
      </xsl:when>
      <xsl:when test="not(@place) and $consecutiveFNs='true'">
        <xsl:number count="tei:note[not(@place)]" level="any"/>
      </xsl:when>
      <xsl:when test="not(@place)">
        <xsl:choose>
	  <xsl:when test="ancestor::tei:floatingText">
	    <xsl:number count="tei:note[not(@place)]" from="tei:floatingText" level="any"/>
	  </xsl:when>
          <xsl:when test="ancestor::tei:front">
            <xsl:number count="tei:note[not(ancestor::tei:floatingText) and not(@place)]" from="tei:front[not(ancestor::tei:floatingText)]" level="any"/>
          </xsl:when>
          <xsl:when test="ancestor::tei:back">
            <xsl:number count="tei:note[not(ancestor::tei:floatingText) and not(@place)]" from="tei:back[not(ancestor::tei:floatingText)]" level="any"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:number count="tei:note[not(ancestor::tei:floatingText) and not(@place)]" from="tei:body[not(ancestor::tei:floatingText)]" level="any"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:when test="tei:isEndNote(.)">
        <xsl:choose>
          <xsl:when test="$consecutiveFNs = 'true'">
            <xsl:number count="tei:note[tei:isEndNote(.)]" level="any"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:choose>
	      <xsl:when test="ancestor::tei:floatingText">
                <xsl:number count="tei:note[tei:isEndNote(.)]" from="tei:floatingText" level="any"/>
	      </xsl:when>
              <xsl:when test="ancestor::tei:front">
                <xsl:number count="tei:note[not(ancestor::tei:floatingText) and tei:isEndNote(.)]" from="tei:front[not(ancestor::tei:floatingText)]" level="any"/>
              </xsl:when>
              <xsl:when test="ancestor::tei:back">
                <xsl:number count="tei:note[not(ancestor::tei:floatingText) and tei:isEndNote(.)]" from="tei:back[not(ancestor::tei:floatingText)]" level="any"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:number count="tei:note[not(ancestor::tei:floatingText) and tei:isEndNote(.)]" from="tei:body[not(ancestor::tei:floatingText)]" level="any"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <xsl:choose>
          <xsl:when test="$consecutiveFNs = 'true'">
            <xsl:number count="tei:note[tei:isFootNote(.)]" level="any"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:choose>
	      <xsl:when test="ancestor::tei:floatingText">
                <xsl:number count="tei:note[tei:isFootNote(.)]" from="tei:floatingText" level="any"/>
	      </xsl:when>
              <xsl:when test="ancestor::tei:front">
                <xsl:number count="tei:note[not(ancestor::tei:floatingText) and tei:isFootNote(.)]" from="tei:front[not(ancestor::tei:floatingText)]" level="any"/>
              </xsl:when>
              <xsl:when test="ancestor::tei:back">
                <xsl:number count="tei:note[not(ancestor::tei:floatingText) and tei:isFootNote(.)]" from="tei:back[not(ancestor::tei:floatingText)]" level="any"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:number count="tei:note[not(ancestor::tei:floatingText) and tei:isFootNote(.)]" from="tei:body[not(ancestor::tei:floatingText)]" level="any"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>  

  <xsl:template match="tei:note">
    <xsl:choose>
      <xsl:when test="@place='none'"/>

      <xsl:when test="not(@place) and (parent::tei:head or ancestor::tei:bibl or
		      ancestor::tei:biblFull or ancestor::tei:biblStruct)">
	<xsl:call-template name="makeText">
	  <xsl:with-param name="letters"> (</xsl:with-param>
	</xsl:call-template>
	<xsl:apply-templates/>
	<xsl:call-template name="makeText">
	  <xsl:with-param name="letters">)</xsl:with-param>
	</xsl:call-template>
      </xsl:when>

      <xsl:when test="tokenize(@place,' ')=('sup','above')">
	<xsl:call-template name="makeInline">
	  <xsl:with-param name="style">sup</xsl:with-param>
	</xsl:call-template>
      </xsl:when>

      <xsl:when test="tokenize(@place,' ')=('sub','below')">
	<xsl:call-template name="makeInline">
	  <xsl:with-param name="style">sub</xsl:with-param>
	</xsl:call-template>
      </xsl:when>

      <xsl:when test="@place='comment'">
	<xsl:call-template name="commentNote"/>
      </xsl:when>

      <xsl:when test="@place='inline' and not(tei:isInline(.))">
	<xsl:call-template name="displayNote"/>
      </xsl:when>
      
      <xsl:when test="@place='inline'">
	<xsl:call-template name="plainNote"/>
      </xsl:when>

      <xsl:when test="tei:isEndNote(.) or $autoEndNotes='true'">
	<xsl:call-template name="endNote"/>
      </xsl:when>

      <xsl:when test="tei:isFootNote(.)">
	<xsl:call-template name="footNote"/>
      </xsl:when>

      <xsl:when test="tei:isMarginal(@place)">
	<xsl:call-template name="marginalNote"/>
      </xsl:when>

      <xsl:when test="not(tei:isInline(.)) or tei:q">
	<xsl:call-template name="displayNote"/>
      </xsl:when>

      <xsl:when test="@place">
	<xsl:message>WARNING: unknown @place for note, <xsl:value-of select="@place"/></xsl:message>
	<xsl:call-template name="displayNote"/>
      </xsl:when>

      <xsl:otherwise>
	<xsl:call-template name="plainNote"/>
      </xsl:otherwise>
      </xsl:choose>
  </xsl:template>

  <xsl:template name="marginalNote">
    <xsl:call-template name="plainNote"/>
  </xsl:template>

  <xsl:template name="endNote">
    <xsl:call-template name="plainNote"/>
  </xsl:template>

  <xsl:template name="footNote">
    <xsl:call-template name="plainNote"/>
  </xsl:template>

  <xsl:template name="commentNote">
    <xsl:call-template name="plainNote"/>
  </xsl:template>

  <xsl:template name="displayNote">
    <xsl:call-template name="plainNote"/>
  </xsl:template>

  <xsl:template name="plainNote">
    <xsl:text> [</xsl:text>
    <xsl:choose>
      <xsl:when test="@n">
	<xsl:value-of select="@n"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:sequence select="tei:i18n('Note')"/>
	<xsl:text>: </xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:apply-templates/>
    <xsl:text>] </xsl:text>
  </xsl:template>

  <xsl:template match="tei:bibl">
    <xsl:choose>
      <xsl:when test="parent::tei:cit[tei:match(@rend,'display')] or
		      (parent::tei:cit and tei:p) or  parent::tei:q[tei:isInline(.)]">
        <xsl:call-template name="makeInline">
	  <xsl:with-param name="style">citbibl</xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="parent::tei:q/parent::tei:head or parent::tei:q[tei:match(@rend,'inline')]">
        <xsl:call-template name="makeBlock">
	  <xsl:with-param name="style">citbibl</xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="not(tei:isInline(.))">
        <xsl:call-template name="makeBlock">
	  <xsl:with-param name="style">biblfree</xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="makeInline">
	  <xsl:with-param name="style">
            <xsl:text>bibl</xsl:text>
	  </xsl:with-param>
	  <xsl:with-param name="before">
	    <xsl:if test="parent::tei:cit">
	      <xsl:text> (</xsl:text>
	    </xsl:if>
	  </xsl:with-param>
	  <xsl:with-param name="after">
	    <xsl:if test="parent::tei:cit">
	      <xsl:text>)</xsl:text>
	    </xsl:if>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

<xsl:template match="tite:b" mode="tite">
  <hi xmlns="http://www.tei-c.org/ns/1.0"  rend="bold">
    <xsl:apply-templates 
	select="@*|*|text()|comment()|processing-instruction" mode="tite"/>  
  </hi>
</xsl:template>

<xsl:template match="tite:i" mode="tite">
  <hi xmlns="http://www.tei-c.org/ns/1.0"  rend="italic">
    <xsl:apply-templates 
	select="@*|*|text()|comment()|processing-instruction" mode="tite"/>  
  </hi>
</xsl:template>

<xsl:template match="tite:ul" mode="tite">
  <hi xmlns="http://www.tei-c.org/ns/1.0"  rend="underline">
    <xsl:apply-templates 
	select="@*|*|text()|comment()|processing-instruction" mode="tite"/>  
  </hi>
</xsl:template>

<xsl:template match="tite:sup" mode="tite">
  <hi xmlns="http://www.tei-c.org/ns/1.0"  rend="sup">
    <xsl:apply-templates 
	select="@*|*|text()|comment()|processing-instruction" mode="tite"/>  
  </hi>
</xsl:template>

<xsl:template match="tite:sub" mode="tite">
  <hi xmlns="http://www.tei-c.org/ns/1.0"  rend="sub">
    <xsl:apply-templates 
	select="@*|*|text()|comment()|processing-instruction" mode="tite"/>  
  </hi>
</xsl:template>

<xsl:template match="tite:smcap" mode="tite">
  <hi xmlns="http://www.tei-c.org/ns/1.0"  rend="smcap">
    <xsl:apply-templates 
	select="@*|*|text()|comment()|processing-instruction" mode="tite"/>  
  </hi>
</xsl:template>

  <xsl:template match="*"  mode="tite">
    <xsl:copy>
      <xsl:apply-templates select="@*|*|processing-instruction()|comment()|text()"  mode="tite"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="comment()|@*|processing-instruction()|text()" mode="tite">
    <xsl:copy-of select="."/>
  </xsl:template>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element head in plain mode</desc>
  </doc>
  <xsl:template match="tei:head" mode="plain">
    <xsl:if test="preceding-sibling::tei:head">
      <xsl:text> </xsl:text>
    </xsl:if>
    <xsl:apply-templates mode="plain"/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process all elements in plain mode</desc>
   </doc>
  <xsl:template match="tei:*" mode="plain">
      <xsl:apply-templates mode="plain"/>
  </xsl:template>
  <xsl:template match="tei:note" mode="plain"/>
  <xsl:template match="tei:app" mode="plain"/>
  <xsl:template match="tei:pb" mode="plain"/>
  <xsl:template match="tei:lb" mode="plain">
    <xsl:choose>
      <xsl:when test="@type='hyphenInWord'"/>
      <xsl:otherwise>
	<xsl:text> </xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:figure" mode="plain">
    <xsl:text>[</xsl:text>
    <xsl:sequence select="tei:i18n('figureWord')"/>
    <xsl:text>]</xsl:text>
  </xsl:template>
  <xsl:template match="tei:figDesc" mode="plain"/>
  <xsl:template match="tei:ptr" mode="plain"/>

  <xsl:template name="makeItem">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template name="makeLabelItem">
    <xsl:apply-templates/>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process linebreaks</desc>
   </doc>

  <xsl:template match="tei:lb">
    <xsl:choose>
      <xsl:when test="@type='hyphenInWord' and tei:match(@rend,'hidden')"/>
      <xsl:when test="tei:match(@rend,'hidden')">
        <xsl:text> </xsl:text>
      </xsl:when>
      <xsl:when test="tei:match(@rend,'-') or @type='hyphenInWord'">
        <xsl:text>-</xsl:text>
	<xsl:call-template name="lineBreak"/>
      </xsl:when>
      <xsl:when test="tei:match(@rend,'above')">
        <xsl:text>⌜</xsl:text>
      </xsl:when>
      <xsl:when test="tei:match(@rend,'below')">
        <xsl:text>⌞</xsl:text>
      </xsl:when>
      <xsl:when test="not(tei:isInline(..)) and (tei:isLast(.) or
		      tei:isFirst(.))"/>
      <xsl:when test="tei:match(@rend,'show')">
	<xsl:call-template name="lineBreak"/>
      </xsl:when>
      <xsl:when test="tei:match(@rend,'paragraph')">
	<xsl:call-template name="lineBreakAsPara"/>
      </xsl:when>
      <xsl:when test="not(tei:isInline(..)) and (tei:isLast(.) or tei:isFirst(.))"/>
      <xsl:otherwise>
	<xsl:call-template name="lineBreak"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="lineBreak">
    <xsl:text> </xsl:text>
  </xsl:template>
  <xsl:template name="lineBreakAsPara">
    <xsl:text> </xsl:text>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process forename</desc>
   </doc>
  <xsl:template match="tei:forename">
    <xsl:choose>
      <xsl:when test="parent::*/tei:surname"/>
      <xsl:otherwise>
	<xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process surname</desc>
   </doc>
  <xsl:template match="tei:surname">
    <xsl:if test="parent::*/tei:forename">
      <xsl:for-each select="parent::*/tei:forename">
	<xsl:apply-templates/>
	<xsl:text> </xsl:text>
      </xsl:for-each>
    </xsl:if>
    <xsl:apply-templates/>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>abbreviation marker and expansion inside expan</desc>
   </doc>
  <xsl:template match="tei:expan/tei:am"/>
  <xsl:template match="tei:expan/tei:ex">
    <xsl:call-template name="makeInline">
      <xsl:with-param name="before">(</xsl:with-param>
      <xsl:with-param name="after">)</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="tei:gap" priority="10">
    <xsl:call-template name="makeInline">
      <xsl:with-param name="before">[...]</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="tei:unclear">
    <xsl:call-template name="makeInline">
      <xsl:with-param name="style">unclear</xsl:with-param>
      <xsl:with-param name="after">[?]</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="tei:geogName|tei:roleName">
    <xsl:choose>
      <xsl:when test="*">
        <xsl:apply-templates/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="makeInline"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>mentioned element</desc>
   </doc>
  <xsl:template match="tei:mentioned">
    <xsl:call-template name="makeInline">
      <xsl:with-param name="style">mentioned</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>foreign element</desc>
   </doc>
  <xsl:template match="tei:foreign">
    <xsl:call-template name="makeInline">
      <xsl:with-param name="style">foreign</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>term element</desc>
   </doc>
  <xsl:template match="tei:term">
    <xsl:call-template name="makeInline">
      <xsl:with-param name="style">term</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>The del element</desc>
  </doc>
  <xsl:template match="tei:del">
    <xsl:call-template name="makeInline">
      <xsl:with-param name="style">strikethrough</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>The add element</desc>
  </doc>
  <xsl:template match="tei:add">
    <xsl:choose>
      <xsl:when test="@place='sup' or @place='above'">
	<xsl:call-template name="makeInline">
	  <xsl:with-param name="style">sup</xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="@place='sub' or @place='below'">
	<xsl:call-template name="makeInline">
	  <xsl:with-param name="style">sub</xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="not(tei:isInline(*[last()]))">
	<xsl:call-template name="makeBlock">
	  <xsl:with-param name="style">add</xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="makeInline">
	  <xsl:with-param name="style">add</xsl:with-param>
	  <xsl:with-param name="after">&#10217;</xsl:with-param>
	  <xsl:with-param name="before">&#10216;</xsl:with-param>
	</xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>Process element label</desc>
  </doc>
  <xsl:template match="tei:label">
    <xsl:call-template name="makeInline">
      <xsl:with-param name="style" select="@type"/>
    </xsl:call-template>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>processing milestone elements</desc>
   </doc>
  <xsl:template match="tei:milestone">
    <xsl:choose>
      <xsl:when test="tei:match(@rend,'hr')">
	<xsl:call-template name="horizontalRule"/>
      </xsl:when>
      <xsl:when test="@unit='line'">
	<xsl:text> </xsl:text>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="makeInline">
	  <xsl:with-param name="style" select="@type"/>
	  <xsl:with-param name="before">
	    <xsl:if test="not(@unit='unspecified')">
              <xsl:value-of select="@unit"/>
              <xsl:text> </xsl:text>
	    </xsl:if>
	  </xsl:with-param>
	  <xsl:with-param name="after" select="@n"/>
	</xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element docAuthor in "author" mode"</desc>
   </doc>
  <xsl:template match="tei:docAuthor" mode="author">
    <xsl:apply-templates/>
    <xsl:choose>
      <xsl:when test="count(following-sibling::tei:docAuthor)=1">
	<xsl:if test="count(preceding-sibling::tei:docAuthor)&gt;1">
	  <xsl:text>,</xsl:text>
	</xsl:if>
	<xsl:text> </xsl:text>
	<xsl:sequence select="tei:i18n('and')"/>
	<xsl:text> </xsl:text>
      </xsl:when>
      <xsl:when test="following-sibling::tei:docAuthor">
	<xsl:text>, </xsl:text>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element author in "author" mode"</desc>
   </doc>
  <xsl:template match="tei:author" mode="author">
    <xsl:apply-templates select="*[not(self::tei:email or self::tei:affiliation)]|text()"/>
    <xsl:choose>
      <xsl:when test="count(following-sibling::tei:author)=1">
	<xsl:if test="count(preceding-sibling::tei:author)&gt;1">
	  <xsl:text>,</xsl:text>
	</xsl:if>
	<xsl:text> </xsl:text>
	<xsl:sequence select="tei:i18n('and')"/>
	<xsl:text> </xsl:text>
      </xsl:when>
      <xsl:when test="following-sibling::tei:author">, </xsl:when>
    </xsl:choose>
  </xsl:template>
</xsl:stylesheet>
