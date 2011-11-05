<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:tei="http://www.tei-c.org/ns/1.0"
                
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="tei"
                version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet dealing with elements from the core module. </p>
         <p>This software is dual-licensed:

1. Distributed under a Creative Commons Attribution-ShareAlike 3.0
Unported License http://creativecommons.org/licenses/by-sa/3.0/ 

2. http://www.opensource.org/licenses/BSD-2-Clause
		
All rights reserved.

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
         <p>Id: $Id$</p>
         <p>Copyright: 2011, TEI Consortium</p>
      </desc>
   </doc>
  <xsl:output indent="no"/>
    <xsl:strip-space elements="tei:author tei:forename tei:surname tei:editor"/>

  <xsl:key name="MNAMES"
            match="tei:monogr/tei:author[tei:surname]|tei:monogr/tei:editor[tei:surname]"
            use="ancestor::tei:biblStruct/@xml:id"/>
  <xsl:key name="ANAMES"
            match="tei:analytic/tei:author[tei:surname]|tei:analytic/tei:editor[tei:surname]"
            use="ancestor::tei:biblStruct/@xml:id"/>
  

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process all elements to find out their nesting depth</desc>
   </doc>
  <xsl:template match="tei:*" mode="depth">99</xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process all elements in plain mode</desc>
   </doc>
  <xsl:template match="tei:*" mode="plain">
      <xsl:apply-templates mode="plain"/>
  </xsl:template>
  <xsl:template match="tei:note" mode="plain"/>
  <xsl:template match="tei:app" mode="plain"/>
  <xsl:template match="tei:pb" mode="plain"/>
  <xsl:template match="tei:lb" mode="plain"/>
  <xsl:template match="tei:figure" mode="plain"/>
  <xsl:template match="tei:figDesc" mode="plain"/>
  <xsl:template match="tei:ptr" mode="plain"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process tei:sic</desc>
   </doc>
  <xsl:template match="tei:sic">
      <xsl:apply-templates/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process tei:corr</desc>
   </doc>
  <xsl:template match="tei:corr"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process tei:item in runin mode</desc>
   </doc>
  <xsl:template match="tei:item" mode="runin">
      <xsl:text> • </xsl:text>
      <xsl:apply-templates/>
      <xsl:text>&#160;</xsl:text>
  </xsl:template>



  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element edition</desc>
   </doc>
  <xsl:template match="tei:edition">
      <xsl:apply-templates/>
      <xsl:if test="ancestor::tei:biblStruct or ancestor::tei:biblFull">
         <xsl:call-template name="tei:makeText">
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


   <!-- biblStruct -->
<xsl:template match="tei:biblStruct" mode="xref">
      <xsl:choose>
         <xsl:when test="count(key('ANAMES',@xml:id))=1">
	           <xsl:value-of select="key('ANAMES',@xml:id)/tei:surname"/>
         </xsl:when>
         <xsl:when test="count(key('ANAMES',@xml:id))=2">
	           <xsl:value-of select="key('ANAMES',@xml:id)[1]/tei:surname"/>
	           <xsl:call-template name="tei:makeText">
		     <xsl:with-param name="letters"> and </xsl:with-param>
		   </xsl:call-template>
	           <xsl:value-of select="key('ANAMES',@xml:id)[2]/tei:surname"/>
         </xsl:when>
         <xsl:when test="count(key('ANAMES',@xml:id))&gt;2">
	           <xsl:value-of select="key('ANAMES',@xml:id)[1]/tei:surname"/>
	           <xsl:call-template name="tei:makeText">
		     <xsl:with-param name="letters"> et al.</xsl:with-param>
		   </xsl:call-template>
         </xsl:when>
         <xsl:when test="count(key('MNAMES',@xml:id))=1">
	           <xsl:value-of select="key('MNAMES',@xml:id)/tei:surname"/>
         </xsl:when>
         <xsl:when test="count(key('MNAMES',@xml:id))=2">
	           <xsl:value-of select="key('MNAMES',@xml:id)[1]/tei:surname"/>
	           <xsl:call-template name="tei:makeText">
		     <xsl:with-param name="letters"> and </xsl:with-param>
		   </xsl:call-template>
	           <xsl:value-of select="key('MNAMES',@xml:id)[2]/tei:surname"/>
         </xsl:when>
         <xsl:when test="count(key('MNAMES',@xml:id))&gt;2">
	           <xsl:value-of select="key('MNAMES',@xml:id)[1]/tei:surname"/>
	           <xsl:call-template name="tei:makeText">
		     <xsl:with-param name="letters"> et al.</xsl:with-param>
		   </xsl:call-template>
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
	           <xsl:call-template name="tei:makeText">
		     <xsl:with-param name="letters"> (ed.)</xsl:with-param>
		   </xsl:call-template>
         </xsl:when>
         <xsl:when test="count(tei:*[1]/tei:editor)&gt;1">
	           <xsl:call-template name="tei:makeText">
		     <xsl:with-param name="letters"> (eds.)</xsl:with-param>
		   </xsl:call-template>
         </xsl:when>
      </xsl:choose>
      <xsl:choose>
         <xsl:when test="tei:monogr/tei:imprint/tei:date/@when">
	           <xsl:call-template name="tei:makeText">
		     <xsl:with-param name="letters"> (</xsl:with-param>
		   </xsl:call-template>
	           <xsl:value-of select="substring-before(tei:monogr/tei:imprint/tei:date/@when,'-')"/>
	           <xsl:call-template name="tei:makeText">
		     <xsl:with-param name="letters">)</xsl:with-param>
		   </xsl:call-template>
         </xsl:when>
         <xsl:when test="tei:monogr/tei:imprint/tei:date">
	           <xsl:call-template name="tei:makeText">
		     <xsl:with-param name="letters"> (</xsl:with-param>
		   </xsl:call-template>
	           <xsl:value-of select="tei:monogr/tei:imprint/tei:date"/>
	           <xsl:call-template name="tei:makeText">
		     <xsl:with-param name="letters">)</xsl:with-param>
		   </xsl:call-template>
         </xsl:when>
      </xsl:choose>
   </xsl:template>

   <!-- authors and editors -->
<xsl:template match="tei:editor|tei:author">
  <xsl:choose>
    <xsl:when test="ancestor::tei:bibl">
      <xsl:apply-templates/>
    </xsl:when>
    <xsl:when test="self::tei:author and not(following-sibling::tei:author)">
      <xsl:apply-templates/>
      <xsl:call-template name="tei:makeText">
	<xsl:with-param name="letters">. </xsl:with-param>
      </xsl:call-template>
    </xsl:when>
    <xsl:when test="self::tei:editor and not(following-sibling::tei:editor)">
      <xsl:apply-templates/>
      <xsl:call-template name="tei:makeText">
	<xsl:with-param name="letters"> (ed</xsl:with-param>
      </xsl:call-template>
      <xsl:if test="preceding-sibling::tei:editor">s</xsl:if>
      <xsl:call-template name="tei:makeText">
	<xsl:with-param name="letters">.) </xsl:with-param>
      </xsl:call-template>
    </xsl:when>
    <xsl:otherwise>
      <xsl:apply-templates/>
      <xsl:call-template name="tei:makeText">
	<xsl:with-param name="letters">, </xsl:with-param>
      </xsl:call-template>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

   <xsl:template match="tei:surname">
      <xsl:if test="../tei:forename">
         <xsl:apply-templates select="../tei:forename" mode="use"/>
         <xsl:call-template name="tei:makeText">
	   <xsl:with-param name="letters"><xsl:text> </xsl:text></xsl:with-param>
	 </xsl:call-template>
      </xsl:if>
      <xsl:if test="../tei:nameLink">
         <xsl:apply-templates select="../tei:nameLink" mode="use"/>
         <xsl:call-template name="tei:makeText">
	   <xsl:with-param name="letters"><xsl:text> </xsl:text></xsl:with-param>
	 </xsl:call-template>
      </xsl:if>
      <xsl:apply-templates/>
   </xsl:template>

   <xsl:template match="tei:forename">
   </xsl:template>

   <xsl:template match="tei:nameLink">
</xsl:template>

   <xsl:template match="tei:forename" mode="use">
      <xsl:if test="preceding-sibling::tei:forename">
         <xsl:call-template name="tei:makeText">
	   <xsl:with-param name="letters"><xsl:text> </xsl:text></xsl:with-param>
	 </xsl:call-template>
      </xsl:if>
      <xsl:apply-templates/>
   </xsl:template>

   <xsl:template match="tei:nameLink" mode="use">
      <xsl:apply-templates/>
   </xsl:template>

   <!-- title  -->
   <xsl:template match="tei:titlePart" mode="simple">
      <xsl:if test="preceding-sibling::tei:titlePart">
         <xsl:call-template name="tei:makeText">
	   <xsl:with-param name="letters"> — </xsl:with-param>
	 </xsl:call-template>
      </xsl:if>
      <xsl:value-of select="."/>
   </xsl:template>

   <xsl:template match="tei:title" mode="simple">
      <xsl:value-of select="."/>
   </xsl:template>

   <xsl:template match="tei:titlePart">
      <xsl:if test="preceding-sibling::tei:titlePart">
         <xsl:call-template name="tei:makeText">
	   <xsl:with-param name="letters"> — </xsl:with-param>
	 </xsl:call-template>
      </xsl:if>
      <xsl:apply-templates/>
   </xsl:template>

   <xsl:template match="tei:title">
      <xsl:choose>
         <xsl:when test="parent::tei:titleStmt/parent::tei:fileDesc">
            <xsl:if test="preceding-sibling::tei:title">
	      <xsl:call-template name="tei:makeText">
		<xsl:with-param name="letters"> — </xsl:with-param>
	      </xsl:call-template>
            </xsl:if>
            <xsl:apply-templates/>
         </xsl:when>
         <xsl:when test="@level='m' or not(@level)">
	   <xsl:call-template name="emphasize">
	     <xsl:with-param name="class">
	       <xsl:text>titlem</xsl:text>
	     </xsl:with-param>
	     <xsl:with-param name="content">
	       <xsl:apply-templates/>
	     </xsl:with-param>
	   </xsl:call-template>
	   <xsl:if test="ancestor::tei:biblStruct or ancestor::tei:biblFull">
	     <xsl:call-template name="tei:makeText">
	       <xsl:with-param name="letters">, </xsl:with-param>
	     </xsl:call-template>
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
	   <xsl:if test="following-sibling::* and
			 (ancestor::tei:biblStruct  or ancestor::tei:biblFull)">
	     <xsl:call-template name="tei:makeText">
	       <xsl:with-param name="letters">
	       </xsl:with-param>
	     </xsl:call-template>
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
	   <xsl:call-template name="tei:makeText">
	     <xsl:with-param name="letters"><xsl:text> </xsl:text></xsl:with-param>
	   </xsl:call-template>
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
	   <xsl:if test="ancestor::tei:biblStruct or ancestor::tei:biblFull">
	     <xsl:call-template name="tei:makeText">
	       <xsl:with-param
		   name="letters">. </xsl:with-param>
	     </xsl:call-template>
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
	   <xsl:if test="ancestor::tei:biblStruct  or ancestor::tei:biblFull">
	     <xsl:call-template name="tei:makeText">
	       <xsl:with-param
		   name="letters">. </xsl:with-param>
	     </xsl:call-template>
	   </xsl:if>
         </xsl:when>
         <xsl:when test="ancestor::tei:bibl">
	   <xsl:apply-templates/>
         </xsl:when>
         <xsl:otherwise>
	   <xsl:call-template name="emphasize">
	     <xsl:with-param name="class">
	       <xsl:text>titlem</xsl:text>
	     </xsl:with-param>
	     <xsl:with-param name="content">
	       <xsl:apply-templates/>
	     </xsl:with-param>
	   </xsl:call-template>
         </xsl:otherwise>
      </xsl:choose>
   </xsl:template>
   

   <xsl:template match="tei:meeting">
      <xsl:call-template name="tei:makeText">
	<xsl:with-param name="letters"> (</xsl:with-param>
      </xsl:call-template>
      <xsl:apply-templates/>
      <xsl:call-template name="tei:makeText">
	<xsl:with-param name="letters">)</xsl:with-param>
      </xsl:call-template>
      <xsl:if test="following-sibling::* and (ancestor::tei:biblStruct  or ancestor::tei:biblFull)">
         <xsl:call-template name="tei:makeText">
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
     <xsl:call-template name="tei:makeText">
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
            <xsl:call-template name="tei:makeText">
	      <xsl:with-param name="letters">, </xsl:with-param>
	    </xsl:call-template>
         </xsl:when>
         <xsl:when test="../tei:publisher">
            <xsl:call-template name="tei:makeText">
	      <xsl:with-param name="letters">: </xsl:with-param>
	    </xsl:call-template>
         </xsl:when>
         <xsl:otherwise>
            <xsl:call-template name="tei:makeText">
	      <xsl:with-param name="letters">. </xsl:with-param>
	    </xsl:call-template>
         </xsl:otherwise>
      </xsl:choose>
   </xsl:template>

   <xsl:template match="tei:publisher">
     <xsl:call-template name="makeSpan"/>
      <xsl:if test="ancestor::tei:biblStruct or ancestor::tei:biblFull">
         <xsl:call-template name="tei:makeText">
	   <xsl:with-param name="letters">. </xsl:with-param>
	 </xsl:call-template>
      </xsl:if>
   </xsl:template>

   <!-- details and notes -->
   <xsl:template match="tei:biblScope">
      <xsl:choose>
         <xsl:when test="ancestor::tei:bibl">
            <xsl:apply-templates/>
         </xsl:when>
         <xsl:when test="@type='vol' or @type='volume'">
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
            <xsl:call-template name="tei:makeText">
	      <xsl:with-param name="letters">chapter </xsl:with-param>
	    </xsl:call-template>
            <xsl:apply-templates/>
         </xsl:when>
         <xsl:when test="@type='issue' or @type='nr'">
            <xsl:call-template name="tei:makeText">
	      <xsl:with-param name="letters"> (</xsl:with-param>
	    </xsl:call-template>
            <xsl:apply-templates/>
            <xsl:call-template name="tei:makeText">
	      <xsl:with-param name="letters">) </xsl:with-param>
	    </xsl:call-template>
         </xsl:when>
         <xsl:when test="@type='page_from'">
	   <xsl:text>pp. </xsl:text>
	   <xsl:apply-templates/>
	 </xsl:when>
         <xsl:when test="@type='page_to'">
	   <xsl:text>-</xsl:text>
	   <xsl:apply-templates/>
	 </xsl:when>
         <xsl:when test="@type='pp' or @type='pages'">
            <xsl:choose>
               <xsl:when test="contains(.,'-')">
	                 <xsl:call-template name="tei:makeText">
			   <xsl:with-param
			       name="letters">pp. </xsl:with-param>
			 </xsl:call-template>
               </xsl:when>
               <xsl:when test="contains(.,'ff')">
	                 <xsl:call-template name="tei:makeText">
			   <xsl:with-param
			       name="letters">pp. </xsl:with-param>
			 </xsl:call-template>
               </xsl:when>
               <xsl:when test="contains(.,' ')">
	                 <xsl:call-template name="tei:makeText">
			   <xsl:with-param
			       name="letters">pp. </xsl:with-param>
			 </xsl:call-template>
               </xsl:when>
               <xsl:otherwise>
	                 <xsl:call-template name="tei:makeText">
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
         <xsl:when test="@type='vol' and      following-sibling::tei:biblScope[@type='issue']">
            <xsl:call-template name="tei:makeText">
	      <xsl:with-param name="letters"><xsl:text> </xsl:text></xsl:with-param>
	    </xsl:call-template>
         </xsl:when>
         <xsl:when test="@type='vol' and following-sibling::tei:biblScope">
            <xsl:call-template name="tei:makeText">
	      <xsl:with-param name="letters"><xsl:text> </xsl:text></xsl:with-param>
	    </xsl:call-template>
         </xsl:when>
         <xsl:when test="following-sibling::tei:biblScope">
            <xsl:call-template name="tei:makeText">
	      <xsl:with-param name="letters"><xsl:text> </xsl:text></xsl:with-param>
	    </xsl:call-template>
         </xsl:when>
         <xsl:when test="ancestor::tei:biblStruct or ancestor::tei:biblFull">
            <xsl:call-template name="tei:makeText">
	      <xsl:with-param name="letters">. </xsl:with-param>
	    </xsl:call-template>
         </xsl:when>
      </xsl:choose>

   </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element name and tei:persName</desc>
   </doc>
  <xsl:template match="tei:name|tei:persName">
      <xsl:apply-templates/>
      <xsl:choose>
         <xsl:when test="not(ancestor::tei:person|ancestor::tei:biblStruct)"/>
         <xsl:when test="following-sibling::tei:name|following-sibling::tei:persName">
	   <xsl:call-template name="tei:makeText">
	     <xsl:with-param name="letters">, </xsl:with-param>
	   </xsl:call-template>
         </xsl:when>
      </xsl:choose>
  </xsl:template>

  <xsl:template match="tei:bibl/tei:note|tei:biblStruct/tei:note">
    <xsl:call-template name="tei:makeText">
      <xsl:with-param name="letters"> (</xsl:with-param>
    </xsl:call-template>
      <xsl:apply-templates/>
    <xsl:call-template name="tei:makeText">
      <xsl:with-param name="letters">)</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

</xsl:stylesheet>