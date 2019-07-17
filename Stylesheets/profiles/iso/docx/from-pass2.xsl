<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns="http://www.tei-c.org/ns/1.0"
                xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
                xmlns:cals="http://www.oasis-open.org/specs/tm9901"
                xmlns:iso="http://www.iso.org/ns/1.0"
                xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
                xmlns:mml="http://www.w3.org/1998/Math/MathML"
                xmlns:o="urn:schemas-microsoft-com:office:office"
                xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"
                xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
                xmlns:rel="http://schemas.openxmlformats.org/package/2006/relationships"
                xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
                xmlns:v="urn:schemas-microsoft-com:vml"
                xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
                xmlns:w10="urn:schemas-microsoft-com:office:word"
                xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
                xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
                xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
                xmlns:custprops="http://schemas.openxmlformats.org/officeDocument/2006/custom-properties"
                xmlns:vt="http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes"
                xmlns:html="http://www.w3.org/1999/xhtml"
                version="2.0"
                exclude-result-prefixes="a pic rel ve o teidocx r m v wp w10 w wne mml vt cals tbx iso custprops">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p> TEI stylesheet for simplifying TEI ODD markup </p>
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

    <!-- ******************************************************************************************* -->
    <!-- second stage processing -->

      <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
	<desc> Analyze numbers, marking them up to allow for decimal
	character changing </desc>
      </doc>
    <xsl:template match="text()" mode="pass2">
        <xsl:choose>
            <xsl:when test="parent::tei:num">
                <xsl:value-of select="."/>
            </xsl:when>
            <xsl:when test="parent::tei:seg[tei:match(@rend,'nonumber')]">
                <xsl:value-of select="."/>
            </xsl:when>
            <!-- do not search for numbers inside math -->
            <xsl:when test="ancestor-or-self::m:t">
                <xsl:value-of select="."/>
            </xsl:when>
            <xsl:when test="ancestor::mml:math">
                <xsl:value-of select="."/>
            </xsl:when>

            <xsl:when test="ancestor::tbx:termEntry">
                <xsl:value-of select="."/>
            </xsl:when>

            <xsl:otherwise>
                <xsl:analyze-string select="." regex="((\d+&#160;\d+)+,?\d*|\d+,\d+)">
                    <xsl:matching-substring>
                        <num>
                            <xsl:value-of select="regex-group(1)"/>
                        </num>
                    </xsl:matching-substring>
                    <xsl:non-matching-substring>
                        <xsl:value-of select="."/>
                    </xsl:non-matching-substring>
                </xsl:analyze-string>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Assign a unique ID to each<gi>bibl</gi></desc>
    </doc>
    <xsl:template match="tei:bibl" mode="pass2">
      <xsl:copy>
	<xsl:if test="not(@xml:id and not(w:bookmarkStart))">
	  <xsl:attribute name="xml:id">
	    <xsl:text>BIB_</xsl:text>
	    <xsl:number level="any" count="tei:bibl" />
	  </xsl:attribute>
	</xsl:if>
	<xsl:attribute name="n" select="tei:idno[@type='orgref']" />
	<xsl:attribute name="type">
	  <xsl:choose>
	    <xsl:when test="tei:edition">dated</xsl:when>
	    <xsl:otherwise>undated</xsl:otherwise>
	  </xsl:choose>
	</xsl:attribute>
	<xsl:apply-templates select="@*|*|processing-instruction()|comment()|text()" mode="pass2" />
      </xsl:copy>
    </xsl:template>
    
   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
   <desc>Remove unwanted elements from bibl</desc></doc>
    <xsl:template match="tei:bibl/tei:idno[@type='orgref']" mode="pass2">
    </xsl:template>

      <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>Look at the sections we have generated, and put
        them in &lt;front&gt; or &lt;body&gt; as appropriate</desc></doc>
	<xsl:template match="tei:text" mode="pass2">
	  <xsl:variable name="Doctext">
	    <text>
	      <xsl:for-each select="tei:fw">
                <xsl:copy-of select="."/>
	      </xsl:for-each>
	      <front>
                <xsl:apply-templates select="tei:body/tei:div[@type='toc']" mode="pass2"/>
                <xsl:apply-templates select="tei:body/tei:div[@type='foreword']" mode="pass2"/>
                <xsl:apply-templates select="tei:body/tei:div[@type='introduction']" mode="pass2"/>
	      </front>
	      <body>
                <xsl:for-each select="tei:body">
		  <xsl:for-each select="tei:div|tei:p|tei:table|cals:table">
		    <xsl:choose>
		      <xsl:when test="self::tei:div[@type='toc']"/>
		      <xsl:when test="self::tei:div[@type='foreword']"/>
		      <xsl:when test="self::tei:div[@type='introduction']"/>
		      <xsl:when test="self::tei:div[@type='bibliography']"/>
		      <xsl:when test="self::tei:div[@type='annex']"/>
		      <xsl:otherwise>
			<xsl:apply-templates select="." mode="pass2"/>
		      </xsl:otherwise>
		    </xsl:choose>
		  </xsl:for-each>
                </xsl:for-each>
	      </body>
	      <back>
                <xsl:apply-templates select="tei:body/tei:div[@type='bibliography' or @type='annex']"
				     mode="pass2"/>
	      </back>
	      
	      <!-- copy last milestone -->
	      <xsl:apply-templates select="tei:body/tei:milestone[count(//tei:body/tei:milestone)]" mode="pass2"/>
	    </text>
	  </xsl:variable>
	  <xsl:apply-templates select="$Doctext" mode="pass3"/>
    </xsl:template>

      <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc> inner lists and notes in lists must be moved to inside items </desc></doc>
    <xsl:template match="tei:list/tei:list" mode="pass2"/>
    <xsl:template match="tei:list/tei:note" mode="pass2"/>

    <xsl:template match="tei:item" mode="pass2">
        <item>
            <xsl:copy-of select="@*"/>
            <xsl:variable name="me" select="generate-id()"/>
            <xsl:apply-templates mode="pass2"/>
            <!-- find following sibling lists and notes -->
            <xsl:for-each select="following-sibling::tei:list[preceding-sibling::tei:item[1][generate-id()=$me]]">
                <list>
                    <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="pass2"/>
                </list>
            </xsl:for-each>
            <xsl:for-each select="following-sibling::tei:note[preceding-sibling::tei:item[1][generate-id()=$me]]">
                <note>
                    <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="pass2"/>
                </note>
            </xsl:for-each>
        </item>
    </xsl:template>

    <!-- overrides for part 2 -->
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
    <desc>Zap &lt;div&gt; with empty head only </desc></doc>
    
    <xsl:template match="tei:div[count(*)=1 and tei:head[.='']]" mode="pass2"/>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
    <desc>Zap spurious page break </desc></doc>
    <xsl:template match="tei:body/tei:p[count(*)=1 and tei:pb]" mode="pass2"/>


    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
    <desc>Zap empty paragraph </desc></doc>
    <xsl:template match="tei:p[not(*) and not(text())]" mode="pass2"/>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
    <desc>End of bookmark not needed </desc></doc>
    <xsl:template match="w:bookmarkEnd" mode="pass0"/>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
    <desc>Grammatical errors discarded</desc></doc>
    <xsl:template match="w:proofErr" mode="pass0"/>

    <xsl:template match="tei:div[@type='termsAndDefinitions']" mode="pass2">
      <xsl:copy>
	<xsl:copy-of select="@*"/>
	<xsl:variable name="name">
	  <xsl:text>termHeading2</xsl:text>
	</xsl:variable>
	<xsl:for-each-group select="*" group-starting-with="tei:p[@rend=$name]">
	  <xsl:choose>
	    <xsl:when test="self::tei:p[@rend=$name]">
	      <xsl:apply-templates select="." mode="group"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:apply-templates select="current-group()" mode="pass2"/>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:for-each-group>
      </xsl:copy>
    </xsl:template>
  <xsl:template match="tei:note/tei:p/tei:hi[tei:match(@rend,'FootnoteReference')]"
		mode="pass2" priority="1001">
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
  <desc>Merge adjacent &lt;termEntry&gt; with same ID</desc></doc>
  <xsl:template match="tbx:termEntry" mode="pass2">
      <xsl:variable name="ID" select="@id"/>
      <xsl:choose>
	<xsl:when test="$ID=preceding-sibling::tbx:termEntry/@id"/>
	<xsl:when test="not(following-sibling::tbx:termEntry[@id=$ID])">
	  <xsl:copy>
	    <xsl:apply-templates select="@*" mode="pass2"/>
	    <xsl:apply-templates mode="pass2"/>
	  </xsl:copy>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:copy>
	    <xsl:copy-of select="@*"/>
	    <xsl:for-each select="tbx:langSet">
	      <xsl:copy>
		<xsl:apply-templates select="@*" mode="pass2"/>
		<xsl:apply-templates mode="pass2" select="../tbx:note"/>
		<xsl:apply-templates mode="pass2" select="../tbx:descripGrp"/>
		<xsl:apply-templates mode="pass2"/>
	      </xsl:copy>
	    </xsl:for-each>
	    
	    <xsl:for-each select="following-sibling::tbx:termEntry[@id=$ID]/tbx:langSet">
	      <xsl:copy>
		<xsl:apply-templates select="@*" mode="pass2"/>
		<xsl:apply-templates mode="pass2" select="tbx:note"/>
		<xsl:apply-templates mode="pass2" select="../tbx:descripGrp"/>
		<xsl:apply-templates mode="pass2"/>
	      </xsl:copy>
	    </xsl:for-each>
	  </xsl:copy>
	</xsl:otherwise>
      </xsl:choose>

  </xsl:template>
  <xsl:template match="tbx:langSet/@xml:lang" mode="pass2">
    <xsl:choose>
      <xsl:when
	  test="parent::tbx:langSet/tbx:ntig/tbx:termGrp/tbx:termNote[@type='geographicalUsage']">
	<xsl:copy-of select="."/>
      </xsl:when>
      <xsl:when test="contains(.,'-')">
	<xsl:attribute name="xml:lang">
	  <xsl:value-of
	      select="substring-before(.,'-')"/>
	</xsl:attribute>
      </xsl:when>
      <xsl:otherwise>
	<xsl:copy-of select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
   <desc>Remove [SOURCE: ] from source</desc></doc>

  <xsl:template match="tbx:admin[@type='entrySource']/text()" mode="pass2">
    <xsl:analyze-string select="replace(.,'\[SOURCE: ','')" regex="\]$">
      <xsl:matching-substring>
      </xsl:matching-substring>
      <xsl:non-matching-substring>
	<xsl:value-of select="."/>
      </xsl:non-matching-substring>
    </xsl:analyze-string>
  </xsl:template>


  <xsl:template match="tbx:note[@type='noteTerm']" mode="pass2">
    <xsl:if
	test="not(ancestor::tbx:ntig/preceding-sibling::tbx:ntig)">
      <xsl:copy>
	<xsl:apply-templates mode="pass2"/>
      </xsl:copy>
    </xsl:if>
  </xsl:template>
  <!--
      <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
      <desc>Strip label from note on definition</desc></doc>
      <xsl:template match="tbx:descrip/tbx:note/text()" mode="pass2">
      <xsl:value-of select="replace(.,'Note to definition: ','')"/>
      </xsl:template>
  -->

  <xsl:template match="tbx:descrip/tei:hi[tei:match(@rend,'domain')]" mode="pass2"/>

   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
   <desc>Remove space before a moved domain</desc></doc>
  <xsl:template match="tbx:descrip/text()" mode="pass2">
    <xsl:choose>
      <xsl:when test="preceding-sibling::node()[1][self::tei:hi/tei:match(@rend,'domain')]">
	<xsl:value-of select="normalize-space(.)"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
   <desc>Move domain and source to separate containers</desc></doc>

  <xsl:template match="tbx:descripGrp" mode="pass2">
    <xsl:if test="tbx:descrip/tei:hi[tei:match(@rend,'domain')]">
      <descripGrp xmlns="http://www.lisa.org/TBX-Specification.33.0.html">
	<descrip type="subjectField">
	  <xsl:for-each select="tbx:descrip/tei:hi[tei:match(@rend,'domain')]">
	    <xsl:value-of select="translate(.,'&lt;&gt;〈〉','')"/>
	  </xsl:for-each>
	</descrip>
      </descripGrp>
    </xsl:if>
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates mode="pass2"/>
    </xsl:copy>
    <xsl:if test="tbx:descrip/tei:hi[tei:match(@rend,'source')]">
      <xsl:for-each select="tbx:descrip">
	<admin xmlns="http://www.lisa.org/TBX-Specification.33.0.html" type="source">
	  <xsl:for-each select="tei:hi[tei:match(@rend,'source')]|tei:ref">
	    <xsl:copy-of select="."/>
	  </xsl:for-each>
	</admin>
      </xsl:for-each>
    </xsl:if>
  </xsl:template>

  <xsl:template match="tbx:descrip/tei:hi[tei:match(@rend,'source')]" mode="pass2"/>

  <xsl:template match="tbx:descrip/tei:ref" mode="pass2">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates mode="pass2"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="tbx:descrip" mode="pass2">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates mode="pass2"/>
      <xsl:for-each select="ancestor::tbx:termEntry/following-sibling::*[1][self::tei:list]">
	<xsl:copy>
	  <xsl:copy-of select="@*"/>
	  <xsl:apply-templates mode="pass2"/>
	</xsl:copy>
      </xsl:for-each>
    </xsl:copy>
   </xsl:template>
   
   <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
   <desc>Zap empty &lt;list&gt; and one after termEntry </desc></doc>

   <xsl:template match="tei:list" mode="pass2">
     <xsl:choose>
       <xsl:when test="preceding-sibling::*[1][self::tbx:termEntry]">
       </xsl:when>
       <xsl:when test="count(*)=0">
       </xsl:when>
       <xsl:otherwise>
	 <xsl:copy>
	   <xsl:copy-of select="@*"/>
	   <xsl:apply-templates mode="pass2"/>
	 </xsl:copy>
       </xsl:otherwise>
     </xsl:choose>
   </xsl:template>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
    <desc>Strip prefix from note</desc></doc>

   <xsl:template match="tbx:note/text()" mode="pass2">
     <xsl:analyze-string select="." regex="^(NOTE|Note|ANMERKUNG)[^:]+:\s(.*)">
       <xsl:matching-substring>
	 <xsl:value-of select="regex-group(2)"/>
       </xsl:matching-substring>
       <xsl:non-matching-substring>
	 <xsl:value-of select="."/>
       </xsl:non-matching-substring>
     </xsl:analyze-string>
   </xsl:template>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
    <desc>Zap empty <gi>seg</gi> in term</desc></doc>

   <xsl:template match="tbx:term/tei:seg[.=' ']" mode="pass2"/>


    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
    <desc>Zap empty <gi>seg</gi> in <gi>hi</gi> in term</desc></doc>

   <xsl:template match="tbx:term/tei:hi[.=' ']" mode="pass2">
   </xsl:template>

    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  >
    <desc>Strip words and punctuation from term</desc></doc>

   <xsl:template match="tbx:term/tei:hi[.=', ']" mode="pass2"/>

   <xsl:template match="tbx:term/text()" mode="pass2">
     <xsl:variable name="s" select="replace(.,'DEPRECATED: ','')"/>
     <xsl:analyze-string select="$s" regex="(.+),\s*$">
       <xsl:matching-substring>
	 <xsl:value-of select="regex-group(1)"/>
       </xsl:matching-substring>
       <xsl:non-matching-substring>
	 <xsl:value-of select="$s"/>
       </xsl:non-matching-substring>
     </xsl:analyze-string>
   </xsl:template>

   <!--
  <xsl:template match="tbx:descripGrp" mode="pass2">
    <xsl:copy>
      <xsl:apply-templates select="tbx:descrip"/>
      <xsl:if test="tbx:descrip/tei:hi[tei:match(@rend,'source')]">
	<termNote type="source">
	  <xsl:copy-of select="tbx:descrip/tei:hi[tei:match(@rend,'source')]"/>
	</termNote>
      </xsl:if>
    </xsl:copy>
  </xsl:template>
   -->

    <xsl:template match="iso:error" mode="pass2">
      <note place="comment" resp="ISO_validator">
	<date>
	  <xsl:attribute name="when">
	    <xsl:value-of select="tei:whatsTheDate()"/>
	  </xsl:attribute>
	</date>
	<xsl:value-of select="."/>
      </note>
    </xsl:template>

</xsl:stylesheet>
