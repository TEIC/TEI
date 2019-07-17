<?xml version="1.0" encoding="UTF-8"?>

<!--
  This is a module that is used in both tei2fodt and tei2odt. It creates the 
  major components of the ODT file output, which in the former case are 
  sections of the single file output, and in the latter are components of
  separate files which are saved out and compressed into an archive forming 
  the ODT file. 
  
  These templates are expected to expand and develop as we encounter more 
  varieties of content which have to be encoded in the jTEI input files. 
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xd="http://www.oxygenxml.com/ns/doc/xsl"
    exclude-result-prefixes="#all" version="2.0"
    xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0"
    xmlns:style="urn:oasis:names:tc:opendocument:xmlns:style:1.0"
    xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0"
    xmlns:table="urn:oasis:names:tc:opendocument:xmlns:table:1.0"
    xmlns:draw="urn:oasis:names:tc:opendocument:xmlns:drawing:1.0"
    xmlns:fo="urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0"
    xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:dc="http://purl.org/dc/elements/1.1/"
    xmlns:meta="urn:oasis:names:tc:opendocument:xmlns:meta:1.0"
    xmlns:number="urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0"
    xmlns:svg="urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0"
    xmlns:chart="urn:oasis:names:tc:opendocument:xmlns:chart:1.0"
    xmlns:dr3d="urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0"
    xmlns:math="http://www.w3.org/1998/Math/MathML"
    xmlns:form="urn:oasis:names:tc:opendocument:xmlns:form:1.0"
    xmlns:script="urn:oasis:names:tc:opendocument:xmlns:script:1.0"
    xmlns:config="urn:oasis:names:tc:opendocument:xmlns:config:1.0"
    xmlns:ooo="http://openoffice.org/2004/office" xmlns:ooow="http://openoffice.org/2004/writer"
    xmlns:oooc="http://openoffice.org/2004/calc" xmlns:dom="http://www.w3.org/2001/xml-events"
    xmlns:xforms="http://www.w3.org/2002/xforms" xmlns:xsd="http://www.w3.org/2001/XMLSchema"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xmlns:rpt="http://openoffice.org/2005/report"
    xmlns:of="urn:oasis:names:tc:opendocument:xmlns:of:1.2"
    xmlns:xhtml="http://www.w3.org/1999/xhtml" xmlns:grddl="http://www.w3.org/2003/g/data-view#"
    xmlns:officeooo="http://openoffice.org/2009/office"
    xmlns:tableooo="http://openoffice.org/2009/table"
    xmlns:drawooo="http://openoffice.org/2010/draw"
    xmlns:calcext="urn:org:documentfoundation:names:experimental:calc:xmlns:calcext:1.0"
    xmlns:field="urn:openoffice:names:experimental:ooo-ms-interop:xmlns:field:1.0"
    xmlns:formx="urn:openoffice:names:experimental:ooxml-odf-interop:xmlns:form:1.0"
    xmlns:css3t="http://www.w3.org/TR/css3-text/" office:version="1.2"
    office:mimetype="application/vnd.oasis.opendocument.text"
    xpath-default-namespace="http://www.tei-c.org/ns/1.0"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:teix="http://www.tei-c.org/ns/Examples"
    xmlns:saxon="http://saxon.sf.net/"
    xmlns:expath-file="http://expath.org/ns/file"
    xmlns:java="http://www.java.com/"
    xmlns:i18n="i18n"
    xmlns:local="local"
  >
  
  <xsl:import href="../jtei.common.xsl"/>
  
    <xd:doc scope="stylesheet">
        <xd:desc>
            <xd:p><xd:b>Created on:</xd:b> Feb 23, 2014</xd:p>
            <xd:p><xd:b>Author:</xd:b> mholmes</xd:p>
            <xd:p>This stylesheet is a helper file for ODT/FODT conversion, 
            containing templates which are used in the creation both of 
            FODT (standalone ODT) files and conventional ODT archive files. </xd:p>
        </xd:desc>
    </xd:doc>
  
  <xsl:preserve-space elements="teix:*"/>
  
  <xsl:template match="teiHeader"/>
  
  <xsl:template match="text">
        
        <!--            We used turn on change-tracking by default, because this is for
                reviewing/editing purposes. However, I have commented this out 
                because SL reports that it caused problems in the Word doc 
                generated from the ODT file. Most likely, using this element 
                without any actual tracked changes is not a good idea. We have 
                to leave it to the editor to turn on change-tracking. -->
        <!--<text:tracked-changes text:track-changes="true"></text:tracked-changes>-->
        
        <!--                I'm ignoring the following, hoping we don't need it. -->
        <!--<text:sequence-decls>
                    <text:sequence-decl text:display-outline-level="0" text:name="Illustration"/>
                    <text:sequence-decl text:display-outline-level="0" text:name="Table"/>
                    <text:sequence-decl text:display-outline-level="0" text:name="Text"/>
                    <text:sequence-decl text:display-outline-level="0" text:name="Drawing"/>
                </text:sequence-decls>-->
        
        <!--                We need to grab the paper title and authors from the header. -->
        <text:p text:style-name="teiHead0">
          <xsl:for-each select="/TEI/teiHeader[1]/fileDesc[1]/titleStmt[1]/title[@type='main'], /TEI/teiHeader[1]/fileDesc[1]/titleStmt[1]/title[not(@type='main')]">
            <xsl:apply-templates/>
            <xsl:if test="position() != last()">
              <xsl:text>: </xsl:text>
            </xsl:if>
          </xsl:for-each>
        </text:p>
        
        <text:p text:style-name="teiHead0">
          <xsl:for-each select="/TEI/teiHeader[1]/fileDesc[1]/titleStmt[1]/author/name">
            <xsl:choose>
              <xsl:when test="position() gt 1 and position() = last()"><xsl:text> and </xsl:text></xsl:when>
              <xsl:when test="position() gt 1"><xsl:text>, </xsl:text></xsl:when>
            </xsl:choose>
            <xsl:value-of select="normalize-space(.)"/>
          </xsl:for-each>
        </text:p>
        
        <!--             Next, we add keywords. -->
        <!--              MDH: COMMENTED OUT. KEYWORDS ARE ADDED AFTER THE ABSTRACT. -->
        <!--              <text:p text:style-name="teiHead1">
                Keywords:
              </text:p>
              
              <text:p text:style-name="teiPara">
                <xsl:for-each select="/TEI/teiHeader[1]/profileDesc[1]/textClass[1]/keywords[1]/term">
                  <xsl:choose>
                    <xsl:when test="position() gt 1 and position() = last()"><xsl:text> and </xsl:text></xsl:when>
                    <xsl:when test="position() gt 1"><xsl:text>, </xsl:text></xsl:when>
                  </xsl:choose>
                  <xsl:value-of select="normalize-space(.)"/>
                </xsl:for-each>
              </text:p>-->
        
        <xsl:apply-templates select="//front"/>
        
        <xsl:apply-templates select="//body"/>
        
        <xsl:apply-templates select="//back"/>
        
        <xsl:call-template name="aboutAuthors"/>
        
  </xsl:template>

  <xsl:template match="text" mode="not">
    <text:p text:style-name="teiHead0">
      <xsl:apply-templates select="/TEI/teiHeader[1]/fileDesc[1]/titleStmt[1]/title[@type='main']"/>
    </text:p>
    
    <text:p text:style-name="teiHead0">
      <xsl:for-each select="/TEI/teiHeader[1]/fileDesc[1]/titleStmt[1]/author/name">
        <xsl:choose>
          <xsl:when test="position() gt 1 and position() = last()"><xsl:text> and </xsl:text></xsl:when>
          <xsl:when test="position() gt 1"><xsl:text>, </xsl:text></xsl:when>
        </xsl:choose>
        <xsl:value-of select="normalize-space(.)"/>
      </xsl:for-each>
    </text:p>
    
    <xsl:apply-templates select="//front"/>
    
    <xsl:apply-templates select="//body"/>
    
    <xsl:apply-templates select="//back"/>
    
    <xsl:call-template name="aboutAuthors"/>
  </xsl:template>
    
    <xsl:template name="aboutAuthors">
      <xsl:if test="/TEI/teiHeader[1]/fileDesc[1]/titleStmt[1]/author/affiliation">
        <text:p/><text:p/>
        <xsl:variable name="author-label" select="i18n:key('author-label')"/>
        <xsl:choose>
          <xsl:when test="count(/TEI/teiHeader[1]/fileDesc[1]/titleStmt[1]/author) gt 1"><text:p text:style-name="teiHead1">
            <xsl:value-of select="(i18n:plural($author-label)/@pl, concat($author-label, 's'))[1]"/>
          </text:p></xsl:when>
          <xsl:otherwise><text:p text:style-name="teiHead1">
            <xsl:value-of select="$author-label"/>
          </text:p></xsl:otherwise>
        </xsl:choose>
        <xsl:for-each select="/TEI/teiHeader[1]/fileDesc[1]/titleStmt[1]/author">
          <text:p text:style-name="teiHead4">
            <xsl:value-of select="upper-case(string-join(name/(forename, surname), ' '))"/>
          </text:p>          
          <text:p text:style-name="teiPara">
            <xsl:apply-templates select="affiliation"/>
          </text:p>
        </xsl:for-each>
      </xsl:if>
    </xsl:template>
  
  <!--Front -->
  <xsl:template match="front">
    <xsl:apply-templates select="div[@type='abstract']"/>
    <xsl:call-template name="front.divs"/>
  </xsl:template>
  
<!--Regular templates for body text. -->
    
<!--    Shouldn't need to do anything with divs. -->
    <xsl:template match="div"><xsl:apply-templates/></xsl:template>
    
<!--    However, the abstract does need a special header. Keywords are included after the abstract too. -->
    <xsl:template match="div[@type='abstract']">
      <xsl:variable name="current" select="."/>
        <text:p text:style-name="teiHead1">
          <xsl:value-of select="i18n:key(concat(@type, '-label'), (@xml:lang, $jtei.lang)[.][1])"/>
        </text:p>
        <xsl:apply-templates/>
      
<!--    Add the keywords.  -->
      <xsl:if test="not(following-sibling::div[@type='abstract'])">
        <text:p text:style-name="teiPara">
          <xsl:value-of select="concat(i18n:key('keywords-label'), ': ')"/>
          <xsl:value-of select="string-join(//textClass/keywords/term, ', ')"/></text:p>
      </xsl:if>
    </xsl:template>
  
  <xsl:template name="front.divs">
    <xsl:variable name="current" select="."/>
    <xsl:for-each select="for $i in $div.types.front[. != 'abstract'] return $current/div[@type = $i]">
      <text:p text:style-name="teiHead4">
        <xsl:value-of select="upper-case(i18n:key(concat(@type, '-label')))"/>
      </text:p>
      <xsl:apply-templates/>
    </xsl:for-each>
  </xsl:template>
  
  
  <!--    So does an appendix. -->
  <xsl:template match="div[@type='appendix']">
<!--    Add a couple of blank lines. -->
    <text:p/><text:p/>
    <text:p text:style-name="teiHead1">
      <xsl:value-of select="concat(i18n:key(concat(@type, '-label')), ' ')"/>
      <xsl:value-of select="if (preceding::div[@type='appendix'] or following::div[@type='appendix']) then xs:string(count(preceding::div[@type='appendix']) + 1) else ''"/></text:p>
    <xsl:apply-templates/>
  </xsl:template>
    
<!--    Heads are styled depending on their embedding level. -->
    <xsl:template match="div/head">
      <xsl:variable name="sectionNumber" select="local:getSectionNumber(./parent::div, '')"/>
        <text:h text:style-name="teiHead{count(ancestor::div)}" text:outline-level="{count(ancestor::div)}">
          <xsl:if test="parent::div[@xml:id] and not(preceding-sibling::*)">
            <text:bookmark text:name="{parent::div/@xml:id}"/>
          </xsl:if>
          <xsl:value-of select="concat($sectionNumber, ' ')"/>
          <xsl:apply-templates/>
        </text:h>
    </xsl:template>
  
<!--  It is possible to have a head as part of the body without an intervening div. -->
  <xsl:template match="body/head">
    <text:h text:style-name="teiHead1" text:outline-level="1">
<!--We don't bother with a bookmark or with a section number; this is a single-
      section article. -->
      <xsl:apply-templates/>
    </text:h>
  </xsl:template>
  
  <xsl:function name="local:getSectionNumber" as="xs:string">
    <xsl:param name="div" as="node()"/>
    <xsl:param name="lowerLevels" as="xs:string"/>
    <xsl:variable name="thisLevel" select="count($div/preceding-sibling::div) + 1"/>
    <xsl:choose>
<!--     When it's in the back matter, we don't number it.  -->      
      <xsl:when test="$div/ancestor::back"><xsl:value-of select="''"/></xsl:when>
      <xsl:when test="$div/ancestor::div"><xsl:value-of select="local:getSectionNumber($div/parent::div, string-join((xs:string($thisLevel), $lowerLevels), '.'))"/></xsl:when>
      <xsl:otherwise><xsl:value-of select="string-join((xs:string($thisLevel), $lowerLevels), '.')"/></xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  
<!--  This function generates the linking text for sections (i.e. headers, based on nesting, 
      figures and tables (much simpler). -->
  <xsl:function name="local:getSectionLinkText" as="xs:string">
    <xsl:param name="targEl" as="node()"/>
    <xsl:param name="pointerEl" as="node()"/>
    <xsl:variable name="immediatePrecedingText" select="($pointerEl/preceding-sibling::node()/descendant-or-self::text()[not(ancestor::note[following::* intersect $pointerEl])][normalize-space()])[last()]"/>
    <xsl:variable name="capitalize" select="if ($jtei.lang = ('de') or not($pointerEl) or not($immediatePrecedingText) or $immediatePrecedingText[matches(., '[\.!?]\s*$')]) then true() else false()"/>
    <xsl:message><xsl:value-of select="$targEl/local-name()"/></xsl:message>
    <xsl:choose>
      <xsl:when test="$targEl[self::div][ancestor::back]">
        <xsl:variable name="sectionNumber" select="if ($targEl/preceding::div[@type='appendix'] or $targEl/following::div[@type='appendix']) then xs:string(count($targEl/preceding::div[@type='appendix']) + 1) else ''"/>
        <xsl:value-of select="concat(i18n:key('appendix-label'), ' ', replace($sectionNumber, '\.$', ''))"/>
      </xsl:when>
      <xsl:when test="$targEl[self::div]">
        <xsl:variable name="sectionNumber" select="local:getSectionNumber($targEl, '')"/>
        <xsl:value-of select="concat(local:capitalize(i18n:key('section-label'), $capitalize), ' ', replace($sectionNumber, '\.$', ''))"/>
      </xsl:when>
      <xsl:when test="$targEl[self::figure[graphic]]">
        <xsl:value-of select="concat(local:capitalize(i18n:key('figure-label'), $capitalize), ' ', count($targEl/preceding::figure[graphic]) + 1)"/>
      </xsl:when>
      <xsl:when test="$targEl[self::figure[*:egXML or eg]]">
        <xsl:value-of select="concat(local:capitalize(i18n:key('example-label'), $capitalize), ' ', count($targEl/preceding::figure[*:egXML or eg]) + 1)"/>
      </xsl:when>
      <xsl:when test="$targEl[self::table]">
        <xsl:value-of select="concat(local:capitalize(i18n:key('table-label'), $capitalize), ' ', count($targEl/preceding::table) + 1)"/>
      </xsl:when>
      <xsl:when test="$targEl[self::note]">
        <xsl:value-of select="concat(local:capitalize(i18n:key('note-label'), $capitalize), ' ', local:get.note.nr($targEl))"/>
      </xsl:when>
      <xsl:otherwise><xsl:value-of select="''"/></xsl:otherwise>
    </xsl:choose>
    
  </xsl:function>
  
  <xsl:function name="local:capitalize">
    <xsl:param name="string" as="xs:string"/>
    <xsl:param name="capitalize" as="xs:boolean"/>
    <xsl:value-of select="if ($capitalize) then concat(upper-case(substring($string, 1, 1)), substring($string, 2)) else $string"/>
  </xsl:function>

<!-- Paragraphs are styled with a single style. 
    However, we have a problem with paras nested in list items, 
    which requires some fancy footwork. -->
    <xsl:template match="p">
      <xsl:param name="note.context" select="ancestor::*[self::front|self::body|self::back]" tunnel="yes" as="element()?"/>
        <xsl:choose>
          <xsl:when test="normalize-space(.) = '' and not(*)"></xsl:when>
          <!-- [RvdB] give right styling to simple/simplified paragraphs inside lists -->
          <xsl:when test="(parent::item and not(child::cit or child::table or child::teix:egXML or child::figure or child::list[not(matches(@rend, 'inline'))] or child::eg)) or @rend = 'teiListItem'">
            <text:p text:style-name="teiListItem">
              <xsl:apply-templates/>
            </text:p>
          </xsl:when>  
          <xsl:when test="parent::note"><xsl:apply-templates/></xsl:when>
          <xsl:when test="not(child::cit or child::table or child::teix:egXML or child::figure or child::list[not(matches(@rend, 'inline'))] or child::eg)">
                <text:p text:style-name="teiPara"><xsl:apply-templates/></text:p>
            </xsl:when>
            <xsl:otherwise>
<!-- We need to pre-process to pull quotes out of paragraphs.               -->
                <xsl:variable name="preProcessed">
                    <xsl:call-template name="preProcessNestedBlocks">
                        <xsl:with-param name="inputPara" select="."/>
                    </xsl:call-template>
                </xsl:variable>
                <xsl:apply-templates select="$preProcessed/*">
                  <xsl:with-param name="note.context" select="$note.context" tunnel="yes"/>
                </xsl:apply-templates>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    
    
<!--    This template is designed to take a TEI <p> with a nested block element 
        and return separate <p> elements for the bits in between the blockquote,
        as well as the blockquote itself. ODT can't handle nested blocks, it
        seems.
        -->
    <xsl:template name="preProcessNestedBlocks">
        <xsl:param name="inputPara"/>
<!--        First we output the first block before the first interrupting element. -->
        <tei:p>
          <!-- [RvdB] add @rend='teiListItem' to simplified paragraphs inside lists -->
          <xsl:if test="$inputPara/parent::item">
            <xsl:attribute name="rend">teiListItem</xsl:attribute>
          </xsl:if>
          <xsl:copy-of select="$inputPara/(*|text())[not(self::cit or self::table or self::teix:egXML or self::figure or self::list[not(matches(@rend, 'inline'))] or self::eg) and not(preceding-sibling::cit or preceding-sibling::table or preceding-sibling::teix:egXML or preceding-sibling::figure or preceding-sibling::list[not(matches(@rend, 'inline'))] or preceding-sibling::eg)]"/>
        </tei:p>
<!--        Now we work through each cit and its following bits. -->
      <xsl:for-each select="cit | table | teix:egXML | figure | list[not(matches(@rend, 'inline'))] | eg">
            <xsl:variable name="pos" select="position()"/>
            <xsl:copy-of select="."/>
            <tei:p>
              <!-- [RvdB] add @rend='teiListItem' to simplified paragraphs inside lists -->
              <xsl:if test="$inputPara/parent::item">
                <xsl:attribute name="rend">teiListItem</xsl:attribute>
              </xsl:if>
              <xsl:copy-of select="$inputPara/(*|text())[not(self::cit or self::table or self::teix:egXML or self::figure or self::list[not(matches(@rend, 'inline'))] or self::eg) and count(preceding-sibling::cit | preceding-sibling::table | preceding-sibling::teix:egXML | preceding-sibling::figure | preceding-sibling::list[not(matches(@rend, 'inline'))] | preceding-sibling::eg) = $pos]"/>
            </tei:p>
        </xsl:for-each>
    </xsl:template>
    
<!--    Emphasis and other similar things are italic. -->
    <xsl:template match="emph[@rendition='#italic' or not(@rendition)] | mentioned | foreign | term | hi[@rendition='#italic'] | title[@level=('m', 'j')]">
        <text:span text:style-name="teiItalics"><xsl:apply-templates/></text:span>
    </xsl:template>
    
    
<!--    Tag names and the like are always done with a monospaced font. -->
    <xsl:template match="gi | att | val">
        <text:span text:style-name="teiCode">
          <xsl:value-of select="local:get.delimiter('start', .)"/>
          <xsl:apply-templates/>
          <xsl:value-of select="local:get.delimiter('end', .)"/>
        </text:span>
    </xsl:template>
    <xsl:template match="tag">
      <text:span text:style-name="teiCode">
        <xsl:value-of select="local:get.delimiter('start', .)"/>
        <xsl:apply-templates/>
        <xsl:value-of select="local:get.delimiter('end', .)"/>
      </text:span>
    </xsl:template>
    <xsl:template match="ident | code">
        <text:span text:style-name="teiCode"><xsl:apply-templates/></text:span>
    </xsl:template>
    
<!--    Quotations, inline and block, and quotation-marked things.    -->
<!--    We can handle article titles at the same time. The same issues with regard to following
        punctuation hold true. -->
<!--    NOTE ON HANDLING OF PUNCTUATION: The current setup in this and the following template 
        only deal with text nodes; this should be rewritten so that it handles element nodes 
        which match the context too. However, they are likely to be very rare. -->
  <xsl:template match="quote[not(parent::cit)] | q[not(parent::cit)] | title[@level='a'] | title[@level='u'] | soCalled">
<!--   Single quotes or double quotes? Depends on nesting level. -->
    <xsl:variable name="quoteLevel" select="count(ancestor::quote|ancestor::q|ancestor::title[@level='a']|ancestor::title[@level='u']|ancestor::soCalled)"/>
    <xsl:choose>
      <xsl:when test="$quoteLevel mod 2 = 1"><xsl:value-of select="$lsquo"/><xsl:apply-templates/><xsl:call-template name="include.punctuation"/><xsl:value-of select="$rsquo"/></xsl:when>
      <xsl:otherwise><xsl:value-of select="$ldquo"/><xsl:apply-templates/><xsl:call-template name="include.punctuation"/><xsl:value-of select="$rdquo"/></xsl:otherwise>
    </xsl:choose>
  </xsl:template>
    
    <xsl:template match="supplied">[<xsl:apply-templates/>]</xsl:template>
  
<!-- Gaps are turned into ellipses; if they directly follow a period, we don't add a leading 
  space (to get the Chicago four-dot effect), but otherwise there is a leading space. 
  There's always a following space. -->
  <xsl:template match="gap"><xsl:if test="not(preceding-sibling::node()[1][self::text()][ends-with(., '.')])"><text:s/></xsl:if>…<text:s/></xsl:template>
  
<!--  We handle ordinal numbers by superscripting the post-numeric bit. -->
  <xsl:template match="num[@type='ordinal']">
    <xsl:analyze-string regex="^\d+" select="text()">
      <xsl:matching-substring><xsl:value-of select="."/></xsl:matching-substring>
      <xsl:non-matching-substring><text:span text:style-name="teiSuperscript"><xsl:value-of select="."/></text:span></xsl:non-matching-substring>
    </xsl:analyze-string>
  </xsl:template>
    
<!--  <cit>s have been unravelled from their embedding in <p>s, but their child::quote
      may itself contain <p>s. There may also be <q> elements in <cit>.  -->
    <xsl:template match="cit">
      <xsl:choose>
        <xsl:when test="quote/p | q/p">
          <xsl:for-each select="quote/p | q/p">
            <text:p text:style-name="teiBlockquote">
              <xsl:apply-templates/>
            </text:p>
          </xsl:for-each>
        </xsl:when>
        <xsl:otherwise>
          <text:p text:style-name="teiBlockquote">
            <xsl:apply-templates select="quote/(*|text()) | q/(*|text())"/>
          </text:p>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:if test="ref">
        <text:p text:style-name="teiBlockquoteRef"><xsl:apply-templates select="ref/(*|text())"/></text:p>
      </xsl:if>
    </xsl:template>
    
    <xsl:template match="cit/ref">
        <text:p text:style-name="teiPara">(<xsl:apply-templates/>)</text:p>
    </xsl:template>
    
<!--    Links -->
  <xsl:template match="ref[@target[not(starts-with(., '#'))]] | ptr[@target[not(starts-with(., '#'))]]">
        <text:a xlink:type="simple" xlink:href="{@target}"><xsl:choose><xsl:when test="*|text()"><xsl:apply-templates/></xsl:when><xsl:otherwise><xsl:value-of select="@target"/></xsl:otherwise></xsl:choose></text:a>
    </xsl:template>
    
<!--    Internal cross-references. -->
  <xsl:template match="ref[@type='crossref']">
    <xsl:variable name="targId" select="substring-after(@target, '#')"/>
    <xsl:variable name="targetEl" as="element()" select="$docRoot//*[@xml:id=$targId][1]"/>
    <xsl:message><xsl:value-of select="concat('Finding link to ', for $s in string($targetEl) return concat(substring($s, 1, 40), if (string-length($s) > 40) then '...' else ''))"/></xsl:message>
    <text:a xlink:type="simple" xlink:href="{@target}">
      <xsl:apply-templates/>
    </text:a>    
  </xsl:template>
  
  <xsl:template match="ptr[@type='crossref']">
    <xsl:variable name="current" select="."/>
    <xsl:variable name="targIds" select="for $a in tokenize(@target, '\s+') return substring-after($a, '#')"/>
    <xsl:variable name="labels">
      <xsl:for-each select="$targIds">
        <xsl:variable name="targetEl" select="$docRoot//*[@xml:id=current()][1]"/>
        <xsl:message><xsl:value-of select="concat('Finding link to ', for $s in string($targetEl) return concat(substring($s, 1, 40), if (string-length($s) > 40) then '...' else ''))"/></xsl:message>
        <label type="{$targetEl/name()}" n="{if ($targetEl/self::note) then concat('ftn', local:get.note.nr($targetEl)) else current()}">
          <xsl:choose>
            <xsl:when test="local-name($targetEl) = 'div'"><xsl:value-of select="local:getSectionLinkText($targetEl, $current)"/></xsl:when>
            <xsl:when test="local-name($targetEl) = 'figure'"><xsl:value-of select="local:getSectionLinkText($targetEl, $current)"/></xsl:when>
            <xsl:when test="local-name($targetEl) = 'table'"><xsl:value-of select="local:getSectionLinkText($targetEl, $current)"/></xsl:when>
            <xsl:when test="local-name($targetEl) = 'note'"><xsl:value-of select="local:getSectionLinkText($targetEl, $current)"/></xsl:when>
          </xsl:choose>
        </label>
      </xsl:for-each>
    </xsl:variable>
    <xsl:for-each-group select="$labels/*" group-adjacent="@type">
      <xsl:variable name="counter.group" select="position()"/>
      <xsl:call-template name="enumerate"></xsl:call-template>
      <xsl:for-each select="current-group()">
        <xsl:call-template name="enumerate"></xsl:call-template>
        <xsl:choose>
          <xsl:when test="normalize-space()">
            <xsl:variable name="label.formatted">
              <xsl:choose>
                <!-- pluralize if there are multiple targets of the same type -->
                <xsl:when test="not(@type = preceding-sibling::*[1]/@type) and @type = following-sibling::*[1]/@type">
                  <!-- if no specific plural can be found, just add an -s -->
                  <xsl:value-of select="(
                    for $i in 
                      i18n:plural(lower-case(normalize-space(replace(., '\d', ''))))[@pl]
                    return replace(., substring($i, 2), substring($i/@pl, 2))
                    , replace(., '^(\w+)', '$1s'))[1]"/>
                </xsl:when>
                <xsl:when test="@type = preceding-sibling::*[1]/@type">
                  <xsl:value-of select="normalize-space(replace(., '^(\w+)', ''))"/>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:value-of select="."/>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:variable>
            <text:a xlink:type="simple" xlink:href="#{@n}">
              <xsl:value-of select="if ($counter.group = 1 and position() = 1 or $jtei.lang = ('de')) then $label.formatted else lower-case($label.formatted)"/>
            </text:a>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="concat('[bad link to item: ', @n, ']')"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:for-each>
    </xsl:for-each-group>
  </xsl:template>
      
<!--    Footnotes -->
    <xsl:template match="note">
      <xsl:param name="note.counter" tunnel="yes" as="xs:integer" select="0"/>
      <xsl:param name="note.context" tunnel="yes" as="element()?" select="ancestor::*[self::front|self::body|self::back]"/>
      <xsl:variable name="note.nr" select="local:get.note.nr(.) + $note.counter"/>
      <xsl:call-template name="include.punctuation"/>
      <text:span text:style-name="T1"><text:note text:id="{$note.context/name()}.ftn{$note.nr}" text:note-class="{if (@place eq 'end') then 'endnote' else 'footnote'}"><text:note-citation><xsl:number value="$note.nr" format="{local:format.note.nr($note.context)}"/></text:note-citation><text:note-body><text:p text:style-name="teiFootnote"><text:span text:style-name="footnote_20_reference"/><xsl:text>. </xsl:text> <xsl:apply-templates /></text:p></text:note-body></text:note></text:span>
    </xsl:template>
    
<!--    Tables. -->
    <xsl:template match="table">
        <xsl:variable name="current" select="."/>
        <xsl:variable name="referenceNode" select="$docRoot//table[deep-equal((@*|node()), $current/(@*|node()))]"/>
        <xsl:variable name="tableNum" select="count($referenceNode/preceding::table) + 1"/>
        <xsl:variable name="tableId" select="concat('Table_', $tableNum)"/>
        <xsl:variable name="numCols" select="sum(for $c in descendant::row[1]/cell return if ($c/@cols) then xs:integer($c/@cols) else 1)"/>
        <!-- [RvdB] solution for tables with multiple (non-adjacent) header rows: group by header row and wrap groups in separate tables (which still look as a single table) -->
        <xsl:for-each-group select="*" group-starting-with="row[@role='label']">
            <table:table table:style-name="{$tableId}">
            <!--       Table column widths.          -->
            <xsl:for-each select="1 to $numCols">
                <table:table-column table:style-name="{concat($tableId, '_col_', .)}"/>
            </xsl:for-each>
            <!--        We also have to handle header rows separately, because they need to be in a 
            special grouping element. -->
            <xsl:if test="current-group()/self::row[@role='label']">
                <table:table-header-rows>
                    <xsl:for-each select="current-group()/self::row[@role='label']">
                        <table:table-row><xsl:apply-templates/></table:table-row>
                    </xsl:for-each>
                </table:table-header-rows>
            </xsl:if>
            <table:table-rows><xsl:apply-templates select="current-group()"/></table:table-rows>
        </table:table>
    </xsl:for-each-group>
    
    <!--        We have to deal with a table headers manually and put it after the table. -->
    <xsl:for-each select="head">
      <text:p text:style-name="{if (following-sibling::head) then 'teiTableFigureCaptionFirst' else 'teiTableFigureCaptionLast'}">
        <xsl:if test="parent::table/@xml:id and position() = 1">
          <text:bookmark text:name="{parent::table/@xml:id}"/>
        </xsl:if>
        <xsl:if test="not(matches(., '^[Tt]able')) and not(@type='license')">
          <xsl:value-of select="concat(local:capitalize(i18n:key('table-label'), true()), ' ', $tableNum, '. ')"/>
        </xsl:if><xsl:apply-templates select="*|text()"/><xsl:call-template name="punctuate-head"/></text:p>
    </xsl:for-each>
  </xsl:template>
  
<!--    Suppress normal processing of the table head element and table header rows. -->
    <xsl:template match="table/head"/>
    <xsl:template match="row[@role='label']"/>    
  
    <xsl:template match="row[not(@role='label')]">
        <table:table-row><xsl:apply-templates/></table:table-row>
    </xsl:template>
    
    <xsl:template match="cell">
      <table:table-cell table:style-name="{if (@role='label' or parent::row[@role='label']) then if (following-sibling::cell) then 'table_head_left' else 'table_head_right' else if (following-sibling::cell) then 'table_cell_left' else 'table_cell_right'}">
        <xsl:if test="@cols"><xsl:attribute name="table:number-columns-spanned"><xsl:value-of select="@cols"/></xsl:attribute></xsl:if>
        <xsl:if test="@rows"><xsl:attribute name="table:number-rows-spanned"><xsl:value-of select="@rows"/></xsl:attribute></xsl:if>
        <!-- [RvdB] egXML generates its own <text:p/>, so skip it here -->
        <xsl:choose>
            <xsl:when test="teix:egXML">
                <xsl:apply-templates/>
            </xsl:when>
            <xsl:otherwise><text:p text:style-name="{if (@role='label' or parent::row[@role='label']) then 'teiParaTinyMarginsHeader' else 'teiParaTinyMargins'}"><xsl:apply-templates/></text:p></xsl:otherwise>
        </xsl:choose>
      </table:table-cell>
    </xsl:template>

<!--    Standard code is a bit tricky. -->
  <xsl:template match="eg">
    <!-- determine maximal amount of preceding whitespace that can be stripped out -->
    <xsl:variable name="stripIndent" select="min((for $line in tokenize(., '\n')[.] return string-length(replace($line, '^(\s+).*', '$1'))))"/>
    <text:p text:style-name="teiCodeBlock">
      <xsl:if test="parent::figure and following-sibling::head"><xsl:attribute name="fo:keep-with-next">always</xsl:attribute></xsl:if>
      <xsl:if test="parent::figure/@xml:id">
        <text:bookmark text:name="{parent::figure/@xml:id}"/>
      </xsl:if>
      <xsl:analyze-string select="." regex="\n">
        <xsl:matching-substring>
          <text:line-break/>
        </xsl:matching-substring>
        <xsl:non-matching-substring>
          <xsl:analyze-string select="if ($stripIndent > 0) then replace(., concat('^\s{', $stripIndent, '}'), '') else ." regex="\s">
            <xsl:matching-substring><text:s/></xsl:matching-substring>
            <xsl:non-matching-substring><xsl:copy-of select="."/></xsl:non-matching-substring>
          </xsl:analyze-string>
        </xsl:non-matching-substring>
      </xsl:analyze-string>
    </text:p>
  </xsl:template>
    
<!--    egXML is going to be quite exciting. -->
    <xsl:template match="teix:egXML[not(ancestor::teix:egXML)]">
      <text:p text:style-name="teiEgXML">
        <xsl:if test="parent::figure and following-sibling::head"><xsl:attribute name="fo:keep-with-next">always</xsl:attribute></xsl:if>
        <xsl:if test="parent::figure[@xml:id]">
          <text:bookmark text:name="{parent::figure/@xml:id}"/>
        </xsl:if>
          <xsl:apply-templates/>
      </text:p>
    </xsl:template>
    
<!--    Handling of all tags within an egXML. -->
  <xsl:template match="*[not(local-name(.) = 'egXML')][ancestor::teix:egXML]"><!-- Opening tag, including any attributes. --><text:span text:style-name="teiXmlTag">&lt;<xsl:value-of select="name()"/></text:span><xsl:for-each select="@*"><text:span text:style-name="teiXmlAttName"><xsl:text> </xsl:text><xsl:value-of select="name()"/>=</text:span><text:span text:style-name="teiXmlAttVal">"<xsl:value-of select="local:escapeEntitiesForEgXMLAttribute(.)"/>"</text:span></xsl:for-each><xsl:choose><xsl:when test="local:isSelfClosing(.)"><text:span text:style-name="teiXmlTag">/&gt;</text:span></xsl:when><xsl:otherwise><text:span text:style-name="teiXmlTag">&gt;</text:span><xsl:apply-templates select="* | text() | comment() | processing-instruction()"/><text:span text:style-name="teiXmlTag">&lt;/<xsl:value-of select="name()"/>&gt;</text:span></xsl:otherwise></xsl:choose></xsl:template>
    
    <!-- We also need to process XML comments in egXML. -->
    <xsl:template match="teix:*/comment()">
        <text:span text:style-name="teiXmlComment">
          <xsl:value-of select="local:get.delimiter('start', .)"/> <xsl:value-of select="."/> <xsl:value-of select="local:get.delimiter('end', .)"/></text:span><xsl:text>
</xsl:text>
    </xsl:template>
    
  <!-- We also need to process XML processing instructions in egXML. -->
  <xsl:template match="teix:*/processing-instruction()">
    <text:span text:style-name="teiXmlPi"><xsl:value-of select="local:get.delimiter('start', .)"/><xsl:value-of select="string-join((name(), .), ' ')"/>    <xsl:value-of select="local:get.delimiter('end', .)"/></text:span><xsl:text>
</xsl:text>
  </xsl:template>
  
  <!--    Handling of whitespace is tricky within egXML. We basically want to preserve it,
    with some linebreaks, and try to indent helpfully if there were linebreaks in the original. -->
    <xsl:template match="text()[ancestor::teix:egXML]">
        <xsl:variable name="container" select="parent::*"/>
        <xsl:variable name="currNode" select="."/>
        <xsl:analyze-string select="." regex="\n">
            <xsl:matching-substring>
                <text:line-break/>
                <xsl:for-each select="$currNode/ancestor::*[not(descendant-or-self::teix:egXML)]"><text:s/></xsl:for-each>
                <xsl:if test="$currNode/following-sibling::node()"><text:s/></xsl:if>
            </xsl:matching-substring>
          <xsl:non-matching-substring><xsl:value-of select="local:escapeEntitiesForEgXML(.)"/></xsl:non-matching-substring>
        </xsl:analyze-string>
    </xsl:template>
    
    
<!--    Bibliography. -->
    <xsl:template match="div[@type='bibliography']">
        <text:p text:style-name="teiHead1">
          <xsl:value-of select="i18n:key('bibliography-label')"/>
        </text:p>
        <xsl:apply-templates/>
    </xsl:template>
    
    <xsl:template match="listBibl">
        <xsl:apply-templates/>
    </xsl:template>
    
    <xsl:template match="listBibl/bibl">
        <text:p text:style-name="teiBiblioEntry">
<!--          Let's see whether this bibl has the same 
              authors as the previous one. -->
            <xsl:variable name="authorOrEditor" select="child::*[1]/local-name()"/>
            <xsl:choose>
              <xsl:when test="$authorOrEditor = ('author', 'editor', 'orgName') and preceding-sibling::bibl">
                <!--<xsl:comment><xsl:value-of select="local:getAuthorsOrEditors(., $authorOrEditor)"/></xsl:comment>-->
                <xsl:choose>
                  <xsl:when test="local:getAuthorsOrEditors(., $authorOrEditor) = local:getAuthorsOrEditors(preceding-sibling::bibl[1], $authorOrEditor)">
<!--           The authors or editors for this bibl match those from the 
               preceding one, so they should be rendered as a dash. -->
                    <xsl:text>———</xsl:text>
                    <xsl:apply-templates select="child::node()[not(local-name() = $authorOrEditor) or preceding-sibling::*[not(local-name() = $authorOrEditor)]]"/>
                  </xsl:when>
                  <xsl:otherwise><xsl:apply-templates/></xsl:otherwise>
                </xsl:choose>
              </xsl:when>
              <xsl:otherwise><xsl:apply-templates/></xsl:otherwise>
            </xsl:choose>
        </text:p>
    </xsl:template>
  
  <xsl:function name="local:getAuthorsOrEditors" as="xs:string?">
    <xsl:param name="bibl" as="element(bibl)"/>
    <xsl:param name="tagName"/>
    <xsl:variable name="personList" select="for $e in $bibl/child::*[local-name() = $tagName][not(preceding-sibling::*) or not(preceding-sibling::*[local-name() != $tagName])] return $e"/>
    <xsl:value-of select="normalize-space(string-join($personList, ' '))"/>
  </xsl:function>
  
<!--  Figures with egXML or eg. -->
  <xsl:template match="figure[teix:egXML or eg]">
    <xsl:variable name="current" select="."/>
    <xsl:variable name="referenceNode" select="$docRoot//figure[teix:egXML or eg][deep-equal((@*|node()), $current/(@*|node()))]"/>
    <xsl:variable name="exampleNum" select="count($referenceNode/preceding::figure[teix:egXML or eg]) + 1"/>
    <xsl:apply-templates select="teix:egXML | eg"/>
    <xsl:for-each select="head">
      <text:p text:style-name="{if (following-sibling::head) then 'teiTableFigureCaptionFirst' else 'teiTableFigureCaptionLast'}"><xsl:if test="not(matches(., '^[Ee]xample')) and not(@type='license')"><xsl:value-of select="concat(local:capitalize(i18n:key('example-label'), true()), ' ', $exampleNum, '. ')"/></xsl:if><xsl:apply-templates select="*|text()"/><xsl:call-template name="punctuate-head"/></text:p>
    </xsl:for-each>
    
  </xsl:template>
  
<!--  Figures with graphics. -->
  <xsl:template match="figure[graphic]">
    <xsl:variable name="current" select="."/>
    <xsl:variable name="referenceNode" select="$docRoot//figure[graphic][deep-equal((@*|node()), $current/(@*|node()))]"/>
    <xsl:variable name="graphicNum" select="count($referenceNode/preceding::figure[graphic]) + 1"/>
    
    <xsl:variable name="graphic" select="graphic"/>
    <text:p text:style-name="teiFigure">
      <xsl:if test="@xml:id">
        <text:bookmark text:name="{@xml:id}"/>
      </xsl:if>
<!--      We have to figure out an optimal height and width.  -->
      <xsl:variable name="pixelWidth" select="xs:integer(substring-before($graphic/@width, 'px'))"/>
      <xsl:variable name="pixelHeight" select="xs:integer(substring-before($graphic/@height, 'px'))"/>
      <xsl:variable name="proportions" select="$pixelWidth div $pixelHeight"/>
      <draw:frame draw:style-name="teiGraphic1" draw:name="graphic_{$graphicNum}" text:anchor-type="as-char" draw:z-index="0">
        <xsl:choose>
<!--      The ideal situation is that an image which is wider 
          than it is tall should simply be set to full
          page width, because that will always fit on a page. 
          When the image is low-res, though, we need to 
          make it smaller. -->
          <xsl:when test="$pixelWidth ge $pixelHeight and $pixelWidth lt 1000">
            <xsl:variable name="imgWidth" select="round($pixelWidth div 60)"/>
            <xsl:attribute name="svg:width"><xsl:value-of select="$imgWidth"/>cm</xsl:attribute>
            <xsl:attribute name="svg:height"><xsl:value-of select="format-number($imgWidth div $proportions, '#')"/>cm</xsl:attribute>
          </xsl:when>
          <xsl:when test="$pixelWidth ge $pixelHeight">
            <xsl:attribute name="svg:width">16cm</xsl:attribute>
            <xsl:attribute name="svg:height"><xsl:value-of select="format-number(16 div $proportions, '#')"/>cm</xsl:attribute>
          </xsl:when>
<!--      When the proportions are reversed, we have some thinking to do.
          Let's assume we want a maximum height of 18cm (which leaves enough
          space for margins and for the figure caption).
          This results in poor rendering of low-res images, so we need to 
          add an approach like the one above. -->
          <xsl:when test="$pixelHeight lt 1000">
            <xsl:attribute name="svg:height">12cm</xsl:attribute>
            <xsl:attribute name="svg:width"><xsl:value-of select="format-number(18 * $proportions, '#')"/>cm</xsl:attribute>
          </xsl:when>
          <xsl:otherwise>
            <xsl:attribute name="svg:height">18cm</xsl:attribute>
            <xsl:attribute name="svg:width"><xsl:value-of select="format-number(18 * $proportions, '#')"/>cm</xsl:attribute>
          </xsl:otherwise>
        </xsl:choose>
        
        <xsl:variable name="href">
          <xsl:for-each select="graphic">
            <xsl:variable name="imagetype" select="tokenize(@url,'\.')[last()]"/>
            <xsl:text>Pictures/resource</xsl:text>
            <xsl:value-of select="$graphicNum"/>
            <xsl:text>.</xsl:text>
            <xsl:value-of select="$imagetype"/>
          </xsl:for-each>
        </xsl:variable>
        <draw:image xlink:href="{$href}" xlink:type="simple" xlink:show="embed" xlink:actuate="onLoad"/>
<!--        We are abandoning FODT for simplicity's sake. -->
        
       <!-- 
          <xsl:choose>
              <xsl:when test="$outputFormat = 'fodt'">
                  <draw:image>
                      <office:binary-data><xsl:value-of select="local:getGraphicAsBase64(resolve-uri(graphic/@url, base-uri(//TEI[1])))"/></office:binary-data>
                  </draw:image>
              </xsl:when>
              <xsl:when test="$outputFormat = 'odt'">
                  <draw:image xlink:href="Pictures/{tokenize($graphic/@url, '/')[last()]}" xlink:type="simple" xlink:show="embed" xlink:actuate="onLoad"/>
              </xsl:when>
          </xsl:choose>
        -->
        
    </draw:frame>
    </text:p>
    <!--        We have to deal with a figure captions manually and put them after the graphic. -->
    <xsl:for-each select="head">
      <text:p text:style-name="{if (following-sibling::head) then 'teiTableFigureCaptionFirst' else 'teiTableFigureCaptionLast'}"><xsl:if test="not(matches(., '^[Ff]igure')) and not(@type='license')"><xsl:value-of select="concat(local:capitalize(i18n:key('figure-label'), true()), ' ', $graphicNum, '. ')"/></xsl:if><xsl:apply-templates select="*|text()"/><xsl:call-template name="punctuate-head"/></text:p>
    </xsl:for-each>
  </xsl:template>
  
<!--    Lists of various kinds. -->
<!-- [RvdB] added preprocessing step, which just copies the list, but wraps all contents of <item> in <p> prior to further processing -->
  <xsl:template match="list">
    <xsl:param name="note.counter" tunnel="yes" as="xs:integer" select="0"/>
    <xsl:param name="note.context" select="ancestor::*[self::front|self::body|self::back]" tunnel="yes" as="element()?"/>
    <xsl:variable name="current" select="."/>
    <xsl:variable name="prepared">
      <xsl:apply-templates select="." mode="list-prepare"/>
    </xsl:variable>
    <xsl:for-each select="$prepared/list">
      <xsl:choose>
        <xsl:when test="matches(@rend, 'inline')">
          <xsl:for-each select="item">
            <xsl:choose>
              <xsl:when test="matches(parent::list/@rend, 'bulleted')"><text:s/>•<text:s/><xsl:apply-templates><xsl:with-param name="note.counter" select="$note.counter + ($current/preceding::tei:note[1]/local:get.note.nr(.), 0)[1]" tunnel="yes"/><xsl:with-param name="note.context" select="$note.context" tunnel="yes"/></xsl:apply-templates></xsl:when>
              <xsl:when test="matches(parent::list/@rend, 'ordered')">
                <xsl:variable name="number"><xsl:number level="multiple" format="1.a"/></xsl:variable>
                <text:s/><xsl:value-of select="concat('(', replace($number, '\.', ''), ')')"/><text:s/><xsl:apply-templates><xsl:with-param name="note.counter" select="$note.counter + ($current/preceding::tei:note[1]/local:get.note.nr(.), 0)[1]" tunnel="yes"/><xsl:with-param name="note.context" select="$note.context" tunnel="yes"/></xsl:apply-templates></xsl:when>
              <xsl:otherwise><xsl:if test="preceding-sibling::item"><text:s/></xsl:if><xsl:apply-templates><xsl:with-param name="note.counter" select="$note.counter + ($current/preceding::tei:note[1]/local:get.note.nr(.), 0)[1]" tunnel="yes"/><xsl:with-param name="note.context" select="$note.context" tunnel="yes"/></xsl:apply-templates></xsl:otherwise>
            </xsl:choose>
          </xsl:for-each>
        </xsl:when>
        <xsl:otherwise>
          <text:list text:style-name="{if (@rend='bulleted') then 'teiListBulleted' else if (@rend='ordered') then 'teiListOrdered' else 'teiListSimple'}"><xsl:apply-templates><xsl:with-param name="note.counter" select="$note.counter + ($current/preceding::tei:note[1]/local:get.note.nr(.), 0)[1]" tunnel="yes"/><xsl:with-param name="note.context" select="$note.context" tunnel="yes"/></xsl:apply-templates></text:list>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>
  
  <!-- [RvdB] wrap all contents of <item> in <p> prior to further processing -->
  <xsl:template match="list[not(matches(@rend, 'inline'))]/item" mode="list-prepare">
    <xsl:variable name="current" select="."/>
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:for-each-group select="node()" group-starting-with="node()[self::p]">
        <xsl:choose>
          <xsl:when test="current-group()[1][not(self::p)]">
            <xsl:call-template name="p.create">
              <xsl:with-param name="context" select="current-group()"/>
              <xsl:with-param name="current" select="$current"/>
            </xsl:call-template>
          </xsl:when>
          <xsl:otherwise>
            <xsl:copy-of select="current-group()[1]"/>
            <xsl:if test="current-group()[position() > 1]">
              <xsl:call-template name="p.create">
                <xsl:with-param name="context" select="current-group()[position() > 1]"/>
                <xsl:with-param name="current" select="$current"/>
              </xsl:call-template>
            </xsl:if>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:for-each-group>
    </xsl:copy>
  </xsl:template>
  
  <!-- [RvdB] in first list pass, just copy the element but process punctuation already 
       (otherwise, node context gets lost, which disturbs punctuation processing) -->
  <xsl:template match="quote[not(parent::cit)] | q[not(parent::cit)] | title[@level='a'] | title[@level='u'] | soCalled" mode="list-prepare">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()" mode="#current"/>
      <xsl:call-template name="include.punctuation"/>
    </xsl:copy>
  </xsl:template>
  
  <!-- [RvdB] in first list pass, just copy the element but process punctuation already 
       (otherwise, node context gets lost, which disturbs punctuation processing) -->
  <xsl:template match="note" mode="list-prepare">
    <xsl:call-template name="include.punctuation"/>
    <xsl:copy>
      <xsl:apply-templates select="@*|node()" mode="#current"/>
    </xsl:copy>
  </xsl:template>

  <!-- [RvdB] in first list pass, process punctuation already  for text nodes
       (otherwise, node context gets lost, which disturbs punctuation processing) -->  
  <xsl:template match="text()" mode="list-prepare" priority="1">
    <xsl:apply-imports/>
  </xsl:template>
  
  <xsl:template match="@*|node()" mode="list-prepare">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()" mode="#current"/>
    </xsl:copy>
  </xsl:template>
    
    <xsl:template match="list/head">
      <text:list-header><text:p text:style-name="teiListHead"><xsl:apply-templates/><xsl:call-template name="punctuate-head"/></text:p></text:list-header>
    </xsl:template>
  
<!--  Suppress the label element, because we handle it inside the item. -->
  <xsl:template match="list/label"/>
    
  <xsl:template match="list/item">
    <text:list-item>
      <xsl:for-each select="preceding-sibling::*[1]/self::label">
        <text:p text:style-name="teiListItem">
          <text:span text:style-name="teiListItemLabel"><xsl:apply-templates/></text:span>
        </text:p>
      </xsl:for-each>
      <xsl:choose>
<!-- If there are nested block lists, things get nasty. -->
        <xsl:when test="child::list[not(matches(@rend, 'inline'))]">
<!-- We have to split the content into pieces which are block lists 
          and non-block-lists. Then we process the former as 
          normal, and the latter within text:p elements. -->
<!--        First we output the first block before the first interrupting element. -->
          <text:p text:style-name="teiListItem">
            <xsl:apply-templates select="(*|text())[not(self::list[not(matches(@rend, 'inline'))] or preceding-sibling::list[not(matches(@rend, 'inline'))])]"/>
          </text:p>
          <!--        Now we work through each list and its following bits. -->
          <xsl:for-each select="list[not(matches(@rend, 'inline'))]">
            <xsl:variable name="pos" select="position()"/>
            <xsl:apply-templates select="."/>
            <text:p text:style-name="teiListItem">
              <xsl:apply-templates select="(*|text())[not(self::list[not(matches(@rend, 'inline'))]) and count(preceding-sibling::list[not(matches(@rend, 'inline'))]) = $pos]"/>
            </text:p>
          </xsl:for-each>
        </xsl:when>
        <!-- Otherwise, things are relatively straightforward. -->
        <xsl:otherwise>
          <!-- [RvdB] just process <p> children as regular paragraphs -->
          <xsl:apply-templates/>
        </xsl:otherwise>
      </xsl:choose>
    </text:list-item>
  </xsl:template>

  <xsl:template match="lb">
    <text:line-break/>
  </xsl:template>
    
<!--    Catch-all: just apply-templates if you don't know what to do. -->
    <xsl:template match="*" priority="-1">
        <xsl:apply-templates/>
    </xsl:template>
    
    
    <!-- Utility functions. -->
    <xsl:function name="local:isSelfClosing" as="xs:boolean">
      <xsl:param name="el" as="node()"/>
        <xsl:variable name="tagName" select="local-name($el)"/>
        <xsl:value-of select="$tagName = ('lb', 'pb', 'cb') or xs:string($el) = ''"/>
    </xsl:function>
    
    
<!--   Processing of images. 
      We have to pre-process images into base64 format so that we can 
      embed them in the output file. This is done before processing anything
      else.
   -->
  <xsl:function name="local:getGraphicAsBase64" as="xs:string">
    <xsl:param name="graphicUrl" as="xs:string"/>
      <xsl:message>Processing image <xsl:value-of select="$graphicUrl"/>...</xsl:message>
<!--      Let's see if we can find the actual file. 
         This is quite messy and not expected to
         work on non-*nix file systems. -->
      <xsl:variable name="filePath" select="replace(replace(resolve-uri($graphicUrl), '%20', ' '), 'file:', '')"/>
      <xsl:message>File is deemed to be at: <xsl:value-of select="$filePath"/></xsl:message>
      <xsl:value-of select="expath-file:read-binary($filePath)"/>
  </xsl:function>
  
</xsl:stylesheet>

    
