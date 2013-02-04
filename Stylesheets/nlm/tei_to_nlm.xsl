<?xml version="1.0"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:exist="http://exist.sourceforge.net/NS/exist" xmlns="" xpath-default-namespace="http://www.tei-c.org/ns/1.0" xmlns:mml="http://www.w3.org/1998/Math/MathML" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" exclude-result-prefixes="#all">

  <!--
        Written by Martin Holmes, University of Victoria Humanities Computing and 
    Media Centre, beginning in 2008.
    
    This file is released under the Mozilla Public Licence version 1.1 (MPL 1.1).
    
    This transformation is designed to convert the limited subset of TEI elements
    used in the teiJournal project into an NLM file. NLM is:
    
    The National Center for Biotechnology Information (NCBI) of the National Library of
    Medicine (NLM) created the Journal Archiving and Interchange Tag Suite.
    
    Our target is actually the NLM Journal Publishing Tag Set, described here:
    
    http://dtd.nlm.nih.gov/publishing/tag-library/3.0/index.html
    
    The reason for creating NLM conversion is that Open Journal Systems has committed 
    to supporting NLM, so this provides a method of migrating data from teiJournal 
    to OJS.
    
    -->
  <xsl:output method="xml" doctype-public="-//NLM//DTD Journal Publishing DTD v3.0 20080202//EN" doctype-system="http://dtd.nlm.nih.gov/publishing/3.0/journalpublishing3.dtd" xpath-default-namespace="" indent="yes"></xsl:output>

  <xsl:template match="/">
    <article dtd-version="3.0" xmlns:mml="http://www.w3.org/1998/Math/MathML" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
      <xsl:if test="TEI/@xml:lang">
        <xsl:copy-of select="TEI/@xml:lang"></xsl:copy-of>
      </xsl:if>
<!-- We have an ontology of TEI contribution types; this does not match 
     NLM at all. In NLM 3.0, the list of article-type values is suggested but 
     not fixed, so we can use those which are appropriate, and use our own 
     where there's no match. In 2.3, the list is fixed, so we'd have to convert 
     all our non-matching ones to "other". -->
        <xsl:choose>
          <xsl:when test="TEI/@rend='article'">
  <xsl:attribute name="article-type">research-article</xsl:attribute>
</xsl:when>
<xsl:when test="TEI/@rend='review_article'">
  <xsl:attribute name="article-type">review-article</xsl:attribute>
</xsl:when>
<xsl:when test="TEI/@rend='fiction'">
  <xsl:attribute name="article-type">fiction</xsl:attribute>
</xsl:when>
<xsl:when test="TEI/@rend='poetry'">
  <xsl:attribute name="article-type">poetry</xsl:attribute>
</xsl:when>
<xsl:when test="TEI/@rend='review'">
  <xsl:attribute name="article-type">book-review</xsl:attribute>
</xsl:when>
<xsl:when test="TEI/@rend='books_received'">
  <xsl:attribute name="article-type">books-received</xsl:attribute>
</xsl:when>
<xsl:when test="TEI/@rend='note'">
  <xsl:attribute name="article-type">brief-report</xsl:attribute>
</xsl:when>
<xsl:when test="TEI/@rend='discussion'">
  <xsl:attribute name="article-type">discussion</xsl:attribute>
</xsl:when>
<xsl:when test="TEI/@rend='editorial'">
  <xsl:attribute name="article-type">editorial</xsl:attribute>
</xsl:when>
<xsl:when test="TEI/@rend='preface'">
  <xsl:attribute name="article-type">editorial</xsl:attribute>
</xsl:when>
<xsl:when test="TEI/@rend='column'">
  <xsl:attribute name="article-type">discussion</xsl:attribute>
</xsl:when>
<xsl:when test="TEI/@rend='afterword'">
  <xsl:attribute name="article-type">editorial</xsl:attribute>
</xsl:when>
<xsl:when test="TEI/@rend='announcement'">
  <xsl:attribute name="article-type">announcement</xsl:attribute>
</xsl:when>
<xsl:when test="TEI/@rend='corrigendum'">
  <xsl:attribute name="article-type">correction</xsl:attribute>
</xsl:when>
<xsl:when test="TEI/@rend='advertisement'">
  <xsl:attribute name="article-type">announcement</xsl:attribute>
</xsl:when>
<xsl:when test="TEI/@rend='doc_list'">
  <xsl:attribute name="article-type">collection</xsl:attribute>
</xsl:when>
<xsl:when test="TEI/@rend='site_page'">
  <xsl:attribute name="article-type">editorial</xsl:attribute>
</xsl:when>
<xsl:when test="TEI/@rend='obituary'">
  <xsl:attribute name="article-type">obituary</xsl:attribute>
</xsl:when>
          <xsl:otherwise><xsl:attribute name="article-type">other</xsl:attribute></xsl:otherwise>
        </xsl:choose>
        
      
      <!--      This is the metadata block, essentially. -->
      <xsl:element name="front">

        <!-- Here we dig through the teiHeader to extract what we need to create journal metadata
     and article metadata. First, journal metadata: -->
        <xsl:element name="journal-meta">
          <journal-id journal-id-type="publisher">
          <xsl:choose>
          <xsl:when test="TEI/teiHeader[1]/fileDesc[1]/publicationStmt[1]/publisher[1]/choice[1]/abbr[1]">
            
              <xsl:value-of select="TEI/teiHeader[1]/fileDesc[1]/publicationStmt[1]/publisher[1]/choice[1]/abbr[1]"></xsl:value-of>
            
          </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="TEI/teiHeader[1]/fileDesc[1]/publicationStmt[1]/publisher[1]"></xsl:value-of>
            </xsl:otherwise>
          </xsl:choose>
            </journal-id>
          <issn>
            <xsl:value-of select="TEI/teiHeader[1]/fileDesc[1]/seriesStmt[1]/idno[@type='issn']"></xsl:value-of>
          </issn>
          <publisher>
            <publisher-name>
              <xsl:choose>
                <xsl:when test="TEI/teiHeader[1]/fileDesc[1]/publicationStmt[1]/publisher[1]/choice/abbr">
                  <xsl:value-of select="TEI/teiHeader[1]/fileDesc[1]/publicationStmt[1]/publisher[1]/choice/abbr"></xsl:value-of>
                  <xsl:text> (</xsl:text>
                  <xsl:apply-templates select="TEI/teiHeader[1]/fileDesc[1]/publicationStmt[1]/publisher[1]/choice/expan"></xsl:apply-templates>
                  <xsl:text>)</xsl:text>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:value-of select="TEI/teiHeader[1]/fileDesc[1]/publicationStmt[1]/publisher[1]"></xsl:value-of>
                </xsl:otherwise>
              </xsl:choose>
            </publisher-name>
          </publisher>

        </xsl:element>

        <!--        Next, article metadata. -->
        <xsl:element name="article-meta">
          <article-id pub-id-type="other">
            <xsl:value-of select="TEI/@xml:id"></xsl:value-of>
          </article-id>
          <article-categories>
            <subj-group>
              <subject><xsl:value-of select="TEI/@rend" /></subject>
            </subj-group>
          </article-categories>
          
          <title-group>
            <article-title><xsl:apply-templates select="TEI/teiHeader[1]/fileDesc[1]/titleStmt[1]/title[@type='main']" /></article-title>
            <subtitle><xsl:apply-templates select="TEI/teiHeader[1]/fileDesc[1]/titleStmt[1]/title[@type='sub']" /></subtitle>
            <alt-title alt-title-type="runningRecto"><xsl:apply-templates select="TEI/teiHeader[1]/fileDesc[1]/titleStmt[1]/title[@type='runningRecto']" /></alt-title>
            <alt-title alt-title-type="runningVerso"><xsl:apply-templates select="TEI/teiHeader[1]/fileDesc[1]/titleStmt[1]/title[@type='runningVerso']" /></alt-title>
          </title-group>
          
          <contrib-group>
            <xsl:for-each select="TEI/teiHeader[1]/fileDesc[1]/titleStmt[1]/author">
              <contrib contrib-type="author">
                <xsl:apply-templates select="name"></xsl:apply-templates>
                <aff><xsl:apply-templates select="name/affiliation" /></aff>
              </contrib>
            </xsl:for-each>
          </contrib-group>
          
          <xsl:for-each select="TEI/teiHeader[1]/fileDesc[1]/seriesStmt[1]/respStmt">
            <contrib-group>
              <xsl:for-each select="name">
                <contrib>
                  <xsl:attribute name="contrib-type" select="../resp"></xsl:attribute>
                  <xsl:apply-templates select="." />
                </contrib>
              </xsl:for-each>
            </contrib-group>
          </xsl:for-each>

          <xsl:call-template name="dateToDMYTags"><xsl:with-param name="inDate" select="TEI/teiHeader[1]/fileDesc[1]/publicationStmt[1]/date[1]/@when" /><xsl:with-param name="outTagName">pub-date</xsl:with-param></xsl:call-template>
          
          <volume><xsl:apply-templates select="TEI/teiHeader[1]/fileDesc[1]/seriesStmt[1]/idno[@type='vol']" /></volume>
          
          <issue><xsl:apply-templates select="TEI/teiHeader[1]/fileDesc[1]/seriesStmt[1]/idno[@type='issue']" /></issue>
          
          <elocation-id><xsl:value-of select="TEI/teiHeader[1]/fileDesc[1]/publicationStmt[1]/idno[@type='itemNo']/@n" /></elocation-id>

<!-- If there is an abstract, copy it in here for the sake of completeness. -->
          <xsl:if test="TEI/text/body/div[@type='abstract']">
            <abstract>
              <xsl:apply-templates select="TEI/text/body/div[@type='abstract']/*" />
            </abstract>
          </xsl:if>

<!--        Right now, profilDesc only contains keywords, but other templates 
            might be added later. -->
        <xsl:apply-templates select="TEI/teiHeader/profileDesc"></xsl:apply-templates>



        </xsl:element>

      </xsl:element>

      <!--Here's the meat of the document. -->
      <xsl:element name="body">

        <xsl:apply-templates select="TEI/text/body"></xsl:apply-templates>

      </xsl:element>

      <!--      This will contain the appendices, reference list, etc. -->

      <xsl:element name="back">
<!-- Appendices first. -->
        <xsl:if test="TEI/text/back/div[@type='appendix']">
          <xsl:element name="app-group">
            <xsl:for-each select="TEI/text/back/div[@type='appendix']">
              <xsl:element name="app">
                <xsl:if test="@xml:id">
                  <xsl:attribute name="id" select="@xml:id" />
                </xsl:if>
                <xsl:apply-templates select="./*" />
              </xsl:element>
            </xsl:for-each>
          </xsl:element>
        </xsl:if>
        
<!-- Reference list        -->
        <xsl:if test="TEI/text/back/div[@type='bibliogr']">
          <xsl:variable name="bibl" select="TEI/text/back/div[@type='bibliogr']"></xsl:variable>
          <xsl:element name="ref-list">
<!-- If there's no title tag, we have to add an empty one. -->
            <title><xsl:value-of select="$bibl/head" /></title>
<!-- Now process each of the reference items. -->
            <xsl:for-each select="$bibl/listBibl/biblStruct">
              <xsl:apply-templates select="." />
            </xsl:for-each>
          </xsl:element>
        </xsl:if>
      </xsl:element>
    </article>

  </xsl:template>
  
<!-- Handling of items in profileDesc. -->
  <xsl:template match="profileDesc"><xsl:apply-templates /></xsl:template>
  <xsl:template match="textClass"><xsl:apply-templates /></xsl:template>
  <xsl:template match="keywords">
    <xsl:element name="kwd-group">
      <xsl:if test="@scheme">
        <xsl:attribute name="kwd-group-type" select="@scheme" />
      </xsl:if>
      <xsl:for-each select="list/item">
        <xsl:element name="kwd">
          <xsl:value-of select="." />
        </xsl:element>
      </xsl:for-each>
    </xsl:element>
  </xsl:template>
  
  <xsl:template name="dateToDMYTags">
    <xsl:param name="inDate" />
    <xsl:param name="outTagName" />
    <xsl:element name="{$outTagName}">
      <xsl:variable name="frags" select="tokenize($inDate, '-')" />
      <xsl:if test="count($frags) gt 2">
        <day><xsl:value-of select="$frags[3]" /></day>
      </xsl:if>
      <xsl:if test="count($frags) gt 1">
        <month><xsl:value-of select="$frags[2]" /></month>
      </xsl:if>
      <xsl:if test="count($frags) gt 0">
        <year><xsl:value-of select="$frags[1]" /></year>
      </xsl:if>
    </xsl:element>
  </xsl:template>
  
<!-- Block-level templates. -->
<!--  Document divisions/sections. -->
  <xsl:template match="div">
    <xsl:element name="sec">
<!-- If there's no title tag, we have to add an empty one. -->
      <xsl:if test="not(child::head)"><title></title></xsl:if>
      <xsl:apply-templates />
    </xsl:element>
  </xsl:template>
  
  <xsl:template match="div/head">
    <xsl:element name="title">
      <xsl:apply-templates />
    </xsl:element>
  </xsl:template>
  
  <xsl:template match="p | ab">
    <xsl:element name="p">
      <xsl:apply-templates />
    </xsl:element>
  </xsl:template>
  
  <xsl:template match="list">
    <xsl:element name="list">
      <xsl:if test="@type">
        <xsl:attribute name="list-type" select="@type" />
      </xsl:if>
      <xsl:apply-templates />
    </xsl:element>
  </xsl:template>
  
  <xsl:template match="list/item">
    <xsl:element name="list-item">
      <p>
        <xsl:apply-templates />
      </p>
    </xsl:element>
  </xsl:template>
  
    <!-- Tables are simple for the moment, but they'll get complicated. -->
    <xsl:template match="table">
      <xsl:element name="table-wrap">
        <xsl:if test="@rend">
          <xsl:attribute name="specific-use" select="@rend"/>
        </xsl:if>
        <xsl:if test="head">
          <xsl:element name="caption">
            <xsl:element name="p">
              <xsl:apply-templates select="head"/>
            </xsl:element>
          </xsl:element>
          </xsl:if>
        <xsl:element name="table">
          <xsl:if test="@xml:id"><xsl:attribute name="id" select="@xml:id" /></xsl:if>
            <xsl:apply-templates/>
        </xsl:element>
      </xsl:element>
    </xsl:template>
    <xsl:template match="table/row">
        <xsl:element name="tr">
            <xsl:apply-templates/>
        </xsl:element>
    </xsl:template>
    <xsl:template match="table/row/cell">
      <xsl:choose>
        <xsl:when test="parent::row/@role='label'">
          <xsl:element name="th">
            <xsl:apply-templates />
          </xsl:element>
        </xsl:when>
        <xsl:otherwise>
          <xsl:element name="td">
            <xsl:apply-templates />
          </xsl:element>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:template>
  
<!-- Suppress handling of the table caption (head), because the NLM model moves it 
     into the parent table-wrap element, so we have to handle it at that point
     (above). -->
  <xsl:template match="table/head"></xsl:template>
  
<!--  Low-level element templates. -->
  
<!-- Copy xml:id to id attribute in output. -->
  <xsl:template match="@xml:id"><xsl:attribute name="id" select="." /></xsl:template>
  
<!-- Copy xml:lang to output. -->
  <xsl:template match="@xml:lang"><xsl:copy-of select="." /></xsl:template>
  
<!-- Quotations. Our TEI model is more sophisticated that NLM here; we need 
to simplify a bit. -->
  <xsl:template match="cit">
    <xsl:if test="quote">
      <xsl:element name="disp-quote">
        <xsl:if test="@rend">
          <xsl:attribute name="specific-use" select="@rend" />
        </xsl:if>
<!-- If the quote is already divided into paragraphs, we can simply process
     it; if not, we must supply a p tag, because NLM requires a block tag 
     inside disp-quote. -->
        <xsl:choose>
          <xsl:when test="quote/p">
            <xsl:apply-templates select="quote" />
          </xsl:when>
          <xsl:otherwise>
            <xsl:element name="p">
              <xsl:apply-templates select="quote" />
            </xsl:element>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:if test="@rend='block' and ref">
          <xsl:element name="attrib">
            <xsl:apply-templates select="ref" />
          </xsl:element>
        </xsl:if>
      </xsl:element>
    </xsl:if>
    <xsl:if test="not(@rend='block') and ref">
       <xsl:apply-templates select="ref" />
    </xsl:if>
  </xsl:template>
  
  <xsl:template match="ref">
<!-- For ref elements, we need to distinguish between internal and external 
because NLM handles them differently. -->
    <xsl:choose>
<!-- Internal reference. -->
      <xsl:when test="starts-with(@target, '#')">
        <xsl:element name="xref">
          <xsl:variable name="targId" select="substring-after(@target, '#')" />
          <xsl:attribute name="rid" select="$targId" />
<!-- If it's a reference to a biblStruct element in the back matter, then it should 
be given the recommended @ref-type. -->
          <xsl:if test="//back//biblStruct[@xml:id = $targId]"><xsl:attribute name="ref-type">bibr</xsl:attribute></xsl:if>
          
<!-- We have to be careful of embedded tags, because xref has a very 
     impoverished content model. -->
          <xsl:apply-templates />
        </xsl:element>
      </xsl:when>
<!-- External reference. -->
      <xsl:otherwise>
        <xsl:element name="ext-link">
          <xsl:attribute name="xlink:href" select="@target" />
<!-- We have to be careful of embedded tags, because ext-link has a very 
     impoverished content model. -->
          <xsl:apply-templates />
        </xsl:element>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
<!-- Abbreviations.  -->
  <xsl:template match="choice[abbr]">
    <xsl:apply-templates select="abbr"></xsl:apply-templates>
  </xsl:template>
  
  <xsl:template match="abbr">
    <xsl:choose>
      <xsl:when test="not(ancestor::ref)">
        <xsl:element name="abbrev">
        <xsl:apply-templates />
        <xsl:if test="preceding-sibling::expan"><def><p><xsl:apply-templates select="preceding-sibling::expan[1]" /></p></def></xsl:if>
        <xsl:if test="following-sibling::expan"><def><p><xsl:apply-templates select="following-sibling::expan[1]" /></p></def></xsl:if>
        </xsl:element>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="." />
      </xsl:otherwise>
    </xsl:choose>
    
  </xsl:template>
  
  
<!-- Name elements are complicated, because we distinguish them by type, 
     but NLM makes a distinction between the names of people and other 
     types of name. The only appropriate NLM element I can find for the latter
     is named-content. 
     There is a further annoying complication, in that the <name> element 
     in NLM can't show up inside regular body text, apparently.      
-->

  <xsl:template match="name[@type='person' or not(@type)][not(ancestor::div)]">
    <xsl:element name="name">
      <surname><xsl:apply-templates select="surname" /></surname>
      <given-names><xsl:apply-templates select="forename" /></given-names>
    </xsl:element>
  </xsl:template>
  
  <xsl:template match="name[@type='person' or not(@type)][ancestor::div]">
    <xsl:element name="named-content">
      <xsl:attribute name="content-type">person</xsl:attribute>
      <xsl:value-of select="." />
    </xsl:element>
  </xsl:template>
  
  <xsl:template match="affiliation/name[@type='org']">
    <xsl:element name="institution">
      <xsl:value-of select="." />
    </xsl:element>
  </xsl:template>
  
  <xsl:template match="name[@type and not(@type='person')][ancestor::div]">
    <xsl:element name="named-content">
      <xsl:attribute name="content-type" select="@type" />
      <xsl:apply-templates />
    </xsl:element>
  </xsl:template>
  
<!-- Notes (footnotes/endnotes). -->
  <xsl:template match="note">
    <xsl:element name="fn">
      <xsl:choose>
        <xsl:when test="not(child::p)">
          <xsl:element name="p">
            <xsl:apply-templates />
          </xsl:element>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates />
        </xsl:otherwise>
      </xsl:choose>
    </xsl:element>
  </xsl:template>
  
<!-- Figures and graphics. -->
  <xsl:template match="figure">
    <xsl:element name="fig">
      <xsl:if test="@xml:id">
        <xsl:attribute name="id" select="@xml:id" />
      </xsl:if>
      <xsl:apply-templates />
    </xsl:element>  
  </xsl:template>
  
  <xsl:template match="figure/head">
    <caption><title><xsl:apply-templates /></title></caption>
  </xsl:template>
  
  <xsl:template match="graphic">
    <xsl:element name="graphic">
      <xsl:attribute name="xlink:href" select="tei:resolveURI(.,@url)" />
    </xsl:element>
  </xsl:template>
  
<!-- Verse lines and groups. -->
  <xsl:template match="lg">
    <xsl:element name="verse-group">
      <xsl:apply-templates />
    </xsl:element>
  </xsl:template>
  
  <xsl:template match="l">
    <xsl:element name="verse-line">
      <xsl:apply-templates />
    </xsl:element>
  </xsl:template>
  
  
<!-- Bibliographical items. -->
  <xsl:template match="listBibl/biblStruct">
    <ref id="{@xml:id}">
<!-- Now the hard bit. NLM's model is radically different from TEI's. -->
      <element-citation publication-type="{@type}">
        
<!-- We're going to take the approach of picking out the information we 
     need from where we expect to find it, rather than trying to apply 
     templates in some generic way, because the structured nature of 
     TEI biblStructs is so different from the rather looser structure 
     of NLM. -->
        
<!-- First we look for authors. These might be in the analytic or in the 
monogr; try analytic first. -->
        <xsl:if test="analytic">
<!-- In this case, we should be able to get both title and authors from here. -->
          <person-group person-group-type="author">
            <xsl:for-each select="analytic/author/name">
              <name>
                <surname><xsl:apply-templates select="surname" /></surname>
                <given-names><xsl:apply-templates select="forename" /></given-names>
              </name>
            </xsl:for-each>
          </person-group>
<!-- Now we can get the title from here. NLM has an article-title element, which 
     will do for journal articles; it uses chapter-title for book chapters. -->
          <xsl:choose>
            <xsl:when test="@type='book'"><chapter-title><xsl:apply-templates select="analytic/title" /></chapter-title></xsl:when>
            <xsl:otherwise><article-title><xsl:apply-templates select="analytic/title" /></article-title></xsl:otherwise>
          </xsl:choose> 
        </xsl:if>
<!-- Now we look for additional information in the monogr element. -->
        <xsl:if test="monogr">
<!-- If there's a monogr title element, it could be the level m or j (books and 
journals), or it could be s (series title). We need to handle each separately.-->
          <xsl:for-each select="monogr/title[@level='m' or @level='u' or @level='j']">
            <source><xsl:value-of select="." /></source>
          </xsl:for-each>
<!-- Series titles. -->
          <xsl:for-each select="monogr/title[@level='s']">
            <series><xsl:value-of select="." /></series>
          </xsl:for-each>
<!-- Now we should look for authors and editors at the monogr level. -->
          <xsl:if test="monogr/author">
            <person-group person-group-type="author">
              <xsl:for-each select="monogr/author/name">
                <name>
                  <surname><xsl:apply-templates select="surname" /></surname>
                  <given-names><xsl:apply-templates select="forename" /></given-names>
                </name>
              </xsl:for-each>
            </person-group>
          </xsl:if>
          <xsl:if test="monogr/editor">
            <person-group person-group-type="editor">
              <xsl:for-each select="monogr/editor/name">
                <name>
                  <surname><xsl:apply-templates select="surname" /></surname>
                  <given-names><xsl:apply-templates select="forename" /></given-names>
                </name>
              </xsl:for-each>
            </person-group>
            <xsl:if test="edition">
              <edition><xsl:value-of select="edition" /></edition>  
            </xsl:if>
          </xsl:if>
          
<!-- In TEI, idno elements are used for a variety of tasks. -->
          <xsl:for-each select="monogr/idno">
            <xsl:choose>
              <xsl:when test="@type='ISSN'">
                <issn><xsl:value-of select="." /></issn>
              </xsl:when>
              <xsl:when test="@type='ISBN'">
                <isbn><xsl:value-of select="." /></isbn>
              </xsl:when>
            </xsl:choose>
          </xsl:for-each>
          
<!-- Now we get the publication information from the imprint element. This can 
     be handled with more conventional one-to-one mapping, so templates will do.-->
          <xsl:apply-templates select="monogr/imprint" />
        </xsl:if>
<!-- We should process any respStmt elements in analytic or monogr. -->
          <xsl:apply-templates select="descendant::respStmt">
          </xsl:apply-templates>
      </element-citation>
    </ref>
  </xsl:template>
  
<!-- These are mini-templates for the content of the imprint element in a 
biblStruct. -->
  <xsl:template match="biblStruct/monogr/imprint">
    <xsl:apply-templates />
  </xsl:template>
  
  <xsl:template match="publisher">
    <publisher-name><xsl:apply-templates /></publisher-name>
  </xsl:template>
  
  <xsl:template match="pubPlace">
    <publisher-loc><xsl:apply-templates /></publisher-loc>
  </xsl:template>
  
  <xsl:template match="biblStruct/monogr/imprint/date">
    <year><xsl:value-of select="substring(@when, 1, 4)" /></year>
    <xsl:if test="string-length(@when) gt 6">
      <month><xsl:value-of select="substring(@when, 6, 2)" /></month>
      <xsl:if test="string-length(@when) gt 9">
        <day><xsl:value-of select="substring(@when, 9, 2)" /></day>
      </xsl:if>
    </xsl:if>
  </xsl:template>
  
  <xsl:template match="biblStruct/monogr/imprint/biblScope">
    <xsl:choose>
      <xsl:when test="@type='vol'">
        <volume><xsl:value-of select="." /></volume>
      </xsl:when>
      <xsl:when test="@type='issue'">
        <issue><xsl:value-of select="." /></issue>
      </xsl:when>
      <xsl:when test="@type='pp'">
        <fpage><xsl:value-of select="substring-before(., '-')"/></fpage>
        <lpage><xsl:value-of select="substring-after(., '-')"/></lpage>
      </xsl:when>
<!--       ANY MORE??? -->
      <!--<xsl:when test="@type=''"></xsl:when>
      <xsl:when test="@type=''"></xsl:when>
      <xsl:when test="@type=''"></xsl:when>-->
      <xsl:otherwise><comment><xsl:value-of select="." /></comment></xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template match="biblStruct//note">
    <comment><xsl:value-of select="." /></comment>
  </xsl:template>
  
  <xsl:template match="respStmt[ancestor::biblStruct]">
    <person-group person-group-type="{resp}">
      <xsl:for-each select="name">
        <name>
                  <surname><xsl:apply-templates select="surname" /></surname>
                  <given-names><xsl:apply-templates select="forename" /></given-names>
                </name>
      </xsl:for-each>
    </person-group>
  </xsl:template>
  
<!-- The soCalled element has no analogue in NLM; the best we can do 
is probably to use style-content, enabling us to include the quotes and
at the same time explain them. -->
  <xsl:template match="soCalled">
    <styled-content style-type="scare quotes" style="::before: content(open-quote); ::after: content(close-quote);">
      <xsl:apply-templates />
    </styled-content>
  </xsl:template>
  
<!-- The mentioned tag is similarly difficult, as is the tag element, and term. -->
  <xsl:template match="mentioned">
    <styled-content style-type="mentioned" style="font-style: italic;">
      <xsl:apply-templates />
    </styled-content>
  </xsl:template>

  <xsl:template match="tag">
    <styled-content style-type="XML tag" style="::before: content(&lt;); ::after: content(&gt;); font-family: monospace;">
      <xsl:apply-templates />
    </styled-content>
  </xsl:template>
  
  <xsl:template match="term">
    <styled-content style-type="term" style="font-style: italic;">
      <xsl:apply-templates />
    </styled-content>
  </xsl:template>
  
<!-- Line breaks are equivalent to <break/>, but cannot appear in paragraphs 
     What nonsense. -->
  <xsl:template match="lb"><xsl:comment>There should be a line-break here.</xsl:comment></xsl:template>
  
<!-- Inline elements. -->
<!-- There's no standard way to do document titles in the body text in NLM. -->
  <xsl:template match="title[not(ancestor::biblStruct) and not(ancestor::teiHeader)]">
    <named-content content-type="title_level_{@level}">
<!-- That takes care of identifying the title type. Now we might as well 
have a shot at styling it. -->
      <xsl:choose>
        <xsl:when test="@level='m' or @level='j'">
          <italic><xsl:apply-templates /></italic>
        </xsl:when>
        <xsl:when test="@level='a'">
          &quot;<xsl:apply-templates />&quot;
        </xsl:when>
      </xsl:choose>
    </named-content>  
  </xsl:template>
  
<!--  Text style markup. -->
<!--  Bold. -->
  <xsl:template match="hi[@rend='bold']">
    <xsl:element name="bold"><xsl:apply-templates /></xsl:element>
  </xsl:template>
<!--  Italics. -->
  <xsl:template match="hi[@rend='italic']">
    <xsl:element name="italic"><xsl:apply-templates /></xsl:element>
  </xsl:template> 
<!--  Underline. -->
  <xsl:template match="hi[@rend='underline']">
    <xsl:element name="underline"><xsl:apply-templates /></xsl:element>
  </xsl:template>
<!--  Overline. -->
  <xsl:template match="hi[@rend='overline']">
    <xsl:element name="overline"><xsl:apply-templates /></xsl:element>
  </xsl:template>

</xsl:stylesheet>
