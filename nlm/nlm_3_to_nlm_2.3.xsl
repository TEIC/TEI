<?xml version="1.0"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" exclude-result-prefixes="#all">

  <!--
        Written by Martin Holmes, University of Victoria Humanities Computing and 
    Media Centre, beginning in 2008.
    
    This file is released under the Mozilla Public Licence version 1.1 (MPL 1.1).
    
    This transformation is designed to convert NLM 3.0 documents to NLM 2.3;
    it works only with the subset of NLM 3.0 used by the conversion from teiJournal
    documents to NLM 3.0.
    
    NLM is:
    
    The National Center for Biotechnology Information (NCBI) of the National Library of
    Medicine (NLM) created the Journal Archiving and Interchange Tag Suite.
    
    Our source is actually the NLM Journal Publishing Tag Set, described here:
    
    http://dtd.nlm.nih.gov/publishing/tag-library/3.0/index.html
    
    and the target is the older 2.3 version, described here:
    
    http://dtd.nlm.nih.gov/publishing/tag-library/2.3/index.html
    
    The reason for creating NLM conversion is that Open Journal Systems has committed 
    to supporting NLM, so this provides a method of migrating data from teiJournal 
    to OJS; however, OJS will initially support only NLM 2.3.
    
    -->
  <xsl:output method="xml" doctype-public="-//NLM//DTD Journal Publishing DTD v2.3 20070202//EN" doctype-system="http://dtd.nlm.nih.gov/publishing/2.3/journalpublishing.dtd" xpath-default-namespace="" indent="yes"></xsl:output>

  <xsl:template match="/">

       
       <xsl:apply-templates></xsl:apply-templates>
       

  </xsl:template>
  
  <!--    Root element needs its article-type attribute massaging a bit. -->
    <!-- We have an ontology of TEI contribution types; this does not match 
     NLM at all. In NLM 3.0, the list of article-type values is suggested but 
     not fixed, so we can use those which are appropriate, and use our own 
     where there's no match. In 2.3, the list is fixed, so we'd have to convert 
     all our non-matching ones to "other". -->
     <xsl:template match="article">
       <article xmlns:xlink="http://www.w3.org/1999/xlink"
          xmlns:mml="http://www.w3.org/1998/Math/MathML"
          xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
       <xsl:choose>
<xsl:when test="@article-type='fiction'">
  <xsl:attribute name="article-type">other</xsl:attribute>
</xsl:when>
<xsl:when test="@article-type='poetry'">
  <xsl:attribute name="article-type">other</xsl:attribute>
</xsl:when>
          <xsl:otherwise><xsl:attribute name="article-type" select="@article-type" /></xsl:otherwise>
       </xsl:choose>
         <xsl:apply-templates/>
       </article>
     </xsl:template>
  
<!-- The 3.0 element-citation is just citation in 2.3. -->
  <xsl:template match="element-citation">
    <citation>
      <xsl:apply-templates />
    </citation>
  </xsl:template>
  
<!-- The 3.0 element <styled-content> doesn't exist. Each type of content 
will need to be handled separately. -->
  <xsl:template match="styled-content[@style-type='scare quotes']">
    &quot;<xsl:apply-templates/>&quot;
  </xsl:template>
    <xsl:template match="styled-content[@style-type='term' or @style-type='mentioned']">
    <italic><xsl:apply-templates/></italic>
    </xsl:template>
    <xsl:template match="styled-content[@style-type='XML tag']">
    <monospace>&lt;<xsl:apply-templates/>&gt;</monospace>
  </xsl:template>
  
<!-- The @orientation attribute is required in 3.0 and not allowed in 2.3. -->
  <xsl:template match="@orientation"></xsl:template>
  
<!-- @specific-use is not allowed on table-wrap. -->
  <xsl:template match="table-wrap">
    <xsl:element name="table-wrap">
      <xsl:if test="@specific-use">
        <xsl:attribute name="content-type" select="@specific-use" />
      </xsl:if>
      <xsl:apply-templates />
    </xsl:element>
  </xsl:template>
  
<!-- @specific-use is also not allowed on disp-quote. -->
  <xsl:template match="disp-quote">
    <xsl:element name="disp-quote">
      <xsl:if test="@specific-use">
        <xsl:attribute name="content-type" select="@specific-use" />
      </xsl:if>
      <xsl:apply-templates />
    </xsl:element>
  </xsl:template>
  
  
<!-- The person-group/@person-group-type attribute in 2.3 is limited to a 
fixed list of items.  Some of our types will not do. -->
  <xsl:template match="person-group">
    <person-group>
      <xsl:if test="@person-group-type">
        <xsl:if test="@person-group-type='allauthors' or 
                      @person-group-type='assignee' or 
                      @person-group-type='author' or 
                      @person-group-type='compiler' or 
                      @person-group-type='director' or 
                      @person-group-type='editor' or 
                      @person-group-type='guest-editor' or 
                      @person-group-type='inventor' or 
                      @person-group-type='transed' or 
                      @person-group-type='translator'">
            <xsl:copy-of select="@person-group-type"></xsl:copy-of>
          </xsl:if>
      </xsl:if>
      <xsl:apply-templates />
    </person-group>
  </xsl:template>
  
<!-- named-content elements cannot occur inside ext-link. -->
  <xsl:template match="named-content[ancestor::ext-link]">
    <xsl:value-of select="." />
  </xsl:template>
  
<!--  Lastly, a generic copy-all template with low priority, so that stuff not 
      handled above is copied to the output. -->
      <xsl:template match="@*|node()|text()|comment()|processing-instruction()" priority="-1">
        <xsl:copy>
            <xsl:apply-templates select="@*|node()|text()|comment()|processing-instruction()"/>
        </xsl:copy>
    </xsl:template>
  
</xsl:stylesheet>
