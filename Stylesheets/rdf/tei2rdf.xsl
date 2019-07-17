<?xml version="1.0" encoding="utf-8"?>
<XSL:stylesheet xmlns:XSL="http://www.w3.org/1999/XSL/Transform"
                version="2.0"
                xpath-default-namespace="http://www.tei-c.org/ns/1.0">
   <XSL:import href="../tools/getfiles.xsl"/>
   <XSL:import href="crm.xsl"/>
   <XSL:param name="corpus">./</XSL:param>
   <XSL:template match="*">
      <XSL:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
   </XSL:template>
   <XSL:template match="text()|comment()|@*|processing-instruction()"/>
   <XSL:template match="name">
      <XSL:call-template name="teiname"/>
   </XSL:template>
   <XSL:template match="date">
      <XSL:call-template name="E52"/>
   </XSL:template>
   <XSL:template match="author">
      <XSL:call-template name="E65"/>
   </XSL:template>
   <XSL:template match="title">
      <XSL:call-template name="E35"/>
   </XSL:template>
   <XSL:template match="teiCorpus">
      <XSL:call-template name="TEI"/>
   </XSL:template>
   <XSL:template match="fileDesc">
      <XSL:call-template name="F24"/>
   </XSL:template>
   <XSL:template match="publicationStmt">
      <XSL:call-template name="F30"/>
   </XSL:template>
   <XSL:template match="TEI">
      <XSL:call-template name="TEI"/>
   </XSL:template>
   <XSL:template match="text">
      <XSL:call-template name="E31"/>
   </XSL:template>
   <XSL:template match="persName">
      <XSL:call-template name="E82"/>
   </XSL:template>
   <XSL:template match="placeName">
      <XSL:call-template name="E48"/>
   </XSL:template>
   <XSL:template match="birth">
      <XSL:call-template name="E67"/>
   </XSL:template>
   <XSL:template match="death">
      <XSL:call-template name="E69"/>
   </XSL:template>
   <XSL:template match="event">
      <XSL:call-template name="E5"/>
   </XSL:template>
   <XSL:template match="geo">
      <XSL:call-template name="E47"/>
   </XSL:template>
   <XSL:template match="org">
      <XSL:call-template name="E74"/>
   </XSL:template>
   <XSL:template match="person">
      <XSL:call-template name="E21"/>
   </XSL:template>
   <XSL:template match="place">
      <XSL:call-template name="E53"/>
   </XSL:template>
   <XSL:template match="residence">
      <XSL:call-template name="P74"/>
   </XSL:template>
   <XSL:template match="ab">
      <XSL:call-template name="anonblock"/>
   </XSL:template>
   <XSL:template name="typology">
      <E55_Type xmlns="http://purl.org/NET/crm-owl#"
                xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                rdf:about="http://www.tei-c.org/type/place">
         <label xmlns="http://www.w3.org/2000/01/rdf-schema#">place</label>
      </E55_Type>
      <E55_Type xmlns="http://purl.org/NET/crm-owl#"
                xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                rdf:about="http://www.tei-c.org/type/place/placeName">
         <label xmlns="http://www.w3.org/2000/01/rdf-schema#">contains an absolute or relative place name.  [13.2.3. ]</label>
         <P127_has_broader_term rdf:resource="http://www.tei-c.org/type/place"/>
      </E55_Type>
      <E55_Type xmlns="http://purl.org/NET/crm-owl#"
                xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                rdf:about="http://www.tei-c.org/type/place/bloc">
         <label xmlns="http://www.w3.org/2000/01/rdf-schema#">(bloc) contains the name of a geo-political unit consisting of two or more nation states or
    countries. [13.2.3. ]</label>
         <P127_has_broader_term rdf:resource="http://www.tei-c.org/type/place"/>
      </E55_Type>
      <E55_Type xmlns="http://purl.org/NET/crm-owl#"
                xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                rdf:about="http://www.tei-c.org/type/place/country">
         <label xmlns="http://www.w3.org/2000/01/rdf-schema#">(country) contains the name of a geo-political unit, such as a nation, country, colony, or
    commonwealth, larger than or administratively superior to a region and smaller than a bloc. [13.2.3. ]</label>
         <P127_has_broader_term rdf:resource="http://www.tei-c.org/type/place"/>
      </E55_Type>
      <E55_Type xmlns="http://purl.org/NET/crm-owl#"
                xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                rdf:about="http://www.tei-c.org/type/place/region">
         <label xmlns="http://www.w3.org/2000/01/rdf-schema#">contains the name of an administrative unit such as a state, province, or county, larger
    than a settlement, but smaller than a country. [13.2.3. ]</label>
         <P127_has_broader_term rdf:resource="http://www.tei-c.org/type/place"/>
      </E55_Type>
      <E55_Type xmlns="http://purl.org/NET/crm-owl#"
                xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                rdf:about="http://www.tei-c.org/type/place/district">
         <label xmlns="http://www.w3.org/2000/01/rdf-schema#">contains the name of any kind of subdivision of a settlement, such as a parish, ward, or other administrative or geographic unit. [13.2.3. ]</label>
         <P127_has_broader_term rdf:resource="http://www.tei-c.org/type/place"/>
      </E55_Type>
      <E55_Type xmlns="http://purl.org/NET/crm-owl#"
                xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                rdf:about="http://www.tei-c.org/type/place/settlement">
         <label xmlns="http://www.w3.org/2000/01/rdf-schema#">contains the name of a settlement such as a city, town, or village identified as a single geo-political or administrative unit. [13.2.3. ]</label>
         <P127_has_broader_term rdf:resource="http://www.tei-c.org/type/place"/>
      </E55_Type>
      <E55_Type xmlns="http://purl.org/NET/crm-owl#"
                xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                rdf:about="http://www.tei-c.org/type/place/geogName">
         <label xmlns="http://www.w3.org/2000/01/rdf-schema#">(geographical name) identifies a name associated with some geographical feature such as Windrush Valley or Mount Sinai. [13.2.3. ]</label>
         <P127_has_broader_term rdf:resource="http://www.tei-c.org/type/place"/>
      </E55_Type>
   </XSL:template>
</XSL:stylesheet>
