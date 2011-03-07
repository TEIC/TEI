<?xml version="1.0" encoding="utf-8"?>
<XSL:stylesheet
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:XSL="http://www.w3.org/1999/XSL/Transform" version="1.0">
   <XSL:import href="rdf.xsl"/>

   <XSL:template match="tei:*">
      <XSL:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
   </XSL:template>

   <XSL:template
       match="text()|comment()|@*|processing-instruction()"/>

   <XSL:template match="tei:person">
      <XSL:call-template name="E21"/>
   </XSL:template>
   <XSL:template match="tei:place">
      <XSL:call-template name="E53"/>
   </XSL:template>
   <XSL:template match="tei:persName">
      <XSL:call-template name="E82"/>
   </XSL:template>
   <XSL:template match="tei:placeName">
      <XSL:call-template name="E48"/>
   </XSL:template>
   <XSL:template match="tei:event">
      <XSL:call-template name="E5"/>
   </XSL:template>
   <XSL:template match="tei:org">
      <XSL:call-template name="E74"/>
   </XSL:template>
   <XSL:template match="tei:residence">
      <XSL:call-template name="P74"/>
   </XSL:template>
   <XSL:template match="tei:birth">
      <XSL:call-template name="E67"/>
   </XSL:template>
   <XSL:template match="tei:death">
      <XSL:call-template name="E69"/>
   </XSL:template>
   <XSL:template match="tei:geo">
      <XSL:call-template name="E47"/>
   </XSL:template>
   <XSL:template match="tei:name">
      <XSL:call-template name="teiname"/>
   </XSL:template>

   <XSL:template match="tei:*[@ana]">
      <XSL:call-template name="teiname"/>
   </XSL:template>
</XSL:stylesheet>