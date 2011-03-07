<?xml version="1.0" encoding="utf-8"?>
<XSL:stylesheet xmlns:XSL="http://www.w3.org/1999/XSL/Transform" version="2.0"
                xpath-default-namespace="http://www.tei-c.org/ns/1.0">
   <XSL:import href="crm.xsl"/>

   <XSL:template match="*">
      <XSL:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
   </XSL:template>
   <XSL:template match="text()|comment()|@*|processing-instruction()"/>
   <XSL:template match="person">
      <XSL:call-template name="E21"/>
   </XSL:template>
   <XSL:template match="place">
      <XSL:call-template name="E53"/>
   </XSL:template>
   <XSL:template match="persName">
      <XSL:call-template name="E82"/>
   </XSL:template>
   <XSL:template match="placeName">
      <XSL:call-template name="E48"/>
   </XSL:template>
   <XSL:template match="event">
      <XSL:call-template name="E5"/>
   </XSL:template>
   <XSL:template match="org">
      <XSL:call-template name="E74"/>
   </XSL:template>
   <XSL:template match="residence">
      <XSL:call-template name="P74"/>
   </XSL:template>
   <XSL:template match="birth">
      <XSL:call-template name="E67"/>
   </XSL:template>
   <XSL:template match="death">
      <XSL:call-template name="E69"/>
   </XSL:template>
   <XSL:template match="geo">
      <XSL:call-template name="E47"/>
   </XSL:template>
   <XSL:template match="name">
      <XSL:call-template name="teiname"/>
   </XSL:template>
</XSL:stylesheet>