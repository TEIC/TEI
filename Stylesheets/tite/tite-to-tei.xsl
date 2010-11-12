<?xml version="1.0" encoding="utf-8"?>
<XSL:stylesheet xmlns:XSL="http://www.w3.org/1999/XSL/Transform" version="2.0"
                xpath-default-namespace="http://www.tei-c.org/ns/1.0">
   <XSL:import href="tite-acdc.xsl"/>
   <XSL:output cdata-section-elements="eg"/>
   <XSL:template match="*">
      <XSL:copy>
         <XSL:apply-templates select="*|@*|processing-instruction()|comment()|text()"/>
      </XSL:copy>
   </XSL:template>
   <XSL:template match="text()|comment()|@*|processing-instruction()">
      <XSL:copy/>
   </XSL:template>
   <XSL:template match="b">
      <XSL:call-template name="b"/>
   </XSL:template>
   <XSL:template match="i">
      <XSL:call-template name="i"/>
   </XSL:template>
   <XSL:template match="ul">
      <XSL:call-template name="ul"/>
   </XSL:template>
   <XSL:template match="sub">
      <XSL:call-template name="sub"/>
   </XSL:template>
   <XSL:template match="sup">
      <XSL:call-template name="sup"/>
   </XSL:template>
   <XSL:template match="smcap">
      <XSL:call-template name="smcap"/>
   </XSL:template>
   <XSL:template match="cols">
      <XSL:call-template name="cols"/>
   </XSL:template>
   <XSL:template match="ornament">
      <XSL:call-template name="ornament"/>
   </XSL:template>
</XSL:stylesheet>