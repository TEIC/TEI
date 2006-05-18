<?xml version="1.0" standalone="yes"?>
<axsl:stylesheet xmlns:axsl="http://www.w3.org/1999/XSL/Transform" xmlns:sch="http://www.ascc.net/xml/schematron" xmlns:tei="http://www.tei-c.org/ns/1.0" version="1.0" tei:dummy-for-xmlns="">
  <axsl:output method="text"/>
  <axsl:template match="*|@*" mode="schematron-get-full-path">
    <axsl:apply-templates select="parent::*" mode="schematron-get-full-path"/>
    <axsl:text>/</axsl:text>
    <axsl:if test="count(. | ../@*) = count(../@*)">@</axsl:if>
    <axsl:value-of select="name()"/>
    <axsl:text>[</axsl:text>
    <axsl:value-of select="1+count(preceding-sibling::*[name()=name(current())])"/>
    <axsl:text>]</axsl:text>
  </axsl:template>
  <axsl:template match="/">Schematron rules for TEI
<axsl:apply-templates select="/" mode="M2"/></axsl:template>
  <axsl:template match="//tei:choice" priority="4000" mode="M2">
    <axsl:choose>
      <axsl:when test="@src and not(parent::tei:choice/tei:seg |   parent::tei:choice/tei:abbr |   parent::tei:choice/tei:expan)"/>
      <axsl:otherwise>In pattern @src and not(parent::tei:choice/tei:seg | parent::tei:choice/tei:abbr | parent::tei:choice/tei:expan):
   The src attribute on choice only applies when the children are abbr, expan or seg
</axsl:otherwise>
    </axsl:choose>
    <axsl:apply-templates mode="M2"/>
  </axsl:template>
  <axsl:template match="//tei:choice" priority="3999" mode="M2">
    <axsl:choose>
      <axsl:when test="@src"/>
      <axsl:otherwise>In pattern @src:
   The src attribute on choice is unwise
</axsl:otherwise>
    </axsl:choose>
    <axsl:apply-templates mode="M2"/>
  </axsl:template>
  <axsl:template match="text()" priority="-1" mode="M2"/>
  <axsl:template match="text()" priority="-1"/>
</axsl:stylesheet>
