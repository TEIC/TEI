<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns="http://www.tei-c.org/ns/1.0" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xpath-default-namespace="http://www.tei-c.org/ns/1.0" exclude-result-prefixes="tei xs" version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p> TEI Utility stylesheet defining functions for use in
      generating JSON</p>
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
      <p>Id: $Id$</p>
      <p>Copyright: 2013, TEI Consortium</p>
    </desc>
  </doc>
  <xsl:strip-space elements="*"/>
  <xsl:output method="text" encoding="utf-8"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc> JSON string with quotes escaped </desc>
  </doc>
  <xsl:function name="tei:jsonString" as="xs:string">
    <xsl:param name="content"/>
    <xsl:variable name="inq">"</xsl:variable>
    <xsl:variable name="outq">\\"</xsl:variable>
    <xsl:value-of select="replace(replace(normalize-space($content),'\\','\\\\'),$inq,$outq)"/>
  </xsl:function>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc> JSON name:value pair. Third parameter is boolean to say whether
     or not the value should be enclosed in quotes </desc>
  </doc>
  <xsl:function name="tei:json" as="xs:string">
    <xsl:param name="label"/>
    <xsl:param name="content"/>
    <xsl:param name="string"/>
    <xsl:variable name="dq">"</xsl:variable>
    <xsl:value-of select="concat($dq,$label,$dq,':',tei:jsonValue($content,$string))"/>
  </xsl:function>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc> JSON value. Second parameter is boolean to say whether
     or not the value should be enclosed in quotes </desc>
  </doc>
  <xsl:function name="tei:jsonValue" as="xs:string">
    <xsl:param name="content"/>
    <xsl:param name="string"/>
    <xsl:variable name="dq">"</xsl:variable>
    <xsl:value-of select="concat(if ($string) then $dq else '',$content,if ($string) then $dq else '')"/>
  </xsl:function>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc> JSON object, to be enclosed in {}, content separated by ,
  </desc>
  </doc>
  <xsl:function name="tei:jsonObject" as="xs:string">
    <xsl:param name="content" as="xs:string*"/>
    <xsl:variable name="r">
      <xsl:value-of select="$content" separator=","/>
    </xsl:variable>
    <xsl:value-of select="concat('{',$r,'}')"/>
  </xsl:function>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc> JSON label:array, to be encloded in []. If
     the label is empty it is omitted.
     Second parameter is a sequence of strings.
     Third parameter is boolean to say whether
     or not the value should be enclosed in quotes 
   </desc>
  </doc>
  <xsl:function name="tei:jsonArray" as="xs:string">
    <xsl:param name="label"/>
    <xsl:param name="contents"/>
    <xsl:param name="string"/>
    <xsl:variable name="dq">"</xsl:variable>
    <xsl:variable name="strings" as="xs:string*">
      <xsl:for-each select="$contents">
        <xsl:value-of select="tei:jsonValue(.,$string)"/>
      </xsl:for-each>
    </xsl:variable>
    <xsl:variable name="r">
      <xsl:value-of select="($strings)" separator=","/>
    </xsl:variable>
    <xsl:value-of select="if ($label !='') then    concat($dq,$label,$dq,':','[',$r,']') else concat('[',$r,']')"/>
  </xsl:function>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc> calculate an XPATH to take us from root of doc to where we are
     now </desc>
  </doc>
  <xsl:function name="tei:xpath" as="xs:string">
    <xsl:param name="context"/>
    <xsl:for-each select="$context">
      <xsl:variable name="r">
        <xsl:for-each select="ancestor::*">
          <xsl:value-of select="name()"/>
          <xsl:text>[</xsl:text>
          <xsl:value-of select="position()"/>
          <xsl:text>]</xsl:text>
          <xsl:text>/</xsl:text>
        </xsl:for-each>
        <xsl:value-of select="name()"/>
        <xsl:text>[</xsl:text>
        <xsl:value-of select="position()"/>
        <xsl:text>]</xsl:text>
      </xsl:variable>
      <xsl:value-of select="$r"/>
    </xsl:for-each>
  </xsl:function>
</xsl:stylesheet>
