<?xml version="1.0" encoding="UTF-8"?>
<XSL:stylesheet xmlns:XSL="http://www.w3.org/1999/XSL/Transform" version="2.0">
   <XSL:template mode="pass2 #default" match="@place">
      <XSL:attribute name="place">
         <XSL:choose>
            <XSL:when test=".='supralinear'">above</XSL:when>
            <XSL:when test=".='pageTop'">top</XSL:when>
            <XSL:when test=".='top-center'">top-centre</XSL:when>
            <XSL:when test=".='bot-right'">bottom-right</XSL:when>
            <XSL:when test=".='bot-left'">bottom-left</XSL:when>
            <XSL:when test=".='bottom-center'">bottom-centre</XSL:when>
            <XSL:when test=".='bot-center'">bottom-centre</XSL:when>
            <XSL:when test=".='foot'">bottom</XSL:when>
            <XSL:when test=".='tablefoot'">tablebottom</XSL:when>
            <XSL:when test=".='right'">margin-right</XSL:when>
            <XSL:when test=".='marg1'">margin-right</XSL:when>
            <XSL:when test=".='marg2'">margin-right</XSL:when>
            <XSL:when test=".='marg3'">margin-right</XSL:when>
            <XSL:when test=".='marg4'">margin-right</XSL:when>
            <XSL:when test=".='margin-outer'">margin</XSL:when>
            <XSL:when test=".='left'">margin-left</XSL:when>
            <XSL:when test=".='in'">inline</XSL:when>
            <XSL:when test=".='display'">block</XSL:when>
            <XSL:otherwise>
               <XSL:value-of select="."/>
            </XSL:otherwise>
         </XSL:choose>
      </XSL:attribute>
   </XSL:template>
   <XSL:template mode="pass2 #default" match="@unit">
      <XSL:attribute name="unit">
         <XSL:choose>
            <XSL:when test=".='char'">chars</XSL:when>
            <XSL:when test=".='characters'">chars</XSL:when>
            <XSL:when test=".='line'">lines</XSL:when>
            <XSL:when test=".='page'">pages</XSL:when>
            <XSL:when test=".='word'">words</XSL:when>
            <XSL:otherwise>
               <XSL:value-of select="."/>
            </XSL:otherwise>
         </XSL:choose>
      </XSL:attribute>
   </XSL:template>
   <XSL:template mode="pass2 #default" match="@rend">
      <XSL:attribute name="rendition">
         <XSL:choose>
            <XSL:when test=".='upper-roman'">simple:allcaps</XSL:when>
            <XSL:when test=".='uc'">simple:allcaps</XSL:when>
            <XSL:when test=".='blackLetter'">simple:blackletter</XSL:when>
            <XSL:when test=".='blackletterType'">simple:blackletter</XSL:when>
            <XSL:when test=".='FrakturType'">simple:blackletter</XSL:when>
            <XSL:when test=".='gothic'">simple:blackletter</XSL:when>
            <XSL:when test=".='b'">simple:bold</XSL:when>
            <XSL:when test=".='bo'">simple:bold</XSL:when>
            <XSL:when test=".='bol'">simple:bold</XSL:when>
            <XSL:when test=".='strong'">simple:bold</XSL:when>
            <XSL:when test=".='border'">simple:boxed</XSL:when>
            <XSL:when test=".='center'">simple:centre</XSL:when>
            <XSL:when test=".='decorInit'">simple:dropcap</XSL:when>
            <XSL:when test=".='italics'">simple:italic</XSL:when>
            <XSL:when test=".='ITALIC'">simple:italic</XSL:when>
            <XSL:when test=".='i'">simple:italic</XSL:when>
            <XSL:when test=".='it'">simple:italic</XSL:when>
            <XSL:when test=".='ital'">simple:italic</XSL:when>
            <XSL:when test=".='large'">simple:larger</XSL:when>
            <XSL:when test=".='left'">simple:left</XSL:when>
            <XSL:when test=".='braced'">simple:leftbraced</XSL:when>
            <XSL:when test=".='spaceletter'">simple:letterspace</XSL:when>
            <XSL:when test=".='roman'">simple:normalweight</XSL:when>
            <XSL:when test=".='right-aligned'">simple:right</XSL:when>
            <XSL:when test=".='rotateCounterclockwise'">simple:rotateleft</XSL:when>
            <XSL:when test=".='rotateClockwise'">simple:rotateright</XSL:when>
            <XSL:when test=".='sc'">simple:smallcaps</XSL:when>
            <XSL:when test=".='smallCap'">simple:smallcaps</XSL:when>
            <XSL:when test=".='small'">simple:smaller</XSL:when>
            <XSL:when test=".='sub'">simple:subscript</XSL:when>
            <XSL:when test=".='sup'">simple:superscript</XSL:when>
            <XSL:when test=".='super'">simple:superscript</XSL:when>
            <XSL:when test=".='u'">simple:underline</XSL:when>
            <XSL:otherwise>
               <XSL:for-each select="tokenize(.,' ')">
                  <XSL:text>simple:</XSL:text>
                  <XSL:value-of select="."/>
                  <XSL:if test="position()!=last()">
                     <XSL:text/>
                  </XSL:if>
               </XSL:for-each>
            </XSL:otherwise>
         </XSL:choose>
      </XSL:attribute>
   </XSL:template>
   <XSL:template mode="pass2 #default" match="@role">
      <XSL:attribute name="role">
         <XSL:choose>
            <XSL:when test=".='LABEL'">label</XSL:when>
            <XSL:otherwise>
               <XSL:value-of select="."/>
            </XSL:otherwise>
         </XSL:choose>
      </XSL:attribute>
   </XSL:template>
</XSL:stylesheet>
