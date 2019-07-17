<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns="http://www.tei-c.org/ns/1.0"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:iso="http://www.iso.org/ns/1.0"
  xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
  xmlns:o="urn:schemas-microsoft-com:office:office"
  xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
  xmlns:rel="http://schemas.openxmlformats.org/package/2006/relationships"
  xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
  xmlns:v="urn:schemas-microsoft-com:vml"
  xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
  xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
  xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"
  xmlns:w10="urn:schemas-microsoft-com:office:word"
  xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
  xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
  xmlns:mml="http://www.w3.org/1998/Math/MathML"
  xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html" version="2.0"
  exclude-result-prefixes="ve o r m v wp w10 w wne mml tbx pic rel a         tei teidocx xs iso">
  <!-- import base conversion style -->
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet"
    type="stylesheet">
    <desc>
      <p>This software is dual-licensed: 1. Distributed under a Creative Commons
        Attribution-ShareAlike 3.0 Unported License
        http://creativecommons.org/licenses/by-sa/3.0/ 2.
        http://www.opensource.org/licenses/BSD-2-Clause Redistribution and use
        in source and binary forms, with or without modification, are permitted
        provided that the following conditions are met: * Redistributions of
        source code must retain the above copyright notice, this list of
        conditions and the following disclaimer. * Redistributions in binary
        form must reproduce the above copyright notice, this list of conditions
        and the following disclaimer in the documentation and/or other materials
        provided with the distribution. This software is provided by the
        copyright holders and contributors "as is" and any express or implied
        warranties, including, but not limited to, the implied warranties of
        merchantability and fitness for a particular purpose are disclaimed. In
        no event shall the copyright holder or contributors be liable for any
        direct, indirect, incidental, special, exemplary, or consequential
        damages (including, but not limited to, procurement of substitute goods
        or services; loss of use, data, or profits; or business interruption)
        however caused and on any theory of liability, whether in contract,
        strict liability, or tort (including negligence or otherwise) arising in
        any way out of the use of this software, even if advised of the
        possibility of such damage. </p>
      <p>Author: See AUTHORS for the basic scheme. This profile constructed by
        Lou Burnard</p>
      <p>Copyright: 2013, TEI Consortium; 2014 Lou Burnard Consulting</p>
    </desc>
  </doc>
  <!--
            <p>Numéro de scan* : 10010200</p>
            <p>Numéros de pages* : 1 à 2</p>
            <p>Type de document : </p>
            <p>Date : 03/11/1977 <hi rend="italic">(dd/MM/yyyy)</hi></p>
            <p>Lieu : chez FLL</p>
            <p>Oulipiens présents : FLL; HM;CB;IC;PF;LF;MB; JR; NA (<hi rend="italic">intiales
                    seulement, séparées par des point-virgules)</hi></p>
            <p>Invités : Jacques Rigaud</p>
            <p>Président : FLL</p>
            <p>Secrétaire : PF</p>
            <p>Expéditeur : <ref target=" FORMTEXT "><anchor xml:id="Text11"/>     </ref></p>
            <p>Destinataires : <ref target=" FORMTEXT "><anchor xml:id="Text12"/>     </ref></p>
        -->

  
  <xsl:template match="tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title"
    mode="pass3">
   
    <title>
    
      <xsl:text> Réunion du </xsl:text>
      <date>
        <xsl:value-of
          select='substring-before(substring-after(//tei:body/tei:p[4],":"),"(")'
        />
      </date>
      <xsl:value-of select='substring-after( //tei:body/tei:p[5], ":")'/>
      <xsl:text> (</xsl:text><xsl:value-of
        select='substring-before(substring-after(//tei:body/tei:p[3],":"),"(")'/>
      <xsl:text>) : version TEI</xsl:text>
    </title>
    <respStmt>
      <resp>transcription</resp>
      <name>
        <xsl:value-of select="//tei:author"/>
      </name>
    </respStmt>
  </xsl:template>
  
  <xsl:template match="tei:teiHeader/tei:fileDesc/tei:sourceDesc/tei:p"
    mode="pass3">
    <xsl:variable name="partix">
      <xsl:value-of
        select='substring-before(substring-after(//tei:body/tei:p[6],":"),"(")'
      />
    </xsl:variable>
    <xsl:variable name="invites">
      <xsl:value-of select='substring-after(//tei:body/tei:p[7],":")'/>
    </xsl:variable>
    <xsl:variable name="pres">
      <xsl:value-of select='substring-after(//tei:body/tei:p[8],":")'/>
    </xsl:variable>
    <xsl:variable name="sec">
      <xsl:value-of select='substring-after(//tei:body/tei:p[9],":")'/>
    </xsl:variable>
    <xsl:variable name="exp">
      <xsl:choose>
        <xsl:when test="//tei:body/tei:p[10]/tei:ref/tei:anchor"/>
        <xsl:otherwise>
          <xsl:value-of select='substring-after(//tei:body/tei:p[10],":")'/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="dest">
      <xsl:choose>
        <xsl:when test="//tei:body/tei:p[10]/tei:ref/tei:anchor"/>
        <xsl:otherwise>
          <xsl:value-of select='substring-after(//tei:body/tei:p[11],":")'/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <bibl>Encodé a partir du scan <xsl:value-of
        select='substring-after(//tei:body/tei:p[1],":")'/>
      <xsl:text> pp. </xsl:text>
      <xsl:value-of select='substring-after(//tei:body/tei:p[2],":")'/>
    </bibl>
    <bibl><meeting><date><xsl:attribute name="when">
      <xsl:value-of select='tei:whenify(normalize-space(substring-before(substring-after(//tei:body/tei:p[4],":"),"(")))'/></xsl:attribute></date>
      <placeName>  <xsl:value-of select='substring-after( //tei:body/tei:p[5], ":")'/></placeName>
      <list type="present"><xsl:for-each select="tokenize($partix,' ')">
        <xsl:if test="normalize-space(.) ne ''">
          <item><persName>
            <xsl:attribute name="ref">
              <xsl:text>#</xsl:text>
              <xsl:sequence select="normalize-space(.)"/>
            </xsl:attribute>
          </persName></item>
        </xsl:if>
      </xsl:for-each>
       <item> <persName role="invité">
          <xsl:value-of select="normalize-space($invites)"/>
        </persName></item>
        <item><persName role="président">
          <xsl:attribute name="ref">
            <xsl:text>#</xsl:text>
            <xsl:sequence select="normalize-space($pres)"/>
          </xsl:attribute>
        </persName></item>
        <item><persName role="secretaire">
          <xsl:attribute name="ref">
            <xsl:text>#</xsl:text>
          <xsl:value-of select="normalize-space($sec)"/></xsl:attribute>
        </persName></item>
        <xsl:if test="string-length($exp) gt 1">
          <item><persName role="expéditeur">
            <xsl:value-of select="normalize-space($exp)"/>
          </persName></item>
        </xsl:if>
        <xsl:if test="string-length($dest) gt 1">
         <item> <persName role="destinataire">
            <xsl:value-of select="normalize-space($dest)"/>
          </persName></item>
        </xsl:if></list></meeting></bibl>
  </xsl:template>
  
  <xsl:template match="tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:p"
    mode="pass3">
  <p>Edition numérique distribué par le projet ANR DifDePo</p>
  </xsl:template>
  
  
  <xsl:template match="//tei:change" mode="pass3">
    <change>
      <xsl:attribute name="when">
        <xsl:value-of select="tei:whatsTheDate()"/>
      </xsl:attribute>
      Converted from Docx to TEI
    </change>
  </xsl:template>
  <xsl:template match="tei:appInfo" mode="pass2"/>
  <xsl:template match="tei:encodingDesc" mode="pass2"/>
  <xsl:template match="tei:editionStmt" mode="pass2"/>
  <xsl:template match="tei:author" mode="pass3"/>
  <xsl:template match="tei:hi[tei:match(@rend,'illisible')]" mode="pass3">
    <unclear>
      <xsl:apply-templates mode="pass3"/>
    </unclear>
  </xsl:template>
  <xsl:template match="tei:hi[tei:match(@rend,'locuteur')]" mode="pass3">
    <label type="locuteur">
      <!--xsl:apply-templates mode="pass3"/-->
      <xsl:value-of select="normalize-space(.)"/>
    </label>
  </xsl:template>
  <xsl:template match="tei:hi[tei:match(@rend,'manifestation')]" mode="pass3">
    <name type="manif">
      <xsl:apply-templates mode="pass3"/>
    </name>
  </xsl:template>
  <xsl:template match="tei:hi[tei:match(@rend,'notion')]" mode="pass3">
    <term>
      <xsl:apply-templates mode="pass3"/>
    </term>
  </xsl:template>
  <xsl:template match="tei:hi[tei:match(@rend,'organisation')]" mode="pass3">
    <orgName>
      <xsl:apply-templates mode="pass3"/>
    </orgName>
  </xsl:template>
  <xsl:template match="tei:hi[tei:match(@rend,'personneCitee')]" mode="pass3">
    <persName>
      <xsl:apply-templates mode="pass3"/>
    </persName>
  </xsl:template>
  <xsl:template match="tei:hi[tei:match(@rend,'refDocument')]" mode="pass3">
    <ref>
      <xsl:apply-templates mode="pass3"/>
    </ref>
  </xsl:template>
  <xsl:template match="tei:hi[tei:match(@rend,'titre')]" mode="pass3">
    <title>
      <xsl:apply-templates mode="pass3"/>
    </title>
  </xsl:template>
  <xsl:template match="tei:text" mode="pass3">
    <xsl:element name="text">
      <xsl:attribute name="decls">
        <xsl:text>#</xsl:text>
        <xsl:value-of select='normalize-space(substring-before(substring-after(//tei:body/tei:p[3],":"),"("))'/>
      </xsl:attribute>
      <xsl:apply-templates mode="pass3"/>
    </xsl:element>
  </xsl:template>
  
  <xsl:template match="tei:body" mode="pass3">
    <pb/>
    <xsl:for-each-group select="*"
      group-adjacent="if
						  (tei:is-front(.))  then 1
						  else  if (tei:is-back(.))   then 2
						  else  if (tei:is-meta(.))   then 4
						  else 3">
      <xsl:choose>
        <xsl:when test="current-grouping-key()=4"/>
        <xsl:when test="current-grouping-key()=1">
          <front>
            <xsl:apply-templates select="current-group()" mode="pass3"/>
          </front>
        </xsl:when>
        <xsl:when test="current-grouping-key()=2">
          <back>
            <xsl:apply-templates select="current-group()" mode="pass3"/>
          </back>
        </xsl:when>
        <xsl:when test="current-grouping-key()=3">
          <body>
            <xsl:apply-templates select="current-group()" mode="pass3"/>
          </body>
        </xsl:when>
      </xsl:choose>
    </xsl:for-each-group>
  </xsl:template>

<xsl:template match="tei:p[@rend='rubrique']" mode="pass3">
  <label type="rubrique"><xsl:apply-templates mode="pass3"/></label>
</xsl:template>

  <xsl:template match="tei:p/@rend" mode="pass3"/>
  
  <!-- and copy everything else -->
  <xsl:template match="@*|comment()|processing-instruction()|text()"
    mode="pass3">
    <xsl:copy-of select="."/>
  </xsl:template>
  
  <xsl:template match="*" mode="pass3">
    <xsl:copy>
      <xsl:apply-templates
        select="*|@*|processing-instruction()|comment()|text()" mode="pass3"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="tei:opener" mode="pass3">
    <p>
      <xsl:apply-templates
        select="*|@*|processing-instruction()|comment()|text()" mode="pass3"/>
    </p>
  </xsl:template>
  <xsl:template match="tei:closer" mode="pass3">
    <p>
      <xsl:apply-templates
        select="*|@*|processing-instruction()|comment()|text()" mode="pass3"/>
    </p>
  </xsl:template>
  <xsl:function name="tei:is-meta" as="xs:boolean">
    <xsl:param name="p"/>
    <xsl:variable name="pPos">
      <xsl:number select="$p"/>
    </xsl:variable>
    <xsl:value-of select="$pPos &lt; 12"/>
  </xsl:function>
  <xsl:function name="tei:is-front" as="xs:boolean">
    <xsl:param name="p"/>
    <xsl:choose>
      <xsl:when test="$p[tei:match(@rend,'incipit')]">true</xsl:when>
       <xsl:when test="$p[self::tei:opener]">true</xsl:when>
      <xsl:otherwise>false</xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <xsl:function name="tei:is-back" as="xs:boolean">
    <xsl:param name="p"/>
    <xsl:choose>
      <xsl:when test="$p[self::tei:closer]">true</xsl:when>
      <xsl:when test="$p[tei:match(@rend,'closer')]">true</xsl:when>
      <xsl:when test="$p[self::tei:byline]">true</xsl:when>
      <xsl:otherwise>false</xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  
  <xsl:function name="tei:whenify">
    <xsl:param name="str"/>
    <xsl:message><xsl:value-of select="$str"/></xsl:message>
    <xsl:value-of select="substring($str,7,4)"/><xsl:text>-</xsl:text>
    <xsl:value-of select="substring($str,4,2)"/><xsl:text>-</xsl:text>
    <xsl:value-of select="substring($str,1,2)"/>
  </xsl:function>
 <!-- <xsl:template match="tei:persName[.=' ']">
    <xsl:value-of select="."/>
  </xsl:template>-->
</xsl:stylesheet>
