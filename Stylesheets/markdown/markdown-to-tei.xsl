<xsl:stylesheet 
    version="2.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns="http://www.tei-c.org/ns/1.0"
    exclude-result-prefixes="tei xs">

  <xsl:import href="../common/common_makeTEIStructure.xsl"/>

  <xsl:param name="input-encoding" select="'UTF-8'" as="xs:string"/>
  <xsl:param name="input-uri" select="README.md"/>
 
<xsl:output omit-xml-declaration="yes" indent="yes"/>

 <xsl:template name="main">
   <TEI>
     <teiHeader>
       <fileDesc>
	 <titleStmt>
           <title></title>
	 </titleStmt>
	 <publicationStmt>
           <p>from markdown</p>
	 </publicationStmt>
	 <sourceDesc>
           <p>new born</p>
      </sourceDesc>
       </fileDesc>
     </teiHeader>
     <xsl:for-each select="unparsed-text($input-uri, $input-encoding)">
       <xsl:call-template name="convertStructure"/>
     </xsl:for-each>
   </TEI>
 </xsl:template>

 <xsl:template name="gatherText">
     <xsl:for-each select="tokenize(., '\n')">
       <xsl:sequence select="tei:parseLine(.)"/>
     </xsl:for-each>
 </xsl:template>

 <xsl:function name="tei:parseLine" as="element()*">
  <xsl:param name="vLine" as="xs:string*"/>
   
   <xsl:variable name="nLine" select="normalize-space($vLine)"/>
    <xsl:choose>
      <xsl:when test="string-length($nLine)=0"/>
      <xsl:when test="starts-with($vLine, '    ')">
        <xsl:analyze-string select="$vLine" regex="^(    )+(.*)$">
          <xsl:matching-substring>
            <BCODE level="{string-length(regex-group(1))}">
              <xsl:sequence select="tei:parseString(regex-group(2))"/>
            </BCODE>
          </xsl:matching-substring>
        </xsl:analyze-string>
      </xsl:when>
      <xsl:when test="starts-with($nLine, '```')">
        <!-- fenced code block (in some flavors delimited with ~~~ instead) -->
            <FCODE/>
      </xsl:when>
      <xsl:when test="starts-with($nLine, '#')">
     	  <xsl:analyze-string select="$nLine" regex="^(#+) ?(.*)(#*)$">
     	    <xsl:matching-substring>
             <HEAD level="{string-length(regex-group(1))}">
     		      <xsl:sequence select="tei:parseString(regex-group(2))"/>
     	      </HEAD>
     	    </xsl:matching-substring>
     	    <xsl:non-matching-substring>
     	      <xsl:sequence select="tei:parseString(.)"/>
     	    </xsl:non-matching-substring>
     	  </xsl:analyze-string>
      </xsl:when>
      <xsl:when test="starts-with($nLine, '- ') or starts-with($nLine, '* ')">
          <ITEM n="item">
            <xsl:sequence select="tei:parseString(substring($nLine, 3))"/>
          </ITEM>
      </xsl:when>
      <xsl:when test="matches($nLine,'^[0-9]\. ')">
          <NITEM n="item">
            <xsl:sequence select="tei:parseString(normalize-space(substring-after($nLine, '. ')))"/>
          </NITEM>
       </xsl:when>
      <xsl:when test="matches($nLine,'^>')">
        <BQUOTE n="item">
          <xsl:sequence select="tei:parseString(substring($nLine, 2))"/>
        </BQUOTE>
      </xsl:when>
      <xsl:otherwise>
        <p>
          <xsl:sequence select="tei:parseString($nLine)"/>
        </p>
       </xsl:otherwise>
      </xsl:choose>
 </xsl:function>

 <xsl:function name="tei:parseString" as="node()*">
  <xsl:param name="pS" as="xs:string"/>

  <!-- match
    bold/italic __bold__ _italic_
  | bold/italic **bold** *italic*
  | links with text description [description](link)
  | inline code `code`
  | deleted ~~del~~
  | inline links (not very happy about the regex) 
  -->
  <xsl:analyze-string select="$pS" flags="x" regex=
  '(__?(.*?)__?)
  |
   (\*\*?(.*?)\*\*?)
  |
  (\[(.*?)\]\((.*?)\))
   |
   (`(.*?)`)
   |
   (~~(.*?)~~)
   |
   (https?://(\w(\w|\d)*\.)*(aero|arpa|biz|com|coop|edu|info|int|gov|mil|museum|name|net|org|pro|localhost)/?((\w|\d|\.)+/?)*\??((\w|\d)*=(\w|\d)*)?(&amp;(\w|\d)*=(\w|\d)*)*)
   '>
   <xsl:matching-substring>
    <xsl:choose>
     <xsl:when test="regex-group(1)">
       <xsl:variable name="rend">
        <xsl:choose><xsl:when test="substring(regex-group(1), 1, 2)='__'">bold</xsl:when><xsl:otherwise>italic</xsl:otherwise></xsl:choose>
       </xsl:variable>
       <hi rend="{$rend}">
          <xsl:sequence select="tei:parseString(regex-group(2))"/>
	     </hi>
     </xsl:when>
     <xsl:when test="regex-group(3)">
       <xsl:variable name="rend">
         <xsl:choose><xsl:when test="substring(regex-group(3), 1, 2)='**'">bold</xsl:when><xsl:otherwise>italic</xsl:otherwise></xsl:choose>
       </xsl:variable>
       <hi rend="{$rend}">
         <xsl:sequence select="tei:parseString(regex-group(4))"/>
       </hi>
     </xsl:when>
     <xsl:when test="regex-group(5)">
      <ref target="{regex-group(7)}">
       <xsl:sequence select="regex-group(6)"/>
      </ref>
     </xsl:when>
      <xsl:when test="regex-group(8)">
        <code>
          <xsl:sequence select="regex-group(9)"/>
        </code>
      </xsl:when>
      <xsl:when test="regex-group(10)">
        <del>
          <xsl:sequence select="regex-group(11)"/>
        </del>
      </xsl:when>
      <xsl:when test="regex-group(12)">
        <ref target="{regex-group(12)}">
          <xsl:sequence select="regex-group(12)"/>
        </ref>
      </xsl:when>
      
    </xsl:choose>
   </xsl:matching-substring>
   <xsl:non-matching-substring>
    <xsl:value-of select="."/>
   </xsl:non-matching-substring>
  </xsl:analyze-string>
 </xsl:function>


</xsl:stylesheet> 
