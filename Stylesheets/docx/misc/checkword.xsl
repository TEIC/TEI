<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns="http://www.tei-c.org/ns/1.0"
                xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
                xmlns:o="urn:schemas-microsoft-com:office:office"
                xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
                xmlns:rel="http://schemas.openxmlformats.org/package/2006/relationships"
                xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
                xmlns:v="urn:schemas-microsoft-com:vml"
                xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
                xmlns:w10="urn:schemas-microsoft-com:office:word"
                xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
                xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
                xmlns:mml="http://www.w3.org/1998/Math/MathML"
                xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
                version="2.0"
                exclude-result-prefixes="ve o r m v wp w10 w wne mml tbx">

  <xsl:key name="RIDS" match="*[@r:id]" use="1"/>
  <xsl:key name="RIDS" match="*[@r:embed]" use="1"/>
  <xsl:key name="STYLES" match="w:rStyle" use="1"/>
  <xsl:key name="STYLES" match="w:pStyle" use="1"/>
  <xsl:key name="STYLELIST" match="w:pStyle" use="@w:val"/>
  <xsl:key name="STYLELIST" match="w:rStyle" use="@w:val"/>
  <xsl:key name="R" match="rel:Relationship" use="@Id"/>
  <xsl:key name="S" match="w:style" use="@w:styleId"/>
  <xsl:output indent="yes"/>
  <xsl:template match="/">
      <xsl:message>Check whether styles used match styles defined: </xsl:message>
      <xsl:for-each select="key('STYLES',1)">
         <xsl:if test="generate-id() =generate-id(key('STYLELIST',@w:val)[1])">
	           <xsl:variable name="v" select="@w:val"/>
	           <xsl:message>Examine style <xsl:value-of select="$v"/>
            </xsl:message>
	           <xsl:for-each select="document('styles.xml',/)">
	              <xsl:choose>
	                 <xsl:when test="count(key('S',$v))=0">
	                    <xsl:message>ERROR: no entry for <xsl:value-of select="$v"/>
                     </xsl:message>
	                 </xsl:when>
	                 <xsl:otherwise>
	                    <xsl:for-each select="key('S',$v)">
		                      <xsl:message> .... <xsl:value-of select="$v"/>: <xsl:value-of select="w:name/@w:val"/>
		                      </xsl:message>
	                    </xsl:for-each>
	                 </xsl:otherwise>
	              </xsl:choose>
	           </xsl:for-each>
         </xsl:if>
      </xsl:for-each>

      <xsl:message>Check whether rIds are listed in document rels: </xsl:message>
      <xsl:for-each select="key('RIDS',1)">
         <xsl:variable name="i">
	   <xsl:value-of select="@r:id|@r:embed"/>
	 </xsl:variable>
	 <xsl:for-each select="document('_rels/document.xml.rels',/)">
	   <xsl:choose>
	     <xsl:when test="count(key('R',$i))=0">
	       <xsl:message>ERROR: no entry for <xsl:value-of select="$i"/>
	       </xsl:message>
	     </xsl:when>
	     <xsl:otherwise>
	       <xsl:for-each select="key('R',$i)">
		 <xsl:message> .... <xsl:value-of select="$i"/>: <xsl:value-of select="@Target"/>
		 </xsl:message>
	       </xsl:for-each>
	     </xsl:otherwise>
	   </xsl:choose>
         </xsl:for-each>
      </xsl:for-each>
  </xsl:template>

</xsl:stylesheet>