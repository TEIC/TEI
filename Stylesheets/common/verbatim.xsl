<?xml version="1.0"?>
<xsl:stylesheet 
    version="1.0" 
    xmlns:xlink="http://www.w3.org/1999/xlink"
    xmlns:xhtml="http://www.w3.org/1999/xhtml"
    xmlns:dbk="http://docbook.org/ns/docbook"
    xmlns:rng="http://relaxng.org/ns/structure/1.0"
    xmlns:tei="http://www.tei-c.org/ns/1.0" 
    xmlns:teix="http://www.tei-c.org/ns/Examples"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:param name="startBold">&lt;span class="element"&gt;</xsl:param>
  <xsl:param name="endBold">&lt;/span&gt;</xsl:param>
  <xsl:param name="startItalic">&lt;span class="attribute"&gt;</xsl:param>
  <xsl:param name="endItalic">&lt;/span&gt;</xsl:param>
  <xsl:param name="startRed">&lt;span style="color:red"&gt;</xsl:param>
  <xsl:param name="endRed">&lt;/span&gt;</xsl:param>
  <xsl:param name="spaceCharacter">&#xA0;</xsl:param>

  <xsl:key name="Namespaces" match="*[ancestor::teix:egXML]" use="namespace-uri()"/>

  <xsl:key name="Namespaces" match="*[not(ancestor::*)]" use="namespace-uri()"/>


  <xsl:template name="newLine"/>
  <xsl:template name="lineBreak">
    <xsl:param name="id"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>
  <xsl:template match="text()" mode="verbatim">
    <xsl:choose>
      <xsl:when test="not(preceding-sibling::node())">
            <xsl:value-of select="."/>
      </xsl:when>
      <xsl:when test="normalize-space(.)=''">
        <xsl:for-each select="following-sibling::*[1]">
          <xsl:call-template name="lineBreak">
            <xsl:with-param name="id">7</xsl:with-param>
          </xsl:call-template>
          <xsl:call-template name="makeIndent"/>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="wraptext">
          <xsl:with-param name="indent">
            <xsl:for-each select="parent::*">
              <xsl:call-template name="makeIndent"/>
            </xsl:for-each>
          </xsl:with-param>
          <xsl:with-param name="text">
            <xsl:value-of select="."/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="comment()" mode="verbatim">
    <xsl:call-template name="lineBreak"/>
    <xsl:text>&lt;!--</xsl:text>
    <xsl:value-of select="."/>
    <xsl:text>--&gt;</xsl:text>
    <xsl:call-template name="lineBreak"/>
  </xsl:template>
  <xsl:template name="wraptext">
    <xsl:param name="indent"/>
    <xsl:param name="text"/>
    <xsl:choose>
      <xsl:when test="$text='&#10;'"/>
      <xsl:when test="contains($text,'&#10;')">
        <xsl:value-of select="substring-before($text,'&#10;')"/>
        <xsl:call-template name="lineBreak">
          <xsl:with-param name="id">6</xsl:with-param>
        </xsl:call-template>
        <xsl:value-of select="$indent"/>
        <xsl:call-template name="wraptext">
          <xsl:with-param name="indent">
            <xsl:value-of select="$indent"/>
          </xsl:with-param>
          <xsl:with-param name="text">
            <xsl:value-of select="substring-after($text,'&#10;')"/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$text"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="*" mode="verbatim">
    <xsl:choose>
      <xsl:when test="not(parent::*)  or parent::teix:egXML">
	<xsl:choose>
	  <xsl:when test="preceding-sibling::*">
	    <xsl:call-template name="lineBreak"/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:call-template name="newLine"/>
	  </xsl:otherwise>
	</xsl:choose>
        <xsl:call-template name="makeIndent"/>
      </xsl:when>
      <xsl:when test="not(preceding-sibling::node())">
	<xsl:call-template name="lineBreak"/>
        <xsl:call-template name="makeIndent"/>
      </xsl:when>
      <xsl:when test="preceding-sibling::node()[1]/self::*">
        <xsl:call-template name="lineBreak">
          <xsl:with-param name="id">1</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="makeIndent"/>
      </xsl:when>
      <xsl:when test="preceding-sibling::node()[1]/self::text()"> </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="lineBreak">
          <xsl:with-param name="id">9</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="makeIndent"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:value-of disable-output-escaping="yes" select="$startBold"/>
    <xsl:text>&lt;</xsl:text>
    <xsl:choose>
	  <xsl:when
	      test="namespace-uri()='http://docbook.org/ns/docbook'">
	    <xsl:text>dbk:</xsl:text>
	    <xsl:value-of select="local-name(.)"/>
	  </xsl:when>
	  <xsl:when
	      test="namespace-uri()='http://www.w3.org/1999/xhtml'">
	    <xsl:text>xhtml:</xsl:text>
	    <xsl:value-of select="local-name(.)"/>
	  </xsl:when>
	  <xsl:when
	      test="namespace-uri()='http://www.w3.org/1999/xlink'">
	    <xsl:text>xlink:</xsl:text>
	    <xsl:value-of select="local-name(.)"/>
	  </xsl:when>
      <xsl:when test="namespace-uri()='http://relaxng.org/ns/structure/1.0'">
        <xsl:text>rng:</xsl:text>
        <xsl:value-of select="local-name(.)"/>
      </xsl:when>
      <xsl:when test="namespace-uri()='http://www.w3.org/2005/11/its'">
        <xsl:text>its:</xsl:text>
        <xsl:value-of select="local-name(.)"/>
      </xsl:when>
      <xsl:when test="namespace-uri()='http://www.w3.org/1999/XSL/Transform'">
        <xsl:value-of disable-output-escaping="yes" select="$startRed"/>
        <xsl:text>xsl:</xsl:text>
        <xsl:value-of select="local-name(.)"/>
        <xsl:value-of disable-output-escaping="yes" select="$endRed"/>
      </xsl:when>
      <xsl:when
	  test="namespace-uri()='http://www.tei-c.org/ns/Examples'">
	<xsl:value-of select="local-name(.)"/>
      </xsl:when>
      <xsl:when test="not(namespace-uri()='')">
        <xsl:value-of select="local-name(.)"/>
	<xsl:text> xmlns="</xsl:text>
	<xsl:value-of select="namespace-uri()"/>
	<xsl:text>"</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="local-name(.)"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:apply-templates select="@*" mode="verbatim"/>
  <xsl:if test="(not(parent::*) or (parent::teix:egXML)) and not(preceding-sibling::*)">
    <xsl:apply-templates select="." mode="ns"/>
  </xsl:if>
<!--
    <xsl:if test="descendant-or-self::*[namespace-uri()='http://www.w3.org/2005/11/its']|descendant-or-self::*/@*[namespace-uri()='http://www.w3.org/2005/11/its']">
          <xsl:call-template name="lineBreak"/>
	  <xsl:text>  xmlns:its="http://www.w3.org/2005/11/its" </xsl:text>
    </xsl:if>
    <xsl:if test="descendant-or-self::*[namespace-uri()='http://www.tei-c.org/ns/1.0']|descendant-or-self::*/@*[namespace-uri()='http://www.tei-c.org/ns/1.0']">
    <xsl:call-template name="lineBreak"/>
    <xsl:text>  xmlns:tei="http://www.tei-c.org/ns/1.0" </xsl:text>
    </xsl:if>
    <xsl:if test="descendant-or-self::*[namespace-uri()='http://docbook.org/ns/docbook']|descendant-or-self::*/@*[namespace-uri()='http://docbook.org/ns/docbook']">
          <xsl:call-template name="lineBreak"/>
	  <xsl:text>  xmlns:dbk="http://docbook.org/ns/docbook" </xsl:text>
    </xsl:if>
    <xsl:if test="descendant-or-self::*[namespace-uri()='http://www.w3.org/1999/xhtml']|descendant-or-self::*/@*[namespace-uri()='http://www.w3.org/1999/xhtml']">
          <xsl:call-template name="lineBreak"/>
	  <xsl:text>  xmlns:xhtml="http://www.w3.org/1999/xhtml" </xsl:text>
    </xsl:if>
    <xsl:if test="descendant-or-self::*[namespace-uri()='http://www.w3.org/1999/xlink']|descendant-or-self::*/@*[namespace-uri()='http://www.w3.org/1999/xlink']">
          <xsl:call-template name="lineBreak"/>
	  <xsl:text>  xmlns:xlink="http://www.w3.org/1999/xlink" </xsl:text>
    </xsl:if>
    <xsl:if test="descendant-or-self::*[namespace-uri()='http://www.w3.org/2001/XMLSchema']|descendant-or-self::*/@*[namespace-uri()='http://www.w3.org/2001/XMLSchema']">
          <xsl:call-template name="lineBreak"/>
	  <xsl:text>  xmlns:xs="http://www.w3.org/2001/XMLSchema" </xsl:text>
    </xsl:if>
    <xsl:if test="descendant-or-self::*[namespace-uri()='http://www.ascc.net/xml/schematron']|descendant-or-self::*/@*[namespace-uri()='http://www.ascc.net/xml/schematron']">
          <xsl:call-template name="lineBreak"/>
	  <xsl:text>  xmlns:sch="http://www.ascc.net/xml/schematron" </xsl:text>
    </xsl:if>
    <xsl:if test="descendant-or-self::*[namespace-uri()='http://www.w3.org/2001/XMLSchema-instance']|descendant-or-self::*/@*[namespace-uri()='http://www.w3.org/2001/XMLSchema-instance']">
          <xsl:call-template name="lineBreak"/>
	  <xsl:text>  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" </xsl:text>
    </xsl:if>

    <xsl:if test="descendant-or-self::*[namespace-uri()='http://www.w3.org/1999/XSL/Transform']">
          <xsl:call-template name="lineBreak"/>
            <xsl:value-of disable-output-escaping="yes" select="$startRed"/>
	  <xsl:text>   xmlns:xsl="http://www.w3.org/1999/XSL/Transform" </xsl:text>
            <xsl:value-of disable-output-escaping="yes"
			  select="$endRed"/>
          <xsl:call-template name="lineBreak"/>
    </xsl:if>
-->
     

    <xsl:choose>
      <xsl:when test="child::node()">
        <xsl:text>&gt;</xsl:text>
        <xsl:value-of disable-output-escaping="yes" select="$endBold"/>
        <xsl:apply-templates mode="verbatim"/>
        <xsl:choose>
          <xsl:when test="child::node()[last()]/self::text()[normalize-space(.)='']">
            <xsl:call-template name="lineBreak">
              <xsl:with-param name="id">3</xsl:with-param>
            </xsl:call-template>
            <xsl:call-template name="makeIndent"/>
          </xsl:when>
          <xsl:when test="child::node()[last()]/self::comment()">
            <xsl:call-template name="lineBreak">
              <xsl:with-param name="id">4</xsl:with-param>
            </xsl:call-template>
            <xsl:call-template name="makeIndent"/>
          </xsl:when>
          <xsl:when test="child::node()[last()]/self::*">
            <xsl:call-template name="lineBreak">
              <xsl:with-param name="id">5</xsl:with-param>
            </xsl:call-template>
            <xsl:call-template name="makeIndent"/>
          </xsl:when>
        </xsl:choose>
        <xsl:value-of disable-output-escaping="yes" select="$startBold"/>
        <xsl:text>&lt;/</xsl:text>
        <xsl:choose>
	  <xsl:when
	      test="namespace-uri()='http://docbook.org/ns/docbook'">
	    <xsl:text>dbk:</xsl:text>
	    <xsl:value-of select="local-name(.)"/>
	  </xsl:when>
	  <xsl:when
	      test="namespace-uri()='http://www.w3.org/1999/xhtml'">
	    <xsl:text>xhtml:</xsl:text>
	    <xsl:value-of select="local-name(.)"/>
	  </xsl:when>
	  <xsl:when
	      test="namespace-uri()='http://www.w3.org/1999/xlink'">
	    <xsl:text>xlink:</xsl:text>
	    <xsl:value-of select="local-name(.)"/>
	  </xsl:when>

          <xsl:when test="namespace-uri()='http://relaxng.org/ns/structure/1.0'">
            <xsl:text>rng:</xsl:text>
            <xsl:value-of select="local-name(.)"/>
          </xsl:when>
          <xsl:when test="namespace-uri()='http://www.w3.org/2005/11/its'">
            <xsl:text>its:</xsl:text>
            <xsl:value-of select="local-name(.)"/>
          </xsl:when>
          <xsl:when test="namespace-uri()='http://www.w3.org/1999/XSL/Transform'">
            <xsl:value-of disable-output-escaping="yes" select="$startRed"/>
            <xsl:text>xsl:</xsl:text>
            <xsl:value-of select="local-name(.)"/>
            <xsl:value-of disable-output-escaping="yes" select="$endRed"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="local-name(.)"/>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:text>&gt;</xsl:text>
        <xsl:value-of disable-output-escaping="yes" select="$endBold"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>/&gt;</xsl:text>
        <xsl:value-of disable-output-escaping="yes" select="$endBold"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="makeIndent">
    <xsl:for-each select="ancestor::*[not(namespace-uri()='http://www.tei-c.org/ns/1.0')]">
      <xsl:value-of select="$spaceCharacter"/>
    </xsl:for-each>
  </xsl:template>

<xsl:template match="@*" mode="verbatim">
  <xsl:variable name="L">
    <xsl:for-each select="../@*">
      <xsl:value-of select="."/>
    </xsl:for-each>
  </xsl:variable>
  <xsl:if
      test="count(../@*)&gt;3 or string-length($L)&gt;40 or namespace-uri()='http://www.w3.org/2005/11/its'">
    <xsl:call-template name="lineBreak">
      <xsl:with-param name="id">5</xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="makeIndent"/>
  </xsl:if>
  <xsl:text>&#xA0;</xsl:text>
  <xsl:value-of disable-output-escaping="yes" select="$startItalic"/>
  <xsl:choose>
    <xsl:when test="namespace-uri()='http://www.w3.org/2005/11/its'">
      <xsl:text>its:</xsl:text>
    </xsl:when>
    <xsl:when
	test="namespace-uri()='http://www.w3.org/XML/1998/namespace'">
      <xsl:text>xml:</xsl:text>
    </xsl:when>
    <xsl:when test="namespace-uri()='http://www.w3.org/1999/xlink'">
      <xsl:text>xlink:</xsl:text>
    </xsl:when>
  </xsl:choose>
  <xsl:value-of select="local-name(.)"/>
  <xsl:value-of disable-output-escaping="yes" select="$endItalic"/>
  <xsl:text>="</xsl:text>
  <xsl:value-of select="."/>
  <xsl:text>"</xsl:text>
</xsl:template>


<xsl:template match="text()|comment()|processing-instruction()" mode="ns"/>

<xsl:template match="*" mode="ns">
  <xsl:param name="list"/>
  <xsl:variable name="used">
    <xsl:for-each select="namespace::*">
      <xsl:variable name="ns" select="."/>
      <xsl:choose>
	<xsl:when test="contains($list,$ns)"/>
	<xsl:when test=".='http://www.tei-c.org/ns/Examples'"/>
	<xsl:when test="name(.)=''"/>
	<xsl:when test=".='http://www.w3.org/XML/1998/namespace'"/>
	<xsl:otherwise>
	  <xsl:call-template name="lineBreak"/>
	  <xsl:text>  </xsl:text>
	  <xsl:text>xmlns:</xsl:text>
	  <xsl:value-of select="name(.)"/>
	  <xsl:text>="</xsl:text>
	  <xsl:value-of select="."/>
	  <xsl:text>"</xsl:text>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:variable>
  <xsl:copy-of select="$used"/>
  <xsl:apply-templates mode="ns">
    <xsl:with-param name="list">
      <xsl:value-of select="$list"/>
      <xsl:value-of select="$used"/>
    </xsl:with-param>
  </xsl:apply-templates>
</xsl:template>

</xsl:stylesheet>

