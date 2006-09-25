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
  <xsl:template name="lineBreak">
    <xsl:param name="id"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>
  <xsl:template match="text()" mode="verbatim">
    <xsl:choose>
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
    <xsl:text>&#10;&lt;!--</xsl:text>
    <xsl:value-of select="."/>
    <xsl:text>--&gt;&#10;</xsl:text>
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
      <xsl:when test="not(preceding-sibling::node())">
        <xsl:call-template name="lineBreak">
          <xsl:with-param name="id">2</xsl:with-param>
        </xsl:call-template>
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
      <xsl:otherwise>
        <xsl:value-of select="local-name(.)"/>
      </xsl:otherwise>
    </xsl:choose>
  <xsl:if test="not(parent::*)">
    <xsl:if test="descendant-or-self::*[namespace-uri()='http://www.w3.org/2005/11/its']|@*[namespace-uri()='http://www.w3.org/2005/11/its']">
      <xsl:text>&#10;  xmlns:its="http://www.w3.org/2005/11/its" </xsl:text>
    </xsl:if>
    <xsl:if test="descendant-or-self::*[namespace-uri()='http://www.tei-c.org/ns/1.0']|@*[namespace-uri()='http://www.tei-c.org/ns/1.0']">
      <xsl:text>&#10;  xmlns:tei="http://www.tei-c.org/ns/1.0" </xsl:text>
    </xsl:if>
    <xsl:if test="descendant-or-self::*[namespace-uri()='http://docbook.org/ns/docbook']|@*[namespace-uri()='http://docbook.org/ns/docbook']">
      <xsl:text>&#10;  xmlns:dbk="http://docbook.org/ns/docbook" </xsl:text>
    </xsl:if>
    <xsl:if test="descendant-or-self::*[namespace-uri()='http://www.w3.org/1999/xhtml']|@*[namespace-uri()='http://www.w3.org/1999/xhtml']">
      <xsl:text>&#10;  xmlns:xhtml="http://www.w3.org/1999/xhtml" </xsl:text>
    </xsl:if>
    <xsl:if test="descendant-or-self::*[namespace-uri()='http://www.w3.org/1999/xlink']|@*[namespace-uri()='http://www.w3.org/1999/xlink']">
      <xsl:text>&#10;  xmlns:xlink="http://www.w3.org/1999/xlink" </xsl:text>
    </xsl:if>
    <xsl:if test="descendant-or-self::*[namespace-uri()='http://www.w3.org/2001/XMLSchema']|@*[namespace-uri()='http://www.w3.org/2001/XMLSchema']">
      <xsl:text>&#10;  xmlns:xs="http://www.w3.org/2001/XMLSchema" </xsl:text>
    </xsl:if>
    <xsl:if test="descendant-or-self::*[namespace-uri()='http://www.ascc.net/xml/schematron']|@*[namespace-uri()='http://www.ascc.net/xml/schematron']">
      <xsl:text>&#10;  xmlns:sch="http://www.ascc.net/xml/schematron" </xsl:text>
    </xsl:if>
    <xsl:if test="descendant-or-self::*[namespace-uri()='http://www.w3.org/1999/XSL/Transform']|@*[namespace-uri()='http://www.w3.org/1999/XSL/Transform']">
      <xsl:text>&#10;  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" </xsl:text>
    </xsl:if>
    <xsl:if test="descendant-or-self::*[namespace-uri()='http://www.w3.org/2000/svg']|@*[namespace-uri()='http://www.w3.org/2000/svg']">
      <xsl:text>&#10;  xmlns:svg="http://www.w3.org/2000/svg"</xsl:text>
    </xsl:if>

  </xsl:if>

    <xsl:for-each select="@*">
      <xsl:if
        test="count(../@*)&gt;3 or string-length(../@*)&gt;60 or namespace-uri()='http://www.w3.org/2005/11/its'">
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
    </xsl:for-each>
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
</xsl:stylesheet>
