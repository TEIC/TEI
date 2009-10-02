<?xml version="1.0"?>
<xsl:stylesheet 
    version="1.0" 
    xmlns:xd="http://www.pnp-software.com/XSLTdoc"
    xmlns:sch="http://www.ascc.net/xml/schematron"
    xmlns:m="http://www.w3.org/1998/Math/MathML"
    xmlns:atom="http://www.w3.org/2005/Atom"  
    xmlns:estr="http://exslt.org/strings"
    xmlns:xlink="http://www.w3.org/1999/xlink"
    xmlns:xhtml="http://www.w3.org/1999/xhtml"
    xmlns:dbk="http://docbook.org/ns/docbook"
    xmlns:rng="http://relaxng.org/ns/structure/1.0"
    xmlns:tei="http://www.tei-c.org/ns/1.0" 
    xmlns:teix="http://www.tei-c.org/ns/Examples"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    exclude-result-prefixes="xlink xhtml dbk rng sch m tei teix xd atom" >


  <xsl:strip-space elements="teix:* rng:* xsl:* xhtml:* atom:* m:*"/>

  <xsl:param name="startComment">&lt;span class="comment"&gt;</xsl:param>
  <xsl:param name="endComment">&lt;/span&gt;</xsl:param>
  <xsl:param name="startElement">&lt;span class="element"&gt;</xsl:param>
  <xsl:param name="endElement">&lt;/span&gt;</xsl:param>
  <xsl:param name="startElementName">&lt;span class="elementname"&gt;</xsl:param>
  <xsl:param name="endElementName">&lt;/span&gt;</xsl:param>
  <xsl:param name="startAttribute">&lt;span class="attribute"&gt;</xsl:param>
  <xsl:param name="endAttribute">&lt;/span&gt;</xsl:param>
  <xsl:param name="startAttributeValue">&lt;span class="attributevalue"&gt;</xsl:param>
  <xsl:param name="endAttributeValue">&lt;/span&gt;</xsl:param>
  <xsl:param name="startNamespace">&lt;span class="namespace"&gt;</xsl:param>
  <xsl:param name="endNamespace">&lt;/span&gt;</xsl:param>

  <xsl:param name="spaceCharacter">&#xA0;</xsl:param>
  <xsl:param name="showNamespaceDecls">true</xsl:param>

  <xsl:param name="forceWrap">false</xsl:param>
  <xsl:param name="wrapLength">65</xsl:param>
  <xsl:param name="attLength">40</xsl:param>
  <xsl:param name="attsOnSameLine">3</xsl:param>
  <xsl:key name="Namespaces" match="*[ancestor::teix:egXML]" use="namespace-uri()"/>

  <xsl:key name="Namespaces" match="*[not(ancestor::*)]" use="namespace-uri()"/>
  <xd:doc>
    <xd:short>Make newline</xd:short>
    <xd:detail>[common] generate a new line; this template will be
    overridden by different output formats</xd:detail>
    <xd:param name="id">identifier (used for debugging only)</xd:param>
  </xd:doc>
  <xsl:template name="lineBreak">
    <xsl:param name="id"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:template>

  <xd:doc>
    <xd:short>Process comments in verbatim mode</xd:short>
    <xd:detail>[common] </xd:detail>
  </xd:doc>
  <xsl:template match="comment()" mode="verbatim">
    <xsl:choose>
      <xsl:when test="ancestor::Wrapper"/>
      <xsl:when test="ancestor::xhtml:Wrapper"/>
      <xsl:otherwise>
    <xsl:call-template name="lineBreak">
      <xsl:with-param name="id">21</xsl:with-param>
    </xsl:call-template>
    <xsl:value-of  disable-output-escaping="yes" select="$startComment"/>
    <xsl:text>&lt;!--</xsl:text>
    <xsl:choose>
      <xsl:when test="$forceWrap='true'">
	<xsl:call-template name="reformatText">
	  <xsl:with-param name="sofar">0</xsl:with-param>
	  <xsl:with-param name="indent">
	    <xsl:text> </xsl:text>
          </xsl:with-param>
          <xsl:with-param name="text">
	    <xsl:value-of select="normalize-space(.)"/>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="."/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>--&gt;</xsl:text>
    <xsl:value-of  disable-output-escaping="yes"
		   select="$endComment"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xd:doc>
    <xd:short>Process text nodes in verbatim mode</xd:short>
    <xd:detail>[common] </xd:detail>
  </xd:doc>
  <xsl:template match="text()" mode="verbatim">
    <xsl:choose>
      <xsl:when test="$forceWrap='true'">
	<xsl:variable name="indent">
	  <xsl:for-each select="parent::*">
	    <xsl:call-template name="makeIndent"/>
	  </xsl:for-each>
	</xsl:variable>
	<xsl:if test="string-length(.)&gt;$wrapLength or parent::sch:assert">
	  <xsl:text>&#10;</xsl:text>
	  <xsl:value-of select="$indent"/>
	</xsl:if>
        <xsl:call-template name="reformatText">
          <xsl:with-param name="sofar">0</xsl:with-param>
          <xsl:with-param name="indent">
	    <xsl:value-of select="$indent"/>
          </xsl:with-param>
          <xsl:with-param name="text">
	    <xsl:value-of select="normalize-space(.)"/>
	  </xsl:with-param>
	</xsl:call-template>
	<xsl:if test="string-length(.)&gt;$wrapLength or parent::sch:assert">
	  <xsl:text>&#10;</xsl:text>
	  <xsl:value-of select="$indent"/>
	</xsl:if>
      </xsl:when>
      <xsl:when test="not(preceding-sibling::node() or
		      contains(.,'&#10;'))">
	<xsl:if test="starts-with(.,' ')">
	  <xsl:text> </xsl:text>
	</xsl:if>
	<xsl:call-template name="Text">
	  <xsl:with-param name="words">
	    <xsl:value-of select="normalize-space(.)"/>
	  </xsl:with-param>
	</xsl:call-template>
	<xsl:if test="substring(.,string-length(.),1)=' '">
	  <xsl:text> </xsl:text>
	</xsl:if>
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
          <xsl:with-param name="count">0</xsl:with-param>
          <xsl:with-param name="indent">
            <xsl:for-each select="parent::*">
              <xsl:call-template name="makeIndent"/>
            </xsl:for-each>
          </xsl:with-param>
          <xsl:with-param name="text">
	    <xsl:choose>
	      <xsl:when test="starts-with(.,'&#10;') and not
			      (preceding-sibling::node())">
		<xsl:value-of select="translate(substring(.,2),'&#10;','&#x2324;')"/>
	      </xsl:when>
	      <xsl:otherwise>
		    <xsl:value-of select="translate(.,'&#10;','&#x2324;')"/>
	      </xsl:otherwise>
	    </xsl:choose>
          </xsl:with-param>
        </xsl:call-template>
	<!--
	<xsl:if test="substring(.,string-length(.))=' '">
	  <xsl:text> </xsl:text>
	</xsl:if>
	-->
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xd:doc>
    <xd:short>Reflow text</xd:short>
    <xd:detail>[common] </xd:detail>
    <xd:param name="indent">indentation to insert at start of line</xd:param>
    <xd:param name="text">text to manage</xd:param>.
    <xd:param name="sofar">text already processed</xd:param>
  </xd:doc>
  <xsl:template name="reformatText">
    <xsl:param name="indent"/>
    <xsl:param name="text"/>
    <xsl:param name="sofar"/>
    <xsl:choose>
      <xsl:when test="$sofar&gt;$wrapLength">
	<xsl:text>&#10;</xsl:text>
	<xsl:value-of select="$indent"/>
	<xsl:call-template name="reformatText">
	  <xsl:with-param name="text">
	    <xsl:value-of
		select="$text"/>
	  </xsl:with-param>
	  <xsl:with-param name="sofar">
	    <xsl:text>0</xsl:text>
	  </xsl:with-param>
	  <xsl:with-param name="indent">
	    <xsl:value-of select="$indent"/>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:when test="not(contains($text,' '))">
	<xsl:call-template name="Text">
	  <xsl:with-param name="words">
	    <xsl:value-of select="$text"/>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
	<xsl:variable name="chunk">
	    <xsl:value-of
		select="substring-before($text,' ')"/>
	</xsl:variable>
	<xsl:call-template name="Text">
	  <xsl:with-param name="words">
	    <xsl:value-of select="$chunk"/>
	    <xsl:text> </xsl:text>
	  </xsl:with-param>
	</xsl:call-template>
	<xsl:call-template name="reformatText">
	  <xsl:with-param name="text">
	    <xsl:value-of
		select="substring-after($text,' ')"/>
	  </xsl:with-param>
	  <xsl:with-param name="sofar">
	    <xsl:value-of select="$sofar + string-length($chunk) + 1"/>
	  </xsl:with-param>
	  <xsl:with-param name="indent">
	    <xsl:value-of
		select="$indent"/>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xsl:template name="wraptext">
    <xsl:param name="indent"/>
    <xsl:param name="text"/>
    <xsl:param name="count">0</xsl:param>

    <xsl:variable name="finalSpace">
      <xsl:choose>
	<xsl:when test="substring($text,string-length($text),1)=' '">
	  <xsl:text> </xsl:text>
	</xsl:when>
	<xsl:when test="substring($text,string-length($text),1)='&#x2324;'">
	  <xsl:text> </xsl:text>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:text></xsl:text>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="normalize-space($text)=''"/>
      <xsl:when test="contains($text,'&#x2324;')">
	<xsl:if test="$count &gt; 0">
	  <xsl:value-of select="$indent"/>
	  <xsl:text> </xsl:text>
	</xsl:if>
	<xsl:if test="starts-with($text,' ')">
	  <xsl:text> </xsl:text>
	</xsl:if>
	<xsl:call-template name="Text">
	  <xsl:with-param name="words">
	    <xsl:value-of
		select="normalize-space(substring-before($text,'&#x2324;'))"/>
	  </xsl:with-param>
	</xsl:call-template>
	<xsl:call-template name="lineBreak">
	  <xsl:with-param name="id">6</xsl:with-param>
	</xsl:call-template>
	<xsl:value-of select="$indent"/>
	<xsl:call-template name="wraptext">
	  <xsl:with-param name="indent">
	    <xsl:value-of select="$indent"/>
	  </xsl:with-param>
	  <xsl:with-param name="text">
	    <xsl:value-of
		select="normalize-space(substring-after($text,'&#x2324;'))"/>
	    <xsl:value-of select="$finalSpace"/>
	  </xsl:with-param>
	  <xsl:with-param name="count">
	    <xsl:value-of select="$count + 1"/>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
	<xsl:if test="starts-with($text,' ')">
	  <xsl:text> </xsl:text>
	</xsl:if>
	<xsl:if test="$count &gt; 0 and parent::*">
	  <xsl:value-of select="$indent"/>
	  <xsl:text> </xsl:text>
	</xsl:if>
	<xsl:call-template name="Text">
	  <xsl:with-param name="words">
	    <xsl:value-of select="normalize-space($text)"/>
	    <xsl:value-of select="$finalSpace"/>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="Text">
    <xsl:param name="words"/>
    <xsl:choose>
      <xsl:when test="contains($words,'&amp;')">
	<xsl:value-of
	    select="substring-before($words,'&amp;')"/>
	<xsl:text>&amp;amp;</xsl:text>
	<xsl:call-template name="Text">
	  <xsl:with-param name="words">
	    <xsl:value-of select="substring-after($words,'&amp;')"/>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="$words"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xd:doc>
    <xd:short>Process default elements in verbatim mode</xd:short>
    <xd:detail>[common] </xd:detail>
  </xd:doc>
  <xsl:template match="*" mode="verbatim">
    <xsl:choose>
      <xsl:when test="parent::xhtml:Wrapper"/>
<!--      <xsl:when test="child::node()[last()]/self::text()[not(.='')] and child::node()[1]/self::text()[not(.='')]"/>-->
      <xsl:when test="not(parent::*)  or parent::teix:egXML">
	<xsl:choose>
	  <xsl:when test="preceding-sibling::node()[1][self::text()]
			  and following-sibling::node()[1][self::text()]"/>
	  <xsl:when test="preceding-sibling::*">
	    <xsl:call-template name="lineBreak">
	      <xsl:with-param name="id">-1</xsl:with-param>
	    </xsl:call-template>
	  </xsl:when>
	</xsl:choose>
      </xsl:when>
      <xsl:when test="not(preceding-sibling::node())">
	<xsl:call-template name="lineBreak">
          <xsl:with-param name="id">-2</xsl:with-param>
	</xsl:call-template>
        <xsl:call-template name="makeIndent"/>
      </xsl:when>
      <xsl:when test="preceding-sibling::node()[1]/self::*">
        <xsl:call-template name="lineBreak">
          <xsl:with-param name="id">1</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="makeIndent"/>
      </xsl:when>
      <xsl:when test="preceding-sibling::node()[1]/self::text()">
	</xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="lineBreak">
          <xsl:with-param name="id">9</xsl:with-param>
        </xsl:call-template>
        <xsl:call-template name="makeIndent"/>
      </xsl:otherwise>
    </xsl:choose>

    <xsl:value-of disable-output-escaping="yes" select="$startElement"/>
    <xsl:text>&lt;</xsl:text>
    <xsl:call-template name="makeElementName">
      <xsl:with-param name="start">true</xsl:with-param>
    </xsl:call-template>
    <xsl:apply-templates select="@*" mode="verbatim"/>
    <xsl:if test="$showNamespaceDecls='true' or parent::teix:egXML[@rend='full']">
      <xsl:choose>
      <xsl:when test="not(parent::*)">
	<xsl:apply-templates select="." mode="ns"/>
      </xsl:when>
      <xsl:when test="parent::teix:egXML and not(preceding-sibling::*)">
	<xsl:apply-templates select="." mode="ns"/>
      </xsl:when>
      </xsl:choose>
    </xsl:if>

    <xsl:choose>
      <xsl:when test="child::node()">
        <xsl:text>&gt;</xsl:text>
        <xsl:value-of disable-output-escaping="yes" select="$endElement"/>
        <xsl:apply-templates mode="verbatim"/>
        <xsl:choose>
          <xsl:when test="child::node()[last()]/self::text() and child::node()[1]/self::text()"/>

	  <xsl:when test="not(parent::*)  or parent::teix:egXML">
            <xsl:call-template name="lineBreak">
              <xsl:with-param name="id">23</xsl:with-param>
            </xsl:call-template>
	  </xsl:when>
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
        <xsl:value-of disable-output-escaping="yes" select="$startElement"/>
        <xsl:text>&lt;/</xsl:text>
	<xsl:call-template name="makeElementName">
	  <xsl:with-param name="start">false</xsl:with-param>
	</xsl:call-template>
        <xsl:text>&gt;</xsl:text>
        <xsl:value-of disable-output-escaping="yes" select="$endElement"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>/&gt;</xsl:text>
        <xsl:value-of disable-output-escaping="yes" select="$endElement"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


  <xsl:template name="makeElementName">
    <xsl:param name="start"/>
    <xsl:choose>

      <xsl:when
	  test="namespace-uri()='http://docbook.org/ns/docbook'">
	<xsl:value-of disable-output-escaping="yes" select="$startNamespace"/>
	<xsl:text>dbk:</xsl:text>
	<xsl:value-of disable-output-escaping="yes" select="$endNamespace"/>
	<xsl:value-of disable-output-escaping="yes" select="$startElementName"/>
	<xsl:value-of select="local-name(.)"/>
	<xsl:value-of disable-output-escaping="yes" select="$endElementName"/>
      </xsl:when>

      <xsl:when
	  test="namespace-uri()='http://www.w3.org/2001/XMLSchema'">
	<xsl:value-of disable-output-escaping="yes" select="$startNamespace"/>
	<xsl:text>xsd:</xsl:text>
	<xsl:value-of disable-output-escaping="yes"
		      select="$endNamespace"/>
	<xsl:value-of disable-output-escaping="yes" select="$startElementName"/>
	
	<xsl:value-of select="local-name(.)"/>
	<xsl:value-of disable-output-escaping="yes" select="$endElementName"/>
	
      </xsl:when>

      <xsl:when
	  test="namespace-uri()='http://www.ascc.net/xml/schematron'">
	<xsl:value-of disable-output-escaping="yes" select="$startNamespace"/>
	<xsl:text>sch:</xsl:text>
	<xsl:value-of disable-output-escaping="yes"
		      select="$endNamespace"/>
	<xsl:value-of disable-output-escaping="yes" select="$startElementName"/>
	
	<xsl:value-of select="local-name(.)"/>
	<xsl:value-of disable-output-escaping="yes" select="$endElementName"/>
	
      </xsl:when>

      <xsl:when
	  test="namespace-uri()='http://www.w3.org/1998/Math/MathML'">
	<xsl:value-of disable-output-escaping="yes" select="$startNamespace"/>
	<xsl:text>m:</xsl:text>
	<xsl:value-of disable-output-escaping="yes"
		      select="$endNamespace"/>
	<xsl:value-of disable-output-escaping="yes" select="$startElementName"/>
	
	<xsl:value-of select="local-name(.)"/>
	<xsl:value-of disable-output-escaping="yes" select="$endElementName"/>
	
      </xsl:when>

      <xsl:when
	  test="namespace-uri()='http://purl.oclc.org/dsdl/nvdl/ns/structure/1.0'">
	<xsl:value-of disable-output-escaping="yes" select="$startNamespace"/>
	<xsl:text>nvdl:</xsl:text>
	<xsl:value-of disable-output-escaping="yes"
		      select="$endNamespace"/>
	<xsl:value-of disable-output-escaping="yes" select="$startElementName"/>
	
	<xsl:value-of select="local-name(.)"/>
	<xsl:value-of disable-output-escaping="yes" select="$endElementName"/>
	
      </xsl:when>

      <xsl:when
	  test="namespace-uri()='http://relaxng.org/ns/compatibility/annotations/1.0'">
	<xsl:value-of disable-output-escaping="yes" select="$startNamespace"/>
	<xsl:text>a:</xsl:text>
	<xsl:value-of disable-output-escaping="yes"
		      select="$endNamespace"/>
	<xsl:value-of disable-output-escaping="yes" select="$startElementName"/>
	
	<xsl:value-of select="local-name(.)"/>
	<xsl:value-of disable-output-escaping="yes" select="$endElementName"/>
	
      </xsl:when>
      <xsl:when
	  test="namespace-uri()='http://www.w3.org/1999/xhtml'">
	<xsl:value-of disable-output-escaping="yes" select="$startNamespace"/>
	<xsl:text>xhtml:</xsl:text>
	<xsl:value-of disable-output-escaping="yes"
		      select="$endNamespace"/>
	<xsl:value-of disable-output-escaping="yes" select="$startElementName"/>
	
	<xsl:value-of select="local-name(.)"/>
	<xsl:value-of disable-output-escaping="yes" select="$endElementName"/>
	
      </xsl:when>

      <xsl:when
	  test="namespace-uri()='http://www.w3.org/1999/xlink'">
	<xsl:value-of disable-output-escaping="yes" select="$startNamespace"/>
	<xsl:text>xlink:</xsl:text>
	<xsl:value-of disable-output-escaping="yes"
		      select="$endNamespace"/>
	<xsl:value-of disable-output-escaping="yes" select="$startElementName"/>
	
	<xsl:value-of select="local-name(.)"/>
	<xsl:value-of disable-output-escaping="yes" select="$endElementName"/>
	
      </xsl:when>

      <xsl:when
	  test="namespace-uri()='http://relaxng.org/ns/structure/1.0'">
	<xsl:value-of disable-output-escaping="yes" select="$startNamespace"/>
	<xsl:text>rng:</xsl:text>
	<xsl:value-of disable-output-escaping="yes"
		      select="$endNamespace"/>
	<xsl:value-of disable-output-escaping="yes" select="$startElementName"/>
	<xsl:value-of select="local-name(.)"/>
	<xsl:value-of disable-output-escaping="yes" select="$endElementName"/>
	
      </xsl:when>

      <xsl:when
	  test="namespace-uri()='http://earth.google.com/kml/2.1'">
	<xsl:value-of disable-output-escaping="yes" select="$startNamespace"/>
	<xsl:text>kml:</xsl:text>
	<xsl:value-of disable-output-escaping="yes"
		      select="$endNamespace"/>
	<xsl:value-of disable-output-escaping="yes" select="$startElementName"/>
	<xsl:value-of select="local-name(.)"/>
	<xsl:value-of disable-output-escaping="yes" select="$endElementName"/>
	
      </xsl:when>

      <xsl:when
	  test="namespace-uri()='http://www.w3.org/2005/11/its'">
	<xsl:value-of disable-output-escaping="yes" select="$startNamespace"/>
	<xsl:text>its:</xsl:text>
	<xsl:value-of disable-output-escaping="yes"
		      select="$endNamespace"/>
	<xsl:value-of disable-output-escaping="yes" select="$startElementName"/>
	<xsl:value-of select="local-name(.)"/>
	<xsl:value-of disable-output-escaping="yes" select="$endElementName"/>
      </xsl:when>

      <xsl:when test="namespace-uri()='http://www.w3.org/1999/XSL/Transform'">
        <xsl:value-of disable-output-escaping="yes" select="$startNamespace"/>
        <xsl:text>xsl:</xsl:text>
        <xsl:value-of select="local-name(.)"/>
        <xsl:value-of disable-output-escaping="yes" select="$endNamespace"/>
      </xsl:when>

      <xsl:when
	  test="namespace-uri()='http://www.tei-c.org/ns/Examples'">
	<xsl:value-of disable-output-escaping="yes" select="$startElementName"/>
	<xsl:value-of select="local-name(.)"/>
	<xsl:value-of disable-output-escaping="yes" select="$endElementName"/>
      </xsl:when>

      <xsl:when
	  test="namespace-uri()='http://www.w3.org/2005/Atom'">
	<xsl:value-of disable-output-escaping="yes" select="$startNamespace"/>
	<xsl:text>atom:</xsl:text>
	<xsl:value-of disable-output-escaping="yes"
		      select="$endNamespace"/>
	<xsl:value-of disable-output-escaping="yes" select="$startElementName"/>
	<xsl:value-of select="local-name(.)"/>
	    <xsl:value-of disable-output-escaping="yes" select="$endElementName"/>
      </xsl:when>

      <xsl:when
	  test="namespace-uri()='http://purl.org/rss/1.0/modules/event/'">
	<xsl:value-of disable-output-escaping="yes" select="$startNamespace"/>
	<xsl:text>ev:</xsl:text>
	<xsl:value-of disable-output-escaping="yes"
		      select="$endNamespace"/>
	<xsl:value-of disable-output-escaping="yes" select="$startElementName"/>
	<xsl:value-of select="local-name(.)"/>
	<xsl:value-of disable-output-escaping="yes" select="$endElementName"/>
	
      </xsl:when>

      <xsl:when test="not(namespace-uri()='')">
	<xsl:value-of disable-output-escaping="yes" select="$startElementName"/>
	<xsl:value-of select="local-name(.)"/>
	<xsl:value-of disable-output-escaping="yes" select="$endElementName"/>
	<xsl:if test="$start='true'">
	  <xsl:text> xmlns="</xsl:text>
	  <xsl:value-of select="namespace-uri()"/>
	  <xsl:text>"</xsl:text>
	  <xsl:call-template name="lineBreak">
	    <xsl:with-param name="id">5</xsl:with-param>
	  </xsl:call-template>
	  <xsl:call-template name="makeIndent"/>
	</xsl:if>
      </xsl:when>

      <xsl:otherwise>
	<xsl:value-of disable-output-escaping="yes" select="$startElementName"/>
	<xsl:value-of select="local-name(.)"/>
	<xsl:value-of disable-output-escaping="yes" select="$endElementName"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


    <xsl:template name="makeIndent">
      <xsl:variable name="depth"
		    select="count(ancestor::*[not(namespace-uri()='http://www.tei-c.org/ns/1.0')])"/>
      <xsl:call-template name="makeSpace">
	<xsl:with-param name="d">
	  <xsl:value-of select="$depth"/>
	</xsl:with-param>
      </xsl:call-template>
  </xsl:template>

  <xsl:template name="makeSpace">
    <xsl:param name="d"/>
    <xsl:if test="number($d)&gt;1">
      <xsl:value-of select="$spaceCharacter"/>
      <xsl:call-template name="makeSpace">
	<xsl:with-param name="d">
	  <xsl:value-of select="$d -1"/>
	</xsl:with-param>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

<xsl:template match="@*" mode="verbatim">
  <xsl:variable name="L">
    <xsl:for-each select="../@*">
      <xsl:value-of select="."/>
    </xsl:for-each>
  </xsl:variable>
    <xsl:if test="count(../@*)&gt;$attsOnSameLine or 
		  string-length($L)&gt;$attLength or
		  ancestor::tei:cell[not(@rend='wovenodd-col2')] or
		  namespace-uri()='http://www.w3.org/2005/11/its' or
		  string-length(.)+string-length(name(.)) &gt;
		  $attLength">
    <xsl:call-template name="lineBreak">
      <xsl:with-param name="id">5</xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="makeIndent"/>
  </xsl:if>
  <xsl:value-of select="$spaceCharacter"/>
  <xsl:value-of disable-output-escaping="yes" select="$startAttribute"/>
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
    <xsl:when
	test="namespace-uri()='http://www.example.org/ns/nonTEI'">
      <xsl:text>my:</xsl:text>
    </xsl:when>
    <xsl:when
	test="namespace-uri()='http://relaxng.org/ns/compatibility/annotations/1.0'">
      <xsl:text>a:</xsl:text>
    </xsl:when>
<!--    <xsl:otherwise>
    <xsl:for-each select="namespace::*">
      <xsl:if test="not(name(.)='')">
	  <xsl:value-of select="name(.)"/>
	  <xsl:text>:</xsl:text>
      </xsl:if>
    </xsl:for-each>
    </xsl:otherwise>
-->
  </xsl:choose>
  <xsl:value-of select="local-name(.)"/>
  <xsl:value-of disable-output-escaping="yes" select="$endAttribute"/>
  <xsl:text>="</xsl:text>
  <xsl:value-of disable-output-escaping="yes" select="$startAttributeValue"/>
  <xsl:apply-templates select="." mode="attributetext"/>
  <xsl:value-of disable-output-escaping="yes" select="$endAttributeValue"/>
  <xsl:text>"</xsl:text>
</xsl:template>

<xsl:template match="@*" mode="attributetext">
  <xsl:choose>
    <xsl:when test="string-length(.)&gt;$attLength and contains(.,' ')">
      <xsl:call-template name="reformatText">
	<xsl:with-param name="sofar">0</xsl:with-param>
	<xsl:with-param name="indent">
	  <xsl:text> </xsl:text>
	</xsl:with-param>
	<xsl:with-param name="text">
	  <xsl:value-of select="normalize-space(.)"/>
	</xsl:with-param>
      </xsl:call-template>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="."/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template match="text()|comment()|processing-instruction()" mode="ns"/>

<xsl:template match="*" mode="ns">
  <xsl:param name="list"/>
    <xsl:variable name="used">
    <xsl:for-each select="namespace::*">
      <xsl:variable name="ns" select="."/>
      <xsl:choose>
	<xsl:when test="contains($list,$ns)"/>
	<xsl:when test=".='http://relaxng.org/ns/structure/1.0'"/>
	<xsl:when test=".='http://www.w3.org/2001/XInclude'"/>
	<xsl:when test=".='http://www.tei-c.org/ns/Examples'"/>
	<xsl:when test=".='http://www.ascc.net/xml/schematron'"/>
	<xsl:when test=".='http://relaxng.org/ns/compatibility/annotations/1.0'"/>
	<xsl:when test="name(.)=''"/>
	<xsl:when test=".='http://www.w3.org/XML/1998/namespace'"/>
	<xsl:otherwise>
	  <xsl:text>&#10;&#160;&#160;</xsl:text>
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


<!-- alternative tools for line-breaking -->

  <xsl:template name="breakline">
    <xsl:choose>
      <xsl:when test="string-length(.)&lt;$wrapLength">
        <xsl:value-of select="."/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="words" select="estr:tokenize(.)"/>
        <xsl:apply-templates mode="word" select="$words[1]">
          <xsl:with-param name="len" select="0"/>
        </xsl:apply-templates>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="token" mode="word">
    <xsl:param name="len"/>
    <xsl:choose>
      <xsl:when test="$len +string-length(.) &gt; $wrapLength">
        <xsl:text>&#10;	</xsl:text>
        <xsl:value-of select="."/>
        <xsl:text> </xsl:text>
        <xsl:if test="following-sibling::token">
          <xsl:apply-templates mode="word" select="following-sibling::token[1]">
            <xsl:with-param name="len" select="8"/>
          </xsl:apply-templates>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="."/>
        <xsl:text> </xsl:text>
        <xsl:if test="following-sibling::token">
          <xsl:apply-templates mode="word" select="following-sibling::token[1]">
            <xsl:with-param name="len" select="$len + string-length(.)"/>
          </xsl:apply-templates>
        </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="italicize"/>

  <xsl:template match="token" mode="commentline">
    <xsl:call-template name="italicize">
      <xsl:with-param name="text">
        <xsl:value-of select="translate(.,'&#10;','')"/>
      </xsl:with-param>
    </xsl:call-template>
    <xsl:if test="following-sibling::token">
      <xsl:text>&#10;</xsl:text>
      <xsl:choose>
        <xsl:when test="contains(.,'--&gt;')">
          <xsl:apply-templates mode="normalline"
            select="following-sibling::token[1]"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates mode="commentline"
            select="following-sibling::token[1]"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:if>
  </xsl:template>

  <xsl:template match="token" mode="normalline">
    <xsl:choose>
      <xsl:when test="contains(.,'&lt;!--')">
        <xsl:call-template name="italicize">
          <xsl:with-param name="text">
            <xsl:value-of select="translate(.,'&#10;','')"/>
          </xsl:with-param>
        </xsl:call-template>
        <xsl:if test="following-sibling::token">
          <xsl:text>&#10;	  </xsl:text>
          <xsl:choose>
            <xsl:when test="contains(.,'--&gt;')">
              <xsl:apply-templates mode="normalline"
                select="following-sibling::token[1]"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:apply-templates mode="commentline"
                select="following-sibling::token[1]"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="breakline"/>
        <xsl:if test="following-sibling::token">
          <xsl:text>&#10;	  </xsl:text>
          <xsl:apply-templates mode="normalline"
            select="following-sibling::token[1]"/>
        </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="token" mode="verbatimline">
    <xsl:call-template name="breakline"/>
    <xsl:if test="following-sibling::token">
      <xsl:text>&#10;</xsl:text>
      <xsl:apply-templates mode="verbatimline"
        select="following-sibling::token[1]"/>
    </xsl:if>
  </xsl:template>


</xsl:stylesheet>

