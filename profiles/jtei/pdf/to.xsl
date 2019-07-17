<?xml version="1.0" encoding="UTF-8"?>

<xsl:stylesheet version="2.0" xmlns:xd="http://www.pnp-software.com/XSLTdoc"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:fo="http://www.w3.org/1999/XSL/Format"
  xmlns:exsl="http://exslt.org/common"
  xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:eg="http://www.tei-c.org/ns/Examples"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:local="local"
  xmlns:i18n="i18n"
  exclude-result-prefixes="#all">
  
  <xsl:import href="../jtei.common.xsl"/>

  <xsl:output method="xml" indent="no" encoding="UTF-8"/>
  
  <!-- attribute sets -->
  <xsl:attribute-set name="global.flow.properties">
    <xsl:attribute name="font-family">GentiumPlus</xsl:attribute>
    <xsl:attribute name="font-size">10pt</xsl:attribute>
    <xsl:attribute name="line-height">2</xsl:attribute>
    <xsl:attribute name="text-align">justify</xsl:attribute>
    <xsl:attribute name="orphans">3</xsl:attribute>
    <xsl:attribute name="widows">3</xsl:attribute>
  </xsl:attribute-set>
  
  <xsl:attribute-set name="global.side.margins">
    <xsl:attribute name="margin-left">1cm</xsl:attribute>
    <xsl:attribute name="margin-right">1cm</xsl:attribute>
  </xsl:attribute-set>
  
  <xsl:attribute-set name="headerfooter.properties">
    <xsl:attribute name="font-family">Roboto-medium</xsl:attribute>
    <xsl:attribute name="font-size">7pt</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="heading.properties">
    <xsl:attribute name="font-family">Roboto</xsl:attribute>
    <xsl:attribute name="font-size">13pt</xsl:attribute>
    <xsl:attribute name="text-transform">uppercase</xsl:attribute>
    <xsl:attribute name="keep-with-next">always</xsl:attribute>
    <xsl:attribute name="space-before">1em</xsl:attribute>
    <xsl:attribute name="space-after">1em</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="heading.body.properties">
    <xsl:attribute name="font-family">Roboto-medium</xsl:attribute>
    <xsl:attribute name="keep-with-next">always</xsl:attribute>
    <xsl:attribute name="space-before">1em</xsl:attribute>
  </xsl:attribute-set>
  
  <xsl:attribute-set name="heading.lowerblock.properties">
    <xsl:attribute name="font-family">Roboto-medium</xsl:attribute>
    <xsl:attribute name="font-size">8.3pt</xsl:attribute>
    <xsl:attribute name="keep-with-next">always</xsl:attribute>
  </xsl:attribute-set>
  
  <xsl:attribute-set name="imageblock.properties">
    <xsl:attribute name="inline-progression-dimension.maximum">100%</xsl:attribute>
    <xsl:attribute name="block-progression-dimension.maximum">20cm</xsl:attribute> 
  </xsl:attribute-set>
  
  <xsl:attribute-set name="back.font.properties">
    <xsl:attribute name="font-size">9pt</xsl:attribute>
  </xsl:attribute-set>
  
  <xsl:attribute-set name="block.spacing.properties">
    <xsl:attribute name="space-before">1em</xsl:attribute>
    <xsl:attribute name="space-after">2em</xsl:attribute>
  </xsl:attribute-set>
  
  <xsl:attribute-set name="indentedblock.properties">
    <xsl:attribute name="margin-left">2em</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="pnr.properties">
    <xsl:attribute name="font-family">Roboto-medium</xsl:attribute>
    <xsl:attribute name="font-size">7pt</xsl:attribute>
  </xsl:attribute-set>
  
  <xsl:attribute-set name="egXML.properties">
    <xsl:attribute name="text-align">left</xsl:attribute>
    <xsl:attribute name="text-indent">0pt</xsl:attribute>
    <xsl:attribute name="keep-together.within-page">5</xsl:attribute>
<!--    <xsl:attribute name="orphans">5</xsl:attribute>
    <xsl:attribute name="widows">5</xsl:attribute>-->
  </xsl:attribute-set>
  
  <xsl:attribute-set name="egXML.tag.properties">
    <xsl:attribute name="color">#000099</xsl:attribute>
  </xsl:attribute-set>
  
  <xsl:attribute-set name="egXML.attribute.name.properties">
    <xsl:attribute name="color">#f5844c</xsl:attribute>
  </xsl:attribute-set>
  
  <xsl:attribute-set name="egXML.attribute.value.properties">
    <xsl:attribute name="color">#993300</xsl:attribute>
  </xsl:attribute-set>
  
  <xsl:attribute-set name="egXML.comment.properties">
    <xsl:attribute name="color">#009900</xsl:attribute>
  </xsl:attribute-set>
  
  <xsl:attribute-set name="egXML.pi.properties">
    <xsl:attribute name="color">#812ABE</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="monospace.properties">
    <xsl:attribute name="font-family">DejaVu-mono</xsl:attribute>
    <xsl:attribute name="font-size">.8em</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="table.properties">
    <xsl:attribute name="border">solid 0.1mm grey</xsl:attribute>
    <xsl:attribute name="text-align">left</xsl:attribute>
  </xsl:attribute-set>
  
  <xsl:attribute-set name="cell.properties">
    <xsl:attribute name="padding">0.5em</xsl:attribute>
  </xsl:attribute-set>
  
  <!-- ==================================================================================== -->
  <!-- PAGE DEFINITIONS                                                                     -->
  <!-- ==================================================================================== -->
  
  <xsl:template name="pageDef">
    <fo:layout-master-set>
      <fo:simple-page-master
        master-name="only"
        page-height="29.7cm"
        page-width="21cm"
        margin-top="2cm"
        margin-bottom="2cm"
        margin-left="2.5cm"
        margin-right="2.5cm">
        <fo:region-body
          margin-top="2cm"
          margin-bottom="2cm"
          xsl:use-attribute-sets="global.side.margins"/>
        <fo:region-before
          region-name="any-before"
          extent="2cm"/>
        <fo:region-after
          region-name="any-after"
          extent="2cm"/>
      </fo:simple-page-master>
      <fo:page-sequence-master master-name="article">
        <fo:repeatable-page-master-alternatives>
          <fo:conditional-page-master-reference
            master-reference="only"
            page-position="any"
            blank-or-not-blank="not-blank"/>
        </fo:repeatable-page-master-alternatives>
      </fo:page-sequence-master>
    </fo:layout-master-set>
  </xsl:template>

  <!-- ==================================================================================== -->
  <!-- TEXT SKELETON                                                                        -->
  <!-- ==================================================================================== -->

  <xsl:template match="tei:TEI">    
    <fo:root xmlns:fo="http://www.w3.org/1999/XSL/Format">
      <!-- page definitions  -->
      <xsl:call-template name="pageDef"/>
      <!-- PDF outline -->
      <xsl:call-template name="PDF-outline"/>
      <fo:page-sequence
        master-reference="only"
        format="1"
        initial-page-number="1">
        <!-- header -->
        <fo:static-content flow-name="any-before">
          <xsl:call-template name="page.header"/>
        </fo:static-content>
        <!-- footer -->
        <fo:static-content flow-name="any-after">
          <xsl:call-template name="page.footer"/>
        </fo:static-content>
        <!-- body -->
        <fo:flow flow-name="xsl-region-body" 
          xsl:use-attribute-sets="global.flow.properties">
          <xsl:call-template name="front"/>
          <xsl:call-template name="body"/>
          <xsl:call-template name="back"/>
        </fo:flow>
      </fo:page-sequence>
    </fo:root>
  </xsl:template>
  
  <xsl:template name="page.header">
    <fo:block xsl:use-attribute-sets="global.side.margins headerfooter.properties" text-align="right">
      <!-- a list-block looks like the best bet to get negative indent for page numbers with FOP --> 
      <fo:list-block>
        <fo:list-item>
          <fo:list-item-label><fo:block><xsl:apply-templates select="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[@type='main']"/></fo:block></fo:list-item-label>
          <fo:list-item-body end-indent="-4em">
            <fo:block><fo:page-number/></fo:block>
          </fo:list-item-body>
        </fo:list-item>
      </fo:list-block>
    </fo:block>
  </xsl:template>
  
  <xsl:template name="page.footer">
    <fo:block xsl:use-attribute-sets="global.side.margins headerfooter.properties" 
      padding-top="1em" border-top="solid black .5px">
      <xsl:choose>
        <xsl:when test="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:seriesStmt">
          <xsl:for-each select="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:seriesStmt">
            <xsl:value-of select="tei:title[@level='j']"/>
            <xsl:text>, </xsl:text>
            <xsl:value-of select="tei:biblScope[@unit='issue']/concat(upper-case(substring(@unit, 1, 1)), substring(@unit, 2))"/>
            <xsl:text> </xsl:text>
            <xsl:value-of select="tei:biblScope[@unit='issue']/@n"/>
            <xsl:text>, </xsl:text>
            <xsl:value-of select="preceding-sibling::tei:publicationStmt/tei:date"/>
            <fo:block/>
            <fo:inline font-style="italic"><xsl:value-of select="tei:biblScope"/></fo:inline>
          </xsl:for-each>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="string-join(('Journal of the Text Encoding Initiative', /tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:idno, /tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:date), ', ')"/>          
        </xsl:otherwise>
      </xsl:choose>
    </fo:block>
  </xsl:template>
  
  <xsl:template name="PDF-outline">
    <fo:bookmark-tree>
      <xsl:apply-templates select=".//tei:text/(tei:body|tei:back)/tei:div[tei:head]" mode="PDF-outline"/>
    </fo:bookmark-tree>
  </xsl:template>
  
  <xsl:template match="tei:div[tei:head]" mode="PDF-outline">
    <xsl:variable name="prepared">
      <xsl:apply-templates select="tei:head/node()[not(self::tei:note)]"/>
    </xsl:variable>
    <fo:bookmark internal-destination="{(@xml:id, generate-id())[1]}">
      <fo:bookmark-title>
        <xsl:apply-templates select="." mode="label"/>
        <xsl:value-of select="$prepared"/>
      </fo:bookmark-title>
      <xsl:apply-templates select="tei:div" mode="PDF-outline"/>
    </fo:bookmark>
  </xsl:template>
  
  <xsl:template name="front">
    <xsl:call-template name="article.title"/>
    <xsl:apply-templates select="/tei:TEI/tei:text/tei:front/tei:div[@type='abstract']"/>
    <xsl:apply-templates select="/tei:TEI/tei:teiHeader/tei:profileDesc/tei:textClass"/>
    <xsl:call-template name="front.divs"/>
  </xsl:template>
  
  <xsl:template name="body">
    <fo:block>
      <xsl:apply-templates select="/tei:TEI/tei:text/tei:body"/>
    </fo:block>
  </xsl:template>
  
  <xsl:template name="back">
    <xsl:variable name="variable.content">
      <xsl:call-template name="appendixes"/>
      <xsl:apply-templates select="/tei:TEI/tei:text/tei:back/tei:div[@type='bibliography']"/>
      <xsl:call-template name="endnotes"/>      
    </xsl:variable>
    <xsl:if test="$variable.content[normalize-space()]">
      <fo:block xsl:use-attribute-sets="block.spacing.properties" keep-with-next="always"/>  
      <xsl:copy-of select="$variable.content"/>
    </xsl:if>
    <fo:block xsl:use-attribute-sets="block.spacing.properties" keep-with-next="always"/>
    <xsl:call-template name="authors"/>    
  </xsl:template>
  
  <!-- ==================================================================================== -->
  <!-- FRONT STRUCTURE                                                                      -->
  <!-- ==================================================================================== -->
  
  <xsl:template name="article.title">
    <fo:block margin-top="2cm" margin-bottom="3cm"
      border-top="solid black 5px" border-bottom="solid black 3px" 
      padding-top="1cm" padding-bottom="1cm">
      <fo:block font-family="GentiumBookBasic" font-style="italic" font-size="24pt" line-height="1.2">
        <xsl:for-each select="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[@type = 'main'], /tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:title[not(@type = 'main')]">
          <xsl:apply-templates/>
          <xsl:if test="position() != last()">
            <xsl:text>: </xsl:text>
          </xsl:if>
        </xsl:for-each>
      </fo:block>
      <fo:block font-family="Roboto-medium" font-size="11pt" margin-top="1cm">
        <xsl:for-each select="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:author">
          <xsl:call-template name="enumerate"/>
          <xsl:value-of select="string-join(tei:name/(tei:forename, tei:surname), ' ')"/>
        </xsl:for-each>
      </fo:block>
    </fo:block>
  </xsl:template>
  
  <xsl:template name="front.divs">
    <xsl:for-each select="for $i in $div.types.front[. != 'abstract'] return /tei:TEI/tei:text/tei:front/tei:div[@type = $i]">
      <xsl:apply-templates select="."/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="tei:front/tei:div[@type = $div.types.front]">
    <fo:block>
      <fo:block xsl:use-attribute-sets="heading.properties" font-family="Roboto" font-size="13pt">
        <xsl:value-of select="i18n:key(concat(@type, '-label'))"/>
      </fo:block>
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>
      
  <!-- ==================================================================================== -->
  <!-- BODY STRUCTURE                                                                       -->
  <!-- ==================================================================================== -->

  <xsl:template match="tei:div">
    <fo:block id="{(@xml:id,generate-id())[1]}">
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>
  
  <xsl:template match="tei:body/tei:head">
    <fo:block xsl:use-attribute-sets="heading.body.properties">
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>
  
  <xsl:template match="tei:div/tei:head">
    <xsl:variable name="depth" select="count(ancestor::tei:div)"/>
    <xsl:variable name="font-size">
      <xsl:choose>
        <xsl:when test="$depth &lt;= 1">15</xsl:when>
        <xsl:when test="$depth = 2">12</xsl:when>
        <xsl:when test="$depth = 3">10</xsl:when>
        <xsl:otherwise>8</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <fo:block xsl:use-attribute-sets="heading.body.properties" font-size="{$font-size}pt">
      <xsl:apply-templates select="parent::*" mode="label"/>
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>
  
  <xsl:template match="tei:body//tei:div/tei:p|tei:body/tei:p">
    <!-- a list-block looks like the best bet to get negative indent for paragraph numbers with FOP -->
    <fo:list-block>
      <fo:list-item>  
        <fo:list-item-label start-indent="-2em">
          <fo:block xsl:use-attribute-sets="pnr.properties" padding-top="2.5pt">
            <xsl:number count="tei:body//tei:div/tei:p|tei:body/tei:p" level="any"/>
          </fo:block>
        </fo:list-item-label>
        <fo:list-item-body>
          <fo:block>
            <xsl:apply-templates/>
          </fo:block>
        </fo:list-item-body>
      </fo:list-item>
    </fo:list-block>
  </xsl:template>
  
  <xsl:template match="tei:p" priority="0">
    <fo:block>
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>
  
  <xsl:template match="tei:lb">
    <fo:block/>
  </xsl:template>

  <!-- generate endnote pointer after subsequent punctuaton  -->
  <xsl:template match="tei:note">
    <xsl:param name="note.context" select="ancestor::*[self::tei:front|self::tei:body|self::tei:back]" tunnel="yes" as="element()?"/>
    <xsl:variable name="note.nr" select="local:get.note.nr(.)"/>
    <!-- only 'pull' subsequent punctuation once (i.e. unless it is done for the preceding element) -->
    <xsl:call-template name="include.punctuation"/>
    <fo:inline font-size="5.4pt" vertical-align="super">
      <fo:basic-link internal-destination="{$note.context/name()}.note{$note.nr}" id="{$note.context/name()}.noteptr{$note.nr}">
        <xsl:number value="$note.nr" format="{local:format.note.nr($note.context)}"/>
      </fo:basic-link>
    </fo:inline>
  </xsl:template>

  <!-- group figure contents and headings in a block --> 
  <xsl:template match="tei:figure">
    <fo:block xsl:use-attribute-sets="block.spacing.properties" keep-together.within-page="5">
      <xsl:for-each select="@xml:id">
        <xsl:attribute name="id"><xsl:value-of select="."/></xsl:attribute>
      </xsl:for-each>
      <fo:block xsl:use-attribute-sets="heading.lowerblock.properties imageblock.properties">
        <xsl:apply-templates select="." mode="label"/>
        <xsl:for-each select="tei:head[not(@type='license')]">
          <xsl:apply-templates select="node()"/>
          <xsl:call-template name="punctuate-head"/>
        </xsl:for-each>
      </fo:block>
      <xsl:apply-templates select="*[not(self::tei:head)]"/>
      <xsl:for-each select="tei:head[@type eq 'license']">
        <fo:block xsl:use-attribute-sets="heading.lowerblock.properties">
          <xsl:apply-templates/>
          <xsl:call-template name="punctuate-head"/>
        </fo:block>
      </xsl:for-each>
    </fo:block>
  </xsl:template>
  
  <xsl:template match="tei:figure/tei:graphic">
    <fo:external-graphic xsl:use-attribute-sets="imageblock.properties" content-height="scale-down-to-fit" scaling="uniform" src="{resolve-uri(@url, base-uri())}"/>
  </xsl:template>
  
  <xsl:template match="eg:egXML">
    <fo:block xsl:use-attribute-sets="egXML.properties monospace.properties">
      <xsl:if test="not(parent::tei:figure)">
        <xsl:variable name="xslt.doc" select="doc('')"/>
        <xsl:for-each select="$xslt.doc//xsl:attribute-set[@name='block.spacing.properties']/*">
          <xsl:attribute name="{@name}"><xsl:value-of select="."/></xsl:attribute>
        </xsl:for-each>
<!--        
        <xsl:for-each select="$xslt.doc//xsl:attribute-set[@name='indentblock.properties']/*">
          <xsl:attribute name="{@name}"><xsl:value-of select="."/></xsl:attribute>
        </xsl:for-each>
-->      
      </xsl:if>
      <xsl:apply-templates select="node()" mode="egXML"/>
    </fo:block>
  </xsl:template>

  <xsl:template match="tei:eg">
    <!-- determine maximal amount of preceding whitespace that can be stripped out -->
    <xsl:variable name="stripIndent" select="min((for $line in tokenize(., '\n')[.] return string-length(replace($line, '^(\s+).*', '$1'))))"/>
    <fo:block xsl:use-attribute-sets="egXML.properties monospace.properties">
      <xsl:analyze-string select="." regex="\n">
        <xsl:matching-substring>
          <fo:block/>
        </xsl:matching-substring>
        <xsl:non-matching-substring>
          <xsl:analyze-string select="if ($stripIndent > 0) then replace(., concat('^\s{', $stripIndent, '}'), '') else ." regex="\s">
            <xsl:matching-substring>
              <!-- zero-width space + normal space gives best balance between preserve-space and wrapping -->
              <xsl:text>&#8203; </xsl:text>
            </xsl:matching-substring>
            <xsl:non-matching-substring><xsl:copy-of select="."/></xsl:non-matching-substring>
          </xsl:analyze-string>
        </xsl:non-matching-substring>
      </xsl:analyze-string>
    </fo:block>
  </xsl:template>
  
  <!-- last resort for long words that don't wrap: manual wrap function (currently unused) -->
  <xsl:function name="local:manualWrap">
    <xsl:param name="string"/>
    <xsl:variable name="limit" select="80"/>
    <xsl:choose>
      <xsl:when test="string-length($string) > $limit">
        <xsl:value-of select="string-join((substring($string, 1, $limit), local:manualWrap(substring($string, $limit + 1))), '&#8203; ')"/>
      </xsl:when>
      <xsl:otherwise><xsl:value-of select="$string"/></xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  
  <xsl:template match="*" mode="egXML">
    <xsl:choose>
      <xsl:when test="node()">
        <xsl:call-template name="createTag">
          <xsl:with-param name="type" select="'start'"/>
        </xsl:call-template>
        <xsl:apply-templates select="node()" mode="egXML"/>
        <xsl:call-template name="createTag">
          <xsl:with-param name="type" select="'end'"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="createTag">
          <xsl:with-param name="type" select="'empty'"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template match="comment()" mode="egXML">
    <fo:inline xsl:use-attribute-sets="egXML.comment.properties">
      <xsl:value-of select="local:get.delimiter('start', .)"/>
      <xsl:value-of select="."/>
      <xsl:value-of select="local:get.delimiter('end', .)"/>
    </fo:inline>
  </xsl:template>
  
  <xsl:template match="processing-instruction()" mode="egXML">
    <fo:inline xsl:use-attribute-sets="egXML.pi.properties">
      <xsl:value-of select="local:get.delimiter('start', .)"/>
      <xsl:value-of select="string-join((name(), .), ' ')"/>
      <xsl:value-of select="local:get.delimiter('end', .)"/>
    </fo:inline>
  </xsl:template>

  <!-- eg:egXML//text() processing: borrowed from tei2odt --> 
  <!--    Handling of whitespace is tricky within egXML. We basically want to preserve it,
    with some linebreaks, and try to indent helpfully if there were linebreaks in the original. -->
  <xsl:template match="text()[ancestor::eg:egXML]" mode="egXML">
    <xsl:variable name="container" select="parent::*"/>
    <xsl:variable name="currNode" select="."/>
    <xsl:analyze-string select="." regex="\n">
      <xsl:matching-substring>
        <fo:block/>
        <xsl:for-each select="$currNode/ancestor::*[not(descendant-or-self::eg:egXML)]"><xsl:text>&#160;</xsl:text></xsl:for-each>
        <xsl:if test="$currNode/following-sibling::node()"><xsl:text>&#160;</xsl:text></xsl:if>
      </xsl:matching-substring>
      <xsl:non-matching-substring>
        <xsl:variable name="depth" select="count($currNode/ancestor::*[not(descendant-or-self::eg:egXML)])"/>
<!--
          <fo:inline start-indent="{$depth + 2}em" text-indent="{$depth + 2}em"><xsl:copy-of select="."/></fo:inline>
-->
        <xsl:value-of select="local:escapeEntitiesForEgXML(.)"/>
      </xsl:non-matching-substring>
    </xsl:analyze-string>
  </xsl:template>
  
  <!-- serialize abstract content of "createTag" template to XSL FO -->
  <xsl:template match="tei:seg[starts-with(@type, 'abstract.egXML')]" mode="serialize">
    <fo:inline>
      <xsl:for-each select="document('')//xsl:attribute-set[@name = concat(current()/@type/substring-after(., 'abstract.'), '.properties')]/xsl:attribute">
        <xsl:attribute name="{@name}"><xsl:value-of select="."/></xsl:attribute>
      </xsl:for-each>
      <xsl:apply-templates mode="#current"/>
    </fo:inline>
  </xsl:template> 
  
  <xsl:template match="tei:cit">
    <xsl:apply-templates select="tei:quote"/>
  </xsl:template>
  
  <xsl:template match="tei:cit[not(ancestor::tei:note)]/tei:quote">
    <xsl:call-template name="blockquote"/>
  </xsl:template>
  
  <!-- serialize abstract content of "blockquote" template to XSL FO -->
  <xsl:template match="tei:seg[@type='abstract.blockquote']" mode="serialize">
    <fo:block xsl:use-attribute-sets="indentedblock.properties">
      <xsl:copy-of select="node()" copy-namespaces="no"/>
    </fo:block>
  </xsl:template>
    
  <xsl:template match="tei:cit[ancestor::tei:note]">
    <xsl:apply-templates/>
  </xsl:template>
  
  <!-- put parentheses around <bibl> or <ref> in <cit> -->
  <xsl:template match="tei:cit/tei:bibl|tei:cit/tei:ref" priority="1">
    <xsl:if test="not(ancestor::tei:note)">
      <fo:block/>
    </xsl:if>
    <xsl:text>(</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>)</xsl:text>
  </xsl:template>
  
  <xsl:template match="tei:quote|tei:q">
    <xsl:variable name="quote.bool" select="if (not(parent::tei:cit) or ancestor::tei:note) then true() else false()"/>
    <xsl:variable name="quoteLevel" select="local:get.quoteLevel(.)"/>
    <xsl:if test="$quote.bool">
      <xsl:choose>
        <xsl:when test="$quoteLevel mod 2 = 1">
          <xsl:value-of select="$lsquo"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$ldquo"/>          
        </xsl:otherwise>
      </xsl:choose>
    </xsl:if>
    <xsl:apply-templates/>
    <xsl:call-template name="include.punctuation"/>
    <xsl:if test="$quote.bool">
      <xsl:choose>
        <xsl:when test="$quoteLevel mod 2 = 1">
          <xsl:value-of select="$rsquo"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$rdquo"/>          
        </xsl:otherwise>
      </xsl:choose>
    </xsl:if>
  </xsl:template>  
  
  <xsl:template match="tei:quote//tei:supplied|tei:q//tei:supplied">
    <xsl:text>[</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>]</xsl:text>
  </xsl:template>
  
  <xsl:template match="tei:quote//tei:gap|tei:q//tei:gap">
    <xsl:if test="preceding-sibling::node()[1]/self::text()[matches(., '[\S-[.]]$')]">
      <xsl:text> </xsl:text>
    </xsl:if>
    <xsl:text>…</xsl:text>
    <xsl:if test="following-sibling::node()[1]/self::text()[matches(., '^\S')]">
      <xsl:text> </xsl:text>
    </xsl:if>
  </xsl:template>
  
  <xsl:template match="tei:quote/tei:p">
    <xsl:apply-templates/>
    <xsl:if test="following-sibling::tei:p"><fo:block/></xsl:if>
  </xsl:template>
  
  <!-- skip <p> inside inline lists -->
  <xsl:template match="tei:list[tokenize(@rend, '\s+') = 'inline']/tei:item/tei:p">
    <xsl:apply-templates/>
  </xsl:template>  
  
  <!-- replace external <ptr/> with link, whose label = @target -->
  <xsl:template match="tei:ptr[not(@type)]|tei:ref[not(@type)]">
    <fo:basic-link external-destination="{@target}" border="dotted thin grey">
      <xsl:apply-templates select="if (normalize-space()) then node() else @target"/>
    </fo:basic-link>
  </xsl:template>
  
  <xsl:template match="tei:ptr[starts-with(@target, 'video:')]" priority="1">
    <fo:block>
      <xsl:value-of select="concat('[', @target, ']')"/>
    </fo:block>
  </xsl:template>
  
  <xsl:template match="tei:ptr[@type eq 'crossref']">
    <xsl:variable name="labels">
      <xsl:call-template name="get.crossref.labels"/>
    </xsl:variable>
    <xsl:for-each-group select="$labels/*" group-adjacent="@type">
      <xsl:variable name="counter.group" select="position()"/>
      <xsl:call-template name="enumerate"/>
      <xsl:for-each select="current-group()">
        <xsl:call-template name="enumerate"/>
        <xsl:choose>
          <xsl:when test="normalize-space()">
            <xsl:variable name="label.formatted">
              <xsl:call-template name="format.crossref.labels"/>
            </xsl:variable>
            <fo:basic-link internal-destination="{@n}" border="dotted thin grey">
              <xsl:value-of select="if ($counter.group = 1 and position() = 1 or $jtei.lang = ('de')) then $label.formatted else lower-case($label.formatted)"/>
            </fo:basic-link>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="concat('[bad link to item: ', @n, ']')"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:for-each>
    </xsl:for-each-group>
  </xsl:template>

  <xsl:template match="tei:ref[@type = ('crossref', 'bibl')]">
    <fo:basic-link internal-destination="{substring-after(@target, '#')}" border="dotted thin grey">
      <xsl:apply-templates/>
    </fo:basic-link>
  </xsl:template>
  
  <xsl:template match="tei:soCalled">
    <xsl:variable name="quoteLevel" select="local:get.quoteLevel(.)"/>
    <xsl:choose>
      <xsl:when test="$quoteLevel mod 2 = 1">
        <xsl:value-of select="$lsquo"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$ldquo"/>          
      </xsl:otherwise>
    </xsl:choose>
    <xsl:apply-templates/>
    <xsl:call-template name="include.punctuation"/>
    <xsl:choose>
      <xsl:when test="$quoteLevel mod 2 = 1">
        <xsl:value-of select="$rsquo"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$rdquo"/>          
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="tei:gi|tei:att|tei:val">
    <fo:inline xsl:use-attribute-sets="monospace.properties">
      <xsl:apply-templates select="@*[not(name() = ('scheme'))]"/>
      <xsl:value-of select="local:get.delimiter('start', .)"/>
      <xsl:apply-templates/>
      <xsl:value-of select="local:get.delimiter('end', .)"/>
    </fo:inline>
  </xsl:template>
  
  <xsl:template match="tei:tag">
    <fo:inline xsl:use-attribute-sets="monospace.properties">
      <xsl:apply-templates select="@*[not(name() = ('scheme', 'type'))]"/>
      <xsl:value-of select="local:get.delimiter('start', .)"/>
      <xsl:apply-templates/>
      <xsl:value-of select="local:get.delimiter('end', .)"/>
    </fo:inline>
  </xsl:template>
  
  <xsl:template match="tei:code|tei:ident">
    <fo:inline xsl:use-attribute-sets="monospace.properties">
      <xsl:apply-templates/>
    </fo:inline>
  </xsl:template>
  
  <xsl:template match="tei:emph|tei:mentioned|tei:term|tei:foreign">
    <fo:inline font-style="italic">
      <xsl:apply-templates/>
    </fo:inline>
  </xsl:template>
  
  <!-- only highlight //title[@level = ('m', 'j')] -->
  <xsl:template match="tei:title[@level = ('m', 'j')]">
    <fo:inline font-style="italic">
      <xsl:apply-templates/>
    </fo:inline>
  </xsl:template>
  
  <xsl:template match="tei:title[@level = ('a', 'u')]">
    <xsl:variable name="quoteLevel" select="local:get.quoteLevel(.)"/>
    <xsl:choose>
      <xsl:when test="$quoteLevel mod 2 = 1">
        <xsl:value-of select="$lsquo"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$ldquo"/>          
      </xsl:otherwise>
    </xsl:choose>
    <xsl:apply-templates/>
    <xsl:call-template name="include.punctuation"/>
    <xsl:choose>
      <xsl:when test="$quoteLevel mod 2 = 1">
        <xsl:value-of select="$rsquo"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$rdquo"/>          
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template match="tei:text//tei:title[not(@level)]">
    <xsl:apply-templates/>
  </xsl:template>
  
  <!-- untag bibliographical elements (except title or ref) in running text/notes: just process contents -->
  <xsl:template match="tei:bibl[not(ancestor::tei:div[@type='bibliography']|ancestor::tei:cit)]|tei:bibl[not(ancestor::tei:div[@type='bibliography'])]/*[not(self::tei:title or self::tei:ref or self::tei:ptr)]">
    <xsl:apply-templates/>
  </xsl:template>
  
  <xsl:template match="tei:text//tei:idno[not(parent::tei:bibl)]">
    <xsl:apply-templates/>
  </xsl:template>
  
  <xsl:template match="tei:list[tokenize(@rend, '\s+') = 'inline']">
    <xsl:for-each select="tei:item">
      <xsl:variable name="marker">
        <xsl:call-template name="get.inline.list.marker"/>
      </xsl:variable>
      <fo:inline>
        <xsl:value-of select="$marker"/>
      </fo:inline>
      <xsl:text> </xsl:text>
      <xsl:apply-templates/>
      <xsl:if test="following-sibling::tei:item">
        <xsl:text> </xsl:text>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>
  
  <xsl:template match="tei:list">
    <xsl:variable name="maxLabelWidth" select="if (tokenize(@rend, '\s+') = 'ordered') then max(((count(ancestor::tei:list)) + 0.5, 2)) else 2"/>
    <xsl:apply-templates select="tei:head"/>
    <fo:list-block xsl:use-attribute-sets="block.spacing.properties" start-indent="from-parent(start-indent)" provisional-label-separation="{$maxLabelWidth}em" provisional-distance-between-starts="{$maxLabelWidth}em">
      <xsl:if test="not(ancestor::tei:list)">
        <xsl:attribute name="margin-left">1em</xsl:attribute>
      </xsl:if>
      <xsl:apply-templates select="node()[not(self::tei:head)]"/>
    </fo:list-block>
  </xsl:template>  
  
  <xsl:template match="tei:table/tei:head">
    <fo:block xsl:use-attribute-sets="heading.lowerblock.properties">
      <xsl:apply-templates select="parent::*" mode="label"/>
      <xsl:apply-templates/>
      <xsl:call-template name="punctuate-head"/>
    </fo:block>
  </xsl:template>
  
  <xsl:template match="tei:list/tei:head">
    <fo:block xsl:use-attribute-sets="heading.lowerblock.properties">
      <xsl:apply-templates/>
      <xsl:call-template name="punctuate-head"/>
    </fo:block>
  </xsl:template>

  <xsl:template match="tei:list[not(@type='gloss')]/tei:item">
    <fo:list-item>
      <xsl:if test="not(tei:list)">
        <xsl:attribute name="keep-together.within-page">5</xsl:attribute>
      </xsl:if>
      <fo:list-item-label text-align="end" end-indent="label-end()">
        <fo:block>
          <xsl:choose>
            <xsl:when test="tokenize(parent::tei:list/@rend, '\s+') = 'simple'"/>
            <xsl:when test="tokenize(parent::tei:list/@rend, '\s+') = 'ordered'">
              <xsl:variable name="nr"><xsl:number level="multiple" format="1.1.1.1.1"/></xsl:variable>
              <xsl:value-of select="$nr"/>
              <xsl:if test="string-length($nr) = 1">
                <xsl:text>.</xsl:text>
              </xsl:if>
            </xsl:when>
            <xsl:otherwise>
              <xsl:variable name="depth" select="count(ancestor::tei:list[not(tokenize(@rend, '\s+') = 'ordered')])"/>
              <xsl:attribute name="font-family">DejaVu</xsl:attribute>
              <xsl:choose>
                <xsl:when test="$depth = 1">
                  <xsl:text>•</xsl:text>
                </xsl:when>
                <xsl:when test="$depth = 2">
                  <xsl:text>⚬</xsl:text>
                </xsl:when>
                <xsl:when test="$depth = 3">
                  <xsl:text>▪</xsl:text>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:text>–</xsl:text>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:otherwise>
          </xsl:choose>
        </fo:block>
      </fo:list-item-label>
      <fo:list-item-body start-indent="body-start()">
        <fo:block><xsl:apply-templates/></fo:block>
      </fo:list-item-body>
    </fo:list-item>
  </xsl:template>
  
  <xsl:template match="tei:list[@type='gloss']/tei:label">
    <fo:list-item space-before="1em" space-after="1em" keep-with-next="always">
      <fo:list-item-label><fo:block/></fo:list-item-label>
      <fo:list-item-body font-weight="bold">
        <fo:block><xsl:apply-templates/></fo:block>
      </fo:list-item-body>
    </fo:list-item>
  </xsl:template>

  <xsl:template match="tei:list[@type='gloss']/tei:item">
    <fo:list-item>
      <xsl:if test="not(tei:list)">
        <xsl:attribute name="keep-together.within-page">5</xsl:attribute>
      </xsl:if>
      <fo:list-item-label><fo:block/></fo:list-item-label>
      <fo:list-item-body margin-left="2em">
        <fo:block><xsl:apply-templates/></fo:block>
      </fo:list-item-body>
    </fo:list-item>
  </xsl:template>
  
  <xsl:template match="tei:table">
    <fo:block xsl:use-attribute-sets="block.spacing.properties">
      <xsl:apply-templates select="tei:head"/>
      <fo:table id="{(@xml:id, generate-id())[1]}" xsl:use-attribute-sets="table.properties" table-layout="fixed">
        <!-- mimic @table-layout="auto" by determining optimal column width based on maximal word length per column -->
        <xsl:call-template name="generate.cols"/>
        <fo:table-body>
          <xsl:apply-templates select="node()[not(self::tei:head)]"/>
        </fo:table-body>
      </fo:table>
    </fo:block>
  </xsl:template>
  
  <xsl:template match="tei:row">
    <fo:table-row xsl:use-attribute-sets="table.properties">
      <xsl:if test="@role = 'label'">
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="text-align">center</xsl:attribute>
      </xsl:if>
      <xsl:apply-templates/>
    </fo:table-row>
  </xsl:template>
  
  <xsl:template match="tei:cell">
    <fo:table-cell xsl:use-attribute-sets="table.properties cell.properties">    
      <xsl:for-each select="@cols">
        <xsl:attribute name="number-columns-spanned"><xsl:value-of select="."/></xsl:attribute>
      </xsl:for-each>
      <xsl:for-each select="@rows">
        <xsl:attribute name="number-rows-spanned"><xsl:value-of select="."/></xsl:attribute>
      </xsl:for-each>
      <xsl:if test="(self::*|parent::tei:row)/@role = 'label'">
        <xsl:attribute name="font-weight">bold</xsl:attribute>
        <xsl:attribute name="text-align">center</xsl:attribute>
      </xsl:if>
      <fo:block><xsl:apply-templates/></fo:block>      
    </fo:table-cell>
  </xsl:template>
  
  <xsl:template name="generate.cols">
    <!-- normalize table (expanding @cols and @rows) -->
    <xsl:variable name="table.normalized">
      <xsl:call-template name="table.normalize"/>
    </xsl:variable>
    <!-- generate an overview of the maximal word length per column -->
    <xsl:variable name="max.length.per.column">
      <xsl:for-each select="$table.normalized/*">
        <xsl:call-template name="get.max.length.per.column"/>
      </xsl:for-each>
    </xsl:variable>
    <!-- determine the percentage of this maximal word length based on the total maximal word length of all columns -->
    <xsl:variable name="totalwidth" select="sum($max.length.per.column//@max.length)"/>
    <xsl:for-each select="$max.length.per.column/*">
      <fo:table-column column-number="{position()}" column-width="proportional-column-width({number(@max.length) div number($totalwidth) * 100})"/>
    </xsl:for-each>
  </xsl:template>
  
  <!-- "colspan" and "rowspan" mode templates based on the helpful "table normalization" code at http://andrewjwelch.com/code/xslt/table/table-normalization.html -->
  <xsl:template name="table.normalize">
    <xsl:variable name="table_with_no_colspans">
      <xsl:apply-templates select="." mode="colspan"/>
    </xsl:variable>
    <xsl:for-each select="$table_with_no_colspans">
      <xsl:apply-templates mode="rowspan" />
    </xsl:for-each>
  </xsl:template>
  
  <xsl:template match="@*|node()" mode="rowspan colspan">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()" mode="#current" />
    </xsl:copy>
  </xsl:template>
  
  <xsl:template match="tei:table" mode="rowspan">
    <xsl:copy>
      <xsl:copy-of select="tei:row[1]" />
      <xsl:apply-templates select="tei:row[2]" mode="rowspan">
        <xsl:with-param name="previousRow" select="tei:row[1]" />
      </xsl:apply-templates>
    </xsl:copy>
  </xsl:template>
  
  <xsl:template match="tei:cell[@cols]" mode="colspan">
    <xsl:variable name="current" select="."/>
    <xsl:for-each select="1 to @cols">
      <xsl:copy-of select="$current"/>
    </xsl:for-each>
  </xsl:template>
  
  <xsl:template match="tei:row" mode="rowspan">
    <xsl:param name="previousRow" as="element()" />
    <xsl:variable name="currentRow" select="." />
    <xsl:variable name="normalizedTDs">
      <xsl:for-each select="$previousRow/tei:cell">
        <xsl:choose>
          <xsl:when test="@rows &gt; 1">
            <xsl:copy>
              <xsl:attribute name="rows">
                <xsl:value-of select="@rows - 1" />
              </xsl:attribute>
              <xsl:copy-of select="@*[not(name() = 'rows')]" />
              <xsl:copy-of select="node()" />
            </xsl:copy>
          </xsl:when>
          <xsl:otherwise>
            <xsl:copy-of select="$currentRow/tei:cell[1 + count(current()/preceding-sibling::tei:cell[not(@rows) or (@rows = 1)])]" />
          </xsl:otherwise>
        </xsl:choose>
      </xsl:for-each>
    </xsl:variable>
    <xsl:variable name="newRow" as="element(tei:row)">
      <xsl:copy>
        <xsl:copy-of select="$currentRow/@*" />
        <xsl:copy-of select="$normalizedTDs" />
      </xsl:copy>
    </xsl:variable>    
    <xsl:copy-of select="$newRow" />
    <xsl:apply-templates select="following-sibling::tei:row[1]" mode="rowspan">
      <xsl:with-param name="previousRow" select="$newRow" />
    </xsl:apply-templates>
  </xsl:template>
  
  <xsl:template name="get.max.length.per.column">
    <xsl:variable name="current" select="."/>
    <xsl:for-each select="tei:row[1]/tei:cell">
      <xsl:variable name="pos" select="position()"/>
      <!-- find the longest word per column (divided by @cols) -->
      <local:column n="{$pos}" max.length="{
        max(
          for $cell in $current//tei:cell[$pos]
          return max((
            for $a in tokenize($cell, '\s+') 
            return string-length($a) div max(($cell/@cols, 1)), 
            for $a in $cell//(tei:figure[1])/tei:graphic return 10
          ))
        )}"/>
    </xsl:for-each>
  </xsl:template>
  
  <!-- ==== -->
  <!-- back -->
  <!-- ==== -->
  
  <xsl:template match="tei:div[@type= ('bibliography')]">
    <fo:block xsl:use-attribute-sets="block.spacing.properties"/>
    <fo:block border-top="solid 1px black" xsl:use-attribute-sets="back.font.properties">
      <fo:block xsl:use-attribute-sets="heading.properties">
        <xsl:value-of select="i18n:key(concat(@type, '-label'), (@xml:lang, $jtei.lang)[.][1])"/>
      </fo:block>
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>
  
  <xsl:template name="appendixes">
    <xsl:if test=".//tei:back/tei:div[@type='appendix']">
      <fo:block xsl:use-attribute-sets="back.font.properties">
        <fo:block xsl:use-attribute-sets="heading.properties">
          <xsl:value-of select="i18n:key('appendixes-label')"/>
        </fo:block>
        <xsl:apply-templates select=".//tei:back/tei:div[@type='appendix']"/>
      </fo:block>
    </xsl:if>
  </xsl:template>
  
  <xsl:template match="tei:teiHeader/tei:profileDesc/tei:textClass">
    <fo:block xsl:use-attribute-sets="back.font.properties">
      <fo:block xsl:use-attribute-sets="heading.properties">
        <xsl:value-of select="i18n:key('index-label')"/>
      </fo:block>
      <fo:block>
        <fo:inline font-family="Roboto" font-size="9pt" font-weight="bold">
          <xsl:value-of select="concat(i18n:key('keywords-label'), ': ')"/>
        </fo:inline>
        <xsl:value-of select="string-join(tei:keywords/tei:term, ', ')"/>
      </fo:block>
    </fo:block>
  </xsl:template>
  
  <xsl:template name="authors">
    <fo:block xsl:use-attribute-sets="back.font.properties">
      <fo:block xsl:use-attribute-sets="heading.properties">
        <xsl:variable name="author-label" select="i18n:key('author-label')"/>
        <xsl:choose>
          <xsl:when test="count(/tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:author) gt 1">
            <xsl:value-of select="(i18n:plural($author-label)/@pl, concat($author-label, 's'))[1]"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="$author-label"/>
          </xsl:otherwise>
        </xsl:choose>
      </fo:block>
      <fo:block>
        <xsl:for-each select="//tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:author">
          <fo:block xsl:use-attribute-sets="heading.body.properties" font-size="9pt" text-transform="uppercase">
            <xsl:value-of select="string-join(tei:name/(tei:forename, tei:surname), ' ')"/>
          </fo:block> 
          <fo:block>
            <xsl:apply-templates select="tei:affiliation"/>
          </fo:block>
        </xsl:for-each>
      </fo:block>
    </fo:block>
  </xsl:template>  
    
  <xsl:template match="tei:listBibl/tei:head">
    <xsl:variable name="depth" select="count(ancestor::tei:div)"/>
    <xsl:variable name="font-size">
      <xsl:choose>
        <xsl:when test="$depth &lt;= 1">15</xsl:when>
        <xsl:when test="$depth = 2">12</xsl:when>
        <xsl:when test="$depth = 3">10</xsl:when>
        <xsl:otherwise>8</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <fo:block xsl:use-attribute-sets="heading.body.properties" font-size="{$font-size}pt">
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>
  
  <!-- untag all other <bibl> contents (except for <ref>) -->
  <xsl:template match="tei:listBibl//tei:bibl/*" priority="-0.5">
    <xsl:apply-templates/>
  </xsl:template>
  
  <!-- check if author(ing instance/s) are the same as for preceding bibliographical entry -->
  <xsl:template match="tei:listBibl//tei:bibl">
    <xsl:variable name="dateOrTitle" select="(tei:date|tei:title)[1]"/>
    <fo:block id="{@xml:id}" start-indent="2em" text-indent="-2em">
      <xsl:call-template name="get.author.instance">
        <xsl:with-param name="dateOrTitle" select="$dateOrTitle"/>
      </xsl:call-template>
      <xsl:apply-templates select="$dateOrTitle|node()[. >> ($dateOrTitle,current())[1]]"/>
    </fo:block>
  </xsl:template>

  <xsl:template name="endnotes">
    <xsl:if test=".//tei:text//tei:note">
      <fo:block>
        <fo:block xsl:use-attribute-sets="heading.properties">
          <xsl:value-of select="i18n:key('notes-label')"/>
        </fo:block>
        <xsl:apply-templates select=".//tei:text//tei:note" mode="endnotes"/>
      </fo:block>
    </xsl:if>
  </xsl:template>
  
  <xsl:template match="tei:note" mode="endnotes">
    <xsl:variable name="note.nr" select="local:get.note.nr(.)"/>
    <xsl:variable name="note.context" select="ancestor::*[self::tei:front|self::tei:body|self::tei:back]"/>
    <fo:block>
      <fo:inline font-weight="bold" height="100%">
        <fo:basic-link internal-destination="{$note.context/name()}.noteptr{$note.nr}" id="{$note.context/name()}.note{$note.nr}" space-end="1em">
          <xsl:number value="$note.nr" format="{local:format.note.nr($note.context)}"/>
        </fo:basic-link>
      </fo:inline>
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>
  
  <xsl:template match="tei:note/tei:p">
    <xsl:apply-templates/>
    <xsl:if test="following-sibling::tei:p">
      <fo:block/>
    </xsl:if>
  </xsl:template>

      
</xsl:stylesheet>
