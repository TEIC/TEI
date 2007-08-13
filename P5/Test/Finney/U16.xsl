<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="2.0" xmlns:tei="http://www.tei-c.org/ns/1.0">
  <xsl:template match="/">
    <html>
      <head>
        <link href="U16.css" media="all" rel="stylesheet" type="text/css"/>
        <title>
          <xsl:value-of select="//tei:titleStmt/tei:title"/>
        </title>
      </head>
      <body>
        <xsl:apply-templates select="//tei:teiHeader"/>
        <xsl:apply-templates select="//tei:text"/>
      </body>
    </html>
  </xsl:template>
  <xsl:template match="tei:abbr">
    <xsl:choose>
      <xsl:when test="@type='fn'">
        <span class="abbr-fn" title="abbreviation (final nu)">
          <xsl:apply-templates/>
        </span>
      </xsl:when>
      <xsl:when test="@type='ns'">
        <span class="abbr-ns" title="abbreviation (nomen sacrum)">
          <xsl:apply-templates/>
        </span>
      </xsl:when>
      <xsl:otherwise>
        <span class="default">
          <xsl:apply-templates/>
        </span>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:add" mode="apparatus">
    <xsl:apply-templates mode="apparatus"/>
    <xsl:text> added</xsl:text>
  </xsl:template>
  <xsl:template match="tei:app">
    <xsl:apply-templates select="tei:rdg[1]"/>
    <xsl:variable name="id">
      <xsl:text>apanc</xsl:text>
      <xsl:number/>
    </xsl:variable>
    <xsl:variable name="href">
      <xsl:text>#ap</xsl:text>
      <xsl:number/>
    </xsl:variable>
    <a>
      <xsl:attribute name="id" select="$id"/>
      <xsl:attribute name="href" select="$href"/>
      <span class="superscript">
        <xsl:text>•</xsl:text>
        <xsl:number/>
      </span>
    </a>
  </xsl:template>
  <xsl:template match="tei:app" mode="apparatus">
    <p>
      <xsl:variable name="id">
        <xsl:text>ap</xsl:text>
        <xsl:number/>
      </xsl:variable>
      <xsl:variable name="href">
        <xsl:text>#apanc</xsl:text>
        <xsl:number/>
      </xsl:variable>
      <a>
        <xsl:attribute name="id" select="$id"/>
        <xsl:attribute name="href" select="$href"/>
        <xsl:text>•</xsl:text>
        <xsl:number/>
      </a>
      <xsl:text>. </xsl:text>
      <xsl:apply-templates mode="apparatus"/>
    </p>
  </xsl:template>
  <xsl:template match="tei:body">
    <div class="body">
      <xsl:apply-templates/>
    </div>
  </xsl:template>
  <xsl:template match="tei:cell">
    <td>
      <xsl:apply-templates/>
    </td>
  </xsl:template>
  <xsl:template match="tei:del" mode="apparatus">
    <xsl:apply-templates mode="apparatus"/>
    <xsl:text> deleted</xsl:text>
  </xsl:template>
  <xsl:template match="tei:div[@type='transcription']">
    <h2>Transcription</h2>
    <div class="transcription">
      <xsl:apply-templates/>
    </div>
    <h2>Apparatus</h2>
    <xsl:apply-templates select="//tei:app" mode="apparatus"/>
    <h2>Notes</h2>
    <xsl:apply-templates select="//tei:note" mode="notes"/>
  </xsl:template>
  <xsl:template match="tei:encodingDesc">
    <div class="encodingDesc">
      <h2>Encoding</h2>
      <xsl:apply-templates select="tei:samplingDecl"/>
      <xsl:apply-templates select="tei:editorialDecl"/>
      <xsl:apply-templates select="tei:refsDecl"/>
    </div>
  </xsl:template>
  <xsl:template match="tei:fileDesc">
    <div class="fileDesc">
      <xsl:apply-templates select="tei:titleStmt"/>
      <xsl:apply-templates select="tei:publicationStmt"/>
      <xsl:apply-templates select="tei:sourceDesc"/>
    </div>
  </xsl:template>
  <xsl:template match="tei:fw">
    <div class="fw">
      <xsl:apply-templates/>
    </div>
  </xsl:template>
  <xsl:template match="tei:gap">
    <xsl:choose>
      <xsl:when test="@extent and (@reason = 'lost')">
        <span class="supplied">
          <xsl:attribute name="title">
            <xsl:text>~ </xsl:text>
            <xsl:value-of select="@extent"/>
            <xsl:text> letters not supplied</xsl:text>
          </xsl:attribute>
          <xsl:text> … </xsl:text>
        </span>
      </xsl:when>
      <xsl:otherwise>
        <div class="gap">
          <xsl:attribute name="title">
            <xsl:value-of select="@reason"/>
          </xsl:attribute>
          <xsl:text>…</xsl:text>
        </div>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:head">
    <h3>
      <xsl:apply-templates/>
    </h3>
  </xsl:template>
  <xsl:template match="tei:history">
    <xsl:if test=".//tei:origDate">
      <xsl:text>Date: </xsl:text>
      <xsl:value-of select=".//tei:origDate"/>
      <xsl:text>. </xsl:text>
    </xsl:if>
  </xsl:template>
  <xsl:template match="tei:idno">
    <xsl:value-of select="."/>
    <xsl:text> (</xsl:text>
    <xsl:value-of select="@type"/>
    <xsl:text>)</xsl:text>
  </xsl:template>
  <xsl:template match="tei:item">
    <li><xsl:apply-templates/></li>
  </xsl:template>
  <xsl:template match="tei:lb">
    <br/>
    <span class="lb">
      <xsl:if test="./@n mod 5 = 0">
        <xsl:value-of select="./@n"/>
      </xsl:if>
    </span>
  </xsl:template>
  <xsl:template match="tei:lb" mode="apparatus">
    <xsl:text>|</xsl:text>
  </xsl:template>
  <xsl:template match="tei:list">
    <ul><xsl:apply-templates/></ul>
  </xsl:template>
  <xsl:template match="tei:milestone">
    <xsl:choose>
      <xsl:when test="@unit='bk'">
        <span class="bk">
          <xsl:value-of select="./@n"/>
          <xsl:text> </xsl:text>
        </span>
      </xsl:when>
      <xsl:when test="@unit='ch'">
        <span class="ch">
          <xsl:text> ch. </xsl:text>
          <xsl:value-of select="./@n"/>
          <xsl:text> </xsl:text>
        </span>
      </xsl:when>
      <xsl:when test="@unit='vs'">
        <span class="vs">
          <xsl:text> vs. </xsl:text>
          <xsl:value-of select="./@n"/>
          <xsl:text> </xsl:text>
        </span>
      </xsl:when>
      <xsl:when test="@unit='paragraph'">
        <span class="paragraph">
          <xsl:text> ¶ </xsl:text>
        </span>
      </xsl:when>
      <xsl:when test="@unit='quire'">
        <div class="quire">quire <xsl:value-of select="./@n"/></div>
      </xsl:when>
      <xsl:otherwise>
        <span class="milestone">
          <xsl:text> [</xsl:text>
          <xsl:value-of select="./@n"/>
          <xsl:text>] </xsl:text>
        </span>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:msContents">
    <xsl:if test="./tei:summary">
      <xsl:text>Contents: </xsl:text>
      <xsl:value-of select="./tei:summary"/>
      <xsl:text>. </xsl:text>
    </xsl:if>
  </xsl:template>
  <xsl:template match="tei:msDescription">
    <div class="msDescription">
      <xsl:apply-templates select="tei:msIdentifier"/>
      <xsl:apply-templates select="tei:msContents"/>
      <xsl:apply-templates select="tei:physDesc"/>
      <xsl:apply-templates select="tei:history"/>
    </div>
  </xsl:template>
  <xsl:template match="tei:msIdentifier">
    <xsl:if test="./tei:idno">
      <xsl:text>Manuscript </xsl:text>
      <xsl:apply-templates select="./tei:idno"/>
      <xsl:text>. </xsl:text>
    </xsl:if>
    <xsl:if test="./tei:repository">
      <xsl:text>Location: </xsl:text>
      <xsl:value-of select="./tei:repository"/>
      <xsl:if test="./tei:settlement and ./tei:country">
        <xsl:text>, </xsl:text>
        <xsl:value-of select="./tei:settlement"/>
        <xsl:text>, </xsl:text>
        <xsl:value-of select="./tei:country"/>
      </xsl:if>
      <xsl:text>. </xsl:text>
    </xsl:if>
    <xsl:for-each select="./tei:altIdentifier">
      <xsl:if test="position() = 1">
        <xsl:text>Other IDs: </xsl:text>
      </xsl:if>
      <xsl:apply-templates/>
      <xsl:choose>
        <xsl:when test="position() != last()">
          <xsl:text>; </xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>. </xsl:text>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
    <xsl:for-each select="./tei:altName">
      <xsl:if test="position() = 1">
        <xsl:text>Other names: </xsl:text>
      </xsl:if>
      <xsl:apply-templates/>
      <xsl:choose>
        <xsl:when test="position() != last()">
          <xsl:text>; </xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>. </xsl:text>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>
  <xsl:template match="tei:note">
    <xsl:variable name="id">
      <xsl:text>fnanc</xsl:text>
      <xsl:number/>
    </xsl:variable>
    <xsl:variable name="href">
      <xsl:text>#fn</xsl:text>
      <xsl:number/>
    </xsl:variable>
    <a>
      <xsl:attribute name="id" select="$id"/>
      <xsl:attribute name="href" select="$href"/>
      <span class="superscript">
        <xsl:text>*</xsl:text>
        <xsl:number/>
      </span>
    </a>
  </xsl:template>
  <xsl:template match="tei:note" mode="notes">
    <xsl:variable name="id">
      <xsl:text>fn</xsl:text>
      <xsl:number/>
    </xsl:variable>
    <xsl:variable name="href">
      <xsl:text>#fnanc</xsl:text>
      <xsl:number/>
    </xsl:variable>
    <p>
      <a>
        <xsl:attribute name="id">
          <xsl:value-of select="$id"/>
        </xsl:attribute>
        <xsl:attribute name="href">
          <xsl:value-of select="$href"/>
        </xsl:attribute>
        <xsl:text>*</xsl:text>
        <xsl:number/>
      </a>
      <xsl:text>. </xsl:text>
      <xsl:apply-templates/>
    </p>
  </xsl:template>
  <xsl:template match="tei:p">
    <p>
      <xsl:apply-templates/>
    </p>
  </xsl:template>
  <xsl:template match="tei:pb">
    <div class="pb">p. <xsl:value-of select="./@n"/></div>
  </xsl:template>
  <xsl:template match="tei:physDesc">
    <xsl:if test="./tei:objectDesc/@form">
      <xsl:text>Form: </xsl:text>
      <xsl:value-of select="./tei:objectDesc/@form"/>
      <xsl:text>. </xsl:text>
    </xsl:if>
    <xsl:if test="./tei:objectDesc/tei:supportDesc/@material">
      <xsl:text>Material: </xsl:text>
      <xsl:value-of select="./tei:objectDesc/tei:supportDesc/@material"/>
      <xsl:text>. </xsl:text>
    </xsl:if>
    <xsl:if test="./tei:objectDesc/tei:supportDesc/tei:extent">
      <xsl:text>Extent: </xsl:text>
      <xsl:value-of select="./tei:objectDesc/tei:supportDesc/tei:extent"/>
      <xsl:text>. </xsl:text>
    </xsl:if>
  </xsl:template>
  <xsl:template match="tei:profileDesc">
    <div class="profileDesc">
      <xsl:comment>See the source file.</xsl:comment>
    </div>
  </xsl:template>
  <xsl:template match="tei:publicationStmt">
    <div class="publicationStmt">
      <xsl:text>Copyright © </xsl:text>
      <xsl:value-of select="tei:date"/>
      <xsl:text>, </xsl:text>
      <xsl:value-of select="tei:authority"/>. <xsl:value-of select="tei:availability"/>
    </div>
  </xsl:template>
  <xsl:template match="tei:quote">
    <q><xsl:apply-templates/></q>
  </xsl:template>
  <xsl:template match="tei:rdg" mode="apparatus">
    <p>
      <xsl:choose>
        <xsl:when test="./@varSeq">
          <xsl:value-of select="./@varSeq"/>
          <xsl:text>. </xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>• </xsl:text>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:apply-templates mode="apparatus"/>
      <xsl:choose>
        <xsl:when test="@hand">
          <xsl:text> (</xsl:text>
          <xsl:value-of select="@hand"/>
          <xsl:text>)</xsl:text>
        </xsl:when>
      </xsl:choose>
    </p>
  </xsl:template>
  <xsl:template match="tei:ref">
    <a target="_blank">
      <xsl:attribute name="href" select="@target"/>
      <xsl:apply-templates/>
    </a>
  </xsl:template>
  <xsl:template match="tei:respStmt">
    <xsl:value-of select="./tei:name"/> (<xsl:value-of select="./tei:resp"/>) </xsl:template>
  <xsl:template match="tei:revisionDesc">
    <div class="revisionDesc">
      <xsl:comment>See the source file.</xsl:comment>
    </div>
  </xsl:template>
  <xsl:template match="tei:row">
    <tr>
      <xsl:apply-templates/>
    </tr>
  </xsl:template>
  <xsl:template match="tei:sourceDesc">
    <div class="sourceDesc">
      <h2>Sources</h2>
      <xsl:for-each select="./node()">
        <div class="source">
          <xsl:apply-templates/>
        </div>
      </xsl:for-each>
    </div>
  </xsl:template>
  <xsl:template match="tei:supplied">
    <span class="supplied" title="supplied">
      <xsl:apply-templates/>
    </span>
  </xsl:template>
  <xsl:template match="tei:table">
    <table border="1">
      <xsl:apply-templates/>
    </table>
  </xsl:template>
  <xsl:template match="tei:teiHeader">
    <div class="header">
      <xsl:apply-templates select="tei:fileDesc"/>
      <xsl:apply-templates select="tei:encodingDesc"/>
      <xsl:apply-templates select="tei:profileDesc"/>
      <xsl:apply-templates select="tei:revisionDesc"/>
    </div>
  </xsl:template>
  <xsl:template match="tei:text">
    <div class="text">
      <xsl:apply-templates select="tei:front"/>
      <xsl:apply-templates select="tei:body"/>
      <xsl:apply-templates select="tei:back"/>
    </div>
  </xsl:template>
  <xsl:template match="tei:title">
    <xsl:choose>
      <xsl:when test="./@level='m'">
        <span class="italic">
          <xsl:apply-templates/>
        </span>
      </xsl:when>
      <xsl:when test="./@level='j'">
        <xsl:text>"</xsl:text>
        <xsl:apply-templates/>
        <xsl:text>"</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="tei:titleStmt">
    <div class="titleStmt">
      <h1>
        <xsl:value-of select="tei:title"/>
      </h1>
      <xsl:for-each select="tei:respStmt">
        <h2>
          <xsl:apply-templates select="."/>
        </h2>
      </xsl:for-each>
    </div>
  </xsl:template>
  <xsl:template match="tei:unclear">
    <xsl:choose>
      <xsl:when test="@cert='doubtful'">
        <span class="unclear-doubtful" title="unclear (doubtful)">
          <xsl:apply-templates/>
        </span>
      </xsl:when>
      <xsl:when test="@cert='unread'">
        <span class="unclear-unread" title="unclear (illegible)">
          <xsl:apply-templates/>
        </span>
      </xsl:when>
      <xsl:otherwise>
        <span class="default">
          <xsl:apply-templates/>
        </span>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
</xsl:stylesheet>