<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:tmp="http://www.wwp.neu.edu/temp/ns"
  xmlns:sch="http://purl.oclc.org/dsdl/schematron"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns="http://www.w3.org/1999/xhtml"
  exclude-result-prefixes="#all"
  xpath-default-namespace="http://www.tei-c.org/ns/1.0"
  version="3.0">

  <!--
      namespaces.xslt — Generate an XHTML table of information about
         namespace nodes in the input (which is intended to be
         P5/p5.xml, but could be any other XML file).
      Written 2022-03-14 by Syd Bauman.
      Copyright 2022 Syd Bauman and the Text Encoding Initiative
         Consortium, available for copying, use, modification,
         publication, distribution, etc. under the terms of the BSD
         2-Clause license.
         
      Dependency: The output XHTML relies on the WWP version of a
      Javascript sort-the-table routine. If that routine is not
      available the table will not be sortable, but will still be
      viewable.
  -->
  
  <xsl:output method="xhtml"/>

  <xsl:param name="T1_header_color" select="'#D0E1F2'"/>
  <xsl:param name="T2_header_color" select="'#E1F2D0'"/>
  <xsl:param name="T1_bkgrnd_color" select="'#F7F9FB'"/>
  <xsl:param name="T2_bkgrnd_color" select="'#F9FBF7'"/>
  <xsl:param name="inputFileName" select="tokenize( base-uri(/), '/')[last()]"/>

  <!--
      Summary of construct names used herein:
        $NSns = namespace nodes
        $NSgrps = namespace nodes, grouped
        $tmp:ns = temporary namespace elements
        $tmp:nss = temporary namespaces elements
        $tmp:nsus = temporary namespace URI elements
  -->

  <!-- 
       First, create a sequence of <tmp:ns> elements, one for each
       namespace (node, declared on <sch:ns>, or declared on
       tei:*/@ns) in the input. Each <tmp:ns> element contains the
       needed info about the namespace: its prefix (on @pfx), the
       actual namespace URI (on @uri), and a combination of the two
       for use as a sorting or grouping key (on @both).
  -->
  <xsl:variable name="NSns" as="element(tmp:ns)+">
    <xsl:apply-templates select="//namespace-node()"/>
    <xsl:apply-templates select="//sch:ns"/>
    <xsl:apply-templates select="//tei:*[@ns]"/>
  </xsl:variable>

  <!-- 
       Next, group the nodes into sets based on both the namespace URI
       and the prefix assigned, and generate a single <tmp:nss>
       element for each such set, which element contains the needed
       information about this set (or group) of namespaces: their
       prefix (on @pfx), their namespace URI (on @uri), and the count
       of namespaces (i.e. of <tmp:ns>) that have that particular
       combination of prefix and namespace URI.
  -->
  <xsl:variable name="NSgrps" as="element(tmp:nss)+">
    <xsl:for-each-group select="$NSns" group-by="@both">
      <tmp:nss
        pfx="{current-group()[1]/@prefix}"
        uri="{current-group()[1]/@uri}"
        cnt="{count( current-group() )}"/>
    </xsl:for-each-group>    
  </xsl:variable>

  <!-- 
       Next, group the nodes into sets based on only the namespace
       URI, and generate a single <tmp:nss> element for each such set,
       which element contains the needed information about this set
       (or group) of namespaces: their prefixes (on @pfx), their
       namespace URI (on @uri), and the count of namespaces
       (i.e. <tmp:ns>) that have that particular combination namespace
       URI.
  -->
  <xsl:variable name="NSURIgrps" as="element(tmp:nsus)+">
    <xsl:for-each-group select="$NSns" group-by="@uri">
      <xsl:variable name="prefixes" select="current-group()/@prefix => distinct-values()"/>
      <tmp:nsus
        pfx="{for $prefix in $prefixes return concat( $prefix, ': ')}"
        uri="{current-group()[1]/@uri}"
        cnt="{count( current-group() )}"/>
    </xsl:for-each-group>    
  </xsl:variable>
  
  <!-- ********************************** -->

  <xsl:template match="/">
    <xsl:text>&#x0A;</xsl:text>
    <html xmlns="http://www.w3.org/1999/xhtml">
      <xsl:call-template name="head"/>
      <body>
        <xsl:call-template name="preTables"/>
        <xsl:call-template name="table1"/>
        <hr style="margin: 1em 0em 1em 0em;"/>
        <xsl:call-template name="table2"/>
        <xsl:call-template name="postTables"/>
      </body>
    </html>
  </xsl:template>

  <!-- ********************************** -->

  <xsl:template name="head">
    <head>
      <title xsl:expand-text="yes">Namespaces in {$inputFileName}</title>
      <script type="application/javascript"
              src="http://www.wwp.neu.edu/utils/bin/javascript/sorttable.js">
      </script>
      <style type="text/css">
        body { padding: 2em; }
        table#one > thead { background-color: <xsl:value-of select="$T1_header_color"/>; padding: 0.6ex; }
        table#two > thead { background-color: <xsl:value-of select="$T2_header_color"/>; padding: 0.6ex; }
        table#one { background-color: <xsl:value-of select="$T1_bkgrnd_color"/>; }
        table#two { background-color: <xsl:value-of select="$T2_bkgrnd_color"/>; }
        tbody > tr > td { padding: 0.5ex 1.0ex 0.5ex 1.0ex; }
        td.uri { font-family: monospace; font-size: small; }
        td.seq, td.cnt { text-align: right; font-family: monospace; }
        td.pfx { text-align: right; }
        dl > dt { font-weight: bold; }
        dl > dt:after { content:": " }
        dl > dd { padding: 0ex 0ex 0.5ex 0ex; }
      </style>
    </head>
  </xsl:template>
  
  <xsl:template name="preTables">
    <h1 xsl:expand-text="yes">Namespaces in {$inputFileName}</h1>
    <p>Collection of all namespace nodes, namespaces declared on
    <tt>&lt;sch:ns></tt> elements, and namespaces declared on TEI
    tagset documentation elements found in the input document. See
    colophon (below tables) for version information. Click on a column
    header to sort by that field.</p>
    <p>Columns:</p>
    <dl>
      <dt>seq #</dt>
      <dd>Essentially an arbitrary number to make it easier for humans to refer to a particular row of the table.</dd>
      <dt style="background-color: {$T1_header_color};">count</dt>
      <dd>The number of times this namespace URI occurs in the input document.</dd>
      <dt style="background-color: {$T2_header_color};">count</dt>
      <dd>The number of times this namespace URI occurs bound to this namespace prefix in the input document.</dd>
      <dt>namespace</dt>
      <dd>The namespace URI of the namespace node(s).</dd>
      <dt style="background-color: {$T1_header_color};">prefixes</dt>
      <dd>The namespace prefixes used for this URI (in reverse alphabetic order).</dd>
      <dt style="background-color: {$T2_header_color};">prefix</dt>
      <dd>The namespace prefix of the namespace node.</dd>
    </dl>
  </xsl:template>
  
  <xsl:template name="table1">
    <table id="one" class="sortable" border="2">
      <thead>
        <tr>
          <th>seq #</th>
          <th>count</th>
          <th>namespace</th>
          <th>prefixes</th>
        </tr>
      </thead>
      <tbody>
        <xsl:call-template name="table1Body"/>
      </tbody>
    </table>
  </xsl:template>

  <xsl:template name="table2">
    <table id="two" class="sortable" border="2">
      <thead>
        <tr>
          <th>seq #</th>
          <th>count</th>
          <th>namespace</th>
          <th>prefix</th>
        </tr>
      </thead>
      <tbody>
        <xsl:call-template name="table2Body"/>
      </tbody>
    </table>
  </xsl:template>
  
  <xsl:template name="postTables">
    <hr/>
    <h2>Colophon</h2>
    <dl xsl:expand-text="yes">
      <dt>this document created</dt>
      <dd>{current-dateTime()}</dd>
      <dt>input file</dt>
      <dd>{base-uri(/)}</dd>
      <dt>stylesheet</dt>
      <dd>{static-base-uri()}</dd>
      <dt>input version</dt>
      <dd>{/TEI/teiHeader/fileDesc/editionStmt/edition/ref[2]}</dd>
      <dt>input git commit</dt>
      <dd>{/TEI/teiHeader/fileDesc/editionStmt/edition/ref[3]}</dd>
      <dt>input date</dt>
      <dd>{/TEI/teiHeader/fileDesc/editionStmt/edition/date/@when}</dd>
    </dl>
  </xsl:template>

  <!-- ********************************** -->

  <xsl:template name="table1Body">
    <!-- Process each <tmp:nss> element into an <html:tr> for output. -->
    <xsl:apply-templates select="$NSURIgrps"/>
  </xsl:template>
  
  <xsl:template name="table2Body">
    <!-- Process each <tmp:nss> element into an <html:tr> for output. -->
    <xsl:apply-templates select="$NSgrps"/>
  </xsl:template>

  <!-- ********************************** -->

  <xsl:template match="namespace-node()">
    <tmp:ns prefix="{name(.)}" uri="{.}" both="{concat( ., '%%', name(.) )}"/>
  </xsl:template>
  
  <xsl:template match="sch:ns">
    <tmp:ns prefix="{@prefix}" uri="{@uri}" both="{@uri||'%%'||@prefix}"/>
  </xsl:template>
  
  <xsl:template match="tei:*">
    <tmp:ns prefix="" uri="{@ns}" both="{@uri||'%%'}"/>
  </xsl:template>

  <!-- ********************************** -->
  
  <xsl:template match="tmp:nss">
    <tr xsl:expand-text="yes">
      <td class="seq">{position()}</td>
      <td class="cnt">{@cnt}</td>
      <td class="uri">{@uri}</td>
      <td class="pfx">{if ( @pfx != '') then @pfx||':' else '&#xA0;'}</td>
    </tr>
  </xsl:template>

  <xsl:template match="tmp:nsus">
    <tr xsl:expand-text="yes">
      <td class="seq">{position()}</td>
      <td class="cnt">{@cnt}</td>
      <td class="uri">{@uri}</td>
      <td class="pfx">{string-join( tokenize( @pfx ) => sort() => reverse(), ',&#x20;')}</td>
    </tr>
  </xsl:template>
  
</xsl:stylesheet>
