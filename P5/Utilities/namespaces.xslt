<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:tmp="http://www.wwp.neu.edu/temp/ns"
  xmlns:math="http://www.w3.org/2005/xpath-functions/math"
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

  <xsl:template match="/">
    <xsl:text>&#x0A;</xsl:text>
    <html xmlns="http://www.w3.org/1999/xhtml">
      <xsl:call-template name="head"/>
      <body>
        <xsl:call-template name="preTable"/>
        <xsl:call-template name="table"/>
        <xsl:call-template name="postTable"/>
      </body>
    </html>
  </xsl:template>

  <!-- ********************************** -->

  <xsl:template name="head">
    <head>
      <title>Namespaces in P5</title>
      <script type="application/javascript"
              src="http://www.wwp.neu.edu/utils/bin/javascript/sorttable.js">
      </script>
      <style type="text/css">
        body { padding: 2em; }
        thead { background-color: #D0E1F2; padding: 0.6ex; }
        tbody > tr > td { padding: 0.5ex 1.0ex 0.5ex 1.0ex; }
        td.uri { font-family: monospace; font-size: small; }
        td.seq, td.cnt { text-align: right; font-family: monospace; }
        td.pfx { text-align: right; }
      </style>
    </head>
  </xsl:template>
  
  <xsl:template name="preTable">
    <h1>Namespaces in P5</h1>
    <p>Namespace nodes in p5.xml. See colophon (below table) for
    version information. Click on a column header to sort by that
    field.</p>
    <p>Columns:</p>
    <dl>
      <dt>seq #</dt>
      <dd>Essentially an arbitrary number to make it easier for humans to refer to a particular row of the table.</dd>
      <dt>count</dt>
      <dd>The number of times this namespace URI occurs bound to this namespace prefix in the input document.</dd>
      <dt>namespace</dt>
      <dd>The namespace URI of a namespace node.</dd>
      <dt>prefix</dt>
      <dd>The namespace prefix a namespace node.</dd>
    </dl>
  </xsl:template>
  
  <xsl:template name="table">
    <table class="sortable" border="2">
      <thead>
        <tr>
          <th>seq #</th>
          <th>count</th>
          <th>namespace</th>
          <th>prefix</th>
        </tr>
      </thead>
      <tbody>
        <xsl:call-template name="tableBody"/>
      </tbody>
    </table>
  </xsl:template>

  <xsl:template name="postTable">
    <hr/>
    <h2>Colophon</h2>
    <dl xsl:expand-text="yes">
      <dt>this document created</dt>
      <dd>{current-dateTime()}</dd>
      <dt>input file</dt>
      <dd>{document-uri(/)}</dd>
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

  <xsl:template name="tableBody">
    <!--
        Summary of construct names used herein:
          $NSns = namespace nodes
          $NSgrps = namespace nodes, grouped
          $tmp:ns = temporary namespace elements
          $tmp:nss = temporary namespaces elements
    -->
    <!-- 
         First, create a sequence of <tmp:ns> elements, one for each
         namespace node in the input. Each element contains the needed
         info about the namespace: its prefix (on @pfx), the actual
         namespace URI (on @uri), and a combination of the two for use
         as a sorting or grouping key (on @both).
    -->
    <xsl:variable name="NSns" as="element(tmp:ns)+">
      <xsl:apply-templates select="//namespace-node()">
        <xsl:sort select="."/>
        <!-- Not necessary to sort; I just imagine it is faster that way -->
      </xsl:apply-templates>
    </xsl:variable>
    <!-- 
         Next, group the nodes into sets based on both the namespace
         URI and the prefi assigned, and generate a single <tmp:nss>
         element for each such set, which element contains the needed
         information about this set (or group) of namespaces: their
         prefix (on @pfX), their namespace URI (on @uri), and the
         count of namespaces (i.e. <tmp:ns>) that have that particular
         combination of prefi and namespace URI.
    -->
    <xsl:variable name="NSgrps" as="element(tmp:nss)+">
      <xsl:for-each-group select="$NSns" group-by="@both">
        <tmp:nss
          pfx="{current-group()[1]/@prefix}"
          uri="{current-group()[1]/@uri}"
          cnt="{count( current-group() )}"/>
      </xsl:for-each-group>    
    </xsl:variable>
    <!-- Process each <tmp:nss> element into an <html:tr> for output. -->
    <xsl:apply-templates select="$NSgrps"/>
  </xsl:template>

  <xsl:template match="namespace-node()">
    <tmp:ns prefix="{name(.)}" uri="{.}" both="{concat( ., '%%', name(.) )}"/>
  </xsl:template>

  <xsl:template match="tmp:nss">
    <tr xsl:expand-text="yes">
      <td class="seq">{position()}</td>
      <td class="cnt">{@cnt}</td>
      <td class="uri">{@uri}</td>
      <td class="pfx">{if ( @pfx != '') then @pfx||':' else '&#xA0;'}</td>
    </tr>
  </xsl:template>

</xsl:stylesheet>
