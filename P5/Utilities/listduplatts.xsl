<xsl:stylesheet 
  version="2.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:teix="http://www.tei-c.org/ns/Examples"
  xmlns:xs="http://www.w3.org/2001/XMLSchema" 
  xpath-default-namespace="http://www.tei-c.org/ns/1.0">
  
  <!-- Read in a complete schema specification (i.e. p5.xml, p5subset.xml, or -->
  <!-- a compiled customization), and write out a list of attrs that are both -->
  <!-- in a class and locally defined. -->
  <!-- modified 2016-05-03 by Syd: -->
  <!-- a) add header comment -->
  <!-- b) to put some metadata in output -->
  <!-- c) sort classname/attr column on attr name 1st (not class name 1st) -->
  <!-- d) drop 1st column ("State") and last (empty) column ("Elements in which this attribute occurs")-->
  <!-- e) fix bug in finding descriptions (we now use @xml:lang even on English) -->
  <!-- f) change column header names -->
  <!-- g) add a *tiny* bit more CSS to make a bit more readable -->
  <!-- f) add metadata in footer -->
  
  <xsl:output method="html" indent="yes" encoding="utf-8"/>
  
  <xsl:variable name="doc" select="/"/>
  
  <xsl:template match="/">
    <html>
      <head>
        <title>Attributes duplicated in classes</title>
        <meta name="generated_by" content="P5/Utilities/listduplatts.xsl"/>
        <style type="text/css">
          td { vertical-align: top; padding: 1ex; }
          body { margin: 1em; }
        </style>
      </head>
      <body>
        <p style="text-align:center;">
          <xsl:text>Generated </xsl:text>
          <xsl:value-of select="normalize-space( substring-before( current-dateTime() cast as xs:string, '.') )"/>
          <xsl:text>Z from P5 “</xsl:text>
          <xsl:value-of select="normalize-space( $doc/TEI/teiHeader/fileDesc/editionStmt )"/>
          <xsl:text>”.</xsl:text>
        </p>
        <table border="1">
          <thead>
            <tr>
              <th>class name / attribute</th>
              <th>class description</th>
              <th>locally defined attrs</th>
            </tr>
          </thead>          
          <xsl:variable name="x">
            <xsl:for-each select="//classSpec//attDef[tei:dupl(@ident)]">
              <xsl:sort select="@ident"/>
              <xsl:sort select="ancestor::classSpec/@ident"/>
              <att xmlns="http://www.tei-c.org/ns/1.0" desc="{desc[lang('en')][1]}" me="{@ident}" them="{ancestor::classSpec/@ident}"/>
            </xsl:for-each>
          </xsl:variable>
          <xsl:for-each select="$x/att">
            <xsl:variable name="me" select="@me"/>
            <tr>
              <td><a href="http://www.tei-c.org/release/doc/tei-p5-doc/en/html/ref-{@them}"><xsl:value-of select="@them"/></a>
                <xsl:text>/@</xsl:text>
                <span style="color:green"><xsl:value-of select="$me"/></span>
              </td>
              <td><xsl:value-of select="@desc"/></td>
              <td>
                <ol>
                  <xsl:for-each select="$doc//elementSpec//attDef[@ident=$me and not(@mode)]">
                    <xsl:sort select="ancestor::elementSpec/@ident"/>
                    <li>
                      <a href="http://www.tei-c.org/release/doc/tei-p5-doc/en/html/ref-{ancestor::elementSpec/@ident}.html"><xsl:value-of select="ancestor::elementSpec/@ident"/></a>:
                      <xsl:value-of select="desc[lang('en')][1]"/></li>
                  </xsl:for-each>
                </ol>
              </td>
            </tr>
          </xsl:for-each>
        </table>
        <hr style="margin: 1em;"/>
        <p>Note: this table does not include <tt>&lt;attDef></tt>s that have an <tt>@mode</tt>:        </p>
        <ul>
          <xsl:for-each select="//attDef[ @mode ]">
            <li style="font-family: monospace;">
              <xsl:value-of select="concat(
                ancestor::*[@ident][1]/@ident,
                '/@',
                @ident
                )"/>
            </li>
          </xsl:for-each>
        </ul>
      </body>
    </html>
  </xsl:template>
  
  <xsl:function name="tei:dupl" as="xs:boolean">
    <xsl:param name="ident"/>
    <xsl:sequence
      select="if ( $doc//elementSpec//attDef[@ident eq $ident  and  not( @mode ) ] )
      then true()
      else false()"/>
  </xsl:function>
  
</xsl:stylesheet>
