<xsl:stylesheet 
  version="3.0"
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
  
  <xsl:output method="xhtml" indent="yes" encoding="UTF-8"/>
  
  <xsl:variable name="doc" select="/"/>
  
  <xsl:template match="/">
    <html>
      <head>
        <title>Attributes duplicated in classes</title>
        <meta name="generated_by" content="P5/Utilities/listduplatts.xsl"/>
        <style type="text/css">
          td { vertical-align: top; padding: 1ex; }
          body { margin: 2em; background-color: #F0F2F4; }
          table { background-color: white; }
          thead > tr > th { padding: 0.5ex 0ex 0.5ex 0ex; background-color: #F4F2F0; }
          li { padding: 1ex 0em 0em 0em; }
          td.n { vertical-align: middle; }
          
          span.att:before { content: "@"; }
          span.att { font-family: Monaco, monospace; font-size: 80%; }
          
          span.val:before { content: '"'; }
          span.val { font-family: Monaco, monospace; font-size: 80%; }
          span.val:after { content: '"'; }
          
          span.gi { font-family: Monaco, monospace; font-size: 80%; }
          span.gi:after { content: '>'; }
          span.gi:before { content: '&lt;'; }
          
          span.code { font-family: Monaco, monospace; font-size: 80%; }
          
          span.ident { font-family: Monaco, monospace; font-size: 80%; }
          
          span.term { font-style: italic; }
          
          span.mentioned { quotes: '"' '"' "'" "'"; }
          :lang(en) > span.mentioned { quotes: '\201C' '\201D' '\2018' '\2019' }
          :lang(fr) > span.mentioned { quotes: '« ' ' »' }
          :lang(de) > span.mentioned { quotes: '»' '«' '\2039' '\203A' }
          span.mentioned:before { content: open-quote; }
          span.mentioned:after { content: close-quote; }
          
          span.q { quotes: '"' '"' "'" "'"; }
          :lang(en) > span.q { quotes: '\201C' '\201D' '\2018' '\2019' }
          :lang(fr) > span.q { quotes: '« ' ' »' }
          :lang(de) > span.q { quotes: '»' '«' '\2039' '\203A' }
          span.q:before { content: open-quote; }
          span.q:after { content: close-quote; }
          
          span.soCalled { quotes: '"' '"' "'" "'"; }
          :lang(en) > span.soCalled { quotes: '\201C' '\201D' '\2018' '\2019' }
          :lang(fr) > span.soCalled { quotes: '« ' ' »' }
          :lang(de) > span.soCalled { quotes: '»' '«' '\2039' '\203A' }
          span.soCalled:before { content: open-quote; }
          span.soCalled:after { content: close-quote; }
          
          

        </style>
      </head>
      <body>
        <p style="text-align:center; background-color: white; padding: 1ex 1ex 1ex 1ex;">
          <xsl:text>Generated </xsl:text>
          <xsl:value-of select="normalize-space( substring-before( adjust-dateTime-to-timezone( current-dateTime(),'PT00H' cast as xs:dayTimeDuration ) cast as xs:string, '.') )"/>
          <xsl:text>Z from P5 “</xsl:text>
          <xsl:value-of select="normalize-space( $doc/TEI/teiHeader/fileDesc/editionStmt )"/>
          <xsl:text>”.</xsl:text>
        </p>
        <!-- generate list of duplicated attr names -->
        <xsl:variable name="duplicated_all" as="xs:string*">
          <xsl:for-each select="//attDef[ tei:dupl( @ident ) ]">
            <xsl:sort select="@ident"/>
            <xsl:value-of select="@ident"/>
          </xsl:for-each>
        </xsl:variable>
        <xsl:variable name="duplicated_uniqe" select="distinct-values( $duplicated_all )"/>
        <table border="1">
          <thead>
            <tr>
              <th>#: attr</th>
              <th>classes</th>
              <th>elements</th>
            </tr>
          </thead>
          <tbody>
            <xsl:for-each select="$duplicated_uniqe">
              <xsl:variable name="me" select="."/>
              <tr>
                <!-- column 1: row # for reference and name of attr -->
                <td class="n">
                  <xsl:value-of select="concat( position(),': @')"/>
                  <a style="color:blue;" href="http://www.tei-c.org/release/doc/tei-p5-doc/en/html/REF-ATTS.html#{$me}">
                    <xsl:value-of select="$me"/>
                  </a>
                </td>
                <!-- column 2: classes which have this attr -->
                <td class="class">
                  <ol type="A">
                    <xsl:for-each select="$doc//classSpec//attDef[ @ident eq $me ]">
                      <xsl:sort select="ancestor::classSpec/@ident"/>
                      <xsl:variable name="class" select="ancestor::classSpec/@ident"/>
                      <li>
                        <a style="color:green;" href="http://www.tei-c.org/release/doc/tei-p5-doc/en/html/ref-{$class}.html">
                          <xsl:value-of select="$class"/>
                        </a>
                        <xsl:text>: </xsl:text>
                        <xsl:apply-templates select="desc[ lang('en') ][1]"/>
                      </li>
                    </xsl:for-each>
                  </ol>
                </td>
                <!-- column 3: elements which have this attr -->
                <td class="gi">
                  <ol type="1">
                    <xsl:for-each select="$doc//elementSpec//attDef[ @ident eq $me ]">
                      <xsl:sort select="ancestor::elementSpec/@ident"/>
                      <xsl:variable name="gi" select="ancestor::elementSpec/@ident"/>
                      <li>
                        <a style="color:purple;" href="http://www.tei-c.org/release/doc/tei-p5-doc/en/html/ref-{$gi}.html">
                          <xsl:value-of select="$gi"/>
                        </a>
                        <xsl:text>: </xsl:text>
                        <xsl:apply-templates select="desc[ lang('en') ][1]"/>
                      </li>
                    </xsl:for-each>
                  </ol>
                </td>
              </tr>
            </xsl:for-each>
          </tbody>
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
    <xsl:sequence select="
      if (
           $doc//elementSpec//attDef[ @ident eq $ident  and  not( @mode ) ]
           and
           $doc//classSpec//attDef [  @ident eq $ident  and  not( @mode ) ]
         )
      then true()
      else false()"/>
  </xsl:function>
  
  <!-- templates for phrase-level elements within <desc> -->
  <xsl:template match="ptr|ref">
    <a href="@target">
      <xsl:value-of select="if ( normalize-space(.) eq '' ) then @target else normalize-space(.)"/>
    </a>
  </xsl:template>
  
  <xsl:template match="*">
    <span class="{local-name(.)}"><xsl:apply-templates/></span>
  </xsl:template>
  
</xsl:stylesheet>
