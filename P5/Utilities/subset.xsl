<xsl:stylesheet version="3.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns="http://www.tei-c.org/ns/1.0"
  xpath-default-namespace="http://www.tei-c.org/ns/1.0">
  <!-- © 2020 TEI-C. See near end of file for licensing info. -->
  <!--
    Read in p5.xml (the TEI P5 Guidelines source XML), and write out
    a subset thereof that contains only:
     * the metadata (that is, the <teiHeader>),
     * the division structure (each <div> with just its attributes and
       heading),
     * the bibliography structure (each bibliographic entry with just
       its @xml:id), and most importantly
     * the specification elements.
    The output (typically referred to as "p5subset.xml") is intended
    for use in ODD processing that produces schemas and reference
    documentation, for which the 6+ MiB of prose is not needed.
  -->
  <!--
    Note: the xmlns default namespace declaration on <stylesheet> is
    not technically needed, but is there just to make explicit that
    both the input and output are TEI.
  -->
  <!--
    Written years ago, probably in XSLT 1.0, by Sebastian Rahtz.
    Re-written in XSLT 3.0 2020-05-10/11 by Syd Bauman.

    ――Revision Hx――
    2020-08-19 by Syd Bauman: Retain shells of bibliographic entries
    in case something (that is kept) points to them.
    2020-08-29 by Syd Bauman: Turns out we can't keep indent=yes, as
    Saxon (both 9.9 and 10.0) inserts a whitespace-only text node at
    the end of a <gloss> or <desc> (and probably anything else) if it
    happens to end with a child element.
    2020-08-30 by Syd Bauman: Better commenting. Also just copy @xml:id
    of bibliographic entries rather than processing them.
    2021-10-01 by Syd Bauman: Per #2187, add <constraintSpec> to list
    for which we keep_important_stuff_with_contents.
  -->

  <xsl:output method="xml" indent="no"/>
  
  <!-- In most cases, skip the element, but consider its children: -->
  <xsl:mode on-no-match="shallow-skip"/>
  
  <!-- The important parts get copied over in their entirety: -->
  <xsl:template name="keep_important_stuff_with_contents"
                match="teiHeader
                     | elementSpec
                     | macroSpec
                     | classSpec
                     | moduleSpec
                     | dataSpec
                     | constraintSpec ">
    <xsl:text>&#x0A;</xsl:text>
    <xsl:copy-of select="." /> <!--Comments & PIs inside me are copied, too-->
  </xsl:template>

  <!-- Each main structural element is kept, but its contents is kept
       or ditched based on its own merit[1] -->
  <xsl:template match="TEI | text | front | body | back">
    <xsl:text>&#x0A;</xsl:text>
    <xsl:copy>
      <xsl:apply-templates/>
    </xsl:copy>
  </xsl:template>

  <!-- Keep stub bibliography entries just in case someone points to them -->
  <xsl:template match="listBibl/bibl[@xml:id]|listBibl/biblStruct[@xml:id]">
    <xsl:text>&#x0A;</xsl:text>
    <bibl>
      <xsl:copy-of select="@xml:id"/>
      <xsl:comment>
        <xsl:text> This &lt;bibl> is a stub </xsl:text>
        <xsl:if test="self::biblStruct">
          <xsl:text>that is actually a &lt;biblStruct> in the source </xsl:text>
        </xsl:if>
      </xsl:comment>
    </bibl>
  </xsl:template>
  
  <!--
    Process divisions just so we have a structure thereof; keep the
    element itself and its heading, but keep the rest of its contents
    based on its own merit[1]
  -->
  <xsl:template match="div" name="keep_skeleton">
    <xsl:text>&#x0A;</xsl:text>
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:copy-of select="head"/>
      <xsl:apply-templates/>
    </xsl:copy>
  </xsl:template>

  <!--
      Footnote [1]: That is, throw away or keep the content based on
      the rest of this stylesheet — keep that which is matched by the
      keep_important_stuff_with_contents template, above, toss the
      rest.
  -->

</xsl:stylesheet>

<!--
    Copyright 2020 TEI-Consortium
    Available under The 2-Clause BSD License
    (https://opensource.org/licenses/BSD-2-Clause):

    Redistribution and use in source and binary forms, with or
    without modification, are permitted provided that the following
    conditions are met:

    1. Redistributions of source code must retain the above
       copyright notice, this list of conditions and the
       following disclaimer.

    2. Redistributions in binary form must reproduce the above
       copyright notice, this list of conditions and the following
       disclaimer in the documentation and/or other materials 
       provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
    CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES,
    INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
    MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
    BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
    EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
    TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
    ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
    TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
    THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
    SUCH DAMAGE.
-->
