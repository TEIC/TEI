<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" exclude-result-prefixes="#all"
  xpath-default-namespace="http://www.tei-c.org/ns/1.0"
  xmlns="http://www.tei-c.org/ns/1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:tei="http://www.tei-c.org/ns/1.0"
  xmlns:xi="http://www.w3.org/2001/XInclude"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  xmlns:sch="http://purl.oclc.org/dsdl/schematron"  
  >

  <xsl:variable name="myName" select="'TEI-to-tei_customization.xslt'"/>
  <xsl:variable name="version" select="'0.9.0b'"/>
  <xsl:param name="versionDate" select="format-date(current-date(),'[Y]-[M01]-[D01]')"/>

  <!--
      ** generator for tei_customization,
      ** aka TOCTOC (TEI ODD Customization for writing TEI ODD Customizations),
      ** aka odd4odds (ODD for writing ODDs)
      ** Written by Syd Bauman
      ** Copyleft 2016 Syd Bauman, TEI Consortium, and Women Writers Project
      ** 
      ** To use this program, run it with the source to TEI P5 as
      ** input. The output will be an ODD file which should be 
      ** submitted to an ODD processor to generate schemas (and
      ** custom documentation) for use in writing ODD customization
      ** files.
      **
      ** Changes to this program which produce significant changes
      ** to the output ODD (like adding a new element) should be
      ** documented in that which becomes the <revisionDesc> of the
      ** output ODD, which is in the definition of the XSLT variable
      ** $revisionDesc, near the top of the file.
      ** Changes that produce cosmetic, minor, or no change to the
      ** output ODD should be documented in the CHANGE LOG block
      ** comment, immediately below.
  -->
  <!--
      *********************** CHANGE LOG for XSLT changes *********************** 
      2017-11-05 by Syd: Harumph. Found that our output is not valid against
        p5odds.rng because ident/@type is not allowed to be "test", so I removed
        the attribute.
      2017-04-07 by Syd: bug fixes:
        * allow <classSpec> to be empty
        * actually enforce constraints listed in remarks for <schemaSpec>
      2017-01-16 by Syd: embarrassing — copy over useful bits from last time I
        did the exact same thing. That was 2016-06-10/12, immediately before the
        advanced TEI class James & I did at DHSI. I had entirely forgotten I had
        already done this. Sigh.
      *********************** segnahc TLSX rof GOL EGNAHC ***********************
  -->

  <xsl:variable name="revisionDesc">
    <revisionDesc>
      <change who="#sbauman.emt" when="2024-04-12">
	Added <gi>sch:rule</gi> elements PRN to avoid new warning
	about contextless Schematron.
      </change>
      <change who="#sbauman.emt" when="2023-06-06">
        <list>
          <item>Remove the <ident>altIdent-only-NCName</ident>
          constraint, as it is no longer needed — the content of
          <gi>altIdent</gi> in P5 is now just <ident>xs:NCName</ident>.</item>
          <item>update to use version 4.6.0 of P5</item>
          <item>Per <ref
          target="https://github.com/TEIC/TEI/issues/2285">TEI ticket
          #2285</ref> disallow <gi>altIdent</gi> as a direct child of
          <gi>classSpec</gi>, <gi>constraintSpec</gi>,
          <gi>dataSpec</gi>, <gi>macroSpec</gi>; thus leaving it as
          only available as a child of <gi>attDef</gi>,
          <gi>elementSpec</gi>, and <gi>valItem</gi>. In order to do
          this, remove <gi>altIdent</gi> from <name
          type="class">model.identSynonyms</name> (leaving that class
          with only <gi>gloss</gi> and <gi>equiv</gi>, i.e. the same
          as <name type="class">model.identEquiv</name>, sigh) and add
          it back to <gi>attDef</gi> and <gi>valItem</gi>.</item>
        </list>
      </change>
      <change who="#sbauman.emt" when="2023-04-04">
        We will soon not allow more than 1 child of <gi>content</gi>,
        so updated the content model of <gi>schemaSpec</gi> to have
        only 1 child (in this case, <gi>sequence</gi>).
      </change>
      <change who="#sbauman.emt" when="2022-06-25">
        Since <ref
        target="https://github.com/TEIC/TEI/issues/1735">TEI ticket
        #1735</ref> has now addressed the <code>( content | valList
        )*</code> portion of the content models of <gi>dataSpec</gi>
        and <gi>macroSpec</gi> (changing them to <code>( content |
        valList )?</code>), we no longer need to do that here. Thus
        undo most of the changes of 2018-07-19.
      </change>
      <change who="#sbauman.emt" when="2022-04-03">
        Motivated by <ref
        target="https://github.com/TEIC/Stylesheets/issues/136">#136</ref>,
        restrict <att>docLang</att> of <gi>schemaSpec</gi> to only 1 value.
      </change>
      <change who="#sbauman.emt" when="2020-10-29">
        Per <ref
        target="https://github.com/TEIC/TEI/issues/2050">#2050</ref>,
        replace <val>nonfatal</val> with <val>warning</val> or
        <val>information</val> on <att>role</att> attributes. Turns
        out there is only one such case (on <val>tei-source</val>),
        which I changed to <val>information</val>. Also fix the
        comment that precedes that rule, as P5 no longer has the
        att.readFrom class.
      </change>
      <change who="#sbauman.emt" when="2019-11-08">
        As part of working through <ref
        target="https://github.com/TEIC/Stylesheets/issues/402">Stylesheets
        ticket #402</ref>, Martin Holmes and I thought that an
        <gi>elementSpec</gi> addressing a TEI element should not be in
        mode <val>add</val>. This is intended to enforce that rule.
      </change>
      <change who="#sbauman.emt" when="2018-07-19">
        Further addressing <ref
        target="https://github.com/TEIC/TEI/issues/1735">TEI ticket
        #1735</ref>: constrain content of both <gi>dataSpec</gi> and
        <gi>macroSpec</gi> so that at most 1 <gi>content</gi> or 1
        <gi>valList</gi> child is permitted. While there give the
        <att>ident</att> attribute a semi-controlled vocabulary.
      </change>
      <change who="#sbauman.emt" when="2018-02-02">
        <list>
          <item>update to use version 3.3.0 of P5</item>
          <item>format $versionDate so it does not have the timezone appended</item>
          <item>use $versionDate in <gi>date</gi> in TEI Header, too (instead of
          calling current-date() again)</item>
        </list>
      </change>
      <change who="#sbauman.emt" when="2018-01-22">
        Per <ref
        target="https://github.com/TEIC/TEI/issues/1735">#1735</ref>,
        add constraints on <gi>content</gi> child of
        <gi>elementSpec</gi>:
        <list>
          <item>if <att>mode</att> of <gi>elementSpec</gi> is
          <val>add</val> or <val>replace</val>, require a
          <gi>content</gi></item>
          <item>if <att>mode</att> of <gi>elementSpec</gi> is
          <val>delete</val>, require said <gi>elementSpec</gi> to be
          empty</item>
        </list>
      </change>
      <change who="#sbauman.emt" when="2017-11-09">
        <list>
          <item>Add constraint to check that <att>xml:id</att> values are unique. We
          do this because this file (and the RELAX NG produced from it) needs to be
          validated with ID/IDREF checking turned off. Since TEI does not use IDREFs
          you might think that it is not a problem to turn it off, but that is also
          the feature that checks for ID uniqueness. So we test for it separately.</item>
          <item>Add <att>xml:lang</att> and a somewhat bogus <att>versionDate</att> (set
            from a parameter) to various <gi>gloss</gi>, <gi>desc</gi>, and <gi>remarks</gi>
            elements as required by tei_odds schema.</item>
        </list>
      </change>
      <change who="#sbauman.emt" when="2017-01-18">
        Change <ident>altIdent-only-NCName</ident> to test value for a colon
        if <emph>not</emph> a child of <gi>taxonomy</gi> or <gi>valItem</gi> (rather than test
        that value is castable as NCName if it <emph>is</emph> a child of an ODD element other
        than <gi>valItem</gi>, because some processors don't like the NCName test).
      </change>
      <change who="#sbauman.emt" when="2017-01-16">
        <list>
          <item>Bug fix: check for <gi>specGrpRef</gi> was checking that it
            pointed to <gi>elementSpec</gi>, not <gi>specGrp</gi></item>
          <item>Remove our definition of <gi>content</gi>, as TEI’s now does what
            we want.</item>
          <item>Remove constraints testing for both <att>url</att> and <att>key</att>
            on <gi>moduleRef</gi>, and for both <att>include</att> and <att>except</att>
            on either <gi>moduleRef</gi> or <gi>classRef</gi>, as the TEI mechanism
            for doing so (an <gi>attList</gi> with <att>org</att>=<val>choice</val>)
            seems to be working now.</item>
        </list>
      </change>
      <change who="#sbauman.emt" when="2017-01-15">
        <list>
          <item>Change generation system — create odd4odds.odd from a single XSLT
            transform run with TEI P5 is input</item>
          <item>Update driver file to P5 3.1.0 (from 2.7.0)</item>
          <item>Remove declaration of content of <gi>schemaSpec</gi>, as P5 now allows
            <gi>constraintSpec</gi>.</item>
          <item>Convert to Pure ODD</item>
        </list>
      </change>
      <change who="#sbauman.emt" when="2016-06-05">
        <list>
          <item>restore deleted <ident type="class">model.resourceLike</ident></item>
          <item>allow <gi>funder</gi></item>
          <item>use <gi>dataRef</gi> instead of <gi>macroRef</gi> inside
            <gi>datatype</gi></item>
          <item>test the <att>source</att> attr of <gi>dataRef</gi>, too</item>
          <item>lots of whitespace tweaks just to make more readable</item>
          <item>delete two remaining references to non-existent <att>allowText</att></item>
          <item>loosen the <ident>mode-child-sanity</ident> constraint to consider an attribute
            other than <att>mode</att> or <att>ident</att> to be a <soCalled>child</soCalled>.
            This may be a bit too loose, may want to expand list of attrs that are
            <code>except</code>ed in the test, and thus flag an error</item>
        </list>
      </change>
      <change who="#sbauman.emt" when="2015-06-19">
        Fix bug in <name>only-1-per</name> contraint (which was added
        674 days ago — why did this bug last even 1 day?)
      </change>
      <change who="#sbauman.emt" when="2015-01-24">
        Changes for <soCalled>Pure ODD</soCalled>:
        <list>
          <item>constrain content of <gi>content</gi></item>
          <item>delete <ident type="class">att.global.rendition</ident> and <ident
          type="class" >att.global.responsibility</ident></item>
          <item>delete <att>allowText</att> (as I think TEI has decided to go with
          <gi>textNode</gi> instead)</item>
          <item>require <att>key</att> of <gi>elementRef</gi></item>
          <item>change expression of all datatypes (which happen to all be <ident
          type="datatype">data.enumerated</ident>) from RNG to Pure ODD</item>
          <item>change expression of most of our content models from RNG to Pure ODD.</item>
      </list>
      </change>
      <change who="#sbauman.emt" when="2015-01-23">
        Changes for <soCalled>Pure ODD</soCalled>:
        <list>
          <item>constrain <att>key</att> of <gi>classRef</gi>, <gi>elementRef</gi>, and
          <gi>macroRef</gi></item>
          <item>constrain <att>include</att> and <att>except</att> of <gi>classRef</gi>, also
          flagging an error if both are present</item>
      </list>
      </change>
      <change who="#sbauman.emt" when="2015-01-22">
        <list>
          <item n="r25493">Re-generate based on current TEI (in preperation for conversion to
            <soCalled>Pure ODD</soCalled>)</item>
          <item>Allow <gi>editionStmt</gi> (and <gi>edition</gi>) in the
            <gi>teiHeader</gi></item>
          <item>Remove our check for colon in <att>prefix</att> of <gi>schemaSpec</gi> and
            <gi>elementSpec</gi>, as TEI no longer allows it.</item>
          <item>Removed our alterations to <att>type</att> of <gi>list</gi>, as TEI has improved
            it.</item>
        </list>
      </change>
      <change who="#sbauman.emt" when="2013-08-14">
        Add constraint <name>only-1-per</name>, which warns user iff
        there are more than 1 <gi>elementSpec</gi> with the same
        <att>ident</att>
      </change>
      <change who="#sbauman.emt" when="2013-05-09">
        <list>
          <item>warn if no <gi>schemaSpec</gi> or greater than 1 of 'em</item>
        </list>
      </change>
      <change who="#sbauman.emt" when="2013-05-08">
        <list>
          <item>warn against 2 <gi>moduleRef</gi>s with same <att>key</att></item>
          <item>check that the 7 required elements are included when <att>include</att> is used
            (we already check that they are not deleted or excepted)</item>
          <item>warn when an element with a <att>mode</att> of <val>delete</val> has
            children</item>
          <item>warn when an element with a <att>mode</att> of <val>change</val>,
            <val>add</val>, or <val>replace</val> has no children</item>
        </list>
      </change>
      <change who="#sbauman.emt" when="2013-05-04">
        <list>
          <item>Change name</item>
          <item>Add health warning about using <q>Check ID/IDREF</q> in oXygen</item>
        </list>
      </change>
      <change who="#sbauman.emt" when="2013-03-09">
        Added <gi>sch:pattern</gi> to list of elements allowed inside
        <gi>constraint</gi>.
      </change>
      <change who="#sbauman.emt" when="2012-06-17">
        Remove the new <gi>notatedMusic</gi> and <gi>gb</gi> elments.
      </change>
      <change who="#sbauman.emt" when="2011-09-20">
        <list>
          <item>neaten up a bit for readers of this XML file</item>
          <item>allow <gi>xi:fallback</gi> to have XML mixed content</item>
          <item>check that <gi>moduleRef</gi> that uses <att>url</att> has a (new!)
            <att>prefix</att></item>
        </list>
      </change>
      <change who="#sbauman.emt" when="2011-09-05">
        Test that required elements are not removed.
      </change>
      <change who="#sbauman.emt" when="2011-09-04">
        <list>
          <item>changed check for colons in <att>prefix</att> of <gi>schemaSpec</gi> from a
            Schematron warning to a RELAX NG constraint</item>
          <item>warn if the value of <att>prefix</att> of <gi>moduleRef</gi> matches the
            <att>prefix</att> of anything else</item>
        </list>
      </change>
      <change who="#sbauman.emt" when="2011-09">
        Beefed up prose, then corrections per Julia, including
        changing name of language (and thus <att>prefix</att>).
      </change>
      <change who="#sbauman.emt" when="2011-09-01">
        <list>
          <item>remove addition of <name type="class">att.identifiable</name> to <name
            type="class">att.combineable</name>, as it's there already in current P5</item>
          <item>remove (extraneous?) <gi scheme="Schematron">rule</gi> elements that have no
            <att>context</att></item>
          <item>retro-write several change log entries</item>
        </list>
      </change>
      <change who="#sbauman.emt" when="2011-08-30">
        <list>
          <item>make <att>mode</att> of <gi>classes</gi> required</item>
          <item>issue warning if we find a <gi>classSpec</gi>, <gi>elementSpec</gi>, or a
            <gi>macroSpec</gi> outside of <gi>schemaSpec</gi>, and not referenced from within
            <gi>schemaSpec</gi></item>
          <item>bug fix: <att>include</att> of <gi>moduleRef</gi> had been a <val>semi</val>
            list &#x2014; changed to <val>closed</val></item>
          <item>require <att>ident</att> of <gi>classSpec</gi></item>
          <item>require <att>ident</att> of <gi>elementSpec</gi></item>
        </list>
      </change>
      <change who="#sbauman.emt" when="2011-08-29">
        Constrain <att>ident</att> of <gi>classSpec</gi>.
      </change>
      <change who="#sbauman.emt" when="2011-08-27">
        During workshop:
        <list>
          <item>constrain <att>start</att> of <gi>schemaSpec</gi></item>
          <item>constrain <att>key</att> of <gi>elmentRef</gi></item>
          <item>improve remakrs of <gi>constraint</gi> so that it correctly reflects that we
          only permit ISO Schematron</item>
          <item>constrain content of <gi>altIdent</gi>, unless it is a child of
          <gi>valItem</gi></item>
        </list>
      </change>
      <change who="#sbauman.emt" when="2011-08-25">
        <list>
          <item>Now that stylesheets support sucking in multiple external schemas that happen to
            have pattern name collision, and references to patterns within them, take advantage
            of it by using real RELAX NG and ISO Schematron schemas.</item>
          <item>Add <gi>persName</gi>, <gi>placeName</gi>, and <gi>orgName</gi>.</item>
        </list>
      </change>
      <change who="#sbauman.emt" when="2011-08-24">
        <list>
          <item>add this <gi>revisionDesc</gi></item>
          <item>add <gi>body</gi> as a starting root element</item>
          <item>made lists into XIncludes</item>
        </list>
      </change>
      <change who="#sbauman.emt" when="2011-01-18">
        <list>
          <item>remove <gi>scriptNote</gi></item>
          <item>add check for required modules</item>
          <item>delete model.entryPart.top</item>
          <item>delete model.msItemPart</item>
          <item>delete model.personPart</item>
          <item>delete model.titlepagePart</item>
          <item>put att.identifiable into att.combinable (reflect a change in P5, maybe?)</item>
        </list>
      </change>
      <change who="#sbauman.emt" from="2010-08-16" to="2010-08-25">various</change>
      <change who="#sbauman.emt" when="2010-08-16">first crack</change>
    </revisionDesc>
  </xsl:variable>

  <!-- Comment on Schematron messages:                                                                   -->
  <!-- Many, of not most, Schematron processors will not put a '<' out in a message: they put out '&gt;' -->
  <!-- instaed. Thus I personally like surrounding element names in <gi>, or U+FE64 and U+FE65. But      -->
  <!-- oXygen does not show those characters, so here I use U+FF1C and U+FF1E instead.                   -->

  <xsl:output method="xml" indent="yes"/>
  
  <!-- ======================== -->
  <!-- list generation routines -->
  <!-- ======================== -->

  <!-- ******** -->
  <!-- elements -->
  <!-- ******** -->
  <xsl:variable name="elements">
    <xsl:apply-templates select="//elementSpec">
      <xsl:sort select="@ident"/>
    </xsl:apply-templates>
  </xsl:variable>
  
  <xsl:variable name="elements-module-in-desc">
    <xsl:apply-templates select="//elementSpec" mode="mod-desc">
      <xsl:sort select="@ident"/>
    </xsl:apply-templates>
  </xsl:variable>
  
  <!-- ********** -->
  <!-- attributes -->
  <!-- ********** -->
  <xsl:variable name="atts-from-classes_class-in-desc">
    <xsl:apply-templates select="//classSpec//attDef">
      <xsl:sort select="@ident"/>
    </xsl:apply-templates>
  </xsl:variable>
  
  <!-- ******* -->
  <!-- modules -->
  <!-- ******* -->
  <xsl:variable name="modules">
    <xsl:apply-templates select="//moduleSpec">
      <xsl:sort select="@ident"/>
    </xsl:apply-templates>
  </xsl:variable>
  
  <!-- ******* -->
  <!-- classes -->
  <!-- ******* -->
  <xsl:variable name="classes">
    <xsl:apply-templates select="//classSpec">
      <xsl:sort select="@ident"/>
    </xsl:apply-templates>
  </xsl:variable>

  <!-- ****** -->
  <!-- macros -->
  <!-- ****** -->
  <xsl:variable name="macros">
    <xsl:apply-templates select="//macroSpec">
      <xsl:sort select="@ident"/>
    </xsl:apply-templates>
  </xsl:variable>

  <!-- ********* -->
  <!-- datatypes -->
  <!-- ********* -->
  <xsl:variable name="datatypes">
    <xsl:apply-templates select="//dataSpec">
      <xsl:sort select="@ident"/>
    </xsl:apply-templates>
  </xsl:variable>

  <!-- ****** -->
  <!-- others -->
  <!-- ****** -->
  <xsl:template name="element-is-in-module">
    <constraintSpec scheme="schematron" ident="element-is-in-module">
      <constraint>
        <xsl:for-each select="//moduleSpec/@ident">
          <xsl:variable name="this" select="."/>
          <xsl:if test="$this ne 'tei'">
            <xsl:variable name="them" select="//elementSpec[@module eq $this]/@ident"/>
            <xsl:variable name="gilist" select="concat('`',string-join( distinct-values( $them ), '`, `'),'`')"/>
            <xsl:variable name="GILIST" select='translate( $gilist, "`", "&apos;")'/>
            <sch:rule context="tei:moduleRef[@key eq '{.}']">
              <sch:let name="include" value="tokenize( normalize-space(@include),' ')"/>
              <sch:let name="except"  value="tokenize( normalize-space(@except), ' ')"/>
              <sch:assert test="every $gi in $include satisfies $gi = ( {$GILIST} )">One or more of the elements included on the '<sch:value-of
                select="@key"/>' ＜moduleRef＞ are not actually available in that module</sch:assert>
              <sch:assert test="every $gi in $except  satisfies $gi = ( {$GILIST} )">One or more of the elements excepted on the '<sch:value-of
                select="@key"/>' ＜moduleRef＞ are not actually available in that module</sch:assert>
            </sch:rule>
          </xsl:if>
        </xsl:for-each>
      </constraint>
    </constraintSpec>
  </xsl:template>

  <!-- *************************************** -->
  <!-- list generation subroutines, as it were -->
  <!-- *************************************** -->
  <xsl:template match="attDef">
    <valItem ident="{@ident}">
      <xsl:apply-templates select="child::gloss[ lang('en') ]"/>
      <desc xml:lang="en" versionDate="{$versionDate}">
        <xsl:text>class: </xsl:text>
        <xsl:value-of select="ancestor::classSpec/@ident"/>
      </desc>
    </valItem>
  </xsl:template>
  
  <xsl:template match="classSpec | dataSpec | elementSpec | moduleSpec | macroSpec">
    <valItem ident="{@ident}">
      <xsl:apply-templates select="child::gloss[ lang('en') ]"/>
    </valItem>
  </xsl:template>

  <xsl:template match="classSpec | dataSpec | elementSpec " mode="mod-desc">
    <valItem ident="{@ident}">
      <xsl:apply-templates select="child::gloss[ lang('en') ]"/>
      <desc xml:lang="en" versionDate="{$versionDate}">
        <xsl:text>module: </xsl:text>
        <xsl:value-of select="@module"/>
      </desc>
    </valItem>
  </xsl:template>

  <!-- ***************************************************** -->
  <!-- pretty identity (used for things inside <desc>, e.g.) -->
  <!-- ***************************************************** -->
  <xsl:template match="node()" mode="#all">
    <xsl:if test="not(ancestor::*)">
      <xsl:text>&#x0A;</xsl:text>
    </xsl:if>
    <xsl:copy>
      <xsl:apply-templates select="@* | node()" mode="#current"/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="@*" mode="#all">
    <xsl:copy/>
  </xsl:template>
  
  <!-- ************ -->
  <!-- main routine -->
  <!-- ************ -->
  <xsl:template match="/TEI">
    <xsl:comment> ************************ WARNING ************************************* </xsl:comment>
    <xsl:comment> WARNING: This is a derived file. Edit the source XSLT program and run  </xsl:comment>
    <xsl:comment> it against P5 again to make changes. See &lt;sourceDesc> and &lt;appInfo>    </xsl:comment>
    <xsl:comment> for info on the XSLT program that was used.                            </xsl:comment>
    <xsl:comment> ********************************************************************** </xsl:comment>
    <TEI xmlns="http://www.tei-c.org/ns/1.0"
      xmlns:xi="http://www.w3.org/2001/XInclude"
      xmlns:rng="http://relaxng.org/ns/structure/1.0"
      xmlns:sch="http://purl.oclc.org/dsdl/schematron"
      version="4.6.0">
      <teiHeader>
        <fileDesc>
          <titleStmt>
            <title>TEI ODD Customization for writing TEI ODD Customizations</title>
            <author>Syd Bauman</author>
            <sponsor>
              <orgName>Women Writers Project</orgName>
            </sponsor>
          </titleStmt>
          <publicationStmt>
            <publisher>Text Encoding Initiative Consortium</publisher>
            <date when="{$versionDate}"/>
            <availability status="restricted">
              <p>Copyright 2017 Syd Bauman and Northeastern WWP; some rights reserved.</p>
              <p>This TEI-encoded ODD file is available under the terms of the <ref
                target="http://www.opensource.org/licenses/bsd-license.php">OSI BSD 2-clause
                License</ref>, to wit:
                <quote>
                  <p>Redistribution and use in source and derived forms, with or without
                    modification, are permitted provided that the following conditions are met: <list>
                      <item>Redistributions of source code must retain the above copyright notice,
                        this list of conditions, and the following disclaimer.</item>
                      <item>Redistributions in binary <note>or other derived</note> form must
                        reproduce the above copyright notice, this list of conditions, and the
                        following disclaimer in the documentation or other materials provided with
                        the distribution.</item>
                    </list>
                    <note>Note, however, that although modification and redistribution of
                      derivatives of this ODD (e.g., the schemas that result from processing with
                      <ref target="http://www.tei-c.org/Roma/">Roma</ref>) is legal, the resulting
                      schema is (definitionally) not TEI-conformant.</note></p>
                  <p>This software is provided by the copyright holders and contributors
                    <soCalled>as is</soCalled> and any express or implied warranties, including,
                    but not limited to, the implied warranties of merchantability and fitness for a
                    particular purpose are disclaimed. In no event shall the copyright holder or
                    contributors be liable for any direct, indirect, incidental, special, exemplary,
                    or consequential damages (including, but not limited to, procurement of
                    substitute goods or services; loss of use, data, or profits; or business
                    interruption) however caused and on any theory of liability, whether in
                    contract, strict liability, or tort (including negligence or otherwise) arising
                    in any way out of the use of this software, even if advised of the possibility
                    of such damage.</p>
                </quote>
              </p>
            </availability>
          </publicationStmt>
          <sourceDesc>
            <p>Generated from <name type="file">TEI-to-tei_customization.xslt</name>, which is based on
              the Women Writers Project’s <name type="file">odd4odds_driver.odd</name>, itself based on
              the TEI Consoritum’s <name type="file">tei_odds.odd</name> by Sebastian Rahtz,
              which can be found on <ref
                target="https://github.com/TEIC/TEI/blob/dev/P5/Exemplars/tei_odds.odd"
                >GitHub</ref></p>
          </sourceDesc>
        </fileDesc>
        <encodingDesc>
          <appInfo>
            <application ident="{$myName}" version="{$version}">
              <desc>This ODD file was generated by running
                <xsl:value-of select="$myName"/> against TEI P5
              “<xsl:value-of select="teiHeader/fileDesc/editionStmt/edition"/>”,
                found at <xsl:value-of select="document-uri(/)"/> on
              <xsl:value-of select="current-dateTime()"/></desc>
            </application>
          </appInfo>
        </encodingDesc>
        <xsl:copy-of select="$revisionDesc"/>
      </teiHeader>
      <text>
        <body>
          <div>
            <head>Introduction</head>
            <p>The primary intent of this schema is to help you and
            your XML editor <emph>write</emph> a customization ODD; it
            in no way pretends to be the last arbiter of what is or is
            not allowed in an ODD file in general, nor even in a TEI
            customization ODD. Thus it makes lots of assumptions
            (read: enforces lots of constraints) that
            <emph>probably</emph> make sense when writing a
            customization schema, but may not in your particular case.
            Thus this schema may well flag as invalid things that are
            perfectly reasonable to have in your ODD.</p>
            <div>
              <head>Background</head>
            <p>The TEI ODD language was designed both for the creation
            and customization of the <title>TEI Guidelines</title>,
            and also for the creation (and perhaps customization) of
            other, non-TEI, markup languages. Thus the TEI ODD
            langauge is, by default, much more flexible than needed
            for writing TEI customization ODDs. For example, the
            Guidelines define the <att>key</att> attribute of the
            <gi>moduleRef</gi> element as any XML name (without a
            namespace prefix, i.e. an <code>xsd:NCName</code>), even
            though the only possible values when used to customize TEI
            are the 20 or so module names defined in the <title>TEI
            Guidelines</title>.</p>
            <p>Of course, when using Roma<note>The canonical
            installation of Roma is available at <ref
            target="http://www.tei-c.org/Roma/">the TEI-C site</ref>,
            but it is an open source tool available on <ref
            target="https://github.com/TEIC/Roma">GitHub</ref>, which
            may be installed on any GNU/Linux system.</note>, a
            web-based front-end editor for ODD files, this is not a
            problem. The web form gives the user only the appropriate
            TEI values to choose from. However, when editing ODD files
            by hand, and thus when teaching TEI customization, it is
            much more efficient to catch errors like mis-spelled
            module names before handing the ODD file to an ODD
            processor (e.g. the aforementioned <name
            type="program">Roma</name> or <ref
            target="https://github.com/TEIC/Stylesheets/blob/dev/bin/teitorelaxng">teitorelaxng</ref>).</p>
            <p>Thus the Women Writers Project has developed this TEI
            customization for the purpose of having a schema to use
            that deliberately makes it easier to write a TEI
            customization, at the expense of the complete flexibility
            ODD provides.</p>
            <p>This schema permits a valid document to use a variety
            of elements as the root element. This is for debugging and
            file maintenance convenience. As always, any conforming
            TEI ODD must have either <gi>TEI</gi> or
            <gi>teiCorpus</gi> as the root element.</p>
            <p>REMINDER: This language is not canonical &#x2014; it is
            intended to be helpful, not definitive</p>
            </div>
          </div>
          
          <div>
            <head>Technical Information</head>
            <!-- combine indirection, differences, 2nd warning here;
            add a how-to-use section that says something like "While
            we can talk about TEI constraint as if it was only one
            step, in fact, like most TEI customizations, validation
            requires two distinct steps: RNC &amp; SCH. Here's how you
            can set oXygen to do both" kinda stuff. -->
            <p>Of course the lists of elements, classes, and on rare
            occasion even modules change as TEI P5 matures. Rather
            than requiring that this customization be manually edited
            with each release of TEI P5, the ODD for this
            customization is itself generated from a source file. To
            generate this customization, an <ref
            target="https://github.com/TEIC/TEI/blob/dev/P5/Utilities/TEI-to-tei_customization.xslt">XSLT
            stylesheet</ref> reads in the source to TEI P5, and from
            it generates the lists of elements, classes, and modules
            needed. The output of the stylesheet is the ODD file for this
            customization.</p>
            <p>We call this TEI language the <name>TEI ODD Customization for writing TEI ODD
                Customizations</name> language (or TOCTOC for short).
              It is instantited in the TEI exemplar <name type="file">tei_customization</name>,
              which differs from <name
                  type="file">tei_odds</name><note>Available in <ref
                    target="https://raw.githubusercontent.com/TEIC/TEI/dev/P5/Exemplars/tei_odds.odd"
                    >the main TEI P5 git repository
                at GitHub</ref></note> in a variety of ways, including:
              <list>
                <item>constraining the elements available (e.g., <gi>gap</gi> and
                    <gi>imprimatur</gi> are not available)</item>
                <item>requiring some otherwise optional attributes (e.g., <att>mode</att> of
                    <gi>classes</gi>, or <att>module</att> on <gi>elementSpec</gi> unless the
                    <att>mode</att> is specified as <val>add</val>)</item>
                <item>limiting the content of <gi>constraint</gi> to ISO Schematron, and thus
                  permitting validation of the Schematron therein</item>
                <item>constraining the values of several attributes</item>
                <item>requiring that the four required modules be imported</item>
                <item>requiring that <gi>altIdent</gi> be an NCName (i.e., not have a prefix) unless
                it is the child of <gi>valItem</gi> (or <gi>taxonomy</gi>)</item>
                <item>flagging <tag>*Spec</tag> elements that are neither in the <gi>schemaSpec</gi>
                  nor referred to from a <gi>specGrpRef</gi> within it</item>
                <item>warning if required elements (e.g., <gi>teiHeader</gi>) are deleted</item>
                <item>using, and encouraging the use of, the
                <att>prefix</att> attribute of
                <gi>moduleRef</gi></item>
              </list>
            </p>
              <p>If editing an XML document associated with this schema in <ref
                  target="http://oxygenxml.com/">oXygen</ref>, you will need to uncheck the <name
                  type="cmd">Check ID/IDREF</name> preference box. It can be found in <name
                  type="cmd">Options > Preferences > XML > XML Parser > RELAX NG</name>.</p>
          </div>
          <div>
            <head>Schema Specification</head>
            <schemaSpec ident="tei_customization" prefix="cust_"
              start="TEI body schemaSpec elementSpec classSpec macroSpec div">
              <xsl:text>&#x0A;</xsl:text>
              <desc>a schema to help <emph>writing</emph> TEI
              customization ODDs; will (incorrectly) flag extensions
              as invalid</desc>
              <xsl:comment> required modules minus lots of elements that probably don't make sense in an ODD </xsl:comment>
              <xsl:text>&#x0A;</xsl:text>
              <moduleRef key="tei"/>
              <moduleRef key="header"
                except="appInfo application cRefPattern catDesc catRef category classCode classDecl correction creation editorialDecl geoDecl handNote hyphenation interpretation keywords langUsage language namespace normalization profileDesc quotation refState refsDecl rendition samplingDecl segmentation stdVals tagUsage tagsDecl taxonomy textClass typeNote scriptNote"/>
              <moduleRef key="core"
                except="add analytic biblStruct binaryObject cb corr del distinct gap gb index l lb lg listBibl measure measureGrp monogr milestone orig pb reg series sic sp speaker stage teiCorpus unclear"/>
              <moduleRef key="textstructure"
                except="argument byline closer dateline div1 div2 div3 div4 div5 div6 div7 docAuthor docDate docEdition docImprint docTitle epigraph floatingText group imprimatur opener postscript salute signed titlePage titlePart trailer"/>
              <xsl:comment> for an ODD, tagdocs is required too </xsl:comment>
              <moduleRef key="tagdocs"/>
              <xsl:comment> access to the RELAX NG and ISO Schematron languages </xsl:comment>
              <xsl:comment> Note: end user may need to change paths </xsl:comment>
              <xsl:comment> (Here they are relative paths to where they would be found from TEIC/TEI/P5/Exmplars/.) </xsl:comment>
              <moduleRef url="./relaxng.rng" prefix="rng_"/>
              <moduleRef url="../iso-schematron.rng" prefix="sch_"/>
              <xsl:comment> allow use of the specialized naming elements, as many people are used to them </xsl:comment>
              <moduleRef key="namesdates" include="persName placeName orgName"/>
              <xsl:comment> allow tables, figures, and formulæ </xsl:comment>
              <moduleRef key="figures" except="notatedMusic"/>
              
              <constraintSpec scheme="schematron" ident="mode-child-sanity">
                <constraint>
                  <sch:rule context="*[ @mode eq 'delete' ]">
                    <sch:report test="child::*">The specification element ＜<sch:name/>＞ has both a
                      mode= of "delete" and child elements, which is incongruous</sch:report>
                  </sch:rule>
                  <sch:rule context="
                      tei:valList[ @mode = ('add','change','replace') ]
                    | tei:moduleSpec[ @mode = ('add','change','replace') ]
                    | tei:schemaSpec[ @mode = ('add','change','replace') ]
                    | tei:elementSpec[ @mode = ('add','change','replace') ]
                    | tei:classSpec[ @mode = ('add','change','replace') ]
                    | tei:macroSpec[ @mode = ('add','change','replace') ]
                    | tei:constraintSpec[ @mode = ('add','change','replace') ]
                    | tei:attDef[ @mode = ('add','change','replace') ]
                    | tei:classes[ @mode = ('add','change','replace') ]">
                    <sch:assert test="child::* | (@* except (@mode, @ident))">The specification
                      element ＜<sch:name/>＞ has a mode= of "<sch:value-of select="@mode"/>", but
                      does not have any child elements or schema-changing attributes, which is
                      incongruous</sch:assert>
                  </sch:rule>
                </constraint>
                <!-- put extra sanity check here? -->
              </constraintSpec>

              <elementSpec module="tagdocs" ident="schemaSpec" mode="change">
                <content>
                  <sequence>
                    <elementRef key="gloss" minOccurs="0" maxOccurs="1"/>
                    <elementRef key="desc"  minOccurs="1" maxOccurs="1"/>
                    <alternate minOccurs="0" maxOccurs="unbounded">
                      <classRef key="model.oddRef"/>
                      <classRef key="model.oddDecl"/>
                    </alternate>
                  </sequence>
                </content>
                <constraintSpec scheme="schematron" ident="required-modules">
                  <gloss>required modules</gloss>
                  <constraint>
                    <sch:rule context="tei:schemaSpec">
                      <sch:assert test="
                        ( tei:moduleRef[ @key eq 'tei']
                          or
                          tei:specGrpRef[ id( substring-after( normalize-space( @target ), '#') )/tei:moduleRef[ @key eq 'tei'] ] )                      
                        and
                          ( tei:moduleRef[ @key eq 'core']
                          or
                          tei:specGrpRef[ id( substring-after( normalize-space( @target ), '#') )/tei:moduleRef[ @key eq 'core'] ] )                      
                        and
                          ( tei:moduleRef[ @key eq 'header']
                          or
                          tei:specGrpRef[ id( substring-after( normalize-space( @target ), '#') )/tei:moduleRef[ @key eq 'header'] ] )                      
                        and
                          ( tei:moduleRef[ @key eq 'textstructure']
                          or
                          tei:specGrpRef[ id( substring-after( normalize-space( @target ), '#') )/tei:moduleRef[ @key eq 'textstructure'] ] )                      
                      "> missing one or more of the required modules (tei, core, header, textstructure). </sch:assert>
                    </sch:rule>
                  </constraint>
                </constraintSpec>
                <constraintSpec scheme="schematron" ident="no-outside-specs">
                  <desc>A <tag>*Spec</tag> element should either be within <gi>schemaSpec</gi>,
                    or be in a <gi>specGrp</gi> referred to by a <gi>specGrpRef</gi> wihin
                    <gi>schemaSpec</gi>.</desc>
                  <constraint>
                    <sch:rule context="tei:classSpec[ not( ancestor::tei:schemaSpec ) ]
                                     | tei:elementSpec[ not( ancestor::tei:schemaSpec ) ]
                                     | tei:macroSpec[ not( ancestor::tei:schemaSpec ) ]">
                      <sch:assert test="//tei:schemaSpec//tei:specGrpRef
                                        [ substring-after( normalize-space( @target ), '#') 
                                          eq
                                          current()/ancestor::tei:specGrp/@xml:id ]"
                        >＜<sch:name/>＞ should be in ＜schemaSpec＞ or referred to from within
                        ＜schemaSpec＞</sch:assert>
                    </sch:rule>
                  </constraint>
                </constraintSpec>
                <constraintSpec scheme="schematron" ident="only-one-schemaSpec">
                  <desc>TEI permits <gi>schemaSpec</gi> as a
                  repeatable child of a variety of elements (including
                  <gi>front</gi>, <gi>body</gi>, <gi>back</gi>,
                  <gi>encodingDesc</gi>, <gi>div</gi>, or any numbered
                  division element). But <name type="cmd">roma</name>
                  only processes the first <gi>schemaSpec</gi> found
                  (in document order). So we just limit ourselves to 1
                  and only 1.</desc>
                  <constraint>
                    <sch:rule context="/">
                      <sch:report test="count( //tei:schemaSpec ) eq 0">There's no ＜schemaSpec＞, so
                        this isn't much of a TEI customization</sch:report>
                      <sch:report test="count( //tei:schemaSpec ) gt 1">You have more than one
                        ＜schemaSpec＞; current ODD processors will only look at the first
                        one</sch:report>
                    </sch:rule>
                  </constraint>
                </constraintSpec>
                <attList>
                  <attDef ident="docLang" mode="change">
                    <desc versionDate="2022-04-03" xml:lang="en">
                      specifies which language to use when creating
                      documentation if the description for an element,
                      attribute, class, or macro is available in more
                      than one language
                    </desc>
                    <datatype minOccurs="1" maxOccurs="1">
                      <dataRef key="teidata.language"/>
                    </datatype>
                    <remarks xml:lang="en" versionDate="{$versionDate}">
                      <p>The current TEI ODD processor only handles 1 value as the value of <att>docLang</att>.</p>
                    </remarks>
                  </attDef>
                  <attDef ident="start" mode="replace">
                    <desc xml:lang="en" versionDate="{$versionDate}">specifies entry points to the schema, i.e. which elements may be used as
                      the root of documents conforming to it.</desc>
                    <datatype minOccurs="0" maxOccurs="unbounded">
                      <dataRef key="teidata.enumerated"/>
                    </datatype>
                    <valList type="closed">
                      <xsl:copy-of select="$elements"/>
                    </valList>
                  </attDef>
                </attList>
                <remarks xml:lang="en" versionDate="{$versionDate}">
                  <p>The content of the tei_customization <gi>schemaSpec</gi> is somewhat more restrictive than
                    TEI. To wit, <list>
                      <item>neither <gi>altIdent</gi> nor <gi>equiv</gi> are permitted</item>
                      <item>there can be at most one <gi>gloss</gi>, which (if present) must precede
                        the <gi>desc</gi></item>
                      <item>there must be one and only one <gi>desc</gi></item>
                      <item>the four core modules must be included</item>
                      <item>the value of <att>start</att> requires values that are valid TEI element
                        names</item>
                    </list>
                  </p>
                </remarks>
              </elementSpec>

              <elementSpec module="tagdocs" ident="moduleRef" mode="change">
                <constraintSpec scheme="schematron" ident="if-url-then-prefix">
                  <desc>This is not strictly necessary. The TEI patterns have a default prefix (the
                    value of <att>ident</att> of <gi>schemaSpec</gi>), so if only one external
                    module is imported, it does not need a prefix — there will not be any collisions
                    of pattern names. But it is good practice to use a <att>prefix</att> anyway, and
                    it is required if more than one external module that use patterns with the same
                    name are included. This is the case, in fact, for the RELAX NG and ISO
                    Schematron schemas that are included in this schema.</desc>
                  <constraint>
                    <sch:rule context="tei:moduleRef[ @url ]">
                      <sch:assert test="@prefix"> a ＜moduleRef＞ that uses the url= attribute should
                        have a prefix= attribute, too </sch:assert>
                    </sch:rule>
                  </constraint>
                </constraintSpec>
                <constraintSpec scheme="schematron" ident="no-duplicate-modules">
                  <constraint>
                    <sch:rule context="tei:moduleRef[ @key ]">
                      <sch:let name="mykey" value="@key"/>
                      <sch:report test="preceding-sibling::tei:moduleRef[ @key eq $mykey ]">The
                          '<sch:value-of select="@key"/>' module is included (by reference) more
                        than once</sch:report>
                    </sch:rule>
                  </constraint>
                </constraintSpec>
                <constraintSpec scheme="schematron" ident="need-required">
                  <constraint>
                    <sch:rule context="tei:moduleRef[ @except ]">
                      <sch:let name="exceptions" value="tokenize( @except, '\s+' )"/>
                      <xsl:comment> We could get away with using a single test, i.e. </xsl:comment>
                      <xsl:comment> "('TEI','teiHeader','fileDesc','titleStmt','title','publicationStmt','sourceDesc')=$exceptions", </xsl:comment>
                      <xsl:comment> but then we wouldn't get individualized error msgs. </xsl:comment>
                      <xsl:comment> In either case, if &amp; when TEI changes the list of </xsl:comment>
                      <xsl:comment> elements which are *required*, we need to change this </xsl:comment>
                      <xsl:comment> code (in <xsl:value-of select="$myName"/>) manually. </xsl:comment>
                      <sch:report test="'TEI'=$exceptions">Removing ＜TEI＞ from your schema
                      guarantees it is not TEI conformant, and will will likely be outright
                      invalid</sch:report>
                      <sch:report test="'teiHeader'=$exceptions">Removing ＜teiHeader＞ from your
                      schema guarantees it is not TEI conformant</sch:report>
                      <sch:report test="'fileDesc'=$exceptions">Removing ＜fileDesc＞ from your
                      schema guarantees it is not TEI conformant</sch:report>
                      <sch:report test="'titleStmt'=$exceptions">Removing ＜titleStmt＞ from your
                      schema guarantees it is not TEI conformant</sch:report>
                      <sch:report test="'title'=$exceptions">Removing ＜title＞ from your schema
                      guarantees it is not TEI conformant</sch:report>
                      <sch:report test="'publicationStmt'=$exceptions">Removing
                      ＜publicationStmt＞ from your schema guarantees it is not TEI
                      conformant</sch:report>
                      <sch:report test="'sourceDesc'=$exceptions">Removing ＜sourceDesc＞ from
                      your schema guarantees it is not TEI conformant</sch:report>
                    </sch:rule>
                  </constraint>
                </constraintSpec>
                    <constraintSpec scheme="schematron" ident="include-required">
                      <constraint>
                        <sch:rule context="tei:moduleRef[ @key eq 'textstructure' and @include ]">
                          <sch:let name="inclusions" value="tokenize( @include, '\s+' )"/>
                          <sch:report test="not('TEI'=$inclusions)">Not including ＜TEI＞ in your
                            schema guarantees it is not TEI conformant, and will likely be outright
                            invalid</sch:report>
                        </sch:rule>
                        <sch:rule context="tei:moduleRef[ @key eq 'header' and @include ]">
                          <sch:let name="inclusions" value="tokenize( @include, '\s+' )"/>
                          <sch:report test="not('teiHeader'=$inclusions)">Not including ＜teiHeader＞
                            in your schema guarantees it is not TEI conformant</sch:report>
                        </sch:rule>
                        <sch:rule context="tei:moduleRef[ @key eq 'header' and @include ]">
                          <sch:let name="inclusions" value="tokenize( @include, '\s+' )"/>
                          <sch:report test="not('fileDesc'=$inclusions)">Not including ＜fileDesc＞ in
                            your schema guarantees it is not TEI conformant</sch:report>
                        </sch:rule>
                        <sch:rule context="tei:moduleRef[ @key eq 'header' and @include ]">
                          <sch:let name="inclusions" value="tokenize( @include, '\s+' )"/>
                          <sch:report test="not('titleStmt'=$inclusions)">Not including ＜titleStmt＞
                            in your schema guarantees it is not TEI conformant</sch:report>
                        </sch:rule>
                        <sch:rule context="tei:moduleRef[ @key eq 'header' and @include ]">
                          <sch:let name="inclusions" value="tokenize( @include, '\s+' )"/>
                          <sch:report test="not('publicationStmt'=$inclusions)">Not including
                            ＜publicationStmt＞ in your schema guarantees it is not TEI
                            conformant</sch:report>
                        </sch:rule>
                        <sch:rule context="tei:moduleRef[ @key eq 'header' and @include ]">
                          <sch:let name="inclusions" value="tokenize( @include, '\s+' )"/>
                          <sch:report test="not('sourceDesc'=$inclusions)">Not including
                            ＜sourceDesc＞ in your schema guarantees it is not TEI
                            conformant</sch:report>
                        </sch:rule>
                        <sch:rule context="tei:moduleRef[ @key eq 'core' and @include ]">
                          <sch:let name="inclusions" value="tokenize( @include, '\s+' )"/>
                          <sch:report test="not('title'=$inclusions)">Not including ＜title＞ in your
                            schema guarantees it is not TEI conformant</sch:report>
                        </sch:rule>
                      </constraint>
                    </constraintSpec>
                <xsl:call-template name="element-is-in-module"/>
                <attList org="group">
                  <attDef ident="include" mode="replace">
                    <desc xml:lang="en" versionDate="{$versionDate}">supplies a list of the elements which are to be copied from the specified
                      module into the schema being defined.</desc>
                    <datatype minOccurs="0" maxOccurs="unbounded">
                      <dataRef key="teidata.enumerated"/>
                    </datatype>
                    <!-- Note: constraint "include-required" should (in
                         some sense) be defined here, but was moved to
                         address #1950. —sb, 2019-12-06 -->
                    <valList type="closed">
                      <xsl:copy-of select="$elements"/>
                    </valList>
                  </attDef>
                  <attDef ident="except" mode="replace">
                    <desc xml:lang="en" versionDate="{$versionDate}">supplies a list of the elements which are not to be copied from the
                      specified module into the schema being defined.</desc>
                    <datatype minOccurs="0" maxOccurs="unbounded">
                      <dataRef key="teidata.enumerated"/>
                    </datatype>
                    <!-- Note: constraint "need-required" should (in
                         some sense) be defined here, but was moved to
                         address #1950. —sb, 2019-12-06 -->
                    <valList type="semi">
                      <xsl:copy-of select="$elements"/>
                    </valList>
                  </attDef>
                  <attDef ident="key" mode="replace">
                    <!-- The only reason we do not remove url= and require -->
                    <!-- key= is because we use <moduleRef> w/ url= in this -->
                    <!-- ODD; not because we ever teach url= in our workshops. -->
                    <datatype minOccurs="1" maxOccurs="1">
                      <dataRef key="teidata.enumerated"/>
                    </datatype>
                    <valList type="closed">
                      <xsl:copy-of select="$modules"/>
                    </valList>
                  </attDef>
                </attList>
              </elementSpec>

              <elementSpec module="tagdocs" ident="classRef" mode="change">
                <attList>
                  <attDef ident="key" mode="replace" usage="req">
                    <datatype minOccurs="1" maxOccurs="1">
                      <dataRef key="teidata.enumerated"/>
                    </datatype>
                    <valList type="semi">
                      <xsl:copy-of select="$classes"/>
                    </valList>
                  </attDef>
                  <attDef ident="include" mode="replace">
                    <desc versionDate="2011-09-21" xml:lang="en">supplies a list of class members
                      which are to be included in the schema being defined.</desc>
                    <datatype minOccurs="0" maxOccurs="unbounded">
                      <dataRef key="teidata.enumerated"/>
                    </datatype>
                    <valList type="semi">
                      <xsl:copy-of select="$atts-from-classes_class-in-desc"/>
                    </valList>
                  </attDef>
                  <attDef ident="except" mode="replace">
                    <desc versionDate="2011-09-21" xml:lang="en">supplies a list of class members
                      which are to be excluded from the schema being defined.</desc>
                    <datatype minOccurs="0" maxOccurs="unbounded">
                      <dataRef key="teidata.enumerated"/>
                    </datatype>
                    <valList type="semi">
                      <xsl:copy-of select="$atts-from-classes_class-in-desc"/>
                    </valList>
                  </attDef>
                </attList>
              </elementSpec>

              <elementSpec module="tagdocs" ident="macroSpec" mode="change">
                <attList>
                  <attDef ident="ident" mode="replace" usage="req">
                    <datatype minOccurs="1" maxOccurs="1">
                      <dataRef key="teidata.enumerated"/>
                    </datatype>
                    <valList type="semi">
                      <xsl:copy-of select="$macros"/>
                    </valList>
                  </attDef>
                </attList>
              </elementSpec>

              <elementSpec module="tagdocs" ident="macroRef" mode="change">
                <attList>
                  <attDef ident="key" mode="replace" usage="req">
                    <datatype minOccurs="1" maxOccurs="1">
                      <dataRef key="teidata.enumerated"/>
                    </datatype>
                    <valList type="semi">
                      <xsl:copy-of select="$macros"/>
                    </valList>
                  </attDef>
                </attList>
              </elementSpec>

              <elementSpec module="tagdocs" ident="classSpec" mode="change">
                <content>
                  <sequence preserveOrder="true">
                    <elementRef minOccurs="0" maxOccurs="1"         key="gloss"/>
                    <classRef   minOccurs="0" maxOccurs="1"         key="model.descLike"/>
                    <elementRef minOccurs="0" maxOccurs="1"         key="classes"/>
                    <elementRef minOccurs="0" maxOccurs="unbounded" key="constraintSpec"/>
                    <elementRef minOccurs="0" maxOccurs="1"         key="attList"/>
                    <elementRef minOccurs="0" maxOccurs="unbounded" key="exemplum"/>
                    <elementRef minOccurs="0" maxOccurs="unbounded" key="remarks"/>
                    <elementRef minOccurs="0" maxOccurs="unbounded" key="listRef"/>
                  </sequence>
                </content>
                <attList>
                  <attDef ident="ident" mode="replace" usage="req">
                    <datatype minOccurs="1" maxOccurs="1">
                      <dataRef key="teidata.enumerated"/>
                    </datatype>
                    <valList type="semi">
                      <xsl:copy-of select="$classes"/>
                    </valList>
                  </attDef>
                </attList>
                <remarks xml:lang="en" versionDate="{$versionDate}">
                  <p>The content of the tei_customization <gi>classSpec</gi> is somewhat more restrictive than
                    TEI. To wit, neither <gi>altIdent</gi> nor <gi>equiv</gi> are permitted, there
                    can be at most one <gi>gloss</gi> and must be one and only one
                    <gi>desc</gi>.</p>
                </remarks>
              </elementSpec>

              <elementSpec module="tagdocs" ident="dataSpec" mode="change">
                <attList>
                  <attDef ident="ident" mode="replace">
                    <datatype minOccurs="1" maxOccurs="1">
                      <dataRef key="teidata.enumerated"/>
                    </datatype>
                    <valList type="semi">
                      <xsl:copy-of select="$datatypes"/>
                    </valList>
                  </attDef>
                </attList>
              </elementSpec>

              <elementSpec module="tagdocs" ident="dataRef" mode="change">
                <attList>
                  <attDef ident="key" mode="replace">
                    <datatype minOccurs="1" maxOccurs="1">
                      <dataRef key="teidata.enumerated"/>
                    </datatype>
                    <valList type="closed">
                      <xsl:copy-of select="$datatypes"/>
                    </valList>
                  </attDef>
                </attList>
              </elementSpec>
                
              <elementSpec module="tagdocs" ident="elementRef" mode="change">
                <attList>
                  <attDef ident="key" mode="replace" usage="req">
                    <datatype minOccurs="1" maxOccurs="1">
                      <dataRef key="teidata.enumerated"/>
                    </datatype>
                    <valList type="closed">
                      <xsl:copy-of select="$elements"/>
                    </valList>
                  </attDef>
                </attList>
              </elementSpec>

              <elementSpec module="tagdocs" ident="classes" mode="change">
                <attList>
                  <attDef mode="change" ident="mode" usage="req"/>
                </attList>
              </elementSpec>

              <elementSpec module="tagdocs" ident="attDef" mode="change">
                <xsl:comment> adding altIdent to content model because it was removed from model.identSynonyms </xsl:comment>
                <content>
                  <sequence>      
                    <alternate minOccurs="0" maxOccurs="unbounded">
                      <elementRef key="altIdent"/>
                      <classRef key="model.identSynonyms"/>
                      <classRef key="model.descLike"/>
                    </alternate>
                    <elementRef key="datatype" minOccurs="0"/>
                    <elementRef key="constraintSpec" minOccurs="0" maxOccurs="unbounded"/>
                    <elementRef key="defaultVal" minOccurs="0"/>
                    <alternate minOccurs="0">
                      <elementRef key="valList"/>     
                      <elementRef key="valDesc" minOccurs="1" maxOccurs="unbounded"/>
                    </alternate>
                    <elementRef key="exemplum" minOccurs="0" maxOccurs="unbounded"/>
                    <elementRef key="remarks" minOccurs="0" maxOccurs="unbounded"/>
                  </sequence>
                </content>
              </elementSpec>

              <elementSpec module="tagdocs" ident="valList" mode="change">
                <attList>
                  <attDef mode="change" ident="type" usage="req"/>
                </attList>
              </elementSpec>

              <elementSpec module="tagdocs" ident="valItem" mode="change">
                <xsl:comment> adding altIdent to content model because it was removed from model.identSynonyms </xsl:comment>
                <content>
                  <sequence>
                    <alternate minOccurs="0" maxOccurs="unbounded">
                      <classRef key="model.identSynonyms"/>
                      <elementRef key="altIdent"/>
                    </alternate>
                    <sequence minOccurs="0">
                      <classRef key="model.descLike" minOccurs="1" maxOccurs="unbounded"/>
                      <sequence minOccurs="0" maxOccurs="unbounded">
                        <classRef key="model.identSynonyms" minOccurs="1" maxOccurs="1"/>
                        <classRef key="model.descLike" minOccurs="0" maxOccurs="unbounded"/>
                      </sequence>
                      <elementRef key="remarks" minOccurs="0" maxOccurs="unbounded"/>
                    </sequence>
                    <elementRef key="paramList" minOccurs="0" maxOccurs="1"/>
                  </sequence>
                </content>
              </elementSpec>

              <elementSpec module="tagdocs" ident="memberOf" mode="change">
                <attList>
                  <attDef ident="key" mode="replace">
                    <datatype minOccurs="1" maxOccurs="1">
                      <dataRef key="teidata.enumerated"/>
                    </datatype>
                    <valList type="semi">
                      <xsl:copy-of select="$classes"/>
                    </valList>
                  </attDef>
                </attList>
              </elementSpec>

              <elementSpec module="tagdocs" ident="elementSpec" mode="change">
                <content>
                  <xsl:comment> same as vanilla except impose order and max of </xsl:comment>
                  <xsl:comment> 1 each for members of glossLike and descLike </xsl:comment>
                  <sequence preserveOrder="true">
                    <elementRef key="altIdent"       minOccurs="0" maxOccurs="1"/>
                    <elementRef key="equiv"          minOccurs="0" maxOccurs="1"/>
                    <elementRef key="gloss"          minOccurs="0" maxOccurs="1"/>
                    <classRef key="model.descLike"   expand="sequenceOptional"/>
                    <elementRef key="classes"        minOccurs="0" maxOccurs="1"/>
                    <elementRef key="content"        minOccurs="0" maxOccurs="1"/>
                    <elementRef key="valList"        minOccurs="0" maxOccurs="1"/>
                    <elementRef key="constraintSpec" minOccurs="0" maxOccurs="unbounded"/>
                    <elementRef key="attList"        minOccurs="0" maxOccurs="1"/>
                    <elementRef key="exemplum"       minOccurs="0" maxOccurs="unbounded"/>
                    <elementRef key="remarks"        minOccurs="0" maxOccurs="unbounded"/>
                    <elementRef key="listRef"        minOccurs="0" maxOccurs="unbounded"/>
                  </sequence>
                </content>
                <constraintSpec scheme="schematron" ident="module-except-when-add">
                  <constraint>
                    <sch:rule context="tei:elementSpec">
                      <sch:assert test="@mode">
                        in a customization ODD, the mode= attribute of ＜elementSpec＞ should be specified
                      </sch:assert>
                      <sch:report test="not( @module )  and  not( @mode='add')">
                        the module= attribute of ＜elementSpec＞ must be specified anytime the mode= is not 'add'
                      </sch:report>
                    </sch:rule>
                  </constraint>
                </constraintSpec>
                <constraintSpec scheme="schematron" ident="only-1-per">
                  <constraint>
                    <sch:rule context="tei:elementSpec">
                      <sch:report test="//tei:elementSpec[ @ident eq current()/@ident  and  not( . is current() ) ]"
                      >Current ODD processors will not correctly handle more than one ＜elementSpec＞ with the same @ident</sch:report>
                    </sch:rule>
                  </constraint>
                </constraintSpec>
                <constraintSpec scheme="schematron" ident="dont-delete-required">
                  <constraint>
                    <sch:rule context="tei:elementSpec">
                      <sch:report test="@mode='delete' and @ident='TEI'">Removing ＜TEI＞ from your
                        schema guarantees it is not TEI conformant</sch:report>
                      <sch:report test="@mode='delete' and @ident='teiHeader'">Removing ＜teiHeader＞
                        from your schema guarantees it is not TEI conformant</sch:report>
                      <sch:report test="@mode='delete' and @ident='fileDesc'">Removing ＜fileDesc＞ from
                        your schema guarantees it is not TEI conformant</sch:report>
                      <sch:report test="@mode='delete' and @ident='titleStmt'">Removing ＜titleStmt＞
                        from your schema guarantees it is not TEI conformant</sch:report>
                      <sch:report test="@mode='delete' and @ident='title'">Removing ＜title＞ from your
                        schema guarantees it is not TEI conformant</sch:report>
                      <sch:report test="@mode='delete' and @ident='publicationStmt'">Removing
                        ＜publicationStmt＞ from your schema guarantees it is not TEI
                        conformant</sch:report>
                      <sch:report test="@mode='delete' and @ident='sourceDesc'">Removing ＜sourceDesc＞
                        from your schema guarantees it is not TEI conformant</sch:report>
		    </sch:rule>
                  </constraint>
                </constraintSpec>
                <constraintSpec scheme="schematron" ident="content_when_adding">
                  <constraint>
                    <sch:rule context="tei:elementSpec[ @mode = ('add','replace') ]">
                      <sch:assert test="tei:content">When adding a new element (whether replacing an old one or not), a content model must be specified; but this ＜elementSpec＞ does not have a ＜content＞ child.</sch:assert>
                    </sch:rule>
                  </constraint>
                </constraintSpec>
                <constraintSpec scheme="schematron" ident="empty_when_deleting">
                  <constraint>
                    <sch:rule context="tei:elementSpec[ @mode eq 'delete']">
                      <sch:report test="*">When used to delete an element from your schema, the ＜elementSpec＞ should be empty</sch:report>
                    </sch:rule>
                  </constraint>
                </constraintSpec>
                <constraintSpec scheme="schematron" ident="add_implies_ns">
                  <constraint>
                    <sch:rule context="tei:elementSpec[ @mode eq 'add'  or  not( @mode ) ]">
                      <sch:assert test="ancestor-or-self::*/@ns">When used to add an element, ＜elementSpec＞ (or its ancestor ＜schemaSpec＞) should have an @ns attribute.</sch:assert>
                    </sch:rule>
                  </constraint>
                </constraintSpec>
                <attList>
                  <attDef ident="ident" mode="replace" usage="req">
                    <datatype minOccurs="1" maxOccurs="1">
                      <dataRef key="teidata.enumerated"/>
                    </datatype>
                    <valList type="semi">
                      <xsl:copy-of select="$elements-module-in-desc"/>
                    </valList>
                  </attDef>
                </attList>
              </elementSpec>

              <elementSpec module="tagdocs" ident="egXML" mode="change"
                ns="http://www.tei-c.org/ns/Examples">
                <xsl:comment> The only difference here is that TEI does not permit </xsl:comment>
                <xsl:comment> &lt;egXML> to be a descendant of &lt;egXML>; but since </xsl:comment>
                <xsl:comment> no one knows why that restriction exists, we've removed it. </xsl:comment>
                <content>
                  <alternate minOccurs="0" maxOccurs="unbounded">
                    <textNode/>
                    <elementRef key="egXML"/>
                    <anyElement except="http://www.tei-c.org/ns/1.0"/>
                  </alternate>
                </content>
              </elementSpec>

              <elementSpec module="tagdocs" ident="constraint" mode="change">
                <content>
                  <alternate minOccurs="1" maxOccurs="unbounded">
                    <elementRef key="sch_assert"/>
                    <elementRef key="sch_let"/>
                    <elementRef key="sch_ns"/>
                    <elementRef key="sch_report"/>
                    <elementRef key="sch_rule"/>
                    <elementRef key="sch_pattern"/>
                  </alternate>
                </content>
                <remarks xml:lang="en" versionDate="{$versionDate}">
                  <p>For our purposes, constraints must be expressed in ISO Schematron. (TEI permits
                    others, including non-XML expressions, although there is no processing of
                    anything other ISO Schematron.)</p>
                </remarks>
              </elementSpec>

              <xsl:comment> in examples we'd like to be able to show use of &lt;xi:include>; of course, </xsl:comment>
              <xsl:comment> we can't put in an &lt;xi:include> element, or it will be processed! Thus, </xsl:comment>
              <xsl:comment> here we use a bogus namespace </xsl:comment>
              <elementSpec ident="include" ns="http://www.example.org/cannot/really/use/XInclude"
                mode="add">
                <!-- the content and attributes were devised based on my reading of the spec at
                 http://www.w3.org/TR/2006/REC-xinclude-20061115/, also having looked at
                 Norm Walsh's implementation for DocBook. -sb -->
                <content>
                  <elementRef key="fallback" minOccurs="0" maxOccurs="1"/>
                </content>
                <attList>
                  <attDef ident="href">
                    <datatype>
                      <dataRef key="teidata.pointer" restriction="[^#]+"/>
                    </datatype>
                  </attDef>
                  <attDef ident="parse">
                    <defaultVal>xml</defaultVal>
                    <valList type="closed">
                      <valItem ident="xml"/>
                      <valItem ident="text"/>
                    </valList>
                  </attDef>
                  <attDef ident="xpointer">
                    <datatype>
                      <dataRef key="teidata.text"/>
                    </datatype>
                  </attDef>
                  <attDef ident="encoding">
                    <datatype>
                      <dataRef key="teidata.text"/>
                    </datatype>
                  </attDef>
                  <attDef ident="accept">
                    <datatype>
                      <dataRef key="teidata.text"/>
                    </datatype>
                  </attDef>
                  <attDef ident="accept-language">
                    <datatype>
                      <dataRef key="teidata.text"/>
                    </datatype>
                  </attDef>
                </attList>
              </elementSpec>
              <xsl:comment> of course &lt;xi:include> refers to &lt;xi:fallback>, so we need to declare</xsl:comment>
              <xsl:comment> that, too, just in case. </xsl:comment>
              <elementSpec ns="http://www.example.org/cannot/really/use/XInclude"
                           ident="fallback" mode="add" >
                <content>
                  <alternate minOccurs="1" maxOccurs="unbounded">
                    <textNode/>
                    <anyElement/>
                  </alternate>
                </content>
              </elementSpec>

              <elementSpec module="tagdocs" ident="altIdent" mode="change">
                <classes>
                  <memberOf key="model.identSynonyms" mode="delete"/>
                </classes>
              </elementSpec>

              <elementSpec module="tagdocs" ident="gi" mode="change">
                <attList>
                  <attDef mode="replace" ident="scheme">
                    <datatype minOccurs="1" maxOccurs="1">
                      <dataRef key="teidata.enumerated"/>
                    </datatype>
                    <valList type="semi">
                      <valItem ident="imaginary"/>
                      <valItem ident="extreme">
                        <gloss xml:lang="en" versionDate="{$versionDate}">Extreme conference proceedings markup</gloss>
                      </valItem>
                      <valItem ident="DBK">
                        <gloss xml:lang="en" versionDate="{$versionDate}">Docbook</gloss>
                      </valItem>
                      <valItem ident="SMIL"/>
                      <valItem ident="Schematron"/>
                      <valItem ident="HTML">
                        <gloss xml:lang="en" versionDate="{$versionDate}">XHTML</gloss>
                      </valItem>
                      <valItem ident="SVG"/>
                      <valItem ident="WWP"/>
                      <valItem ident="DHQ"/>
                    </valList>
                  </attDef>
                </attList>
              </elementSpec>

              <elementSpec module="core" ident="divGen" mode="change">
                <attList>
                  <attDef mode="replace" ident="type">
                    <datatype minOccurs="1" maxOccurs="1">
                      <dataRef key="teidata.enumerated"/>
                    </datatype>
                    <valList type="closed">
                      <valItem ident="attcat">
                        <gloss xml:lang="en" versionDate="{$versionDate}">Catalogue of Attributes</gloss>
                      </valItem>
                      <valItem ident="attclasscat">
                        <gloss xml:lang="en" versionDate="{$versionDate}">Catalogue of Attribute Classes</gloss>
                      </valItem>
                      <valItem ident="elementcat">
                        <gloss xml:lang="en" versionDate="{$versionDate}">Catalogue of Elements</gloss>
                      </valItem>
                      <valItem ident="macrocat">
                        <gloss xml:lang="en" versionDate="{$versionDate}">Catalogue of Macros</gloss>
                      </valItem>
                      <valItem ident="modelclasscat">
                        <gloss xml:lang="en" versionDate="{$versionDate}">Catalogue of Model Classes</gloss>
                      </valItem>
                      <valItem ident="deprecationcat">
                        <gloss xml:lang="en" versionDate="{$versionDate}">Catalogue of Deprecations</gloss>
                      </valItem>
                      <valItem ident="toc">
                        <gloss xml:lang="en" versionDate="{$versionDate}">Table of Contents</gloss>
                      </valItem>
                    </valList>
                  </attDef>
                </attList>
              </elementSpec>

              <elementSpec module="textstructure" ident="div" mode="change">
                <attList>
                  <attDef mode="replace" ident="type">
                    <datatype minOccurs="1" maxOccurs="1">
                      <dataRef key="teidata.enumerated"/>
                    </datatype>
                    <valList type="semi">
                      <valItem ident="section"/>
                      <valItem ident="subsection"/>
                      <valItem ident="subsubsection"/>
                    </valList>
                  </attDef>
                </attList>
              </elementSpec>

              <elementSpec module="tagdocs" ident="ident" mode="change">
                <attList>
                  <attDef mode="replace" ident="type">
                    <datatype minOccurs="1" maxOccurs="1">
                      <dataRef key="teidata.enumerated"/>
                    </datatype>
                    <valList type="semi">
                      <valItem ident="ge">
                        <gloss xml:lang="en" versionDate="{$versionDate}">general entity name</gloss>
                      </valItem>
                      <valItem ident="rng">
                        <gloss xml:lang="en" versionDate="{$versionDate}">RELAX NG identifier</gloss>
                      </valItem>
                      <valItem ident="frag">
                        <gloss xml:lang="en" versionDate="{$versionDate}">DTD fragment identifier</gloss>
                      </valItem>
                      <valItem ident="macro"/>
                      <valItem ident="ns">
                        <gloss xml:lang="en" versionDate="{$versionDate}">namespace</gloss>
                      </valItem>
                      <valItem ident="schema"/>
                      <valItem ident="pe">
                        <gloss xml:lang="en" versionDate="{$versionDate}">parameter entity name</gloss>
                      </valItem>
                      <valItem ident="datatype"/>
                      <valItem ident="file"/>
                      <valItem ident="module"/>
                      <valItem ident="class"/>
                    </valList>
                  </attDef>
                </attList>
              </elementSpec>

              <elementSpec module="header" ident="idno" mode="change">
                <attList>
                  <attDef mode="replace" ident="type">
                    <datatype minOccurs="1" maxOccurs="1">
                      <dataRef key="teidata.enumerated"/>
                    </datatype>
                    <valList type="semi">
                      <valItem ident="doi">
                        <gloss xml:lang="en" versionDate="{$versionDate}">Digital Object Identifier</gloss>
                      </valItem>
                      <valItem ident="isbn">
                        <gloss xml:lang="en" versionDate="{$versionDate}">International Standard Book Number</gloss>
                      </valItem>
                      <valItem ident="url">
                        <desc xml:lang="en" versionDate="{$versionDate}">any form of web address</desc>
                      </valItem>
                    </valList>
                  </attDef>
                </attList>
              </elementSpec>

              <classSpec ident="att.identified" module="tagdocs" mode="change" type="atts">
                <attList>
                  <attDef ident="module" mode="replace">
                    <datatype minOccurs="1" maxOccurs="1">
                      <dataRef key="teidata.enumerated"/>
                    </datatype>
                    <valList type="closed">
                      <xsl:copy-of select="$modules"/>
                    </valList>
                  </attDef>
                </attList>
              </classSpec>
              
              <classSpec ident="att.global" module="tei" mode="change" type="atts">
                <attList>
                  <attDef ident="xml:id" mode="change" ns="http://www.w3.org/XML/1998/namespace">
                    <constraintSpec scheme="schematron" ident="unique_xmlIDs">
                      <constraint>
			<sch:rule context="@xml:id">
                          <sch:let name="myID" value="normalize-space(.)"/>
                          <sch:report test="../(ancestor::*|preceding::*)/@xml:id[ normalize-space(.) eq $myID ]"
                            >The @xml:id "<sch:value-of select="."/>" on ＜<sch:value-of select="name(..)"/>＞ duplicates an @xml:id found earlier in the document</sch:report>
			</sch:rule>
                      </constraint>
                    </constraintSpec>
                  </attDef>
                </attList>
              </classSpec>

              <classSpec ident="model.entryPart.top" module="tei" mode="delete" type="model"/>
              <classSpec ident="model.msItemPart" module="tei" mode="delete" type="model"/>
              <classSpec ident="model.personPart" module="tei" mode="delete" type="model"/>
              <classSpec ident="model.titlepagePart" module="tei" mode="delete" type="model"/>
              <classSpec ident="att.global.rendition" module="tei" mode="delete" type="atts"/>
              <classSpec ident="att.global.responsibility" module="tei" mode="delete" type="atts"/>

              <constraintSpec scheme="schematron" ident="tei-source">
                <desc>Constrains the <att>source</att> attribute of
                various tagset documentation elements to those values
                recommended by TEI</desc>
                <!--
                    WARNING: this rule/@context is not auto-generated,
                    and may need to be updated to match the TEI
                    Guidelines. Sadly, TEI P5 no longer has a separate
                    class for this (it used to be att.readFrom),
                    @source is now global.
                -->
                <constraint>
                  <sch:rule context=" tei:classRef[@source]
                                     |tei:dataRef[@source]
                                     |tei:elementRef[@source]
                                     |tei:macroRef[@source]
                                     |tei:moduleRef[@source]
                                     |tei:schemaSpec[@source]">
                    <sch:assert
                      test="matches(normalize-space(@source), '^tei:([0-9]+\.[0-9]+\.[0-9]+|current)$')"
                      role="information">The @source attribute of ＜<sch:name/>＞ is not in the
                      recommended format, which is either "tei:current" or "tei:x.y.z", where x.y.z is a version number.</sch:assert>
                  </sch:rule>
                </constraint>
              </constraintSpec>

            </schemaSpec>
          </div>
        </body>
      </text>
    </TEI>
  </xsl:template>

</xsl:stylesheet>
