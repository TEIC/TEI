<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.w3.org/1999/xhtml"
                xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
		xmlns:iso="http://www.iso.org/ns/1.0"
		xmlns:cals="http://www.oasis-open.org/specs/tm9901"
                xmlns:html="http://www.w3.org/1999/xhtml"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                xmlns:s="http://www.ascc.net/xml/schematron"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:t="http://www.thaiopensource.com/ns/annotations"
                xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                exclude-result-prefixes="tei html t a rng s iso tbx cals teix"
                version="2.0">
  
   <xsl:import href="../../../xhtml2/tei.xsl"/>
   <xsl:import href="../../../xhtml2/tagdocs.xsl"/>
   <xsl:import href="../../../odds2/teiodds.xsl"/>
   <xsl:import href="../isoutils.xsl"/>
   <xsl:import href="../isotei-schema.xsl"/>
   <xsl:param name="numberFormat">uk</xsl:param>
   <xsl:output encoding="utf-8" omit-xml-declaration="yes" method="xhtml"
               doctype-public="-//W3C//DTD XHTML 1.0 Transitional//EN"
               doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"
               indent="yes"/>

   <xsl:param name="STDOUT">true</xsl:param>
   <xsl:param name="xhtml">true</xsl:param>
   <xsl:param name="splitLevel">-1</xsl:param>
   <xsl:param name="autoToc">true</xsl:param>
   <xsl:param name="institution">ISO</xsl:param>
   <xsl:param name="department"/>
   <xsl:param name="cssFile">http://tei.oucs.ox.ac.uk/TEIISO/iso.css</xsl:param>
   <xsl:param name="cssSecondaryFile">http://www.tei-c.org/release/xml/tei/stylesheet/odd.css</xsl:param>
   <xsl:param name="TEIC">false</xsl:param>
   <xsl:param name="wrapLength">65</xsl:param>
   <xsl:param name="attLength">60</xsl:param>
   <xsl:param name="forceWrap">true</xsl:param>
   <xsl:template name="myi18n">
      <xsl:param name="word"/>
      <xsl:choose>
         <xsl:when test="$word='appendixWords'">
            <xsl:text>Annex</xsl:text>
    </xsl:when>
      </xsl:choose>
   </xsl:template>

   <xsl:template match="/">
     <xsl:variable name="All">
       <xsl:apply-templates mode="checkSchematron"/>
     </xsl:variable>
     <xsl:for-each select="$All">
       <xsl:apply-templates/>
     </xsl:for-each>
   </xsl:template>

   <xsl:template match="processing-instruction()[name()='ISOerror']">
     <span style="border: solid red 1pt; color:red">
       <xsl:value-of select="."/>
     </span>
   </xsl:template>

  <xsl:template name="divClassAttribute">
      <xsl:param name="depth"/>
      <xsl:choose>
         <xsl:when test="@type">
            <xsl:attribute name="class">
               <xsl:value-of select="@type"/>
            </xsl:attribute>
         </xsl:when>
         <xsl:otherwise>
            <xsl:attribute name="class">
               <xsl:text>teidiv</xsl:text>
               <xsl:value-of select="$depth"/>
	              <xsl:text> from-</xsl:text>
	              <xsl:value-of select="local-name(ancestor::tei:body|ancestor::tei:front|ancestor::tei:back)"/>
            </xsl:attribute>
         </xsl:otherwise>
      </xsl:choose>
      <xsl:variable name="ident">
         <xsl:apply-templates mode="ident" select="."/>
      </xsl:variable>
      <xsl:attribute name="id">
         <xsl:value-of select="$ident"/>
      </xsl:attribute>
  </xsl:template>


   <xsl:template match="tei:note[@rend='example']">
      <p>EXAMPLE <xsl:apply-templates/>
      </p>
   </xsl:template>


   <xsl:template match="tei:p[count(*)=1 and tei:gloss]">
      <p style="margin-left: 1em">
         <xsl:apply-templates/>
      </p>
   </xsl:template>

   <xsl:template match="tei:num">
      <span class="isonum">
         <xsl:choose>
            <xsl:when test="$numberFormat='fr'">
	              <xsl:value-of select="."/>
            </xsl:when>
            <xsl:otherwise>
	              <xsl:value-of select="translate(.,', ','.,')"/>
            </xsl:otherwise>
         </xsl:choose>
      </span>
   </xsl:template>

   <xsl:template match="tei:c[@rend='tab']">
      <xsl:text>	</xsl:text>
   </xsl:template>

   <xsl:template match="tei:c[@iso:font]">
      <xsl:value-of select="@n"/>
   </xsl:template>

   <xsl:template match="tei:seg[@iso:provision]">
      <span class="provision_{@iso:provision}">
         <xsl:apply-templates/>
      </span>
   </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl"  type="stylesheet">
      <desc> Process CALS tables, using code borrowed from Andrew Welch
      http://www.biglist.com/lists/xsl-list/archives/200202/msg00666.html</desc>
  </doc>
<xsl:template match="cals:tgroup">
  <table>
    <xsl:if test="@align">
      <xsl:attribute name="align">
        <xsl:value-of select="@align"/>
      </xsl:attribute>
    </xsl:if>

    <xsl:choose>
      <xsl:when test="../@frame='topbot'">
	<xsl:attribute name="style">border-top:thin solid
	black;border-bottom:thin solid black</xsl:attribute>
      </xsl:when>
      <xsl:otherwise>
	<xsl:attribute name="border">0</xsl:attribute>
      </xsl:otherwise>
    </xsl:choose>
    
    <xsl:variable name="colgroup">
      <colgroup>
	<xsl:call-template name="generate.colgroup">
	  <xsl:with-param name="cols" select="count(cals:colspec)"/>
	</xsl:call-template>
      </colgroup>
    </xsl:variable>
    
    <xsl:copy-of select="$colgroup"/>

    <xsl:apply-templates/>

   </table>
</xsl:template>

<xsl:template match="cals:colspec"></xsl:template>

<xsl:template match="cals:spanspec"></xsl:template>

<xsl:template match="cals:thead|cals:tfoot">
  <xsl:element name="{name(.)}">
    <xsl:if test="@align">
      <xsl:attribute name="align">
        <xsl:value-of select="@align"/>
      </xsl:attribute>
    </xsl:if>
    <xsl:if test="@char">
      <xsl:attribute name="char">
        <xsl:value-of select="@char"/>
      </xsl:attribute>
    </xsl:if>
    <xsl:if test="@charoff">
      <xsl:attribute name="charoff">
        <xsl:value-of select="@charoff"/>
      </xsl:attribute>
    </xsl:if>
    <xsl:if test="@valign">
      <xsl:attribute name="valign">
        <xsl:value-of select="@valign"/>
      </xsl:attribute>
    </xsl:if>

    <xsl:apply-templates/>
  </xsl:element>
</xsl:template>

<xsl:template match="cals:tbody">
  <tbody>
    <xsl:if test="@align">
      <xsl:attribute name="align">
        <xsl:value-of select="@align"/>
      </xsl:attribute>
    </xsl:if>
    <xsl:if test="@char">
      <xsl:attribute name="char">
        <xsl:value-of select="@char"/>
      </xsl:attribute>
    </xsl:if>
    <xsl:if test="@charoff">
      <xsl:attribute name="charoff">
        <xsl:value-of select="@charoff"/>
      </xsl:attribute>
    </xsl:if>
    <xsl:if test="@valign">
      <xsl:attribute name="valign">
        <xsl:value-of select="@valign"/>
      </xsl:attribute>
    </xsl:if>

    <xsl:apply-templates/>
  </tbody>
</xsl:template>

<xsl:template match="cals:row">
  <tr>
    <xsl:if test="@align">
      <xsl:attribute name="align">
        <xsl:value-of select="@align"/>
      </xsl:attribute>
    </xsl:if>
    <xsl:if test="@char">
      <xsl:attribute name="char">
        <xsl:value-of select="@char"/>
      </xsl:attribute>
    </xsl:if>
    <xsl:if test="@charoff">
      <xsl:attribute name="charoff">
        <xsl:value-of select="@charoff"/>
      </xsl:attribute>
    </xsl:if>
    <xsl:if test="@valign">
      <xsl:attribute name="valign">
        <xsl:value-of select="@valign"/>
      </xsl:attribute>
    </xsl:if>
    <xsl:apply-templates/>
  </tr>
</xsl:template>

<xsl:template match="cals:thead/cals:row/cals:entry">
  <xsl:call-template name="process.cell">
    <xsl:with-param name="cellgi">th</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="cals:tbody/cals:row/cals:entry">
  <xsl:call-template name="process.cell">
    <xsl:with-param name="cellgi">td</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="cals:tfoot/cals:row/cals:entry">
  <xsl:call-template name="process.cell">
    <xsl:with-param name="cellgi">th</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template name="process.cell">
  <xsl:param name="cellgi">td</xsl:param>


  <xsl:variable name="empty.cell" select="count(node()) = 0"/>

  <xsl:variable name="entry.colnum">
    <xsl:call-template name="entry.colnum"/>
  </xsl:variable>

  <xsl:if test="$entry.colnum != ''">
    <xsl:variable name="prev.entry" select="preceding-sibling::*[1]"/>
    <xsl:variable name="prev.ending.colnum">
      <xsl:choose>
        <xsl:when test="$prev.entry">
          <xsl:call-template name="entry.ending.colnum">
            <xsl:with-param name="entry" select="$prev.entry"/>
          </xsl:call-template>
        </xsl:when>
        <xsl:otherwise>0</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:call-template name="add-empty-entries">
      <xsl:with-param name="number">
        <xsl:choose>
          <xsl:when test="$prev.ending.colnum = ''">0</xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="$entry.colnum - $prev.ending.colnum - 1"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:if>

  <xsl:element name="{$cellgi}">

  <xsl:if test="@spanname">
  	<xsl:variable name="namest"
		      select="ancestor::cals:tgroup/cals:spanspec[@spanname=./@spanname]/@namest"/>
	<xsl:variable name="nameend"
		      select="ancestor::cals:tgroup/cals:spanspec[@spanname=./@spanname]/@nameend"/>
	<xsl:variable name="colst"
		      select="ancestor::*[cals:colspec/@colname=$namest]/cals:colspec[@colname=$namest]/@colnum"/>
	<xsl:variable name="colend"
		      select="ancestor::*[cals:colspec/@colname=$nameend]/cals:colspec[@colname=$nameend]/@colnum"/>
	<xsl:attribute name="colspan">
	  <xsl:value-of
	      select="number($colend) - number($colst) + 1"/></xsl:attribute>
  </xsl:if>

    <xsl:if test="@morerows">
      <xsl:attribute name="rowspan">
        <xsl:value-of select="@morerows+1"/>
      </xsl:attribute>
    </xsl:if>
    <xsl:if test="@namest">
      <xsl:attribute name="colspan">
        <xsl:call-template name="calculate.colspan"/>
      </xsl:attribute>
    </xsl:if>
    <xsl:if test="@align">
      <xsl:attribute name="align">
        <xsl:value-of select="@align"/>
      </xsl:attribute>
    </xsl:if>
    <xsl:if test="@char">
      <xsl:attribute name="char">
        <xsl:value-of select="@char"/>
      </xsl:attribute>
    </xsl:if>
    <xsl:if test="@charoff">
      <xsl:attribute name="charoff">
        <xsl:value-of select="@charoff"/>
      </xsl:attribute>
    </xsl:if>
    <xsl:if test="@valign">
      <xsl:attribute name="valign">
        <xsl:value-of select="@valign"/>
      </xsl:attribute>
    </xsl:if>

	<xsl:if test="@rowsep='1'">
		<xsl:attribute name="style">border-bottom:thin solid black</xsl:attribute>
	</xsl:if>

    <xsl:if test="not(preceding-sibling::*)
                  and ancestor::cals:row/@id">
      <a name="{ancestor::cals:row/@id}"/>
    </xsl:if>

    <xsl:if test="@id">
      <a name="{@id}"/>
    </xsl:if>

    <xsl:choose>
      <xsl:when test="$empty.cell">
        <xsl:text>&#160;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:element>
</xsl:template>

<xsl:template name="add-empty-entries">
  <xsl:param name="number" select="'0'"/>
  <xsl:choose>
    <xsl:when test="$number &lt;= 0"></xsl:when>
    <xsl:otherwise>
      <td>&#160;</td>
      <xsl:call-template name="add-empty-entries">
        <xsl:with-param name="number" select="$number - 1"/>
      </xsl:call-template>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template name="entry.colnum">
  <xsl:param name="entry" select="."/>

  <xsl:choose>
    <xsl:when test="$entry/@colname">
      <xsl:variable name="colname" select="$entry/@colname"/>
      <xsl:variable name="colspec"
		    select="$entry/ancestor::cals:tgroup/cals:colspec[@colname=$colname]"/>
      <xsl:call-template name="colspec.colnum">
        <xsl:with-param name="colspec" select="$colspec"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:when test="$entry/@namest">
      <xsl:variable name="namest" select="$entry/@namest"/>
      <xsl:variable name="colspec"
		    select="$entry/ancestor::cals:tgroup/cals:colspec[@colname=$namest]"/>
      <xsl:call-template name="colspec.colnum">
        <xsl:with-param name="colspec" select="$colspec"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:when test="count($entry/preceding-sibling::*) = 0">1</xsl:when>
    <xsl:otherwise>
      <xsl:variable name="pcol">
        <xsl:call-template name="entry.ending.colnum">
          <xsl:with-param name="entry"
			  select="$entry/preceding-sibling::*[1]"/>
        </xsl:call-template>
      </xsl:variable>
      <xsl:value-of select="$pcol + 1"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template name="entry.ending.colnum">
  <xsl:param name="entry" select="."/>

  <xsl:choose>
    <xsl:when test="$entry/@colname">
      <xsl:variable name="colname" select="$entry/@colname"/>
      <xsl:variable name="colspec"
		    select="$entry/ancestor::cals:tgroup/cals:colspec[@colname=$colname]"/>
      <xsl:call-template name="colspec.colnum">
	<xsl:with-param name="colspec" select="$colspec"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:when test="$entry/@nameend">
      <xsl:variable name="nameend" select="$entry/@nameend"/>
      <xsl:variable name="colspec"
		    select="$entry/ancestor::cals:tgroup/cals:colspec[@colname=$nameend]"/>
      <xsl:call-template name="colspec.colnum">
	<xsl:with-param name="colspec" select="$colspec"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:when test="count($entry/preceding-sibling::*) = 0">1</xsl:when>
    <xsl:otherwise>
      <xsl:variable name="pcol">
	<xsl:call-template name="entry.ending.colnum">
	  <xsl:with-param name="entry"
			  select="$entry/preceding-sibling::*[1]"/>
	</xsl:call-template>
      </xsl:variable>
      <xsl:value-of select="$pcol + 1"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template name="colspec.colnum">
  <xsl:param name="colspec" select="."/>
  <xsl:choose>
    <xsl:when test="$colspec/@colnum">
      <xsl:value-of select="$colspec/@colnum"/>
    </xsl:when>
    <xsl:when test="$colspec/preceding-sibling::cals:colspec">
      <xsl:variable name="prec.colspec.colnum">
        <xsl:call-template name="colspec.colnum">
          <xsl:with-param name="colspec"
                          select="$colspec/preceding-sibling::cals:colspec[1]"/>
        </xsl:call-template>
      </xsl:variable>
      <xsl:value-of select="$prec.colspec.colnum + 1"/>
    </xsl:when>
    <xsl:otherwise>1</xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="generate.colgroup">
  <xsl:param name="cols" select="1"/>
  <xsl:param name="count" select="1"/>
  <xsl:choose>
    <xsl:when test="$count&gt;$cols"></xsl:when>
    <xsl:otherwise>
      <xsl:call-template name="generate.col">
        <xsl:with-param name="countcol" select="$count"/>
      </xsl:call-template>
      <xsl:call-template name="generate.colgroup">
        <xsl:with-param name="cols" select="$cols"/>
        <xsl:with-param name="count" select="$count+1"/>
      </xsl:call-template>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="generate.col">
  <xsl:param name="countcol">1</xsl:param>
  <xsl:param name="colspecs" select="./cals:colspec"/>
  <xsl:param name="count">1</xsl:param>
  <xsl:param name="colnum">1</xsl:param>

  <xsl:choose>
    <xsl:when test="$count>count($colspecs)">
      <col/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:variable name="colspec" select="$colspecs[$count=position()]"/>
      <xsl:variable name="colspec.colnum">
        <xsl:choose>
          <xsl:when test="$colspec/@colnum">
            <xsl:value-of select="$colspec/@colnum"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="$colnum"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:variable>

      <xsl:choose>
        <xsl:when test="$colspec.colnum=$countcol">
          <col>
            <xsl:if test="$colspec/@align">
              <xsl:attribute name="align">
                <xsl:value-of select="$colspec/@align"/>
              </xsl:attribute>
            </xsl:if>
            <xsl:if test="$colspec/@char">
              <xsl:attribute name="char">
                <xsl:value-of select="$colspec/@char"/>
              </xsl:attribute>
            </xsl:if>
            <xsl:if test="$colspec/@charoff">
              <xsl:attribute name="charoff">
                <xsl:value-of select="$colspec/@charoff"/>
              </xsl:attribute>
            </xsl:if>
          </col>
        </xsl:when>
        <xsl:otherwise>
          <xsl:call-template name="generate.col">
            <xsl:with-param name="countcol" select="$countcol"/>
            <xsl:with-param name="colspecs" select="$colspecs"/>
            <xsl:with-param name="count" select="$count+1"/>
            <xsl:with-param name="colnum">
              <xsl:choose>
                <xsl:when test="$colspec/@colnum">
                  <xsl:value-of select="$colspec/@colnum + 1"/>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:value-of select="$colnum + 1"/>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:with-param>
           </xsl:call-template>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:otherwise>
  </xsl:choose>

</xsl:template>

<xsl:template name="colspec.colwidth">
  <!-- when this macro is called, the current context must be an entry -->
  <xsl:param name="colname"></xsl:param>
  <!-- .. = row, ../.. = thead|tbody, ../../.. = cals:tgroup -->
  <xsl:param name="colspecs" select="../../../../cals:tgroup/cals:colspec"/>
  <xsl:param name="count">1</xsl:param>
  <xsl:choose>
    <xsl:when test="$count>count($colspecs)"></xsl:when>
    <xsl:otherwise>
      <xsl:variable name="colspec" select="$colspecs[$count=position()]"/>
      <xsl:choose>
        <xsl:when test="$colspec/@colname=$colname">
          <xsl:value-of select="$colspec/@colwidth"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:call-template name="colspec.colwidth">
            <xsl:with-param name="colname" select="$colname"/>
            <xsl:with-param name="colspecs" select="$colspecs"/>
            <xsl:with-param name="count" select="$count+1"/>
          </xsl:call-template>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="calculate.colspan">
  <xsl:param name="entry" select="."/>
  <xsl:variable name="namest" select="$entry/@namest"/>
  <xsl:variable name="nameend" select="$entry/@nameend"/>

  <xsl:variable name="scol">
    <xsl:call-template name="colspec.colnum">
      <xsl:with-param name="colspec"
		      select="$entry/ancestor::cals:tgroup/cals:colspec[@colname=$namest]"/>
    </xsl:call-template>
  </xsl:variable>
  <xsl:variable name="ecol">
    <xsl:call-template name="colspec.colnum">
      <xsl:with-param name="colspec"
		      select="$entry/ancestor::cals:tgroup/cals:colspec[@colname=$nameend]"/>
    </xsl:call-template>
  </xsl:variable>
  <xsl:value-of select="$ecol - $scol + 1"/>
</xsl:template>

<xsl:template match="tbx:termEntry">
  <div class="termEntry">
      <xsl:for-each select="tbx:langSet">
         <xsl:choose>
            <xsl:when test="starts-with(../@id,'autoTermNum')">
	      <p class="TermNum"><xsl:number/></p>
            </xsl:when>
            <xsl:otherwise>
	      <p class="TermNum">
		<xsl:value-of select="substring-after(../@id,'CDB_')"/>
	      </p>
            </xsl:otherwise>
         </xsl:choose>
         <xsl:for-each select="tbx:ntig">
	           <xsl:variable name="Thing">
	              <xsl:value-of select="substring-before(tbx:termGrp/tbx:termNote[@type='administrativeStatus'],'-admn-sts')"/>
	           </xsl:variable>
	           <xsl:variable name="style">
	              <xsl:choose>
	                 <xsl:when test="$Thing='preferredTerm'">termPreferred</xsl:when>
	                 <xsl:when test="$Thing='deprecatedTerm'">termDeprecated</xsl:when>
	                 <xsl:when test="$Thing='admittedTerm'">termAdmitted</xsl:when>
	                 <xsl:when test="$Thing='symbol'">symbol</xsl:when>
	              </xsl:choose>
	           </xsl:variable>
		   <xsl:call-template name="block-element">
		     <xsl:with-param name="style" select="$style"/>
		   </xsl:call-template>
         </xsl:for-each>

         <xsl:apply-templates select="tbx:descripGrp/tbx:descrip[@type='definition']"/>
         <xsl:apply-templates select="tbx:descripGrp/tbx:admin"/>
         <xsl:apply-templates select="tbx:note"/>
      </xsl:for-each>
      <xsl:apply-templates select="tbx:descripGrp/tbx:descrip[@type='definition']"/>
      <xsl:apply-templates select="tbx:descripGrp/tbx:admin"/>
      <xsl:apply-templates select="tbx:note"/>
  </div>
   </xsl:template>

   <xsl:template match="tbx:termGrp/tbx:termNote"/>

   <xsl:template match="tbx:descrip">
      <xsl:call-template name="block-element">
         <xsl:with-param name="style">Definition</xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="tbx:note">
      <xsl:call-template name="block-element">
         <xsl:with-param name="style">
	   <xsl:text>noteTermEntry</xsl:text>
         </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template match="tbx:admin">
      <xsl:call-template name="block-element">
         <xsl:with-param name="style">entrySource</xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template name="block-element">
     <xsl:param name="style"/>
     <p>
       <xsl:if test="not($style='')">
	 <xsl:attribute name="class">
	   <xsl:value-of select="$style"/>
	 </xsl:attribute>
       </xsl:if>
       <xsl:apply-templates/>
     </p>
   </xsl:template>

   <xsl:template name="generateError">
     <xsl:param name="message"/>
     <xsl:processing-instruction name="ISOerror">
       <xsl:value-of select="$message"/>
     </xsl:processing-instruction>
   </xsl:template>

   <xsl:template name="copyIt">
      <xsl:copy>
	<xsl:apply-templates select="@*" mode="checkSchematron"/>
	<xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
   </xsl:template>

   <xsl:template name="copyMe">
      <xsl:copy-of select="."/>
   </xsl:template>

   <xsl:template match="text()">
       <xsl:value-of select="translate(.,'&#2011;','-')"/>
   </xsl:template>

</xsl:stylesheet>
