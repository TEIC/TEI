<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xmlns:tei="http://www.tei-c.org/ns/1.0" 
    version="1.0">
  
<xsl:template name="lang"/>
<xsl:template name="langName"/>

<xsl:key name="Mo" match="tei:moduleSpec" use="1"/>
<xsl:key name="E"  match="tei:elementSpec" use="@module"/>
<xsl:key name="Ma" match="tei:macroSpec" use="@module"/>
<xsl:key name="AC" match="tei:classSpec[@type='atts']" use="@module"/>
<xsl:key name="MC" match="tei:classSpec[@type='model']" use="@module"/>


  <xsl:template match="/">
    <html>
      <head>
	<title><xsl:value-of select="$langName"/>
	  TEI Localization</title>
	<meta name="keywords" content="TEI, "/>
	<style type="text/css">
	  td {
	  vertical-align: top;
	  }
	  td.att {width: 50%; }
	  td.class {width: 50%; }
	  td.gloss {
	   color:blue;
	   width: 50%; 
	   }
	  td.desc {
	   width: 50%; 
	   }
	  span.macro {
	    font-weight: bold;
	  }
	  span.class {
	    font-weight: bold;
	  }
	  span.att {
	    color: green;
	  }
	</style>
      </head>
      <body>
	
	<h1><xsl:value-of select="$langName"/> TEI Localization</h1>

	  <p>This is the <xsl:value-of select="$langName"/>
	  localization of the TEI standard. Below please find
	  translations of definitions of all elements, attributes,
	  attribute values, attribute classes, model classes and
	  macros.  In case of
	  discrepancies the current English standard available at the
	  TEI website is authoritative. </p>

<xsl:choose>

  <xsl:when test="$lang='zh-tw'">
    <p>The localization is
    funded by the National Digital Archive Program 國家數位典藏計
    畫 and Dharma Drum Buddhist College 法鼓佛教研修學院.
    </p>
    <p>Translators: Wu Tianling 吳恬綾, Huang Weining 黃韋寧<br/>
    Project Directors: Marcus Bingenheimer, Aming Tu</p>
  </xsl:when>

  <xsl:when test="$lang='ja'">
    <p>The localization was prepared by
    OHYA Kazushi (大矢 一志 ),  Tsurumi University, Yokohama</p>
  </xsl:when>

  <xsl:when test="$lang='es'">
    <p>The localization was prepared by 
    Carmen Arronis Llopis, University of Alicante</p>
  </xsl:when>

  <xsl:when test="$lang='fr'">
    <p>The localisation is managed by a consortium
    chaired by Jean-Luc Benoit, ATILF</p>
  </xsl:when>

  <xsl:when test="$lang='it'">
    <p>The localisation was prepared by
    Marco Venuti (University of Venice) and Letizia Cirillo
    (University of Bologna) </p>
  </xsl:when>
</xsl:choose>
<p>Modules:</p>
<ol>
  <ul>
    <xsl:for-each select="key('Mo',1)">
      <xsl:sort select="@ident"/>
      <li><a href="#{@ident}">
	<xsl:value-of select="@ident"/>
      </a></li>
    </xsl:for-each>
  </ul>                     
    </ol>
        
    <xsl:apply-templates select="key('Mo',1)"/>
    
      </body>
    </html>
  </xsl:template>
  
  <xsl:template match="tei:moduleSpec">
    
    <h2>
    <a name="{@ident}"> [<xsl:value-of select="@ident"/></a>]
    <xsl:value-of select="tei:desc"/></h2>


    <table border="1">
      <xsl:for-each select="key('E',@ident)">
	<xsl:sort select="@ident"/>
	<tr>
	  <td><table><tr>
	    <td class="class">
	      <b><xsl:text>&lt;</xsl:text>
	      <xsl:value-of select="@ident"/>
	      <xsl:text>&gt;</xsl:text></b>
	    </td>
	    <td class="class"></td>
	  </tr>
	  <tr> 
	    <td class="gloss"> 
	    <xsl:value-of select="tei:gloss[not(@xml:lang)]"/></td>
	    <td class="gloss"> 
	    <xsl:value-of select="tei:gloss[@xml:lang=$lang]"/></td>
	  </tr>
	  <tr> 
	    <td class="desc"> <xsl:value-of select="tei:desc[not(@xml:lang)]"/></td>
	    <td class="desc"> <xsl:value-of select="tei:desc[@xml:lang=$lang]"/></td>
	  </tr>
	  <xsl:if test="tei:attList">
	    <tr>
	      <td colspan="2"><table border="1">
		<thead><b>Attributes:</b></thead>
		<xsl:for-each select="tei:attList/tei:attDef">
		  <tr>
		    <td class="att"><span class="att">
		    <xsl:value-of select="./@ident"/>
		    </span><br/>
		  <xsl:value-of select="tei:desc[not(@xml:lang)]"/>
		  <xsl:if test="tei:valDesc[not(@xml:lang)]">
		    <br/>(<xsl:value-of select="tei:valDesc[not(@xml:lang)]"/>)
		  </xsl:if>
		  <xsl:if test="tei:valList">
		    <p style="margin-left:5%;">
		      <br/><span style="color:grey">Value list:</span>
		      <xsl:for-each select="tei:valList/tei:valItem">
			<span style="color:brown;margin-left:5%;"><br/><xsl:value-of select="./@ident"/>
			<br/><xsl:value-of select="tei:gloss[not(@xml:lang)] | desc[not(@xml:lang)]"/></span>
		      </xsl:for-each>
		    </p>
		  </xsl:if>
		</td>
		<td><span style="color:green"><xsl:value-of select="./@ident"/></span><br/>
		<xsl:value-of select="tei:desc[@xml:lang=$lang]"/>
		<xsl:if test="valDesc[@xml:lang=$lang]">
		  <br/>(<xsl:value-of select="valDesc[@xml:lang=$lang]"/>)
		</xsl:if>
		<xsl:if test="tei:valList">
		  <p style="margin-left:5%;">
		    <br/><span style="color:grey">Value list:</span>
		    <xsl:for-each select="tei:valList/tei:valItem">
		      <span style="color:brown;margin-left:5%;"><br/><xsl:value-of select="./@ident"/>
		      <br/><xsl:value-of select="tei:gloss[@xml:lang=$lang] | desc[@xml:lang=$lang]"/></span>
		    </xsl:for-each>
		  </p>
		</xsl:if>
		</td>
		  </tr>		  
		</xsl:for-each>
	      </table>
	      </td>
	    </tr>
	  </xsl:if>
	</table></td></tr>
      </xsl:for-each>
    </table>

    <xsl:if test="count(key('AC',@ident))&gt;0">
    <table>
	<tr><th colspan="2"> <h1><a name="attribute">2. Attribute
	Classes</a></h1><br/>(Attribute classes group together
	elements which share some set of common attributes)</th></tr>
	<xsl:for-each select="key('AC',@ident)">
	  <xsl:sort select="@ident"/>
	  <tr>
	    <td class="class"><span class="class"><xsl:value-of
	    select="@ident"/>
	  </span>
	    <br/>
	    <xsl:value-of select="tei:desc[not(@xml:lang)]"/></td>
	    <td class="class"><br/>
	    <xsl:value-of select="tei:desc[@xml:lang=$lang]"/></td>
	  </tr>
	</xsl:for-each>
    </table>
    </xsl:if>

    <xsl:if test="count(key('MC',@ident))&gt;0">

      <table>
	<tr><th colspan="2"><h1><a name="model">3. Model Classes</a></h1><br/>(Members of a given TEI model class share the property that they can all appear in the same location within a document)</th></tr>

	<xsl:for-each select="key('MC',@ident)">
	  <xsl:sort select="@ident"/>
	  <tr>
	    <td  class="class"><span class="class"><xsl:value-of select="@ident"/></span><br/><xsl:value-of select="tei:desc[not(@xml:lang)]"/></td>
	    <td  class="class"><br/><xsl:value-of select="tei:desc[@xml:lang=$lang]"/></td>
	  </tr>
	</xsl:for-each>
      </table>
    </xsl:if>
    

    <xsl:if test="count(key('Ma',@ident))&gt;0">
      <table>
	<tr><th colspan="2"><h1><a name="macro">4. Macros</a></h1>
	<br/>(Shortcut names for frequently occurring parts of other
	declarations)</th></tr>
	<xsl:for-each  select="key('Ma',@ident)">

	  <xsl:sort select="@ident"/>
	  <tr>
	    <td class="class"><span class="macro">
	      <xsl:value-of select="@ident"/>
	      </span>
	      <br/>
	    <xsl:value-of select="tei:desc[not(@xml:lang)]"/></td>
	    <td class="class"><br/>
	    <xsl:value-of select="tei:desc[@xml:lang=$lang]"/>
	    </td>
	  </tr>
	</xsl:for-each>
      </table>
    </xsl:if>
    
  </xsl:template>
  
   
    
</xsl:stylesheet>
