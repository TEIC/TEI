<!-- 
     Text Encoding Initiative Consortium XSLT stylesheet family version 3.0
     RCS: $Id: teihtml.xsl,v 1.9 2000/05/08 16:21:13 rahtz Exp rahtz $
     XSL stylesheet to format TEI XML documents to HTML or XSL FO
     
     Copyright 1999-2003 Text Encoding Initiative Consortium  
     #include LICENSE
--> 
<xsl:stylesheet
    version="1.0"
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    xmlns:rss="http://purl.org/rss/1.0/"
    xmlns:dc="http://purl.org/dc/elements/1.1/"
    xmlns:syn="http://purl.org/rss/1.0/modules/syndication/"
    xmlns:taxo="http://purl.org/rss/1.0/modules/taxonomy/"
    xmlns:html="http://www.w3.org/1999/xhtml"
    xmlns:cc="http://web.resource.org/cc/"
    extension-element-prefixes="yaslt"
    xmlns:yaslt="http://www.mod-xslt2.com/ns/1.0"
    exclude-result-prefixes="rdf dc syn taxo cc rss rdf html yaslt tei" 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
>
  <xsl:import href="../html/tei.xsl"/>


  
  <xsl:output 
      method="html"  
      doctype-public="-//W3C//DTD HTML 4.0 Transitional//EN" 
      doctype-system="http://www.w3.org/TR/html4/loose.dtd"
      indent="no"/>
  
  <!-- parameterization -->
  <xsl:param name="rsslimit">11</xsl:param>
  <xsl:param name="serverName">localhost</xsl:param>
  <xsl:param name="serverPort">8080</xsl:param>
  <xsl:param name="spacer"> &gt; </xsl:param>
  <xsl:param name="normalStyle">screen</xsl:param>
  <xsl:param name="navbarFile">../../../../../navbar.xml</xsl:param>
  <xsl:param name="lang">en</xsl:param>

  <xsl:variable name="URLPREFIX">
<!--
    <xsl:if test="not($serverName='')">
      <xsl:text>http://</xsl:text>
      <xsl:value-of select="$serverName"/>
      <xsl:text>:</xsl:text>
      <xsl:value-of select="$serverPort"/>
      <xsl:value-of select="substring-before($REQUEST,'TEIC/')"/>
      <xsl:text>TEIC</xsl:text>
    </xsl:if>
-->
  </xsl:variable>
  <xsl:template name="metaHook">
    <xsl:param name="title"/>
    <meta name="DC.Title" content="{$title}"/>
    <meta name="DC.Language" content="(SCHEME=iso639) en"/> 
    <meta name="DC.Creator" content="TEI,Oxford University Computing Services, 13 Banbury Road, Oxford OX2 6NN, United Kingdom"/>
    <meta name="DC.Creator.Address" content="tei@oucs.ox.ac.uk"/>
<!--
      <style>
	#hdr{
	 background: url("<xsl:value-of select="$URLPREFIX"/>/logos/TEI-glow.png");
	 background-repeat : no-repeat;
	}
      </style>
-->
 <xsl:if test="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:notesStmt/tei:note[@type='rsslink']/tei:ref">
   <xsl:for-each select="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:notesStmt/tei:note[@type='rsslink']">
     <link href="{tei:ref/@target}" rel="alternate" type="application/rss+xml" title="{xref}" />
   </xsl:for-each>
 </xsl:if>

  </xsl:template>
  
  
  <!-- *************** params ************************** -->
  <xsl:param name="STDOUT">true</xsl:param>
  <xsl:param name="alignNavigationPanel"></xsl:param>
  <xsl:param name="authorWord"></xsl:param>
  <xsl:param name="autoToc"></xsl:param>
  <xsl:param name="bottomNavigationPanel">true</xsl:param>
  <xsl:param name="cssFile">/release/xml/teip4/stylesheet/tei.css</xsl:param>
  <xsl:param name="cssSecondaryFile">/release/xml/teip4/stylesheet/teic.css</xsl:param>
  <xsl:param name="pageLayout">CSS</xsl:param>
  <xsl:param name="dateWord"></xsl:param>
  <xsl:param name="feedbackURL">http://www.tei-c.org/Consortium/TEI-contact.html</xsl:param>
  <xsl:param name="feedbackWords">Contact</xsl:param>
  <xsl:param name="homeURL">/</xsl:param>
  <xsl:param name="homeWords">TEI Home</xsl:param>
  <xsl:param name="institution">Text Encoding Initiative</xsl:param>
  <xsl:param name="parentURL">http://www.tei-c.org</xsl:param>
  <xsl:param name="parentWords">TEI </xsl:param>
  <xsl:param name="searchURL">http://search.ox.ac.uk/web/related/natproj/tei</xsl:param>
  <xsl:param name="searchWords">Search this site</xsl:param>
  <xsl:param name="showTitleAuthor">1</xsl:param>
  <xsl:param name="subTocDepth">-1</xsl:param>
  <xsl:param name="splitLevel">
    <xsl:choose>
      <xsl:when test="/tei:TEI/@rend='nosplit'">-1</xsl:when>
      <xsl:otherwise>0</xsl:otherwise>
    </xsl:choose>
  </xsl:param>
  <xsl:param name="topNavigationPanel"></xsl:param>
  <xsl:param name="urlChunkPrefix">.ID=</xsl:param>
  <xsl:template name="copyrightStatement">Copyright TEI Consortium</xsl:template>
  <xsl:param name="logoFile">TEI-175.jpg</xsl:param>
  <xsl:template name="logoPicture">
    <img src="/logos/{$logoFile}" alt="" width="180" />
  </xsl:template>
  
<xsl:template name="includeCSS">
  <link rel="stylesheet" type="text/css" media="screen">
    <xsl:attribute name="href">
      <xsl:value-of select="$URLPREFIX"/>
      <xsl:value-of select="$cssFile"/>
      <xsl:text></xsl:text>
    </xsl:attribute>
  </link>
  <link rel="stylesheet" type="text/css" media="screen">
    <xsl:attribute name="href">
      <xsl:value-of select="$URLPREFIX"/>
      <xsl:value-of select="$cssSecondaryFile"/>
    </xsl:attribute>
  </link>
</xsl:template>

  <xsl:template name="generateSubTitle">
    <xsl:variable name="authority">
      <xsl:value-of select="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:authority"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$authority=''">Text Encoding Initiative Consortium</xsl:when>
      <xsl:otherwise><xsl:value-of select="$authority"/></xsl:otherwise>
    </xsl:choose>
    
  </xsl:template>
  
  <xsl:template match="html:*">
    <xsl:element name="{local-name()}">
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>
  
  
  <xsl:template match="tei:p/tei:name">
    <b><xsl:apply-templates/></b>
  </xsl:template>
  
  
  <xsl:template match="tei:note[@type='proposal']" >
    <p align="right">
      <b>Proposal <xsl:number level="any" count="note"/> :</b>
      <xsl:apply-templates/>
    </p>
  </xsl:template>
  

  <!-- 
   ** We use <note type='action'> for action items in meeting minutes.
   ** The three logical children (who, what, and when; which would be
   ** encoded with <name>, <resp>, and <dueDate> in an <action> - see
   ** unused template below) are encoded as <label>, PCDATA, and
   ** <date>. This presents a problem, as it is difficult to
   ** appropriately process the "what" part without re-processing the
   ** "who" and "when". in this version of the template, we simply
   ** ignore all <label> and <date> children of the current <note>
   ** when we're pocessing the "what" part. Obviously not ideal, but
   ** it permits the "what" to have internal encoding except for
   ** <label> or <date>. To improve the template we could try to
   ** ignore only the 1st <label> child and the last <date> child.
   ** Better yet, we'd improve the encoding and have a child element
   ** instead of PCDATA for the "what" part.
   -->
  <xsl:template match="tei:note[@type='action']" >
    <p align="right">
      <b>Action <xsl:value-of select="tei:label"/> by <xsl:value-of select="tei:date"/> </b>
      <xsl:text>: </xsl:text>
      <xsl:apply-templates select="./*[not( name() = 'label'  or  name() = 'date')]|text()"/>
    </p>
  </xsl:template>
  
  <xsl:template match="tei:note[@type='action']" mode="table">
    <tr>
      <td><xsl:value-of select="tei:label"/></td>
      <td><xsl:apply-templates select="./*[not( name() = 'label'  or  name() = 'date')]|text()"/></td>
      <td><xsl:value-of select="tei:date"/> </td>
    </tr>
  </xsl:template>

  <xsl:template match="tei:front">
    <xsl:apply-templates/>
    <xsl:if test="//tei:note[@type='action']">
      <h4>Actions</h4>
      <table>
	<tr>
	  <td>Resp</td><td>action</td><td>due date</td>
	</tr>
	<xsl:apply-templates select="//tei:note[@type='action']" mode="table"/>
      </table>
    </xsl:if>    
  </xsl:template>
  
  <!-- templates to match Syd's boffo <action> tags -->
  <!-- currently unused, as we use <note type='action'>, templates above, instead -->
  <xsl:template match="tei:action" >
    <p align="right">
      <b>Action <xsl:value-of select="tei:name"/> by <xsl:value-of select="tei:dueDate"/> </b>
      <xsl:text>: </xsl:text>
      <xsl:apply-templates select="tei:resp"/>
    </p>
  </xsl:template>
  

<xsl:template name="stdfooter">
  <xsl:param name="date"/>
  <xsl:param name="author"/>
  <xsl:param name="style" select="'plain'"/>

  <hr/>
  <div class="inlinetoc">
    Style: 
    <a class="inlinetoc"  href="{$masterFile}?style=printable">Single file</a> |
    <a class="inlinetoc"  href="{$masterFile}?style={$normalStyle}">Normal</a> |
    <!--
	<a class="inlinetoc"  href="{$REQUEST}?style=.pdf">PDF</a> |
   -->
   <a class="inlinetoc"  href="{$masterFile}?style=raw">XML</a>
 </div>
 <hr/>
 
 <address>
   <xsl:value-of select="$date"/>
   <br/>
   <xsl:choose>
     <xsl:when
	 test="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:availability/rdf:RDF/cc:License">
       This document is copyright <xsl:call-template name="generateAuthor"/>
       and licensed under a <a>
       <xsl:attribute name="href">
	 <xsl:value-of
	     select="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:availability/rdf:RDF/cc:License/@rdf:about"/>
       </xsl:attribute>
     Creative Commons</a> licence</xsl:when>
     <xsl:otherwise>
       <xsl:if test="not($author='')">
	 <xsl:value-of select="$author"/>
       </xsl:if>
       <br/>
       Unless otherwise indicated, 
       these pages are <xsl:call-template name="copyrightStatement"/> 
     </xsl:otherwise>
   </xsl:choose>
   
   <xsl:comment><xsl:text>
     Generated </xsl:text>
     <xsl:if test="not($masterFile='index')">
       <xsl:text>from </xsl:text>
       <xsl:value-of select="$masterFile"/>
     </xsl:if>
     <xsl:text> using an XSLT version </xsl:text>
     <xsl:value-of select="system-property('xsl:version')"/> stylesheet
     based on <xsl:value-of select="$teixslHome"/>teihtml.xsl
     processed using <xsl:value-of select="system-property('xsl:vendor')"/>
     on <xsl:call-template name="whatsTheDate"/>
   </xsl:comment>
 </address>
</xsl:template>


<xsl:template name="crumbPath">
    <ul class="breadcrumb">
      <li class="breadcrumb-first">
	<a target="_top" class="breadcrumb" href="{$URLPREFIX}{$homeURL}">
	  <xsl:value-of select="$homeLabel"/>
	</a>
      </li>
      <xsl:call-template name="walkTree">
	<xsl:with-param name="path">
	  <xsl:choose>
	    <xsl:when test="contains($REQUEST,'.ID=')">
	      <xsl:value-of select="substring-before(substring-after($REQUEST,'TEIC/'),'.ID=')"/> 
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:value-of select="substring-after($REQUEST,'TEIC/')"/> 
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:with-param>
	<xsl:with-param name="class">breadcrumb</xsl:with-param>
      </xsl:call-template>
    </ul>
  </xsl:template>
  
  <xsl:template name="walkTree">
    <xsl:param name="path"/>
    <xsl:param name="class"/>
    <xsl:param name="whole" select="''"/>
    <xsl:choose>
      <xsl:when test="contains($path,'/')">
	<xsl:variable name="current">
	  <xsl:value-of select="substring-before($path,'/')"/>            
	</xsl:variable>
	<xsl:variable name="rest">
	  <xsl:value-of select="substring-after($path,'/')"/>            
	</xsl:variable>
	<xsl:call-template name="aCrumb">
	  <xsl:with-param name="crumbBody">
	    <xsl:choose>
	      <xsl:when test="$rest='index.xsp' and $ID=''">
		<xsl:value-of select="$current"/>
	      </xsl:when>
	      <xsl:when test="$rest='index.xml' and $ID=''">
		<xsl:value-of select="$current"/>
	      </xsl:when>
	      <xsl:otherwise>
		<a class="{$class}" target="_top">
		  <xsl:attribute name="href">
		    <xsl:value-of select="$URLPREFIX"/><xsl:value-of select="$whole"/>/<xsl:value-of select="$current"/>
		    <xsl:text>/</xsl:text>
		  </xsl:attribute>
		  <xsl:value-of select="$current"/>
		</a>
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:with-param>
	</xsl:call-template>
	<xsl:call-template name="walkTree"> 
	  <xsl:with-param name="class"><xsl:value-of select="$class"/></xsl:with-param>
	  <xsl:with-param name="path" select="$rest"/>
	  <xsl:with-param name="spacer" select="$spacer"/>
	  <xsl:with-param name="whole">
	    <xsl:value-of select="$whole"/>/<xsl:value-of select="$current"/>
	  </xsl:with-param>
	</xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
	<xsl:if test="not($path='index.xsp' or $path='index.xml')">
	  <xsl:call-template name="aCrumb">
	    <xsl:with-param name="crumbBody">
<a class="{$class}" target="_top">
	    <xsl:attribute name="href">
	      <xsl:value-of select="$URLPREFIX"/><xsl:value-of select="$whole"/>/<xsl:value-of select="$path"/>
	    </xsl:attribute>
	    <xsl:value-of select="$path"/>
	      </a>
	    </xsl:with-param>
	  </xsl:call-template>
	</xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template name="unknownRendBlock">
    <xsl:param name="rend"/>
    <xsl:param name="rest"/>
    <xsl:attribute name="class">
      <xsl:value-of select="$rend"/>
    </xsl:attribute>
    <xsl:call-template name="applyRend">
      <xsl:with-param name="parms" select="$rest"/>
    </xsl:call-template>
  </xsl:template>
  

  <xsl:template name="unknownRendInline">
    <xsl:param name="rend"/>
    <xsl:param name="rest"/>
    <span class="{$rend}">
      <xsl:if test="@xml:id">
	<xsl:attribute name="id"><xsl:value-of
	select="@xml:id"/></xsl:attribute>
      </xsl:if>
      <xsl:call-template name="applyRend">
	<xsl:with-param name="parms" select="$rest"/>
      </xsl:call-template>
    </span>
  </xsl:template>


<xsl:template match="tei:note[@place='end']">
  <xsl:variable name="identifier">
    <xsl:call-template name="noteN"/>
  </xsl:variable>
    <a class="notelink" href="#{concat('Note',$identifier)}">
    <sup><xsl:value-of select="$identifier"/></sup></a>
</xsl:template>


<xsl:template name="corpusBody">
<!--
  <p align="right">
   <xsl:for-each select="document('/Applications/preface.xml')/tei:TEI/tei:text/tei:body">
    <xsl:apply-templates/> 
   </xsl:for-each>
  </p>
</xsl:if>
-->
<xsl:if test="@id='apps'">
 <p>There are currently
<xsl:value-of select="count(tei:TEI)"/> projects represented here.
Why not add details of yours? Fill in the
<a href="newproj.xml">new project</a> form here.</p>
</xsl:if>

<table>
<tr>
 <td valign="top">
   <h4>Alphabetical Listing</h4>
   <p>
   <ul>
    <xsl:for-each select="//tei:TEI">
     <xsl:sort select="@n" order="ascending"/>
     <li>
      <a href="{@id}.xml">
       <xsl:value-of select=".//tei:titleStmt/tei:title"/>
      </a>
     </li>
    </xsl:for-each>
   </ul>
  </p>
 </td>
 <td valign="top">
   <h4>Date Listing</h4>
   <p>
   <ul>
    <xsl:for-each select="//tei:TEI">
    <xsl:sort select=".//tei:resp[text()!='edit'][1]/ancestor::tei:change/tei:date/@value" order="descending" data-type="text"/>
     <li>
      <a href="{@id}.xml">
	<xsl:value-of select=".//tei:titleStmt/tei:title"/>
      </a><xsl:text> </xsl:text>
       <xsl:call-template name="latestUpdate"/>
     </li>
    </xsl:for-each>
   </ul>
  </p>
 </td>
</tr>
</table>

</xsl:template>

<xsl:template name="latestUpdate">
  <xsl:variable name="respCreate" select="tei:teiHeader//tei:resp[text()='create']"/>
  <xsl:variable name="respUpdate" select="tei:teiHeader//tei:resp[text()='update']"/>
  <xsl:text>entry created </xsl:text>
  <xsl:choose>
    <xsl:when test="count($respCreate)!=1">
      <xsl:message terminate="yes">Error! File <xsl:value-of select="@id"/>.xml does not have exactly one create resp</xsl:message>        
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="tei:teiHeader//tei:resp[text()='create']/ancestor::tei:change/tei:date[1]"/>
    </xsl:otherwise>
  </xsl:choose>
  <xsl:if test="$respUpdate">
    <xsl:text>; last update </xsl:text>
    <xsl:for-each select="$respUpdate">
      <xsl:sort select="ancestor::tei:change/tei:date/@value" order="descending" data-type="text"/>
      <xsl:if test="position()=1">
        <xsl:value-of select="ancestor::tei:change/tei:date[1]"/>
      </xsl:if>
    </xsl:for-each>
  </xsl:if>
</xsl:template>


<xsl:template match="tei:ptr[@type='transclude' and
		     starts-with(@rend,'rss')]">
<xsl:call-template name="showRSS">
  <xsl:with-param name="url" select="@url"/>
  <xsl:with-param name="rend" select="@rend"/>
</xsl:call-template>
</xsl:template>

<xsl:template name="showRSS">
  <xsl:param name="url"/>
  <xsl:param name="rend"/>
  <xsl:param name="limit"/>
  <xsl:choose>
    <xsl:when test="document($url)/rdf:RDF">
      <xsl:for-each select="document($url)/rdf:RDF">  
	<xsl:variable name="Limit">
	  <xsl:choose>
	    <xsl:when test="not($limit ='')"><xsl:value-of select="$limit"/></xsl:when>
	    <xsl:when test="$rend='rssfirst'">2</xsl:when>
	    <xsl:otherwise><xsl:value-of select="$rsslimit"/></xsl:otherwise>
	  </xsl:choose>
	</xsl:variable>
	<ul type="rss">
	  <xsl:for-each select="rss:item[position() &lt; $Limit]">
	    <li class="rss"><a href="{normalize-space(rss:link)}">
	      <xsl:apply-templates select="rss:title"/></a>     
	      <xsl:if test="not($rend='rssbrief')">
		<br/><xsl:apply-templates select="rss:description"/> 
	      </xsl:if>
	      <xsl:if test="dc:created">
		<br/>[item created <xsl:apply-templates select="dc:created"/>]
	      </xsl:if>
	    </li>
	  </xsl:for-each>
	</ul>
      </xsl:for-each>
    </xsl:when>
    <xsl:when test="document($url)/rss[@version='2.0']">
      <ul class="rss">
	<xsl:for-each select="document($url)/rss/channel/item">
	  <li class="rss">
	    <a href="{link}"><xsl:value-of select="title"/></a>&#160;
	      <xsl:if test="not($rend='rssbrief')">
		<xsl:value-of disable-output-escaping="yes"
			      select="description"/>
	      </xsl:if>
	  </li>
      </xsl:for-each>
      </ul>
    </xsl:when>
  </xsl:choose>
</xsl:template>

<xsl:template name="searchbox">
  <form target="_blank" action="http://www.google.com/search" method="get">
    <input value="www.tei-c.org.uk" 
	   name="as_sitesearch" 
   type="hidden"/>
    <input size="12" name="as_q" id="query" type="text"/>
    <input name="Search" value="Search" type="submit"/>
  </form>

</xsl:template>

</xsl:stylesheet>


