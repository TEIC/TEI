<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:v="urn:schemas-microsoft-com:vml"
                xmlns:o="urn:schemas-microsoft-com:office:office"
                xmlns:w="urn:schemas-microsoft-com:office:word"
                xmlns:dt="uuid:C2F41010-65B3-11d1-A29F-00AA00C14882"
                xmlns:h="http://www.w3.org/1999/xhtml"
                exclude-result-prefixes="h v o w dt"
                version="1.0">

   <xsl:output method="xml" indent="yes"/>

   <xsl:param name="filename"/>

   <xsl:key name="H" match="h:h1" use="1"/>
   <xsl:key name="H" match="h:h2" use="1"/>
   <xsl:key name="H" match="h:h3" use="1"/>
   <xsl:key name="H" match="h:h4" use="1"/>

   <xsl:template match="h:html">
      <xsl:variable name="author">
         <xsl:choose>
            <xsl:when test="h:head/h:meta[@name='Author']">
               <xsl:value-of select="h:head/h:meta[@name='Author']/@content"/>
            </xsl:when>
            <xsl:when test="h:body/h:address">
               <xsl:variable name="add">
                  <xsl:value-of select="h:body/h:address"/>
               </xsl:variable>
               <xsl:choose>
                  <xsl:when test="contains($add,'Author: ')">
                     <xsl:value-of select="substring-after($add,'Author: ')"/>
                  </xsl:when>
                  <xsl:otherwise>
                     <xsl:value-of select="$add"/>
                  </xsl:otherwise>
               </xsl:choose>
            </xsl:when>
            <xsl:otherwise>OUCS</xsl:otherwise>
         </xsl:choose>
      </xsl:variable>
      <xsl:variable name="date">
         <xsl:choose>
            <xsl:when test="h:body/h:address">
               <xsl:variable name="add">
                  <xsl:value-of select="h:body/h:address"/>
               </xsl:variable>
               <xsl:choose>
                  <xsl:when test="contains($add,'Date: ')">
                     <xsl:value-of select="substring-after($add,'Date: ')"/>
                  </xsl:when>
                  <xsl:when test="contains($add,'Last updated: ')">
                     <xsl:value-of select="substring-after($add,'Last updated: ')"/>
                  </xsl:when>
                  <xsl:when test="contains($add,'Last Updated: ')">
                     <xsl:value-of select="substring-after($add,'Last Updated: ')"/>
                  </xsl:when>
                  <xsl:otherwise>
                     <xsl:value-of select="$add"/>
                  </xsl:otherwise>
               </xsl:choose>
            </xsl:when>
            <xsl:otherwise>OUCS</xsl:otherwise>
         </xsl:choose>
      </xsl:variable>
      <TEI.2>
         <teiHeader>
            <fileDesc>
               <titleStmt>
                  <title>
                     <xsl:apply-templates select="h:head/h:title"/>
                  </title>
                  <author>
                     <xsl:value-of select="$author"/>
                  </author>
               </titleStmt>
               <editionStmt>
                  <edition>
  	                  <date>
                        <xsl:value-of select="$date"/>
                     </date>
                  </edition>
               </editionStmt>
               <publicationStmt>
                  <authority>OUCS</authority>
	                 <address>
                     <email>advisory@oucs.ox.ac.uk</email>
                  </address>
               </publicationStmt>
               <sourceDesc>
                  <p>This is the master version of an original document,
created by converting an HTML file to TEI XML on TODAYSDATE.</p>
               </sourceDesc>
            </fileDesc>
            <revisionDesc>
               <change>
	                 <date>$Date$</date>
                  <respStmt>
                     <name>$Author$</name>
                  </respStmt>
	                 <item>$Revision$</item>
               </change>
            </revisionDesc>
         </teiHeader>
         <text>
            <xsl:apply-templates/>
         </text>
      </TEI.2>
      <xsl:text>
</xsl:text>
   </xsl:template>

   <xsl:template match="h:head"/>

   <xsl:template match="h:body">
      <body>
         <xsl:apply-templates/>
         <!--This code is intended to balance the div tags created by headers with -->
  <!--     the correct number of /divs. At the moment it fails if there are -->
  <!-- Hn tags inside td's, which we are intending to make cell labels -->
   <xsl:variable name="starter">    
            <xsl:value-of select="substring(name(key('H',1)[1]),2)"/>
         </xsl:variable>
         <xsl:variable name="ender">
            <xsl:value-of select="substring(name(key('H',1)[last()]),2)"/>
         </xsl:variable>
         <xsl:variable name="difference">
            <xsl:value-of select="$starter - $ender"/>
         </xsl:variable>
         <xsl:comment>
     Started with a <xsl:value-of select="$starter"/>, ended with a <xsl:value-of select="$ender"/>
   diff=<xsl:value-of select="$difference"/>
         </xsl:comment>
         <!-- test call of for loop to handle closure of divs
got to find a way to set a trigger for closing div if no headers -->
 <xsl:call-template name="for-loop">
            <xsl:with-param name="i" select="$ender"/>
            <xsl:with-param name="stepsize" select="1"/>
            <xsl:with-param name="until" select="$starter"/>
            <xsl:with-param name="insert" select="'/div'"/>
         </xsl:call-template> 
         <!-- old div closing code was here-->

  </body>
   </xsl:template>


   <xsl:template match="h:h1|h:h2|h:h3|h:h4">
  <!--headers inside td's are labels-->
  <xsl:choose>
         <xsl:when test="parent::h:td">
            <emph>
               <xsl:value-of select="."/>
            </emph>
         </xsl:when>
         <xsl:otherwise>
            <xsl:if test="preceding::h:h1 or preceding::h:h2 or preceding::h:h3 or preceding::h:h4">
               <xsl:variable name="previous">
                  <xsl:value-of select="substring(name(preceding::*[name()='h1' or name()='h2' or name()='h3' or name()='h4'][1]),2)"/>
               </xsl:variable>
               <xsl:variable name="me">
                  <xsl:value-of select="substring(name(.),2)"/>
               </xsl:variable>
               <xsl:variable name="difference">
                  <xsl:value-of select="$me - $previous"/>
               </xsl:variable>
               <!--
   <xsl:message>
 I am a <xsl:value-of select="$me"/>, previous is <xsl:value-of select="$previous"/>, difference is <xsl:value-of select="$difference"/>

   </xsl:message>
-->
<!-- test call of for loop to handle this ... -->
<xsl:if test="$difference &lt; 1">
                  <xsl:call-template name="for-loop">
                     <xsl:with-param name="i" select="$previous"/>
                     <xsl:with-param name="stepsize" select="1"/>
                     <xsl:with-param name="until" select="$me"/>
                     <xsl:with-param name="insert" select="'/div'"/>
                  </xsl:call-template>
               </xsl:if>
               <xsl:if test="$difference &gt; 1">
                  <xsl:call-template name="for-loop">
                     <xsl:with-param name="i" select="$previous"/>
                     <xsl:with-param name="stepsize" select="1"/>
                     <xsl:with-param name="until" select="$me - 2"/>
                     <xsl:with-param name="insert" select="'div'"/>
                  </xsl:call-template> 
               </xsl:if>
            </xsl:if>
            <xsl:choose>
               <xsl:when test="h:a[@name]">
                  <xsl:text disable-output-escaping="yes">&lt;div id="</xsl:text>
                  <xsl:value-of select="h:a[@name]/@name"/>
                  <xsl:text disable-output-escaping="yes">"&gt;</xsl:text>
               </xsl:when>
               <xsl:when test="preceding::h:a[1][@name]">
                  <xsl:text disable-output-escaping="yes">&lt;div id="</xsl:text>
                  <xsl:value-of select="preceding::h:a[1][@name]/@name"/>
                  <xsl:text disable-output-escaping="yes">"&gt;</xsl:text>
               </xsl:when>   
               <xsl:otherwise>
                  <xsl:text disable-output-escaping="yes">&lt;div&gt;</xsl:text>
               </xsl:otherwise>
            </xsl:choose>
            <head>
               <xsl:apply-templates mode="head"/>
            </head>
         </xsl:otherwise>
      </xsl:choose>
   </xsl:template>

   <!-- low-level markup -->
<xsl:template match="h:td/h:p">
      <xsl:apply-templates/>
   </xsl:template>

   <xsl:template match="h:p">
      <p>
         <xsl:apply-templates/>
      </p>
   </xsl:template>

   <xsl:template match="h:strong|h:b|h:em|h:i" mode="head">
      <xsl:apply-templates/>
   </xsl:template>

   <xsl:template match="h:strong|h:b">
      <hi>
         <xsl:apply-templates/>
      </hi>
   </xsl:template>

   <xsl:template match="h:tt|h:var|h:kbd|h:samp|h:code">
      <code>
         <xsl:apply-templates/>
      </code>
   </xsl:template>

   <xsl:template match="h:em|h:i">
      <emph>
         <xsl:apply-templates/>
      </emph>
   </xsl:template>

   <xsl:template match="h:font"><!-- just continue -->
  <xsl:apply-templates/>
   </xsl:template>

   <xsl:template match="h:big|h:small|h:center"><!-- just continue -->
  <xsl:apply-templates/>
   </xsl:template>

   <xsl:template match="h:span"><!-- just continue -->
  <xsl:apply-templates/>
   </xsl:template>
   <xsl:template match="h:span[@class]"><!-- just continue -->
     <hi rend="{@class}">
       <xsl:apply-templates/>
     </hi>
   </xsl:template>
   <!-- links -->

<xsl:template match="h:a[@href]">
      <xsl:variable name="content" select="descendant-or-self::text()"/>
      <xsl:choose>
         <xsl:when test="@href = $content">
            <xsl:choose>
               <xsl:when test="starts-with(@href,'#')">
                  <ptr target="{substring(@href,2)}"/>
               </xsl:when>
               <xsl:otherwise>
                  <xptr url="{@href}"/>
               </xsl:otherwise>
            </xsl:choose>
         </xsl:when>
         <xsl:otherwise>
            <xsl:choose>
               <xsl:when test="starts-with(@href,'#')">
                  <ref target="{substring(@href,2)}">
                     <xsl:value-of select="$content"/>
                  </ref>
               </xsl:when>
               <xsl:otherwise>
                  <xref url="{@href}">
                     <xsl:value-of select="$content"/>
                  </xref>
               </xsl:otherwise>
            </xsl:choose>
         </xsl:otherwise>
      </xsl:choose>
   </xsl:template>

   <!-- ====== quotes, verbatim ====== -->
<xsl:template match="h:blockquote">
      <p>
         <q rend="display">
            <xsl:apply-templates/>
         </q>
      </p>
   </xsl:template>

   <xsl:template match="h:pre">
      <eg>
         <xsl:apply-templates/>
      </eg>
   </xsl:template>

   <!-- ============ Lists ============== -->
<xsl:template match="h:dl">
      <list type="gloss">
         <xsl:apply-templates/>
      </list>
   </xsl:template>

   <xsl:template match="h:dt">
      <label>
         <xsl:apply-templates/>
      </label>
   </xsl:template>

   <xsl:template match="h:dd">
      <item>
         <xsl:apply-templates/>
      </item>
   </xsl:template>

   <xsl:template match="h:ol">
      <list type="ordered">
         <xsl:apply-templates/>
      </list>
   </xsl:template>

   <xsl:template match="h:ul">
      <list type="unordered">
         <xsl:apply-templates/>
      </list>
   </xsl:template>

   <xsl:template match="h:li">
      <item>
         <xsl:apply-templates/>
      </item>
   </xsl:template>

   <!-- ===== sub, sup -->
<xsl:template match="h:sub">
      <hi rend="sub">
         <xsl:apply-templates/>
      </hi>
   </xsl:template>
   <xsl:template match="h:sup">
      <hi rend="sup">
         <xsl:apply-templates/>
      </hi>
   </xsl:template>



   <!-- tables -->
<xsl:template match="h:tr">
      <row>
         <xsl:apply-templates/>
      </row>
   </xsl:template>

   <xsl:template match="h:p/h:table[@border]">
      <table border="{@border}">
         <xsl:apply-templates/>
      </table>
   </xsl:template>

   <xsl:template match="h:table[@border]">
      <p>
         <table border="{@border}">
            <xsl:apply-templates/>
         </table>
      </p>
   </xsl:template>

   <xsl:template match="h:td">
      <cell>
         <xsl:if test="@valign">
            <xsl:attribute name="valign">
               <xsl:value-of select="@valign"/>
            </xsl:attribute>
         </xsl:if>
         <xsl:if test="h:p[@align]">
            <xsl:attribute name="align">
               <xsl:value-of select="child::h:p/@align"/>
            </xsl:attribute>
         </xsl:if>
         <xsl:apply-templates/>
      </cell>
   </xsl:template>
   <xsl:template match="h:address"/>



   <xsl:template match="text()">
      <xsl:variable name="N">
         <xsl:value-of select="name(preceding-sibling::*[1])"/>
      </xsl:variable>
      <xsl:choose>
         <xsl:when test="$N='h1' or $N='h3' or $N='h3' or $N='h4'">
            <p>
               <xsl:value-of select="normalize-space(.)"/>
            </p>
         </xsl:when>
         <xsl:otherwise>
            <xsl:choose>
               <xsl:when test="parent::h:td">
                  <xsl:value-of select="."/>
               </xsl:when>
               <xsl:otherwise>
                  <xsl:value-of select="translate(.,' ',' ')"/>      
               </xsl:otherwise>
            </xsl:choose>
         </xsl:otherwise>
      </xsl:choose>
   </xsl:template>

   <xsl:template match="h:img">
      <figure file="{@src}">
         <xsl:if test="@alt">
            <figDesc>
               <xsl:value-of select="@alt"/>
            </figDesc>
         </xsl:if>
      </figure>
   </xsl:template>

   <xsl:template match="h:form|h:input|h:textarea|h:option|h:select">
      <xsl:copy>
         <xsl:copy-of select="@*"/>
         <xsl:apply-templates/>
      </xsl:copy>
   </xsl:template>
   <xsl:template name="for-loop">
<!-- simplified from tidwell p86. only does step up or down until -->
    <xsl:param name="i" select="1"/>
      <xsl:param name="stepsize" select="1"/>
      <xsl:param name="until" select="1"/>
      <xsl:param name="iteration" select="1"/>
      <xsl:param name="insert" select="''"/>

      <xsl:variable name="step">
  
         <xsl:choose>
            <xsl:when test="$until &lt; $i">
               <xsl:text>-</xsl:text>
               <xsl:value-of select="$stepsize"/>
            </xsl:when>
            <xsl:otherwise>
               <xsl:value-of select="$stepsize"/>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>  
    
      <xsl:text disable-output-escaping="yes">&lt;</xsl:text>      
      <xsl:value-of select="$insert"/>
      <xsl:text disable-output-escaping="yes">&gt;</xsl:text>
      <xsl:variable name="testpassed">
         <xsl:choose>
            <xsl:when test="$i != $until">
               <xsl:text>true</xsl:text>
            </xsl:when>
            <xsl:otherwise>
               <xsl:text>false</xsl:text>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>
      <xsl:if test="$testpassed = 'true'">    

         <xsl:call-template name="for-loop">
            <xsl:with-param name="i" select="$i + $step"/>
            <xsl:with-param name="stepsize" select="$stepsize"/>
            <xsl:with-param name="until" select="$until"/>
            <xsl:with-param name="iteration" select="$iteration + 1"/>
            <xsl:with-param name="insert" select="$insert"/>
         </xsl:call-template>
      </xsl:if>
   </xsl:template>
</xsl:stylesheet>