<!-- 
Text Encoding Initiative Consortium XSLT stylesheet family
$Date$, $Revision$, $Author$

XSL stylesheet to format TEI XML documents to HTML or XSL FO

 
#include LICENSE
--> 

<xsl:stylesheet
  xmlns:tei="http://www.tei-c.org/ns/1.0"
 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
  version="1.0"  >

<xsl:template name="doPageTable">
 <xsl:param name="currentID"/>
 <xsl:variable name="BaseFile">
   <xsl:value-of select="$masterFile"/>
   <xsl:call-template name="addCorpusID"/>
 </xsl:variable>

 <xsl:call-template name="outputChunk">
   <xsl:with-param name="ident">
     <xsl:choose>
       <xsl:when test="$STDOUT='true'"/>
       <xsl:when test="not($currentID='')">
         <xsl:value-of select="$currentID"/>
       </xsl:when>
       <xsl:otherwise>
         <xsl:value-of select="$BaseFile"/>         
       </xsl:otherwise>
     </xsl:choose>
   </xsl:with-param>
   <xsl:with-param name="content">
    <xsl:call-template name="tableFrame">       
      <xsl:with-param name="currentID" select="$currentID"/>
    </xsl:call-template>
  </xsl:with-param>
 </xsl:call-template>
</xsl:template>

<xsl:template name="tableFrame">
<xsl:param name="currentID"/>
<xsl:param name="content"/>
<html><xsl:call-template name="addLangAtt"/>
 <xsl:comment>THIS FILE IS GENERATED FROM AN XML MASTER. 
              DO NOT EDIT</xsl:comment>
 <head>
   <xsl:variable name="pagetitle">
     <xsl:choose>
      <xsl:when test="$currentID=''">
          <xsl:call-template name="generateTitle"/>
      </xsl:when>
      <xsl:otherwise>
          <xsl:call-template name="generateTitle"/>:
        <xsl:choose>
          <xsl:when test="count(key('IDS',$currentID))&gt;0">
             <xsl:for-each select="key('IDS',$currentID)">  
                <xsl:apply-templates select="." mode="header"/>
             </xsl:for-each>
          </xsl:when>
          <xsl:otherwise>
           <xsl:apply-templates select="descendant::tei:text" mode="xpath">
               <xsl:with-param name="xpath" select="$currentID" />
               <xsl:with-param name="action" select="'header'" />
           </xsl:apply-templates>
         </xsl:otherwise>
       </xsl:choose>
      </xsl:otherwise>
     </xsl:choose>
   </xsl:variable>
     <title><xsl:value-of select="$pagetitle"/></title>
 <xsl:call-template name="includeCSS"/>
 <xsl:call-template name="metaHook">
   <xsl:with-param name="title" select="$pagetitle"/>
 </xsl:call-template>
 <xsl:call-template name="javaScript"/>
 </head>
 <body  class="pagetable" >
  <xsl:call-template name="bodyHook"/>
  <xsl:call-template name="bodyJavaScript"/>

  <xsl:call-template name="startHeader"/>

 <table border="0">
   <tr>
     <td align="left" valign="top" rowspan="2" width="{$linksWidth}" class="linktext">
       <xsl:call-template name="leftHandFrame">
         <xsl:with-param name="currentID" select="$ID"/>
       </xsl:call-template>
     </td>
     <td  valign="top">
      <xsl:call-template name="generateSubTitle"/>
      <h2 class="maintitle"><xsl:call-template name="generateTitle"/></h2>
     </td>
     <td valign="top">
       <xsl:call-template name="searchbox"/>
     </td>
   </tr>
   <tr>
    <td  valign="top" class="maintext" colspan="2">
     <xsl:choose>
         <xsl:when test="$currentID='#'">
            <xsl:copy-of select="$content"/>
          </xsl:when>
          <xsl:otherwise>
           <xsl:call-template name="mainFrame">
            <xsl:with-param name="currentID" select="$currentID"/>
           </xsl:call-template>
          </xsl:otherwise>
      </xsl:choose>
     </td>
   </tr>
  </table>
  <xsl:call-template name="endFooter"/>
 </body>
</html>
</xsl:template>

<xsl:template name="endFooter"/>

<xsl:template name="startHeader"/>

<xsl:template match="*" mode="paging">

  <xsl:choose>
     <xsl:when test="self::tei:divGen[@type='summary']">
           <xsl:call-template name="summaryToc"/>
     </xsl:when>
     <xsl:when test="self::tei:divGen">
       <xsl:apply-templates select="."/>
     </xsl:when>
     <xsl:when test="starts-with(name(),'div')">
      <xsl:if test="not(preceding-sibling::tei:*) or preceding-sibling::tei:titlePage">
           <h2><xsl:apply-templates select="." mode="header"/></h2>
           <xsl:call-template name="doDivBody"/>
           <xsl:call-template name="printDivnotes"/>
                <xsl:if test="$bottomNavigationPanel='true'">
                 <xsl:call-template name="xrefpanel">
                  <xsl:with-param name="homepage" 
                   select="concat($masterFile,$standardSuffix)"/>
                  <xsl:with-param name="mode" select="name(.)"/>
                 </xsl:call-template>
                </xsl:if>
      </xsl:if>
     </xsl:when>
     <xsl:otherwise>
       <xsl:apply-templates select="."/>
       <xsl:apply-templates select="following-sibling::tei:*[1]" mode="paging"/>
     </xsl:otherwise>    
  </xsl:choose>

</xsl:template>


<xsl:template name="leftHandFrame">
  <xsl:param name="currentID"/>
     <xsl:call-template name="logoFramePicture"/>
     <br/>
     <xsl:if test="$feedbackURL">
     <a target="_top" href="{$feedbackURL}" class="frametoc">
        <xsl:call-template name="feedbackWords"/></a>
     <br/>
     </xsl:if>

     <hr/>
     <xsl:choose>
      <xsl:when test="$currentID=''">
         <xsl:call-template name="linkListContents">
           <xsl:with-param name="style" select="'frametoc'"/>
         </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:choose>
          <xsl:when test="count(key('IDS',$currentID))&gt;0">
             <xsl:for-each select="key('IDS',$currentID)">  
               <xsl:call-template name="linkListContents">
                 <xsl:with-param name="style" select="'frametoc'"/>
               </xsl:call-template>
             </xsl:for-each>
          </xsl:when>
          <xsl:otherwise>
           <xsl:apply-templates select="descendant::tei:text" mode="xpath">
               <xsl:with-param name="xpath" select="$currentID" />
               <xsl:with-param name="action" select="'toclist'" />
           </xsl:apply-templates>
        </xsl:otherwise>
       </xsl:choose>
      </xsl:otherwise>
     </xsl:choose>
</xsl:template>

<xsl:template name="mainFrame">
 <xsl:param name="currentID"/>
       <xsl:choose>
      <xsl:when test="$currentID=''">
   <!-- we need to locate the first interesting object in the file, ie
    the first grandchild of <text > -->
      <xsl:for-each select=" descendant::tei:text/tei:*[1]/tei:*[1]">
        <xsl:apply-templates select="." mode="paging"/>


        <xsl:if test="following-sibling::tei:div/tei:head and not(ancestor-or-self::tei:TEI[@rend='nomenu'])">
          <xsl:call-template name="contentsHeading"/>
          <ul class="toc">
              <xsl:apply-templates 
                select="following-sibling::tei:div" mode="maketoc">
                <xsl:with-param name="forcedepth" select="'0'"/>
              </xsl:apply-templates>
          </ul>
        </xsl:if>
      </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <xsl:choose>
          <xsl:when test="count(key('IDS',$currentID))&gt;0">
             <xsl:for-each select="key('IDS',$currentID)">  
                <h2><xsl:apply-templates select="." mode="header"/></h2>
                <xsl:call-template name="doDivBody"/>
		<xsl:call-template name="printDivnotes"/>
                <xsl:if test="$bottomNavigationPanel='true'">
                 <xsl:call-template name="xrefpanel">
                  <xsl:with-param name="homepage" 
                   select="concat($masterFile,$standardSuffix)"/>
                  <xsl:with-param name="mode" select="name(.)"/>
                 </xsl:call-template>
                </xsl:if>
             </xsl:for-each>
          </xsl:when>
          <xsl:otherwise>
           <!-- the passed ID is a pseudo-XPath expression
            which starts below TEI/tei:text.
            The real XPath syntax is changed to avoid problems
            -->
           <xsl:apply-templates select="ancestor-or-self::tei:TEI/descendant::tei:text" mode="xpath">
               <xsl:with-param name="xpath" select="$currentID" />
           </xsl:apply-templates>
        </xsl:otherwise>
       </xsl:choose>
      </xsl:otherwise>
     </xsl:choose>
     <xsl:call-template name="stdfooter">
          <xsl:with-param name="date">
           <xsl:call-template name="generateDate"/>
          </xsl:with-param>
          <xsl:with-param name="author">
            <xsl:call-template name="generateAuthorList"/>
          </xsl:with-param>
          <xsl:with-param name="style" select="'framestdlink'"/>
      </xsl:call-template>
</xsl:template>
</xsl:stylesheet>

