<?xml version="1.0"?>
<!--
#######################################
Roma Stylesheet

#######################################
author: Arno Mittelbach <arno-oss@mittelbach-online.de>
version: 0.9
date: 10.06.2004

#######################################
Description

-->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:param name="elementName"/>
<xsl:param name="elementDesc"/>
<xsl:param name="elementClasses"/>
<xsl:param name="elementContents"/>
<xsl:param name="elementFullContents"/>
<xsl:param name="elementsModule"/>
<xsl:param name="MESSAGE"/>
<xsl:param name="host">http://localhost:8080/cocoon/Query</xsl:param>
<xsl:param name="selectedMode">addElement</xsl:param>
<xsl:param name="elementChangedName"/>
<xsl:param name="module"/>
<xsl:param name="changeNameERROR"/>


  <xsl:template match="/">
   <p class="roma">
    
    <xsl:if test="$selectedMode='addElement'">
      <a href="?mode=listAddedElements">go back to list</a><br/>
    </xsl:if>
    <xsl:if test="$selectedMode='changeElement'">
      <a><xsl:attribute
      name="href">?mode=changeModule&amp;module=<xsl:value-of select="$module"/></xsl:attribute>go back to list</a><br/>
    </xsl:if>

    <form method="POST">
     <xsl:attribute name="action"><xsl:if
test="$selectedMode='addElement'">?mode=elementAdded</xsl:if><xsl:if
test="$selectedMode='changeElement'">?mode=elementChanged</xsl:if></xsl:attribute>
     <input type="hidden" name="module">
      <xsl:attribute name="value"><xsl:value-of
       select="$elementsModule"/></xsl:attribute>
     </input>
     <xsl:if test="$selectedMode='addElement'">
       <input type="hidden" name="added" value="true"/>
     </xsl:if>
     <table>
      <tr>
       <td class="headline" colspan="2"><xsl:value-of disable-output-escaping="yes" select="$res_form_headline"/></td>
      </tr>
      <tr>
	<xsl:if test="//errorList/error/location[text()='name']">
	  <xsl:attribute name="class">error</xsl:attribute>
	</xsl:if>
       <td class="formlabel"><xsl:value-of disable-output-escaping="yes" select="$res_form_name"/></td>
       <td class="formfield">
	 <xsl:if test="not($selectedMode='changeElement')">
	   <xsl:if test="not($elementName='')">
	     <xsl:value-of select="$elementName"/>
	     <input type="hidden" name="name">
	       <xsl:attribute name="value">
		 <xsl:value-of select="$elementName"/>
	       </xsl:attribute>
	     </input>
	   </xsl:if>  
	   <xsl:if test="$elementName=''">
	     <input type="text" size="53" name="name">
	       <xsl:if
		test="//errorList/error/location[text()='name']">
		 <xsl:attribute name="value"><xsl:value-of select="//errorList/error[child::location[text()='name']]/oldValue"/></xsl:attribute>
	       </xsl:if>
	     </input>
	   </xsl:if>  
	 </xsl:if>
	 <xsl:if test="$selectedMode='changeElement'">
	   <input type="text" size="53" name="name">
	     <xsl:if test="not($elementName='')">
	       <xsl:attribute name="value"><xsl:value-of
	       select="$elementName"/></xsl:attribute>
	       <xsl:if test="$selectedMode='changeElement'">
		 <xsl:attribute name="readonly">true</xsl:attribute>
	       </xsl:if>
	     </xsl:if>
	   </input>
	 </xsl:if>
       </td>
      </tr>
      <xsl:if test="$selectedMode='changeElement'">
       <tr>
	 <xsl:if test="//errorList/error/location[node()='name']">
	   <xsl:attribute name="class">error</xsl:attribute>
	 </xsl:if>
        <td class="formlabel">Change Tagname</td>
        <td class="formfield">
         <input type="text" size="53" name="newName">
	   <xsl:if
	    test="//errorList/error/location[node()='name']">
	     <xsl:attribute name="value"><xsl:value-of
	    select="//errorList/error[child::location[node()='name']]/oldValue"/></xsl:attribute>
	    </xsl:if>
	    <xsl:if
	     test="not(//errorList/error/location[node()='name'])">
	    <xsl:attribute name="value"><xsl:value-of
	      select="$elementChangedName"/></xsl:attribute>
	    </xsl:if>
 	 </input>
        </td>    
       </tr>
      </xsl:if>
      <tr>
        <td class="formlabeltop"><xsl:value-of disable-output-escaping="yes" select="$res_form_modelClasses"/></td>
        <td>
           <xsl:call-template name="modelClassList"/>
        </td>
      </tr>
      <tr>
        <td class="formlabeltop"><xsl:value-of disable-output-escaping="yes" select="$res_form_attributeClasses"/></td>
        <td>
           <xsl:call-template name="attClassList"/>
         </td>
      </tr>
      <tr>
	<xsl:if test="//errorList/error/location[node()='contents']">
	  <xsl:attribute name="class">error</xsl:attribute>
	</xsl:if>
	<td class="formlabeltop"><xsl:value-of disable-output-escaping="yes" select="$res_form_contents"/></td>
	<td>
	  <xsl:call-template name="contentTypes"/>
	  <xsl:if test="$selectedMode='addElement'"><br/>
	  <textarea name="contentmodel" rows="5" cols="50">
	    <xsl:choose>
	      <xsl:when test="$elementFullContents=''">
		<xsl:text>&lt;content xmlns:rng="http://relaxng.org/ns/structure/1.0"&gt;
&lt;/content&gt;</xsl:text>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:value-of select="$elementFullContents"/>
	      </xsl:otherwise>
	    </xsl:choose>
	  </textarea>
	 </xsl:if>
	</td>
      </tr>
      <tr>
	<td class="formlabeltop"><xsl:value-of disable-output-escaping="yes" select="$res_form_description"/></td>
	<td>
           <textarea rows="5" cols="40" name="description">
            <xsl:if test="not($elementDesc='')"><xsl:value-of select="$elementDesc"/></xsl:if>
           </textarea>
        </td>
      </tr>
      <tr>
       <td class="button" colspan="2"><input type="submit"/></td>
      </tr>
     </table>
    </form>
   </p>
   <xsl:call-template name="generateDivs"/>

  </xsl:template>


  <xsl:template name="generateDivs">
    <xsl:for-each select="/addElement/modelClassList/*">  
     <div class="descriptionPopup">
      <xsl:attribute name="id">descDiv_modelClass_<xsl:value-of select="className"/></xsl:attribute>
      <xsl:value-of select="classDesc"/>
     </div>
    </xsl:for-each>
    <xsl:for-each select="/addElement/attClassList/*">  
     <div class="descriptionPopup">
      <xsl:attribute name="id">descDiv_attClass_<xsl:value-of select="className"/></xsl:attribute>
      <xsl:value-of select="classDesc"/>
     </div>
    </xsl:for-each>

  </xsl:template>

<xsl:template name="modelClassList">
  <xsl:for-each select="/addElement/modelClassList">
  <table class="modelClasses">
    <xsl:call-template name="makeRow">
      <xsl:with-param name="M">1</xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="makeRow">
      <xsl:with-param name="M">2</xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="makeRow">
      <xsl:with-param name="M">3</xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="makeRow">
      <xsl:with-param name="M">4</xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="makeRow">
      <xsl:with-param name="M">5</xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="makeRow">
      <xsl:with-param name="M">6</xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="makeRow">
      <xsl:with-param name="M">7</xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="makeRow">
      <xsl:with-param name="M">0</xsl:with-param>
    </xsl:call-template>
  </table>
  </xsl:for-each>
</xsl:template>

<xsl:template name="makeRow">
  <xsl:param name="M"/>
  <tr>
    <xsl:for-each select="modelClass[position() mod 8 = $M]">
      <xsl:call-template name="makeCell"/>
    </xsl:for-each>
  </tr>
</xsl:template>


<xsl:template name="makeCell">
  <td>
    <input class="checkbox" type="checkbox">
      <xsl:attribute name="name">class|<xsl:value-of select="className"/></xsl:attribute>
      <xsl:attribute name="value"><xsl:value-of select="className"/></xsl:attribute>
      <xsl:variable name="currentClass"><xsl:value-of select="className"/></xsl:variable>
      <xsl:if test="contains( $elementClasses, $currentClass )">
	<xsl:attribute name="checked">1</xsl:attribute>
      </xsl:if>
    </input>
    <span>
      <xsl:attribute name="id">descSpan_modelClass_<xsl:value-of select="className"/></xsl:attribute>
      <xsl:attribute name="onMouseover">descriptionPopup_Show( 'modelClass_<xsl:value-of select="className"/>' )</xsl:attribute>
      <xsl:attribute name="onMouseout">descriptionPopup_Hide( 'modelClass_<xsl:value-of select="className"/>' )</xsl:attribute>
      <a>
	<xsl:attribute name="href"><xsl:value-of select="$host"/>/class.xq?name=<xsl:value-of select="className"/></xsl:attribute>
	<xsl:attribute name="target">_blank</xsl:attribute>
	<xsl:value-of select="className"/>
      </a>
    </span>
  </td>
</xsl:template>


<xsl:template name="attClassList">
  <xsl:for-each select="/addElement/attClassList">
    <table class="attClasses">
    <xsl:call-template name="makeAttRow">
      <xsl:with-param name="M">1</xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="makeAttRow">
      <xsl:with-param name="M">2</xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="makeAttRow">
      <xsl:with-param name="M">3</xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="makeAttRow">
      <xsl:with-param name="M">0</xsl:with-param>
    </xsl:call-template>
    </table>
  </xsl:for-each>
</xsl:template>

<xsl:template name="makeAttRow">
  <xsl:param name="M"/>
  <tr>
    <xsl:for-each select="attClass[position() mod 4 = $M]">
      <xsl:call-template name="makeCell"/>
    </xsl:for-each>
  </tr>
</xsl:template>

<xsl:template name="makeAttCell">
  <td>
    <input class="checkbox" type="checkbox">
      <xsl:attribute name="name">class|<xsl:value-of select="className"/></xsl:attribute>
      <xsl:attribute name="value"><xsl:value-of select="className"/></xsl:attribute>
      <xsl:variable name="currentClass"><xsl:value-of select="className"/></xsl:variable>
      <xsl:if test="contains( $elementClasses, $currentClass )">
	<xsl:attribute name="checked">1</xsl:attribute>
      </xsl:if>
    </input>
    <span>
      <xsl:attribute name="id">descSpan_attClass_<xsl:value-of select="className"/></xsl:attribute>
      <xsl:attribute name="onMouseover">descriptionPopup_Show( 'attClass_<xsl:value-of select="className"/>' )</xsl:attribute>
      <xsl:attribute name="onMouseout">descriptionPopup_Hide( 'attClass_<xsl:value-of select="className"/>' )</xsl:attribute>
      <a>
	<xsl:attribute name="href"><xsl:value-of select="$host"/>/class.xq?name=<xsl:value-of select="className"/></xsl:attribute>
	<xsl:attribute name="target">_blank</xsl:attribute>
	<xsl:value-of select="className"/>
      </a>
    </span>
  </td>
</xsl:template>


  <xsl:template name="contentTypes">
     <xsl:if test="$selectedMode='addElement'">
       <div class="HideItem">
	 <select name="content" size="1">
	   <option value="text">
	     <xsl:if test="$elementContents='text'">
	       <xsl:attribute name="selected">1</xsl:attribute>
	     </xsl:if>
	     Text
	   </option>
	   <option value="userContent">
	     <xsl:if test="$elementContents='userContent'">
	       <xsl:attribute name="selected">1</xsl:attribute>
	     </xsl:if>
	     User content
	   </option>
	   <option value="empty">
	     <xsl:if test="$elementContents='empty'">
	       <xsl:attribute name="selected">1</xsl:attribute>
	     </xsl:if>
	     Empty
	   </option>
	   <xsl:for-each select="/addElement/dataList/*">
	     <option>
	       <xsl:attribute name="value"><xsl:value-of select="dataName"/></xsl:attribute>
	       <xsl:variable name="currentDataName"><xsl:value-of select="dataName"/></xsl:variable>
	       <xsl:if test="$currentDataName=$elementContents">
		 <xsl:attribute name="selected">1</xsl:attribute>
	       </xsl:if>
	       <xsl:value-of select="dataName"/>
	     </option>
	   </xsl:for-each>
	   <xsl:for-each select="/addElement/macroList/*">
	     <option>
	       <xsl:attribute name="value"><xsl:value-of select="macroName"/></xsl:attribute>
	       <xsl:variable name="currentDataName"><xsl:value-of select="macroName"/></xsl:variable>
	       <xsl:if test="$currentDataName=$elementContents">
		 <xsl:attribute name="selected">1</xsl:attribute>
	       </xsl:if>
	       <xsl:value-of select="macroName"/>
	     </option>
	   </xsl:for-each>
	 </select>
       </div>
     </xsl:if>
     <xsl:if test="$selectedMode='changeElement'">
      <textarea rows="8" cols="80" name="content"><xsl:if
test="//errorList/error/location[node()='contents']"><xsl:value-of
select="//errorList/error[child::location[node()='contents']]/oldValue"/></xsl:if><xsl:if test="not(//errorList/error/location[node()='contents'])"><xsl:value-of select="$elementContent"/></xsl:if></textarea>
     </xsl:if>
  </xsl:template>

</xsl:stylesheet>
