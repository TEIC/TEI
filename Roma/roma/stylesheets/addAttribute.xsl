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
  <xsl:param name="element"/>
  <xsl:param name="module"/>
  <xsl:param name="class"/>
  <xsl:param name="type"/>
  <xsl:param name="added"/>
  <xsl:param name="attributeName"/>
  <xsl:param name="MESSAGE"/>
  <xsl:param name="ERRORS"/>
  
  <xsl:template match="/">
    <p class="roma">
      <a>
	<xsl:attribute
	 name="href">?mode=listAddedAttributes&amp;element=<xsl:value-of
	 select="$element"/>&amp;module=<xsl:value-of
	 select="$module"/>&amp;class=<xsl:value-of select="$class"/></xsl:attribute>go back to list</a><br/>
	 
	 <form method="POST" action="?mode=attributeAdded">
	   <input type="hidden" name="element">
	     <xsl:attribute name="value"><xsl:value-of
	     select="$element"/></xsl:attribute>
	   </input>
	   <input type="hidden" name="module">
	     <xsl:attribute name="value"><xsl:value-of
	     select="$module"/></xsl:attribute>
	   </input>
	   <input type="hidden" name="class">
	     <xsl:attribute name="value"><xsl:value-of
	     select="$class"/></xsl:attribute>
	   </input>
	   <input type="hidden" name="type">
	     <xsl:attribute name="value"><xsl:value-of
	     select="$type"/></xsl:attribute>
	   </input>
	   <input type="hidden" name="added">
	     <xsl:attribute name="value"><xsl:if test="not(string(//currentAttribute/attDef/added)='')"><xsl:value-of
	     select="//currentAttribute/attDef/added"/></xsl:if><xsl:if
	     test="string(//currentAttribute/attDef/added)='' and not($added='')"><xsl:value-of
	     select="$added"/></xsl:if></xsl:attribute>
	   </input>
	   <table>
	     <tr><td class="headline" colspan="4"><xsl:value-of select="$res_form_headline"/></td></tr>
	     <xsl:if test="$type='change'">
	       <tr>
		 <td class="formlabel">
		   <input type="hidden" name="name">
		     <xsl:attribute name="value"><xsl:value-of
		     select="//currentAttribute/attDef/attName"/></xsl:attribute>
		 </input><xsl:value-of select="$res_form_name"/></td>
		 <td class="formfield"><xsl:value-of select="//currentAttribute/attDef/attName"/></td>
	       </tr>
	     </xsl:if>
	     <xsl:if test="not($type='change')">
	       <tr>
		 <xsl:if
		  test="//errorList/error/location[text()='name']">
		   <xsl:attribute name="class">error</xsl:attribute>
		 </xsl:if>
		 <td class="formlabel"><xsl:value-of select="$res_form_headline"/></td>
		 <td class="formfield">
		   <input type="text" size="53" name="name">
		     <xsl:if test="//errorList/error/location[text()='name']">
		       <xsl:attribute name="value"><xsl:value-of select="//errorList/error[child::location[text()='name']]/oldValue"/></xsl:attribute>
		     </xsl:if>
		   </input>
		 </td>
	       </tr>
	     </xsl:if>
	     <tr>
	       <td class="formlabel"><xsl:value-of select="$res_form_optional"/></td>
	       <td class="formfield">
		 <input class="radio" type="radio" name="optional" value="true">
		   <xsl:if
		    test="string(//currentAttribute/attDef/optional)='opt'">
		     <xsl:attribute name="checked">1</xsl:attribute>
		   </xsl:if>
		   <xsl:if
		    test="not(//currentAttribute)">
		     <xsl:attribute name="checked">1</xsl:attribute>
		   </xsl:if>
		 </input>
		 yes
		 <br/>
		 <input class="radio" type="radio" name="optional" value="no">
		   <xsl:if
		    test="string(//currentAttribute/attDef/optional)='req'">
		     <xsl:attribute name="checked">1</xsl:attribute>
		   </xsl:if>
		 </input>
		 no
	       </td>
	     </tr>
	     <tr>
	       <td class="formlabel"><xsl:value-of select="$res_form_contents"/></td>
	       <td class="formfield"><xsl:call-template name="contentTypes"/></td>
	     </tr>
	     <tr>
	       <td class="formlabel"><xsl:value-of select="$res_form_defaultValue"/></td>
	       <td class="formfield">
		 <input type="text" name="defaultValue" size="53">
		   <xsl:if test="//currentAttribute">
		     <xsl:attribute name="value">
		       <xsl:value-of select="//currentAttribute/attDef/default"/>
		     </xsl:attribute>
		   </xsl:if>
		 </input>
	       </td>
	     </tr>
	     <tr>
	       <td class="formlabel"><xsl:value-of select="$res_form_valList"/></td>
	       <td class="formfield">
		 <input type="text" name="valList" size="53">
		   <xsl:if test="//currentAttribute">
		     <xsl:attribute name="value">
		       <xsl:value-of select="//currentAttribute/attDef/valList"/>
		     </xsl:attribute>
		   </xsl:if>
		 </input>
	       </td>
	     </tr>
	     <tr>
	       <td class="formlabeltop"><xsl:value-of select="$res_form_description"/></td>
	       <td class="formfield">
		 <textarea name="description" rows="5"
		 cols="45"><xsl:value-of select="//currentAttribute/attDef/desc"/></textarea>
	       </td>
	     </tr>
	     <tr>
	       <td class="button" colspan="2"><input type="submit"/></td>
	     </tr>
	   </table>
	 </form>
    </p>
  </xsl:template>
  
  <xsl:template name="contentTypes">
    <div class="HideItem">
      <select name="content" size="1">
	<option value="text">
	  <xsl:if
	   test="string(//currentAttribute/attDef/datatype)='text'">
	    <xsl:attribute name="selected">1</xsl:attribute>
	  </xsl:if>
	  Text
	</option>
	<xsl:for-each select="/addAttribute/dataList/*">
	  <option>
	    <xsl:attribute name="value"><xsl:value-of
	    select="dataName"/></xsl:attribute>
	    <xsl:if
	     test="string(//currentAttribute/attDef/datatype)=string(./dataName)">
	      <xsl:attribute name="selected">1</xsl:attribute>
	    </xsl:if>
	    <xsl:value-of select="dataName"/>
	  </option>
	</xsl:for-each>
	<xsl:for-each select="/addAttribute/w3cdataList/*">
	  <option>
	    <xsl:attribute name="value"><xsl:value-of
	    select="."/></xsl:attribute>
	    <xsl:if
	     test="string(//currentAttribute/attDef/datatype)=string(.)">
	      <xsl:attribute name="selected">1</xsl:attribute>
	    </xsl:if>
	    <xsl:value-of select="."/>
	  </option>
	</xsl:for-each>
      </select>
    </div>
  </xsl:template>
  
</xsl:stylesheet>