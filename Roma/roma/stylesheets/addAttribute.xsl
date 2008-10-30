<?xml version="1.0" encoding="utf-8"?>
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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:param name="element"/>
  <xsl:param name="module"/>
  <xsl:param name="class"/>
  <xsl:param name="type"/>
  <xsl:param name="closed"/>
  <xsl:param name="minOccurs"/>
  <xsl:param name="maxOccurs"/>
  <xsl:param name="added">true</xsl:param>
  <xsl:param name="attributeName"/>
  <xsl:param name="MESSAGE"/>
  <xsl:param name="ERRORS"/>
  <xsl:template match="/">
    <p>
      <xsl:value-of select="name(.)"/>
    </p>
<!--
    <pre>
      <xsl:for-each select=".//currentAttribute">
      <xsl:for-each select=".//*">
	<xsl:for-each select="ancestor::*">
	  <xsl:text> </xsl:text>
	</xsl:for-each>
	<xsl:text>&lt;</xsl:text>
	<xsl:value-of select="local-name()"/>
	<xsl:text>&gt;</xsl:text>
	  <xsl:text>
</xsl:text>
	<xsl:for-each select="@*">
	<xsl:for-each select="ancestor::*">
	  <xsl:text> </xsl:text>
	</xsl:for-each>
	  <xsl:text>  </xsl:text>
	  <xsl:value-of select="local-name()"/>="<xsl:value-of select="."/>"
	</xsl:for-each>
	<xsl:text>[</xsl:text>
	<xsl:value-of select="text()"/>
	<xsl:text>]
 </xsl:text>
      </xsl:for-each>
      </xsl:for-each>
    </pre>
-->

    <p class="roma">
      <a>
        <xsl:attribute name="href">
          <xsl:text>?mode=listAddedAttributes&amp;element=</xsl:text>
          <xsl:value-of select="$element"/>
          <xsl:text>&amp;module=</xsl:text>
          <xsl:value-of select="$module"/>
          <xsl:text>&amp;class=</xsl:text>
          <xsl:value-of select="$class"/>
        </xsl:attribute>
        <xsl:text>go back to list</xsl:text>
      </a>
      <br/>
      <form method="POST" action="?mode=attributeAdded">
        <input type="hidden" name="element" value="{$element}"/>
        <input type="hidden" name="module" value="{$module}"/>
        <input type="hidden" name="class" value="{$class}"/>
        <input type="hidden" name="type" value="{$type}"/>
        <input type="hidden" name="added">
          <xsl:attribute name="value">
            <xsl:choose>
              <xsl:when test="string-length(//currentAttribute/attDef/added)&gt;0">
                <xsl:value-of select="//currentAttribute/attDef/added"/>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="$added"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:attribute>
        </input>
        <input type="hidden" id="changedName" name="changedName"  value="false"/>
        <input type="hidden" id="changedDesc" name="changedDesc"  value="false"/>
        <input type="hidden" id="changedUsage" name="changedUsage" value="false"/>
        <input type="hidden" id="changedContent" name="changedContent" value="false"/>
<!--
<ul>
<li>element is <xsl:value-of select="$element"/></li>
<li>module is <xsl:value-of select="$module"/></li>
<li>class is <xsl:value-of select="$class"/></li>
<li>type is <xsl:value-of select="$type"/></li>
<li>closed is <xsl:value-of select="$closed"/></li>
<li>minOccurs is <xsl:value-of select="$minOccurs"/></li>
<li>maxOccurs is <xsl:value-of select="$maxOccurs"/></li>
<li>added is <xsl:value-of select="$added"/></li>
</ul>
-->
        <table>
          <tr>
            <td class="headline" colspan="4">
              <xsl:value-of disable-output-escaping="yes" select="$res_form_headline"/>
            </td>
          </tr>
          <tr>
            <xsl:choose>
	      <xsl:when test="$type='changed'">
                <td class="formlabel">
                  <xsl:value-of disable-output-escaping="yes" select="$res_form_name"/>
                </td>
                <td class="formfield">
                  <input type="text" size="53"
			 name="altname">
		    <xsl:attribute name="value">
		      <xsl:value-of
			  select="//currentAttribute/attDef/attName"/>
		    </xsl:attribute>
		  </input>
                </td>
              </xsl:when>
              <xsl:otherwise>
                <xsl:if test="//errorList/error/location[text()='name']">
                  <xsl:attribute name="class">error</xsl:attribute>
                </xsl:if>
                <td class="formlabel">
                  <xsl:value-of disable-output-escaping="yes" select="$res_form_name"/>
                </td>
                <td class="formfield">
		  <xsl:value-of
		      select="//currentAttribute/attDef/attName"/>:
                  <input type="hidden" name="name" value="{//currentAttribute/attDef/attName}"/>
                  <input type="text" size="53" name="altname"
			 onChange="setChanged(this,'changedName')">
		    <xsl:attribute name="value">
			<xsl:choose>
			  <xsl:when test="//errorList/error/location[text()='name']">
			    <xsl:value-of
				select="//errorList/error[child::location[text()='name']]/oldValue"/>
			  </xsl:when>
			  <xsl:when
			      test="//currentAttribute/attDef/altName=''">
			    <xsl:value-of select="//currentAttribute/attDef/attName"/>
			  </xsl:when>
			  <xsl:otherwise>
			    <xsl:value-of select="//currentAttribute/attDef/altName"/>
			  </xsl:otherwise>
			</xsl:choose>
		      </xsl:attribute>
                  </input>
                </td>
              </xsl:otherwise>
            </xsl:choose>
          </tr>
          <tr>
            <td class="formlabel">
              <xsl:value-of disable-output-escaping="yes" select="$res_form_className"/>
            </td>
            <td>
              <xsl:value-of select="$class"/>
            </td>
          </tr>
          <tr>
            <td class="formlabel">
              <xsl:value-of disable-output-escaping="yes" select="$res_form_optional"/>
            </td>
            <td class="formfield">
	      <input class="radio" type="radio" name="optional"
		     value="true" onChange="setChanged(this,'changedUsage')">
		<xsl:choose>
		  <xsl:when   test="not(//currentAttribute/attDef/optional='req')">
		    <xsl:attribute name="checked">
		      <xsl:text>1</xsl:text>
		    </xsl:attribute>
		  </xsl:when>
		  <xsl:when  test="not(//currentAttribute)">
		    <xsl:attribute name="checked">
		      <xsl:text>1</xsl:text>
		    </xsl:attribute>
		  </xsl:when>
	      </xsl:choose>
	      </input> 
	      yes <br/>
	      <input class="radio" type="radio" name="optional"
		     value="false" onChange="setChanged(this,'changedUsage')">
		<xsl:if test="//currentAttribute/attDef/optional='req'">
		  <xsl:attribute name="checked">1</xsl:attribute>
		</xsl:if>
	    </input> no </td>
	  </tr>
          <tr>
            <td class="formlabel">
              <xsl:value-of disable-output-escaping="yes" select="$res_form_contents"/>
            </td>
            <td class="formfield">
              <xsl:call-template name="contentTypes"/>
            </td>
          </tr>
          <tr>
            <td class="formlabel">
              <xsl:value-of disable-output-escaping="yes" select="$res_form_defaultValue"/>
            </td>
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
            <td class="formlabel">
              <xsl:value-of disable-output-escaping="yes" select="$res_form_closed"/>
            </td>
            <td class="formfield"><input class="radio" type="radio" name="closed" value="true"><xsl:if test="string-length(//currentAttribute/attDef/valList)&gt;0"><xsl:attribute name="checked">1</xsl:attribute></xsl:if></input> yes <br/>
							<input class="radio" type="radio" name="closed" value="false"><xsl:if test="string-length(//currentAttribute/attDef/valList)=0"><xsl:attribute name="checked">1</xsl:attribute></xsl:if></input> no </td>
          </tr>
          <tr>
            <td class="formlabel">
              <xsl:value-of disable-output-escaping="yes" select="$res_form_valList"/>
            </td>
            <td class="formfield">
              <input type="text" name="valList" size="53">
                <xsl:attribute name="value">
                  <xsl:value-of select="//currentAttribute/attDef/valList"/>
                </xsl:attribute>
              </input>
            </td>
          </tr>
          <tr>
            <td class="formlabeltop">
              <xsl:value-of disable-output-escaping="yes" select="$res_form_description"/>
            </td>
            <td class="formfield">
              <textarea name="description" rows="5" cols="70" onChange="setChanged(this,'changedDesc')">
                <xsl:value-of select="//currentAttribute/attDef/desc"/>
              </textarea>
            </td>
          </tr>
          <tr>
            <td class="button" colspan="2">
              <input type="submit" value="Save"/>
            </td>
          </tr>
        </table>
      </form>
    </p>
  </xsl:template>
  <xsl:template name="contentTypes">
    <div class="HideItem">
      <select name="content" size="1" onChange="setChanged(this,'changedContent')">
        <option value="text"><xsl:if test="string(//currentAttribute/attDef/datatype)='text'"><xsl:attribute name="selected">1</xsl:attribute></xsl:if> Text </option>
        <xsl:for-each select="/addAttribute/dataList/*">
          <option>
            <xsl:attribute name="value">
              <xsl:value-of select="dataName"/>
            </xsl:attribute>
            <xsl:if test="string(//currentAttribute/attDef/datatype)=string(./dataName)">
              <xsl:attribute name="selected">1</xsl:attribute>
            </xsl:if>
            <xsl:value-of select="dataName"/>
          </option>
        </xsl:for-each>
      </select>
      <xsl:variable name="currentMin">
        <xsl:choose>
          <xsl:when test="string-length(//currentAttribute/attDef/datatype/@minOccurs)&gt;0">
            <xsl:value-of select="//currentAttribute/attDef/datatype/@minOccurs"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text>1</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:variable name="currentMax">
        <xsl:choose>
          <xsl:when test="string-length(//currentAttribute/attDef/datatype/@maxOccurs)&gt;0">
            <xsl:value-of select="//currentAttribute/attDef/datatype/@maxOccurs"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text>1</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:variable>
      <xsl:text> &gt;=</xsl:text>
      <select name="minOccurs" size="1" onChange="setChanged(this,'changedContent')">
        <option value="0">
          <xsl:if test="$currentMin=0">
            <xsl:attribute name="selected">1</xsl:attribute>
          </xsl:if>
          <xsl:text>0</xsl:text>
        </option>
        <option value="1">
          <xsl:if test="$currentMin=1">
            <xsl:attribute name="selected">1</xsl:attribute>
          </xsl:if>
          <xsl:text>1</xsl:text>
        </option>
        <option value="2">
          <xsl:if test="$currentMin=2">
            <xsl:attribute name="selected">1</xsl:attribute>
          </xsl:if>
          <xsl:text>2</xsl:text>
        </option>
      </select>
      <xsl:text> &lt;=</xsl:text>
      <select name="maxOccurs" size="1" onChange="setChanged(this,'changedContent')">
        <option value="0">
          <xsl:if test="$currentMax=0">
            <xsl:attribute name="selected">1</xsl:attribute>
          </xsl:if>
          <xsl:text>0</xsl:text>
        </option>
        <option value="1">
          <xsl:if test="$currentMax=1">
            <xsl:attribute name="selected">1</xsl:attribute>
          </xsl:if>
          <xsl:text>1</xsl:text>
        </option>
        <option value="2">
          <xsl:if test="$currentMax=2">
            <xsl:attribute name="selected">1</xsl:attribute>
          </xsl:if>
          <xsl:text>2</xsl:text>
        </option>
        <option value="3">
          <xsl:if test="$currentMax=3">
            <xsl:attribute name="selected">1</xsl:attribute>
          </xsl:if>
          <xsl:text>3</xsl:text>
        </option>
        <option value="4">
          <xsl:if test="$currentMax=4">
            <xsl:attribute name="selected">1</xsl:attribute>
          </xsl:if>
          <xsl:text>4</xsl:text>
        </option>
        <option value="5">
          <xsl:if test="$currentMax=5">
            <xsl:attribute name="selected">1</xsl:attribute>
          </xsl:if>
          <xsl:text>5</xsl:text>
        </option>
        <option value="unbounded">
          <xsl:if test="$currentMax='unbounded'">
            <xsl:attribute name="selected">1</xsl:attribute>
          </xsl:if>
          <xsl:text>unbounded</xsl:text>
        </option>
      </select>
    </div>
  </xsl:template>
</xsl:stylesheet>
