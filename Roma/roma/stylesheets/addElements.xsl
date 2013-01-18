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
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:param name="elementName"/>
  <xsl:param name="elementDesc"/>
  <xsl:param name="elementClasses"/>
  <xsl:param name="elementContent"/>
  <xsl:param name="elementFullContents"/>
  <xsl:param name="elementsModule"/>
  <xsl:param name="MESSAGE"/>
  <xsl:param name="selectedMode">addElement</xsl:param>
  <xsl:param name="elementChangedName"/>
  <xsl:param name="elementNamespace"/>
  <xsl:param name="module"/>
  <xsl:param name="lang"/>
  <xsl:param name="doclang"/>
  <xsl:param name="TEIWEB">http://www.tei-c.org/release/doc/tei-p5-doc/</xsl:param>
  <xsl:param name="changeNameERROR"/>



  <xsl:variable name="empty">
    <xsl:text>&lt;content&gt;&lt;rng:empty xmlns:rng="http://relaxng.org/ns/structure/1.0"/&gt;&lt;/content&gt;</xsl:text>
  </xsl:variable>
  <xsl:variable name="text">
    <xsl:text>&lt;content&gt;&lt;rng:text xmlns:rng="http://relaxng.org/ns/structure/1.0"/&gt;&lt;/content&gt;</xsl:text>
  </xsl:variable>
  <xsl:variable name="before">
    <xsl:text>name="</xsl:text>
  </xsl:variable>
  <xsl:variable name="start">
    <xsl:text>&lt;content&gt;&lt;rng:ref xmlns:rng</xsl:text>
  </xsl:variable>
  <xsl:variable name="after">
    <xsl:text>"/&gt;&lt;/content&gt;</xsl:text>
  </xsl:variable>
    
  <xsl:template match="/">
    <p class="roma">
      <xsl:if test="$selectedMode='addElement'">
        <a href="?mode=listAddedElements">go back to list</a>
        <br/>
      </xsl:if>
      <xsl:if test="$selectedMode='changeElement'">
        <a><xsl:attribute name="href">?mode=changeModule&amp;module=<xsl:value-of select="$module"/></xsl:attribute>go back to list</a>
        <br/>
      </xsl:if>
      <form accept-charset="utf-8"  method="POST">
        <xsl:attribute name="action">
          <xsl:if test="$selectedMode='addElement'">?mode=elementAdded</xsl:if>
          <xsl:if test="$selectedMode='changeElement'">?mode=elementChanged</xsl:if>
        </xsl:attribute>
        <input type="hidden" name="module">
          <xsl:attribute name="value">
            <xsl:value-of select="$elementsModule"/>
          </xsl:attribute>
        </input>
        <xsl:if test="$selectedMode='addElement'">
          <input type="hidden" name="added" value="true"/>
        </xsl:if>
        <input type="hidden" id="changedClasses" name="changedClasses" value="false"/>
        <input type="hidden" id="changedDesc" name="changedDesc" value="false"/>
        <input type="hidden" id="changedNamespace" name="changedNamespace" value="false"/>
        <input type="hidden" id="changedContent" name="changedContent" value="false"/>
        <table>
          <tr>
            <td class="headline" colspan="2"><xsl:value-of select="$res_form_headline"/>: <xsl:value-of select="$elementName"/></td>
          </tr>
          <tr>
            <xsl:if test="//errorList/error/location[text()='name']">
              <xsl:attribute name="class">error</xsl:attribute>
            </xsl:if>
            <td class="formlabel">
              <xsl:value-of disable-output-escaping="yes" select="$res_form_name"/>
            </td>
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
                    <xsl:if test="//errorList/error/location[text()='name']">
                      <xsl:attribute name="value">
                        <xsl:value-of select="//errorList/error[child::location[text()='name']]/oldValue"/>
                      </xsl:attribute>
                    </xsl:if>
                  </input>
                </xsl:if>
              </xsl:if>
              <xsl:if test="$selectedMode='changeElement'">
                <input type="text" size="53" name="name">
                  <xsl:if test="not($elementName='')">
                    <xsl:attribute name="value">
                      <xsl:value-of select="$elementName"/>
                    </xsl:attribute>
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
              <td class="formlabel">Change Name</td>
              <td class="formfield">
                <input type="text" size="53" name="newName">
                  <xsl:if test="//errorList/error/location[node()='name']">
                    <xsl:attribute name="value">
                      <xsl:value-of select="//errorList/error[child::location[node()='name']]/oldValue"/>
                    </xsl:attribute>
                  </xsl:if>
                  <xsl:if test="not(//errorList/error/location[node()='name'])">
                    <xsl:attribute name="value">
                      <xsl:value-of select="$elementChangedName"/>
                    </xsl:attribute>
                  </xsl:if>
                </input>
              </td>
            </tr>
          </xsl:if>
          <tr>
            <xsl:if test="//errorList/error/location[node()='elementNamespace']">
              <xsl:attribute name="class">error</xsl:attribute>
            </xsl:if>
            <td class="formlabel">Namespace</td>
            <td class="formfield">
              <input type="text" size="53" name="elementNamespace" onChange="setChanged(this,'changedNamespace')">
                <xsl:if test="//errorList/error/location[node()='elementNamespace']">
                  <xsl:attribute name="value">
                    <xsl:value-of select="//errorList/error[child::location[node()='name']]/oldValue"/>
                  </xsl:attribute>
                </xsl:if>
                <xsl:if test="not(//errorList/error/location[node()='elementNamespace'])">
                  <xsl:attribute name="value">
                    <xsl:value-of select="$elementNamespace"/>
                  </xsl:attribute>
                </xsl:if>
              </input>
            </td>
          </tr>
          <tr>
            <td class="formlabeltop">
              <xsl:value-of disable-output-escaping="yes" select="$res_form_description"/>
            </td>
            <td>
              <span>
		<textarea rows="5" cols="70" name="description" onChange="setChanged(this,'changedDesc')">
                  <xsl:if test="not($elementDesc='')">
                    <xsl:value-of select="$elementDesc"/>
                  </xsl:if>
                </textarea>
              </span>
            </td>
          </tr>
          <tr>
            <td class="formlabeltop">
              <xsl:value-of disable-output-escaping="yes" select="$res_form_modelClasses"/>
            </td>
            <td>
              <xsl:call-template name="modelClassList"/>
            </td>
          </tr>
          <tr>
            <td class="formlabeltop">
              <xsl:value-of disable-output-escaping="yes" select="$res_form_attributeClasses"/>
            </td>
            <td>
              <xsl:call-template name="attClassList"/>
            </td>
          </tr>
          <tr>
            <xsl:if test="//errorList/error/location[node()='contents']">
              <xsl:attribute name="class">error</xsl:attribute>
            </xsl:if>
            <td class="formlabeltop">
              <xsl:value-of disable-output-escaping="yes" select="$res_form_contents"/>
            </td>
            <td>
              <xsl:call-template name="contentTypes"/>
              <xsl:if test="$selectedMode='addElement'">
                <br/>
                <span>
                  <textarea name="contentmodel" rows="5" cols="70" onChange="setChanged(this,'changedContent')">
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
                </span>
              </xsl:if>
            </td>
          </tr>
          <tr>
            <td class="button" colspan="2">
              <input type="submit" class="submit" value="Save"/>
            </td>
          </tr>
        </table>
        <input type="hidden" name="originalClasses">
          <xsl:attribute name="value">
            <xsl:for-each select="/addElement/attClassList/attClass">
              <xsl:if test="contains( $elementClasses, className )">
                <xsl:value-of select="className"/>
                <xsl:text> </xsl:text>
              </xsl:if>
            </xsl:for-each>
            <xsl:for-each select="/addElement/modelClassList/modelClass">
              <xsl:if test="contains( $elementClasses, className )">
                <xsl:value-of select="className"/>
                <xsl:text> </xsl:text>
              </xsl:if>
            </xsl:for-each>
          </xsl:attribute>
        </input>
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
    <div class="classes">
      <xsl:for-each select="/addElement/modelClassList/modelClass">
        <xsl:variable name="currentClass">
          <xsl:value-of select="className"/>
        </xsl:variable>
        <span class="class">
          <input class="checkbox" type="checkbox" name="class|{className}" value="{className}" onChange="setChangedClass(this)">
            <xsl:if test="contains( $elementClasses, $currentClass )">
              <xsl:attribute name="checked">1</xsl:attribute>
            </xsl:if>
          </input>
          <span onMouseover="descriptionPopup_Show( 'modelClass_{className}')" onMouseout="descriptionPopup_Hide( 'modelClass_{className}')" id="descSpan_modelClass_{className}">
            <a target="_new" href="{concat($TEIWEB, $doclang, '/html/ref-', className, '.html')}">
	             <xsl:value-of select="className"/>
            </a>
          </span>
        </span>
      </xsl:for-each>
    </div>
  </xsl:template>
  <xsl:template name="attClassList">
    <div class="classes">
      <xsl:for-each select="/addElement/attClassList/attClass">
        <xsl:variable name="currentClass">
          <xsl:value-of select="className"/>
        </xsl:variable>
        <span class="class">
          <input class="checkbox" type="checkbox" name="class|{className}" value="{className}" onChange="setChangedClass(this)">
            <xsl:if test="contains( $elementClasses, $currentClass )">
              <xsl:attribute name="checked">1</xsl:attribute>
            </xsl:if>
          </input>
          <span>
            <xsl:attribute name="id">
              <xsl:text>descSpan_attClass_</xsl:text>
              <xsl:value-of select="className"/>
            </xsl:attribute>
            <xsl:attribute name="onMouseover">
              <xsl:text>descriptionPopup_Show( 'attClass_</xsl:text>
              <xsl:value-of select="className"/>
              <xsl:text>' )</xsl:text>
            </xsl:attribute>
            <xsl:attribute name="onMouseout">
              <xsl:text>descriptionPopup_Hide( 'attClass_</xsl:text>
              <xsl:value-of select="className"/>
              <xsl:text>' )</xsl:text>
            </xsl:attribute>
            <a target="_new" href="{concat($TEIWEB, $doclang, '/html/ref-', className, '.html')}">
	             <xsl:value-of select="className"/>
            </a>
          </span>
        </span>
      </xsl:for-each>
    </div>
  </xsl:template>
  <xsl:template name="contentTypes">
    <xsl:if test="$selectedMode='addElement'">
      <div class="HideItem">
	<xsl:variable name="currentContent">
	  <xsl:if  test="starts-with($elementFullContents,$start)">
	    <xsl:value-of select="substring-before(substring-after($elementFullContents,$before),$after)"/>
	  </xsl:if>
	</xsl:variable>

        <select name="content" size="1">
	  <option value="userContent">
	    <xsl:if test="$elementContents='userContent'">
	      <xsl:attribute name="selected">1</xsl:attribute>
	    </xsl:if>
	    <xsl:text>User content </xsl:text>
	  </option>
	  <option value="text">
	    <xsl:if  test="$elementFullContents=$text">
	      <xsl:attribute name="selected">
		<xsl:text>1</xsl:text>
	      </xsl:attribute>
	    </xsl:if>
	    <xsl:text>Text </xsl:text>
	  </option>
	  <option value="empty">
	    <xsl:if  test="$elementFullContents=$empty">
	      <xsl:attribute name="selected">
		<xsl:text>1</xsl:text>
	      </xsl:attribute>
	    </xsl:if>
	    <xsl:text>Empty </xsl:text>
	  </option>
          <xsl:for-each select="/addElement/dataList/*">
            <option>
              <xsl:if test="dataName=$currentContent">
                <xsl:attribute name="selected">1</xsl:attribute>
              </xsl:if>
              <xsl:value-of select="dataName"/>
            </option>
          </xsl:for-each>
          <xsl:for-each select="/addElement/macroList/*">
            <option>
              <xsl:if test="macroName=$currentContent">
                <xsl:attribute name="selected">1</xsl:attribute>
              </xsl:if>
              <xsl:value-of select="macroName"/>
            </option>
          </xsl:for-each>
        </select>
      </div>
    </xsl:if>
    <xsl:if test="$selectedMode='changeElement'"> 
     <span>
        <textarea rows="8" cols="80" name="content" onChange="setChanged(this,'changedContent')">
          <xsl:choose>
            <xsl:when test="//errorList/error/location[node()='contents']">
              <xsl:value-of select="//errorList/error[child::location[node()='contents']]/oldValue"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="$elementContent"/>
            </xsl:otherwise>
          </xsl:choose>
        </textarea>
      </span>
    </xsl:if>
  </xsl:template>
</xsl:stylesheet>
