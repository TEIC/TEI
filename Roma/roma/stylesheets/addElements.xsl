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
<xsl:param name="elementsModule"/>
<xsl:param name="MESSAGE"/>
<xsl:param name="host"/>
<xsl:param name="selectedMode">addElement</xsl:param>
<xsl:param name="headline">Add new elements</xsl:param>
<xsl:param name="elementChangedName"/>
<xsl:param name="contentERROR"/>
<xsl:param name="module"/>
<xsl:param name="ERRORS"/>
<xsl:param name="changeNameERROR"/>


  <xsl:template match="/">
   <p class="roma">
    <xsl:call-template name="topLinks"/>
    <h1><xsl:value-of select="$headline"/></h1>
    <xsl:if test="not($MESSAGE='')">
     <p class="success">
      <xsl:value-of select="$MESSAGE"/>
     </p>
    </xsl:if>
    <xsl:if test="not($ERRORS='')">
     <xsl:if test="not($contentERROR='')"><p
      class="error"><xsl:value-of select="$contentERROR"/></p></xsl:if>
     <xsl:if test="not($changeNameERROR='')"><p
      class="error"><xsl:value-of
      select="$changeNameERROR"/></p></xsl:if>
    </xsl:if>
    
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
       <td class="headline" colspan="2">Defining a new element</td>
      </tr>
      <tr>
	<xsl:if test="//errorList/error/location[text()='name']">
	  <xsl:attribute name="class">error</xsl:attribute>
	</xsl:if>
       <td class="formlabel">Name</td>
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
        <td class="formlabeltop">Model Classes</td>
        <td>
         <table class="noBorder">
          <tr>
           <xsl:call-template name="modelClassList"/>
          </tr>
         </table>
        </td>
      </tr>
      <tr>
        <td class="formlabeltop">Attribute Classes</td>
        <td>
         <table class="noBorder">
          <tr>
           <xsl:call-template name="attClassList"/>
          </tr>
         </table>
         </td>
      </tr>
      <tr>
	<xsl:if test="//errorList/error/location[node()='contents']">
	  <xsl:attribute name="class">error</xsl:attribute>
	</xsl:if>
        <td class="formlabeltop">Contents</td>
        <td>
         <xsl:call-template name="contentTypes"/>
        </td>
      </tr>
      <tr>
        <td class="formlabeltop">Description</td>
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
<!-- generate divs -->
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


  



  <xsl:template name="topLinks">
   <table class="topLinks">
    <tr>
     <td>
       <xsl:if test="$selectedMode='changeElement'">
        <xsl:attribute name="class">selected</xsl:attribute>
       </xsl:if>
       <a href="?mode=main">Change Modules</a>
     </td>
     <td>
       <xsl:if test="$selectedMode='addElement'">
        <xsl:attribute name="class">selected</xsl:attribute>
       </xsl:if>
      <a href="?mode=listAddedElements">Add Elements</a>
     </td>
     <td><a href="?mode=changeClasses">Change Classes</a></td>
     <td><a href="?mode=customizeLanguage">Customize language</a></td>
     <td><a href="?mode=createSchema">Create Schema</a></td>
     <td><a href="?mode=createDocumentation">Create Documentation</a></td>
     <td><a href="?mode=saveCustomization">Save Customization</a></td>
     <td class="newCustomization"><a href="?mode=newCustomization">Create new Customization</a></td>
    </tr>
   </table>
  </xsl:template>

  <xsl:template name="modelClassList">
          <xsl:for-each select="/addElement/modelClassList/*">
          <xsl:if test="((position() mod 9)=0) or position()=1">
            <td>
             <table class="modelClasses">
              <tr>
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
                 <xsl:attribute name="href">http://<xsl:value-of select="$host"/>:8080/exist/tei/class.xq?name=<xsl:value-of select="className"/></xsl:attribute>
	         <xsl:attribute name="target">_blank</xsl:attribute>
                 <xsl:value-of select="className"/>
                </a>
              </span>
             </td></tr><tr><td>
	     <xsl:if
	      test="not(string(following-sibling::modelClass[1]/className)='')">
	       <input class="checkbox" type="checkbox">
              <xsl:attribute name="name">class|<xsl:value-of select="following-sibling::modelClass[1]/className"/></xsl:attribute>
              <xsl:attribute name="value"><xsl:value-of select="following-sibling::modelClass[1]/className"/></xsl:attribute>
              <xsl:variable name="currentClass"><xsl:value-of select="following-sibling::modelClass[1]/className"/></xsl:variable>
              <xsl:if test="contains( $elementClasses, $currentClass )">
                <xsl:attribute name="checked">1</xsl:attribute>
              </xsl:if>
             </input>
              <span>
               <xsl:attribute name="id">descSpan_modelClass_<xsl:value-of select="following-sibling::modelClass[1]/className"/></xsl:attribute>
	       <xsl:attribute name="onMouseover">descriptionPopup_Show( 'modelClass_<xsl:value-of select="following-sibling::modelClass[1]/className"/>' )</xsl:attribute>
	       <xsl:attribute name="onMouseout">descriptionPopup_Hide( 'modelClass_<xsl:value-of select="following-sibling::modelClass[1]/className"/>' )</xsl:attribute>
                <a>
                 <xsl:attribute name="href">http://<xsl:value-of select="$host"/>:8080/exist/tei/class.xq?name=<xsl:value-of select="following-sibling::modelClass[1]/className"/></xsl:attribute>
	         <xsl:attribute name="target">_blank</xsl:attribute>
                 <xsl:value-of select="following-sibling::modelClass[1]/className"/>
                </a>
              </span></xsl:if>
	      </td></tr><tr><td>
	      <xsl:if
	       test="not(string(following-sibling::modelClass[2]/className)='')">
		<input class="checkbox" type="checkbox">
              <xsl:attribute name="name">class|<xsl:value-of select="following-sibling::modelClass[2]/className"/></xsl:attribute>
              <xsl:attribute name="value"><xsl:value-of select="following-sibling::modelClass[2]/className"/></xsl:attribute>
              <xsl:variable name="currentClass"><xsl:value-of select="following-sibling::modelClass[2]/className"/></xsl:variable>
              <xsl:if test="contains( $elementClasses, $currentClass )">
                <xsl:attribute name="checked">1</xsl:attribute>
              </xsl:if>
             </input>
              <span>
               <xsl:attribute name="id">descSpan_modelClass_<xsl:value-of select="following-sibling::modelClass[2]/className"/></xsl:attribute>
	       <xsl:attribute name="onMouseover">descriptionPopup_Show( 'modelClass_<xsl:value-of select="following-sibling::modelClass[2]/className"/>' )</xsl:attribute>
	       <xsl:attribute name="onMouseout">descriptionPopup_Hide( 'modelClass_<xsl:value-of select="following-sibling::modelClass[2]/className"/>' )</xsl:attribute>
                <a>
                 <xsl:attribute name="href">http://<xsl:value-of select="$host"/>:8080/exist/tei/class.xq?name=<xsl:value-of select="following-sibling::modelClass[2]/className"/></xsl:attribute>
	         <xsl:attribute name="target">_blank</xsl:attribute>
                 <xsl:value-of select="following-sibling::modelClass[2]/className"/>
                </a>
              </span></xsl:if></td></tr><tr><td>
	      <xsl:if
	       test="not(string(following-sibling::modelClass[3]/className)='')">
             <input class="checkbox" type="checkbox">
              <xsl:attribute name="name">class|<xsl:value-of select="following-sibling::modelClass[3]/className"/></xsl:attribute>
              <xsl:attribute name="value"><xsl:value-of select="following-sibling::modelClass[3]/className"/></xsl:attribute>
              <xsl:variable name="currentClass"><xsl:value-of select="following-sibling::modelClass[3]/className"/></xsl:variable>
              <xsl:if test="contains( $elementClasses, $currentClass )">
                <xsl:attribute name="checked">1</xsl:attribute>
              </xsl:if>
             </input>
              <span>
               <xsl:attribute name="id">descSpan_modelClass_<xsl:value-of select="following-sibling::modelClass[3]/className"/></xsl:attribute>
	       <xsl:attribute name="onMouseover">descriptionPopup_Show( 'modelClass_<xsl:value-of select="following-sibling::modelClass[3]/className"/>' )</xsl:attribute>
	       <xsl:attribute name="onMouseout">descriptionPopup_Hide( 'modelClass_<xsl:value-of select="following-sibling::modelClass[3]/className"/>' )</xsl:attribute>
                <a>
                 <xsl:attribute name="href">http://<xsl:value-of select="$host"/>:8080/exist/tei/class.xq?name=<xsl:value-of select="following-sibling::modelClass[3]/className"/></xsl:attribute>
	         <xsl:attribute name="target">_blank</xsl:attribute>
                 <xsl:value-of select="following-sibling::modelClass[3]/className"/>
                </a>
              </span></xsl:if></td></tr><tr><td>
	      <xsl:if
	       test="not(string(following-sibling::modelClass[4]/className)='')">
             <input class="checkbox" type="checkbox">
              <xsl:attribute name="name">class|<xsl:value-of select="following-sibling::modelClass[4]/className"/></xsl:attribute>
              <xsl:attribute name="value"><xsl:value-of select="following-sibling::modelClass[4]/className"/></xsl:attribute>
              <xsl:variable name="currentClass"><xsl:value-of select="following-sibling::modelClass[4]/className"/></xsl:variable>
              <xsl:if test="contains( $elementClasses, $currentClass )">
                <xsl:attribute name="checked">1</xsl:attribute>
              </xsl:if>
             </input>
              <span>
               <xsl:attribute name="id">descSpan_modelClass_<xsl:value-of select="following-sibling::modelClass[4]/className"/></xsl:attribute>
	       <xsl:attribute name="onMouseover">descriptionPopup_Show( 'modelClass_<xsl:value-of select="following-sibling::modelClass[4]/className"/>' )</xsl:attribute>
	       <xsl:attribute name="onMouseout">descriptionPopup_Hide( 'modelClass_<xsl:value-of select="following-sibling::modelClass[4]/className"/>' )</xsl:attribute>
                <a>
                 <xsl:attribute name="href">http://<xsl:value-of select="$host"/>:8080/exist/tei/class.xq?name=<xsl:value-of select="following-sibling::modelClass[4]/className"/></xsl:attribute>
                 <xsl:value-of select="following-sibling::modelClass[4]/className"/>
                </a>
              </span>
	      </xsl:if>
	      </td></tr>
             <tr><td>
	     <xsl:if
	      test="not(string(following-sibling::modelClass[6]/className)='')">
	       <input class="checkbox" type="checkbox">
              <xsl:attribute name="name">class|<xsl:value-of select="following-sibling::modelClass[5]/className"/></xsl:attribute>
              <xsl:attribute name="value"><xsl:value-of select="following-sibling::modelClass[5]/className"/></xsl:attribute>
              <xsl:variable name="currentClass"><xsl:value-of select="following-sibling::modelClass[5]/className"/></xsl:variable>
              <xsl:if test="contains( $elementClasses, $currentClass )">
                <xsl:attribute name="checked">1</xsl:attribute>
              </xsl:if>
             </input>
              <span>
               <xsl:attribute name="id">descSpan_modelClass_<xsl:value-of select="following-sibling::modelClass[5]/className"/></xsl:attribute>
	       <xsl:attribute name="onMouseover">descriptionPopup_Show( 'modelClass_<xsl:value-of select="following-sibling::modelClass[5]/className"/>' )</xsl:attribute>
	       <xsl:attribute name="onMouseout">descriptionPopup_Hide( 'modelClass_<xsl:value-of select="following-sibling::modelClass[5]/className"/>' )</xsl:attribute>
                <a>
                 <xsl:attribute name="href">http://<xsl:value-of select="$host"/>:8080/exist/tei/class.xq?name=<xsl:value-of select="following-sibling::modelClass[5]/className"/></xsl:attribute>
	         <xsl:attribute name="target">_blank</xsl:attribute>
                 <xsl:value-of select="following-sibling::modelClass[5]/className"/>
                </a>
              </span>
	     </xsl:if>
               </td></tr>
             <tr><td>
	     <xsl:if
	      test="not(string(following-sibling::modelClass[6]/className)='')">
	       <input class="checkbox" type="checkbox">
		 <xsl:attribute name="name">class|<xsl:value-of select="following-sibling::modelClass[6]/className"/></xsl:attribute>
		 <xsl:attribute name="value"><xsl:value-of select="following-sibling::modelClass[6]/className"/></xsl:attribute>
		 <xsl:variable name="currentClass"><xsl:value-of select="following-sibling::modelClass[6]/className"/></xsl:variable>
		 <xsl:if test="contains( $elementClasses, $currentClass )">
		   <xsl:attribute name="checked">1</xsl:attribute>
		 </xsl:if>
	       </input>
	       <span>	
		 <xsl:attribute name="id">descSpan_modelClass_<xsl:value-of select="following-sibling::modelClass[6]/className"/></xsl:attribute>
		 <xsl:attribute name="onMouseover">descriptionPopup_Show( 'modelClass_<xsl:value-of select="following-sibling::modelClass[6]/className"/>' )</xsl:attribute>
		 <xsl:attribute name="onMouseout">descriptionPopup_Hide( 'modelClass_<xsl:value-of select="following-sibling::modelClass[6]/className"/>' )</xsl:attribute>
		 <a>
		   <xsl:attribute name="href">http://<xsl:value-of select="$host"/>:8080/exist/tei/class.xq?name=<xsl:value-of select="following-sibling::modelClass[6]/className"/></xsl:attribute>
		   <xsl:attribute name="target">_blank</xsl:attribute>
		   <xsl:value-of select="following-sibling::modelClass[6]/className"/>
		 </a>
	       </span>
	     </xsl:if>
               </td></tr>
             <tr><td>
	     <xsl:if
	      test="not(string(following-sibling::modelClass[7]/className)='')">
	       <input class="checkbox" type="checkbox">
		 <xsl:attribute name="name">class|<xsl:value-of select="following-sibling::modelClass[7]/className"/></xsl:attribute>
		 <xsl:attribute name="value"><xsl:value-of select="following-sibling::modelClass[7]/className"/></xsl:attribute>
		 <xsl:variable name="currentClass"><xsl:value-of select="following-sibling::modelClass[7]/className"/></xsl:variable>
		 <xsl:if test="contains( $elementClasses, $currentClass )">
		   <xsl:attribute name="checked">1</xsl:attribute>
		 </xsl:if>
	       </input>
	       <span>
		 <xsl:attribute name="id">descSpan_modelClass_<xsl:value-of select="following-sibling::modelClass[7]/className"/></xsl:attribute>
		 <xsl:attribute name="onMouseover">descriptionPopup_Show( 'modelClass_<xsl:value-of select="following-sibling::modelClass[7]/className"/>' )</xsl:attribute>
		 <xsl:attribute name="onMouseout">descriptionPopup_Hide( 'modelClass_<xsl:value-of select="following-sibling::modelClass[7]/className"/>' )</xsl:attribute>
		 <a>
		   <xsl:attribute name="href">http://<xsl:value-of select="$host"/>:8080/exist/tei/class.xq?name=<xsl:value-of select="following-sibling::modelClass[7]/className"/></xsl:attribute>
		   <xsl:attribute name="target">_blank</xsl:attribute>
		   <xsl:value-of select="following-sibling::modelClass[7]/className"/>
		 </a>
	       </span>
	     </xsl:if>
	       </td></tr>
             </table>
            </td>
           </xsl:if>
          </xsl:for-each>
  </xsl:template>


  <xsl:template name="attClassList">
       <xsl:for-each select="/addElement/attClassList/*">
        <xsl:if test="((position() mod 5)=0) or position()=1">
             <td>
             <table class="attClasses">
              <tr>
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
                 <xsl:attribute name="href">http://<xsl:value-of select="$host"/>:8080/exist/tei/class.xq?name=<xsl:value-of select="className"/></xsl:attribute>
	         <xsl:attribute name="target">_blank</xsl:attribute>
                 <xsl:value-of select="className"/>
                </a>
              </span>
             </td></tr><tr><td>
	     <xsl:if
	      test="not(string(following-sibling::attClass[1]/className)='')">
	       <input class="checkbox" type="checkbox">
		 <xsl:attribute name="name">class|<xsl:value-of select="following-sibling::attClass[1]/className"/></xsl:attribute>
		 <xsl:attribute name="value"><xsl:value-of select="following-sibling::attClass[1]/className"/></xsl:attribute>
		 <xsl:variable name="currentClass"><xsl:value-of select="following-sibling::attClass[1]/className"/></xsl:variable>
		 <xsl:if test="contains( $elementClasses, $currentClass )">
		   <xsl:attribute name="checked">1</xsl:attribute>
		 </xsl:if>
	       </input>
	       <span>
		 <xsl:attribute name="id">descSpan_attClass_<xsl:value-of select="following-sibling::attClass[1]/className"/></xsl:attribute>
		 <xsl:attribute name="onMouseover">descriptionPopup_Show( 'attClass_<xsl:value-of select="following-sibling::attClass[1]/className"/>' )</xsl:attribute>
		 <xsl:attribute name="onMouseout">descriptionPopup_Hide( 'attClass_<xsl:value-of select="following-sibling::attClass[1]/className"/>' )</xsl:attribute>
		 <a>
		   <xsl:attribute name="href">http://<xsl:value-of select="$host"/>:8080/exist/tei/class.xq?name=<xsl:value-of select="following-sibling::attClass[1]/className"/></xsl:attribute>
		   <xsl:attribute name="target">_blank</xsl:attribute>
		   <xsl:value-of select="following-sibling::attClass[1]/className"/>
		 </a>
	       </span>
	     </xsl:if>
             </td></tr><tr><td>
	     <xsl:if
	      test="not(string(following-sibling::attClass[2]/className)='')">
	       <input class="checkbox" type="checkbox">
		 <xsl:attribute name="name">class|<xsl:value-of select="following-sibling::attClass[2]/className"/></xsl:attribute>
		 <xsl:attribute name="value"><xsl:value-of select="following-sibling::attClass[2]/className"/></xsl:attribute>
		 <xsl:variable name="currentClass"><xsl:value-of select="following-sibling::attClass[2]/className"/></xsl:variable>
		 <xsl:if test="contains( $elementClasses, $currentClass )">
		   <xsl:attribute name="checked">1</xsl:attribute>
		 </xsl:if>
	       </input>
	       <span>
		 <xsl:attribute name="id">descSpan_attClass_<xsl:value-of select="following-sibling::attClass[2]/className"/></xsl:attribute>
		 <xsl:attribute name="onMouseover">descriptionPopup_Show( 'attClass_<xsl:value-of select="following-sibling::attClass[2]/className"/>' )</xsl:attribute>
		 <xsl:attribute name="onMouseout">descriptionPopup_Hide( 'attClass_<xsl:value-of select="following-sibling::attClass[2]/className"/>' )</xsl:attribute>
		 <a>
		   <xsl:attribute name="href">http://<xsl:value-of select="$host"/>:8080/exist/tei/class.xq?name=<xsl:value-of select="following-sibling::attClass[2]/className"/></xsl:attribute>
		   <xsl:attribute name="target">_blank</xsl:attribute>
		   <xsl:value-of select="following-sibling::attClass[2]/className"/>
		 </a>
	       </span>
	     </xsl:if>
	     </td></tr>
	       <tr>
		 <td>
		   <xsl:if
		    test="not(string(following-sibling::attClass[3]/className)='')">
		     <input class="checkbox" type="checkbox">
		       <xsl:attribute name="name">class|<xsl:value-of select="following-sibling::attClass[3]/className"/></xsl:attribute>
		       <xsl:attribute name="value"><xsl:value-of select="following-sibling::attClass[3]/className"/></xsl:attribute>
		       <xsl:variable name="currentClass"><xsl:value-of select="following-sibling::attClass[3]/className"/></xsl:variable>
		       <xsl:if test="contains( $elementClasses, $currentClass )">
			 <xsl:attribute name="checked">1</xsl:attribute>
		       </xsl:if>
		     </input>
		     <span>
		       <xsl:attribute name="id">descSpan_attClass_<xsl:value-of select="following-sibling::attClass[3]/className"/></xsl:attribute>
		       <xsl:attribute name="onMouseover">descriptionPopup_Show( 'attClass_<xsl:value-of select="following-sibling::attClass[3]/className"/>' )</xsl:attribute>
		       <xsl:attribute name="onMouseout">descriptionPopup_Hide( 'attClass_<xsl:value-of select="following-sibling::attClass[3]/className"/>' )</xsl:attribute>
		       <a>
			 <xsl:attribute name="href">http://<xsl:value-of select="$host"/>:8080/exist/tei/class.xq?name=<xsl:value-of select="following-sibling::attClass[3]/className"/></xsl:attribute>
			 <xsl:attribute name="target">_blank</xsl:attribute>
			 <xsl:value-of select="following-sibling::attClass[3]/className"/>
		       </a>
		     </span>
		   </xsl:if>
		 </td>
	       </tr>
             </table>
            </td>
           </xsl:if>
          </xsl:for-each>
  </xsl:template>


  <xsl:template name="contentTypes">
     <xsl:if test="$selectedMode='addElement'">
       <div class="HideItem">
	 <select name="content" size="1">
	   <option value="empty">
	     <xsl:if test="$elementContents='empty'">
	       <xsl:attribute name="selected">1</xsl:attribute>
	     </xsl:if>
	     Empty
	   </option>
	   <option value="text">
	     <xsl:if test="$elementContents='text'">
	       <xsl:attribute name="selected">1</xsl:attribute>
	     </xsl:if>
	     Text
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