<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:prop="http://schemas.openxmlformats.org/officeDocument/2006/custom-properties"
                xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
                xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties"
                xmlns:dc="http://purl.org/dc/elements/1.1/"
                xmlns:dcterms="http://purl.org/dc/terms/"
                xmlns:dcmitype="http://purl.org/dc/dcmitype/"
                xmlns:iso="http://www.iso.org/ns/1.0"
                xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"
                xmlns:mml="http://www.w3.org/1998/Math/MathML"
                xmlns:mo="http://schemas.microsoft.com/office/mac/office/2008/main"
                xmlns:mv="urn:schemas-microsoft-com:mac:vml"
                xmlns:o="urn:schemas-microsoft-com:office:office"
                xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"
                xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
                xmlns:rel="http://schemas.openxmlformats.org/package/2006/relationships"
                xmlns:tbx="http://www.lisa.org/TBX-Specification.33.0.html"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teidocx="http://www.tei-c.org/ns/teidocx/1.0"
                xmlns:v="urn:schemas-microsoft-com:vml"
                xmlns:ve="http://schemas.openxmlformats.org/markup-compatibility/2006"
                xmlns:w10="urn:schemas-microsoft-com:office:word"
                xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
                xmlns:wne="http://schemas.microsoft.com/office/word/2006/wordml"
                xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
                xmlns:html="http://www.w3.org/1999/xhtml"                
                
                xmlns="http://www.tei-c.org/ns/1.0"
                version="2.0"
                exclude-result-prefixes="a cp dc dcterms dcmitype prop     iso m mml mo mv o pic r rel     tbx tei teidocx v xs ve w10 w wne wp">
    
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet for converting Word docx files to TEI </p>
         <p>This software is dual-licensed:

1. Distributed under a Creative Commons Attribution-ShareAlike 3.0
Unported License http://creativecommons.org/licenses/by-sa/3.0/ 

2. http://www.opensource.org/licenses/BSD-2-Clause
		
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

* Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

This software is provided by the copyright holders and contributors
"as is" and any express or implied warranties, including, but not
limited to, the implied warranties of merchantability and fitness for
a particular purpose are disclaimed. In no event shall the copyright
holder or contributors be liable for any direct, indirect, incidental,
special, exemplary, or consequential damages (including, but not
limited to, procurement of substitute goods or services; loss of use,
data, or profits; or business interruption) however caused and on any
theory of liability, whether in contract, strict liability, or tort
(including negligence or otherwise) arising in any way out of the use
of this software, even if advised of the possibility of such damage.
</p>
         <p>Author: See AUTHORS</p>
         <p>Id: $Id$</p>
         <p>Copyright: 2008, TEI Consortium</p>
      </desc>
   </doc>

   <xsl:template name="getTableBorderStyles">
     <xsl:param name="tblBorders"/>
     <xsl:if test="$tblBorders//w:left[@w:sz!='']">
       <xsl:text>border-left: </xsl:text><xsl:value-of select="$tblBorders//w:left[1]/@w:sz"/><xsl:text>; </xsl:text>
     </xsl:if>
     <xsl:if test="$tblBorders//w:left[@w:val='nil']">
       <xsl:text>border-left: 0; </xsl:text>
     </xsl:if>
     <xsl:if test="$tblBorders//w:bottom[@w:sz!='']">
       <xsl:text>border-bottom: </xsl:text><xsl:value-of select="$tblBorders//w:bottom[1]/@w:sz"/><xsl:text>; </xsl:text>
     </xsl:if>
     <xsl:if test="$tblBorders//w:bottom[@w:val='nil']">
       <xsl:text>border-bottom: 0; </xsl:text>
     </xsl:if>
     <xsl:if test="$tblBorders//w:right[@w:sz!='']">
       <xsl:text>border-right: </xsl:text><xsl:value-of select="$tblBorders//w:right[1]/@w:sz"/><xsl:text>; </xsl:text>
     </xsl:if>
     <xsl:if test="$tblBorders//w:right[@w:val='nil']">
       <xsl:text>border-right: 0; </xsl:text>
     </xsl:if>
     <xsl:if test="$tblBorders//w:top[@w:sz!='']">
       <xsl:text>border-top: </xsl:text><xsl:value-of select="$tblBorders//w:top[1]/@w:sz"/><xsl:text>; </xsl:text>
     </xsl:if>
     <xsl:if test="$tblBorders//w:top[@w:val='nil']">
       <xsl:text>border-top: 0; </xsl:text>
     </xsl:if>
   </xsl:template>
    
    	<xsl:template match="w:tbl" mode="paragraph">
	     <xsl:choose>
	        <xsl:when test="$tableMethod='word'">
	           <xsl:copy>
		             <xsl:copy-of select="@*"/>
		             <xsl:apply-templates/>
	           </xsl:copy>
	        </xsl:when>
	        <xsl:when test="$tableMethod='cals'">
	      <!-- preprocess the table to expand colspans, add row numbers, and
		   simplify vertical merge info -->

	      <xsl:variable name="TABLE">

	      <xsl:variable name="tableBorders">
		<xsl:choose>
		  <xsl:when test="w:tblPr/w:tblBorders/w:top[@w:val='single']">
		    <xsl:copy-of select="w:tblPr/w:tblBorders/w:top"/>
		  </xsl:when>
		  <xsl:when test=".//w:tr[1]/w:tc[1]/w:tcPr/w:tcBorders/w:top[@w:val='single']">
		    <xsl:copy-of select=".//w:tr[1]/w:tc[1]/w:tcPr/w:tcBorders/w:top"/>
		  </xsl:when>
		</xsl:choose>
		<xsl:choose>
		  <xsl:when test="w:tblPr/w:tblBorders/w:left[@w:val='single']">
		    <xsl:copy-of select="w:tblPr/w:tblBorders/w:left"/>
		  </xsl:when>
		  <xsl:when test=".//w:tr[1]/w:tc[1]/w:tcPr/w:tcBorders/w:left[@w:val='single']">
		    <xsl:copy-of select=".//w:tr[1]/w:tc[1]/w:tcPr/w:tcBorders/w:left"/>
		  </xsl:when>
		</xsl:choose>
		<xsl:choose>
		  <xsl:when test="w:tblPr/w:tblBorders/w:bottom[@w:val='single']">
		    <xsl:copy-of select="w:tblPr/w:tblBorders/w:bottom"/>
		  </xsl:when>
		  <xsl:when test=".//w:tr[last()]/w:tc[last()]/w:tcPr/w:tcBorders/w:bottom[@w:val='single']">
		    <xsl:copy-of select=".//w:tr[last()]/w:tc[last()]/w:tcPr/w:tcBorders/w:bottom"/>
		  </xsl:when>
		</xsl:choose>
		<xsl:choose>
		  <xsl:when test="w:tblPr/w:tblBorders/w:right[@w:val='single']">
		    <xsl:copy-of select="w:tblPr/w:tblBorders/w:right"/>
		  </xsl:when>
		  <xsl:when test=".//w:tr[last()]/w:tc[last()]/w:tcPr/w:tcBorders/w:right[@w:val='single']">
		    <xsl:copy-of select=".//w:tr[last()]/w:tc[last()]/w:tcPr/w:tcBorders/w:right"/>
		  </xsl:when>
		</xsl:choose>		
	      </xsl:variable>
	      <xsl:variable name="tableBorderStyles">
		<xsl:call-template name="getTableBorderStyles">
		  <xsl:with-param name="tblBorders" select="$tableBorders"/>
		</xsl:call-template>
	      </xsl:variable>
		             <table xmlns="http://www.oasis-open.org/specs/tm9901">
			       <xsl:if test="normalize-space($tableBorderStyles)!=''">
				 <xsl:attribute name="iso:style">
				   <xsl:value-of select="normalize-space($tableBorderStyles)"/>
				 </xsl:attribute>
			       </xsl:if>
			       <xsl:attribute name="frame">
		                   <xsl:choose>
		      <!-- lets face it, most tables do have
			   borders, especially in ISO; but not in footers! -->
		      <xsl:when test="not(w:tblPr/w:tblBorders) and
				      parent::w:ftr">
			                        <xsl:text>none</xsl:text>
		                      </xsl:when>
		                      <xsl:when test="not($tableBorders)">
					<!-- if really no info on borders, default 
					     to all (? is this really what we want?) -->
					<xsl:text>all</xsl:text>
		                      </xsl:when>
		                      <xsl:otherwise>
					<xsl:for-each select="$tableBorders">
					  <xsl:choose>
					    <xsl:when test="    w:top/@w:val='single'
							    and w:bottom/@w:val='single'
							    and w:right/@w:val='single'        
							    and w:left/@w:val='single'">all</xsl:when>
					    <xsl:when test="    w:top/@w:val='single' 
							    and w:bottom/@w:val='single'
							    and not(w:right/@w:val='single') 
							    and not(w:left/@w:val='single')">topbot</xsl:when>
					    <xsl:when test="    w:top/@w:val='single' 
							    and not(w:bottom/@w:val='single') 
							    and not(w:right/@w:val='single') 
							    and not(w:left/@w:val='single')">top</xsl:when>
					    <xsl:when test="    not(w:top/@w:val='single') 
							    and w:bottom/@w:val='single' 
							    and not(w:right/@w:val='single') 
							    and not(w:left/@w:val='single')">bottom</xsl:when>
					    <xsl:when test="    not(w:top/@w:val='single') 
							    and not(w:bottom/@w:val='single') 
							    and w:right/@w:val='single' 
							    and w:left/@w:val='single'">sides</xsl:when>
					    <xsl:otherwise>
					      <!-- start guessing -->
					      <xsl:variable name="sideBorders">
						<xsl:choose>
						  <xsl:when test="w:left/@w:val='single' 
								  or w:right/@w:val='single'
								  or ../../w:tr[1]/w:tc[1]/w:tcPr/w:tcBorders/w:left[@w:val='single']
								  or ../../w:tr[last()]/w:tc[last()]/w:tcPr/w:tcBorders/w:right[@w:val='single']">true</xsl:when> 
						  <xsl:otherwise>false</xsl:otherwise>
						</xsl:choose>
					      </xsl:variable>
					      <xsl:choose>
						<xsl:when test="$sideBorders='true'
								and(w:bottom/@w:val='single' or w:top/@w:val='single')">all</xsl:when>
						<xsl:otherwise>
						  <xsl:text>none</xsl:text>
						</xsl:otherwise>
					      </xsl:choose>
					    </xsl:otherwise>
					  </xsl:choose>
					</xsl:for-each>
		                      </xsl:otherwise>
		                   </xsl:choose>
		                </xsl:attribute>
		                <xsl:attribute name="colsep">
		                   <xsl:choose>
		                      <xsl:when test="w:tblPr/w:tblBorders/w:insideV/@w:val='single'">1</xsl:when>
		                      <xsl:otherwise>0</xsl:otherwise>
		                   </xsl:choose>
		                </xsl:attribute>
		                <xsl:attribute name="rowsep">
		                   <xsl:choose>
		                      <xsl:when test="w:tblPr/w:tblBorders/w:insideH/@w:val='single'">1</xsl:when>
		                      <xsl:otherwise>0</xsl:otherwise>
		                   </xsl:choose>
		                </xsl:attribute>
		                <xsl:call-template name="cals-table-header"/>
		                <tgroup>
		                   <xsl:for-each select="w:tblGrid/w:gridCol">
		                      <colspec colnum="{position()}" colname="c{position()}">
			                        <xsl:attribute name="colwidth" select="concat(number(@w:w) div 20,'pt')"/>
		                      </colspec>
		                   </xsl:for-each>
		                   <tbody>
		                      <xsl:for-each select="w:tr">
			                        <xsl:copy>
			                           <xsl:variable name="ROWPOS">
			                              <xsl:number/>
			                           </xsl:variable>
			                           <xsl:for-each select="w:tc">
						     <xsl:variable name="cellBorderStyles">
						       <xsl:choose>
							 <xsl:when test="w:tcBorders">
							   <xsl:call-template name="getTableBorderStyles">
							     <xsl:with-param name="tblBorders" select="w:tcBorders"/>
							   </xsl:call-template>
							 </xsl:when>
							 <xsl:when test="w:tcPr/w:tcBorders">
							   <xsl:call-template name="getTableBorderStyles">
							     <xsl:with-param name="tblBorders" select="w:tcPr/w:tcBorders"/>
							   </xsl:call-template>
							 </xsl:when>
						       </xsl:choose>
						     </xsl:variable>
			                              <xsl:variable name="VMERGE">
			                                 <xsl:choose>
				                                   <xsl:when test="w:tcPr/w:vMerge/@w:val='restart'">
				                                      <xsl:text>start</xsl:text>
				                                   </xsl:when>
				                                   <xsl:when test="w:tcPr[not(w:vMerge)]">
				                                      <xsl:text>start</xsl:text>
				                                   </xsl:when>
				                                   <xsl:otherwise>
				                                      <xsl:text>continue</xsl:text>
				                                   </xsl:otherwise>
			                                 </xsl:choose>
			                              </xsl:variable>
			                              <xsl:variable name="innards">
			                                 <xsl:copy-of select="w:tcPr"/>
			                              </xsl:variable>
			                              <xsl:copy>
			                                 <xsl:variable
							     name="N"
							     select="position()"/>
							 <xsl:if test="normalize-space($cellBorderStyles)!=''">
							   <xsl:attribute name="iso:style">
							     <xsl:value-of select="normalize-space($cellBorderStyles)"/>
							   </xsl:attribute>
							 </xsl:if>
			                                 <xsl:attribute name="rowsep">
				                                   <xsl:choose>
				                                      <xsl:when test="w:tcPr/w:tcBorders/w:bottom[@w:sz=0 or @w:val='nil']">
				                                         <xsl:text>0</xsl:text>
				                                      </xsl:when>
				                                      <xsl:when test="w:tcPr/w:tcBorders/w:bottom[@w:sz&gt;0]">
				                                         <xsl:text>1</xsl:text>
				                                      </xsl:when>
				                                      <xsl:when test="parent::w:tr/following-sibling::w:tr[1]/w:tc[$N]/w:tcPr/w:tcBorders/w:top[@w:sz&gt;0]">
				                                         <xsl:text>1</xsl:text>
				                                      </xsl:when>
				                                      <xsl:when test="ancestor::w:tbl/w:tblPr/w:tblBorders/w:insideH[@w:sz=0          or @w:val='nil']">
				                                         <xsl:text>0</xsl:text>
				                                      </xsl:when>
				                                      <xsl:when test="ancestor::w:tbl/w:tblPr/w:tblBorders/w:insideH">
				                                         <xsl:text>1</xsl:text>
				                                      </xsl:when>
				                                      <xsl:when test="not(parent::w:tr/following-sibling::w:tr)          and not(ancestor::w:ftr)">
				                                         <xsl:text>1</xsl:text>
				                                      </xsl:when>
				                                      <xsl:otherwise>
				                                         <xsl:text>0</xsl:text>
				                                      </xsl:otherwise>
				                                   </xsl:choose>
			                                 </xsl:attribute>
			                                 <xsl:attribute name="colsep">
				                                   <xsl:choose>
				                                      <xsl:when test="following-sibling::w:tc[1]/w:tcPr/w:tcBorders/w:left[@w:sz&gt;0]">
				                                         <xsl:text>1</xsl:text>
				                                      </xsl:when>
				                                      <xsl:when test="w:tcPr/w:tcBorders/w:right[@w:sz=0 or @w:val='nil']">
				                                         <xsl:text>0</xsl:text>
				                                      </xsl:when>
				                                      <xsl:when test="w:tcPr/w:tcBorders/w:right[@w:sz&gt;0]">
				                                         <xsl:text>1</xsl:text>
				                                      </xsl:when>
				                                      <xsl:when test="ancestor::w:tbl/w:tblPr/w:tblBorders/w:insideV[@w:sz=0          or @w:val='nil']">
				                                         <xsl:text>0</xsl:text>
				                                      </xsl:when>
				                                      <xsl:when test="not(ancestor::w:tbl/w:tblPr/w:tblBorders)          and ancestor::w:ftr">
				                                         <xsl:text>0</xsl:text>
				                                      </xsl:when>
				                                      <xsl:otherwise>
				                                         <xsl:text>1</xsl:text>
				                                      </xsl:otherwise>
				                                   </xsl:choose>
			                                 </xsl:attribute>
			                                 <xsl:attribute name="ROWPOS" select="$ROWPOS"/>
			                                 <xsl:attribute name="VMERGE" select="$VMERGE"/>
			                                 <xsl:copy-of select="@*"/>
			                                 <xsl:copy-of select="*"/>
			                              </xsl:copy>
			                              <xsl:if test="w:tcPr/w:gridSpan/@w:val">
			                                 <xsl:variable name="N" select="number(w:tcPr/w:gridSpan/@w:val)           cast as xs:integer"/>
			                                 <xsl:for-each select="2 to $N">
				                                   <w:tc DUMMY="yes">
								     <xsl:copy-of select="$innards"/>
				                                   </w:tc>
			                                 </xsl:for-each>
			                              </xsl:if>
			                           </xsl:for-each>
			                        </xsl:copy>
		                      </xsl:for-each>
		                   </tbody>
		                </tgroup>
		             </table>
		             <!--
		    <xsl:comment>START</xsl:comment>
		    <TABLE>
		    <xsl:copy-of select="$TABLE"/>
		    </TABLE>
		    <xsl:comment>END</xsl:comment>
		-->
	      </xsl:variable>
	      <xsl:variable name="n">
		<xsl:number level="any"/>
	      </xsl:variable>
	      <xsl:for-each select="$TABLE">
		<xsl:apply-templates mode="insideCalsTable">
		  <xsl:with-param name="n" select="$n" tunnel="yes"/>
		</xsl:apply-templates>
	      </xsl:for-each>
	    </xsl:when>
	    <xsl:otherwise>
	      <table rend="rules">
		<xsl:call-template name="table-header"/>
		<xsl:for-each select="w:tr">
		  <row>
		    <xsl:for-each select="w:tc">
		      <cell>
			<xsl:if test="preserveEffects='true'">
			  <xsl:attribute name="tei:align">
			    <xsl:choose>
			      <xsl:when test="w:p/w:pPr/w:jc">
				<xsl:value-of
				  select="w:p[1]/w:pPr/w:jc/@w:val"/>
			      </xsl:when>
			      <xsl:otherwise>
				<xsl:text>left</xsl:text>
			      </xsl:otherwise>
			    </xsl:choose>
			  </xsl:attribute>
			</xsl:if>
			<xsl:if test="w:tcPr/w:gridSpan">
			  <xsl:attribute name="cols" select="w:tcPr/w:gridSpan/@w:val"/>
			</xsl:if>
			<xsl:variable name="val" select="w:p[1]/w:pPr/w:pStyle/@w:val"/>
			<xsl:choose>
			  <xsl:when test="$val='[No Paragraph Style]'"/>
			  <xsl:when test="$val='Table text (9)'"/>
			  <xsl:when test="$val='Table Contents'"/>
			  <xsl:when test="string-length($val)=0"/>
			  <xsl:otherwise>
			    <xsl:attribute name="rend">
			      <xsl:value-of select="replace($val,' ','_')"/>
			      <xsl:if test="w:tcPr/w:shd/@w:fill and not(w:tcPr/w:shd/@w:fill='auto')">
				<xsl:text> background-color(</xsl:text>
				<xsl:value-of select="w:tcPr/w:shd/@w:fill"/>
				<xsl:text>)</xsl:text>
			      </xsl:if>
			    </xsl:attribute>
			  </xsl:otherwise>
			  </xsl:choose>
			  <xsl:if test="w:tcPr/w:gridSpan">
			    <xsl:attribute name="cols">
			      <xsl:value-of select="w:tcPr/w:gridSpan/@w:val"/>
			    </xsl:attribute>
			  </xsl:if>
			  <xsl:call-template name="mainProcess"/>
		      </cell>
		    </xsl:for-each>
		  </row>
		</xsl:for-each>
	      </table>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:template>
	
	  <xsl:template match="*" mode="insideCalsTable">
	     <xsl:copy>
	        <xsl:copy-of select="@*"/>
	        <xsl:apply-templates mode="insideCalsTable"/>	    
	     </xsl:copy>
	  </xsl:template>

	  <xsl:template match="w:tr" mode="insideCalsTable">
	     <row xmlns="http://www.oasis-open.org/specs/tm9901">
	        <xsl:for-each select="w:tc[not(@DUMMY='yes')]">
		  <xsl:variable name="cellBorderStyles">
		    <xsl:call-template name="getTableBorderStyles">
		      <xsl:with-param name="tblBorders" select="w:tcPr/w:tcBorders"/>
		    </xsl:call-template>
		  </xsl:variable>
	           <xsl:choose>
		             <xsl:when test="w:tcPr/w:vMerge[not(@w:val='restart')]"/>
		             <xsl:otherwise>
		                <entry>
		                   <xsl:variable name="ROWPOS" select="@ROWPOS"/>
		                   <xsl:variable name="COLPOS">
		                      <xsl:number/>
		                   </xsl:variable>
		                   <xsl:copy-of select="@rowsep"/>
		                   <xsl:copy-of select="@colsep"/>
		                   <xsl:attribute name="colname">
		                      <xsl:text>c</xsl:text>
		                      <xsl:value-of select="$COLPOS"/>
		                   </xsl:attribute>
				   <xsl:if test="normalize-space($cellBorderStyles)!=''">
				     <xsl:attribute
					 name="iso:style">
				       <xsl:value-of select="normalize-space($cellBorderStyles)"/>
				     </xsl:attribute>
				   </xsl:if>
		                   <xsl:if test="w:p/w:pPr/w:jc">
		                      <xsl:attribute name="align">
			                        <xsl:value-of select="w:p[w:pPr/w:jc/@w:val][1]/w:pPr/w:jc/@w:val"/>
		                      </xsl:attribute>
		                   </xsl:if>
		                   <xsl:if test="w:tcPr/w:gridSpan">
		                      <xsl:attribute name="namest">
			                        <xsl:text>c</xsl:text>
			                        <xsl:value-of select="$COLPOS"/>
		                      </xsl:attribute>
		                      <xsl:attribute name="nameend">
			                        <xsl:text>c</xsl:text>
			                        <xsl:value-of select="$COLPOS+number(w:tcPr/w:gridSpan/@w:val)-1"/>
		                      </xsl:attribute>
		                   </xsl:if>
		                   <xsl:if test="w:tcPr/w:vAlign">
		                      <xsl:attribute name="valign">
			                        <xsl:choose>
			                           <xsl:when test="w:tcPr/w:vAlign/@w:val='center'">middle</xsl:when>
			                           <xsl:otherwise>
			                              <xsl:value-of select="w:tcPr/w:vAlign/@w:val"/>
			                           </xsl:otherwise>
			                        </xsl:choose>
		                      </xsl:attribute>
		                   </xsl:if>
		                   <xsl:if test="w:tcPr/w:textDirection[@w:val='btLr']">
		                      <xsl:attribute name="rotate">
			                        <xsl:text>1</xsl:text>
		                      </xsl:attribute>
		                   </xsl:if>
		                   <xsl:if test="w:tcPr/w:vMerge/@w:val='restart'">
		                      <xsl:variable name="MOREROWS">
			                        <xsl:choose>
			                           <xsl:when test="parent::w:tr/following-sibling::w:tr/w:tc[position()=$COLPOS][@VMERGE='start']">
			                              <xsl:for-each select="(parent::w:tr/following-sibling::w:tr/w:tc[position()=$COLPOS][@VMERGE='start'])[1]">
			                                 <xsl:value-of select="@ROWPOS"/>
			                              </xsl:for-each>
			                           </xsl:when>
			                           <xsl:otherwise>
			                              <xsl:value-of select="parent::w:tr/following-sibling::w:tr[last()]/w:tc/@ROWPOS"/>
			                           </xsl:otherwise>
			                        </xsl:choose>
		                      </xsl:variable>
		                      <!--
			  <xsl:message>start a merged cell at <xsl:value-of
			  select="$ROWPOS"/>/<xsl:value-of select="$COLPOS"/>
			  <xsl:text>: </xsl:text>
			  <xsl:value-of select="$MOREROWS"/>-<xsl:value-of
			  select="$ROWPOS"/> =<xsl:value-of select="."/>
			  </xsl:message>
		      -->
		      <xsl:attribute name="morerows">
			<xsl:value-of select="number($MOREROWS) -            number($ROWPOS) -1"/>
		      </xsl:attribute>
				   </xsl:if>
				   <xsl:choose>
				     <xsl:when test="count(w:p)&gt;1
						     or w:p/w:pPr/w:pStyle">
					<xsl:call-template name="cellContents"/>
		                      </xsl:when>
		                      <xsl:otherwise>
			                        <xsl:apply-templates/>
		                      </xsl:otherwise>
		                   </xsl:choose>
		                </entry>
		             </xsl:otherwise>
	           </xsl:choose>
	        </xsl:for-each>
	     </row>

	  </xsl:template>

	  <xsl:template name="cellContents">
-	     <xsl:apply-templates select="w:p" mode="inTable"/> 
	  </xsl:template>

	  <xsl:template match="w:p" mode="inTable">
	     <p rend="{w:pPr/w:pStyle/@w:val}">
	        <xsl:apply-templates/>
	     </p>
	  </xsl:template>

	  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
	    getting the basic table structure
    </desc>
   </doc>
	  <xsl:template match="w:gridCol|w:tblBorders|w:tblCellMar|w:tblGrid|w:tblLayout|w:tblLook|w:tblPr|w:tblW|w:tc|w:tcBorders|w:tcPr|w:tcW|w:tr|w:trPr|w:vAlign|w:top|w:left|w:bottom|w:right|w:insideH|w:insideV">

		    <xsl:choose>
			      <xsl:when test="$tableMethod='word'">
				        <xsl:copy>
					          <xsl:copy-of select="@*"/>
					          <xsl:apply-templates/>
				        </xsl:copy>
			      </xsl:when>
			      <xsl:otherwise>
				        <xsl:apply-templates/>
			      </xsl:otherwise>
		    </xsl:choose>
	  </xsl:template>



    <xsl:template name="table-header">
        <xsl:variable name="precedingTableTitle"
                    select="preceding-sibling::w:p[w:pPr/w:pStyle/@w:val='TableTitle'             or w:pPr/w:pStyle/@w:val=$Tabletitle][1]"/>
        <xsl:if test="$precedingTableTitle and $precedingTableTitle/following-sibling::w:tbl[1] and generate-id()=generate-id($precedingTableTitle/following-sibling::w:tbl[1])">
            <head>
                <xsl:apply-templates select="$precedingTableTitle"/>
            </head>
        </xsl:if>
    </xsl:template>
    
    <xsl:template name="cals-table-header">
        <xsl:variable name="precedingTableTitle"
                    select="preceding-sibling::w:p[w:pPr/w:pStyle/@w:val='TableTitle'             or w:pPr/w:pStyle/@w:val=$Tabletitle][1]"/>
        <xsl:if test="$precedingTableTitle and $precedingTableTitle/following-sibling::w:tbl[1] and generate-id()=generate-id($precedingTableTitle/following-sibling::w:tbl[1])">
            <title xmlns="http://www.oasis-open.org/specs/tm9901">
                <xsl:apply-templates select="$precedingTableTitle"/>
            </title>
        </xsl:if>
    </xsl:template>
    
</xsl:stylesheet>
