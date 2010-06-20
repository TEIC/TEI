<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:fo="http://www.w3.org/1999/XSL/Format"
                xmlns:s="http://www.ascc.net/xml/schematron"
                xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns:html="http://www.w3.org/1999/xhtml"
                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
		xmlns:sch="http://purl.oclc.org/dsdl/schematron"
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="fo s a tei html rng teix xs sch"
                version="2.0">

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet for weaving TEI ODD documents</p>
         <p> This library is free software; you can redistribute it
    and/or modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later
    version. This library is distributed in the hope that it will be
    useful, but WITHOUT ANY WARRANTY; without even the implied
    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
    PURPOSE. See the GNU Lesser General Public License for more
    details. You should have received a copy of the GNU Lesser General
    Public License along with this library; if not, write to the Free
    Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
    02111-1307 USA </p>
         <p>Author: See AUTHORS</p>
         <p>Id: $Id$</p>
         <p>Copyright: 2008, TEI Consortium</p>
      </desc>
   </doc>

   <xsl:key name="CHILDMOD" match="Element" use="@module"/>

   <xsl:template match="tei:attDef" mode="summary">
      <xsl:variable name="name">
         <xsl:choose>
            <xsl:when test="tei:altIdent">
               <xsl:value-of select="tei:altIdent"/>
            </xsl:when>
            <xsl:otherwise>
               <xsl:value-of select="@ident"/>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>
      <xsl:element namespace="{$outputNS}" name="{$rowName}">
         <xsl:element namespace="{$outputNS}" name="{$cellName}">
            <xsl:attribute name="{$rendName}">
               <xsl:text>odd_label</xsl:text>
            </xsl:attribute>
            <xsl:element namespace="{$outputNS}" name="{$codeName}">
               <xsl:attribute name="{$rendName}">
                  <xsl:text>att</xsl:text>
               </xsl:attribute>
               <xsl:value-of select="$name"/>
            </xsl:element>
         </xsl:element>
         <xsl:element namespace="{$outputNS}" name="{$cellName}">
            <xsl:attribute name="{$rendName}">
               <xsl:text>odd_value</xsl:text>
            </xsl:attribute>
            <xsl:call-template name="makeDescription"/>
            <xsl:apply-templates select="valList"/>
         </xsl:element>
      </xsl:element>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element attDef</desc>
   </doc>
  <xsl:template match="tei:attDef">
      <xsl:variable name="name">
         <xsl:choose>
            <xsl:when test="tei:altIdent">
               <xsl:value-of select="tei:altIdent"/>
            </xsl:when>
            <xsl:otherwise>
               <xsl:value-of select="@ident"/>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>
    
      <xsl:element namespace="{$outputNS}" name="{$rowName}">
         <xsl:element namespace="{$outputNS}" name="{$cellName}">
            <xsl:attribute name="{$rendName}">
               <xsl:text>odd_label</xsl:text>
            </xsl:attribute>
	           <xsl:value-of select="$name"/>
         </xsl:element>
         <xsl:element namespace="{$outputNS}" name="{$cellName}">
            <xsl:attribute name="{$rendName}">
               <xsl:text>odd_value</xsl:text>
            </xsl:attribute>
            <xsl:call-template name="makeDescription"/>
            <xsl:element namespace="{$outputNS}" name="{$tableName}">
               <xsl:attribute name="{$rendName}">
                  <xsl:text>attDef</xsl:text>
               </xsl:attribute>
               <xsl:element namespace="{$outputNS}" name="{$rowName}">
                  <xsl:element namespace="{$outputNS}" name="{$cellName}">
                     <xsl:attribute name="{$rendName}">
                        <xsl:text>odd_label</xsl:text>
	                    </xsl:attribute>
	                    <xsl:element namespace="{$outputNS}" name="{$hiName}">
		                      <xsl:attribute name="{$rendName}">
		                         <xsl:text>label</xsl:text>
		                      </xsl:attribute>
		                      <xsl:attribute name="xml:lang">
		                         <xsl:value-of select="$documentationLanguage"/>
		                      </xsl:attribute>
		                      <xsl:call-template name="i18n">
		                         <xsl:with-param name="word">Status</xsl:with-param>
		                      </xsl:call-template>
	                    </xsl:element>
                     <xsl:text> </xsl:text>
                  </xsl:element>
                  <xsl:element namespace="{$outputNS}" name="{$cellName}">
                     <xsl:attribute name="{$rendName}">
                        <xsl:text>odd_value</xsl:text>
                     </xsl:attribute>
	                    <xsl:element namespace="{$outputNS}" name="{$segName}">
		                      <xsl:attribute name="xml:lang">
		                         <xsl:value-of select="$documentationLanguage"/>
		                      </xsl:attribute>		
		                      <xsl:choose>
		                         <xsl:when test="@usage='mwa'">
		                            <xsl:call-template name="i18n">
		                               <xsl:with-param name="word">Mandatory when applicable</xsl:with-param>
		                            </xsl:call-template>
		                         </xsl:when>
		                         <xsl:when test="@usage='opt'">
		                            <xsl:call-template name="i18n">
		                               <xsl:with-param name="word">Optional</xsl:with-param>
		                            </xsl:call-template>
		                         </xsl:when>
		                         <xsl:when test="@usage='rec'">
		                            <xsl:call-template name="i18n">
		                               <xsl:with-param name="word">Recommended</xsl:with-param>
		                            </xsl:call-template>
		                         </xsl:when>
		                         <xsl:when test="@usage='req'">
		                            <xsl:element namespace="{$outputNS}" name="{$hiName}">
		                               <xsl:attribute name="{$rendName}">
                                    <xsl:text>required</xsl:text>
		                               </xsl:attribute>
		                               <xsl:call-template name="i18n">
			                                 <xsl:with-param name="word">Required</xsl:with-param>
		                               </xsl:call-template>
		                            </xsl:element>
		                         </xsl:when>
		                         <xsl:when test="@usage='rwa'">
		                            <xsl:call-template name="i18n">
		                               <xsl:with-param name="word">Recommended when applicable</xsl:with-param>
		                            </xsl:call-template>
                           </xsl:when>
		                         <xsl:otherwise>
		                            <xsl:call-template name="i18n">
		                               <xsl:with-param name="word">Optional</xsl:with-param>
		                            </xsl:call-template>
                           </xsl:otherwise>
		                      </xsl:choose>
	                    </xsl:element>
	                 </xsl:element>
	              </xsl:element>
	              <xsl:apply-templates mode="weave"/>
	           </xsl:element>
         </xsl:element>
      </xsl:element>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element attDef/tei:datatype</desc>
   </doc>
  <xsl:template match="tei:attDef/tei:datatype" mode="weave">
      <xsl:element namespace="{$outputNS}" name="{$rowName}">
         <xsl:element namespace="{$outputNS}" name="{$cellName}">
            <xsl:attribute name="{$rendName}">
               <xsl:text>odd_label</xsl:text>
	           </xsl:attribute>
	           <xsl:element namespace="{$outputNS}" name="{$hiName}">
	              <xsl:attribute name="xml:lang">
	                 <xsl:value-of select="$documentationLanguage"/>
	              </xsl:attribute>
	              <xsl:attribute name="{$rendName}">
	                 <xsl:text>label</xsl:text>
	              </xsl:attribute>
	              <xsl:call-template name="i18n">
	                 <xsl:with-param name="word">Datatype</xsl:with-param>
	              </xsl:call-template>
	           </xsl:element>
            <xsl:text> </xsl:text>
         </xsl:element>
         <xsl:element namespace="{$outputNS}" name="{$cellName}">
            <xsl:attribute name="{$rendName}">
               <xsl:text>odd_value</xsl:text>
            </xsl:attribute>
            <xsl:variable name="minOccurs">
               <xsl:choose>
                  <xsl:when test="@minOccurs">
                     <xsl:value-of select="@minOccurs"/>
                  </xsl:when>
                  <xsl:otherwise>1</xsl:otherwise>
               </xsl:choose>
            </xsl:variable>
            <xsl:variable name="maxOccurs">
               <xsl:choose>
                  <xsl:when test="@maxOccurs='unbounded'">
                     <xsl:text>∞</xsl:text>
                  </xsl:when>
                  <xsl:when test="@maxOccurs">
                     <xsl:value-of select="@maxOccurs"/>
                  </xsl:when>
                  <xsl:otherwise>1</xsl:otherwise>
               </xsl:choose>
            </xsl:variable>
            <xsl:if test="$minOccurs != '1'  or  $maxOccurs != '1'">
               <xsl:text> </xsl:text>
               <xsl:value-of select="$minOccurs"/>
               <xsl:text>–</xsl:text>
               <xsl:value-of select="$maxOccurs"/>
	              <xsl:text> </xsl:text>
	              <xsl:element namespace="{$outputNS}" name="{$segName}">
	                 <xsl:attribute name="xml:lang">
	                    <xsl:value-of select="$documentationLanguage"/>
	                 </xsl:attribute>
	                 <xsl:call-template name="i18n">
	                    <xsl:with-param name="word">occurrences of</xsl:with-param>
	                 </xsl:call-template>
	              </xsl:element>
	              <xsl:value-of select="$spaceCharacter"/>
            </xsl:if>
            <xsl:call-template name="bitOut">
               <xsl:with-param name="grammar"/>
               <xsl:with-param name="element">code</xsl:with-param>
               <xsl:with-param name="content">
                  <Wrapper>
                     <xsl:copy-of select="rng:*"/>
                  </Wrapper>
               </xsl:with-param>
            </xsl:call-template>
	           <xsl:if test="$minOccurs != '1'  or  $maxOccurs != '1'">
	              <xsl:element namespace="{$outputNS}" name="{$segName}">
	                 <xsl:attribute name="xml:lang">
	                    <xsl:value-of select="$documentationLanguage"/>
	                 </xsl:attribute>
	                 <xsl:call-template name="i18n">
	                    <xsl:with-param name="word">separated by whitespace</xsl:with-param>
	                 </xsl:call-template>
	              </xsl:element>
            </xsl:if>
         </xsl:element>
      </xsl:element>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element attList</desc>
   </doc>
  <xsl:template match="tei:attList" mode="show">
      <xsl:call-template name="displayAttList">
         <xsl:with-param name="mode">summary</xsl:with-param>
      </xsl:call-template>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element classSpec</desc>
   </doc>
  <xsl:template match="tei:classSpec">
      <xsl:if test="parent::tei:specGrp">
         <xsl:element namespace="{$outputNS}" name="{$dtName}">
            <xsl:element namespace="{$outputNS}" name="{$hiName}">
               <xsl:attribute name="{$rendName}">
                  <xsl:text>label</xsl:text>
               </xsl:attribute>
	              <xsl:attribute name="xml:lang">
	                 <xsl:value-of select="$documentationLanguage"/>
	              </xsl:attribute>
               <xsl:call-template name="i18n">
                  <xsl:with-param name="word">Class</xsl:with-param>
               </xsl:call-template>
            </xsl:element>: <xsl:value-of select="@ident"/>
         </xsl:element>
         <xsl:element namespace="{$outputNS}" name="{$ddName}">
            <xsl:apply-templates mode="tangle" select="."/>
            <xsl:text>(</xsl:text>
            <xsl:element namespace="{$outputNS}" name="{$segName}">
	              <xsl:attribute name="xml:lang">
	                 <xsl:value-of select="$documentationLanguage"/>
	              </xsl:attribute>
	              <xsl:call-template name="i18n">
	                 <xsl:with-param name="word">Members</xsl:with-param>
	              </xsl:call-template>
	           </xsl:element>
            <xsl:text>: </xsl:text>
            <xsl:call-template name="generateMembers"/>
            <xsl:text>)</xsl:text>
         </xsl:element>
      </xsl:if>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element classSpec</desc>
   </doc>
  <xsl:template match="tei:classSpec" mode="weavebody">
      <xsl:variable name="name">
         <xsl:choose>
            <xsl:when test="tei:altIdent">
               <xsl:value-of select="tei:altIdent"/>
            </xsl:when>
            <xsl:otherwise>
               <xsl:value-of select="@ident"/>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>
      <xsl:element namespace="{$outputNS}" name="{$sectionName}">
         <xsl:call-template name="makeSectionHead">
	           <xsl:with-param name="id">
	              <xsl:value-of select="@ident"/>
	           </xsl:with-param>
	           <xsl:with-param name="name">
	              <xsl:value-of select="$name"/>
	           </xsl:with-param>
         </xsl:call-template>
         <xsl:element namespace="{$outputNS}" name="{$tableName}">
            <xsl:attribute name="{$rendName}">
               <xsl:text>wovenodd</xsl:text>
            </xsl:attribute>
            <xsl:element namespace="{$outputNS}" name="{$rowName}">
               <xsl:element namespace="{$outputNS}" name="{$cellName}">
                  <xsl:attribute name="{$colspan}">2</xsl:attribute>
                  <xsl:attribute name="{$rendName}">
                     <xsl:text>wovenodd-col2</xsl:text>
                  </xsl:attribute>
                  <xsl:element namespace="{$outputNS}" name="{$hiName}">
                     <xsl:attribute name="{$rendName}">
                        <xsl:text>label</xsl:text>
                     </xsl:attribute>
                     <xsl:value-of select="$name"/>
                  </xsl:element>
	                 <xsl:call-template name="specHook">
		                   <xsl:with-param name="name">
		                      <xsl:value-of select="$name"/>
		                   </xsl:with-param>
	                 </xsl:call-template>
                  <xsl:text> </xsl:text>
                  <xsl:call-template name="makeDescription"/>
                  <xsl:if test="tei:listRef">
                     <xsl:for-each select="tei:listRef/tei:ptr">
                        <xsl:text> </xsl:text>
                        <xsl:apply-templates select="." mode="weave"/>
                     </xsl:for-each>
                  </xsl:if>
               </xsl:element>
            </xsl:element>
            <xsl:if test="@generate">
               <xsl:element namespace="{$outputNS}" name="{$rowName}">
                  <xsl:element namespace="{$outputNS}" name="{$cellName}">
                     <xsl:attribute name="{$rendName}">
                        <xsl:text>wovenodd-col1</xsl:text>
                     </xsl:attribute>
		                   <xsl:element namespace="{$outputNS}" name="{$segName}">
		                      <xsl:attribute name="xml:lang">
		                         <xsl:value-of select="$documentationLanguage"/>
		                      </xsl:attribute>
		                      <xsl:call-template name="i18n">
		                         <xsl:with-param name="word">
		                            <xsl:text>Classes defined</xsl:text>
		                         </xsl:with-param>
		                      </xsl:call-template>
		                   </xsl:element>
                  </xsl:element>
                  <xsl:element namespace="{$outputNS}" name="{$cellName}">
                     <xsl:attribute name="{$rendName}">
                        <xsl:text>wovenodd-col2</xsl:text>
                     </xsl:attribute>
                     <xsl:value-of select="@generate"/>
                  </xsl:element>
               </xsl:element>
            </xsl:if>
            <xsl:if test="@module">
               <xsl:call-template name="moduleInfo"/>
            </xsl:if>
            <xsl:if test="@type='model'">
               <xsl:element namespace="{$outputNS}" name="{$rowName}">
                  <xsl:element namespace="{$outputNS}" name="{$cellName}">
                     <xsl:attribute name="{$rendName}">
                        <xsl:text>wovenodd-col1</xsl:text>
                     </xsl:attribute>
                     <xsl:element namespace="{$outputNS}" name="{$hiName}">
                        <xsl:attribute name="{$rendName}">
                           <xsl:text>label</xsl:text>
                        </xsl:attribute>
		                      <xsl:attribute name="xml:lang">
		                         <xsl:value-of select="$documentationLanguage"/>
		                      </xsl:attribute>
                        <xsl:call-template name="i18n">
                           <xsl:with-param name="word">Used by</xsl:with-param>
                        </xsl:call-template>
                     </xsl:element>
                  </xsl:element>
                  <xsl:element namespace="{$outputNS}" name="{$cellName}">
                     <xsl:attribute name="{$rendName}">
                        <xsl:text>wovenodd-col2</xsl:text>
                     </xsl:attribute>
                     <xsl:call-template name="generateParents"/>
                  </xsl:element>
               </xsl:element>
            </xsl:if>
            <xsl:element namespace="{$outputNS}" name="{$rowName}">
               <xsl:element namespace="{$outputNS}" name="{$cellName}">
                  <xsl:attribute name="{$rendName}">
                     <xsl:text>wovenodd-col1</xsl:text>
                  </xsl:attribute>
                  <xsl:element namespace="{$outputNS}" name="{$hiName}">
                     <xsl:attribute name="{$rendName}">
                        <xsl:text>label</xsl:text>
                     </xsl:attribute>
		                   <xsl:attribute name="xml:lang">
		                      <xsl:value-of select="$documentationLanguage"/>
		                   </xsl:attribute>
                     <xsl:call-template name="i18n">
                        <xsl:with-param name="word">Members</xsl:with-param>
                     </xsl:call-template>
                  </xsl:element>
               </xsl:element>
               <xsl:element namespace="{$outputNS}" name="{$cellName}">
                  <xsl:attribute name="{$rendName}">
                     <xsl:text>wovenodd-col2</xsl:text>
                  </xsl:attribute>
                  <xsl:call-template name="generateMembers"/>
               </xsl:element>
            </xsl:element>
            <xsl:if test="@type='atts'">
               <xsl:element namespace="{$outputNS}" name="{$rowName}">
                  <xsl:element namespace="{$outputNS}" name="{$cellName}">
                     <xsl:attribute name="{$rendName}">
                        <xsl:text>wovenodd-col1</xsl:text>
                     </xsl:attribute>
                     <xsl:element namespace="{$outputNS}" name="{$hiName}">
                        <xsl:attribute name="{$rendName}">
                           <xsl:text>label</xsl:text>
                        </xsl:attribute>
		                      <xsl:attribute name="xml:lang">
		                         <xsl:value-of select="$documentationLanguage"/>
		                      </xsl:attribute>
                        <xsl:call-template name="i18n">
                           <xsl:with-param name="word">Attributes</xsl:with-param>
                        </xsl:call-template>
                     </xsl:element>
                  </xsl:element>
                  <xsl:element namespace="{$outputNS}" name="{$cellName}">
                     <xsl:attribute name="{$rendName}">
                        <xsl:text>wovenodd-col2</xsl:text>
                     </xsl:attribute>
                     <xsl:choose>
                        <xsl:when test="not(tei:attList)">
                           <xsl:call-template name="showAttClasses"/>
                        </xsl:when>
                        <xsl:otherwise>
                           <xsl:for-each select="tei:attList">
                              <xsl:call-template name="displayAttList">
                                 <xsl:with-param name="mode">all</xsl:with-param>
                              </xsl:call-template>
                           </xsl:for-each>
                        </xsl:otherwise>
                     </xsl:choose>
                  </xsl:element>
               </xsl:element>
            </xsl:if>
            <xsl:apply-templates mode="weave"/>
        </xsl:element>
      </xsl:element>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element classes</desc>
   </doc>
  <xsl:template match="tei:classes" mode="weave"> </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element elementSpec</desc>
   </doc>
  <xsl:template match="tei:elementSpec">
      <xsl:if test="parent::tei:specGrp">
         <xsl:element namespace="{$outputNS}" name="{$dtName}">
            <xsl:element namespace="{$outputNS}" name="{$segName}">
	              <xsl:attribute name="xml:lang">
	                 <xsl:value-of select="$documentationLanguage"/>
	              </xsl:attribute>
	              <xsl:call-template name="i18n">
	                 <xsl:with-param name="word">Element</xsl:with-param>
	              </xsl:call-template>
	              <xsl:value-of select="$spaceCharacter"/>
	              <xsl:value-of select="@ident"/>
	           </xsl:element>
         </xsl:element>
         <xsl:element namespace="{$outputNS}" name="{$ddName}">
            <xsl:apply-templates mode="tangle" select="."/>
         </xsl:element>
      </xsl:if>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element elementSpec</desc>
   </doc>

  <xsl:template match="tei:elementSpec" mode="weavebody">
      <xsl:variable name="name">
         <xsl:choose>
            <xsl:when test="tei:altIdent">
               <xsl:value-of select="tei:altIdent"/>
            </xsl:when>
            <xsl:otherwise>
               <xsl:value-of select="@ident"/>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>
      <xsl:element namespace="{$outputNS}" name="{$sectionName}">
         <xsl:call-template name="makeSectionHead">
	           <xsl:with-param name="id">
	              <xsl:value-of select="@ident"/>
	           </xsl:with-param>
	           <xsl:with-param name="name">
	              <xsl:text>&lt;</xsl:text>
	              <xsl:choose>
	                 <xsl:when test="tei:content/rng:empty">
	                    <xsl:call-template name="emptySlash">
		                      <xsl:with-param name="name">
		                         <xsl:value-of select="$name"/>
		                      </xsl:with-param>
	                    </xsl:call-template>
	                 </xsl:when>
	                 <xsl:otherwise>
	                    <xsl:value-of select="$name"/>
	                 </xsl:otherwise>
	              </xsl:choose>
	              <xsl:text>&gt;</xsl:text>
	           </xsl:with-param>
         </xsl:call-template>
         <xsl:call-template name="specHook">
	           <xsl:with-param name="name">
	              <xsl:value-of select="$name"/>
	           </xsl:with-param>
         </xsl:call-template>
         <xsl:element namespace="{$outputNS}" name="{$tableName}">
	           <xsl:attribute name="{$rendName}">
	              <xsl:text>wovenodd</xsl:text>
	           </xsl:attribute>
	           <xsl:element namespace="{$outputNS}" name="{$rowName}">
	              <xsl:element namespace="{$outputNS}" name="{$cellName}">
	                 <xsl:attribute name="{$colspan}">2</xsl:attribute>
	                 <xsl:attribute name="{$rendName}">
		                   <xsl:text>wovenodd-col2</xsl:text>
	                 </xsl:attribute>
	                 <xsl:element namespace="{$outputNS}" name="{$hiName}">
		                   <xsl:attribute name="{$rendName}">
		                      <xsl:text>label</xsl:text>
		                   </xsl:attribute>
		                   <xsl:text>&lt;</xsl:text>
		                   <xsl:choose>
		                      <xsl:when test="tei:content/rng:empty">
		                         <xsl:call-template name="emptySlash">
		                            <xsl:with-param name="name">
			                              <xsl:value-of select="$name"/>
		                            </xsl:with-param>
		                         </xsl:call-template>
		                      </xsl:when>
		                      <xsl:otherwise>
		                         <xsl:value-of select="$name"/>
		                      </xsl:otherwise>
		                   </xsl:choose>
		                   <xsl:text>&gt; </xsl:text>
	                 </xsl:element>
	                 <xsl:call-template name="makeDescription"/>
	                 <xsl:if test="tei:listRef">
		                   <xsl:for-each select="tei:listRef/tei:ptr">
		                      <xsl:text> </xsl:text>
		                      <xsl:apply-templates mode="weave" select="."/>
		                   </xsl:for-each>
	                 </xsl:if>
	              </xsl:element>
	           </xsl:element>
	  
	           <xsl:if test="@module">
	              <xsl:call-template name="moduleInfo"/>
	           </xsl:if>
	    
	           <xsl:variable name="myatts">
	              <a>
	                 <xsl:choose>
		                   <xsl:when test="not(tei:attList)">
		                      <xsl:call-template name="showAttClasses"/>
		                   </xsl:when>
		                   <xsl:otherwise>
		                      <xsl:for-each select="tei:attList">
		                         <xsl:call-template name="displayAttList">
		                            <xsl:with-param name="mode">all</xsl:with-param>
		                         </xsl:call-template>
		                      </xsl:for-each>
		                   </xsl:otherwise>
	                 </xsl:choose>
	              </a>
	           </xsl:variable>
	           <xsl:if test="count($myatts/a/*)&gt;0">
	              <xsl:element namespace="{$outputNS}" name="{$rowName}">
	                 <xsl:element namespace="{$outputNS}" name="{$cellName}">
		                   <xsl:attribute name="{$rendName}">
		                      <xsl:text>wovenodd-col1</xsl:text>
		                   </xsl:attribute>
		                   <xsl:element namespace="{$outputNS}" name="{$hiName}">
		                      <xsl:attribute name="{$rendName}">
		                         <xsl:text>label</xsl:text>
		                      </xsl:attribute>
		                      <xsl:attribute name="xml:lang">
		                         <xsl:value-of select="$documentationLanguage"/>
		                      </xsl:attribute>
		                      <xsl:call-template name="i18n">
		                         <xsl:with-param name="word">In addition to global attributes</xsl:with-param>
		                      </xsl:call-template>
		                   </xsl:element>
	                 </xsl:element>
	                 <xsl:element namespace="{$outputNS}" name="{$cellName}">
	                    <xsl:attribute name="{$rendName}">
		                      <xsl:text>wovenodd-col2</xsl:text>
	                    </xsl:attribute>
	                    <xsl:choose>
		                      <xsl:when test="not(tei:attList)">
		                         <xsl:call-template name="showAttClasses"/>
		                      </xsl:when>
		                      <xsl:otherwise>
		                         <xsl:for-each select="tei:attList">
		                            <xsl:call-template name="displayAttList">
		                               <xsl:with-param name="mode">all</xsl:with-param>
		                            </xsl:call-template>
		                         </xsl:for-each>
		                      </xsl:otherwise>
	                    </xsl:choose>
                     <!--
	      <xsl:for-each select="$myatts/a">
		<xsl:copy-of select="*|text()"/>
	      </xsl:for-each>
-->
	      </xsl:element>
	              </xsl:element>
	           </xsl:if>
	           <xsl:element namespace="{$outputNS}" name="{$rowName}">
	              <xsl:element namespace="{$outputNS}" name="{$cellName}">
	                 <xsl:attribute name="{$rendName}">
		                   <xsl:text>wovenodd-col1</xsl:text>
	                 </xsl:attribute>
	                 <xsl:element namespace="{$outputNS}" name="{$hiName}">
		                   <xsl:attribute name="{$rendName}">
		                      <xsl:text>label</xsl:text>
		                   </xsl:attribute>
		                   <xsl:attribute name="xml:lang">
		                      <xsl:value-of select="$documentationLanguage"/>
		                   </xsl:attribute>
		                   <xsl:call-template name="i18n">
		                      <xsl:with-param name="word">Used by</xsl:with-param>
		                   </xsl:call-template>
	                 </xsl:element>
	              </xsl:element>
	              <xsl:element namespace="{$outputNS}" name="{$cellName}">
	                 <xsl:attribute name="{$rendName}">
		                   <xsl:text>wovenodd-col2</xsl:text>
	                 </xsl:attribute>
	                 <xsl:call-template name="generateParents"/>
	              </xsl:element>
	           </xsl:element>
	  
	           <xsl:element namespace="{$outputNS}" name="{$rowName}">
	              <xsl:element namespace="{$outputNS}" name="{$cellName}">
	                 <xsl:attribute name="{$rendName}">
		                   <xsl:text>wovenodd-col1</xsl:text>
	                 </xsl:attribute>
	                 <xsl:element namespace="{$outputNS}" name="{$hiName}">
		                   <xsl:attribute name="{$rendName}">
		                      <xsl:text>label</xsl:text>
		                   </xsl:attribute>
		                   <xsl:attribute name="xml:lang">
		                      <xsl:value-of select="$documentationLanguage"/>
		                   </xsl:attribute>
		                   <xsl:call-template name="i18n">
		                      <xsl:with-param name="word">
		                         <xsl:text>May contain</xsl:text>
		                      </xsl:with-param>
		                   </xsl:call-template>
	                 </xsl:element>     
	              </xsl:element>
	              <xsl:element namespace="{$outputNS}" name="{$cellName}">
	                 <xsl:attribute name="{$rendName}">
		                   <xsl:text>wovenodd-col2</xsl:text>
	                 </xsl:attribute>
	                 <xsl:call-template name="generateChildren"/>
	              </xsl:element>
	           </xsl:element>

	           <xsl:apply-templates mode="weave"/>
	  
	  
	        </xsl:element>
      </xsl:element>
  </xsl:template>
  
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element elementSpec/tei:content</desc>
   </doc>

  <xsl:template match="tei:elementSpec/tei:content" mode="weave">
      <xsl:variable name="name">
         <xsl:choose>
	           <xsl:when test="../tei:altIdent">
	              <xsl:value-of select="../tei:altIdent"/>
	           </xsl:when>
	           <xsl:otherwise>
	              <xsl:value-of select="../@ident"/>
	           </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>
      <xsl:element namespace="{$outputNS}" name="{$rowName}">
         <xsl:element namespace="{$outputNS}" name="{$cellName}">
	           <xsl:attribute name="{$rendName}">
	              <xsl:text>wovenodd-col1</xsl:text>
	           </xsl:attribute>
	           <xsl:element namespace="{$outputNS}" name="{$hiName}">
	              <xsl:attribute name="{$rendName}">
	                 <xsl:text>label</xsl:text>
	              </xsl:attribute>
	              <xsl:attribute name="xml:lang">
	                 <xsl:value-of select="$documentationLanguage"/>
	              </xsl:attribute>
	              <xsl:call-template name="i18n">
	                 <xsl:with-param name="word">Declaration</xsl:with-param>
	              </xsl:call-template>
	           </xsl:element>
         </xsl:element>
         <xsl:element namespace="{$outputNS}" name="{$cellName}">
	           <xsl:attribute name="{$rendName}">
	              <xsl:text>wovenodd-col2</xsl:text>
	           </xsl:attribute>
	           <xsl:call-template name="bitOut">
	              <xsl:with-param name="grammar"/>
	              <xsl:with-param name="content">
	                 <Wrapper>
	                    <rng:element name="{$name}">
		                      <xsl:if test="not(key('SCHEMASPECS',1))">
		                         <rng:ref name="att.global.attributes"/>
		                         <xsl:for-each select="..">
		                            <xsl:call-template name="showClassAtts"/>
		                         </xsl:for-each>
		                      </xsl:if>
		                      <xsl:apply-templates mode="tangle" select="../tei:attList"/>
		                      <xsl:copy-of select="rng:*"/>
	                    </rng:element>
	                 </Wrapper>
	              </xsl:with-param>
	           </xsl:call-template>
	           <xsl:for-each select="tei:valList[@type='closed']">
	              <xsl:call-template name="i18n">
	                 <xsl:with-param name="word">Legal values are</xsl:with-param>
	              </xsl:call-template>
	              <xsl:text>:</xsl:text>
	              <xsl:call-template name="valListChildren"/>
	           </xsl:for-each>
	           <xsl:if test="s:*">
	              <xsl:element namespace="{$outputNS}" name="{$divName}">
	                 <xsl:attribute name="{$rendName}">
	                    <xsl:text>pre</xsl:text>
	                 </xsl:attribute>
	                 <xsl:apply-templates select="s:*" mode="verbatim"/>
	              </xsl:element>
	           </xsl:if>
         </xsl:element>
      </xsl:element>
  </xsl:template>

  <xsl:template match="tei:elementSpec/tei:constraintSpec" mode="weave">
      <xsl:element namespace="{$outputNS}" name="{$rowName}">
         <xsl:element namespace="{$outputNS}" name="{$cellName}">
	           <xsl:attribute name="{$rendName}">
	              <xsl:text>wovenodd-col1</xsl:text>
	           </xsl:attribute>
	           <xsl:element namespace="{$outputNS}" name="{$hiName}">
	              <xsl:attribute name="{$rendName}">
	                 <xsl:text>label</xsl:text>
	              </xsl:attribute>
	              <xsl:attribute name="xml:lang">
	                 <xsl:value-of select="$documentationLanguage"/>
	              </xsl:attribute>
		      <xsl:text>Schematron</xsl:text>
	           </xsl:element>
         </xsl:element>
         <xsl:element namespace="{$outputNS}" name="{$cellName}">
	           <xsl:attribute name="{$rendName}">
	              <xsl:text>wovenodd-col2</xsl:text>
	           </xsl:attribute>
	           <xsl:call-template name="makeDescription"/>
	           <xsl:for-each select="tei:constraint">
	              <xsl:element namespace="{$outputNS}" name="{$divName}">
	                 <xsl:attribute name="{$rendName}">
	                    <xsl:text>pre</xsl:text>
	                 </xsl:attribute>
	                 <xsl:apply-templates mode="verbatim"/>
	              </xsl:element>
	           </xsl:for-each>
         </xsl:element>
      </xsl:element>
  </xsl:template>
  <xsl:template name="showClassAtts">
      <xsl:for-each select="tei:classes/tei:memberOf">
         <xsl:for-each select="key('IDENTS',@key)">
	           <xsl:if test="tei:attList">
	              <rng:ref name="{@ident}.attributes"/>
	           </xsl:if>
	           <xsl:call-template name="showClassAtts"/>
         </xsl:for-each>
      </xsl:for-each>
  </xsl:template>
  
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process the specification elements elements, classes and macros<param name="atts">attributes we have been asked to display</param>
      </desc>
   </doc>
  <xsl:template match="tei:elementSpec|tei:classSpec|tei:macroSpec" mode="show">
      <xsl:param name="atts"/>
      <xsl:variable name="name">
         <xsl:choose>
	           <xsl:when test="tei:altIdent">
	              <xsl:value-of select="tei:altIdent"/>
	           </xsl:when>
	           <xsl:otherwise>
	              <xsl:value-of select="@ident"/>
	           </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>
      <xsl:element namespace="{$outputNS}" name="{$hiName}">
         <xsl:attribute name="{$rendName}">
	           <xsl:text>specList-</xsl:text>
	           <xsl:value-of select="local-name(.)"/>
         </xsl:attribute>
         <xsl:element namespace="{$outputNS}" name="{$xrefName}">
	           <xsl:attribute name="{$urlName}">
	              <xsl:choose>
	                 <xsl:when test="number($splitLevel)=-1">
	                    <xsl:text>#</xsl:text>
			    <xsl:value-of select="$idPrefix"/>
	                    <xsl:value-of select="$name"/>
	                 </xsl:when>
	                 <xsl:when test="$STDOUT='true'">
	                    <xsl:for-each select="key('IDENTS',$name)">
		                      <xsl:call-template name="getSpecURL">
		                         <xsl:with-param name="name">
		                            <xsl:value-of select="$name"/>
		                         </xsl:with-param>
		                         <xsl:with-param name="type">
		                            <xsl:value-of select="substring-before(local-name(),'Spec')"/>
		                         </xsl:with-param>
		                      </xsl:call-template>
	                    </xsl:for-each>
	                 </xsl:when>
	                 <xsl:otherwise>
	                    <xsl:text>ref-</xsl:text>
	                    <xsl:value-of select="$name"/>
	                    <xsl:text>.html</xsl:text>
	                 </xsl:otherwise>
	              </xsl:choose>
	           </xsl:attribute>
	           <xsl:value-of select="$name"/>
	           <xsl:for-each select="key('IDENTS',$name)">
	              <xsl:if test="tei:content/rng:empty">
	                 <xsl:text>/</xsl:text>
	              </xsl:if>
	           </xsl:for-each>
         </xsl:element>
      </xsl:element>
      <xsl:text> </xsl:text>
      <xsl:call-template name="makeDescription"/>
      <xsl:choose>
         <xsl:when test="$atts='-'"/>
         <xsl:when test="$atts='+'">
	           <xsl:call-template name="showAttClasses">
	              <xsl:with-param name="minimal">true</xsl:with-param>
	           </xsl:call-template>
         </xsl:when>
         <xsl:when test="$atts=''"/>
         <xsl:when test="string-length($atts)&gt;0">
	           <xsl:element namespace="{$outputNS}" name="{$tableName}">
	              <xsl:attribute name="{$rendName}">
	                 <xsl:text>specDesc</xsl:text>
	              </xsl:attribute>
	              <xsl:variable name="HERE" select="."/>
		      <xsl:for-each select="tokenize($atts,' ')">
			<xsl:call-template name="doAnAttToken">
			  <xsl:with-param name="HERE" select="$HERE"/>
			  <xsl:with-param name="TOKEN" select="."/>
			</xsl:call-template>
		      </xsl:for-each> 
	           </xsl:element>
         </xsl:when>
         <xsl:otherwise>
	           <xsl:if test="tei:attList//tei:attDef">
	              <xsl:element namespace="{$outputNS}" name="{$tableName}">
	                 <xsl:attribute name="{$rendName}">
	                    <xsl:text>attList</xsl:text>
	                 </xsl:attribute>
	                 <xsl:apply-templates mode="summary" select="tei:attList//tei:attDef"/>
	              </xsl:element>
	           </xsl:if>
	           <xsl:call-template name="showAttClasses">
	              <xsl:with-param name="minimal">true</xsl:with-param>
	           </xsl:call-template>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Show a selected attribute<param name="HERE">the starting node </param>
         <param name="TOKEN">attribute we have been asked to display</param>
      </desc>
   </doc>
  <xsl:template name="doAnAttToken">
      <xsl:param name="HERE"/>
      <xsl:param name="TOKEN"/>
      <xsl:choose>
         <xsl:when test="$HERE/tei:attList//tei:attDef[@ident=$TOKEN]">
	           <xsl:for-each select="$HERE/tei:attList//tei:attDef[@ident=$TOKEN]">
	              <xsl:call-template name="showAnAttribute"/>
	           </xsl:for-each>
         </xsl:when>
         <xsl:otherwise>
	           <xsl:for-each select="$HERE/tei:classes/tei:memberOf">
	              <xsl:for-each select="key('IDENTS',@key)/tei:attList//tei:attDef[@ident=$TOKEN]">
	                 <xsl:call-template name="showAnAttribute"/>
	              </xsl:for-each>
	           </xsl:for-each>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Display of an attribute</desc>
   </doc>
  <xsl:template name="showAnAttribute">
      <xsl:element namespace="{$outputNS}" name="{$rowName}">
         <xsl:element namespace="{$outputNS}" name="{$cellName}">
	           <xsl:attribute name="{$rendName}">
	              <xsl:text>Attribute</xsl:text>
	           </xsl:attribute>
	           <xsl:element namespace="{$outputNS}" name="{$hiName}">
	              <xsl:attribute name="{$rendName}">
	                 <xsl:text>att</xsl:text>
	              </xsl:attribute>
	              <xsl:choose>
	                 <xsl:when test="tei:altIdent">
	                    <xsl:value-of select="tei:altIdent"/>
	                 </xsl:when>
	                 <xsl:otherwise>
	                    <xsl:value-of select="@ident"/>
	                 </xsl:otherwise>
	              </xsl:choose>
	           </xsl:element>
         </xsl:element>
         <xsl:element namespace="{$outputNS}" name="{$cellName}">
	           <xsl:call-template name="makeDescription"/>
         </xsl:element>
      </xsl:element>
  </xsl:template>
  
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element exemplum</desc>
   </doc>
  <xsl:template match="tei:exemplum" mode="doc">
      <xsl:variable name="documentationLanguage">
         <xsl:call-template name="generateDoc"/>
      </xsl:variable>
      <xsl:choose>
         <xsl:when test="parent::tei:exemplum">
	           <xsl:call-template name="showExample"/>
         </xsl:when>
         <xsl:when test="not(@xml:lang)">
	           <xsl:call-template name="showExample"/>
         </xsl:when>
         <xsl:when test="@xml:lang='und'">
	           <xsl:call-template name="showExample"/>
         </xsl:when>
         <xsl:when test="@xml:lang='mul' and not($documentationLanguage='zh-tw')">
	   <!-- will need to generalize this if other langs come along like
		chinese -->
	   <xsl:call-template name="showExample"/>
	 </xsl:when>
         <xsl:when test="@xml:lang=$documentationLanguage">
	   <xsl:call-template name="showExample"/>
         </xsl:when>
         <xsl:when test="not(../tei:exemplum[@xml:lang=$documentationLanguage])                    and (@xml:lang='en'      or @xml:lang='und'      or @xml:lang='mul')">
	   <xsl:call-template name="showExample"/>
         </xsl:when>
      </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process an example</desc>
   </doc>
   <xsl:template name="showExample">
      <xsl:choose>
         <xsl:when test="parent::tei:attDef">
            <xsl:element namespace="{$outputNS}" name="{$rowName}">
	              <xsl:element namespace="{$outputNS}" name="{$cellName}">
	                 <xsl:attribute name="{$colspan}">
	                    <xsl:text>2</xsl:text>
	                 </xsl:attribute>
	                 <xsl:apply-templates/>
	              </xsl:element>
            </xsl:element>
         </xsl:when>
         <xsl:otherwise>
            <xsl:element namespace="{$outputNS}" name="{$rowName}">
	              <xsl:element namespace="{$outputNS}" name="{$cellName}">
	                 <xsl:attribute name="{$rendName}">
	                    <xsl:text>wovenodd-col1</xsl:text>
	                 </xsl:attribute>
	                 <xsl:element namespace="{$outputNS}" name="{$hiName}">
	                    <xsl:attribute name="{$rendName}">
	                       <xsl:text>label</xsl:text>
	                    </xsl:attribute>
	                    <xsl:attribute name="xml:lang">
	                       <xsl:value-of select="$documentationLanguage"/>
	                    </xsl:attribute>
	                    <xsl:call-template name="i18n">
	                       <xsl:with-param name="word">Example</xsl:with-param>
	                    </xsl:call-template>
	                 </xsl:element>
	              </xsl:element>
	              <xsl:element namespace="{$outputNS}" name="{$cellName}">
	                 <xsl:attribute name="{$rendName}">
	                    <xsl:text>wovenodd-col2</xsl:text>
	                 </xsl:attribute>
	                 <xsl:apply-templates/>
	              </xsl:element>
            </xsl:element>
         </xsl:otherwise>
      </xsl:choose>
   </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element item</desc>
   </doc>
  <xsl:template match="tei:item">
      <xsl:choose>
         <xsl:when test="parent::tei:list[@type='gloss'] or preceding-sibling::tei:label">
            <xsl:element namespace="{$outputNS}" name="{$ddName}">
               <xsl:apply-templates/>
            </xsl:element>
         </xsl:when>
         <xsl:when test="parent::tei:list[@type='elementlist']">
            <xsl:apply-templates/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:element namespace="{$outputNS}" name="{$itemName}">
               <xsl:apply-templates/>
            </xsl:element>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element macroSpec</desc>
   </doc>
  <xsl:template match="tei:macroSpec">
      <xsl:if test="parent::tei:specGrp">
         <xsl:element namespace="{$outputNS}" name="{$dtName}">
            <xsl:value-of select="@ident"/>
         </xsl:element>
         <xsl:element namespace="{$outputNS}" name="{$ddName}">
            <xsl:apply-templates mode="tangle" select="."/>
         </xsl:element>
      </xsl:if>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element macroSpec in weavebody mode</desc>
   </doc>
  <xsl:template match="tei:macroSpec" mode="weavebody">
      <xsl:variable name="name">
         <xsl:choose>
            <xsl:when test="tei:altIdent">
               <xsl:value-of select="tei:altIdent"/>
            </xsl:when>
            <xsl:otherwise>
               <xsl:value-of select="@ident"/>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>
      <xsl:element namespace="{$outputNS}" name="{$sectionName}">
         <xsl:call-template name="makeSectionHead">
	           <xsl:with-param name="id">
	              <xsl:value-of select="@ident"/>
	           </xsl:with-param>
	           <xsl:with-param name="name">
	              <xsl:value-of select="$name"/>
	           </xsl:with-param>
         </xsl:call-template>
         <xsl:call-template name="specHook">
            <xsl:with-param name="name">
	              <xsl:value-of select="$name"/>
            </xsl:with-param>
         </xsl:call-template>
         <xsl:element namespace="{$outputNS}" name="{$tableName}">
            <xsl:attribute name="{$rendName}">
               <xsl:text>wovenodd</xsl:text>
            </xsl:attribute>
            <xsl:element namespace="{$outputNS}" name="{$rowName}">
               <xsl:element namespace="{$outputNS}" name="{$cellName}">
                  <xsl:attribute name="{$colspan}">2</xsl:attribute>
                  <xsl:element namespace="{$outputNS}" name="{$hiName}">
                     <xsl:attribute name="{$rendName}">
                        <xsl:text>label</xsl:text>
                     </xsl:attribute>
                     <xsl:value-of select="$name"/>
                  </xsl:element>
                  <xsl:text> </xsl:text>
                  <xsl:call-template name="makeDescription"/>
                  <xsl:if test="tei:listRef">
                     <xsl:for-each select="tei:listRef/tei:ptr">
                        <xsl:text> </xsl:text>
                        <xsl:apply-templates select="." mode="weave"/>
                     </xsl:for-each>
                  </xsl:if>
               </xsl:element>
            </xsl:element>
            <xsl:if test="@module">
               <xsl:call-template name="moduleInfo"/>
            </xsl:if>
            <xsl:choose>
	              <xsl:when test="@type='pe' or @type='dt'">
	                 <xsl:element namespace="{$outputNS}" name="{$rowName}">
		                   <xsl:element namespace="{$outputNS}" name="{$cellName}">
		                      <xsl:attribute name="{$rendName}">
                           <xsl:text>wovenodd-col1</xsl:text>
                        </xsl:attribute>
                        <xsl:element namespace="{$outputNS}" name="{$hiName}">
                           <xsl:attribute name="{$rendName}">
                              <xsl:text>label</xsl:text>
                           </xsl:attribute>
		                         <xsl:attribute name="xml:lang">
		                            <xsl:value-of select="$documentationLanguage"/>
		                         </xsl:attribute>
		                         <xsl:call-template name="i18n">
		                            <xsl:with-param name="word">Used by</xsl:with-param>
		                         </xsl:call-template>
                        </xsl:element>
                     </xsl:element>
                     <xsl:element namespace="{$outputNS}" name="{$cellName}">
                        <xsl:attribute name="{$rendName}">
                           <xsl:text>wovenodd-col2</xsl:text>
                        </xsl:attribute>
                        <xsl:call-template name="generateParents"/>
                     </xsl:element>
                  </xsl:element>
	              </xsl:when>
	           </xsl:choose>
            <xsl:apply-templates mode="weave"/>
        </xsl:element>
      </xsl:element>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element macroSpec/tei:content</desc>
   </doc>
  <xsl:template match="tei:macroSpec/tei:content" mode="weave">
      <xsl:element namespace="{$outputNS}" name="{$rowName}">
         <xsl:element namespace="{$outputNS}" name="{$cellName}">
            <xsl:attribute name="{$rendName}">
               <xsl:text>wovenodd-col1</xsl:text>
            </xsl:attribute>
            <xsl:element namespace="{$outputNS}" name="{$hiName}">
               <xsl:attribute name="{$rendName}">
                  <xsl:text>label</xsl:text>
               </xsl:attribute>
	              <xsl:attribute name="xml:lang">
	                 <xsl:value-of select="$documentationLanguage"/>
	              </xsl:attribute>
               <xsl:call-template name="i18n">
                  <xsl:with-param name="word">Declaration</xsl:with-param>
               </xsl:call-template>
            </xsl:element>
         </xsl:element>
         <xsl:element namespace="{$outputNS}" name="{$cellName}">
            <xsl:attribute name="{$rendName}">
               <xsl:text>wovenodd-col2</xsl:text>
            </xsl:attribute>
            <xsl:call-template name="bitOut">
               <xsl:with-param name="grammar">true</xsl:with-param>
               <xsl:with-param name="content">
                  <Wrapper>
                     <xsl:variable name="entCont">
                        <Stuff>
                           <xsl:apply-templates select="rng:*"/>
                        </Stuff>
                     </xsl:variable>
                     <xsl:variable name="entCount">
                        <xsl:for-each select="$entCont/html:Stuff">
                           <xsl:value-of select="count(*)"/>
                        </xsl:for-each>
                     </xsl:variable>
                     <xsl:choose>
                        <xsl:when test=".=&#34;TEI.singleBase&#34;"/>
                        <xsl:otherwise>
                           <rng:define name="{../@ident}">
                              <xsl:if test="starts-with(.,'component')">
                                 <xsl:attribute name="combine">choice</xsl:attribute>
                              </xsl:if>
                              <xsl:copy-of select="rng:*"/>
                           </rng:define>
                        </xsl:otherwise>
                     </xsl:choose>
                  </Wrapper>
               </xsl:with-param>
            </xsl:call-template>
         </xsl:element>
      </xsl:element>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element moduleSpec</desc>
   </doc>
  <xsl:template match="tei:moduleSpec">
      <xsl:element namespace="{$outputNS}" name="{$dlName}">
         <xsl:attribute name="{$rendName}">
            <xsl:text>moduleSpec</xsl:text>
         </xsl:attribute>
         <xsl:element namespace="{$outputNS}" name="{$labelName}">
            <xsl:attribute name="{$rendName}">
               <xsl:text>moduleSpecHead</xsl:text>
            </xsl:attribute>
	           <xsl:element namespace="{$outputNS}" name="{$segName}">
	              <xsl:attribute name="xml:lang">
	                 <xsl:value-of select="$documentationLanguage"/>
	              </xsl:attribute>
	              <xsl:call-template name="i18n">
	                 <xsl:with-param name="word">Module</xsl:with-param>
	              </xsl:call-template>
	           </xsl:element>
	           <xsl:value-of select="$spaceCharacter"/>
            <xsl:value-of select="@ident"/>
            <xsl:text>: </xsl:text>
            <xsl:call-template name="makeDescription"/>
         </xsl:element>
         <xsl:element namespace="{$outputNS}" name="{$ddName}">
            <xsl:element namespace="{$outputNS}" name="{$ulName}">
               <xsl:if test="key('ElementModule',@ident)">
	                 <xsl:element namespace="{$outputNS}" name="{$itemName}">
	                    <xsl:element namespace="{$outputNS}" name="{$hiName}">
		                      <xsl:attribute name="xml:lang">
		                         <xsl:value-of select="$documentationLanguage"/>
		                      </xsl:attribute>
		                      <xsl:call-template name="i18n">
		                         <xsl:with-param name="word">Elements defined</xsl:with-param>
		                      </xsl:call-template>
	                    </xsl:element>
                     <xsl:text>: </xsl:text>
	                    <xsl:variable name="list">
		                      <List>
		                         <xsl:for-each select="key('ElementModule',@ident)">
		                            <xsl:sort select="@ident"/>
		                            <Item>
		                               <xsl:call-template name="linkTogether">
		                                  <xsl:with-param name="name" select="@ident"/>
		                               </xsl:call-template>
		                            </Item>
		                         </xsl:for-each>
		                      </List>
	                    </xsl:variable>
	                    <xsl:for-each select="$list/List/Item">
		                      <xsl:copy-of select="*|text()"/>
		                      <xsl:if test="following-sibling::Item">
		                         <xsl:call-template name="showSpaceBetweenItems"/>
		                      </xsl:if>
	                    </xsl:for-each>
                  </xsl:element>
               </xsl:if>
               <xsl:if test="key('ClassModule',@ident)">
                  <xsl:element namespace="{$outputNS}" name="{$itemName}">
	                    <xsl:element namespace="{$outputNS}" name="{$hiName}">
		                      <xsl:attribute name="xml:lang">
		                         <xsl:value-of select="$documentationLanguage"/>
		                      </xsl:attribute>
		                      <xsl:call-template name="i18n">
		                         <xsl:with-param name="word">Classes defined</xsl:with-param>
		                      </xsl:call-template>
		                   </xsl:element>
                     <xsl:text>: </xsl:text>
	                    <xsl:variable name="list">
		                      <List>
  		                       <xsl:for-each select="key('ClassModule',@ident)">
		                            <xsl:sort select="@ident"/>
		                            <Item>
		                               <xsl:call-template name="linkTogether">
			                                 <xsl:with-param name="name" select="@ident"/>
		                               </xsl:call-template>
		                            </Item>
		                         </xsl:for-each>
		                      </List>
	                    </xsl:variable>
	                    <xsl:for-each select="$list/List/Item">
		                      <xsl:copy-of select="*|text()"/>
		                      <xsl:if test="following-sibling::Item">
		                         <xsl:call-template name="showSpaceBetweenItems"/>
		                      </xsl:if>
	                    </xsl:for-each>
                  </xsl:element>
               </xsl:if>
               <xsl:if test="key('MacroModule',@ident)">
                  <xsl:element namespace="{$outputNS}" name="{$itemName}">
	                    <xsl:element namespace="{$outputNS}" name="{$segName}">
		                      <xsl:attribute name="xml:lang">
		                         <xsl:value-of select="$documentationLanguage"/>
		                      </xsl:attribute>
		                      <xsl:call-template name="i18n">
		                         <xsl:with-param name="word">Macros defined</xsl:with-param>
		                      </xsl:call-template>
	                    </xsl:element>
	                    <xsl:text>: </xsl:text>
	                    <xsl:variable name="list">
		                      <List>
		                         <xsl:for-each select="key('MacroModule',@ident)">
		                            <xsl:sort select="@ident"/>
		                            <Item>
		                               <xsl:call-template name="linkTogether">
			                                 <xsl:with-param name="name" select="@ident"/>
		                               </xsl:call-template>
		                            </Item>
		                         </xsl:for-each>
		                      </List>
	                    </xsl:variable>
	                    <xsl:for-each select="$list/List/Item">
		                      <xsl:copy-of select="*|text()"/>
		                      <xsl:if test="following-sibling::Item">
		                         <xsl:call-template name="showSpaceBetweenItems"/>
		                      </xsl:if>
	                    </xsl:for-each>
	                 </xsl:element>
               </xsl:if>
            </xsl:element>
         </xsl:element>
      </xsl:element>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process tei:remarks</desc>
   </doc>
  <xsl:template match="tei:remarks" mode="doc">
      <xsl:if test="string-length(.)&gt;0">
         <xsl:element namespace="{$outputNS}" name="{$rowName}">
            <xsl:element namespace="{$outputNS}" name="{$cellName}">
               <xsl:attribute name="{$rendName}">
                  <xsl:text>wovenodd-col1</xsl:text>
               </xsl:attribute>
               <xsl:element namespace="{$outputNS}" name="{$hiName}">
                  <xsl:attribute name="{$rendName}">
                     <xsl:text>label</xsl:text>
                  </xsl:attribute>
	                 <xsl:attribute name="xml:lang">
	                    <xsl:value-of select="$documentationLanguage"/>
	                 </xsl:attribute>
                  <xsl:call-template name="i18n">
                     <xsl:with-param name="word">Note</xsl:with-param>
                  </xsl:call-template>
               </xsl:element>
            </xsl:element>
            <xsl:element namespace="{$outputNS}" name="{$cellName}">
               <xsl:attribute name="{$rendName}">
                  <xsl:text>wovenodd-col2</xsl:text>
               </xsl:attribute>
               <xsl:comment> </xsl:comment>
               <xsl:apply-templates/>
            </xsl:element>
         </xsl:element>
      </xsl:if>
  </xsl:template>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process a specDesc</desc>
   </doc>
  <xsl:template match="tei:specDesc">
      <xsl:element namespace="{$outputNS}" name="{$itemName}">
         <xsl:call-template name="processSpecDesc"/>
      </xsl:element>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process a specGrp</desc>
   </doc>
  <xsl:template match="tei:specGrp"/>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process  a specGrpRef</desc>
   </doc>
  <xsl:template match="tei:specGrpRef"/>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process a specList</desc>
   </doc>
  <xsl:template match="tei:specList">
      <xsl:element namespace="{$outputNS}" name="{$ulName}">
         <xsl:attribute name="{$rendName}">specList</xsl:attribute>
         <xsl:apply-templates/>
      </xsl:element>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process a valDesc</desc>
   </doc>
  <xsl:template match="tei:valDesc" mode="weave">
      <xsl:variable name="documentationLanguage">
         <xsl:call-template name="generateDoc"/>
      </xsl:variable>
      <xsl:choose>
         <xsl:when test="@xml:lang and         not(@xml:lang=$documentationLanguage)">
      </xsl:when>
         <xsl:when test="not(@xml:lang) and         not($documentationLanguage='en')          and         ../tei:valDesc[@xml:lang=$documentationLanguage]">
      </xsl:when>
         <xsl:otherwise>
	           <xsl:element namespace="{$outputNS}" name="{$rowName}">
	              <xsl:element namespace="{$outputNS}" name="{$cellName}">
	                 <xsl:attribute name="{$rendName}">
	                    <xsl:text>odd_label</xsl:text>
	                 </xsl:attribute>
	                 <xsl:element namespace="{$outputNS}" name="{$hiName}">
	                    <xsl:attribute name="{$rendName}">
		                      <xsl:text>label</xsl:text>
	                    </xsl:attribute>
	                    <xsl:attribute name="xml:lang">
		                      <xsl:value-of select="$documentationLanguage"/>
	                    </xsl:attribute>
	                    <xsl:call-template name="i18n">
		                      <xsl:with-param name="word">Values</xsl:with-param>
	                    </xsl:call-template>
	                 </xsl:element>
	                 <xsl:text> </xsl:text>
	              </xsl:element>
	              <xsl:element namespace="{$outputNS}" name="{$cellName}">
	                 <xsl:attribute name="{$rendName}">
	                    <xsl:text>attribute</xsl:text>
	                 </xsl:attribute>
	                 <xsl:apply-templates/>
	              </xsl:element>
	           </xsl:element>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element val</desc>
   </doc>
  <xsl:template match="tei:val">
      <xsl:element namespace="{$outputNS}" name="{$hiName}">
         <xsl:attribute name="{$rendName}">
            <xsl:text>val</xsl:text>
         </xsl:attribute>
         <xsl:apply-templates/>
      </xsl:element>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element att</desc>
   </doc>
  <xsl:template match="tei:att">
      <xsl:element namespace="{$outputNS}" name="{$hiName}">
         <xsl:attribute name="{$rendName}">
            <xsl:text>att</xsl:text>
         </xsl:attribute>
         <xsl:apply-templates/>
      </xsl:element>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element tag</desc>
   </doc>
  <xsl:template match="tei:tag">
      <xsl:element namespace="{$outputNS}" name="{$hiName}">
         <xsl:attribute name="{$rendName}">
            <xsl:text>tag</xsl:text>
         </xsl:attribute>
         <xsl:text>&lt;</xsl:text>
         <xsl:apply-templates/>
         <xsl:text>&gt;</xsl:text>
      </xsl:element>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element valList</desc>
   </doc>
  <xsl:template match="tei:valList" mode="contents">
      <xsl:element namespace="{$outputNS}" name="{$rowName}">
         <xsl:element namespace="{$outputNS}" name="{$cellName}">
            <xsl:attribute name="{$rendName}">
               <xsl:text>odd_label</xsl:text>
            </xsl:attribute>
	           <xsl:element namespace="{$outputNS}" name="{$hiName}">
	              <xsl:attribute name="{$rendName}">
	                 <xsl:text>label</xsl:text>
	              </xsl:attribute>	  
	              <xsl:attribute name="xml:lang">
	                 <xsl:value-of select="$documentationLanguage"/>
	              </xsl:attribute>
	              <xsl:choose>
	                 <xsl:when test="@type='semi'">
                     <xsl:call-template name="i18n">
	                       <xsl:with-param name="word">Suggested values include</xsl:with-param>
	                    </xsl:call-template>
	                    <xsl:text>:</xsl:text>
	                 </xsl:when>
	                 <xsl:when test="@type='open'">
                     <xsl:call-template name="i18n">
	                       <xsl:with-param name="word">Sample values include</xsl:with-param>
	                    </xsl:call-template>
	                    <xsl:text>:</xsl:text>
	                 </xsl:when>
	                 <xsl:when test="@type='closed'">
                     <xsl:call-template name="i18n">
	                       <xsl:with-param name="word">Legal values are</xsl:with-param>
	                    </xsl:call-template>
	                    <xsl:text>:</xsl:text>
	                 </xsl:when>
	                 <xsl:otherwise>Sample values include</xsl:otherwise>
	              </xsl:choose>
	           </xsl:element>
	        </xsl:element>
         <xsl:element namespace="{$outputNS}" name="{$cellName}">
            <xsl:attribute name="{$rendName}">
               <xsl:text>odd_value</xsl:text>
	           </xsl:attribute>
	           <xsl:call-template name="valListChildren"/>
         </xsl:element>
      </xsl:element>
  </xsl:template>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[odds] all the values in a valList</desc>
   </doc>
  <xsl:template name="valListChildren">
      <xsl:element namespace="{$outputNS}" name="{$dlName}">
         <xsl:attribute name="{$rendName}">
	           <xsl:text>valList</xsl:text>
         </xsl:attribute>
         <xsl:for-each select="tei:valItem">
            <xsl:variable name="name">
	              <xsl:choose>
		                <xsl:when test="tei:altIdent">
		                   <xsl:value-of select="tei:altIdent"/>
		                </xsl:when>
		                <xsl:otherwise>
		                   <xsl:value-of select="@ident"/>
		                </xsl:otherwise>
	              </xsl:choose>
	           </xsl:variable>
	           <xsl:element namespace="{$outputNS}" name="{$dtName}">
	              <xsl:attribute name="{$rendName}">
		                <xsl:text>odd_label</xsl:text>
	              </xsl:attribute>
	              <xsl:value-of select="$name"/>
	           </xsl:element>
	           <xsl:element namespace="{$outputNS}" name="{$ddName}">
	              <xsl:attribute name="{$rendName}">
		                <xsl:text>odd_value</xsl:text>
	              </xsl:attribute>
	              <xsl:call-template name="makeDescription"/>
	              <xsl:if test="@ident=../../tei:defaultVal">
		                <xsl:element namespace="{$outputNS}" name="{$hiName}">
		                   <xsl:attribute name="{$rendName}">
		                      <xsl:text>defaultVal</xsl:text>
		                   </xsl:attribute>
		                   <xsl:attribute name="xml:lang">
		                      <xsl:value-of select="$documentationLanguage"/>
		                   </xsl:attribute>
		                   <xsl:text> [</xsl:text>
		                   <xsl:call-template name="i18n">
		                      <xsl:with-param name="word">Default</xsl:with-param>
		                   </xsl:call-template>
		                   <xsl:text>]</xsl:text>
		                </xsl:element>
	              </xsl:if>
              </xsl:element>
         </xsl:for-each>
      </xsl:element>
  </xsl:template>
  
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[odds] </desc>
   </doc>
  <xsl:template name="moduleInfo">
      <xsl:if test="not($oddWeaveLite='true')">
         <xsl:element namespace="{$outputNS}" name="{$rowName}">
            <xsl:element namespace="{$outputNS}" name="{$cellName}">
               <xsl:attribute name="{$rendName}">
                  <xsl:text>wovenodd-col1</xsl:text>
               </xsl:attribute>
               <xsl:element namespace="{$outputNS}" name="{$hiName}">
                  <xsl:attribute name="{$rendName}">
                     <xsl:text>label</xsl:text>
                  </xsl:attribute>
	                 <xsl:attribute name="xml:lang">
	                    <xsl:value-of select="$documentationLanguage"/>
	                 </xsl:attribute>
                  <xsl:call-template name="i18n">
                     <xsl:with-param name="word">Module</xsl:with-param>
                  </xsl:call-template>
               </xsl:element>
            </xsl:element>
            <xsl:element namespace="{$outputNS}" name="{$cellName}">
               <xsl:attribute name="{$rendName}">
                  <xsl:text>wovenodd-col2</xsl:text>
               </xsl:attribute>
               <xsl:call-template name="makeTagsetInfo"/>
            </xsl:element>
         </xsl:element>
      </xsl:if>
  </xsl:template>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>[odds] <param name="mode">mode</param>
      </desc>
   </doc>
  <xsl:template name="displayAttList">
      <xsl:param name="mode"/>
      <xsl:call-template name="showAttClasses"/>
      <xsl:if test=".//tei:attDef">
         <xsl:element namespace="{$outputNS}" name="{$tableName}">
            <xsl:attribute name="{$rendName}">
               <xsl:text>attList</xsl:text>
            </xsl:attribute>
            <xsl:choose>
               <xsl:when test="$mode='all'">
                  <xsl:apply-templates/>
               </xsl:when>
               <xsl:otherwise>
                  <xsl:apply-templates mode="summary"/>
               </xsl:otherwise>
            </xsl:choose>
         </xsl:element>
      </xsl:if>
  </xsl:template>

  <xsl:template name="generateParents">
      <xsl:element namespace="{$outputNS}" name="{$divName}">
         <xsl:attribute name="{$rendName}">parent</xsl:attribute>
         <xsl:variable name="list">
	   <List>
	     <xsl:call-template name="generateParentsByElement">
	       <xsl:with-param name="I" select="concat(@ident,'')"/>
	     </xsl:call-template>
	     <xsl:call-template name="generateParentsByElement">
	       <xsl:with-param name="I" select="concat(@ident,'_alternation')"/>
	     </xsl:call-template>
	     <xsl:call-template name="generateParentsByElement">
	       <xsl:with-param name="I" select="concat(@ident,'_sequence')"/>
	     </xsl:call-template>
	     <xsl:call-template name="generateParentsByElement">
	       <xsl:with-param name="I"
			       select="concat(@ident,'_sequenceOptional')"/>
	     </xsl:call-template>
	     <xsl:call-template name="generateParentsByElement">
	       <xsl:with-param name="I"
			       select="concat(@ident,'_sequenceOptionalRepeatable')"/>
	     </xsl:call-template>
	     <xsl:call-template name="generateParentsByElement">
	       <xsl:with-param name="I"
			       select="concat(@ident,'_sequenceRepeatable')"/>
	     </xsl:call-template>
	     <xsl:call-template name="generateParentsByMacro"/>
	     <xsl:call-template name="generateParentsByClass"/>
	   </List>
         </xsl:variable>
         <xsl:for-each select="$list/List/Item">
	   <xsl:copy-of select="*|text()"/>
	   <xsl:if test="following-sibling::Item">
	     <xsl:call-template name="showSpaceBetweenItems"/>
	   </xsl:if>
         </xsl:for-each>
         <xsl:call-template name="generateParentsByAttribute"/>
      </xsl:element>
  </xsl:template>

  <xsl:template name="generateMembers">
      <xsl:param name="depth">1</xsl:param>
      <xsl:param name="me"/>
      <xsl:variable name="this" select="@ident"/>
      <xsl:choose>
         <xsl:when test="$this=$me"/>
         <xsl:when test="key('CLASSMEMBERS',$this)">
            <xsl:element namespace="{$outputNS}" name="{$hiName}">
               <xsl:attribute name="{$rendName}">
                  <xsl:text>showmembers</xsl:text>
                  <xsl:value-of select="$depth"/>
               </xsl:attribute>
               <xsl:if test="$depth &gt; 1"> [</xsl:if>
	              <xsl:variable name="list">
	                 <List>
	                    <xsl:for-each select="key('CLASSMEMBERS',$this)">
		                      <xsl:sort select="local-name()"/>
		                      <xsl:sort select="@ident"/>
		                      <Item>
		                         <xsl:variable name="cl">
		                            <xsl:choose>
		                               <xsl:when test="self::tei:elementSpec">
			                                 <xsl:text>link_odd_element</xsl:text>
		                               </xsl:when>
		                               <xsl:when test="self::tei:classSpec">
			                                 <xsl:text>link_odd_class</xsl:text>
		                               </xsl:when>
		                            </xsl:choose>
		                         </xsl:variable>
		                         <xsl:call-template name="linkTogether">
		                            <xsl:with-param name="name" select="@ident"/>
		                            <xsl:with-param name="class">
		                               <xsl:value-of select="$cl"/>
		                            </xsl:with-param>
		                         </xsl:call-template>
		                         <xsl:call-template name="generateMembers">
		                            <xsl:with-param name="depth">
		                               <xsl:value-of select="$depth + 1"/>
		                            </xsl:with-param>
		                         </xsl:call-template>
		                      </Item>
	                    </xsl:for-each>
	                 </List>
	              </xsl:variable>
	              <xsl:for-each select="$list/List/Item">
	                 <xsl:copy-of select="*|text()"/>
	                 <xsl:if test="following-sibling::Item">
	                    <xsl:call-template name="showSpaceBetweenItems"/>
	                 </xsl:if>
	              </xsl:for-each>
               <xsl:if test="$depth &gt; 1">] </xsl:if>
            </xsl:element>
         </xsl:when>
         <xsl:when test="$lookupDatabase='true'">
            <xsl:choose>
               <xsl:when test="not($localsource='')">
                  <xsl:for-each select="document($localsource)/tei:TEI">
	                    <xsl:variable name="list">
		                      <List>
		  
		                         <xsl:for-each select="tei:elementSpec[tei:classes/tei:memberOf[@key=$this]]">
		                            <Item>
		                               <xsl:call-template name="showElement">
			                                 <xsl:with-param name="name" select="@ident"/>
		                               </xsl:call-template>
		                            </Item>
		                         </xsl:for-each>
		                      </List>
	                    </xsl:variable>
	                    <xsl:for-each select="$list/List/Item">
		                      <xsl:copy-of select="*|text()"/>
		                      <xsl:if test="following-sibling::Item">
		                         <xsl:call-template name="showSpaceBetweenItems"/>
		                      </xsl:if>
	                    </xsl:for-each>
                  </xsl:for-each>
               </xsl:when>
               <xsl:otherwise>
                  <xsl:variable name="address">
                     <xsl:value-of select="$TEISERVER"/>
                     <xsl:text>classmembers.xql?class=</xsl:text>
                     <xsl:value-of select="@ident"/>
                  </xsl:variable>
                  <xsl:if test="$verbose='true'">
                     <xsl:message>Accessing TEISERVER: <xsl:value-of select="$address"/>
                     </xsl:message>
                  </xsl:if>
                  <xsl:for-each select="document($address)/list/item">
                     <xsl:call-template name="showElement">
                        <xsl:with-param name="name" select="."/>
                     </xsl:call-template>
                     <xsl:if test="following::item">
                        <xsl:text> 
</xsl:text>
                     </xsl:if>
                  </xsl:for-each>
               </xsl:otherwise>
            </xsl:choose>
         </xsl:when>
      </xsl:choose>
  </xsl:template>


  <xsl:template name="generateParentsByElement">
    <xsl:param name="I"/>
    <xsl:for-each select="key('REFS',$I)/ancestor::tei:elementSpec">
      <xsl:sort select="@ident"/>
      <Item>
	<xsl:call-template name="linkTogether">
	  <xsl:with-param name="name" select="@ident"/>
	  <xsl:with-param name="class">link_odd_element</xsl:with-param>
	</xsl:call-template>
      </Item>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="generateParentsByAttribute">
      <xsl:variable name="this" select="@ident"/>
      <xsl:if test="count(key('ATTREFS-CLASS',$this))&gt;0">
	<!--
	<xsl:message>example in <xsl:value-of select="$this"/></xsl:message>
	-->
         <xsl:element namespace="{$outputNS}" name="{$segName}">
	           <xsl:attribute name="xml:lang">
	              <xsl:value-of select="$documentationLanguage"/>
	           </xsl:attribute>
	           <xsl:call-template name="i18n">
	              <xsl:with-param name="word">
	                 <xsl:text> Class</xsl:text>
	              </xsl:with-param>
	           </xsl:call-template>
         </xsl:element>
         <xsl:text>: </xsl:text>
         <xsl:element namespace="{$outputNS}" name="{$ulName}">
	           <xsl:variable name="list">
	              <List>
	                 <xsl:for-each select="key('ATTREFS-CLASS',$this)">
	                    <xsl:sort select="ancestor::tei:classSpec/@ident"/>
	                    <xsl:sort select="@ident"/>
	                    <xsl:element namespace="{$outputNS}" name="{$itemName}">
		                      <xsl:for-each select="ancestor::tei:classSpec">
		                         <Item>
		                            <xsl:call-template name="linkTogether">
		                               <xsl:with-param name="name">
			                                 <xsl:value-of select="@ident"/>
		                               </xsl:with-param>
		                               <xsl:with-param name="class">
			                                 <xsl:text>link_odd_class</xsl:text>
		                               </xsl:with-param>
		                            </xsl:call-template>
		                         </Item>
		                      </xsl:for-each>
		                      <xsl:text>/@</xsl:text>
		                      <xsl:value-of select="ancestor::tei:attDef/@ident"/>
	                    </xsl:element>
	                 </xsl:for-each>
	              </List>
	           </xsl:variable>
	           <xsl:for-each select="$list/List/Item">
	              <xsl:copy-of select="*|text()"/>
	              <xsl:if test="following-sibling::Item">
	                 <xsl:call-template name="showSpaceBetweenItems"/>
	              </xsl:if>
	           </xsl:for-each>
         </xsl:element>
      </xsl:if>

      <xsl:if test="count(key('ATTREFS-ELEMENT',$this))&gt;0">
         <xsl:element namespace="{$outputNS}" name="{$segName}">
	           <xsl:attribute name="xml:lang">
	              <xsl:value-of select="$documentationLanguage"/>
	           </xsl:attribute>
	           <xsl:call-template name="i18n">
	              <xsl:with-param name="word">
	                 <xsl:text>Element</xsl:text>
	              </xsl:with-param>
	           </xsl:call-template>
         </xsl:element>
         <xsl:text>: </xsl:text>
         <xsl:element namespace="{$outputNS}" name="{$ulName}">
	           <xsl:variable name="list">
	              <List>
	                 <xsl:for-each select="key('ATTREFS-ELEMENT',$this)">
	                    <xsl:sort select="ancestor::tei:elementSpec/@ident"/>
	                    <xsl:sort select="@ident"/>
	                    <Item>
		                      <xsl:element namespace="{$outputNS}" name="{$itemName}">
		                         <xsl:for-each select="ancestor::tei:elementSpec">
		                            <xsl:call-template name="linkTogether">
		                               <xsl:with-param name="name">
			                                 <xsl:value-of select="@ident"/>
		                               </xsl:with-param>
		                               <xsl:with-param name="class">
			                                 <xsl:text>link_odd_element</xsl:text>
		                               </xsl:with-param>
		                            </xsl:call-template>
		                         </xsl:for-each>
		                         <xsl:text>/@</xsl:text>
	                          <xsl:value-of select="ancestor::tei:attDef/@ident"/>
		                      </xsl:element>
	                    </Item>
	                 </xsl:for-each>
	              </List>
	           </xsl:variable>
	           <xsl:for-each select="$list/List/Item">
	              <xsl:copy-of select="*|text()"/>
	              <xsl:if test="following-sibling::Item">
	                 <xsl:call-template name="showSpaceBetweenItems"/>
	              </xsl:if>
	           </xsl:for-each>
         </xsl:element>
      </xsl:if>
  </xsl:template>

  <xsl:template name="generateParentsByMacro">
      <xsl:variable name="this" select="@ident"/>
      <xsl:if test="key('MACROREFS',$this)">
         <xsl:for-each select="key('MACROREFS',$this)/ancestor::tei:macroSpec">
	           <xsl:sort select="@ident"/>
	           <Item>
	              <xsl:call-template name="linkTogether">
	                 <xsl:with-param name="name" select="@ident"/>
	                 <xsl:with-param name="class">link_odd_macro</xsl:with-param>
	              </xsl:call-template>
	           </Item>
         </xsl:for-each>
      </xsl:if>
      <!--
      <xsl:for-each select="key('REFS',@ident)/ancestor::tei:elementSpec">
	<xsl:call-template name="linkTogether">
	  <xsl:with-param name="name" select="@ident"/>
	</xsl:call-template>
      </xsl:for-each>
      <xsl:call-template name="generateParentsByMacro"/>
-->
  </xsl:template>

  <xsl:template name="generateParentsByClass">
      <xsl:variable name="this" select="@ident"/>
      <xsl:for-each select="tei:classes/tei:memberOf">
         <xsl:for-each select="key('CLASSES',@key)">
	           <xsl:sort select="@ident"/>
	           <Item>
	              <xsl:if test="@type='model'">
	                 <xsl:call-template name="linkTogether">
	                    <xsl:with-param name="name" select="@ident"/>
	                    <xsl:with-param name="class">link_odd_class</xsl:with-param>
	                 </xsl:call-template>
	              </xsl:if>
	              <!--
	      <xsl:for-each select="key('REFS',@ident)/ancestor::tei:elementSpec">
	      <xsl:call-template name="linkTogether">
	      <xsl:with-param name="name" select="@ident"/>
	      </xsl:call-template>
	      </xsl:for-each>
	      <xsl:call-template name="generateParentsByClass"/>
	      <xsl:call-template name="generateParentsByMacro"/>
	  -->
	</Item>
         </xsl:for-each>
      </xsl:for-each>
  </xsl:template>


   <xsl:template match="tei:classSpec|tei:elementSpec|tei:macroSpec" mode="weave">
      <xsl:call-template name="refdoc"/>
   </xsl:template>


   <xsl:template match="tei:divGen[@type='modelclasscat']">
      <xsl:apply-templates mode="weave" select="key('MODELCLASSDOCS',1)">
         <xsl:sort select="@ident"/>
      </xsl:apply-templates>
  </xsl:template>

   <xsl:template match="tei:divGen[@type='attclasscat']">
      <xsl:apply-templates mode="weave" select="key('ATTCLASSDOCS',1)">
         <xsl:sort select="@ident"/>
      </xsl:apply-templates>
  </xsl:template>


  <xsl:template match="tei:divGen[@type='macrocat']">
      <xsl:apply-templates mode="weave" select="key('MACRODOCS',1)">
         <xsl:sort select="@ident"/>
      </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="tei:divGen[@type='elementcat']">
      <xsl:apply-templates mode="weave" select="key('ELEMENTDOCS',1)">
         <xsl:sort select="@ident"/>
      </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="tei:divGen[@type='attcat']">
      <xsl:element namespace="{$outputNS}" name="{$tableName}">
         <xsl:attribute name="{$rendName}">
	           <xsl:text>attcat</xsl:text>
         </xsl:attribute>
         <xsl:for-each select="key('ATTDOCS',1)">
	           <xsl:sort select="@ident"/>
	           <xsl:variable name="this" select="@ident"/>
	           <xsl:if test="generate-id()=generate-id(key('ATTRIBUTES',$this)[1])">
	              <xsl:element namespace="{$outputNS}" name="{$rowName}">
	                 <xsl:call-template name="identifyElement">
	                    <xsl:with-param name="id">
		                      <xsl:value-of select="$this"/>
	                    </xsl:with-param>
	                 </xsl:call-template>
	                 <xsl:element namespace="{$outputNS}" name="{$cellName}">
	                    <xsl:attribute name="{$rendName}">
	                       <xsl:text>attcat-col1</xsl:text>
	                    </xsl:attribute>
	                    <xsl:value-of select="$this"/>
	                 </xsl:element>
	                 <xsl:element namespace="{$outputNS}" name="{$cellName}">
	                    <xsl:attribute name="{$rendName}">
		                      <xsl:text>attcat-col2</xsl:text>
	                    </xsl:attribute>
	                    <xsl:for-each select="key('ATTRIBUTES-CLASS',$this)">
	                       <xsl:sort select="ancestor::tei:classSpec/@ident"/>
	                       <xsl:for-each select="ancestor::tei:classSpec|ancestor::elementSpec">
		                         <xsl:call-template name="linkTogether">
		                            <xsl:with-param name="name">
		                               <xsl:value-of select="@ident"/>
		                            </xsl:with-param>
		                            <xsl:with-param name="class">
		                               <xsl:text>link_odd</xsl:text>
		                            </xsl:with-param>
		                         </xsl:call-template>
	                       </xsl:for-each>
	                       <xsl:text> </xsl:text>
	                    </xsl:for-each>
	                    <xsl:for-each select="key('ATTRIBUTES-ELEMENT',$this)">
	                       <xsl:sort select="ancestor::tei:elementSpec/@ident"/>
	                       <xsl:for-each select="ancestor::tei:elementSpec">
		                         <xsl:call-template name="linkTogether">
		                            <xsl:with-param name="name">
		                               <xsl:value-of select="@ident"/>
		                            </xsl:with-param>
		                            <xsl:with-param name="class">
		                               <xsl:text>link_odd</xsl:text>
		                            </xsl:with-param>
		                         </xsl:call-template>
	                       </xsl:for-each>
	                       <xsl:value-of select="$spaceCharacter"/>
	                       <xsl:text> </xsl:text>
	                    </xsl:for-each>
	                 </xsl:element>
	              </xsl:element>
	           </xsl:if>
         </xsl:for-each>
      </xsl:element>
  </xsl:template>


  <xsl:template match="tei:exemplum" mode="weave">
      <xsl:if test="teix:egXML/* or teix:egXML/text() or text()">
         <xsl:apply-templates select="." mode="doc"/>
      </xsl:if>
  </xsl:template>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>No-op processing of elements tei:gloss and tei:desc in
    normal modes, as they will always be called explicitly if
    needed.</desc>
   </doc>

  <xsl:template match="tei:desc|tei:gloss" mode="weave"/>

  <xsl:template match="tei:remarks" mode="weave">
      <xsl:variable name="documentationLanguage">
         <xsl:call-template name="generateDoc"/>
      </xsl:variable>
      <xsl:variable name="langs">
         <xsl:value-of select="concat(normalize-space($documentationLanguage),' ')"/>
      </xsl:variable>
      <xsl:variable name="firstLang">
         <xsl:value-of select="substring-before($langs,' ')"/>
      </xsl:variable>
      <xsl:choose>
         <xsl:when test="@xml:lang=$firstLang">
	           <xsl:apply-templates select="." mode="doc"/>
         </xsl:when>
         <xsl:when test="not(@xml:lang) and $documentationLanguage='en'">
	           <xsl:apply-templates select="." mode="doc"/>
         </xsl:when>
         <xsl:otherwise>
	           <xsl:variable name="currentLang">
	              <xsl:call-template name="findLanguage"/>
	           </xsl:variable>
	           <xsl:if test="contains($langs,concat($currentLang,' '))">
	              <xsl:apply-templates select="." mode="doc"/>
	           </xsl:if>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element valList</desc>
   </doc>
  <xsl:template match="tei:valList" mode="weave">
      <xsl:apply-templates mode="contents" select="."/>
  </xsl:template>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element attList</desc>
   </doc>
  <xsl:template match="tei:attList" mode="weave"/>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element defaultVal</desc>
   </doc>
  <xsl:template match="tei:defaultVal" mode="weave"/>

  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element desc</desc>
   </doc>
  <xsl:template match="tei:desc">
      <xsl:apply-templates/>
  </xsl:template>

  <!-- pretty printing of RNC -->
  <xsl:template match="nc" mode="keep">
      <xsl:call-template name="showRNC">
         <xsl:with-param name="style">
            <xsl:text>rnc_nc</xsl:text>
         </xsl:with-param>
         <xsl:with-param name="contents">
	   <xsl:value-of select="."/>
         </xsl:with-param>
      </xsl:call-template>
  </xsl:template>

  <xsl:template match="declaration" mode="keep">
      <xsl:call-template name="showRNC">
         <xsl:with-param name="style">
            <xsl:text>rnc_decl</xsl:text>
         </xsl:with-param>
         <xsl:with-param name="contents">
	   <xsl:value-of select="."/>
         </xsl:with-param>
      </xsl:call-template>
  </xsl:template>

  <xsl:template match="prefix" mode="keep">
      <xsl:call-template name="showRNC">
         <xsl:with-param name="style">
            <xsl:text>rnc_prefix</xsl:text>
         </xsl:with-param>
         <xsl:with-param name="contents">
	   <xsl:value-of select="."/>
         </xsl:with-param>
      </xsl:call-template>
  </xsl:template>

  <xsl:template match="param" mode="keep">
      <xsl:call-template name="showRNC">
         <xsl:with-param name="style">
            <xsl:text>rnc_param</xsl:text>
         </xsl:with-param>
         <xsl:with-param name="contents">
	           <xsl:value-of select="."/>
         </xsl:with-param>
      </xsl:call-template>
  </xsl:template>

 
  <xsl:template match="op" mode="keep">
      <xsl:value-of select="translate (., ' ', ' ')"/>
  </xsl:template>

  <xsl:template match="atom" mode="keep">
      <xsl:call-template name="showRNC">
         <xsl:with-param name="style">
            <xsl:text>rnc_atom</xsl:text>
         </xsl:with-param>
         <xsl:with-param name="contents">
	           <xsl:value-of select="."/>
         </xsl:with-param>
      </xsl:call-template>
  </xsl:template>

  <xsl:template match="t" mode="keep">
      <xsl:choose>
         <xsl:when test=". = '[' or . = ']'">
	           <xsl:call-template name="showRNC">
	              <xsl:with-param name="style">
	                 <xsl:text>rnc_annot</xsl:text>
	              </xsl:with-param>
	              <xsl:with-param name="contents">
	                 <xsl:value-of select="."/>
	              </xsl:with-param>
	           </xsl:call-template>
	        </xsl:when>
         <xsl:otherwise>
            <xsl:value-of select="."/>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>

  <xsl:template match="doc" mode="keep">
      <xsl:call-template name="showRNC">
         <xsl:with-param name="style">
            <xsl:text>rnc_comment</xsl:text>
         </xsl:with-param>
         <xsl:with-param name="contents">
	           <xsl:value-of select="."/>
         </xsl:with-param>
      </xsl:call-template>
  </xsl:template>

  <xsl:template match="annot" mode="keep">
      <xsl:call-template name="showRNC">
         <xsl:with-param name="style">
            <xsl:text>rnc_annot</xsl:text>
         </xsl:with-param>
         <xsl:with-param name="contents">
	           <xsl:value-of select="."/>
         </xsl:with-param>
      </xsl:call-template>
  </xsl:template>

  <xsl:template match="type" mode="keep">
      <xsl:call-template name="showRNC">
         <xsl:with-param name="style">
            <xsl:text>rnc_type</xsl:text>
         </xsl:with-param>
         <xsl:with-param name="contents">
	           <xsl:value-of select="."/>
         </xsl:with-param>
      </xsl:call-template>
  </xsl:template>

  <xsl:template match="keyword" mode="keep">
      <xsl:call-template name="showRNC">
         <xsl:with-param name="style">
            <xsl:text>rnc_keyword</xsl:text>
         </xsl:with-param>
         <xsl:with-param name="contents">
	           <xsl:value-of select="."/>
         </xsl:with-param>
      </xsl:call-template>
  </xsl:template>

  <xsl:template match="tei:attList[@org='choice']">
      <xsl:apply-templates/>
  </xsl:template>


   <xsl:template name="showAttClasses">
      <xsl:param name="minimal">false</xsl:param>
      <xsl:variable name="clatts">
         <xsl:for-each select="ancestor-or-self::tei:elementSpec|ancestor-or-self::tei:classSpec">
            <xsl:call-template name="attClassDetails"/>
         </xsl:for-each>
      </xsl:variable>
      <xsl:if test="$minimal='true'">
         <xsl:text> — </xsl:text>
      </xsl:if>
      <xsl:choose>
         <xsl:when test="$minimal='true' and not($clatts='')">
	           <xsl:text> [+ </xsl:text>
	           <xsl:copy-of select="$clatts"/>
	           <xsl:text>]</xsl:text>
         </xsl:when>
         <xsl:when test="not($clatts='')">
	           <xsl:if test="ancestor::tei:schemaSpec and key('CLASSES','att.global')">
	              <xsl:element namespace="{$outputNS}" name="{$segName}">
	                 <xsl:attribute name="xml:lang">
	                    <xsl:value-of select="$documentationLanguage"/>
	                 </xsl:attribute>
	                 <xsl:call-template name="i18n">
	                    <xsl:with-param name="word">
		                      <xsl:choose>
		                         <xsl:when test=".//tei:attDef">
		                            <xsl:call-template name="i18n">
		                               <xsl:with-param name="word">In addition to global attributes and those inherited from</xsl:with-param>
		                            </xsl:call-template>
		                            <xsl:value-of select="$spaceCharacter"/>
		                         </xsl:when>
		                         <xsl:otherwise>
		                            <xsl:call-template name="i18n">
		                               <xsl:with-param name="word">Global attributes and those inherited from</xsl:with-param>
		                            </xsl:call-template>
		                            <xsl:value-of select="$spaceCharacter"/>
		                         </xsl:otherwise>
		                      </xsl:choose>
	                    </xsl:with-param>
	                 </xsl:call-template>
	              </xsl:element>
	           </xsl:if>
	           <xsl:copy-of select="$clatts"/>
         </xsl:when>
         <xsl:when test="ancestor::tei:schemaSpec and         not(key('CLASSES','att.global'))">
      </xsl:when>
         <xsl:otherwise>
	           <xsl:call-template name="i18n">
	              <xsl:with-param name="word">
	                 <xsl:choose>
	                    <xsl:when test=".//tei:attDef">
		                      <xsl:call-template name="i18n">
		                         <xsl:with-param name="word">In addition to global attributes</xsl:with-param>
		                      </xsl:call-template>
	                    </xsl:when>
	                    <xsl:otherwise>
		                      <xsl:call-template name="i18n">
		                         <xsl:with-param name="word">Global attributes only</xsl:with-param>
		                      </xsl:call-template>
                     </xsl:otherwise>
	                 </xsl:choose>
	              </xsl:with-param>
	           </xsl:call-template>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  
  <xsl:template name="attClassDetails">
      <xsl:param name="depth">1</xsl:param>
      <xsl:for-each select="tei:classes/tei:memberOf">
	        <xsl:choose>
	           <xsl:when test="key('CLASSES',@key)">
	              <xsl:for-each select="key('CLASSES',@key)">
	                 <xsl:if test="@type='atts'">
		                   <xsl:if test="$depth &gt; 1"> (</xsl:if>
		                   <xsl:call-template name="linkTogether">
		                      <xsl:with-param name="name" select="@ident"/>
		                   </xsl:call-template>
		                   <xsl:if test=".//tei:attDef">
		                      <xsl:text> (</xsl:text>
		                      <xsl:for-each select=".//tei:attDef">
		                         <xsl:call-template name="emphasize">
		                            <xsl:with-param name="class">attribute</xsl:with-param>
		                            <xsl:with-param name="content">
			                              <xsl:text>@</xsl:text>
			                              <xsl:choose>
			                                 <xsl:when test="tei:altIdent">
			                                    <xsl:value-of select="tei:altIdent"/>
			                                 </xsl:when>
			                                 <xsl:otherwise>
			                                    <xsl:value-of select="@ident"/>
			                                 </xsl:otherwise>
			                              </xsl:choose>
		                            </xsl:with-param>
		                         </xsl:call-template>
		                         <xsl:if test="following-sibling::tei:attDef">
		                            <xsl:text>, </xsl:text>
		                         </xsl:if>
		                      </xsl:for-each>
		                      <xsl:text>)</xsl:text>
		                      <xsl:if test="$depth=1">
		                         <xsl:call-template name="showSpace"/>
		                      </xsl:if>
		                   </xsl:if>
		                   <xsl:call-template name="attClassDetails">
		                      <xsl:with-param name="depth">
		                         <xsl:value-of select="$depth + 1"/>
		                      </xsl:with-param>
		                   </xsl:call-template>
		                   <xsl:if test="$depth &gt; 1">) </xsl:if>
	                 </xsl:if>
	              </xsl:for-each>
	           </xsl:when>
	           <xsl:when test="ancestor::tei:schemaSpec">
	  </xsl:when>
	           <xsl:otherwise>
	              <xsl:value-of select="@key"/>
	              <xsl:call-template name="showSpace"/>
	           </xsl:otherwise>
	        </xsl:choose>
      </xsl:for-each>
  </xsl:template>

  <xsl:template name="showElement">
      <xsl:param name="name"/>
      <xsl:variable name="documentationLanguage">
         <xsl:call-template name="generateDoc"/>
      </xsl:variable>
      <xsl:choose>
         <xsl:when test="$oddmode='tei'">
            <tei:ref target="#{$name}">
               <xsl:value-of select="$name"/>
            </tei:ref>
         </xsl:when>
         <xsl:when test="$oddmode='html'">
            <xsl:choose>
               <xsl:when test="key('IDENTS',$name) and number($splitLevel)=-1">
                  <a xmlns="http://www.w3.org/1999/xhtml" class="link_element" href="#{$name}">
                     <xsl:value-of select="$name"/>
                  </a>
               </xsl:when>
               <xsl:when test="key('IDENTS',$name) and $STDOUT='true'">
                  <a xmlns="http://www.w3.org/1999/xhtml" class="link_element">
	                    <xsl:attribute name="href">
		                      <xsl:call-template name="getSpecURL">
		                         <xsl:with-param name="name">
		                            <xsl:value-of select="$name"/>
		                         </xsl:with-param>
		                         <xsl:with-param name="type">
		                            <xsl:value-of select="substring-before(local-name(),'Spec')"/>
		                         </xsl:with-param>
		                      </xsl:call-template>
	                    </xsl:attribute>
	                    <xsl:value-of select="$name"/>
                  </a>
               </xsl:when>
               <xsl:when test="key('IDENTS',$name)">
                  <a xmlns="http://www.w3.org/1999/xhtml" class="link_element"
                     href="ref-{$name}{$outputSuffix}">
                     <xsl:value-of select="$name"/>
                  </a>
               </xsl:when>
               <xsl:when test="$TEIC='true'">
	                 <a xmlns="http://www.w3.org/1999/xhtml">
	                    <xsl:attribute name="href">
		                      <xsl:value-of select="$TEISERVER"/>
		                      <xsl:text>tag.xql?name=</xsl:text>
		                      <xsl:value-of select="$name"/>
		                      <xsl:text>&amp;documentationLanguage=</xsl:text>
		                      <xsl:value-of select="$documentationLanguage"/>
	                    </xsl:attribute>
                     <xsl:value-of select="$name"/>
                  </a>
	              </xsl:when>
               <xsl:otherwise>
	                 <xsl:value-of select="$name"/>
               </xsl:otherwise>
            </xsl:choose>
         </xsl:when>
         <xsl:when test="$oddmode='pdf'">
            <fo:inline font-style="italic">
               <xsl:value-of select="$name"/>
            </fo:inline>
         </xsl:when>
         <xsl:otherwise>
            <xsl:value-of select="$name"/>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>

  <xsl:template name="processSpecDesc">
      <xsl:variable name="name">
         <xsl:value-of select="@key"/>
      </xsl:variable>
      <xsl:variable name="atts">
         <xsl:choose>
            <xsl:when test="@rend='noatts'">-</xsl:when>
            <xsl:otherwise>
               <xsl:value-of select="normalize-space(@atts)"/>
            </xsl:otherwise>
         </xsl:choose>
      </xsl:variable>
      <xsl:choose>
         <xsl:when test="$name=''">
            <xsl:message>ERROR: no key attribute on specDesc</xsl:message>
         </xsl:when>
         <xsl:when test="key('IDENTS',$name)">
            <xsl:apply-templates mode="show" select="key('IDENTS',$name)">
               <xsl:with-param name="atts" select="$atts"/>
            </xsl:apply-templates>
         </xsl:when>
         <xsl:when test="not($localsource='')">
            <xsl:for-each select="document($localsource)/tei:TEI">
               <xsl:apply-templates mode="show" select="tei:*[@ident=$name]">
                  <xsl:with-param name="atts" select="$atts"/>
               </xsl:apply-templates>
            </xsl:for-each>
         </xsl:when>
         <xsl:otherwise>
            <xsl:variable name="loc">
               <xsl:value-of select="$TEISERVER"/>
               <xsl:text>copytag.xql?name=</xsl:text>
               <xsl:value-of select="$name"/>
            </xsl:variable>
            <xsl:if test="$verbose='true'">
               <xsl:message>Accessing TEISERVER: <xsl:value-of select="$loc"/>
               </xsl:message>
            </xsl:if>
            <xsl:apply-templates mode="show" select="document($loc)/tei:*">
               <xsl:with-param name="atts" select="$atts"/>
            </xsl:apply-templates>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>

  <xsl:template name="processatts">
      <xsl:param name="values"/>
      <xsl:variable name="here" select="."/>
      <xsl:for-each select="tokenize($values, ' ')">
	<xsl:variable name="v" select="."/>
	<xsl:for-each select="$here">
	  <xsl:apply-templates select="key('IDENTS',.)"/>
	</xsl:for-each>
      </xsl:for-each>
  </xsl:template>


  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element attList</desc>
   </doc>
  <xsl:template match="tei:attList" mode="show">
      <xsl:call-template name="displayAttList">
         <xsl:with-param name="mode">summary</xsl:with-param>
      </xsl:call-template>
  </xsl:template>

  <xsl:template name="makeTagsetInfo">
      <xsl:value-of select="@module"/>
      <xsl:for-each select="key('MODULES',@module)/ancestor::tei:div[last()]">
         <xsl:text> — </xsl:text>
        <xsl:call-template name="makeInternalLink">
            <xsl:with-param name="target" select="@xml:id"/>
            <xsl:with-param name="ptr">true</xsl:with-param>
            <xsl:with-param name="dest">
               <xsl:value-of select="tei:head"/>
	           </xsl:with-param>
        </xsl:call-template>
      </xsl:for-each>
  </xsl:template>


  <xsl:template name="generateChildren">
      <xsl:variable name="name" select="@ident"/>
      <xsl:variable name="Original" select="/"/>
      <xsl:choose>
	        <xsl:when test="tei:content/rng:empty">
	           <xsl:element namespace="{$outputNS}" name="{$segName}">
	              <xsl:attribute name="xml:lang">
	                 <xsl:value-of select="$documentationLanguage"/>
	              </xsl:attribute>
	              <xsl:call-template name="i18n">
	                 <xsl:with-param name="word">Empty element</xsl:with-param>
	              </xsl:call-template>
	           </xsl:element>
	        </xsl:when>
	        <xsl:when test="tei:content/rng:text and    count(tei:content/rng:*)=1">
	           <xsl:element namespace="{$outputNS}" name="{$segName}">
	              <xsl:attribute name="xml:lang">
	                 <xsl:value-of select="$documentationLanguage"/>
	              </xsl:attribute>
	              <xsl:call-template name="i18n">
	                 <xsl:with-param name="word">Character data only</xsl:with-param>
	              </xsl:call-template>
	           </xsl:element>
	        </xsl:when>
	        <xsl:otherwise>
            <xsl:variable name="Children">
               <Children>
                  <xsl:for-each select="tei:content">
                     <xsl:call-template name="followRef"/>
                  </xsl:for-each>
               </Children>
            </xsl:variable>
            <xsl:for-each select="$Children/Children">
               <xsl:choose>
	                 <xsl:when test="Text and count(Element)=0">
	                    <xsl:element namespace="{$outputNS}" name="{$segName}">
		                      <xsl:attribute name="xml:lang">
		                         <xsl:value-of select="$documentationLanguage"/>
		                      </xsl:attribute>
		                      <xsl:call-template name="i18n">
		                         <xsl:with-param name="word">Character data only</xsl:with-param>
		                      </xsl:call-template>
	                    </xsl:element>
	                 </xsl:when>
                  <xsl:when test="count(Element)=0">
	                    <xsl:element namespace="{$outputNS}" name="{$segName}">
		                      <xsl:attribute name="xml:lang">
		                         <xsl:value-of select="$documentationLanguage"/>
		                      </xsl:attribute>
		                      <xsl:call-template name="i18n">
		                         <xsl:with-param name="word">Empty element</xsl:with-param>
		                      </xsl:call-template>
	                    </xsl:element>
                  </xsl:when>
                  <xsl:otherwise>
                     <xsl:element namespace="{$outputNS}" name="{$divName}">
                        <xsl:attribute name="{$rendName}">
                           <xsl:text>specChildren</xsl:text>
                        </xsl:attribute>
                        <xsl:for-each select="Element">
                           <xsl:sort select="@module"/>
                           <xsl:sort select="@name"/>
                           <xsl:if test="generate-id(.)=generate-id(key('CHILDMOD',@module)[1])">
                              <xsl:element namespace="{$outputNS}" name="{$divName}">
                                 <xsl:attribute name="{$rendName}">
                                    <xsl:text>specChild</xsl:text>
                                 </xsl:attribute>
                                 <xsl:if test="string-length(@module)&gt;0">

			                                 <xsl:element namespace="{$outputNS}" name="{$segName}">
			                                    <xsl:attribute name="{$rendName}">
			                                       <xsl:text>specChildModule</xsl:text>
			                                    </xsl:attribute>
			                                    <xsl:value-of select="@module"/>
			                                    <xsl:text>: </xsl:text>
			                                 </xsl:element>
			                              </xsl:if>
			                              <xsl:element namespace="{$outputNS}" name="{$segName}">
			                                 <xsl:attribute name="{$rendName}">
			                                    <xsl:text>specChildElements</xsl:text>
			                                 </xsl:attribute>
			                                 <xsl:for-each select="key('CHILDMOD',@module)">
			                                    <xsl:sort select="@name"/>
			                                    <xsl:variable name="me">
			                                       <xsl:value-of select="@name"/>
			                                    </xsl:variable>
			                                    <xsl:if test="not(preceding-sibling::Element/@name=$me)">
			                                       <xsl:for-each select="$Original">
				                                         <xsl:call-template name="linkTogether">
				                                            <xsl:with-param name="name" select="$me"/>
				                                         </xsl:call-template>
			                                       </xsl:for-each>
			                                       <xsl:call-template name="showSpace"/>
			                                    </xsl:if>
			                                 </xsl:for-each>
			                              </xsl:element>
		                            </xsl:element>
		                         </xsl:if>
		                      </xsl:for-each>
	                    </xsl:element>
	                 </xsl:otherwise>
               </xsl:choose>
            </xsl:for-each>
         </xsl:otherwise>
      </xsl:choose>
  </xsl:template>
  <xsl:template name="followRef">
      <xsl:for-each select=".//rng:ref">
         <xsl:if test="not(starts-with(@name,'any')        or starts-with(@name,'macro.any')       or @name='AnyThing')">
	           <xsl:variable name="Name">
	              <xsl:choose>
	                 <xsl:when test="contains(@name,'_sequence')">
	                    <xsl:value-of select="substring-before(@name,'_')"/>
	                 </xsl:when>
	                 <xsl:when test="contains(@name,'_alternat')">
	                    <xsl:value-of select="substring-before(@name,'_')"/>
	                 </xsl:when>
	                 <xsl:otherwise>
	                    <xsl:value-of select="@name"/>
	                 </xsl:otherwise>
	              </xsl:choose>
	           </xsl:variable>
            <xsl:for-each select="key('IDENTS',$Name)">
               <xsl:choose>
                  <xsl:when test="self::tei:elementSpec">
                     <Element name="{@ident}" module="{@module}"/>
                  </xsl:when>
                  <xsl:when test="self::tei:macroSpec">
                     <xsl:for-each select="tei:content">
		                      <xsl:choose>
		                         <xsl:when test="rng:text and count(rng:*)=1">
		                            <Text/>
		                         </xsl:when>
		                         <xsl:otherwise>
		                            <xsl:call-template name="followRef"/>
		                         </xsl:otherwise>
		                      </xsl:choose>
                     </xsl:for-each>
                  </xsl:when>
                  <xsl:when test="self::tei:classSpec">
                     <xsl:call-template name="followMembers"/>
                  </xsl:when>
               </xsl:choose>
            </xsl:for-each>
         </xsl:if>
      </xsl:for-each>
  </xsl:template>
  <xsl:template name="followMembers">
      <xsl:for-each select="key('CLASSMEMBERS',@ident)">
         <xsl:choose>
            <xsl:when test="self::tei:elementSpec">
               <Element name="{@ident}" module="{@module}"/>
            </xsl:when>
            <xsl:when test="self::tei:classSpec">
               <xsl:call-template name="followMembers"/>
            </xsl:when>
         </xsl:choose>
      </xsl:for-each>
  </xsl:template>

</xsl:stylesheet>