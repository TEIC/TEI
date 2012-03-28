<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:fo="http://www.w3.org/1999/XSL/Format" xmlns:s="http://www.ascc.net/xml/schematron" xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0" xmlns:html="http://www.w3.org/1999/xhtml" xmlns:rng="http://relaxng.org/ns/structure/1.0" xmlns:tei="http://www.tei-c.org/ns/1.0" xmlns:teix="http://www.tei-c.org/ns/Examples" xmlns:sch="http://purl.oclc.org/dsdl/schematron" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" exclude-result-prefixes="fo s a tei html rng teix xs sch" version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p> TEI stylesheet for weaving TEI ODD documents</p>
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
      <p>Copyright: 2011, TEI Consortium</p>
    </desc>
  </doc>
  <xsl:key name="CHILDMOD" match="Element" use="@module"/>


  <xsl:template match="tei:ptr|tei:ref" mode="weave">
    <xsl:choose>
      <xsl:when test="ancestor::tei:remarks or ancestor::tei:listRef or ancestor::tei:valDesc">
        <xsl:choose>
          <xsl:when test="starts-with(@target,'#') and id(substring(@target,2))">
            <xsl:call-template name="makeInternalLink">
              <xsl:with-param name="target" select="substring(@target,2)"/>
              <xsl:with-param name="ptr" select="if (self::tei:ptr)
						 then true() else false()"/>
              <xsl:with-param name="dest">
                <xsl:call-template name="generateEndLink">
                  <xsl:with-param name="where">
                    <xsl:value-of select="substring(@target,2)"/>
                  </xsl:with-param>
                </xsl:call-template>
              </xsl:with-param>
            </xsl:call-template>
          </xsl:when>
          <xsl:when test="starts-with(@target,'#')">
            <xsl:variable name="Chapter">
              <xsl:value-of select="substring(@target,2,2)"/>
            </xsl:variable>
            <xsl:choose>
              <xsl:when test="$Chapter='AB' or        $Chapter='AI' or        $Chapter='CC' or        $Chapter='CE' or        $Chapter='CH' or        $Chapter='CO' or        $Chapter='DI' or        $Chapter='DR' or        $Chapter='DS' or        $Chapter='FS' or        $Chapter='FT' or        $Chapter='GD' or        $Chapter='HD' or        $Chapter='MS' or        $Chapter='ND' or        $Chapter='NH' or        $Chapter='PH' or        $Chapter='SA' or        $Chapter='SG' or        $Chapter='ST' or        $Chapter='TC' or        $Chapter='TD' or        $Chapter='TS' or        $Chapter='USE' or        $Chapter='VE' or        $Chapter='WD'">
                <xsl:call-template name="makeExternalLink">
		  <xsl:with-param name="ptr" select="if (self::tei:ptr)
						 then true() else false()"/>
                  <xsl:with-param name="dest">
                    <xsl:text>http://www.tei-c.org/release/doc/tei-p5-doc/</xsl:text>
                    <xsl:value-of select="$documentationLanguage"/>
                    <xsl:text>/html/</xsl:text>
                    <xsl:value-of select="$Chapter"/>
                    <xsl:text>.html</xsl:text>
                    <xsl:value-of select="@target"/>
                  </xsl:with-param>
                </xsl:call-template>
              </xsl:when>
              <xsl:otherwise>
                <xsl:text>«</xsl:text>
                <xsl:value-of select="@target"/>
                <xsl:text>»</xsl:text>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:when>
          <xsl:otherwise>
            <xsl:apply-imports/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-imports/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

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
          <xsl:if test="@status">
            <xsl:text> status_</xsl:text>
            <xsl:value-of select="@status"/>
          </xsl:if>
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
          <xsl:if test="@status">
            <xsl:text> status_</xsl:text>
            <xsl:value-of select="@status"/>
          </xsl:if>
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
      <xsl:element namespace="{$outputNS}" name="{$dtName}"><xsl:element namespace="{$outputNS}" name="{$hiName}"><xsl:attribute name="{$rendName}"><xsl:text>label</xsl:text></xsl:attribute><xsl:attribute name="xml:lang"><xsl:value-of select="$documentationLanguage"/></xsl:attribute><xsl:call-template name="i18n"><xsl:with-param name="word">Class</xsl:with-param></xsl:call-template></xsl:element>: <xsl:value-of select="@ident"/></xsl:element>
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
            <xsl:text>&#160;</xsl:text>
            <xsl:call-template name="makeDescription"/>
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
              <xsl:call-template name="generateModelParents">
                <xsl:with-param name="showElements">true</xsl:with-param>
              </xsl:call-template>
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
            <xsl:call-template name="generateModelParents">
              <xsl:with-param name="showElements">false</xsl:with-param>
            </xsl:call-template>
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
                <xsl:with-param name="word">Contained by</xsl:with-param>
              </xsl:call-template>
            </xsl:element>
          </xsl:element>
          <xsl:element namespace="{$outputNS}" name="{$cellName}">
            <xsl:attribute name="{$rendName}">
              <xsl:text>wovenodd-col2</xsl:text>
            </xsl:attribute>
            <xsl:call-template name="generateIndirectParents"/>
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
                  <xsl:if test="$autoGlobal='true'">
                    <rng:ref name="att.global.attributes"/>
                  </xsl:if>
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
  <xsl:template match="tei:constraintSpec[parent::tei:elementSpec or parent::tei:attDef]" mode="weave">
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
    <xsl:call-template name="showSpace"/>
    <xsl:call-template name="makeDescription">
      <xsl:with-param name="showListRef">false</xsl:with-param>
    </xsl:call-template>
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
      <xsl:when test="@xml:lang='mul' and not($documentationLanguage='zh-TW')">
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
            <xsl:text>&#160;</xsl:text>
            <xsl:call-template name="makeDescription"/>
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
                <xsl:call-template name="generateModelParents">
                  <xsl:with-param name="showElements">true</xsl:with-param>
                </xsl:call-template>
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
                <xsl:when test=".=&quot;TEI.singleBase&quot;"/>
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
			<xsl:with-param name="reftext">
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
			<xsl:with-param name="reftext">
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
			<xsl:with-param name="reftext">
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
          <xsl:comment>&#160;</xsl:comment>
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
    <desc>[odds] display attribute list  </desc>
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
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[odds] display list of model parents </desc>
  </doc>
  <xsl:template name="generateModelParents">
    <xsl:param name="showElements">true</xsl:param>
    <xsl:variable name="here" select="."/>
    <xsl:element namespace="{$outputNS}" name="{$divName}">
      <xsl:attribute name="{$rendName}">parent</xsl:attribute>
      <xsl:variable name="Parents">
        <xsl:for-each select="key('REFS',@ident)/ancestor::tei:content/parent::*">
          <Element type="{local-name()}" module="{@module}" name="{@ident}">
            <xsl:attribute name="altName">
              <xsl:choose>
                <xsl:when test="tei:altIdent">
                  <xsl:value-of select="tei:altIdent"/>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:value-of select="@ident"/>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:attribute>
          </Element>
        </xsl:for-each>
        <xsl:for-each select="tei:classes/tei:memberOf">
          <xsl:for-each select="key('CLASSES',@key)">
            <xsl:if test="@type='model'">
              <Element type="classSpec" module="{@module}" name="{@ident}">
                <xsl:attribute name="altName">
                  <xsl:choose>
                    <xsl:when test="tei:altIdent">
                      <xsl:value-of select="tei:altIdent"/>
                    </xsl:when>
                    <xsl:otherwise>
                      <xsl:value-of select="@ident"/>
                    </xsl:otherwise>
                  </xsl:choose>
                </xsl:attribute>
              </Element>
            </xsl:if>
          </xsl:for-each>
        </xsl:for-each>
      </xsl:variable>
      <xsl:if test="count($Parents/*)&gt;0">
        <xsl:for-each-group select="$Parents/*" group-by="@name">
          <xsl:sort select="@altName"/>
          <xsl:variable name="name" select="@name"/>
          <xsl:variable name="alt" select="@altName"/>
          <xsl:variable name="type" select="@type"/>
          <xsl:if test="not(@type='elementSpec' and $showElements='false')">
            <xsl:for-each select="$here">
              <xsl:call-template name="linkTogether">
                <xsl:with-param name="name" select="$name"/>
                <xsl:with-param name="reftext" select="$alt"/>
                <xsl:with-param name="class">link_odd_<xsl:value-of select="$type"/></xsl:with-param>
              </xsl:call-template>
            </xsl:for-each>
            <xsl:if test="not(position() = last())">
              <xsl:call-template name="showSpaceBetweenItems"/>
            </xsl:if>
          </xsl:if>
        </xsl:for-each-group>
      </xsl:if>
      <xsl:call-template name="generateParentsByAttribute"/>
    </xsl:element>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
    <desc>[odds] display list of generated parents (via models)   </desc>
  </doc>
  <xsl:template name="generateIndirectParents">
    <xsl:element namespace="{$outputNS}" name="{$divName}">
      <xsl:attribute name="{$rendName}">parent</xsl:attribute>
      <xsl:variable name="here" select="."/>
      <xsl:variable name="name" select="@ident"/>
      <xsl:variable name="Parents">
        <xsl:call-template name="ProcessDirectRefs"/>
        <!-- now look at class membership -->
        <xsl:for-each select="tei:classes/tei:memberOf">
          <xsl:for-each select="key('CLASSES',@key)">
            <xsl:if test="@type='model'">
              <xsl:call-template name="ProcessClass">
                <xsl:with-param name="name" select="$name"/>
              </xsl:call-template>
            </xsl:if>
          </xsl:for-each>
        </xsl:for-each>
      </xsl:variable>
      <xsl:call-template name="displayElementsByModule">
        <xsl:with-param name="List">
          <xsl:copy-of select="$Parents"/>
        </xsl:with-param>
      </xsl:call-template>
    </xsl:element>
  </xsl:template>
  <xsl:template name="ProcessDirectRefs">
    <!-- direct parents -->
    <xsl:for-each select="key('REFS',@ident)/ancestor::tei:content/parent::tei:*">
      <xsl:choose>
        <xsl:when test="self::tei:elementSpec">
          <Element type="{local-name()}" module="{@module}" name="{@ident}">
            <xsl:attribute name="altName">
              <xsl:choose>
                <xsl:when test="tei:altIdent">
                  <xsl:value-of select="tei:altIdent"/>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:value-of select="@ident"/>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:attribute>
          </Element>
        </xsl:when>
        <xsl:when test="self::tei:macroSpec">
          <xsl:call-template name="ProcessDirectRefs"/>
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>
  <xsl:template name="ProcessClass">
    <xsl:param name="name"/>
    <xsl:for-each select="key('REFS',@ident)/ancestor::tei:content/parent::tei:*">
      <xsl:choose>
        <xsl:when test="self::tei:elementSpec">
          <Element type="{local-name()}" module="{@module}" name="{@ident}">
            <xsl:attribute name="altName">
              <xsl:choose>
                <xsl:when test="tei:altIdent">
                  <xsl:value-of select="tei:altIdent"/>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:value-of select="@ident"/>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:attribute>
          </Element>
        </xsl:when>
        <xsl:when test="self::tei:macroSpec">
          <xsl:call-template name="ProcessDirectRefs"/>
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
    <xsl:for-each select="tei:classes/tei:memberOf">
      <xsl:for-each select="key('CLASSES',@key)">
        <xsl:if test="@type='model'">
          <xsl:call-template name="ProcessClass">
            <xsl:with-param name="name" select="$name"/>
          </xsl:call-template>
        </xsl:if>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:template>
  <xsl:template name="generateMembers">
    <xsl:param name="depth">1</xsl:param>
    <xsl:param name="me"/>
    <xsl:variable name="here" select="."/>
    <xsl:variable name="this" select="@ident"/>
    <xsl:if test="not($this=$me) and key('CLASSMEMBERS',$this)">
      <xsl:element namespace="{$outputNS}" name="{$hiName}">
        <xsl:attribute name="{$rendName}">
          <xsl:text>showmembers</xsl:text>
          <xsl:value-of select="$depth"/>
        </xsl:attribute>
        <xsl:if test="$depth &gt; 1"> [</xsl:if>
        <xsl:variable name="list">
          <ClassList>
            <xsl:for-each select="key('CLASSMEMBERS',$this)">
              <Item type="local-name()" ident="@ident">
                <xsl:call-template name="linkTogether">
                  <xsl:with-param name="name" select="@ident"/>
                  <xsl:with-param name="reftext">
                    <xsl:choose>
                      <xsl:when test="tei:altIdent">
                        <xsl:value-of select="tei:altIdent"/>
                      </xsl:when>
                      <xsl:otherwise>
                        <xsl:value-of select="@ident"/>
                      </xsl:otherwise>
                    </xsl:choose>
                  </xsl:with-param>
                  <xsl:with-param name="class">
                    <xsl:text>link_odd_</xsl:text>
                    <xsl:value-of select="local-name()"/>
                  </xsl:with-param>
                </xsl:call-template>
                <xsl:call-template name="generateMembers">
                  <xsl:with-param name="depth">
                    <xsl:value-of select="$depth + 1"/>
                  </xsl:with-param>
                </xsl:call-template>
              </Item>
            </xsl:for-each>
          </ClassList>
        </xsl:variable>
        <xsl:for-each select="$list/ClassList/Item">
          <xsl:sort select="@type"/>
          <xsl:sort select="@ident"/>
          <xsl:copy-of select="*|text()"/>
          <xsl:if test="following-sibling::Item">
            <xsl:call-template name="showSpaceBetweenItems"/>
          </xsl:if>
        </xsl:for-each>
        <xsl:if test="$depth &gt; 1">] </xsl:if>
      </xsl:element>
    </xsl:if>
  </xsl:template>
  <xsl:template name="generateParentsByAttribute">
    <xsl:variable name="this" select="@ident"/>
    <xsl:variable name="list">
      <PattList>
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
		    <xsl:text>link_odd_classSpec</xsl:text>
		  </xsl:with-param>
		</xsl:call-template>
	      </Item>
	    </xsl:for-each>
	    <xsl:text>/@</xsl:text>
	    <xsl:value-of select="ancestor::tei:attDef/@ident"/>
	  </xsl:element>
	</xsl:for-each>
      </PattList>
    </xsl:variable>
    <xsl:if test="count($list/PattList/Item)&gt;0">
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
        <xsl:for-each select="$list/PattList/Item">
          <xsl:copy-of select="*|text()"/>
        </xsl:for-each>
      </xsl:element>
    </xsl:if>
    <xsl:variable name="list2">
      <PattList>
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
		  <xsl:with-param name="reftext">
		    <xsl:choose>
		      <xsl:when test="tei:altIdent">
			<xsl:value-of select="tei:altIdent"/>
		      </xsl:when>
		      <xsl:otherwise>
			<xsl:value-of select="@ident"/>
		      </xsl:otherwise>
		    </xsl:choose>
		  </xsl:with-param>
		  <xsl:with-param name="class">
		    <xsl:text>link_odd_elementSpec</xsl:text>
		  </xsl:with-param>
		</xsl:call-template>
	      </xsl:for-each>
	      <xsl:text>/@</xsl:text>
	      <xsl:for-each select="ancestor::tei:attDef">
		<xsl:value-of select="(tei:altIdent|@ident)[last()]"/>
	      </xsl:for-each>
	    </xsl:element>
	  </Item>
	</xsl:for-each>
      </PattList>
    </xsl:variable>
    <xsl:if test="count($list2/PattList/Item)&gt;0">
      <xsl:element namespace="{$outputNS}" name="{$segName}">
	<xsl:attribute name="xml:lang">
	  <xsl:value-of select="$documentationLanguage"/>
	</xsl:attribute>
	<xsl:call-template name="i18n">
	  <xsl:with-param name="word">
	    <xsl:text>Element</xsl:text>
	  </xsl:with-param>
	</xsl:call-template>
	<xsl:text>: </xsl:text>
      </xsl:element>
      <xsl:element namespace="{$outputNS}" name="{$ulName}">
	<xsl:for-each select="$list2/PattList/Item">
	  <xsl:copy-of select="*|text()"/>
	</xsl:for-each>
      </xsl:element>
    </xsl:if>
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
                <xsl:value-of select="translate($this,':','_')"/>
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
		    <xsl:with-param name="reftext">
		      <xsl:choose>
			<xsl:when test="tei:altIdent">
			  <xsl:value-of select="tei:altIdent"/>
			</xsl:when>
			<xsl:otherwise>
			  <xsl:value-of select="@ident"/>
			</xsl:otherwise>
		      </xsl:choose>
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
                    <xsl:with-param name="reftext">
                      <xsl:choose>
                        <xsl:when test="tei:altIdent">
                          <xsl:value-of select="tei:altIdent"/>
                        </xsl:when>
                        <xsl:otherwise>
                          <xsl:value-of select="@ident"/>
                        </xsl:otherwise>
                      </xsl:choose>
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
    <xsl:value-of select="translate (., ' ', '&#160;')"/>
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
                  <xsl:when test="not($autoGlobal='true')">Attributes</xsl:when>
                  <xsl:when test=".//tei:attDef">In addition to global attributes and those inherited from</xsl:when>
                  <xsl:otherwise>Global attributes and those inherited from</xsl:otherwise>
                </xsl:choose>
              </xsl:with-param>
            </xsl:call-template>
            <xsl:value-of select="$spaceCharacter"/>
          </xsl:element>
        </xsl:if>
        <xsl:copy-of select="$clatts"/>
      </xsl:when>
      <xsl:when test="ancestor::tei:schemaSpec and not(key('CLASSES','att.global'))">
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="i18n">
          <xsl:with-param name="word">
            <xsl:choose>
              <xsl:when test="not($autoGlobal='true')">Attributes</xsl:when>
              <xsl:when test=".//tei:attDef">In addition to global attributes</xsl:when>
              <xsl:otherwise>Global attributes only</xsl:otherwise>
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
            <a xmlns="http://www.w3.org/1999/xhtml" class="link_element" href="ref-{$name}{$outputSuffix}">
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
        <xsl:text>UNKNOWN ELEMENT </xsl:text>
        <xsl:value-of select="$name"/>
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
        <xsl:with-param name="ptr" select="true()"/>
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
      <xsl:when test="tei:content//rng:ref[@name='macro.anyXML']">
        <xsl:element namespace="{$outputNS}" name="{$segName}">
          <xsl:attribute name="xml:lang">
            <xsl:value-of select="$documentationLanguage"/>
          </xsl:attribute>
          <xsl:text>ANY</xsl:text>
        </xsl:element>
      </xsl:when>
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
          <xsl:for-each select="tei:content">
            <xsl:call-template name="followRef"/>
          </xsl:for-each>
        </xsl:variable>
        <xsl:call-template name="displayElementsByModule">
          <xsl:with-param name="List">
            <xsl:copy-of select="$Children"/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="displayElementsByModule">
    <xsl:param name="List"/>
    <xsl:variable name="here" select="."/>
    <xsl:for-each select="$List">
      <xsl:choose>
        <xsl:when test="Element[@type='Text'] and count(Element)=1">
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
            <xsl:for-each-group select="*" group-by="@module">
              <xsl:sort select="@module"/>
              <xsl:element namespace="{$outputNS}" name="{$divName}">
                <xsl:attribute name="{$rendName}">
                  <xsl:text>specChild</xsl:text>
                </xsl:attribute>
                <xsl:if test="string-length(current-grouping-key())&gt;0">
                  <xsl:element namespace="{$outputNS}" name="{$segName}">
                    <xsl:attribute name="{$rendName}">
                      <xsl:text>specChildModule</xsl:text>
                    </xsl:attribute>
                    <xsl:value-of select="current-grouping-key()"/>
                    <xsl:text>: </xsl:text>
                  </xsl:element>
                </xsl:if>
                <xsl:element namespace="{$outputNS}" name="{$segName}">
                  <xsl:attribute name="{$rendName}">
                    <xsl:text>specChildElements</xsl:text>
                  </xsl:attribute>
                  <xsl:for-each-group select="current-group()" group-by="@altName">
                    <xsl:sort select="@altName"/>
                    <xsl:variable name="me" select="@name"/>
                    <xsl:variable name="alt" select="@altName"/>
                    <xsl:variable name="type" select="@type"/>
                    <xsl:for-each select="$here">
                      <xsl:call-template name="linkTogether">
                        <xsl:with-param name="name" select="$me"/>
                        <xsl:with-param name="reftext" select="$alt"/>
                        <xsl:with-param name="class">link_odd_<xsl:value-of select="$type"/></xsl:with-param>
                      </xsl:call-template>
                    </xsl:for-each>
                    <xsl:if test="not(position() = last())">
                      <xsl:call-template name="showSpaceBetweenItems"/>
                    </xsl:if>
                  </xsl:for-each-group>
                </xsl:element>
              </xsl:element>
            </xsl:for-each-group>
          </xsl:element>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
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
              <Element name="{@ident}" module="{@module}" type="{local-name()}">
                <xsl:attribute name="altName">
                  <xsl:choose>
                    <xsl:when test="tei:altIdent">
                      <xsl:value-of select="tei:altIdent"/>
                    </xsl:when>
                    <xsl:otherwise>
                      <xsl:value-of select="@ident"/>
                    </xsl:otherwise>
                  </xsl:choose>
                </xsl:attribute>
              </Element>
            </xsl:when>
            <xsl:when test="self::tei:macroSpec">
              <xsl:for-each select="tei:content">
                <xsl:choose>
                  <xsl:when test="rng:text and count(rng:*)=1">
                    <Element type="TEXT"/>
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
          <Element name="{@ident}" module="{@module}"
		   type="{local-name()}">
	    <xsl:attribute name="altName">
	      <xsl:choose>
		<xsl:when test="tei:altIdent">
		  <xsl:value-of select="tei:altIdent"/>
		</xsl:when>
		<xsl:otherwise>
		  <xsl:value-of select="@ident"/>
		</xsl:otherwise>
	      </xsl:choose>
	    </xsl:attribute>
	  </Element>
        </xsl:when>
        <xsl:when test="self::tei:classSpec">
          <xsl:call-template name="followMembers"/>
        </xsl:when>
      </xsl:choose>
    </xsl:for-each>
  </xsl:template>
</xsl:stylesheet>
