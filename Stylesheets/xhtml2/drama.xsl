<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns="http://www.w3.org/1999/xhtml"
                xmlns:a="http://relaxng.org/ns/compatibility/annotations/1.0"
                xmlns:fo="http://www.w3.org/1999/XSL/Format"
                xmlns:html="http://www.w3.org/1999/xhtml"

                xmlns:rng="http://relaxng.org/ns/structure/1.0"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:teix="http://www.tei-c.org/ns/Examples"
                
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                exclude-result-prefixes="#default  a fo rng tei teix"
                version="2.0">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
      <desc>
         <p> TEI stylesheet dealing with elements from the drama module,
      making HTML output. </p>
         <p> This library is free software; you can redistribute it and/or
      modify it under the terms of the GNU Lesser General Public License as
      published by the Free Software Foundation; either version 2.1 of the
      License, or (at your option) any later version. This library is
      distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
      without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
      PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
      details. You should have received a copy of the GNU Lesser General Public
      License along with this library; if not, write to the Free Software
      Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA </p>
         <p>Author: See AUTHORS</p>
         <p>Id: $Id$</p>
         <p>Copyright: 2008, TEI Consortium</p>
      </desc>
   </doc>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element actor</desc>
   </doc>
  <xsl:template match="tei:actor">
      <span class="actor">
         <xsl:apply-templates/>
      </span>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element camera</desc>
   </doc>
  <xsl:template match="tei:camera">
      <span class="camera">
         <xsl:apply-templates/>
      </span>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element caption</desc>
   </doc>
  <xsl:template match="tei:caption">
      <span class="caption">
         <xsl:apply-templates/>
      </span>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element castGroup</desc>
   </doc>
  <xsl:template match="tei:castGroup">
      <ul>
         <xsl:apply-templates/>
      </ul>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element castItem</desc>
   </doc>
  <xsl:template match="tei:castItem">
      <li>
         <xsl:apply-templates/>
      </li>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element castList</desc>
   </doc>
  <xsl:template match="tei:castList">
      <xsl:if test="tei:head">
         <p>
            <em>
               <xsl:for-each select="tei:head">
                  <xsl:apply-templates/>
               </xsl:for-each>
            </em>
         </p>
      </xsl:if>
      <ul>
         <xsl:apply-templates/>
      </ul>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element castList/tei:head</desc>
   </doc>
  <xsl:template match="tei:castList/tei:head"/>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element role</desc>
   </doc>
  <xsl:template match="tei:role">
      <span class="role">
         <xsl:apply-templates/>
      </span>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element sp/tei:stage</desc>
   </doc>
  <xsl:template match="tei:sp/tei:stage">
      <span class="stage">
         <xsl:apply-templates/>
      </span>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element roleDesc</desc>
   </doc>
  <xsl:template match="tei:roleDesc">
      <blockquote>
	        <xsl:choose>
	           <xsl:when test="@rend">
	              <xsl:attribute name="class">
		                <xsl:value-of select="@rend"/>
	              </xsl:attribute>
	           </xsl:when>
	           <xsl:when test="@rendition">
		             <xsl:call-template name="applyRendition"/>
	           </xsl:when>
	           <xsl:otherwise>
	              <xsl:attribute name="class">
		                <xsl:text>roleDesc</xsl:text>
	              </xsl:attribute>
	           </xsl:otherwise>
	        </xsl:choose>
         <xsl:choose>
            <xsl:when test="tei:p">
               <xsl:apply-templates/>
            </xsl:when>
            <xsl:otherwise>
               <p>
                  <xsl:apply-templates/>
               </p>
            </xsl:otherwise>
         </xsl:choose>
      </blockquote>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element set</desc>
   </doc>
  <xsl:template match="tei:set">
      <span class="set">
         <xsl:apply-templates/>
      </span>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element sound</desc>
   </doc>
  <xsl:template match="tei:sound">
      <span class="sound">
         <xsl:apply-templates/>
      </span>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>
         <p>Process element sp</p>
         <p>
            <p xmlns="http://www.w3.org/1999/xhtml"> elaborated by Nick Nicholas &lt;nicholas@uci.edu&gt;, March
        2001 </p>
         </p>
      </desc>
   </doc>
  <xsl:template match="tei:sp">
      <dl>
         <dt>
	           <xsl:call-template name="makeAnchor"/>
            <xsl:apply-templates select="tei:speaker"/>
         </dt>
         <dd>
            <xsl:apply-templates select="tei:p | tei:l | tei:lg | tei:seg | tei:ab | tei:stage"/>
         </dd>
      </dl>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element sp/tei:p</desc>
   </doc>
  <xsl:template match="tei:sp/tei:p">
      <xsl:apply-templates/>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element stage</desc>
   </doc>
  <xsl:template match="tei:stage">
      <p>
         <span class="stage">
            <xsl:apply-templates/>
         </span>
      </p>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element tech</desc>
   </doc>
  <xsl:template match="tei:tech">
      <span class="tech">
         <xsl:apply-templates/>
      </span>
  </xsl:template>
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc>Process element view</desc>
   </doc>
  <xsl:template match="tei:view">
      <span class="view">
         <xsl:apply-templates/>
      </span>
  </xsl:template>
</xsl:stylesheet>