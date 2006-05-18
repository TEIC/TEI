<?xml version="1.0"?>
<!--
 %% Copyright 2004 Syd Bauman and the Text Encoding Initiative
 %% Consortium. Some rights reserved. For complete copyleft notice, see
 %% block comment at the end of this file.

 %% Tiny little style-sheet designed for one and only one purpose: to
 %% extract element type names from RelaxNG XML syntax grammars, and
 %% create a list of references to them.

 %% Intended use is for TEI to create a list of all TEI elements for use
 %% in p5odds-ex.odd via something like the following:

 %%   xsltproc [path-to-this-stylesheet] //TEI/P5/Schema/*.rng \
 %%   | grep '<ref' > /tmp/insert-me-into-p5odds-ex-dot-odd
-->

<xsl:transform
  xmlns:xsl='http://www.w3.org/1999/XSL/Transform' version="1.0"
  xmlns:rng="http://relaxng.org/ns/structure/1.0"
  >
  
  <xsl:template match="//rng:element">
    <xsl:element name="ref">
      <xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
    </xsl:element>
    <xsl:text>
    </xsl:text>
  </xsl:template>
  
  <xsl:template match="/">
    <xsl:text>
    </xsl:text>
    <xsl:apply-templates select="//rng:element"/>
  </xsl:template>
  
</xsl:transform>

<!--
 %% Copyright 2004 Syd Bauman and the Text Encoding Initiative
 %% Consortium. This program is free software; you can redistribute it
 %% and/or modify it under the terms of the GNU General Public License
 %% as published by the Free Software Foundation; either version 2 of
 %% the License, or (at your option) any later version. This program is
 %% distributed in the hope that it will be useful, but WITHOUT ANY
 %% WARRANTY; without even the implied warranty of MERCHANTABILITY or
 %% FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
 %% for more details. You should have received a copy of the GNU General
 %% Public License along with this program; if not, write to the
 %%        Free Software Foundation, Inc.
 %%        675 Mass Ave
 %%        Cambridge, MA  02139
 %%        USA
 %%        gnu@prep.ai.mit.edu

 %% Syd Bauman, xml textbase programmer/analyst
 %% Brown University Women Writers Project
 %% Box 1841
 %% Providence, RI  02912-1841
 %% 401-863-3835
 %% Syd_Bauman@Brown.edu
-->
