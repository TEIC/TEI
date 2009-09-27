<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" 
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns:xd="http://www.pnp-software.com/XSLTdoc">
	
	<xd:doc type="stylesheet">
		<xd:short> TEI Utility stylesheet for making Word docx files from TEI XML (see tei-docx.xsl)</xd:short>
		<xd:detail> This library is free software; you can redistribute it and/or
			modify it under the terms of the GNU Lesser General Public License as
			published by the Free Software Foundation; either version 2.1 of the
			License, or (at your option) any later version. This library is
			distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
			without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
			PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
			details. You should have received a copy of the GNU Lesser General Public
			License along with this library; if not, write to the Free Software
			Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA </xd:detail>
		<xd:author>See AUTHORS</xd:author>
		<xd:cvsId>$Id$</xd:cvsId>
		<xd:copyright>2008, TEI Consortium</xd:copyright>
	</xd:doc>
	
	
	<xsl:variable name="frScope">Domaine d'application</xsl:variable>
	<xsl:variable name="ANNEX">ANNEX</xsl:variable>
	<xsl:variable name="a2">a2</xsl:variable>
	<xsl:variable name="a3">a3</xsl:variable>
	<xsl:variable name="a4">a4</xsl:variable>
	<xsl:variable name="a5">a5</xsl:variable>
	<xsl:variable name="a6">a6</xsl:variable>
	<xsl:variable name="BibliographyHeading">zzBiblio</xsl:variable>
	<xsl:variable name="BibliographyItem">Bibliography</xsl:variable>
	<xsl:variable name="BibliographyReference">BibliographyReference</xsl:variable>
	<xsl:variable name="DefinitionList">dl</xsl:variable>
	<xsl:variable name="ExampleHeadingChar">Example Heading Char</xsl:variable>
	<xsl:variable name="FigureHeadingChar">Figure Heading Char</xsl:variable>
	<xsl:variable name="FigureTitle">Figure Title</xsl:variable>
	<xsl:variable name="ForewordHeading">zzForeword</xsl:variable>
	<xsl:variable name="FormulaReference">FormulaReference</xsl:variable>
	<xsl:variable name="HeadingChar">Heading Char</xsl:variable>
	<xsl:variable name="HeadingCharFr">Titre de note Car</xsl:variable>
	<xsl:variable name="zzIntroductionHeading">zzIntroduction</xsl:variable>
	<xsl:variable name="IntroductionHeading">Introduction</xsl:variable>
	<xsl:variable name="TableNote">Table note</xsl:variable>
	<xsl:variable name="OldTableNote">TableNote</xsl:variable>
	<xsl:variable name="TableFootnote">Table footnote</xsl:variable>
	<xsl:variable name="NoteHeadingChar">Note Heading Char</xsl:variable>
	<xsl:variable name="TableHeadingChar">Table Heading Char</xsl:variable>
	<xsl:variable name="TableNoteHeadingChar">TableNoteHeading Char</xsl:variable>
	<xsl:variable name="Tabletitle">Table title</xsl:variable>
	<xsl:variable name="TermNum">TermNum</xsl:variable>
	<xsl:variable name="Terms">Term(s)</xsl:variable>
	<xsl:variable name="ExtXref">ExtXref</xsl:variable>

</xsl:stylesheet>