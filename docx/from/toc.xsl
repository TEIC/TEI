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
         
         <p>Copyright: 2013, TEI Consortium</p>
      </desc>
   </doc>
    
    
    <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl">
      <desc> Process a paragraph with a TOC style, which means parsing
      the instrText field </desc>
<!--
        Handle TOC
{ TOC [Switches ] }

Builds a table of contents. The TOC field collects entries for a table of 
contents using heading levels, specified styles (style: A combination of 
formatting characteristics, such as font, font size, and indentation, that 
you name and store as a set. When you apply a style, all of the formatting 
instructions in that style are applied at one time.), or entries specified by 
TC (Table of Contents Entry) fields. Microsoft Word inserts the TOC field 
when you use the Index and Tables command from the Reference submenu on the 
Insert menu.

Note  If the table of contents created by the TOC field affects the 
pagination of the document, you may have to update the field again to reflect 
the correct page numbers.

Switches

\a Identifier 
Lists items captioned with the Caption command (Insert menu, Reference 
submenu) but omits caption labels and numbers. The identifier corresponds to 
the caption label. For example, although a caption on page 12 is "Figure 8: 
Mercury", the field { TOC \a figures } displays entries as 
"Mercury............12".
Use the \c switch to build a table of captions with labels and numbers.

\b BookmarkName 
Collects entries only from the portion of the document marked by the 
specified bookmark (bookmark: A location or selection of text in a file that 
you name for reference purposes. Bookmarks identify a location within your 
file that you can later refer or link to.). 
\c "SEQIdentifier" 
Lists figures, tables, charts, or other items that are numbered by a SEQ 
(Sequence) field. Word uses SEQ fields to number items captioned with the 
Caption command (Insert menu, Reference submenu). SEQIdentifier, which 
corresponds to the caption label, must match the identifier in the SEQ field. 
For example, { TOC \c "tables" } lists all numbered tables. 
\f EntryIdentifier 
Builds a table from TC fields. If EntryIdentifier is specified, the table is 
built only from TC fields with the same identifier (typically a letter). For 
example, { TOC \f t } builds a table of contents from TC fields such as { TC 
"Entry Text" \f t }. 
\h Hyperlinks 
Inserts TOC entries as hyperlinks (hyperlink: Colored and underlined text or 
a graphic that you click to go to a file, a location in a file, a Web page on 
the World Wide Web, or a Web page on an intranet. Hyperlinks can also go to 
newsgroups and to Gopher, Telnet, and FTP sites.). 
\l Levels 
Builds a table of contents from TC fields that assign entries to one of the 
specified levels. For example, { TOC \l 1-4 } builds a table of contents from 
TC fields that assign entries to levels 1-4. TC fields that assign entries to 
lower levels are skipped. 
\n Levels 
Omits page numbers from the table of contents. Page numbers are omitted from 
all levels unless a range of entry levels is specified. For example, { TOC \n 
3-4 } omits page numbers from levels 3 and 4. Delete this switch to include 
page numbers. 
\o "Headings" 
Builds a table of contents from paragraphs formatted with built-in heading 
styles (heading style: Formatting applied to a heading. Microsoft Word has 
nine different built-in styles: Heading 1 through Heading 9.). For example, { 
TOC \o "1-3" } lists only headings formatted with the styles Heading 1 
through Heading 3. If no heading range is specified, all heading levels used 
in the document are listed. Enclose the range numbers in quotation marks. 
\p "Separators" 
Specifies the characters that separate an entry and its page number. For 
example, the field { TOC \p "— " }, with an em dash, displays a result such 
as "Selecting Text— 53." The default is a tab with leader dots. You can use 
up to five characters, which must be enclosed in quotation marks. 
\s Identifier 
Includes a number such as a chapter number before the page number. The 
chapter or other item must be numbered with a SEQ field. Identifier must 
match the identifier in the SEQ field. For example, if you insert { SEQ 
chapter } before each chapter heading, { TOC \o "1-3" \s chapter } displays 
page numbers as 2-14, where "2" is the chapter number. 
\d "Separator" 
When used with the \s switch, specifies the number of characters that 
separate the sequence numbers and page numbers. Enclose the characters in 
quotation marks. Word uses a hyphen (-) if no \d switch is specified. In the 
table of contents generated by { TOC \o "1-3" \s chapter \d ":" }, a colon 
(:) separates chapter numbers and page numbers— for example, "2:14." 
\t "Style,Level, Style,Level,..." 
Builds a table of contents from paragraphs formatted with styles other than 
the built-in heading styles. For example, { TOC \t "chaptertitle,1, 
chapterhead,2" } builds a table of contents from paragraphs formatted with 
the styles "chaptertitle" and "chapterhead." The number after each style name 
indicates the table of contents entry level that corresponds to that style. 
You can use both the \o switch and the \t switch to build a table of 
contents from built-in heading styles and other styles.

\u 
Builds a table of contents by using the applied paragraph outline level 
(outline level: Paragraph formatting you can use to assign a hierarchical 
level (Level 1 through Level 9) to paragraphs in your document. For example, 
after you assign outline levels, you can work with the document in outline 
view or in the Document Map.). 
\w 
Preserves tab entries within table entries. 
\x 
Preserves newline characters within table entries. 
\z 
Hides tab leader and page numbers in Web layout view (Web layout view: A 
view of a document as it will appear in a Web browser. For example, the 
document appears as one long page (without page breaks) and text and tables 
wrap to fit in the window.).
-->
   </doc>
    <xsl:template name="tocSection">
        <divGen type="toc"/>
    </xsl:template>

    <xsl:template name="_tocSection">
      <xsl:variable name="typestring">
	<xsl:value-of select="(.//w:instrText)[1]"/>
      </xsl:variable>
	<xsl:message>TOC: test [<xsl:value-of select="$typestring"/>]</xsl:message>
    <xsl:analyze-string select='$typestring' regex='\s*([A-z]+)\s+(.*)\s+"([^,]+),(.*)"\s*'>
      <xsl:matching-substring>
	<xsl:message>TOC ok, found [<xsl:value-of select="regex-group(1)"/>][<xsl:value-of select="regex-group(2)"/>][<xsl:value-of select="regex-group(3)"/>]</xsl:message>
      </xsl:matching-substring>
      <xsl:non-matching-substring>
	<xsl:message>TOC panic on <xsl:value-of select="."/></xsl:message>
      </xsl:non-matching-substring>
    </xsl:analyze-string>
<!--
        <w:instrText xml:space="preserve"> TOC \o "1-2" \h \z \t "ITLP H3,3" </w:instrText>

        <w:instrText xml:space="preserve"> TOC \h \z \t "ITLP Ex Heading,2" </w:instrText>
-->
        <divGen type="toc"/>
    </xsl:template>
    
    
</xsl:stylesheet>
