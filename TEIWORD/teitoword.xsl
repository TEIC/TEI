<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet
  xmlns:html="http://www.w3.org/1999/xhtml"
  exclude-result-prefixes="html" 
    xmlns:aml="http://schemas.microsoft.com/aml/2001/core" 
    xmlns:dt="uuid:C2F41010-65B3-11d1-A29F-00AA00C14882" 
    xmlns:o="urn:schemas-microsoft-com:office:office" 
    xmlns:sl="http://schemas.microsoft.com/schemaLibrary/2003/core" 
    xmlns:tei="http://www.tei-c.org/ns/1.0"
    xmlns:v="urn:schemas-microsoft-com:vml" 
    xmlns:w10="urn:schemas-microsoft-com:office:word" 
    xmlns:w="http://schemas.microsoft.com/office/word/2003/wordml" 
    xmlns:wx="http://schemas.microsoft.com/office/word/2003/auxHint" 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    version="1.0">

  <xsl:output indent="yes"/>

  <xsl:template match="/">
    <w:wordDocument>
      <xsl:attribute name="xml:space">preserve</xsl:attribute>
      <o:DocumentProperties>
        <o:Title><xsl:value-of select="/TEI/teiHeader/fileDesc/titleStmt/title"/></o:Title>
      </o:DocumentProperties>
      <o:CustomDocumentProperties>
        <xsl:if test="processing-instruction()">
          <o:processingInstructions dt:dt="string">
            <xsl:for-each select="processing-instruction()">
              <xsl:text>&lt;?</xsl:text>
              <xsl:value-of select="name()"/>
              <xsl:text> </xsl:text>
              <xsl:value-of select="."/>
              <xsl:text>?&gt;</xsl:text>
            </xsl:for-each>
          </o:processingInstructions>
        </xsl:if>
      </o:CustomDocumentProperties>
      <w:fonts>
        <w:defaultFonts w:ascii="Times New Roman" w:fareast="Times New Roman" w:h-ansi="Times New Roman" w:cs="Times New Roman"/>
        <w:font w:name="Arial Unicode MS">
          <w:panose-1 w:val="020B0604020202020204"/>
          <w:charset w:val="00"/>
          <w:family w:val="Roman"/>
          <w:notTrueType/>
          <w:pitch w:val="variable"/>
          <w:sig w:usb-0="00000003" w:usb-1="00000000" w:usb-2="00000000" w:usb-3="00000000" w:csb-0="00000001" w:csb-1="00000000"/>
        </w:font>
        <w:font w:name="Tahoma">
          <w:panose-1 w:val="020B0604030504040204"/>
          <w:charset w:val="00"/>
          <w:family w:val="Swiss"/>
          <w:pitch w:val="variable"/>
          <w:sig w:usb-0="21007A87" w:usb-1="80000000" w:usb-2="00000008" w:usb-3="00000000" w:csb-0="000101FF" w:csb-1="00000000"/>
        </w:font>
        <w:font w:name="sans-serif">
          <w:altName w:val="Times New Roman"/>
          <w:panose-1 w:val="00000000000000000000"/>
          <w:charset w:val="00"/>
          <w:family w:val="Roman"/>
          <w:notTrueType/>
          <w:pitch w:val="default"/>
          <w:sig w:usb-0="00000000" w:usb-1="00000000" w:usb-2="00000000" w:usb-3="00000000" w:csb-0="00000000" w:csb-1="00000000"/>
        </w:font>
        <w:font w:name="Thorndale">
          <w:altName w:val="Times New Roman"/>
          <w:charset w:val="00"/>
          <w:family w:val="Roman"/>
          <w:pitch w:val="variable"/>
          <w:sig w:usb-0="00000003" w:usb-1="00000000" w:usb-2="00000000" w:usb-3="00000000" w:csb-0="00000001" w:csb-1="00000000"/>
        </w:font>
        <w:font w:name="Albany">
          <w:altName w:val="Arial"/>
          <w:charset w:val="00"/>
          <w:family w:val="Swiss"/>
          <w:pitch w:val="variable"/>
          <w:sig w:usb-0="00000003" w:usb-1="00000000" w:usb-2="00000000" w:usb-3="00000000" w:csb-0="00000001" w:csb-1="00000000"/>
        </w:font>
        <w:font w:name="StarSymbol">
          <w:altName w:val="Arial Unicode MS"/>
          <w:charset w:val="02"/>
          <w:family w:val="Auto"/>
          <w:pitch w:val="default"/>
          <w:sig w:usb-0="00000000" w:usb-1="00000000" w:usb-2="00000000" w:usb-3="00000000" w:csb-0="00000000" w:csb-1="00000000"/>
        </w:font>
        <w:font w:name="monospace">
          <w:charset w:val="00"/>
          <w:family w:val="Auto"/>
          <w:pitch w:val="default"/>
          <w:sig w:usb-0="00000003" w:usb-1="00000000" w:usb-2="00000000" w:usb-3="00000000" w:csb-0="00000001" w:csb-1="00000000"/>
        </w:font>
      </w:fonts>
      <w:lists>
        <w:listDef w:listDefId="0">
          <w:lsid w:val="00000001"/>
          <w:plt w:val="Multilevel"/>
          <w:tmpl w:val="00000001"/>
          <w:lvl w:ilvl="0">
            <w:start w:val="1"/>
            <w:nfc w:val="23"/>
            <w:suff w:val="Nothing"/>
            <w:lvlText w:val="•"/>
            <w:lvlJc w:val="left"/>
            <w:pPr>
              <w:ind w:left="707" w:hanging="283"/>
            </w:pPr>
            <w:rPr>
              <w:rFonts w:ascii="StarSymbol" w:h-ansi="StarSymbol"/>
              <w:sz w:val="18"/>
            </w:rPr>
          </w:lvl>
          <w:lvl w:ilvl="1">
            <w:start w:val="1"/>
            <w:nfc w:val="23"/>
            <w:suff w:val="Nothing"/>
            <w:lvlText w:val="•"/>
            <w:lvlJc w:val="left"/>
            <w:pPr>
              <w:ind w:left="1414" w:hanging="283"/>
            </w:pPr>
            <w:rPr>
              <w:rFonts w:ascii="StarSymbol" w:h-ansi="StarSymbol"/>
              <w:sz w:val="18"/>
            </w:rPr>
          </w:lvl>
          <w:lvl w:ilvl="2">
            <w:start w:val="1"/>
            <w:nfc w:val="23"/>
            <w:suff w:val="Nothing"/>
            <w:lvlText w:val="•"/>
            <w:lvlJc w:val="left"/>
            <w:pPr>
              <w:ind w:left="2121" w:hanging="283"/>
            </w:pPr>
            <w:rPr>
              <w:rFonts w:ascii="StarSymbol" w:h-ansi="StarSymbol"/>
              <w:sz w:val="18"/>
            </w:rPr>
          </w:lvl>
          <w:lvl w:ilvl="3">
            <w:start w:val="1"/>
            <w:nfc w:val="23"/>
            <w:suff w:val="Nothing"/>
            <w:lvlText w:val="•"/>
            <w:lvlJc w:val="left"/>
            <w:pPr>
              <w:ind w:left="2828" w:hanging="283"/>
            </w:pPr>
            <w:rPr>
              <w:rFonts w:ascii="StarSymbol" w:h-ansi="StarSymbol"/>
              <w:sz w:val="18"/>
            </w:rPr>
          </w:lvl>
          <w:lvl w:ilvl="4">
            <w:start w:val="1"/>
            <w:nfc w:val="23"/>
            <w:suff w:val="Nothing"/>
            <w:lvlText w:val="•"/>
            <w:lvlJc w:val="left"/>
            <w:pPr>
              <w:ind w:left="3535" w:hanging="283"/>
            </w:pPr>
            <w:rPr>
              <w:rFonts w:ascii="StarSymbol" w:h-ansi="StarSymbol"/>
              <w:sz w:val="18"/>
            </w:rPr>
          </w:lvl>
          <w:lvl w:ilvl="5">
            <w:start w:val="1"/>
            <w:nfc w:val="23"/>
            <w:suff w:val="Nothing"/>
            <w:lvlText w:val="•"/>
            <w:lvlJc w:val="left"/>
            <w:pPr>
              <w:ind w:left="4242" w:hanging="283"/>
            </w:pPr>
            <w:rPr>
              <w:rFonts w:ascii="StarSymbol" w:h-ansi="StarSymbol"/>
              <w:sz w:val="18"/>
            </w:rPr>
          </w:lvl>
          <w:lvl w:ilvl="6">
            <w:start w:val="1"/>
            <w:nfc w:val="23"/>
            <w:suff w:val="Nothing"/>
            <w:lvlText w:val="•"/>
            <w:lvlJc w:val="left"/>
            <w:pPr>
              <w:ind w:left="4949" w:hanging="283"/>
            </w:pPr>
            <w:rPr>
              <w:rFonts w:ascii="StarSymbol" w:h-ansi="StarSymbol"/>
              <w:sz w:val="18"/>
            </w:rPr>
          </w:lvl>
          <w:lvl w:ilvl="7">
            <w:start w:val="1"/>
            <w:nfc w:val="23"/>
            <w:suff w:val="Nothing"/>
            <w:lvlText w:val="•"/>
            <w:lvlJc w:val="left"/>
            <w:pPr>
              <w:ind w:left="5656" w:hanging="283"/>
            </w:pPr>
            <w:rPr>
              <w:rFonts w:ascii="StarSymbol" w:h-ansi="StarSymbol"/>
              <w:sz w:val="18"/>
            </w:rPr>
          </w:lvl>
          <w:lvl w:ilvl="8">
            <w:start w:val="1"/>
            <w:nfc w:val="23"/>
            <w:suff w:val="Nothing"/>
            <w:lvlText w:val="•"/>
            <w:lvlJc w:val="left"/>
            <w:pPr>
              <w:ind w:left="6363" w:hanging="283"/>
            </w:pPr>
            <w:rPr>
              <w:rFonts w:ascii="StarSymbol" w:h-ansi="StarSymbol"/>
              <w:sz w:val="18"/>
            </w:rPr>
          </w:lvl>
        </w:listDef>
        <w:list w:ilfo="1">
          <w:ilst w:val="0"/>
        </w:list>
      </w:lists>
      <w:styles>
        <w:versionOfBuiltInStylenames w:val="4"/>
        <w:latentStyles w:defLockedState="off" w:latentStyleCount="156"/>
        <w:style w:type="paragraph" w:default="on" w:styleId="Normal">
          <w:name w:val="Normal"/>
          <w:rsid w:val="00C843FA"/>
          <w:rPr>
            <wx:font wx:val="Times New Roman"/>
            <w:sz w:val="24"/>
            <w:sz-cs w:val="24"/>
            <w:lang w:val="EN-GB" w:fareast="EN-GB" w:bidi="AR-SA"/>
          </w:rPr>
        </w:style>
        <w:style w:type="paragraph" w:styleId="Heading1">
          <w:name w:val="heading 1"/>
          <wx:uiName wx:val="Heading 1"/>
          <w:basedOn w:val="Heading"/>
          <w:next w:val="Textbody"/>
          <w:rsid w:val="00C843FA"/>
          <w:pPr>
            <w:pStyle w:val="Heading1"/>
            <w:keepNext w:val="off"/>
            <w:spacing w:before="0" w:after="0"/>
            <w:outlineLvl w:val="0"/>
          </w:pPr>
          <w:rPr>
            <w:rFonts w:ascii="Thorndale"/>
            <wx:font wx:val="Times New Roman"/>
            <w:b/>
            <w:b-cs/>
            <w:sz w:val="48"/>
            <w:sz-cs w:val="48"/>
          </w:rPr>
        </w:style>
        <w:style w:type="paragraph" w:styleId="Heading2">
          <w:name w:val="heading 2"/>
          <wx:uiName wx:val="Heading 2"/>
          <w:basedOn w:val="Heading"/>
          <w:next w:val="Textbody"/>
          <w:rsid w:val="00C843FA"/>
          <w:pPr>
            <w:pStyle w:val="Heading2"/>
            <w:keepNext w:val="off"/>
            <w:spacing w:before="0" w:after="0"/>
            <w:outlineLvl w:val="1"/>
          </w:pPr>
          <w:rPr>
            <w:rFonts w:ascii="Arial" w:cs="Arial"/>
            <wx:font wx:val="Times New Roman"/>
            <w:b/>
            <w:b-cs/>
            <w:color w:val="000000"/>
            <w:sz w:val="36"/>
            <w:sz-cs w:val="36"/>
          </w:rPr>
        </w:style>
        <w:style w:type="paragraph" w:styleId="Heading3">
          <w:name w:val="heading 3"/>
          <wx:uiName wx:val="Heading 3"/>
          <w:basedOn w:val="Heading"/>
          <w:next w:val="Textbody"/>
          <w:rsid w:val="00C843FA"/>
          <w:pPr>
            <w:pStyle w:val="Heading3"/>
            <w:keepNext w:val="off"/>
            <w:spacing w:before="0" w:after="0"/>
            <w:outlineLvl w:val="2"/>
          </w:pPr>
          <w:rPr>
            <w:rFonts w:ascii="Arial" w:cs="Arial"/>
            <wx:font wx:val="Times New Roman"/>
            <w:b/>
            <w:b-cs/>
          </w:rPr>
        </w:style>
        <w:style w:type="character" w:default="on" w:styleId="DefaultParagraphFont">
          <w:name w:val="Default Paragraph Font"/>
          <w:semiHidden/>
        </w:style>
        <w:style w:type="table" w:default="on" w:styleId="TableNormal">
          <w:name w:val="Normal Table"/>
          <wx:uiName wx:val="Table Normal"/>
          <w:semiHidden/>
          <w:rPr>
            <wx:font wx:val="Times New Roman"/>
          </w:rPr>
          <w:tblPr>
            <w:tblInd w:w="0" w:type="dxa"/>
            <w:tblCellMar>
              <w:top w:w="0" w:type="dxa"/>
              <w:left w:w="108" w:type="dxa"/>
              <w:bottom w:w="0" w:type="dxa"/>
              <w:right w:w="108" w:type="dxa"/>
            </w:tblCellMar>
          </w:tblPr>
        </w:style>
        <w:style w:type="list" w:default="on" w:styleId="NoList">
          <w:name w:val="No List"/>
          <w:semiHidden/>
        </w:style>
        <w:style w:type="paragraph" w:styleId="Default">
          <w:name w:val="Default"/>
          <w:rsid w:val="00C843FA"/>
          <w:pPr>
            <w:pStyle w:val="Default"/>
            <w:widowControl w:val="off"/>
            <w:autoSpaceDN w:val="off"/>
            <w:adjustRightInd w:val="off"/>
          </w:pPr>
          <w:rPr>
            <w:rFonts w:ascii="sans-serif" w:cs="sans-serif"/>
            <wx:font wx:val="Times New Roman"/>
            <w:sz w:val="24"/>
            <w:sz-cs w:val="24"/>
            <w:lang w:val="EN-US" w:fareast="EN-GB" w:bidi="AR-SA"/>
          </w:rPr>
        </w:style>
        <w:style w:type="paragraph" w:styleId="Textbody">
          <w:name w:val="Text body"/>
          <w:basedOn w:val="Default"/>
          <w:rsid w:val="00C843FA"/>
          <w:pPr>
            <w:pStyle w:val="Textbody"/>
            <w:spacing w:after="283"/>
          </w:pPr>
          <w:rPr>
            <wx:font wx:val="Times New Roman"/>
          </w:rPr>
        </w:style>
        <w:style w:type="paragraph" w:styleId="Heading">
          <w:name w:val="Heading"/>
          <w:basedOn w:val="Default"/>
          <w:next w:val="Textbody"/>
          <w:rsid w:val="00C843FA"/>
          <w:pPr>
            <w:pStyle w:val="Heading"/>
            <w:keepNext/>
            <w:spacing w:before="240" w:after="283"/>
          </w:pPr>
          <w:rPr>
            <w:rFonts w:ascii="Albany" w:cs="Arial Unicode MS"/>
            <wx:font wx:val="Times New Roman"/>
            <w:sz w:val="28"/>
            <w:sz-cs w:val="28"/>
          </w:rPr>
        </w:style>
        <w:style w:type="paragraph" w:styleId="List">
          <w:name w:val="List"/>
          <w:basedOn w:val="Textbody"/>
          <w:rsid w:val="00C843FA"/>
          <w:pPr>
            <w:pStyle w:val="List"/>
            <w:spacing w:after="0"/>
          </w:pPr>
          <w:rPr>
            <w:rFonts w:ascii="Times New Roman" w:cs="Tahoma"/>
            <wx:font wx:val="Times New Roman"/>
          </w:rPr>
        </w:style>
        <w:style w:type="paragraph" w:styleId="Header">
          <w:name w:val="header"/>
          <wx:uiName wx:val="Header"/>
          <w:basedOn w:val="Default"/>
          <w:rsid w:val="00C843FA"/>
          <w:pPr>
            <w:pStyle w:val="Header"/>
            <w:tabs>
              <w:tab w:val="center" w:pos="4818"/>
              <w:tab w:val="right" w:pos="9637"/>
            </w:tabs>
          </w:pPr>
          <w:rPr>
            <w:rFonts w:ascii="Times New Roman" w:cs="Tahoma"/>
            <wx:font wx:val="Times New Roman"/>
          </w:rPr>
        </w:style>
        <w:style w:type="paragraph" w:styleId="Footer">
          <w:name w:val="footer"/>
          <wx:uiName wx:val="Footer"/>
          <w:basedOn w:val="Default"/>
          <w:rsid w:val="00C843FA"/>
          <w:pPr>
            <w:pStyle w:val="Footer"/>
            <w:tabs>
              <w:tab w:val="center" w:pos="4818"/>
              <w:tab w:val="right" w:pos="9637"/>
            </w:tabs>
          </w:pPr>
          <w:rPr>
            <w:rFonts w:ascii="Times New Roman" w:cs="Tahoma"/>
            <wx:font wx:val="Times New Roman"/>
          </w:rPr>
        </w:style>
        <w:style w:type="paragraph" w:styleId="TableContents">
          <w:name w:val="Table Contents"/>
          <w:basedOn w:val="Textbody"/>
          <w:rsid w:val="00C843FA"/>
          <w:pPr>
            <w:pStyle w:val="TableContents"/>
            <w:spacing w:after="0"/>
          </w:pPr>
          <w:rPr>
            <wx:font wx:val="Times New Roman"/>
          </w:rPr>
        </w:style>
        <w:style w:type="paragraph" w:styleId="TableHeading">
          <w:name w:val="Table Heading"/>
          <w:basedOn w:val="TableContents"/>
          <w:rsid w:val="00C843FA"/>
          <w:pPr>
            <w:pStyle w:val="TableHeading"/>
            <w:jc w:val="center"/>
          </w:pPr>
          <w:rPr>
            <w:rFonts w:ascii="Times New Roman" w:cs="Tahoma"/>
            <wx:font wx:val="Times New Roman"/>
            <w:b/>
            <w:b-cs/>
          </w:rPr>
        </w:style>
        <w:style w:type="paragraph" w:styleId="Caption">
          <w:name w:val="caption"/>
          <wx:uiName wx:val="Caption"/>
          <w:basedOn w:val="Default"/>
          <w:semiHidden/>
          <w:rsid w:val="00C843FA"/>
          <w:pPr>
            <w:pStyle w:val="Caption"/>
            <w:spacing w:before="120" w:after="120"/>
          </w:pPr>
          <w:rPr>
            <w:rFonts w:ascii="Times New Roman" w:cs="Tahoma"/>
            <wx:font wx:val="Times New Roman"/>
            <w:i/>
            <w:i-cs/>
            <w:sz w:val="20"/>
            <w:sz-cs w:val="20"/>
          </w:rPr>
        </w:style>
        <w:style w:type="paragraph" w:styleId="Sender">
          <w:name w:val="Sender"/>
          <w:basedOn w:val="Default"/>
          <w:rsid w:val="00C843FA"/>
          <w:pPr>
            <w:pStyle w:val="Sender"/>
          </w:pPr>
          <w:rPr>
            <w:rFonts w:ascii="Times New Roman" w:cs="Tahoma"/>
            <wx:font wx:val="Times New Roman"/>
          </w:rPr>
        </w:style>
        <w:style w:type="paragraph" w:styleId="Index">
          <w:name w:val="Index"/>
          <w:basedOn w:val="Default"/>
          <w:rsid w:val="00C843FA"/>
          <w:pPr>
            <w:pStyle w:val="Index"/>
          </w:pPr>
          <w:rPr>
            <w:rFonts w:ascii="Times New Roman" w:cs="Tahoma"/>
            <wx:font wx:val="Times New Roman"/>
          </w:rPr>
        </w:style>
        <w:style w:type="paragraph" w:styleId="Quotations">
          <w:name w:val="Quotations"/>
          <w:basedOn w:val="Default"/>
          <w:rsid w:val="00C843FA"/>
          <w:pPr>
            <w:pStyle w:val="Quotations"/>
            <w:spacing w:after="283"/>
            <w:ind w:left="567" w:right="567"/>
          </w:pPr>
          <w:rPr>
            <w:rFonts w:ascii="Times New Roman" w:cs="Tahoma"/>
            <wx:font wx:val="Times New Roman"/>
            <w:color w:val="000000"/>
          </w:rPr>
        </w:style>
        <w:style w:type="paragraph" w:styleId="HorizontalLine">
          <w:name w:val="Horizontal Line"/>
          <w:basedOn w:val="Default"/>
          <w:next w:val="Textbody"/>
          <w:rsid w:val="00C843FA"/>
          <w:pPr>
            <w:pStyle w:val="HorizontalLine"/>
            <w:pBdr>
              <w:bottom w:val="double" w:sz="6" wx:bdrwidth="45" w:space="0" w:color="808080"/>
            </w:pBdr>
            <w:spacing w:after="283"/>
          </w:pPr>
          <w:rPr>
            <w:rFonts w:ascii="Times New Roman" w:cs="Tahoma"/>
            <wx:font wx:val="Times New Roman"/>
            <w:sz w:val="12"/>
          </w:rPr>
        </w:style>
        <w:style w:type="paragraph" w:styleId="ListContents">
          <w:name w:val="List Contents"/>
          <w:basedOn w:val="Default"/>
          <w:rsid w:val="00C843FA"/>
          <w:pPr>
            <w:pStyle w:val="ListContents"/>
            <w:ind w:left="567"/>
          </w:pPr>
          <w:rPr>
            <wx:font wx:val="Times New Roman"/>
          </w:rPr>
        </w:style>
        <w:style w:type="paragraph" w:styleId="ListHeading">
          <w:name w:val="List Heading"/>
          <w:basedOn w:val="Default"/>
          <w:next w:val="ListContents"/>
          <w:rsid w:val="00C843FA"/>
          <w:pPr>
            <w:pStyle w:val="ListHeading"/>
          </w:pPr>
          <w:rPr>
            <wx:font wx:val="Times New Roman"/>
          </w:rPr>
        </w:style>
        <w:style w:type="paragraph" w:styleId="Textbodysidebar">
          <w:name w:val="Text body.sidebar"/>
          <w:basedOn w:val="Textbody"/>
          <w:rsid w:val="00C843FA"/>
          <w:pPr>
            <w:pStyle w:val="Textbodysidebar"/>
            <w:spacing w:before="40"/>
          </w:pPr>
          <w:rPr>
            <w:rFonts w:ascii="Times New Roman" w:cs="Tahoma"/>
            <wx:font wx:val="Times New Roman"/>
          </w:rPr>
        </w:style>
        <w:style w:type="paragraph" w:styleId="Textbodyframetoc">
          <w:name w:val="Text body.frametoc"/>
          <w:basedOn w:val="Textbody"/>
          <w:rsid w:val="00C843FA"/>
          <w:pPr>
            <w:pStyle w:val="Textbodyframetoc"/>
            <w:spacing w:before="20" w:after="40"/>
          </w:pPr>
          <w:rPr>
            <w:rFonts w:ascii="Times New Roman" w:cs="Tahoma"/>
            <wx:font wx:val="Times New Roman"/>
          </w:rPr>
        </w:style>
        <w:style w:type="paragraph" w:styleId="Textbodyframetoc-this">
          <w:name w:val="Text body.frametoc-this"/>
          <w:basedOn w:val="Textbody"/>
          <w:rsid w:val="00C843FA"/>
          <w:pPr>
            <w:pStyle w:val="Textbodyframetoc-this"/>
            <w:spacing w:before="20" w:after="40"/>
          </w:pPr>
          <w:rPr>
            <w:rFonts w:ascii="Times New Roman" w:cs="Tahoma"/>
            <wx:font wx:val="Times New Roman"/>
          </w:rPr>
        </w:style>
        <w:style w:type="paragraph" w:styleId="Textbodyframetoc-sub">
          <w:name w:val="Text body.frametoc-sub"/>
          <w:basedOn w:val="Textbody"/>
          <w:rsid w:val="00C843FA"/>
          <w:pPr>
            <w:pStyle w:val="Textbodyframetoc-sub"/>
            <w:spacing w:before="20" w:after="40"/>
          </w:pPr>
          <w:rPr>
            <w:rFonts w:ascii="Times New Roman" w:cs="Tahoma"/>
            <wx:font wx:val="Times New Roman"/>
            <w:color w:val="FFCC00"/>
          </w:rPr>
        </w:style>
        <w:style w:type="paragraph" w:styleId="Textbodyrss">
          <w:name w:val="Text body.rss"/>
          <w:basedOn w:val="Textbody"/>
          <w:rsid w:val="00C843FA"/>
          <w:pPr>
            <w:pStyle w:val="Textbodyrss"/>
            <w:spacing w:before="20" w:after="40"/>
          </w:pPr>
          <w:rPr>
            <w:rFonts w:ascii="Times New Roman" w:cs="Tahoma"/>
            <wx:font wx:val="Times New Roman"/>
          </w:rPr>
        </w:style>
        <w:style w:type="paragraph" w:styleId="Heading1maintitle">
          <w:name w:val="Heading 1.maintitle"/>
          <w:basedOn w:val="Heading1"/>
          <w:rsid w:val="00C843FA"/>
          <w:pPr>
            <w:pStyle w:val="Heading1maintitle"/>
            <w:spacing w:before="495" w:after="283"/>
            <w:ind w:left="1500"/>
            <w:outlineLvl w:val="9"/>
          </w:pPr>
          <w:rPr>
            <w:rFonts w:ascii="Arial" w:cs="Arial"/>
            <wx:font wx:val="Times New Roman"/>
            <w:b w:val="off"/>
            <w:b-cs w:val="off"/>
            <w:color w:val="FFFFFF"/>
            <w:sz w:val="24"/>
            <w:sz-cs w:val="24"/>
          </w:rPr>
        </w:style>
        <w:style w:type="paragraph" w:styleId="Heading1maintitlewhite">
          <w:name w:val="Heading 1.maintitlewhite"/>
          <w:basedOn w:val="Heading1"/>
          <w:rsid w:val="00C843FA"/>
          <w:pPr>
            <w:pStyle w:val="Heading1maintitlewhite"/>
            <w:ind w:left="1500"/>
            <w:outlineLvl w:val="9"/>
          </w:pPr>
          <w:rPr>
            <w:rFonts w:ascii="Arial" w:cs="Arial"/>
            <wx:font wx:val="Times New Roman"/>
            <w:b w:val="off"/>
            <w:b-cs w:val="off"/>
            <w:color w:val="FFFFFF"/>
            <w:sz w:val="24"/>
            <w:sz-cs w:val="24"/>
          </w:rPr>
        </w:style>
        <w:style w:type="paragraph" w:styleId="Heading2subtitle">
          <w:name w:val="Heading 2.subtitle"/>
          <w:basedOn w:val="Heading2"/>
          <w:rsid w:val="00C843FA"/>
          <w:pPr>
            <w:pStyle w:val="Heading2subtitle"/>
            <w:ind w:left="1500"/>
            <w:outlineLvl w:val="9"/>
          </w:pPr>
          <w:rPr>
            <wx:font wx:val="Times New Roman"/>
            <w:b w:val="off"/>
            <w:b-cs w:val="off"/>
            <w:color w:val="FFFFFF"/>
            <w:sz w:val="24"/>
            <w:sz-cs w:val="24"/>
          </w:rPr>
        </w:style>
        <w:style w:type="character" w:styleId="Bullets">
          <w:name w:val="Bullets"/>
          <w:rsid w:val="00C843FA"/>
          <w:rPr>
            <w:rFonts w:ascii="StarSymbol" w:cs="StarSymbol"/>
            <w:sz w:val="18"/>
            <w:sz-cs w:val="18"/>
            <w:lang w:val="EN-US"/>
          </w:rPr>
        </w:style>
        <w:style w:type="character" w:styleId="InternetLink">
          <w:name w:val="Internet Link"/>
          <w:rsid w:val="00C843FA"/>
          <w:rPr>
            <w:rFonts w:cs="Tahoma"/>
            <w:color w:val="000080"/>
            <w:u w:val="single"/>
            <w:lang w:val="EN-US"/>
          </w:rPr>
        </w:style>
        <w:style w:type="character" w:styleId="StrongEmphasis">
          <w:name w:val="Strong Emphasis"/>
          <w:rsid w:val="00C843FA"/>
          <w:rPr>
            <w:rFonts w:cs="Tahoma"/>
            <w:b/>
            <w:b-cs/>
            <w:lang w:val="EN-US"/>
          </w:rPr>
        </w:style>
        <w:style w:type="character" w:styleId="SourceText">
          <w:name w:val="Source Text"/>
          <w:rsid w:val="00C843FA"/>
          <w:rPr>
            <w:rFonts w:ascii="Courier New" w:cs="Courier New"/>
            <w:lang w:val="EN-US"/>
          </w:rPr>
        </w:style>
        <w:style w:type="character" w:styleId="SourceTextButton">
          <w:name w:val="Source Text.Button"/>
          <w:basedOn w:val="SourceText"/>
          <w:rsid w:val="00C843FA"/>
          <w:rPr>
            <w:rFonts w:ascii="monospace" w:cs="monospace"/>
            <w:b/>
            <w:b-cs/>
            <w:color w:val="FF0000"/>
            <w:shd w:val="clear" w:color="auto" w:fill="BFBFBF"/>
            <w:lang w:val="EN-US"/>
          </w:rPr>
        </w:style>
        <w:style w:type="character" w:styleId="SourceTextCommand">
          <w:name w:val="Source Text.Command"/>
          <w:basedOn w:val="SourceText"/>
          <w:rsid w:val="00C843FA"/>
          <w:rPr>
            <w:rFonts w:ascii="monospace" w:cs="monospace"/>
            <w:b/>
            <w:b-cs/>
            <w:color w:val="0000FF"/>
            <w:lang w:val="EN-US"/>
          </w:rPr>
        </w:style>
        <w:style w:type="character" w:styleId="SourceTextValue">
          <w:name w:val="Source Text.Value"/>
          <w:basedOn w:val="SourceText"/>
          <w:rsid w:val="00C843FA"/>
          <w:rPr>
            <w:rFonts w:ascii="monospace" w:cs="monospace"/>
            <w:b/>
            <w:b-cs/>
            <w:color w:val="008000"/>
            <w:lang w:val="EN-US"/>
          </w:rPr>
        </w:style>
        <w:style w:type="character" w:styleId="SourceTextCode">
          <w:name w:val="Source Text.Code"/>
          <w:basedOn w:val="SourceText"/>
          <w:rsid w:val="00C843FA"/>
          <w:rPr>
            <w:rFonts w:ascii="monospace" w:cs="monospace"/>
            <w:b/>
            <w:b-cs/>
            <w:color w:val="FF0000"/>
            <w:lang w:val="EN-US"/>
          </w:rPr>
        </w:style>
        <w:style w:type="character" w:styleId="SourceTextField">
          <w:name w:val="Source Text.Field"/>
          <w:basedOn w:val="SourceText"/>
          <w:rsid w:val="00C843FA"/>
          <w:rPr>
            <w:rFonts w:ascii="monospace" w:cs="monospace"/>
            <w:b/>
            <w:b-cs/>
            <w:color w:val="FF0000"/>
            <w:lang w:val="EN-US"/>
          </w:rPr>
        </w:style>
        <w:style w:type="character" w:styleId="SourceTextFilespec">
          <w:name w:val="Source Text.Filespec"/>
          <w:basedOn w:val="SourceText"/>
          <w:rsid w:val="00C843FA"/>
          <w:rPr>
            <w:rFonts w:ascii="monospace" w:cs="monospace"/>
            <w:b/>
            <w:b-cs/>
            <w:color w:val="FF0000"/>
            <w:lang w:val="EN-US"/>
          </w:rPr>
        </w:style>
        <w:style w:type="character" w:styleId="SourceTextInput">
          <w:name w:val="Source Text.Input"/>
          <w:basedOn w:val="SourceText"/>
          <w:rsid w:val="00C843FA"/>
          <w:rPr>
            <w:rFonts w:ascii="monospace" w:cs="monospace"/>
            <w:b/>
            <w:b-cs/>
            <w:color w:val="008000"/>
            <w:lang w:val="EN-US"/>
          </w:rPr>
        </w:style>
        <w:style w:type="character" w:styleId="SourceTextKey">
          <w:name w:val="Source Text.Key"/>
          <w:basedOn w:val="SourceText"/>
          <w:rsid w:val="00C843FA"/>
          <w:rPr>
            <w:rFonts w:ascii="monospace" w:cs="monospace"/>
            <w:b/>
            <w:b-cs/>
            <w:color w:val="008000"/>
            <w:u w:val="single"/>
            <w:lang w:val="EN-US"/>
          </w:rPr>
        </w:style>
        <w:style w:type="character" w:styleId="SourceTextLink">
          <w:name w:val="Source Text.Link"/>
          <w:basedOn w:val="SourceText"/>
          <w:rsid w:val="00C843FA"/>
          <w:rPr>
            <w:rFonts w:ascii="monospace" w:cs="monospace"/>
            <w:b/>
            <w:b-cs/>
            <w:color w:val="008000"/>
            <w:u w:val="single"/>
            <w:lang w:val="EN-US"/>
          </w:rPr>
        </w:style>
        <w:style w:type="character" w:styleId="SourceTextMenu">
          <w:name w:val="Source Text.Menu"/>
          <w:basedOn w:val="SourceText"/>
          <w:rsid w:val="00C843FA"/>
          <w:rPr>
            <w:rFonts w:ascii="monospace" w:cs="monospace"/>
            <w:b/>
            <w:b-cs/>
            <w:color w:val="FF0000"/>
            <w:lang w:val="EN-US"/>
          </w:rPr>
        </w:style>
        <w:style w:type="character" w:styleId="SourceTextKeyword">
          <w:name w:val="Source Text.Keyword"/>
          <w:basedOn w:val="SourceText"/>
          <w:rsid w:val="00C843FA"/>
          <w:rPr>
            <w:rFonts w:ascii="monospace" w:cs="monospace"/>
            <w:b/>
            <w:b-cs/>
            <w:color w:val="0000FF"/>
            <w:lang w:val="EN-US"/>
          </w:rPr>
        </w:style>
        <w:style w:type="character" w:styleId="SourceTextOutput">
          <w:name w:val="Source Text.Output"/>
          <w:basedOn w:val="SourceText"/>
          <w:rsid w:val="00C843FA"/>
          <w:rPr>
            <w:rFonts w:ascii="monospace" w:cs="monospace"/>
            <w:b/>
            <w:b-cs/>
            <w:color w:val="FF0000"/>
            <w:lang w:val="EN-US"/>
          </w:rPr>
        </w:style>
        <w:style w:type="character" w:styleId="SourceTextScreen">
          <w:name w:val="Source Text.Screen"/>
          <w:basedOn w:val="SourceText"/>
          <w:rsid w:val="00C843FA"/>
          <w:rPr>
            <w:rFonts w:ascii="Courier New" w:cs="Tahoma"/>
            <w:shd w:val="clear" w:color="auto" w:fill="FFCC99"/>
            <w:lang w:val="EN-US"/>
          </w:rPr>
        </w:style>
        <w:style w:type="character" w:styleId="SourceTextProgram">
          <w:name w:val="Source Text.Program"/>
          <w:basedOn w:val="SourceText"/>
          <w:rsid w:val="00C843FA"/>
          <w:rPr>
            <w:rFonts w:ascii="Courier New" w:cs="Tahoma"/>
            <w:b/>
            <w:b-cs/>
            <w:lang w:val="EN-US"/>
          </w:rPr>
        </w:style>
        <w:style w:type="character" w:styleId="SourceTextIcon">
          <w:name w:val="Source Text.Icon"/>
          <w:basedOn w:val="SourceText"/>
          <w:rsid w:val="00C843FA"/>
          <w:rPr>
            <w:rFonts w:ascii="monospace" w:cs="monospace"/>
            <w:b/>
            <w:b-cs/>
            <w:u w:val="single"/>
            <w:lang w:val="EN-US"/>
          </w:rPr>
        </w:style>
        <w:style w:type="character" w:styleId="SourceTextLabel">
          <w:name w:val="Source Text.Label"/>
          <w:basedOn w:val="SourceText"/>
          <w:rsid w:val="00C843FA"/>
          <w:rPr>
            <w:rFonts w:ascii="Courier New" w:cs="Tahoma"/>
            <w:b/>
            <w:b-cs/>
            <w:i/>
            <w:i-cs/>
            <w:lang w:val="EN-US"/>
          </w:rPr>
        </w:style>
      </w:styles>
      <w:docPr>
        <w:view w:val="normal"/>
        <w:zoom w:percent="100"/>
        <w:bordersDontSurroundHeader/>
        <w:bordersDontSurroundFooter/>
        <w:proofState w:spelling="clean" w:grammar="clean"/>
        <w:attachedTemplate w:val=""/>
        <w:defaultTabStop w:val="1134"/>
        <w:doNotHyphenateCaps/>
        <w:drawingGridHorizontalSpacing w:val="120"/>
        <w:drawingGridVerticalSpacing w:val="120"/>
        <w:displayHorizontalDrawingGridEvery w:val="0"/>
        <w:displayVerticalDrawingGridEvery w:val="3"/>
        <w:useMarginsForDrawingGridOrigin/>
        <w:doNotShadeFormData/>
        <w:punctuationKerning/>
        <w:characterSpacingControl w:val="CompressPunctuation"/>
        <w:optimizeForBrowser/>
        <w:validateAgainstSchema/>
        <w:ignoreMixedContent w:val="off"/>
        <w:alwaysShowPlaceholderText w:val="off"/>
        <w:doNotUnderlineInvalidXML/>
        <w:compat>
          <w:footnoteLayoutLikeWW8/>
          <w:shapeLayoutLikeWW8/>
          <w:alignTablesRowByRow/>
          <w:forgetLastTabAlignment/>
          <w:doNotUseHTMLParagraphAutoSpacing/>
          <w:layoutRawTableWidth/>
          <w:layoutTableRowsApart/>
          <w:useWord97LineBreakingRules/>
          <w:dontAllowFieldEndSelect/>
          <w:useWord2002TableStyleRules/>
        </w:compat>
        <w:removeWordSchemaOnSave/>
        <w:showXMLTags w:val="off"/>
<!--
        These are some XML save options you may want to set:
          <w:ignoreMixedContent/>
          <w:useXSLTWhenSaving/>
          <w:saveThroughXSLT w:xslt=""/>
          <w:saveInvalidXML/>
      -->
      </w:docPr>
      <xsl:apply-templates/>
    </w:wordDocument>
  </xsl:template>

<xsl:template match="tei:TEI">
      <w:body>
        <wx:sect>
          <tei:TEI>
            <w:p>
              <w:pPr>
                <w:pStyle w:val="Textbody"/>
                <w:spacing w:after="0"/>
                <w:rPr>
                  <wx:font wx:val="sans-serif"/>
                </w:rPr>
              </w:pPr>
            </w:p>
            <wx:pBdrGroup>
              <wx:borders>
                <wx:bottom wx:val="double" wx:bdrwidth="45" wx:space="0" wx:color="808080"/>
              </wx:borders>
              <tei:teiHeader>
                <tei:fileDesc>
                  <w:p>
                    <w:pPr>
                      <w:pStyle w:val="HorizontalLine"/>
                    </w:pPr>
                    <tei:titleStmt>
                      <tei:title>
                        <w:r>
                          <w:rPr>
                            <w:rFonts w:h-ansi="Arial"/>
                            <w:sz w:val="24"/>
                          </w:rPr>
                          <w:t><xsl:value-of select="/TEI/teiHeader/fileDesc/titleStmt/title"/></w:t>
                        </w:r>
                      </tei:title>
                      <w:r>
                        <w:rPr>
                          <w:rFonts w:h-ansi="Arial"/>
                          <w:sz w:val="24"/>
                        </w:rPr>
                        <w:t>
                          <xsl:text> </xsl:text>
                        </w:t>
                      </w:r>
                      <tei:author>
                        <w:proofErr w:type="gramStart"/>
                        <w:r>
                          <w:rPr>
                            <w:rFonts w:h-ansi="Arial"/>
                            <w:sz w:val="24"/>
                          </w:rPr>
                          <w:t><xsl:value-of select="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:titleStmt/tei:author"/></w:t>
                        </w:r>
                        <w:proofErr w:type="gramEnd"/>
                      </tei:author>
                    </tei:titleStmt>
                    <tei:editionStmt>
                      <tei:edition>
                        <tei:date>
                          <w:r>
                            <w:rPr>
                              <w:rFonts w:h-ansi="Arial"/>
                              <w:sz w:val="24"/>
                            </w:rPr>
                            <w:t>
			    <xsl:value-of
				select="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt/tei:date"/>
			    </w:t>
                          </w:r>
                        </tei:date>
                      </tei:edition>
                    </tei:editionStmt>
                    <tei:publicationStmt>
                      <tei:date>
                        <w:r>
                          <w:rPr>
                            <w:rFonts w:h-ansi="Arial"/>
                            <w:sz w:val="24"/>
                          </w:rPr>
			    <xsl:value-of
				select="/tei:TEI/tei:teiHeader/tei:fileDesc/tei:publicationStmt/tei:date"/>
                          <w:t>
			  </w:t>
                        </w:r>
                      </tei:date>
                    </tei:publicationStmt>
                    <tei:sourceDesc>
                      <tei:p/>
                    </tei:sourceDesc>
                  </w:p>
                </tei:fileDesc>
              </tei:teiHeader>

	      <xsl:apply-templates  select="tei:text"/>/>

	    </wx:pBdrGroup>
	  </tei:TEI>
	</wx:sect>
      </w:body>
</xsl:template>

<xsl:template match="tei:head">
    <tei:head>
      <w:p>
	<w:pPr>
	  <w:pStyle w:val="Heading2"/>
	  <w:keepNext/>
	  <w:spacing w:before="240" w:after="283"/>
	  <w:rPr>
	    <w:rFonts w:h-ansi="Arial"/>
	    <wx:font wx:val="Arial"/>
	  </w:rPr>
	</w:pPr>
	<w:r>
	  <w:rPr>
	    <w:rFonts w:h-ansi="Arial"/>
	    <wx:font wx:val="Arial"/>
	  </w:rPr>
	  <w:t><xsl:apply-templates/></w:t>
	</w:r>
      </w:p>
    </tei:head>
</xsl:template>

<xsl:template match="tei:div">
  <w:p>
    <w:pPr>
      <w:pStyle w:val="HorizontalLine"/>
    </w:pPr>
  </w:p>
  <tei:div>
    <xsl:apply-templates/>
  </tei:div>
</xsl:template>

<xsl:template match="tei:p">
  <tei:p>
    <w:p>
      <w:pPr>
	<w:pStyle w:val="Textbody"/>
	<w:rPr>
	  <wx:font wx:val="sans-serif"/>
	</w:rPr>
	<xsl:apply-templates/>
      </w:pPr>
    </w:p>
  </tei:p>
</xsl:template>

<xsl:template match="tei:ref">
  <tei:ref target="{@target}">
    <w:hlink w:dest="{@target}" w:target="_top">
      <w:r>
	<w:rPr>
	  <w:rStyle w:val="InternetLink"/>
	  <w:rFonts w:ascii="Times New Roman" w:cs="sans-serif"/>
	</w:rPr>
	<w:t><xsl:apply-templates/></w:t>
      </w:r>
    </w:hlink>
  </tei:ref>
</xsl:template>


<xsl:template match="tei:list">
  <tei:list>
    <xsl:apply-templates/>
  </tei:list>
</xsl:template>

<xsl:template match="tei:item">
  <w:p>
    <w:pPr>
      <w:pStyle w:val="Textbody"/>
      <w:spacing w:after="0"/>
      <w:ind w:left="424"/>
      <w:rPr>
	<wx:font wx:val="sans-serif"/>
      </w:rPr>
    </w:pPr>
    <tei:item>
      <xsl:apply-templates/>
    </tei:item>
    <w:r>
      <w:rPr>
	<wx:font wx:val="sans-serif"/>
      </w:rPr>
      <w:t>
	<xsl:text> </xsl:text>
      </w:t>
    </w:r>
  </w:p>
</xsl:template>

<xsl:template match="tei:*">
<!--
<xsl:message>make a <xsl:value-of select="name(.)"/></xsl:message>
-->
  <xsl:element name="{name(.)}" namespace="http://www.tei-c.org/ns/1.0">
    <xsl:copy-of select="@*"/>
    <xsl:apply-templates/>
  </xsl:element>
</xsl:template>

<xsl:template match="text()">
  <xsl:if test="not(normalize-space(.)='')">
    <w:r>
      <w:rPr>
	<wx:font wx:val="sans-serif"/>
      </w:rPr>
      <w:t><xsl:value-of select="."/></w:t>
    </w:r>
  </xsl:if>
</xsl:template>


</xsl:stylesheet>
