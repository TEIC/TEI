<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:saxon="http://icl.com/saxon"
                xmlns:sch="http://www.ascc.net/xml/schematron"
                xmlns:xj="http://xml.apache.org/xalan/java"
                xmlns:loc="http://www.thaiopensource.com/ns/location"
                xmlns:err="http://www.thaiopensource.com/ns/error"
                xmlns:i="http://www.iso.org/ns/1.0"
                version="2.0"
                i:dummy-for-xmlns=""
                tei:dummy-for-xmlns="">
  <doc xmlns="http://www.oxygenxml.com/ns/doc/xsl" scope="stylesheet" type="stylesheet">
    <desc>
      <p> TEI stylesheet for simplifying TEI ODD markup </p>
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
   <xsl:output method="text"/>
   <xsl:template match="/">
      <xsl:apply-templates select="/" mode="all"/>
   </xsl:template>
   <xsl:template match="tei:list[@type='termlist']/tei:item" mode="M1" priority="2" name="R1.1">
      <xsl:if test="not(@n)">
         <failed-assertion test="@n">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>Each item in a termlist must have a @n attribute</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M2"/>
   </xsl:template>
   <xsl:template match="tei:list[@type='termlist']/tei:item" mode="all" priority="91">
      <xsl:call-template name="R1.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M1">
      <xsl:apply-templates select="." mode="M2"/>
   </xsl:template>
   <xsl:template match="tei:teiHeader" mode="M2" priority="2" name="R2.1">
      <xsl:if test="not(tei:fileDesc/tei:titleStmt/tei:title[@i:meta='introductoryTitle'])">
         <failed-assertion test="tei:fileDesc/tei:titleStmt/tei:title[@i:meta='introductoryTitle']">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>
		      An introductory component of the title is expected
		    </statement>
         </failed-assertion>
      </xsl:if>
      <xsl:if test="not(tei:fileDesc/tei:titleStmt/tei:title[@i:meta='mainTitle'])">
         <failed-assertion test="tei:fileDesc/tei:titleStmt/tei:title[@i:meta='mainTitle']">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>
		      A main component of the title is expected
		    </statement>
         </failed-assertion>
      </xsl:if>
      <xsl:if test="not(tei:fileDesc/tei:titleStmt/tei:respStmt/tei:name)">
         <failed-assertion test="tei:fileDesc/tei:titleStmt/tei:respStmt/tei:name">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>
		      No respStmt has been provided, giving SC/TC committee numbers
		  </statement>
         </failed-assertion>
      </xsl:if>
      <xsl:if test="not(tei:fileDesc/tei:publicationStmt/tei:idno[@i:meta='serialNumber'])">
         <failed-assertion test="tei:fileDesc/tei:publicationStmt/tei:idno[@i:meta='serialNumber']">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>
		  an idno of type serialNumber is expected</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:if test="not(tei:fileDesc/tei:publicationStmt/tei:idno[@i:meta='stage'])">
         <failed-assertion test="tei:fileDesc/tei:publicationStmt/tei:idno[@i:meta='stage']">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>
		  an idno of type stage is expected</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M3"/>
   </xsl:template>
   <xsl:template match="tei:teiHeader" mode="all" priority="90">
      <xsl:call-template name="R2.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M2">
      <xsl:apply-templates select="." mode="M3"/>
   </xsl:template>
   <xsl:template match="tei:text" mode="M3" priority="2" name="R3.1">
      <xsl:if test="not(tei:front/tei:div[@type='foreword'])">
         <failed-assertion test="tei:front/tei:div[@type='foreword']">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>
		A Foreword clause in the front matter is mandatory</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M4"/>
   </xsl:template>
   <xsl:template match="tei:text" mode="all" priority="89">
      <xsl:call-template name="R3.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M3">
      <xsl:apply-templates select="." mode="M4"/>
   </xsl:template>
   <xsl:template match="tei:text" mode="M4" priority="2" name="R4.1">
      <xsl:if test="not(tei:body/tei:div[@type='scope'])">
         <failed-assertion test="tei:body/tei:div[@type='scope']">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>
		A Scope clause in the body is mandatory</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M5"/>
   </xsl:template>
   <xsl:template match="tei:text" mode="all" priority="88">
      <xsl:call-template name="R4.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M4">
      <xsl:apply-templates select="." mode="M5"/>
   </xsl:template>
   <xsl:template match="tei:div" mode="M5" priority="2" name="R5.1">
      <xsl:if test="not(not(tei:div) or count(tei:div)&gt;1)">
         <failed-assertion test="not(tei:div) or count(tei:div)&gt;1">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>a clause must contain at least two subclauses</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M6"/>
   </xsl:template>
   <xsl:template match="tei:div" mode="all" priority="87">
      <xsl:call-template name="R5.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M5">
      <xsl:apply-templates select="." mode="M6"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='applicationSubset']" mode="M6" priority="2" name="R6.1">
      <xsl:if test="not(.=not(*))">
         <failed-assertion test=".=not(*)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The application subset must be expressed in
								plainText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M7"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='applicationSubset']" mode="all" priority="86">
      <xsl:call-template name="R6.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M6">
      <xsl:apply-templates select="." mode="M7"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='businessUnitSubset']" mode="M7" priority="2"
                 name="R7.1">
      <xsl:if test="not(.=not(*))">
         <failed-assertion test=".=not(*)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The business unit subset must be expressed
								in plainText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M8"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='businessUnitSubset']" mode="all" priority="85">
      <xsl:call-template name="R7.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M7">
      <xsl:apply-templates select="." mode="M8"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='conceptOrigin']" mode="M8" priority="2" name="R8.1">
      <xsl:if test="not(.=not(*))">
         <failed-assertion test=".=not(*)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The concept origin must be expressed in
								plainText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M9"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='conceptOrigin']" mode="all" priority="84">
      <xsl:call-template name="R8.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M8">
      <xsl:apply-templates select="." mode="M9"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='customerSubset']" mode="M9" priority="2" name="R9.1">
      <xsl:if test="not(.=not(*))">
         <failed-assertion test=".=not(*)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The customer subset must be expressed in
								plainText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M10"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='customerSubset']" mode="all" priority="83">
      <xsl:call-template name="R9.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M9">
      <xsl:apply-templates select="." mode="M10"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='databaseType']" mode="M10" priority="2" name="R10.1">
      <xsl:if test="not(.=not(*))">
         <failed-assertion test=".=not(*)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The type of data base must be expressed in
								plainText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M11"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='databaseType']" mode="all" priority="82">
      <xsl:call-template name="R10.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M10">
      <xsl:apply-templates select="." mode="M11"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='domainExpert']" mode="M11" priority="2" name="R11.1">
      <xsl:if test="not(.=not(*))">
         <failed-assertion test=".=not(*)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The domain expert must be expressed in plain
								text.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M12"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='domainExpert']" mode="all" priority="81">
      <xsl:call-template name="R11.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M11">
      <xsl:apply-templates select="." mode="M12"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='elementWorkingStatus']" mode="M12" priority="2"
                 name="R12.1">
      <xsl:if test="not(.='starterElement' or .='workingElement' or .='consolidatedElement' or .='archiveElement' or .='importedElement' or .='exportedElement')">
         <failed-assertion test=".='starterElement' or .='workingElement' or .='consolidatedElement' or .='archiveElement' or .='importedElement' or .='exportedElement'">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/> The element working status must be starterElement, workingElement,
								consolidatedElement, archiveElement, importedElement, or
								exportedElement.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M13"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='elementWorkingStatus']" mode="all" priority="80">
      <xsl:call-template name="R12.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M12">
      <xsl:apply-templates select="." mode="M13"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='entrySource']" mode="M13" priority="2" name="R13.1">
      <xsl:if test="not(.=not(*))">
         <failed-assertion test=".=not(*)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The source of the entry must be expressed in
								plainText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M14"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='entrySource']" mode="all" priority="79">
      <xsl:call-template name="R13.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M13">
      <xsl:apply-templates select="." mode="M14"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='environmentSubset']" mode="M14" priority="2"
                 name="R14.1">
      <xsl:if test="not(.=not(*))">
         <failed-assertion test=".=not(*)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The environment subset must be expressed in
								plainText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M15"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='environmentSubset']" mode="all" priority="78">
      <xsl:call-template name="R14.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M14">
      <xsl:apply-templates select="." mode="M15"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='indexHeading']" mode="M15" priority="2" name="R15.1">
      <xsl:if test="not(.=not(*))">
         <failed-assertion test=".=not(*)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The index heading must be expressed in plain
								text.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M16"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='indexHeading']" mode="all" priority="77">
      <xsl:call-template name="R15.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M15">
      <xsl:apply-templates select="." mode="M16"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='keyword']" mode="M16" priority="2" name="R16.1">
      <xsl:if test="not(.=not(*))">
         <failed-assertion test=".=not(*)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The keyword must be expressed in plain
							text.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M17"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='keyword']" mode="all" priority="76">
      <xsl:call-template name="R16.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M16">
      <xsl:apply-templates select="." mode="M17"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='originatingPerson']" mode="M17" priority="2"
                 name="R17.1">
      <xsl:if test="not(.=not(*))">
         <failed-assertion test=".=not(*)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The name of the originating person must be
								expressed in plainText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M18"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='originatingPerson']" mode="all" priority="75">
      <xsl:call-template name="R17.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M17">
      <xsl:apply-templates select="." mode="M18"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='originatingDatabase']" mode="M18" priority="2"
                 name="R18.1">
      <xsl:if test="not(.=not(*))">
         <failed-assertion test=".=not(*)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The name of the originating database must be
								expressed in plainText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M19"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='originatingDatabase']" mode="all" priority="74">
      <xsl:call-template name="R18.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M18">
      <xsl:apply-templates select="." mode="M19"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='originatingInstitution']" mode="M19" priority="2"
                 name="R19.1">
      <xsl:if test="not(.=not(*))">
         <failed-assertion test=".=not(*)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The name of the originating institution must
								be expressed in plainText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M20"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='originatingInstitution']" mode="all" priority="73">
      <xsl:call-template name="R19.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M19">
      <xsl:apply-templates select="." mode="M20"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='productSubset']" mode="M20" priority="2" name="R20.1">
      <xsl:if test="not(.=not(*))">
         <failed-assertion test=".=not(*)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The product subset must be expressed in
								plainText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M21"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='productSubset']" mode="all" priority="72">
      <xsl:call-template name="R20.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M20">
      <xsl:apply-templates select="." mode="M21"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='projectSubset']" mode="M21" priority="2" name="R21.1">
      <xsl:if test="not(.=not(*))">
         <failed-assertion test=".=not(*)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The project subset must be expressed in
								plainText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M22"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='projectSubset']" mode="all" priority="71">
      <xsl:call-template name="R21.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M21">
      <xsl:apply-templates select="." mode="M22"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='securitySubset']" mode="M22" priority="2" name="R22.1">
      <xsl:if test="not(.='public' or .='confidential')">
         <failed-assertion test=".='public' or .='confidential'">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The security subset
								must be public or confidential.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M23"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='securitySubset']" mode="all" priority="70">
      <xsl:call-template name="R22.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M22">
      <xsl:apply-templates select="." mode="M23"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='searchTerm']" mode="M23" priority="2" name="R23.1">
      <xsl:if test="not(.=not(*))">
         <failed-assertion test=".=not(*)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The search term must be expressed in plain
								text.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M24"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='searchTerm']" mode="all" priority="69">
      <xsl:call-template name="R23.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M23">
      <xsl:apply-templates select="." mode="M24"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='sourceIdentifier']" mode="M24" priority="2"
                 name="R24.1">
      <xsl:if test="not(.=not(*))">
         <failed-assertion test=".=not(*)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The source identifier must be expressed in
								plainText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M25"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='sourceIdentifier']" mode="all" priority="68">
      <xsl:call-template name="R24.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M24">
      <xsl:apply-templates select="." mode="M25"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='sortKey']" mode="M25" priority="2" name="R25.1">
      <xsl:if test="not(.=not(*))">
         <failed-assertion test=".=not(*)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The sort key must be expressed in
							plainText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M26"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='sortKey']" mode="all" priority="67">
      <xsl:call-template name="R25.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M25">
      <xsl:apply-templates select="." mode="M26"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='subsetOwner']" mode="M26" priority="2" name="R26.1">
      <xsl:if test="not(.=not(*))">
         <failed-assertion test=".=not(*)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The name of the subset owner must be
								expressed in plainText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M27"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='subsetOwner']" mode="all" priority="66">
      <xsl:call-template name="R26.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M26">
      <xsl:apply-templates select="." mode="M27"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='antonymConcept']" mode="M27" priority="2"
                 name="R27.1">
      <xsl:if test="not((.=not(*[not(local-name()='hi')]))and(parent::termEntry or parent::descripGrp/parent::termEntry))">
         <failed-assertion test="(.=not(*[not(local-name()='hi')]))and(parent::termEntry or parent::descripGrp/parent::termEntry)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>Antonym concepts should occur at the entry (concept) level. The
								antonym-concept in this element must be expressed in basicText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M28"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='antonymConcept']" mode="all" priority="65">
      <xsl:call-template name="R27.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M27">
      <xsl:apply-templates select="." mode="M28"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='associatedConcept']" mode="M28" priority="2"
                 name="R28.1">
      <xsl:if test="not((.=not(*[not(local-name()='hi')]))and (parent::termEntry or parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet))">
         <failed-assertion test="(.=not(*[not(local-name()='hi')]))and (parent::termEntry or parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>Associated concepts should occur at the termEntry or the langSet
								level. The term in this element must be expressed in
							basicText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M29"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='associatedConcept']" mode="all" priority="64">
      <xsl:call-template name="R28.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M28">
      <xsl:apply-templates select="." mode="M29"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='audio']" mode="M29" priority="2" name="R29.1">
      <xsl:if test="not((.=not(*))and (parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig))">
         <failed-assertion test="(.=not(*))and (parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The content of this element must be plain text. It can occur at the concept (termEntry) level, the langSet level or the term (tig or ntig) level.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M30"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='audio']" mode="all" priority="63">
      <xsl:call-template name="R29.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M29">
      <xsl:apply-templates select="." mode="M30"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='broaderConceptGeneric']" mode="M30" priority="2"
                 name="R30.1">
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::termEntry or parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet))">
         <failed-assertion test="(.=not(*[not(local-name()='hi')])) and (parent::termEntry or parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>Generic broader concepts should occur at the termEntry level or the
								langSet level. The term in this element must be expressed in
								basicText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M31"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='broaderConceptGeneric']" mode="all" priority="62">
      <xsl:call-template name="R30.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M30">
      <xsl:apply-templates select="." mode="M31"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='broaderConceptPartitive']" mode="M31" priority="2"
                 name="R31.1">
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::termEntry or parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet))">
         <failed-assertion test="(.=not(*[not(local-name()='hi')])) and (parent::termEntry or parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>Partitive broader concepts should occur at the termEntry level or
								the langSet level. The term in this element must be expressed in
								basicText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M32"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='broaderConceptPartitive']" mode="all" priority="61">
      <xsl:call-template name="R31.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M31">
      <xsl:apply-templates select="." mode="M32"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='characteristic']" mode="M32" priority="2"
                 name="R32.1">
      <xsl:if test="not((.=not(*))and (parent::tig or parent::ntig or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig))">
         <failed-assertion test="(.=not(*))and (parent::tig or parent::ntig or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>A
								characteristic should only occur at the term (tig or ntig) level.
								The content of this element must be plain text.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M33"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='characteristic']" mode="all" priority="60">
      <xsl:call-template name="R32.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M32">
      <xsl:apply-templates select="." mode="M33"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='classificationCode']" mode="M33" priority="2"
                 name="R33.1">
      <xsl:if test="not((.=not(*)) and (parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig))">
         <failed-assertion test="(.=not(*)) and (parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/> The content of this element must be plainText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M34"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='classificationCode']" mode="all" priority="59">
      <xsl:call-template name="R33.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M33">
      <xsl:apply-templates select="." mode="M34"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='conceptPosition']" mode="M34" priority="2"
                 name="R34.1">
      <xsl:if test="not((.=not(*)) and (parent::termEntry or parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet))">
         <failed-assertion test="(.=not(*)) and (parent::termEntry or parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/> Information about a concept position should occur at the termEntry
								level or the langSet level, and it must be expressed in
							plainText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M35"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='conceptPosition']" mode="all" priority="58">
      <xsl:call-template name="R34.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M34">
      <xsl:apply-templates select="." mode="M35"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='coordinateConceptGeneric']" mode="M35" priority="2"
                 name="R35.1">
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::termEntry or parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet))">
         <failed-assertion test="(.=not(*[not(local-name()='hi')])) and (parent::termEntry or parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>Generic coordinate concepts should occur at the termEntry level or
								the langSet level. The term in this element must be expressed in
								basicText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M36"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='coordinateConceptGeneric']" mode="all" priority="57">
      <xsl:call-template name="R35.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M35">
      <xsl:apply-templates select="." mode="M36"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='coordinateConceptPartitive']" mode="M36" priority="2"
                 name="R36.1">
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::termEntry or parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet))">
         <failed-assertion test="(.=not(*[not(local-name()='hi')])) and (parent::termEntry or parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>Partitive coordinate concepts should occur at the termEntry level
								or the langSet level. The term in this element must be expressed in
								basicText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M37"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='coordinateConceptPartitive']" mode="all"
                 priority="56">
      <xsl:call-template name="R36.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M36">
      <xsl:apply-templates select="." mode="M37"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='context']" mode="M37" priority="2" name="R37.1">
      <xsl:if test="not(parent::tig or parent::ntig or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)">
         <failed-assertion test="parent::tig or parent::ntig or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>A
								context sentence can only occur at the term (tig)
							level.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M38"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='context']" mode="all" priority="55">
      <xsl:call-template name="R37.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M37">
      <xsl:apply-templates select="." mode="M38"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='definition']" mode="M38" priority="2" name="R38.1">
      <xsl:if test="not(parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)">
         <failed-assertion test="parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/> A definition must occur at the termEntry level, the langSet level,
								or the term (tig) level.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M39"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='definition']" mode="all" priority="54">
      <xsl:call-template name="R38.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M38">
      <xsl:apply-templates select="." mode="M39"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='example']" mode="M39" priority="2" name="R39.1">
      <xsl:if test="not(parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)">
         <failed-assertion test="parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/> An example must occur at the termEntry level, the langSet level,
								or the term (tig) level.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M40"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='example']" mode="all" priority="53">
      <xsl:call-template name="R39.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M39">
      <xsl:apply-templates select="." mode="M40"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='explanation']" mode="M40" priority="2" name="R40.1">
      <xsl:if test="not(parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)">
         <failed-assertion test="parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/> An explanation must occur at the termEntry level, the langSet
								level, or the term (tig) level.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M41"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='explanation']" mode="all" priority="52">
      <xsl:call-template name="R40.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M40">
      <xsl:apply-templates select="." mode="M41"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='figure']" mode="M41" priority="2" name="R41.1">
      <xsl:if test="not((.=not(*)) and (parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig))">
         <failed-assertion test="(.=not(*)) and (parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/> The content of this element must be plain
								text.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M42"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='figure']" mode="all" priority="51">
      <xsl:call-template name="R41.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M41">
      <xsl:apply-templates select="." mode="M42"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='otherBinaryData']" mode="M42" priority="2"
                 name="R42.1">
      <xsl:if test="not((.=not(*)) and (parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig))">
         <failed-assertion test="(.=not(*)) and (parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The content of this element must be plain
								text.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M43"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='otherBinaryData']" mode="all" priority="50">
      <xsl:call-template name="R42.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M42">
      <xsl:apply-templates select="." mode="M43"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='quantity']" mode="M43" priority="2" name="R43.1">
      <xsl:if test="not((.=not(*)) and (parent::tig or parent::ntig or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig))">
         <failed-assertion test="(.=not(*)) and (parent::tig or parent::ntig or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>A quantity should occur at the term (tig or
								ntig) level. The content of this element must be plain
							text.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M44"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='quantity']" mode="all" priority="49">
      <xsl:call-template name="R43.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M43">
      <xsl:apply-templates select="." mode="M44"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='range']" mode="M44" priority="2" name="R44.1">
      <xsl:if test="not((.=not(*)) and (parent::tig or parent::ntig or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig))">
         <failed-assertion test="(.=not(*)) and (parent::tig or parent::ntig or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>A range should occur at the term (tig or ntig)
								level. The content of this element must be plainText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M45"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='range']" mode="all" priority="48">
      <xsl:call-template name="R44.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M44">
      <xsl:apply-templates select="." mode="M45"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='relatedConcept']" mode="M45" priority="2"
                 name="R45.1">
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry))">
         <failed-assertion test="(.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>Related concepts
								should occur at the termEntry level or the langSet level. The
								content of this element must be expressed in basicText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M46"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='relatedConcept']" mode="all" priority="47">
      <xsl:call-template name="R45.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M45">
      <xsl:apply-templates select="." mode="M46"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='relatedConceptBroader']" mode="M46" priority="2"
                 name="R46.1">
      <xsl:if test="not((.=not(*[not(local-name()='hi')]) )and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry))">
         <failed-assertion test="(.=not(*[not(local-name()='hi')]) )and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>Broader related
								concepts should occur at the termEntry level or the langSet level.
								The content of this element must be expressed in
							basicText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M47"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='relatedConceptBroader']" mode="all" priority="46">
      <xsl:call-template name="R46.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M46">
      <xsl:apply-templates select="." mode="M47"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='relatedConceptNarrower']" mode="M47" priority="2"
                 name="R47.1">
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry))">
         <failed-assertion test="(.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>Narrower related
								concepts should occur at the termEntry level or the langSet level.
								The content of this element must be expressed in
							basicText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M48"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='relatedConceptNarrower']" mode="all" priority="45">
      <xsl:call-template name="R47.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M47">
      <xsl:apply-templates select="." mode="M48"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='reliabilityCode']" mode="M48" priority="2"
                 name="R48.1">
      <xsl:if test="not(((.='1' or .='2' or .='3' or .='4' or .='5' or .='6' or .='7' or .='8' or .='9' or .='10') and (parent::langSet or parent::termEntry or parent::tig or parent::ntig or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)))">
         <failed-assertion test="((.='1' or .='2' or .='3' or .='4' or .='5' or .='6' or .='7' or .='8' or .='9' or .='10') and (parent::langSet or parent::termEntry or parent::tig or parent::ntig or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig))">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>A reliability code can be a value from 1 (least reliable) to 10
								(most reliable).</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M49"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='reliabilityCode']" mode="all" priority="44">
      <xsl:call-template name="R48.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M48">
      <xsl:apply-templates select="." mode="M49"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='sampleSentence']" mode="M49" priority="2"
                 name="R49.1">
      <xsl:if test="not(parent::tig or parent::ntig or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)">
         <failed-assertion test="parent::tig or parent::ntig or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/> A sample sentence can only occur at the
								term (tig) level.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M50"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='sampleSentence']" mode="all" priority="43">
      <xsl:call-template name="R49.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M49">
      <xsl:apply-templates select="." mode="M50"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='sequentiallyRelatedConcept']" mode="M50" priority="2"
                 name="R50.1">
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry))">
         <failed-assertion test="(.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>Sequentially related
								concepts should occur at the termEntry level or the langSet level.
								The content of this element must be expressed in
							basicText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M51"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='sequentiallyRelatedConcept']" mode="all"
                 priority="42">
      <xsl:call-template name="R50.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M50">
      <xsl:apply-templates select="." mode="M51"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='spatiallyRelatedConcept']" mode="M51" priority="2"
                 name="R51.1">
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry))">
         <failed-assertion test="(.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>Spatially related
								concepts should occur at the termEntry level or the langSet level.
								The content of the element must be expressed in
							basicText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M52"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='spatiallyRelatedConcept']" mode="all" priority="41">
      <xsl:call-template name="R51.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M51">
      <xsl:apply-templates select="." mode="M52"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='subjectField']" mode="M52" priority="2" name="R52.1">
      <xsl:if test="not((.=not(*)) and (parent::termEntry or parent::descripGrp/parent::termEntry))">
         <failed-assertion test="(.=not(*)) and (parent::termEntry or parent::descripGrp/parent::termEntry)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/> A subject field must be a plainText value.
								Subject fields usually occur at the termEntry level.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M53"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='subjectField']" mode="all" priority="40">
      <xsl:call-template name="R52.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M52">
      <xsl:apply-templates select="." mode="M53"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='subordinateConceptGeneric']" mode="M53" priority="2"
                 name="R53.1">
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry))">
         <failed-assertion test="(.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/> Generic subordinate
								concepts should occur at the termEntry level or the langSet level.
								The content of the element must be expressed in
							basicText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M54"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='subordinateConceptGeneric']" mode="all" priority="39">
      <xsl:call-template name="R53.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M53">
      <xsl:apply-templates select="." mode="M54"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='subordinateConceptPartitive']" mode="M54"
                 priority="2"
                 name="R54.1">
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry))">
         <failed-assertion test="(.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/> Partitive
								subordinate concepts should occur at the termEntry level or the
								langSet level. The content of the element must be expressed in
								basicText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M55"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='subordinateConceptPartitive']" mode="all"
                 priority="38">
      <xsl:call-template name="R54.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M54">
      <xsl:apply-templates select="." mode="M55"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='superordinateConceptGeneric']" mode="M55"
                 priority="2"
                 name="R55.1">
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry))">
         <failed-assertion test="(.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>Generic
								superordinate concepts should occur at the termEntry level or the
								langSet level. The content of the element must be expressed in
								basicText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M56"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='superordinateConceptGeneric']" mode="all"
                 priority="37">
      <xsl:call-template name="R55.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M55">
      <xsl:apply-templates select="." mode="M56"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='superordinateConceptPartitive']" mode="M56"
                 priority="2"
                 name="R56.1">
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry))">
         <failed-assertion test="(.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>Partitive
								superordinate concepts should occur at the termEntry level or the
								langSet level. The content of the element must be expressed in
								basicText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M57"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='superordinateConceptPartitive']" mode="all"
                 priority="36">
      <xsl:call-template name="R56.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M56">
      <xsl:apply-templates select="." mode="M57"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='table']" mode="M57" priority="2" name="R57.1">
      <xsl:if test="not((.=not(*)) and (parent::langSet or parent::termEntry or parent::tig or parent::ntig or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig))">
         <failed-assertion test="(.=not(*)) and (parent::langSet or parent::termEntry or parent::tig or parent::ntig or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/> The content of this element must be plain
								text.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M58"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='table']" mode="all" priority="35">
      <xsl:call-template name="R57.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M57">
      <xsl:apply-templates select="." mode="M58"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='temporallyRelatedConcept']" mode="M58" priority="2"
                 name="R58.1">
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry))">
         <failed-assertion test="(.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>Temporally related
								concepts should occur at the termEntry level or the langSet level.
								The content of the element must be expressed in
							basicText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M59"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='temporallyRelatedConcept']" mode="all" priority="34">
      <xsl:call-template name="R58.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M58">
      <xsl:apply-templates select="." mode="M59"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='thesaurusDescriptor']" mode="M59" priority="2"
                 name="R59.1">
      <xsl:if test="not((.=not(*)) and (parent::termEntry or parent::descripGrp/parent::termEntry))">
         <failed-assertion test="(.=not(*)) and (parent::termEntry or parent::descripGrp/parent::termEntry)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>Thesaurus descriptors should occur at the
								termEntry level. The content of this element must be plain
							text.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M60"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='thesaurusDescriptor']" mode="all" priority="33">
      <xsl:call-template name="R59.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M59">
      <xsl:apply-templates select="." mode="M60"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='unit']" mode="M60" priority="2" name="R60.1">
      <xsl:if test="not((not (*) and (parent::tig or parent::ntig or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)))">
         <failed-assertion test="(not (*) and (parent::tig or parent::ntig or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig))">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/> Units should occur at the term (tig or ntig)
								level. Units must be expressed in plainText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M61"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='unit']" mode="all" priority="32">
      <xsl:call-template name="R60.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M60">
      <xsl:apply-templates select="." mode="M61"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='video']" mode="M61" priority="2" name="R61.1">
      <xsl:if test="not((.=not(*)) and (parent::langSet or parent::termEntry or parent::tig or parent::ntig or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig))">
         <failed-assertion test="(.=not(*)) and (parent::langSet or parent::termEntry or parent::tig or parent::ntig or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The content of this element must be in plain
								text.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M62"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='video']" mode="all" priority="31">
      <xsl:call-template name="R61.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M61">
      <xsl:apply-templates select="." mode="M62"/>
   </xsl:template>
   <xsl:template match="tei:descripNote[@type='contextType']" mode="M62" priority="2"
                 name="R62.1">
      <xsl:if test="not(. ='definingContext'  or .='explanatoryContext'  or .='associativeContext'  or .='linguisticContext'  or .='metalinguisticContext'  or .='translatedContext')">
         <failed-assertion test=". ='definingContext'  or .='explanatoryContext'  or .='associativeContext'  or .='linguisticContext'  or .='metalinguisticContext'  or .='translatedContext'">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/> Contexts can only be of one of the following types:
								definingContext, explanatoryContext, associativeContext,
								linguisticContext, metalinguisticContext or
							translatedContext.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M63"/>
   </xsl:template>
   <xsl:template match="tei:descripNote[@type='contextType']" mode="all" priority="30">
      <xsl:call-template name="R62.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M62">
      <xsl:apply-templates select="." mode="M63"/>
   </xsl:template>
   <xsl:template match="tei:descripNote[@type='definitionType']" mode="M63" priority="2"
                 name="R63.1">
      <xsl:if test="not(.=         'intensionalDefinition' or .='extensionalDefinition' or .='partitiveDefinition' or .='translatedDefinition')">
         <failed-assertion test=".=         'intensionalDefinition' or .='extensionalDefinition' or .='partitiveDefinition' or .='translatedDefinition'">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/> Definitions can only be of one of the following types:
								intensionalDefinition, extensionalDefinition, partitiveDefinition,
								or translatedDefinition.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M64"/>
   </xsl:template>
   <xsl:template match="tei:descripNote[@type='definitionType']" mode="all" priority="29">
      <xsl:call-template name="R63.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M63">
      <xsl:apply-templates select="." mode="M64"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='abbreviatedFormFor']" mode="M64" priority="2"
                 name="R64.1">
      <xsl:if test="not(.=not(*[not(local-name()='hi')]))">
         <failed-assertion test=".=not(*[not(local-name()='hi')])">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/> The value of the
								abbreviated form in this element must be expressed in basicText.
							</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M65"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='abbreviatedFormFor']" mode="all" priority="28">
      <xsl:call-template name="R64.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M64">
      <xsl:apply-templates select="." mode="M65"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='administrativeStatus']" mode="M65" priority="2"
                 name="R65.1">
      <xsl:if test="not(.=         'standardizedTerm-admn-sts' or .='preferredTerm-admn-sts' or .='admittedTerm-admn-sts' or .='deprecatedTerm-admn-sts' or .='supersededTerm-admn-sts' or .='legalTerm-admn-sts' or .='regulatedTerm-admn-sts')">
         <failed-assertion test=".=         'standardizedTerm-admn-sts' or .='preferredTerm-admn-sts' or .='admittedTerm-admn-sts' or .='deprecatedTerm-admn-sts' or .='supersededTerm-admn-sts' or .='legalTerm-admn-sts' or .='regulatedTerm-admn-sts'">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The administrative status must be standardizedTerm-admn-sts,
								preferredTerm-admn-sts, admittedTerm-admn-sts,
								deprecatedTerm-admn-sts, supersededTerm-admn-sts,
								legalTerm-admn-sts, or regulatedTerm-admn-sts. </statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M66"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='administrativeStatus']" mode="all" priority="27">
      <xsl:call-template name="R65.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M65">
      <xsl:apply-templates select="." mode="M66"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='antonymTerm']" mode="M66" priority="2" name="R66.1">
      <xsl:if test="not((.=not(*[not(local-name()='hi')])))">
         <failed-assertion test="(.=not(*[not(local-name()='hi')]))">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>Antonym terms should
								occur at the term (tig or ntig) level. The antonym term in this
								element must be expressed in basicText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M67"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='antonymTerm']" mode="all" priority="26">
      <xsl:call-template name="R66.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M66">
      <xsl:apply-templates select="." mode="M67"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='directionality']" mode="M67" priority="2"
                 name="R67.1">
      <xsl:if test="not(. =        'monodirectional' or .='bidirectional' or .='incommensurate')">
         <failed-assertion test=". =        'monodirectional' or .='bidirectional' or .='incommensurate'">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The directionality must be monodirectional, bidirectional, or
								incommensurate. </statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M68"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='directionality']" mode="all" priority="25">
      <xsl:call-template name="R67.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M67">
      <xsl:apply-templates select="." mode="M68"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='etymology']" mode="M68" priority="2" name="R68.1">
      <xsl:if test="not(.=not(*))">
         <failed-assertion test=".=not(*)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>Information about the etymology of a term must
								be expressed in noteText. Etymology information should occur at the
								tig level or at the termCompGrp level.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M69"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='etymology']" mode="all" priority="24">
      <xsl:call-template name="R68.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M68">
      <xsl:apply-templates select="." mode="M69"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='falseFriend']" mode="M69" priority="2" name="R69.1">
      <xsl:if test="not((.=not(*[not(local-name()='hi')])))">
         <failed-assertion test="(.=not(*[not(local-name()='hi')]))">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/> The false friend
								must be expressed in basicText. </statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M70"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='falseFriend']" mode="all" priority="23">
      <xsl:call-template name="R69.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M69">
      <xsl:apply-templates select="." mode="M70"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='frequency']" mode="M70" priority="2" name="R70.1">
      <xsl:if test="not(. =        'commonlyUsed' or .='infrequentlyUsed' or .='rarelyUsed')">
         <failed-assertion test=". =        'commonlyUsed' or .='infrequentlyUsed' or .='rarelyUsed'">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The frequency must be commonlyUsed, infrequentlyUsed, or
								rarelyUsed. </statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M71"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='frequency']" mode="all" priority="22">
      <xsl:call-template name="R70.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M70">
      <xsl:apply-templates select="." mode="M71"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='geographicalUsage']" mode="M71" priority="2"
                 name="R71.1">
      <xsl:if test="not(.=not(*))">
         <failed-assertion test=".=not(*)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The geographical usage must be expressed in
								plainText. </statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M72"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='geographicalUsage']" mode="all" priority="21">
      <xsl:call-template name="R71.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M71">
      <xsl:apply-templates select="." mode="M72"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='grammaticalGender']" mode="M72" priority="2"
                 name="R72.1">
      <xsl:if test="not((.='masculine' or .='feminine' or .='neuter' or .='otherGender'))">
         <failed-assertion test="(.='masculine' or .='feminine' or .='neuter' or .='otherGender')">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/> The gender must be masculine, feminine, neuter, or otherGender.
								Gender should be specified at the term level (tig or ntig) or at the
								termCompGrp level.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M73"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='grammaticalGender']" mode="all" priority="20">
      <xsl:call-template name="R72.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M72">
      <xsl:apply-templates select="." mode="M73"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='grammaticalNumber']" mode="M73" priority="2"
                 name="R73.1">
      <xsl:if test="not(. =         'single' or .='plural' or .='dual' or .='mass' or .='otherNumber')">
         <failed-assertion test=". =         'single' or .='plural' or .='dual' or .='mass' or .='otherNumber'">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The grammatical number must be single, plural, dual, mass, or
								otherNumber. The grammatical number should be specified at the term
								level (tig or ntig) or at the termCompGrp level.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M74"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='grammaticalNumber']" mode="all" priority="19">
      <xsl:call-template name="R73.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M73">
      <xsl:apply-templates select="." mode="M74"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='grammaticalValency']" mode="M74" priority="2"
                 name="R74.1">
      <xsl:if test="not(.=not(*))">
         <failed-assertion test=".=not(*)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/> The grammatical valency must be expressed in
								plainText. </statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M75"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='grammaticalValency']" mode="all" priority="18">
      <xsl:call-template name="R74.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M74">
      <xsl:apply-templates select="." mode="M75"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='homograph']" mode="M75" priority="2" name="R75.1">
      <xsl:if test="not(.=not(*[not(local-name()='hi')]))">
         <failed-assertion test=".=not(*[not(local-name()='hi')])">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/> The homograph must
								be expressed in basicText. </statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M76"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='homograph']" mode="all" priority="17">
      <xsl:call-template name="R75.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M75">
      <xsl:apply-templates select="." mode="M76"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='language-planningQualifier']" mode="M76"
                 priority="2"
                 name="R76.1">
      <xsl:if test="not(. =        'recommendedTerm' or .='nonstandardizedTerm' or .='proposedTerm' or .='newTerm')">
         <failed-assertion test=". =        'recommendedTerm' or .='nonstandardizedTerm' or .='proposedTerm' or .='newTerm'">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The language planning qualifier must be recommendedTerm,
								nonstandardizedTerm, proposedTerm, or newTerm. </statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M77"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='language-planningQualifier']" mode="all"
                 priority="16">
      <xsl:call-template name="R76.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M76">
      <xsl:apply-templates select="." mode="M77"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='lionHotkey']" mode="M77" priority="2" name="R77.1">
      <xsl:if test="not(.=not(*))">
         <failed-assertion test=".=not(*)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The hotkey must be expressed in plainText.
							</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M78"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='lionHotkey']" mode="all" priority="15">
      <xsl:call-template name="R77.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M77">
      <xsl:apply-templates select="." mode="M78"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='normativeAuthorization']" mode="M78" priority="2"
                 name="R78.1">
      <xsl:if test="not(. =        'standardizedTerm' or .='preferredTerm' or .='admittedTerm' or .='deprecatedTerm' or .='supersededTerm' or .='legalTerm' or .='regulatedTerm')">
         <failed-assertion test=". =        'standardizedTerm' or .='preferredTerm' or .='admittedTerm' or .='deprecatedTerm' or .='supersededTerm' or .='legalTerm' or .='regulatedTerm'">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The normative authorization must be standardizedTerm,
								preferredTerm, admittedTerm, deprecatedTerm, supersededTerm,
								legalTerm, regulatedTerm . </statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M79"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='normativeAuthorization']" mode="all" priority="14">
      <xsl:call-template name="R78.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M78">
      <xsl:apply-templates select="." mode="M79"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='partOfSpeech']" mode="M79" priority="2" name="R79.1">
      <xsl:if test="not(.=not(*))">
         <failed-assertion test=".=not(*)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The part of speech must be a plainText value.
								It should be specified only at the term (tig or ntig) level or at
								the termCompGrp level.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M80"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='partOfSpeech']" mode="all" priority="13">
      <xsl:call-template name="R79.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M79">
      <xsl:apply-templates select="." mode="M80"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='processStatus']" mode="M80" priority="2"
                 name="R80.1">
      <xsl:if test="not(. =        'unprocessed' or .='provisionallyProcessed' or .='finalized')">
         <failed-assertion test=". =        'unprocessed' or .='provisionallyProcessed' or .='finalized'">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The process status must be unprocessed, provisionallyProcessed, or
								finalized. </statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M81"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='processStatus']" mode="all" priority="12">
      <xsl:call-template name="R80.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M80">
      <xsl:apply-templates select="." mode="M81"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='pronunciation']" mode="M81" priority="2"
                 name="R81.1">
      <xsl:if test="not(.=not(*[not(local-name()='hi')]))">
         <failed-assertion test=".=not(*[not(local-name()='hi')])">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The pronunciation
								must be expressed in basicText. It should be specified at the term
								(tig or ntig) level or at the termCompGrp level.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M82"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='pronunciation']" mode="all" priority="11">
      <xsl:call-template name="R81.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M81">
      <xsl:apply-templates select="." mode="M82"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='proprietaryRestriction']" mode="M82" priority="2"
                 name="R82.1">
      <xsl:if test="not(. =        'trademark' or .='serviceMark' or .='tradeName')">
         <failed-assertion test=". =        'trademark' or .='serviceMark' or .='tradeName'">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The proprietary restriction must be trademark, serviceMark, or
								tradeName. </statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M83"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='proprietaryRestriction']" mode="all" priority="10">
      <xsl:call-template name="R82.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M82">
      <xsl:apply-templates select="." mode="M83"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='register']" mode="M83" priority="2" name="R83.1">
      <xsl:if test="not(. =        'colloquialRegister' or .='neutralRegister' or .='technicalRegister' or .='in-houseRegister' or .='bench-levelRegister' or .='slangRegister' or .='vulgarRegister')">
         <failed-assertion test=". =        'colloquialRegister' or .='neutralRegister' or .='technicalRegister' or .='in-houseRegister' or .='bench-levelRegister' or .='slangRegister' or .='vulgarRegister'">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The register must be colloquialRegister, neutralRegister,
								technicalRegister, in-houseRegister, bench-levelRegister,
								slangRegister, or vulgarRegister . </statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M84"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='register']" mode="all" priority="9">
      <xsl:call-template name="R83.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M83">
      <xsl:apply-templates select="." mode="M84"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='shortFormFor']" mode="M84" priority="2" name="R84.1">
      <xsl:if test="not(.=not(*[not(local-name()='hi')]))">
         <failed-assertion test=".=not(*[not(local-name()='hi')])">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/> The value of the
								short form in this element must be expressed in
							basicText.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M85"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='shortFormFor']" mode="all" priority="8">
      <xsl:call-template name="R84.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M84">
      <xsl:apply-templates select="." mode="M85"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='temporalQualifier']" mode="M85" priority="2"
                 name="R85.1">
      <xsl:if test="not(. =        'archaicTerm' or .='outdatedTerm' or .='obsoleteTerm')">
         <failed-assertion test=". =        'archaicTerm' or .='outdatedTerm' or .='obsoleteTerm'">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The temporal qualifier must be archaicTerm, outdatedTerm, or
								obsoleteTerm. </statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M86"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='temporalQualifier']" mode="all" priority="7">
      <xsl:call-template name="R85.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M85">
      <xsl:apply-templates select="." mode="M86"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='termLocation']" mode="M86" priority="2" name="R86.1">
      <xsl:if test="not(.=not(*))">
         <failed-assertion test=".=not(*)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The termLocation must be a plainText value. It
								should be specified only at the term (tig or ntig)
							level.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M87"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='termLocation']" mode="all" priority="6">
      <xsl:call-template name="R86.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M86">
      <xsl:apply-templates select="." mode="M87"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='termProvenance']" mode="M87" priority="2"
                 name="R87.1">
      <xsl:if test="not(. =         'transdisciplinaryBorrowing' or .='translingualBorrowing' or .='loanTranslation' or .='neologism')">
         <failed-assertion test=". =         'transdisciplinaryBorrowing' or .='translingualBorrowing' or .='loanTranslation' or .='neologism'">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The term provenance must be transdisciplinaryBorrowing,
								translingualBorrowing, loanTranslation, or neologism.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M88"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='termProvenance']" mode="all" priority="5">
      <xsl:call-template name="R87.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M87">
      <xsl:apply-templates select="." mode="M88"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='termType']" mode="M88" priority="2" name="R88.1">
      <xsl:if test="not(. = 'abbreviation' or .='initialism' or .='acronym' or .='clippedTerm' or .='entryTerm' or .='synonym' or .='internationalScientificTerm' or .='fullForm' or .='transcribedForm' or .='symbol' or .='formula' or .='equation' or .='logicalExpression' or .='commonName' or .='variant' or .='shortForm' or .='transliteratedForm' or .='sku' or .='partNumber' or .='phraseologicalUnit' or .='synonymousPhrase' or .='standardText' or .='string' or .='internationalism' or .='shortcut')">
         <failed-assertion test=". = 'abbreviation' or .='initialism' or .='acronym' or .='clippedTerm' or .='entryTerm' or .='synonym' or .='internationalScientificTerm' or .='fullForm' or .='transcribedForm' or .='symbol' or .='formula' or .='equation' or .='logicalExpression' or .='commonName' or .='variant' or .='shortForm' or .='transliteratedForm' or .='sku' or .='partNumber' or .='phraseologicalUnit' or .='synonymousPhrase' or .='standardText' or .='string' or .='internationalism' or .='shortcut'">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/>The type of term can only be one of the following values:
								abbreviation, initialism, acronym, clippedTerm, entryTerm, synonym,
								internationalScientificTerm, fullForm, transcribedForm, symbol,
								formula, equation, logicalExpression, commonName, variant,
								shortForm, transliteratedForm, sku, partNumber, phraseologicalUnit,
								synonymousPhrase, standardText, string, internationalism,
							shortcut.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M89"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='termType']" mode="all" priority="4">
      <xsl:call-template name="R88.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M88">
      <xsl:apply-templates select="." mode="M89"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='termStructure']" mode="M89" priority="2"
                 name="R89.1">
      <xsl:if test="not(.=not(*))">
         <failed-assertion test=".=not(*)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/> The term structure must be expressed in plain
								text.</statement>
         </failed-assertion>
      </xsl:if>
      <xsl:apply-templates select="." mode="M90"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='termStructure']" mode="all" priority="3">
      <xsl:call-template name="R89.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M89">
      <xsl:apply-templates select="." mode="M90"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='timeRestriction']" mode="M90" priority="2"
                 name="R90.1">
      <xsl:if test="not(.=not(*))">
         <failed-assertion test=".=not(*)">
            <xsl:call-template name="location"/>
            <statement>
               <xsl:apply-templates mode="schematron-get-full-path-2" select="."/> The time restriction must be expressed in
								plainText. </statement>
         </failed-assertion>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='timeRestriction']" mode="all" priority="2">
      <xsl:call-template name="R90.1"/>
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template match="*" mode="M90"/>
   <xsl:template match="*|/" mode="all">
      <xsl:apply-templates select="*" mode="all"/>
   </xsl:template>
   <xsl:template name="location"/>
   <xsl:template match="node() | @*" mode="schematron-get-full-path-2">
      <xsl:text>

* section </xsl:text>
      <xsl:for-each select="ancestor-or-self::tei:div">/<xsl:number level="multiple"/>
         <xsl:text> - </xsl:text>
         <xsl:value-of select="translate(substring(tei:head,1,20),'&#160;',' ')"/>
      </xsl:for-each>
      <xsl:text> (element </xsl:text>
      <xsl:value-of select="local-name()"/>
      <xsl:text>)

</xsl:text>
   </xsl:template>
</xsl:stylesheet>
