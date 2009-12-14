<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:i="http://www.iso.org/ns/1.0"
                version="2.0">
   <xsl:template match="processing-instruction()" mode="checkSchematron">
      <xsl:copy-of select="."/>
   </xsl:template>
   <xsl:template match="@*|text()|comment()" mode="checkSchematron">
      <xsl:copy-of select="."/>
   </xsl:template>
   <xsl:template match="*" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="*|@*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
   </xsl:template>
   <xsl:template match="tei:list[@type='termlist']/tei:item" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(@n)">
         <xsl:processing-instruction name="ISOerror">Each item in a termlist must have a @n attribute</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:note" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(not(parent::tei:div))">
         <xsl:processing-instruction name="ISOerror">an inline note cannot be a direct child of a div</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:teiHeader" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(tei:fileDesc/tei:titleStmt/tei:title[@i:meta='introductoryTitle'])">
         <xsl:processing-instruction name="ISOerror">
		      An introductory component of the title is expected
		    </xsl:processing-instruction>
      </xsl:if>
      <xsl:if test="not(tei:fileDesc/tei:titleStmt/tei:title[@i:meta='mainTitle'])">
         <xsl:processing-instruction name="ISOerror">
		      A main component of the title is expected
		    </xsl:processing-instruction>
      </xsl:if>
      <xsl:if test="not(tei:fileDesc/tei:titleStmt/tei:respStmt/tei:name)">
         <xsl:processing-instruction name="ISOerror">
		      No respStmt has been provided, giving SC/TC committee numbers
		  </xsl:processing-instruction>
      </xsl:if>
      <xsl:if test="not(tei:fileDesc/tei:publicationStmt/tei:idno[@i:meta='wgNumber'])">
         <xsl:processing-instruction name="ISOerror">
		  an idno of type wgNumber is expected</xsl:processing-instruction>
      </xsl:if>
      <xsl:if test="not(tei:fileDesc/tei:publicationStmt/tei:idno[@i:meta='serialNumber'])">
         <xsl:processing-instruction name="ISOerror">
		  an idno of type serialNumber is expected</xsl:processing-instruction>
      </xsl:if>
      <xsl:if test="not(tei:fileDesc/tei:publicationStmt/tei:idno[@i:meta='referenceNumber'])">
         <xsl:processing-instruction name="ISOerror">
		  an idno of type referenceNumber is expected</xsl:processing-instruction>
      </xsl:if>
      <xsl:if test="not(tei:fileDesc/tei:publicationStmt/tei:idno[@i:meta='partNumber'])">
         <xsl:processing-instruction name="ISOerror">
		  an idno of type partNumber is expected</xsl:processing-instruction>
      </xsl:if>
      <xsl:if test="not(tei:fileDesc/tei:publicationStmt/tei:idno[@i:meta='stage'])">
         <xsl:processing-instruction name="ISOerror">
		  an idno of type stage is expected</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:TEI" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(tei:text/tei:front/tei:div[@type='foreword'])">
         <xsl:processing-instruction name="ISOerror">
		A Foreword clause in the front matter is mandatory</xsl:processing-instruction>
      </xsl:if>
      <xsl:if test="not(tei:text)">
         <xsl:processing-instruction name="ISOerror">A text element is essential</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:div" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(not(tei:div) or count(tei:div)&gt;1)">
         <xsl:processing-instruction name="ISOerror">a clause must contain
		  at least two subclauses</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:admin[@type='applicationSubset']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.=not(*))">
         <xsl:processing-instruction name="ISOerror">The application subset must be expressed in
								plainText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:admin[@type='businessUnitSubset']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.=not(*))">
         <xsl:processing-instruction name="ISOerror">The business unit subset must be expressed
								in plainText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:admin[@type='conceptOrigin']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.=not(*))">
         <xsl:processing-instruction name="ISOerror">The concept origin must be expressed in
								plainText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:admin[@type='customerSubset']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.=not(*))">
         <xsl:processing-instruction name="ISOerror">The customer subset must be expressed in
								plainText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:admin[@type='databaseType']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.=not(*))">
         <xsl:processing-instruction name="ISOerror">The type of data base must be expressed in
								plainText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:admin[@type='domainExpert']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.=not(*))">
         <xsl:processing-instruction name="ISOerror">The domain expert must be expressed in plain
								text.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:admin[@type='elementWorkingStatus']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.='starterElement' or .='workingElement' or .='consolidatedElement' or .='archiveElement' or .='importedElement' or .='exportedElement')">
         <xsl:processing-instruction name="ISOerror"> The element working status must be starterElement, workingElement,
								consolidatedElement, archiveElement, importedElement, or
								exportedElement.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:admin[@type='entrySource']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.=not(*))">
         <xsl:processing-instruction name="ISOerror">The source of the entry must be expressed in
								plainText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:admin[@type='environmentSubset']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.=not(*))">
         <xsl:processing-instruction name="ISOerror">The environment subset must be expressed in
								plainText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:admin[@type='indexHeading']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.=not(*))">
         <xsl:processing-instruction name="ISOerror">The index heading must be expressed in plain
								text.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:admin[@type='keyword']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.=not(*))">
         <xsl:processing-instruction name="ISOerror">The keyword must be expressed in plain
							text.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:admin[@type='originatingPerson']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.=not(*))">
         <xsl:processing-instruction name="ISOerror">The name of the originating person must be
								expressed in plainText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:admin[@type='originatingDatabase']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.=not(*))">
         <xsl:processing-instruction name="ISOerror">The name of the originating database must be
								expressed in plainText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:admin[@type='originatingInstitution']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.=not(*))">
         <xsl:processing-instruction name="ISOerror">The name of the originating institution must
								be expressed in plainText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:admin[@type='productSubset']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.=not(*))">
         <xsl:processing-instruction name="ISOerror">The product subset must be expressed in
								plainText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:admin[@type='projectSubset']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.=not(*))">
         <xsl:processing-instruction name="ISOerror">The project subset must be expressed in
								plainText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:admin[@type='securitySubset']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.='public' or .='confidential')">
         <xsl:processing-instruction name="ISOerror">The security subset
								must be public or confidential.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:admin[@type='searchTerm']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.=not(*))">
         <xsl:processing-instruction name="ISOerror">The search term must be expressed in plain
								text.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:admin[@type='sourceIdentifier']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.=not(*))">
         <xsl:processing-instruction name="ISOerror">The source identifier must be expressed in
								plainText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:admin[@type='sortKey']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.=not(*))">
         <xsl:processing-instruction name="ISOerror">The sort key must be expressed in
							plainText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:admin[@type='subsetOwner']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.=not(*))">
         <xsl:processing-instruction name="ISOerror">The name of the subset owner must be
								expressed in plainText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='antonymConcept']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not((.=not(*[not(local-name()='hi')]))and(parent::termEntry or parent::descripGrp/parent::termEntry))">
         <xsl:processing-instruction name="ISOerror">Antonym concepts should occur at the entry (concept) level. The
								antonym-concept in this element must be expressed in basicText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='associatedConcept']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not((.=not(*[not(local-name()='hi')]))and (parent::termEntry or parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet))">
         <xsl:processing-instruction name="ISOerror">Associated concepts should occur at the termEntry or the langSet
								level. The term in this element must be expressed in
							basicText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='audio']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not((.=not(*))and (parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig))">
         <xsl:processing-instruction name="ISOerror">The content of this element must be plain text. It can occur at the concept (termEntry) level, the langSet level or the term (tig or ntig) level.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='broaderConceptGeneric']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::termEntry or parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet))">
         <xsl:processing-instruction name="ISOerror">Generic broader concepts should occur at the termEntry level or the
								langSet level. The term in this element must be expressed in
								basicText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='broaderConceptPartitive']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::termEntry or parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet))">
         <xsl:processing-instruction name="ISOerror">Partitive broader concepts should occur at the termEntry level or
								the langSet level. The term in this element must be expressed in
								basicText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='characteristic']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not((.=not(*))and (parent::tig or parent::ntig or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig))">
         <xsl:processing-instruction name="ISOerror">A
								characteristic should only occur at the term (tig or ntig) level.
								The content of this element must be plain text.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='classificationCode']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not((.=not(*)) and (parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig))">
         <xsl:processing-instruction name="ISOerror"> The content of this element must be plainText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='conceptPosition']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not((.=not(*)) and (parent::termEntry or parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet))">
         <xsl:processing-instruction name="ISOerror"> Information about a concept position should occur at the termEntry
								level or the langSet level, and it must be expressed in
							plainText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='coordinateConceptGeneric']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::termEntry or parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet))">
         <xsl:processing-instruction name="ISOerror">Generic coordinate concepts should occur at the termEntry level or
								the langSet level. The term in this element must be expressed in
								basicText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='coordinateConceptPartitive']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::termEntry or parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet))">
         <xsl:processing-instruction name="ISOerror">Partitive coordinate concepts should occur at the termEntry level
								or the langSet level. The term in this element must be expressed in
								basicText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='context']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(parent::tig or parent::ntig or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)">
         <xsl:processing-instruction name="ISOerror">A
								context sentence can only occur at the term (tig)
							level.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='definition']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)">
         <xsl:processing-instruction name="ISOerror"> A definition must occur at the termEntry level, the langSet level,
								or the term (tig) level.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='example']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)">
         <xsl:processing-instruction name="ISOerror"> An example must occur at the termEntry level, the langSet level,
								or the term (tig) level.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='explanation']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)">
         <xsl:processing-instruction name="ISOerror"> An explanation must occur at the termEntry level, the langSet
								level, or the term (tig) level.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='figure']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not((.=not(*)) and (parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig))">
         <xsl:processing-instruction name="ISOerror"> The content of this element must be plain
								text.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='otherBinaryData']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not((.=not(*)) and (parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig))">
         <xsl:processing-instruction name="ISOerror">The content of this element must be plain
								text.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='quantity']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not((.=not(*)) and (parent::tig or parent::ntig or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig))">
         <xsl:processing-instruction name="ISOerror">A quantity should occur at the term (tig or
								ntig) level. The content of this element must be plain
							text.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='range']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not((.=not(*)) and (parent::tig or parent::ntig or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig))">
         <xsl:processing-instruction name="ISOerror">A range should occur at the term (tig or ntig)
								level. The content of this element must be plainText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='relatedConcept']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry))">
         <xsl:processing-instruction name="ISOerror">Related concepts
								should occur at the termEntry level or the langSet level. The
								content of this element must be expressed in basicText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='relatedConceptBroader']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not((.=not(*[not(local-name()='hi')]) )and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry))">
         <xsl:processing-instruction name="ISOerror">Broader related
								concepts should occur at the termEntry level or the langSet level.
								The content of this element must be expressed in
							basicText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='relatedConceptNarrower']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry))">
         <xsl:processing-instruction name="ISOerror">Narrower related
								concepts should occur at the termEntry level or the langSet level.
								The content of this element must be expressed in
							basicText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='reliabilityCode']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(((.='1' or .='2' or .='3' or .='4' or .='5' or .='6' or .='7' or .='8' or .='9' or .='10') and (parent::langSet or parent::termEntry or parent::tig or parent::ntig or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)))">
         <xsl:processing-instruction name="ISOerror">A reliability code can be a value from 1 (least reliable) to 10
								(most reliable).</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='sampleSentence']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(parent::tig or parent::ntig or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)">
         <xsl:processing-instruction name="ISOerror"> A sample sentence can only occur at the
								term (tig) level.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='sequentiallyRelatedConcept']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry))">
         <xsl:processing-instruction name="ISOerror">Sequentially related
								concepts should occur at the termEntry level or the langSet level.
								The content of this element must be expressed in
							basicText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='spatiallyRelatedConcept']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry))">
         <xsl:processing-instruction name="ISOerror">Spatially related
								concepts should occur at the termEntry level or the langSet level.
								The content of the element must be expressed in
							basicText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='subjectField']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not((.=not(*)) and (parent::termEntry or parent::descripGrp/parent::termEntry))">
         <xsl:processing-instruction name="ISOerror"> A subject field must be a plainText value.
								Subject fields usually occur at the termEntry level.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='subordinateConceptGeneric']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry))">
         <xsl:processing-instruction name="ISOerror"> Generic subordinate
								concepts should occur at the termEntry level or the langSet level.
								The content of the element must be expressed in
							basicText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='subordinateConceptPartitive']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry))">
         <xsl:processing-instruction name="ISOerror"> Partitive
								subordinate concepts should occur at the termEntry level or the
								langSet level. The content of the element must be expressed in
								basicText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='superordinateConceptGeneric']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry))">
         <xsl:processing-instruction name="ISOerror">Generic
								superordinate concepts should occur at the termEntry level or the
								langSet level. The content of the element must be expressed in
								basicText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='superordinateConceptPartitive']"
                 mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry))">
         <xsl:processing-instruction name="ISOerror">Partitive
								superordinate concepts should occur at the termEntry level or the
								langSet level. The content of the element must be expressed in
								basicText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='table']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not((.=not(*)) and (parent::langSet or parent::termEntry or parent::tig or parent::ntig or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig))">
         <xsl:processing-instruction name="ISOerror"> The content of this element must be plain
								text.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='temporallyRelatedConcept']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry))">
         <xsl:processing-instruction name="ISOerror">Temporally related
								concepts should occur at the termEntry level or the langSet level.
								The content of the element must be expressed in
							basicText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='thesaurusDescriptor']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not((.=not(*)) and (parent::termEntry or parent::descripGrp/parent::termEntry))">
         <xsl:processing-instruction name="ISOerror">Thesaurus descriptors should occur at the
								termEntry level. The content of this element must be plain
							text.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='unit']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not((not (*) and (parent::tig or parent::ntig or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)))">
         <xsl:processing-instruction name="ISOerror"> Units should occur at the term (tig or ntig)
								level. Units must be expressed in plainText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='video']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not((.=not(*)) and (parent::langSet or parent::termEntry or parent::tig or parent::ntig or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig))">
         <xsl:processing-instruction name="ISOerror">The content of this element must be in plain
								text.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descripNote[@type='contextType']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(. ='definingContext'  or .='explanatoryContext'  or .='associativeContext'  or .='linguisticContext'  or .='metalinguisticContext'  or .='translatedContext')">
         <xsl:processing-instruction name="ISOerror"> Contexts can only be of one of the following types:
								definingContext, explanatoryContext, associativeContext,
								linguisticContext, metalinguisticContext or
							translatedContext.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:descripNote[@type='definitionType']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.=         'intensionalDefinition' or .='extensionalDefinition' or .='partitiveDefinition' or .='translatedDefinition')">
         <xsl:processing-instruction name="ISOerror"> Definitions can only be of one of the following types:
								intensionalDefinition, extensionalDefinition, partitiveDefinition,
								or translatedDefinition.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='abbreviatedFormFor']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.=not(*[not(local-name()='hi')]))">
         <xsl:processing-instruction name="ISOerror"> The value of the
								abbreviated form in this element must be expressed in basicText.
							</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='administrativeStatus']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.=         'standardizedTerm-admn-sts' or .='preferredTerm-admn-sts' or .='admittedTerm-admn-sts' or .='deprecatedTerm-admn-sts' or .='supersededTerm-admn-sts' or .='legalTerm-admn-sts' or .='regulatedTerm-admn-sts')">
         <xsl:processing-instruction name="ISOerror">The administrative status must be standardizedTerm-admn-sts,
								preferredTerm-admn-sts, admittedTerm-admn-sts,
								deprecatedTerm-admn-sts, supersededTerm-admn-sts,
								legalTerm-admn-sts, or regulatedTerm-admn-sts. </xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='antonymTerm']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not((.=not(*[not(local-name()='hi')])))">
         <xsl:processing-instruction name="ISOerror">Antonym terms should
								occur at the term (tig or ntig) level. The antonym term in this
								element must be expressed in basicText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='directionality']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(. =        'monodirectional' or .='bidirectional' or .='incommensurate')">
         <xsl:processing-instruction name="ISOerror">The directionality must be monodirectional, bidirectional, or
								incommensurate. </xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='etymology']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.=not(*))">
         <xsl:processing-instruction name="ISOerror">Information about the etymology of a term must
								be expressed in noteText. Etymology information should occur at the
								tig level or at the termCompGrp level.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='falseFriend']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not((.=not(*[not(local-name()='hi')])))">
         <xsl:processing-instruction name="ISOerror"> The false friend
								must be expressed in basicText. </xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='frequency']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(. =        'commonlyUsed' or .='infrequentlyUsed' or .='rarelyUsed')">
         <xsl:processing-instruction name="ISOerror">The frequency must be commonlyUsed, infrequentlyUsed, or
								rarelyUsed. </xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='geographicalUsage']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.=not(*))">
         <xsl:processing-instruction name="ISOerror">The geographical usage must be expressed in
								plainText. </xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='grammaticalGender']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not((.='masculine' or .='feminine' or .='neuter' or .='otherGender'))">
         <xsl:processing-instruction name="ISOerror"> The gender must be masculine, feminine, neuter, or otherGender.
								Gender should be specified at the term level (tig or ntig) or at the
								termCompGrp level.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='grammaticalNumber']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(. =         'single' or .='plural' or .='dual' or .='mass' or .='otherNumber')">
         <xsl:processing-instruction name="ISOerror">The grammatical number must be single, plural, dual, mass, or
								otherNumber. The grammatical number should be specified at the term
								level (tig or ntig) or at the termCompGrp level.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='grammaticalValency']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.=not(*))">
         <xsl:processing-instruction name="ISOerror"> The grammatical valency must be expressed in
								plainText. </xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='homograph']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.=not(*[not(local-name()='hi')]))">
         <xsl:processing-instruction name="ISOerror"> The homograph must
								be expressed in basicText. </xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='language-planningQualifier']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(. =        'recommendedTerm' or .='nonstandardizedTerm' or .='proposedTerm' or .='newTerm')">
         <xsl:processing-instruction name="ISOerror">The language planning qualifier must be recommendedTerm,
								nonstandardizedTerm, proposedTerm, or newTerm. </xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='lionHotkey']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.=not(*))">
         <xsl:processing-instruction name="ISOerror">The hotkey must be expressed in plainText.
							</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='normativeAuthorization']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(. =        'standardizedTerm' or .='preferredTerm' or .='admittedTerm' or .='deprecatedTerm' or .='supersededTerm' or .='legalTerm' or .='regulatedTerm')">
         <xsl:processing-instruction name="ISOerror">The normative authorization must be standardizedTerm,
								preferredTerm, admittedTerm, deprecatedTerm, supersededTerm,
								legalTerm, regulatedTerm . </xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='partOfSpeech']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.=not(*))">
         <xsl:processing-instruction name="ISOerror">The part of speech must be a plainText value.
								It should be specified only at the term (tig or ntig) level or at
								the termCompGrp level.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='processStatus']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(. =        'unprocessed' or .='provisionallyProcessed' or .='finalized')">
         <xsl:processing-instruction name="ISOerror">The process status must be unprocessed, provisionallyProcessed, or
								finalized. </xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='pronunciation']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.=not(*[not(local-name()='hi')]))">
         <xsl:processing-instruction name="ISOerror">The pronunciation
								must be expressed in basicText. It should be specified at the term
								(tig or ntig) level or at the termCompGrp level.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='proprietaryRestriction']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(. =        'trademark' or .='serviceMark' or .='tradeName')">
         <xsl:processing-instruction name="ISOerror">The proprietary restriction must be trademark, serviceMark, or
								tradeName. </xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='register']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(. =        'colloquialRegister' or .='neutralRegister' or .='technicalRegister' or .='in-houseRegister' or .='bench-levelRegister' or .='slangRegister' or .='vulgarRegister')">
         <xsl:processing-instruction name="ISOerror">The register must be colloquialRegister, neutralRegister,
								technicalRegister, in-houseRegister, bench-levelRegister,
								slangRegister, or vulgarRegister . </xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='shortFormFor']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.=not(*[not(local-name()='hi')]))">
         <xsl:processing-instruction name="ISOerror"> The value of the
								short form in this element must be expressed in
							basicText.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='temporalQualifier']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(. =        'archaicTerm' or .='outdatedTerm' or .='obsoleteTerm')">
         <xsl:processing-instruction name="ISOerror">The temporal qualifier must be archaicTerm, outdatedTerm, or
								obsoleteTerm. </xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='termLocation']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.=not(*))">
         <xsl:processing-instruction name="ISOerror">The termLocation must be a plainText value. It
								should be specified only at the term (tig or ntig)
							level.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='termProvenance']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(. =         'transdisciplinaryBorrowing' or .='translingualBorrowing' or .='loanTranslation' or .='neologism')">
         <xsl:processing-instruction name="ISOerror">The term provenance must be transdisciplinaryBorrowing,
								translingualBorrowing, loanTranslation, or neologism.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='termType']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(. = 'abbreviation' or .='initialism' or .='acronym' or .='clippedTerm' or .='entryTerm' or .='synonym' or .='internationalScientificTerm' or .='fullForm' or .='transcribedForm' or .='symbol' or .='formula' or .='equation' or .='logicalExpression' or .='commonName' or .='variant' or .='shortForm' or .='transliteratedForm' or .='sku' or .='partNumber' or .='phraseologicalUnit' or .='synonymousPhrase' or .='standardText' or .='string' or .='internationalism' or .='shortcut')">
         <xsl:processing-instruction name="ISOerror">The type of term can only be one of the following values:
								abbreviation, initialism, acronym, clippedTerm, entryTerm, synonym,
								internationalScientificTerm, fullForm, transcribedForm, symbol,
								formula, equation, logicalExpression, commonName, variant,
								shortForm, transliteratedForm, sku, partNumber, phraseologicalUnit,
								synonymousPhrase, standardText, string, internationalism,
							shortcut.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='termStructure']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.=not(*))">
         <xsl:processing-instruction name="ISOerror"> The term structure must be expressed in plain
								text.</xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='timeRestriction']" mode="checkSchematron">
      <xsl:copy>
         <xsl:apply-templates select="@*" mode="checkSchematron"/>
         <xsl:apply-templates select="*|processing-instruction()|comment()|text()" mode="checkSchematron"/>
      </xsl:copy>
      <xsl:if test="not(.=not(*))">
         <xsl:processing-instruction name="ISOerror"> The time restriction must be expressed in
								plainText. </xsl:processing-instruction>
      </xsl:if>
   </xsl:template>
</xsl:stylesheet>