<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:tei="http://www.tei-c.org/ns/1.0"
                xmlns:i="http://www.iso.org/ns/1.0"
                version="2.0">
   <xsl:template match="tei:list[@type='termlist']/tei:item" mode="checkSchematron">
      <xsl:if test="not(@n)">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">Each item in a termlist must have a @n attribute</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:teiHeader" mode="checkSchematron">
      <xsl:if test="not(tei:fileDesc/tei:titleStmt/tei:title[@i:meta='introductoryTitle'])">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">
		      An introductory component of the title is expected
		    </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:if test="not(tei:fileDesc/tei:titleStmt/tei:title[@i:meta='mainTitle'])">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">
		      A main component of the title is expected
		    </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:if test="not(tei:fileDesc/tei:titleStmt/tei:respStmt/tei:name)">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">
		      No respStmt has been provided, giving SC/TC committee numbers
		  </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:if test="not(tei:fileDesc/tei:publicationStmt/tei:idno[@i:meta='serialNumber'])">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">
		  an idno of type serialNumber is expected</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:if test="not(tei:fileDesc/tei:publicationStmt/tei:idno[@i:meta='stage'])">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">
		  an idno of type stage is expected</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:text" mode="checkSchematron">
      <xsl:if test="not(tei:front/tei:div[@type='foreword'])">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">
		A Foreword clause in the front matter is mandatory</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:if test="not(tei:body/tei:div[@type='scope'])">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">
		A Scope clause in the body is mandatory</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:div" mode="checkSchematron">
      <xsl:if test="not(not(tei:div) or count(tei:div)&gt;1)">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">a clause must contain at least two subclauses</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='applicationSubset']" mode="checkSchematron">
      <xsl:if test="not(.=not(*))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The application subset must be expressed in
								plainText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='businessUnitSubset']" mode="checkSchematron">
      <xsl:if test="not(.=not(*))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The business unit subset must be expressed
								in plainText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='conceptOrigin']" mode="checkSchematron">
      <xsl:if test="not(.=not(*))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The concept origin must be expressed in
								plainText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='customerSubset']" mode="checkSchematron">
      <xsl:if test="not(.=not(*))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The customer subset must be expressed in
								plainText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='databaseType']" mode="checkSchematron">
      <xsl:if test="not(.=not(*))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The type of data base must be expressed in
								plainText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='domainExpert']" mode="checkSchematron">
      <xsl:if test="not(.=not(*))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The domain expert must be expressed in plain
								text.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='elementWorkingStatus']" mode="checkSchematron">
      <xsl:if test="not(.='starterElement' or .='workingElement' or .='consolidatedElement' or .='archiveElement' or .='importedElement' or .='exportedElement')">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message"> The element working status must be starterElement, workingElement,
								consolidatedElement, archiveElement, importedElement, or
								exportedElement.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='entrySource']" mode="checkSchematron">
      <xsl:if test="not(.=not(*))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The source of the entry must be expressed in
								plainText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='environmentSubset']" mode="checkSchematron">
      <xsl:if test="not(.=not(*))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The environment subset must be expressed in
								plainText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='indexHeading']" mode="checkSchematron">
      <xsl:if test="not(.=not(*))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The index heading must be expressed in plain
								text.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='keyword']" mode="checkSchematron">
      <xsl:if test="not(.=not(*))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The keyword must be expressed in plain
							text.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='originatingPerson']" mode="checkSchematron">
      <xsl:if test="not(.=not(*))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The name of the originating person must be
								expressed in plainText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='originatingDatabase']" mode="checkSchematron">
      <xsl:if test="not(.=not(*))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The name of the originating database must be
								expressed in plainText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='originatingInstitution']" mode="checkSchematron">
      <xsl:if test="not(.=not(*))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The name of the originating institution must
								be expressed in plainText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='productSubset']" mode="checkSchematron">
      <xsl:if test="not(.=not(*))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The product subset must be expressed in
								plainText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='projectSubset']" mode="checkSchematron">
      <xsl:if test="not(.=not(*))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The project subset must be expressed in
								plainText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='securitySubset']" mode="checkSchematron">
      <xsl:if test="not(.='public' or .='confidential')">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The security subset
								must be public or confidential.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='searchTerm']" mode="checkSchematron">
      <xsl:if test="not(.=not(*))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The search term must be expressed in plain
								text.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='sourceIdentifier']" mode="checkSchematron">
      <xsl:if test="not(.=not(*))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The source identifier must be expressed in
								plainText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='sortKey']" mode="checkSchematron">
      <xsl:if test="not(.=not(*))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The sort key must be expressed in
							plainText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:admin[@type='subsetOwner']" mode="checkSchematron">
      <xsl:if test="not(.=not(*))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The name of the subset owner must be
								expressed in plainText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='antonymConcept']" mode="checkSchematron">
      <xsl:if test="not((.=not(*[not(local-name()='hi')]))and(parent::termEntry or parent::descripGrp/parent::termEntry))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">Antonym concepts should occur at the entry (concept) level. The
								antonym-concept in this element must be expressed in basicText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='associatedConcept']" mode="checkSchematron">
      <xsl:if test="not((.=not(*[not(local-name()='hi')]))and (parent::termEntry or parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">Associated concepts should occur at the termEntry or the langSet
								level. The term in this element must be expressed in
							basicText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='audio']" mode="checkSchematron">
      <xsl:if test="not((.=not(*))and (parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The content of this element must be plain text. It can occur at the concept (termEntry) level, the langSet level or the term (tig or ntig) level.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='broaderConceptGeneric']" mode="checkSchematron">
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::termEntry or parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">Generic broader concepts should occur at the termEntry level or the
								langSet level. The term in this element must be expressed in
								basicText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='broaderConceptPartitive']" mode="checkSchematron">
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::termEntry or parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">Partitive broader concepts should occur at the termEntry level or
								the langSet level. The term in this element must be expressed in
								basicText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='characteristic']" mode="checkSchematron">
      <xsl:if test="not((.=not(*))and (parent::tig or parent::ntig or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">A
								characteristic should only occur at the term (tig or ntig) level.
								The content of this element must be plain text.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='classificationCode']" mode="checkSchematron">
      <xsl:if test="not((.=not(*)) and (parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message"> The content of this element must be plainText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='conceptPosition']" mode="checkSchematron">
      <xsl:if test="not((.=not(*)) and (parent::termEntry or parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message"> Information about a concept position should occur at the termEntry
								level or the langSet level, and it must be expressed in
							plainText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='coordinateConceptGeneric']" mode="checkSchematron">
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::termEntry or parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">Generic coordinate concepts should occur at the termEntry level or
								the langSet level. The term in this element must be expressed in
								basicText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='coordinateConceptPartitive']" mode="checkSchematron">
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::termEntry or parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">Partitive coordinate concepts should occur at the termEntry level
								or the langSet level. The term in this element must be expressed in
								basicText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='context']" mode="checkSchematron">
      <xsl:if test="not(parent::tig or parent::ntig or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">A
								context sentence can only occur at the term (tig)
							level.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='definition']" mode="checkSchematron">
      <xsl:if test="not(parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message"> A definition must occur at the termEntry level, the langSet level,
								or the term (tig) level.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='example']" mode="checkSchematron">
      <xsl:if test="not(parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message"> An example must occur at the termEntry level, the langSet level,
								or the term (tig) level.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='explanation']" mode="checkSchematron">
      <xsl:if test="not(parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message"> An explanation must occur at the termEntry level, the langSet
								level, or the term (tig) level.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='figure']" mode="checkSchematron">
      <xsl:if test="not((.=not(*)) and (parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message"> The content of this element must be plain
								text.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='otherBinaryData']" mode="checkSchematron">
      <xsl:if test="not((.=not(*)) and (parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The content of this element must be plain
								text.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='quantity']" mode="checkSchematron">
      <xsl:if test="not((.=not(*)) and (parent::tig or parent::ntig or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">A quantity should occur at the term (tig or
								ntig) level. The content of this element must be plain
							text.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='range']" mode="checkSchematron">
      <xsl:if test="not((.=not(*)) and (parent::tig or parent::ntig or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">A range should occur at the term (tig or ntig)
								level. The content of this element must be plainText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='relatedConcept']" mode="checkSchematron">
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">Related concepts
								should occur at the termEntry level or the langSet level. The
								content of this element must be expressed in basicText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='relatedConceptBroader']" mode="checkSchematron">
      <xsl:if test="not((.=not(*[not(local-name()='hi')]) )and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">Broader related
								concepts should occur at the termEntry level or the langSet level.
								The content of this element must be expressed in
							basicText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='relatedConceptNarrower']" mode="checkSchematron">
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">Narrower related
								concepts should occur at the termEntry level or the langSet level.
								The content of this element must be expressed in
							basicText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='reliabilityCode']" mode="checkSchematron">
      <xsl:if test="not(((.='1' or .='2' or .='3' or .='4' or .='5' or .='6' or .='7' or .='8' or .='9' or .='10') and (parent::langSet or parent::termEntry or parent::tig or parent::ntig or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">A reliability code can be a value from 1 (least reliable) to 10
								(most reliable).</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='sampleSentence']" mode="checkSchematron">
      <xsl:if test="not(parent::tig or parent::ntig or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message"> A sample sentence can only occur at the
								term (tig) level.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='sequentiallyRelatedConcept']" mode="checkSchematron">
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">Sequentially related
								concepts should occur at the termEntry level or the langSet level.
								The content of this element must be expressed in
							basicText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='spatiallyRelatedConcept']" mode="checkSchematron">
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">Spatially related
								concepts should occur at the termEntry level or the langSet level.
								The content of the element must be expressed in
							basicText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='subjectField']" mode="checkSchematron">
      <xsl:if test="not((.=not(*)) and (parent::termEntry or parent::descripGrp/parent::termEntry))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message"> A subject field must be a plainText value.
								Subject fields usually occur at the termEntry level.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='subordinateConceptGeneric']" mode="checkSchematron">
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message"> Generic subordinate
								concepts should occur at the termEntry level or the langSet level.
								The content of the element must be expressed in
							basicText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='subordinateConceptPartitive']" mode="checkSchematron">
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message"> Partitive
								subordinate concepts should occur at the termEntry level or the
								langSet level. The content of the element must be expressed in
								basicText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='superordinateConceptGeneric']" mode="checkSchematron">
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">Generic
								superordinate concepts should occur at the termEntry level or the
								langSet level. The content of the element must be expressed in
								basicText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='superordinateConceptPartitive']"
                 mode="checkSchematron">
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">Partitive
								superordinate concepts should occur at the termEntry level or the
								langSet level. The content of the element must be expressed in
								basicText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='table']" mode="checkSchematron">
      <xsl:if test="not((.=not(*)) and (parent::langSet or parent::termEntry or parent::tig or parent::ntig or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message"> The content of this element must be plain
								text.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='temporallyRelatedConcept']" mode="checkSchematron">
      <xsl:if test="not((.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">Temporally related
								concepts should occur at the termEntry level or the langSet level.
								The content of the element must be expressed in
							basicText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='thesaurusDescriptor']" mode="checkSchematron">
      <xsl:if test="not((.=not(*)) and (parent::termEntry or parent::descripGrp/parent::termEntry))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">Thesaurus descriptors should occur at the
								termEntry level. The content of this element must be plain
							text.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='unit']" mode="checkSchematron">
      <xsl:if test="not((not (*) and (parent::tig or parent::ntig or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message"> Units should occur at the term (tig or ntig)
								level. Units must be expressed in plainText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descrip[@type='video']" mode="checkSchematron">
      <xsl:if test="not((.=not(*)) and (parent::langSet or parent::termEntry or parent::tig or parent::ntig or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The content of this element must be in plain
								text.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descripNote[@type='contextType']" mode="checkSchematron">
      <xsl:if test="not(. ='definingContext'  or .='explanatoryContext'  or .='associativeContext'  or .='linguisticContext'  or .='metalinguisticContext'  or .='translatedContext')">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message"> Contexts can only be of one of the following types:
								definingContext, explanatoryContext, associativeContext,
								linguisticContext, metalinguisticContext or
							translatedContext.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:descripNote[@type='definitionType']" mode="checkSchematron">
      <xsl:if test="not(.=         'intensionalDefinition' or .='extensionalDefinition' or .='partitiveDefinition' or .='translatedDefinition')">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message"> Definitions can only be of one of the following types:
								intensionalDefinition, extensionalDefinition, partitiveDefinition,
								or translatedDefinition.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='abbreviatedFormFor']" mode="checkSchematron">
      <xsl:if test="not(.=not(*[not(local-name()='hi')]))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message"> The value of the
								abbreviated form in this element must be expressed in basicText.
							</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='administrativeStatus']" mode="checkSchematron">
      <xsl:if test="not(.=         'standardizedTerm-admn-sts' or .='preferredTerm-admn-sts' or .='admittedTerm-admn-sts' or .='deprecatedTerm-admn-sts' or .='supersededTerm-admn-sts' or .='legalTerm-admn-sts' or .='regulatedTerm-admn-sts')">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The administrative status must be standardizedTerm-admn-sts,
								preferredTerm-admn-sts, admittedTerm-admn-sts,
								deprecatedTerm-admn-sts, supersededTerm-admn-sts,
								legalTerm-admn-sts, or regulatedTerm-admn-sts. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='antonymTerm']" mode="checkSchematron">
      <xsl:if test="not((.=not(*[not(local-name()='hi')])))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">Antonym terms should
								occur at the term (tig or ntig) level. The antonym term in this
								element must be expressed in basicText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='directionality']" mode="checkSchematron">
      <xsl:if test="not(. =        'monodirectional' or .='bidirectional' or .='incommensurate')">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The directionality must be monodirectional, bidirectional, or
								incommensurate. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='etymology']" mode="checkSchematron">
      <xsl:if test="not(.=not(*))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">Information about the etymology of a term must
								be expressed in noteText. Etymology information should occur at the
								tig level or at the termCompGrp level.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='falseFriend']" mode="checkSchematron">
      <xsl:if test="not((.=not(*[not(local-name()='hi')])))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message"> The false friend
								must be expressed in basicText. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='frequency']" mode="checkSchematron">
      <xsl:if test="not(. =        'commonlyUsed' or .='infrequentlyUsed' or .='rarelyUsed')">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The frequency must be commonlyUsed, infrequentlyUsed, or
								rarelyUsed. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='geographicalUsage']" mode="checkSchematron">
      <xsl:if test="not(.=not(*))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The geographical usage must be expressed in
								plainText. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='grammaticalGender']" mode="checkSchematron">
      <xsl:if test="not((.='masculine' or .='feminine' or .='neuter' or .='otherGender'))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message"> The gender must be masculine, feminine, neuter, or otherGender.
								Gender should be specified at the term level (tig or ntig) or at the
								termCompGrp level.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='grammaticalNumber']" mode="checkSchematron">
      <xsl:if test="not(. =         'single' or .='plural' or .='dual' or .='mass' or .='otherNumber')">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The grammatical number must be single, plural, dual, mass, or
								otherNumber. The grammatical number should be specified at the term
								level (tig or ntig) or at the termCompGrp level.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='grammaticalValency']" mode="checkSchematron">
      <xsl:if test="not(.=not(*))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message"> The grammatical valency must be expressed in
								plainText. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='homograph']" mode="checkSchematron">
      <xsl:if test="not(.=not(*[not(local-name()='hi')]))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message"> The homograph must
								be expressed in basicText. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='language-planningQualifier']" mode="checkSchematron">
      <xsl:if test="not(. =        'recommendedTerm' or .='nonstandardizedTerm' or .='proposedTerm' or .='newTerm')">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The language planning qualifier must be recommendedTerm,
								nonstandardizedTerm, proposedTerm, or newTerm. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='lionHotkey']" mode="checkSchematron">
      <xsl:if test="not(.=not(*))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The hotkey must be expressed in plainText.
							</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='normativeAuthorization']" mode="checkSchematron">
      <xsl:if test="not(. =        'standardizedTerm' or .='preferredTerm' or .='admittedTerm' or .='deprecatedTerm' or .='supersededTerm' or .='legalTerm' or .='regulatedTerm')">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The normative authorization must be standardizedTerm,
								preferredTerm, admittedTerm, deprecatedTerm, supersededTerm,
								legalTerm, regulatedTerm . </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='partOfSpeech']" mode="checkSchematron">
      <xsl:if test="not(.=not(*))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The part of speech must be a plainText value.
								It should be specified only at the term (tig or ntig) level or at
								the termCompGrp level.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='processStatus']" mode="checkSchematron">
      <xsl:if test="not(. =        'unprocessed' or .='provisionallyProcessed' or .='finalized')">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The process status must be unprocessed, provisionallyProcessed, or
								finalized. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='pronunciation']" mode="checkSchematron">
      <xsl:if test="not(.=not(*[not(local-name()='hi')]))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The pronunciation
								must be expressed in basicText. It should be specified at the term
								(tig or ntig) level or at the termCompGrp level.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='proprietaryRestriction']" mode="checkSchematron">
      <xsl:if test="not(. =        'trademark' or .='serviceMark' or .='tradeName')">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The proprietary restriction must be trademark, serviceMark, or
								tradeName. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='register']" mode="checkSchematron">
      <xsl:if test="not(. =        'colloquialRegister' or .='neutralRegister' or .='technicalRegister' or .='in-houseRegister' or .='bench-levelRegister' or .='slangRegister' or .='vulgarRegister')">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The register must be colloquialRegister, neutralRegister,
								technicalRegister, in-houseRegister, bench-levelRegister,
								slangRegister, or vulgarRegister . </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='shortFormFor']" mode="checkSchematron">
      <xsl:if test="not(.=not(*[not(local-name()='hi')]))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message"> The value of the
								short form in this element must be expressed in
							basicText.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='temporalQualifier']" mode="checkSchematron">
      <xsl:if test="not(. =        'archaicTerm' or .='outdatedTerm' or .='obsoleteTerm')">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The temporal qualifier must be archaicTerm, outdatedTerm, or
								obsoleteTerm. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='termLocation']" mode="checkSchematron">
      <xsl:if test="not(.=not(*))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The termLocation must be a plainText value. It
								should be specified only at the term (tig or ntig)
							level.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='termProvenance']" mode="checkSchematron">
      <xsl:if test="not(. =         'transdisciplinaryBorrowing' or .='translingualBorrowing' or .='loanTranslation' or .='neologism')">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The term provenance must be transdisciplinaryBorrowing,
								translingualBorrowing, loanTranslation, or neologism.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='termType']" mode="checkSchematron">
      <xsl:if test="not(. = 'abbreviation' or .='initialism' or .='acronym' or .='clippedTerm' or .='entryTerm' or .='synonym' or .='internationalScientificTerm' or .='fullForm' or .='transcribedForm' or .='symbol' or .='formula' or .='equation' or .='logicalExpression' or .='commonName' or .='variant' or .='shortForm' or .='transliteratedForm' or .='sku' or .='partNumber' or .='phraseologicalUnit' or .='synonymousPhrase' or .='standardText' or .='string' or .='internationalism' or .='shortcut')">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message">The type of term can only be one of the following values:
								abbreviation, initialism, acronym, clippedTerm, entryTerm, synonym,
								internationalScientificTerm, fullForm, transcribedForm, symbol,
								formula, equation, logicalExpression, commonName, variant,
								shortForm, transliteratedForm, sku, partNumber, phraseologicalUnit,
								synonymousPhrase, standardText, string, internationalism,
							shortcut.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='termStructure']" mode="checkSchematron">
      <xsl:if test="not(.=not(*))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message"> The term structure must be expressed in plain
								text.</xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="tei:termNote[@type='timeRestriction']" mode="checkSchematron">
      <xsl:if test="not(.=not(*))">
         <xsl:call-template name="generateError">
            <xsl:with-param name="message"> The time restriction must be expressed in
								plainText. </xsl:with-param>
         </xsl:call-template>
      </xsl:if>
      <xsl:call-template name="copyIt"/>
   </xsl:template>
   <xsl:template match="processing-instruction()" mode="checkSchematron">
      <xsl:call-template name="copyMe"/>
   </xsl:template>
   <xsl:template match="@*|text()|comment()" mode="checkSchematron">
      <xsl:call-template name="copyMe"/>
   </xsl:template>
   <xsl:template match="*" mode="checkSchematron">
      <xsl:call-template name="copyIt"/>
   </xsl:template>
</xsl:stylesheet>