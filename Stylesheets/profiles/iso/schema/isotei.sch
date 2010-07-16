<?xml version="1.0" encoding="utf-8"?>
<schema xmlns="http://www.ascc.net/xml/schematron">
   <title>Schematron 1.5 rules</title>
   <ns prefix="i" uri="http://www.iso.org/ns/1.0"/>
   <ns prefix="tei" uri="http://www.tei-c.org/ns/1.0"/>
   <pattern name="isoList">
        <rule context="tei:list[@type='termlist']/tei:item">
          <assert test="@n">Each item in a termlist must have a @n attribute</assert>
        </rule>
      </pattern>
   <pattern name="isoHeader-title">
        <rule context="tei:teiHeader">
          <assert test="tei:fileDesc/tei:titleStmt/tei:title[@i:meta='introductoryTitle']">
		      An introductory component of the title is expected
		    </assert>
          <assert test="tei:fileDesc/tei:titleStmt/tei:title[@i:meta='mainTitle']">
		      A main component of the title is expected
		    </assert>
          <assert test="tei:fileDesc/tei:titleStmt/tei:respStmt/tei:name">
		      No respStmt has been provided, giving SC/TC committee numbers
		  </assert>
          <assert test="tei:fileDesc/tei:publicationStmt/tei:idno[@i:meta='serialNumber']">
		  an idno of type serialNumber is expected</assert>
          <assert test="tei:fileDesc/tei:publicationStmt/tei:idno[@i:meta='stage']">
		  an idno of type stage is expected</assert>
        </rule>
      </pattern>
   <pattern name="text-constraint-isoStructure">
        <rule context="tei:text">
          <assert test="tei:front/tei:div[@type='foreword']">
		A Foreword clause in the front matter is mandatory</assert>
        </rule>
      </pattern>
   <pattern name="text-constraint-isoStructure">
        <rule context="tei:text">
          <assert test="tei:body/tei:div[@type='scope']">
		A Scope clause in the body is mandatory</assert>
        </rule>
      </pattern>
   <pattern name="div-constraint-isoDiv">
        <rule context="tei:div">
          <assert test="not(tei:div) or count(tei:div)&gt;1">a clause must contain at least two subclauses</assert>
        </rule>
      </pattern>
   <pattern name="admin_applicationSubset">
        <rule context="tei:admin[@type='applicationSubset']">
          <assert test=".=not(*)">The application subset must be expressed in
								plainText.</assert>
        </rule>
      </pattern>
   <pattern name="admin_businessUnitSubset">
        <rule context="tei:admin[@type='businessUnitSubset']">
          <assert test=".=not(*)">The business unit subset must be expressed
								in plainText.</assert>
        </rule>
      </pattern>
   <pattern name="admin_conceptOrigin">
        <rule context="tei:admin[@type='conceptOrigin']">
          <assert test=".=not(*)">The concept origin must be expressed in
								plainText.</assert>
        </rule>
      </pattern>
   <pattern name="admin_customerSubset">
        <rule context="tei:admin[@type='customerSubset']">
          <assert test=".=not(*)">The customer subset must be expressed in
								plainText.</assert>
        </rule>
      </pattern>
   <pattern name="admin_databaseType">
        <rule context="tei:admin[@type='databaseType']">
          <assert test=".=not(*)">The type of data base must be expressed in
								plainText.</assert>
        </rule>
      </pattern>
   <pattern name="admin_domainExpert">
        <rule context="tei:admin[@type='domainExpert']">
          <assert test=".=not(*)">The domain expert must be expressed in plain
								text.</assert>
        </rule>
      </pattern>
   <pattern name="admin_elementWorkingStatus">
        <rule context="tei:admin[@type='elementWorkingStatus']">
          <assert test=".='starterElement' or .='workingElement' or .='consolidatedElement' or .='archiveElement' or .='importedElement' or .='exportedElement'"> The element working status must be starterElement, workingElement,
								consolidatedElement, archiveElement, importedElement, or
								exportedElement.</assert>
        </rule>
      </pattern>
   <pattern name="admin_entrySource">
        <rule context="tei:admin[@type='entrySource']">
          <assert test=".=not(*)">The source of the entry must be expressed in
								plainText.</assert>
        </rule>
      </pattern>
   <pattern name="admin_environmentSubset">
        <rule context="tei:admin[@type='environmentSubset']">
          <assert test=".=not(*)">The environment subset must be expressed in
								plainText.</assert>
        </rule>
      </pattern>
   <pattern name="admin_indexHeading">
        <rule context="tei:admin[@type='indexHeading']">
          <assert test=".=not(*)">The index heading must be expressed in plain
								text.</assert>
        </rule>
      </pattern>
   <pattern name="admin_keyword">
        <rule context="tei:admin[@type='keyword']">
          <assert test=".=not(*)">The keyword must be expressed in plain
							text.</assert>
        </rule>
      </pattern>
   <pattern name="admin_originatingPerson">
        <rule context="tei:admin[@type='originatingPerson']">
          <assert test=".=not(*)">The name of the originating person must be
								expressed in plainText.</assert>
        </rule>
      </pattern>
   <pattern name="admin_originatingDatabase">
        <rule context="tei:admin[@type='originatingDatabase']">
          <assert test=".=not(*)">The name of the originating database must be
								expressed in plainText.</assert>
        </rule>
      </pattern>
   <pattern name="admin_originatingInstitution">
        <rule context="tei:admin[@type='originatingInstitution']">
          <assert test=".=not(*)">The name of the originating institution must
								be expressed in plainText.</assert>
        </rule>
      </pattern>
   <pattern name="admin_productSubset">
        <rule context="tei:admin[@type='productSubset']">
          <assert test=".=not(*)">The product subset must be expressed in
								plainText.</assert>
        </rule>
      </pattern>
   <pattern name="admin_projectSubset">
        <rule context="tei:admin[@type='projectSubset']">
          <assert test=".=not(*)">The project subset must be expressed in
								plainText.</assert>
        </rule>
      </pattern>
   <pattern name="admin_securitySubset">
        <rule context="tei:admin[@type='securitySubset']">
          <assert test=".='public' or .='confidential'">The security subset
								must be public or confidential.</assert>
        </rule>
      </pattern>
   <pattern name="admin_searchTerm">
        <rule context="tei:admin[@type='searchTerm']">
          <assert test=".=not(*)">The search term must be expressed in plain
								text.</assert>
        </rule>
      </pattern>
   <pattern name="admin_sourceIdentifier">
        <rule context="tei:admin[@type='sourceIdentifier']">
          <assert test=".=not(*)">The source identifier must be expressed in
								plainText.</assert>
        </rule>
      </pattern>
   <pattern name="admin_sortKey">
        <rule context="tei:admin[@type='sortKey']">
          <assert test=".=not(*)">The sort key must be expressed in
							plainText.</assert>
        </rule>
      </pattern>
   <pattern name="admin_subsetOwner">
        <rule context="tei:admin[@type='subsetOwner']">
          <assert test=".=not(*)">The name of the subset owner must be
								expressed in plainText.</assert>
        </rule>
      </pattern>
   <pattern name="descrip_antonymConcept">
        <rule context="tei:descrip[@type='antonymConcept']">
          <assert test="(.=not(*[not(local-name()='hi')]))and(parent::termEntry or parent::descripGrp/parent::termEntry)">Antonym concepts should occur at the entry (concept) level. The
								antonym-concept in this element must be expressed in basicText.</assert>
        </rule>
      </pattern>
   <pattern name="descrip_associatedConcept">
        <rule context="tei:descrip[@type='associatedConcept']">
          <assert test="(.=not(*[not(local-name()='hi')]))and (parent::termEntry or parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet)">Associated concepts should occur at the termEntry or the langSet
								level. The term in this element must be expressed in
							basicText.</assert>
        </rule>
      </pattern>
   <pattern name="descrip_audio">
        <rule context="tei:descrip[@type='audio']">
          <assert test="(.=not(*))and (parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)">The content of this element must be plain text. It can occur at the concept (termEntry) level, the langSet level or the term (tig or ntig) level.</assert>
        </rule>
      </pattern>
   <pattern name="descrip_broaderConceptGeneric">
        <rule context="tei:descrip[@type='broaderConceptGeneric']">
          <assert test="(.=not(*[not(local-name()='hi')])) and (parent::termEntry or parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet)">Generic broader concepts should occur at the termEntry level or the
								langSet level. The term in this element must be expressed in
								basicText.</assert>
        </rule>
      </pattern>
   <pattern name="descrip_broaderConceptPartitive">
        <rule context="tei:descrip[@type='broaderConceptPartitive']">
          <assert test="(.=not(*[not(local-name()='hi')])) and (parent::termEntry or parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet)">Partitive broader concepts should occur at the termEntry level or
								the langSet level. The term in this element must be expressed in
								basicText.</assert>
        </rule>
      </pattern>
   <pattern name="descrip_characteristic">
        <rule context="tei:descrip[@type='characteristic']">
          <assert test="(.=not(*))and (parent::tig or parent::ntig or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)">A
								characteristic should only occur at the term (tig or ntig) level.
								The content of this element must be plain text.</assert>
        </rule>
      </pattern>
   <pattern name="descrip_classificationCode">
        <rule context="tei:descrip[@type='classificationCode']">
          <assert test="(.=not(*)) and (parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)"> The content of this element must be plainText.</assert>
        </rule>
      </pattern>
   <pattern name="descrip_conceptPosition">
        <rule context="tei:descrip[@type='conceptPosition']">
          <assert test="(.=not(*)) and (parent::termEntry or parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet)"> Information about a concept position should occur at the termEntry
								level or the langSet level, and it must be expressed in
							plainText.</assert>
        </rule>
      </pattern>
   <pattern name="descrip_coordinateConceptGeneric">
        <rule context="tei:descrip[@type='coordinateConceptGeneric']">
          <assert test="(.=not(*[not(local-name()='hi')])) and (parent::termEntry or parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet)">Generic coordinate concepts should occur at the termEntry level or
								the langSet level. The term in this element must be expressed in
								basicText.</assert>
        </rule>
      </pattern>
   <pattern name="descrip_coordinateConceptPartitive">
        <rule context="tei:descrip[@type='coordinateConceptPartitive']">
          <assert test="(.=not(*[not(local-name()='hi')])) and (parent::termEntry or parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet)">Partitive coordinate concepts should occur at the termEntry level
								or the langSet level. The term in this element must be expressed in
								basicText.</assert>
        </rule>
      </pattern>
   <pattern name="descrip_context">
        <rule context="tei:descrip[@type='context']">
          <assert test="parent::tig or parent::ntig or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig">A
								context sentence can only occur at the term (tig)
							level.</assert>
        </rule>
      </pattern>
   <pattern name="descrip_definition">
        <rule context="tei:descrip[@type='definition']">
          <assert test="parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig"> A definition must occur at the termEntry level, the langSet level,
								or the term (tig) level.</assert>
        </rule>
      </pattern>
   <pattern name="descrip_example">
        <rule context="tei:descrip[@type='example']">
          <assert test="parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig"> An example must occur at the termEntry level, the langSet level,
								or the term (tig) level.</assert>
        </rule>
      </pattern>
   <pattern name="descrip_explanation">
        <rule context="tei:descrip[@type='explanation']">
          <assert test="parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig"> An explanation must occur at the termEntry level, the langSet
								level, or the term (tig) level.</assert>
        </rule>
      </pattern>
   <pattern name="descrip_figure">
        <rule context="tei:descrip[@type='figure']">
          <assert test="(.=not(*)) and (parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)"> The content of this element must be plain
								text.</assert>
        </rule>
      </pattern>
   <pattern name="descrip_otherBinaryData">
        <rule context="tei:descrip[@type='otherBinaryData']">
          <assert test="(.=not(*)) and (parent::termEntry or parent::langSet or parent::tig or parent::ntig or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)">The content of this element must be plain
								text.</assert>
        </rule>
      </pattern>
   <pattern name="descrip_quantity">
        <rule context="tei:descrip[@type='quantity']">
          <assert test="(.=not(*)) and (parent::tig or parent::ntig or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)">A quantity should occur at the term (tig or
								ntig) level. The content of this element must be plain
							text.</assert>
        </rule>
      </pattern>
   <pattern name="descrip_range">
        <rule context="tei:descrip[@type='range']">
          <assert test="(.=not(*)) and (parent::tig or parent::ntig or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)">A range should occur at the term (tig or ntig)
								level. The content of this element must be plainText.</assert>
        </rule>
      </pattern>
   <pattern name="descrip_relatedConcept">
        <rule context="tei:descrip[@type='relatedConcept']">
          <assert test="(.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry)">Related concepts
								should occur at the termEntry level or the langSet level. The
								content of this element must be expressed in basicText.</assert>
        </rule>
      </pattern>
   <pattern name="descrip_relatedConceptBroader">
        <rule context="tei:descrip[@type='relatedConceptBroader']">
          <assert test="(.=not(*[not(local-name()='hi')]) )and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry)">Broader related
								concepts should occur at the termEntry level or the langSet level.
								The content of this element must be expressed in
							basicText.</assert>
        </rule>
      </pattern>
   <pattern name="descrip_relatedConceptNarrower">
        <rule context="tei:descrip[@type='relatedConceptNarrower']">
          <assert test="(.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry)">Narrower related
								concepts should occur at the termEntry level or the langSet level.
								The content of this element must be expressed in
							basicText.</assert>
        </rule>
      </pattern>
   <pattern name="descrip_reliabilityCode">
        <rule context="tei:descrip[@type='reliabilityCode']">
          <assert test="((.='1' or .='2' or .='3' or .='4' or .='5' or .='6' or .='7' or .='8' or .='9' or .='10') and (parent::langSet or parent::termEntry or parent::tig or parent::ntig or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig))">A reliability code can be a value from 1 (least reliable) to 10
								(most reliable).</assert>
        </rule>
      </pattern>
   <pattern name="descrip_sampleSentence">
        <rule context="tei:descrip[@type='sampleSentence']">
          <assert test="parent::tig or parent::ntig or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig"> A sample sentence can only occur at the
								term (tig) level.</assert>
        </rule>
      </pattern>
   <pattern name="descrip_sequentiallyRelatedConcept">
        <rule context="tei:descrip[@type='sequentiallyRelatedConcept']">
          <assert test="(.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry)">Sequentially related
								concepts should occur at the termEntry level or the langSet level.
								The content of this element must be expressed in
							basicText.</assert>
        </rule>
      </pattern>
   <pattern name="descrip_spatiallyRelatedConcept">
        <rule context="tei:descrip[@type='spatiallyRelatedConcept']">
          <assert test="(.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry)">Spatially related
								concepts should occur at the termEntry level or the langSet level.
								The content of the element must be expressed in
							basicText.</assert>
        </rule>
      </pattern>
   <pattern name="descrip_subjectField">
        <rule context="tei:descrip[@type='subjectField']">
          <assert test="(.=not(*)) and (parent::termEntry or parent::descripGrp/parent::termEntry)"> A subject field must be a plainText value.
								Subject fields usually occur at the termEntry level.</assert>
        </rule>
      </pattern>
   <pattern name="descrip_subordinateConceptGeneric">
        <rule context="tei:descrip[@type='subordinateConceptGeneric']">
          <assert test="(.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry)"> Generic subordinate
								concepts should occur at the termEntry level or the langSet level.
								The content of the element must be expressed in
							basicText.</assert>
        </rule>
      </pattern>
   <pattern name="descrip_subordinateConceptPartitive">
        <rule context="tei:descrip[@type='subordinateConceptPartitive']">
          <assert test="(.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry)"> Partitive
								subordinate concepts should occur at the termEntry level or the
								langSet level. The content of the element must be expressed in
								basicText.</assert>
        </rule>
      </pattern>
   <pattern name="descrip_superordinateConceptGeneric">
        <rule context="tei:descrip[@type='superordinateConceptGeneric']">
          <assert test="(.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry)">Generic
								superordinate concepts should occur at the termEntry level or the
								langSet level. The content of the element must be expressed in
								basicText.</assert>
        </rule>
      </pattern>
   <pattern name="descrip_superordinateConceptPartitive">
        <rule context="tei:descrip[@type='superordinateConceptPartitive']">
          <assert test="(.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry)">Partitive
								superordinate concepts should occur at the termEntry level or the
								langSet level. The content of the element must be expressed in
								basicText.</assert>
        </rule>
      </pattern>
   <pattern name="descrip_table">
        <rule context="tei:descrip[@type='table']">
          <assert test="(.=not(*)) and (parent::langSet or parent::termEntry or parent::tig or parent::ntig or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)"> The content of this element must be plain
								text.</assert>
        </rule>
      </pattern>
   <pattern name="descrip_temporallyRelatedConcept">
        <rule context="tei:descrip[@type='temporallyRelatedConcept']">
          <assert test="(.=not(*[not(local-name()='hi')])) and (parent::langSet or parent::termEntry or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry)">Temporally related
								concepts should occur at the termEntry level or the langSet level.
								The content of the element must be expressed in
							basicText.</assert>
        </rule>
      </pattern>
   <pattern name="descrip_thesaurusDescriptor">
        <rule context="tei:descrip[@type='thesaurusDescriptor']">
          <assert test="(.=not(*)) and (parent::termEntry or parent::descripGrp/parent::termEntry)">Thesaurus descriptors should occur at the
								termEntry level. The content of this element must be plain
							text.</assert>
        </rule>
      </pattern>
   <pattern name="descrip_unit">
        <rule context="tei:descrip[@type='unit']">
          <assert test="(not (*) and (parent::tig or parent::ntig or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig))"> Units should occur at the term (tig or ntig)
								level. Units must be expressed in plainText.</assert>
        </rule>
      </pattern>
   <pattern name="descrip_video">
        <rule context="tei:descrip[@type='video']">
          <assert test="(.=not(*)) and (parent::langSet or parent::termEntry or parent::tig or parent::ntig or parent::descripGrp/parent::langSet or parent::descripGrp/parent::termEntry or parent::descripGrp/parent::tig or parent::descripGrp/parent::ntig)">The content of this element must be in plain
								text.</assert>
        </rule>
      </pattern>
   <pattern name="descripNote_contextType">
        <rule context="tei:descripNote[@type='contextType']">
          <assert test=". ='definingContext'  or .='explanatoryContext'  or .='associativeContext'  or .='linguisticContext'  or .='metalinguisticContext'  or .='translatedContext'"> Contexts can only be of one of the following types:
								definingContext, explanatoryContext, associativeContext,
								linguisticContext, metalinguisticContext or
							translatedContext.</assert>
        </rule>
      </pattern>
   <pattern name="descripNote_definitionType">
        <rule context="tei:descripNote[@type='definitionType']">
          <assert test=".=         'intensionalDefinition' or .='extensionalDefinition' or .='partitiveDefinition' or .='translatedDefinition'"> Definitions can only be of one of the following types:
								intensionalDefinition, extensionalDefinition, partitiveDefinition,
								or translatedDefinition.</assert>
        </rule>
      </pattern>
   <pattern name="termNote_abbreviatedFormFor">
        <rule context="tei:termNote[@type='abbreviatedFormFor']">
          <assert test=".=not(*[not(local-name()='hi')])"> The value of the
								abbreviated form in this element must be expressed in basicText.
							</assert>
        </rule>
      </pattern>
   <pattern name="termNote_administrativeStatus">
        <rule context="tei:termNote[@type='administrativeStatus']">
          <assert test=".=         'standardizedTerm-admn-sts' or .='preferredTerm-admn-sts' or .='admittedTerm-admn-sts' or .='deprecatedTerm-admn-sts' or .='supersededTerm-admn-sts' or .='legalTerm-admn-sts' or .='regulatedTerm-admn-sts'">The administrative status must be standardizedTerm-admn-sts,
								preferredTerm-admn-sts, admittedTerm-admn-sts,
								deprecatedTerm-admn-sts, supersededTerm-admn-sts,
								legalTerm-admn-sts, or regulatedTerm-admn-sts. </assert>
        </rule>
      </pattern>
   <pattern name="termNote_antonymTerm">
        <rule context="tei:termNote[@type='antonymTerm']">
          <assert test="(.=not(*[not(local-name()='hi')]))">Antonym terms should
								occur at the term (tig or ntig) level. The antonym term in this
								element must be expressed in basicText.</assert>
        </rule>
      </pattern>
   <pattern name="termNote_directionality">
        <rule context="tei:termNote[@type='directionality']">
          <assert test=". =        'monodirectional' or .='bidirectional' or .='incommensurate'">The directionality must be monodirectional, bidirectional, or
								incommensurate. </assert>
        </rule>
      </pattern>
   <pattern name="termNote_etymology">
        <rule context="tei:termNote[@type='etymology']">
          <assert test=".=not(*)">Information about the etymology of a term must
								be expressed in noteText. Etymology information should occur at the
								tig level or at the termCompGrp level.</assert>
        </rule>
      </pattern>
   <pattern name="termNote_falseFriend">
        <rule context="tei:termNote[@type='falseFriend']">
          <assert test="(.=not(*[not(local-name()='hi')]))"> The false friend
								must be expressed in basicText. </assert>
        </rule>
      </pattern>
   <pattern name="termNote_frequency">
        <rule context="tei:termNote[@type='frequency']">
          <assert test=". =        'commonlyUsed' or .='infrequentlyUsed' or .='rarelyUsed'">The frequency must be commonlyUsed, infrequentlyUsed, or
								rarelyUsed. </assert>
        </rule>
      </pattern>
   <pattern name="termNote_geographicalUsage">
        <rule context="tei:termNote[@type='geographicalUsage']">
          <assert test=".=not(*)">The geographical usage must be expressed in
								plainText. </assert>
        </rule>
      </pattern>
   <pattern name="termNote_grammaticalGender">
        <rule context="tei:termNote[@type='grammaticalGender']">
          <assert test="(.='masculine' or .='feminine' or .='neuter' or .='otherGender')"> The gender must be masculine, feminine, neuter, or otherGender.
								Gender should be specified at the term level (tig or ntig) or at the
								termCompGrp level.</assert>
        </rule>
      </pattern>
   <pattern name="termNote_grammaticalNumber">
        <rule context="tei:termNote[@type='grammaticalNumber']">
          <assert test=". =         'single' or .='plural' or .='dual' or .='mass' or .='otherNumber'">The grammatical number must be single, plural, dual, mass, or
								otherNumber. The grammatical number should be specified at the term
								level (tig or ntig) or at the termCompGrp level.</assert>
        </rule>
      </pattern>
   <pattern name="termNote_grammaticalValency">
        <rule context="tei:termNote[@type='grammaticalValency']">
          <assert test=".=not(*)"> The grammatical valency must be expressed in
								plainText. </assert>
        </rule>
      </pattern>
   <pattern name="termNote_homograph">
        <rule context="tei:termNote[@type='homograph']">
          <assert test=".=not(*[not(local-name()='hi')])"> The homograph must
								be expressed in basicText. </assert>
        </rule>
      </pattern>
   <pattern name="termNote_language-planningQualifier">
        <rule context="tei:termNote[@type='language-planningQualifier']">
          <assert test=". =        'recommendedTerm' or .='nonstandardizedTerm' or .='proposedTerm' or .='newTerm'">The language planning qualifier must be recommendedTerm,
								nonstandardizedTerm, proposedTerm, or newTerm. </assert>
        </rule>
      </pattern>
   <pattern name="termNote_lionHotkey">
        <rule context="tei:termNote[@type='lionHotkey']">
          <assert test=".=not(*)">The hotkey must be expressed in plainText.
							</assert>
        </rule>
      </pattern>
   <pattern name="termNote_normativeAuthorization">
        <rule context="tei:termNote[@type='normativeAuthorization']">
          <assert test=". =        'standardizedTerm' or .='preferredTerm' or .='admittedTerm' or .='deprecatedTerm' or .='supersededTerm' or .='legalTerm' or .='regulatedTerm'">The normative authorization must be standardizedTerm,
								preferredTerm, admittedTerm, deprecatedTerm, supersededTerm,
								legalTerm, regulatedTerm . </assert>
        </rule>
      </pattern>
   <pattern name="termNote_partOfSpeech">
        <rule context="tei:termNote[@type='partOfSpeech']">
          <assert test=".=not(*)">The part of speech must be a plainText value.
								It should be specified only at the term (tig or ntig) level or at
								the termCompGrp level.</assert>
        </rule>
      </pattern>
   <pattern name="termNote_processStatus">
        <rule context="tei:termNote[@type='processStatus']">
          <assert test=". =        'unprocessed' or .='provisionallyProcessed' or .='finalized'">The process status must be unprocessed, provisionallyProcessed, or
								finalized. </assert>
        </rule>
      </pattern>
   <pattern name="termNote_pronunciation">
        <rule context="tei:termNote[@type='pronunciation']">
          <assert test=".=not(*[not(local-name()='hi')])">The pronunciation
								must be expressed in basicText. It should be specified at the term
								(tig or ntig) level or at the termCompGrp level.</assert>
        </rule>
      </pattern>
   <pattern name="termNote_proprietaryRestriction">
        <rule context="tei:termNote[@type='proprietaryRestriction']">
          <assert test=". =        'trademark' or .='serviceMark' or .='tradeName'">The proprietary restriction must be trademark, serviceMark, or
								tradeName. </assert>
        </rule>
      </pattern>
   <pattern name="termNote_register">
        <rule context="tei:termNote[@type='register']">
          <assert test=". =        'colloquialRegister' or .='neutralRegister' or .='technicalRegister' or .='in-houseRegister' or .='bench-levelRegister' or .='slangRegister' or .='vulgarRegister'">The register must be colloquialRegister, neutralRegister,
								technicalRegister, in-houseRegister, bench-levelRegister,
								slangRegister, or vulgarRegister . </assert>
        </rule>
      </pattern>
   <pattern name="termNote_shortFormFor">
        <rule context="tei:termNote[@type='shortFormFor']">
          <assert test=".=not(*[not(local-name()='hi')])"> The value of the
								short form in this element must be expressed in
							basicText.</assert>
        </rule>
      </pattern>
   <pattern name="termNote_temporalQualifier">
        <rule context="tei:termNote[@type='temporalQualifier']">
          <assert test=". =        'archaicTerm' or .='outdatedTerm' or .='obsoleteTerm'">The temporal qualifier must be archaicTerm, outdatedTerm, or
								obsoleteTerm. </assert>
        </rule>
      </pattern>
   <pattern name="termNote_termLocation">
        <rule context="tei:termNote[@type='termLocation']">
          <assert test=".=not(*)">The termLocation must be a plainText value. It
								should be specified only at the term (tig or ntig)
							level.</assert>
        </rule>
      </pattern>
   <pattern name="termNote_termProvenance">
        <rule context="tei:termNote[@type='termProvenance']">
          <assert test=". =         'transdisciplinaryBorrowing' or .='translingualBorrowing' or .='loanTranslation' or .='neologism'">The term provenance must be transdisciplinaryBorrowing,
								translingualBorrowing, loanTranslation, or neologism.</assert>
        </rule>
      </pattern>
   <pattern name="termNote_termType">
        <rule context="tei:termNote[@type='termType']">
          <assert test=". = 'abbreviation' or .='initialism' or .='acronym' or .='clippedTerm' or .='entryTerm' or .='synonym' or .='internationalScientificTerm' or .='fullForm' or .='transcribedForm' or .='symbol' or .='formula' or .='equation' or .='logicalExpression' or .='commonName' or .='variant' or .='shortForm' or .='transliteratedForm' or .='sku' or .='partNumber' or .='phraseologicalUnit' or .='synonymousPhrase' or .='standardText' or .='string' or .='internationalism' or .='shortcut'">The type of term can only be one of the following values:
								abbreviation, initialism, acronym, clippedTerm, entryTerm, synonym,
								internationalScientificTerm, fullForm, transcribedForm, symbol,
								formula, equation, logicalExpression, commonName, variant,
								shortForm, transliteratedForm, sku, partNumber, phraseologicalUnit,
								synonymousPhrase, standardText, string, internationalism,
							shortcut.</assert>
        </rule>
      </pattern>
   <pattern name="termNote_termStructure">
        <rule context="tei:termNote[@type='termStructure']">
          <assert test=".=not(*)"> The term structure must be expressed in plain
								text.</assert>
        </rule>
      </pattern>
   <pattern name="termNote_timeRestriction">
        <rule context="tei:termNote[@type='timeRestriction']">
          <assert test=".=not(*)"> The time restriction must be expressed in
								plainText. </assert>
        </rule>
      </pattern>
</schema>