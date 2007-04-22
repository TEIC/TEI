<?xml version="1.0"?>
<s:schema xmlns:s="http://www.ascc.net/xml/schematron" xmlns:rng="http://relaxng.org/ns/structure/1.0" xmlns:tei="http://www.tei-c.org/ns/1.0"><s:title>Schematron rules for TEI</s:title><s:ns prefix="tei" uri="http://www.tei-c.org/ns/1.0"/><s:ns prefix="rng" uri="http://relaxng.org/ns/structure/1.0"/><s:pattern name="spanTo_required">
      <s:rule context="tei:addSpan">
	<s:assert test="@spanTo">The spanTo= attribute of <s:name/> is required.</s:assert>
      </s:rule>
    </s:pattern><s:pattern name="spanTo_required">
      <s:rule context="tei:delSpan">
	<s:assert test="@spanTo">The spanTo= attribute of <s:name/> is required.</s:assert>
      </s:rule>
    </s:pattern><s:pattern name="testschemapattern">
      <s:rule context="tei:moduleRef">
	<s:report test="* and @key">
	  child elements of moduleRef are only allowed when an external module
	  is being loaded
	</s:report>
      </s:rule>
    </s:pattern><s:pattern name="enumeration">
      <s:rule context="tei:valList[@type='closed']">
	<!-- This is a pretty complicated test. -->
	<!-- What we're trying to match is cases when the attribute -->
	<!-- we are currently defining (which has a closed value -->
	<!-- list or we wouldn't be here) has a <datatype> of -->
	<!-- data.enumerated, but does *not* specify a non-default -->
	<!-- range of values with minOccurs= or maxOccurs=. -->
	<s:report test="preceding-sibling::tei:datatype[        (not(@minOccurs) or @minOccurs=1 or @minOccurs=0)        and        (not(@maxOccurs) or @maxOccurs=1)      ]      /rng:ref[@name='data.enumerated']">It is not necessary to specify a datatype with a closed value list </s:report>
	<s:report test="preceding-sibling::tei:datatype/rng:ref[@name!='data.enumerated']">Datatypes other than data.enumerated should not be used with closed value lists
	</s:report>
      </s:rule>
      <s:rule context="tei:valList[not(@type) or @type!='closed']">
	<s:assert test="preceding-sibling::tei:datatype/rng:ref[@name='data.enumerated']">Open value lists must be associated with data.enumerated </s:assert>
      </s:rule>
  </s:pattern></s:schema>
