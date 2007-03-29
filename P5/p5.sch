<?xml version="1.0"?>
<s:schema xmlns:s="http://www.ascc.net/xml/schematron" xmlns:rng="http://relaxng.org/ns/structure/1.0" xmlns:tei="http://www.tei-c.org/ns/1.0"><s:title>Schematron rules for TEI</s:title><s:ns prefix="tei" uri="http://www.tei-c.org/ns/1.0"/><s:pattern name="spanTo_required">
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
    </s:pattern></s:schema>
