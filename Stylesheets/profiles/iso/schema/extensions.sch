<?xml version="1.0" encoding="utf-8"?>
<schema xmlns="http://www.ascc.net/xml/schematron" >
	<!-- queryBinding="xslt2" -->
   <title>Extension rules for ISO standards</title>
   <ns prefix="i" uri="http://www.iso.org/ns/1.0"/>
   <ns prefix="tei" uri="http://www.tei-c.org/ns/1.0"/>
   <ns prefix="rng" uri="http://relaxng.org/ns/structure/1.0"/>

   <pattern name="general">
        <rule context="tei:body">
          <assert test="*">Some content for the body of the document is essential.</assert>
        </rule>
      </pattern>
</schema>
