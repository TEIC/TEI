test:
	xsltproc -o test-de.xml --stringparam verbose true --stringparam TEINAMES teinames.xml --stringparam lang de translate-instance.xsl test.xml
	xsltproc -o test-back.xml --stringparam TEINAMES teinames.xml --stringparam lang de translate-frominstance.xsl test-de.xml
