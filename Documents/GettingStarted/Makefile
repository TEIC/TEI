go:
	xmllint --xinclude gettingstarted.xml > gs.xml
	saxon gs.xml /usr/share/xml/tei/stylesheet/odds2/odd2html.xsl  STDOUT=false splitLevel=0 outputDir=html
