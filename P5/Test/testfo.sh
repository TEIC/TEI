#!/bin/bash

# Test file to run the conversions from P5 odd files to documentation
# in PDF format through XSL:FO and FOP. 
# Created by MDH 2016-12-26 as part of work on:
# https://github.com/TEIC/TEI/issues/1559

XSL=`pwd`/../../../Stylesheets
testbasic=ANT_OPTS="-Xss2m -Xmx752m -Djava.awt.headless=true" ant -lib ../Utilities/lib/saxon10he.jar:../Utilities/lib/jing.jar -Dtrang=../Utilities/lib/trang.jar -f antruntest.xml -DdefaultSource=`pwd`/../p5subset.xml -DXSL=${XSL} -Doutputname=testbasic -DoddFile=testbasic.odd -Ddebug=true compileodd docfopdf cleanup

simplePrint=ANT_OPTS="-Xss2m -Xmx752m -Djava.awt.headless=true" ant -lib ../Utilities/lib/saxon10he.jar:../Utilities/lib/jing.jar -Dtrang=../Utilities/lib/trang.jar -f antruntest.xml -DdefaultSource=`pwd`/../p5subset.xml -DXSL=${XSL} -Doutputname=simplePrint -DoddFile=`pwd`/../Exemplars/tei_simplePrint.odd -Ddebug=true compileodd docfopdf cleanup

${testbasic}

${simplePrint}
