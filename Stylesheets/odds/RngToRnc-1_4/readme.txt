RngToRnc 1.4
-------------

Copyright (c) 2003, Pantor Engineering AB

The RngToRnc package contains stylesheets for converting
RELAX NG schemas in the XML Syntax [1] to the Compact Syntax [2].
The conversion is done in two steps. The basic conversion
to the compact syntax is handled by the RngToRncXml.xsl stylesheet.
The result is an intermediate representation of a compact schema
expressed in XML. To get the final result, a second stylesheet
must be applied to the intermediate result. Included in the package
are output modules for plain text and HTML. You can also use
the RngToRncXml.xsl stylesheet as a module in your own stylesheets
if you want other output formats or styling.

If your XSLT processor supports the exslt:node-set () function [3]
or similar, then a translation can be done by a single invocation
of either RngToRncHtml.xsl or RngToRncText.xsl. Otherwise, you
can first apply the RngToRncXml.xsl stylesheet and store the result
in a file, and then use that intermediate file as input to the
output modules. The processor specific instructions are located
in the RngToRncProcessorConfig.xsl module.

For further information and descriptions of stylesheet parameters,
see comments in the stylesheets.

RngToRncClassic.xsl is the non-modularized stylesheet that preceded
this version. It is provided as a fallback if you have trouble with
the more processor dependent nature of the new solution. However, it
can only produce plain text output.

References
----------

[1] http://www.oasis-open.org/committees/relax-ng/spec-20011203.html
[2] http://www.oasis-open.org/committees/relax-ng/compact-20021121.html
[3] http://www.exslt.org/exsl/functions/node-set/
