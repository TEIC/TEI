#!/bin/bash

cd P5
make p5.xml
saxon -s:p5subset.xml -xsl:Utilities/generate_i18n_spec_lists.xsl -o:spec_lists.json