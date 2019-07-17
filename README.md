# Stylesheets

[![Build Status](https://travis-ci.org/TEIC/Stylesheets.svg?branch=dev)](https://travis-ci.org/TEIC/Stylesheets)

TEI XSL Stylesheets

This is a family of XSLT 2.0 stylesheets to transform TEI XML documents to various formats, including XHTML, LaTeX, XSL Formatting Objects, ePub, plain text, RDF, JSON; and to/from Word OOXML (docx) and OpenOfice (odt).  They concentrate on the core TEI modules which are used for simple transcription and "born digital" writing. It is important to understand that they do _not_:

*   cover all TEI elements and possible attribute values
*   attempt to define a standard TEI processing or rendering model

and should not be treated as the definitive view of the TEI Consortium.

## Prerequisites
The package assumes that you have several additional tools installed. Their availability on your system can be verified by issuing the command `make check`.

In particular, Stylesheets assume that you use `ant` version 1.9.x+. If for some reason, you need to use `ant` 1.8.x, you should remove all occurences of the attribute `@zip64Mode` from the file `common/teianttasks.xml`.

It is helpful to have the TEI environment installed locally. Please refer to http://www.tei-c.org/Guidelines/P5/get.xml for hints on how to do that.

It is also possible to avoid manual installation of additional tools, by resorting to the pre-built test environment in Docker described in http://teic.github.io/TCW/testing_and_building.html .

## Usage
The `bin/` directory contains several executable files, which can be run on Linux, OS X, or other Unix operating systems. These perform a variety of transformations and are very useful for, e.g., generating a schema from a TEI ODD. Some examples:
```bash
bin/teitorelaxng --odd ../TEI/P5/Exemplars/tei_all.odd tei_all.rng
```
Assuming you have a copy of the TEI Guidelines repository alongside your copy of the Stylesheets, this will take the tei_all ODD and generate a RelaxNG XML schema for you. Similarly,
```bash
bin/teitornc --odd ../TEI/P5/Exemplars/tei_lite.odd tei_lite.rnc
```
will produce a RelaxNG Compact Syntax schema for TEI Lite.
