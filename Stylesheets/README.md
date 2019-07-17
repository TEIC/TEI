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


## About the Text Encoding Initiative (TEI)

The Text Encoding Initiative (TEI) is a community of practice in the area now known as textual digital humanities. Since 1994 
the primary output of the TEI has been [the TEI/XML guidelines](https://www.tei-c.org/release/doc/tei-p5-doc/en/html/index.html), a standard for the interchange of textual data. A main focii of the TEI is the [TEI-L mailing list](https://listserv.brown.edu/cgi-bin/wa?A1=ind1904&L=TEI-L); the TEI is also on [github](https://github.com/TEIC/TEI) and [docker](https://hub.docker.com/u/teic), a repository called [TAPAS](https://tapasproject.org/) and an [academic journal, the jTEI](https://journals.openedition.org/jtei/). 

TEI/XML can be thought of as a sibling of HTML (they're approximately the same age, depending on how you measure it) which evolved with a focus on defined textual semantics rather than defined display semantics.  [TEI by example](https://teibyexample.org/) is a good introduction to TEI/XML.
The [Text Encoding Initiative Wikipedia article](https://en.wikipedia.org/wiki/Text_Encoding_Initiative) contains some short examples. 
The TEI/XML standard is used by content-based projects such as 
the [British National Corpus](http://www.natcorp.ox.ac.uk), 
the [Perseus Project](http://www.perseus.tufts.edu/), 
the [Women Writers Project](http://www.wwp.northeastern.edu/), 
the [Oxford Text Archive](http://ota.ox.ac.uk/), 
the [Digital Tripitaka](https://journals.tdl.org/jodi/index.php/jodi/article/view/84/83) and 
[SARIT](http://sarit.indology.info/),
and tool-based projects such as 
[CorrespSearch](https://correspsearch.net/),  
[EpiDoc](http://epidoc.sourceforge.net/), 
[Anthologize](http://anthologize.org/), 
[Versioning Machine](http://v-machine.org/), 
and many more diverse projects.
