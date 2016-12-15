This directory contains scripts used during the process of converting
the source of TEI P5 to use "pure ODD", carried out prior to Release
3.0, by Lou Burnard.

This was a semi automatic process, i.e. some tweaks had to be applied
manually. However the bulk of the conversion is handled as shown in
the shell script "purify.sh" and follows these steps:

a) first we create new dataSpec elements for all existing TEI datatype
macros. This is done by applying the stylesheet purifyDataSpecs.xsl to
each existing data.foo file in turn.

b) then we apply the conversions defined by stylesheet purify.xsl to
each existing spec file in turn, *excluding* the dataspecs (pure and
impure)

After the conversion was run, Syd Bauman tidied up the XSLT code a
bit, but I don't think it's been changed significantly.