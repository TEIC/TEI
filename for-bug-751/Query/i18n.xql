declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace rng="http://relaxng.org/ns/structure/1.0";
declare namespace html="http://www.w3.org/1999/xhtml";

<i18n>
{
for $c in collection("/db/TEI")/i18n/*
return $c
}
</i18n>
