declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace rng="http://relaxng.org/ns/structure/1.0";
<macroList>
{
for $t in collection("/db/TEI")//tei:macroSpec[starts-with(@ident,'macro')]
order by $t/@ident
return
<macro><macroName>{data($t/@ident)}</macroName><macroDesc>{$t/tei:desc}</macroDesc></macro>
}
</macroList>
