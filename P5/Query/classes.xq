declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace rng="http://relaxng.org/ns/structure/1.0";
<modelClassList>
{
for $t in collection("/db/TEI")//tei:classSpec[@type='model']
order by $t/@ident
return
<modelClass>
 <className>{data($t/@ident)}</className>
 <classDesc>{$t/tei:desc}</classDesc>
</modelClass>
}
</modelClassList>
