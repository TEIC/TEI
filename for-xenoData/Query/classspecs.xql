declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace rng="http://relaxng.org/ns/structure/1.0";
<List>
{
for $t in
collection("/db/TEI")//tei:classSpec[(@type='atts' or @type='both')]
order by $t/@ident
return $t
}
</List>