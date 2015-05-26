declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace rng="http://relaxng.org/ns/structure/1.0";
<modelClassList>
{
let $lang := request:get-parameter("lang", "en")
for $t in collection("/db/TEI")//tei:classSpec[@type='model']
let $Desc:=
    if ($t/tei:desc[@xml:lang=$lang]) then
        $t/tei:desc[@xml:lang=$lang]
    else
        $t/tei:desc[not(@xml:lang)]
order by $t/@ident
return
<modelClass>
 <className>{data($t/@ident)}</className>
 <classDesc>{data($Desc)}</classDesc>
</modelClass>
}
</modelClassList>
