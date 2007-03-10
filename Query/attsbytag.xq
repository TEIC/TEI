declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace rng="http://relaxng.org/ns/structure/1.0";
declare namespace request="http://exist-db.org/xquery/request";
let $e := request:get-parameter("name", "")
let $lang := request:get-parameter("lang", "")
for $c in collection("/db/TEI")//(tei:elementSpec|tei:classSpec)[@ident=$e]
return
<Tag>
{
for $a in $c//tei:attDef
return 
 <att>
    <name>
    {$a/@usage}
    {data($a/@ident)}</name>
    <default>{data($a/tei:defaultVal)}</default>

    <datatype>
    {
      string($a/tei:datatype/*)
     }</datatype>
     <valList>
       {
	for $v in $a/tei:valList/tei:valItem return
	<valItem>
	   {$v/@ident}
	</valItem>
       }
     </valList>	
    <desc>{    if ($t/tei:desc[@xml:lang=$lang]) then
        data($t/tei:desc[@xml:lang=$lang])
    else
        data($t/tei:desc[not(@xml:lang)])
	}</desc>
</att>
}
</Tag>