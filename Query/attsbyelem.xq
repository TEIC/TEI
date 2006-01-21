declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace rng="http://relaxng.org/ns/structure/1.0";

let $e := request:request-parameter("name", "")
for $c in collection("/db/TEI")//tei:elementSpec[@ident=$e]
return
<Element>
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
    if (tei:valList[@type='closed'])
    then
      'Closed list'
    else
      string($a/tei:datatype/*)
     }
     </datatype>
    <desc>{data($a/tei:desc)}</desc>
</att>
}
</Element>