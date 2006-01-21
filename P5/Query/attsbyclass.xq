declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace rng="http://relaxng.org/ns/structure/1.0";

let $e := request:request-parameter("name", "")
for $c in collection("/db/TEI")//tei:classSpec[@ident=$e]
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
    <datatype>{
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
    <desc>{data($a/tei:desc)}</desc>
</att>
}
</Element>
