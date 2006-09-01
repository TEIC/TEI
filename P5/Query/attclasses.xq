declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace rng="http://relaxng.org/ns/structure/1.0";
<attClassList>
{
let $lang := request:get-parameter("lang", "")
for $t in
collection("/db/TEI")//tei:classSpec[@type='atts' and not(@ident='tei.global'
or @ident='tei.TEIform')]
order by $t/@ident
return
<attClass>
 <className>{data($t/@ident)}</className>
 <classDesc>{$t/tei:desc[@xml:lang=$lang]}</classDesc>
 <module>{string($t/@module)}</module>
 <attributes>
{
for $a in $t//tei:attDef
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
    <desc>{data($a/tei:desc[@xml:lang=$lang])}</desc>
</att>
}
 </attributes>
</attClass>
}
</attClassList>
