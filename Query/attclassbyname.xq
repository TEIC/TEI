declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace rng="http://relaxng.org/ns/structure/1.0";
declare namespace request="http://exist-db.org/xquery/request";
let $class := request:get-parameter("class", "")
let $lang := request:get-parameter("lang", "en")
for $t in collection("/db/TEI")//tei:classSpec[@ident=$class]
let $Desc:=
    if ($t/tei:desc[@xml:lang=$lang]) then
        $t/tei:desc[@xml:lang=$lang]
    else
        $t/tei:desc[not(@xml:lang)]
return
<attClass>
 <className>{data($t/@ident)}</className>
 <classDesc>{$Desc}</classDesc>
 <module>{string($t/@module)}</module>
 <attributes>
{
for $a in $t//tei:attDef
let $ADesc:=
    if ($a/tei:desc[@xml:lang=$lang]) then
        $a/tei:desc[@xml:lang=$lang]
    else
        $a/tei:desc[not(@xml:lang)]

return 
 <att>
    <name>
    {$a/@usage}
    {data($a/@ident)}</name>
    <defaultVal>{data($a/tei:defaultVal)}</default>
     { for $d in  $a/tei:datatype return
	 <datatype>
	    { $d/@minOccurs }
	    { $d/@maxOccurs }
	    { $d/* }
         </datatype>
     }
     {  
      for $d in $a/tei:valList[@type='closed']  return
       <valList>
         {for $dv in $d/tei:valItem return
             <valItem>
	        { $dv/@ident }
             </valItem>
             }
       </valList>
      }
    <desc>{data($ADesc)}</desc>
</att>
}
 </attributes>
</attClass>

