declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace rng="http://relaxng.org/ns/structure/1.0";
<TEI xmlns="http://www.tei-c.org/ns/1.0" 
     xmlns:xi="http://www.w3.org/2001/XInclude"> 
<text>
<body>
<list type="ordered">
{
for $t in collection("/db/TEI")//tei:elementSpec[.//rng:ref[@name='p']]
let $what:=$t/@ident
let $desc:=$t/tei:desc
order by $t/@ident
return
<item><hi>{data($what)}</hi>:
<eg>{$desc}</eg>
</item>
}
</list>
</body>
</text>
</TEI> 