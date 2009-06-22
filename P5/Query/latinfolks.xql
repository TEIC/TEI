for $t in //TEI.2[teiHeader//country[.='England']]
let $lats := $t//div[@lang='l_LA']
where count($lats) > 0
return 
<person>
<txt>{$t//person[1]/persName}</txt>
{$lats}
</person>