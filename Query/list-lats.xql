for $t in collection("lampeter")//text
let $lats := $t//foreign[@lang='lat']
where count($lats) > 2
order by count($lats)
return 
<latin>
{$lats}
<txt>{$t/@id}</txt>
</latin>
