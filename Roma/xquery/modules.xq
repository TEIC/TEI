declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace rng="http://relaxng.org/ns/structure/1.0";
<teiModulesList>
{
for $t in collection("/db/TEI")//tei:moduleSpec[not(@type='decls')]
let $what:=$t/@ident
let $desc:=$t/tei:desc
order by $t/@ident
return
  <teiModule>
   <moduleName>{data($what)}</moduleName>
   <moduleDesc>{data($desc)}</moduleDesc>
  </teiModule>
}
</teiModulesList>
