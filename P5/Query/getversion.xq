declare namespace tei="http://www.tei-c.org/ns/1.0";
declare namespace request="http://exist-db.org/xquery/request";
for $c in collection("/db/TEI")/tei:TEI/tei:teiHeader/tei:fileDesc/tei:editionStmt
return
$c/tei:edition

