<?php
$doc = DOMDocument::load("testminimal.xml");
$xpath = new domxpath($doc);
$xpath->registerNamespace( 'tei', 'http://www.tei-c.org/ns/1.0' );
   $ALL = $xpath->query("//*");
   $ALL_PS = $xpath->query("//tei:p");
   $ALL_HEADS = $xpath->query("//tei:head");
   foreach($ALL_PS as $oElement) {
     print "> " . $oElement->nodeValue;
     }
?>
