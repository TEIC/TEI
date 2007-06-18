<?php

include("../roma/romadom.php");

$element = $_GET['element'];
$xml_input = implode("", file("tei_all.xml.compiled"));
$DOM = new romaDom($xml_input);
$DOM->getXPath($xpath);

function isElement(&$element, $DOM) {
	$DOM->getXPath($xpath);
	$tmp = $xpath->query("//tei:elementSpec[@ident='$element']" );
	if($tmp->length == 0) {
		return false;
	} else {
		return true;
	}
}

if(!isElement($element, $DOM)) {
	echo "$element is not in tei_all schema !!! \n";
	exit(0);
}

/* liste des attributs possibles pour un élément */
echo "<html><head><title>Possible attributes in $element</title></head><body>";
echo "<h2>Possible attributes in $element</h2>";

/* propres à l'élément */
$DOM->getXPath($xpath);
$items = $xpath->query("//tei:elementSpec[@ident='$element']/tei:attList/tei:attList/tei:attDef");
if($items->length > 0) echo "<p>Specific to this element</p>";
echo "<ul>";
foreach($items as $item) {
	$att_name = $item->getAttribute("ident");
	echo "<li>$att_name</li>";
}
echo "</ul>";


/* provenant de classes d'attributs */
$DOM->getXPath($xpath);
$items = $xpath->query("//tei:elementSpec[@ident='$element']/tei:attList/tei:attRef");
$CLASSES = array();
$MEMBERS = array();
foreach($items as $item) {
	$class_name = $item->getAttribute("name");
	if(ereg("att\.", $class_name)) {
		if(ereg("\.attribute\.", $class_name)) {
			$tmp = explode('.attribute.', $class_name);
			$CLASSES[] = $tmp[0];
			$MEMBERS[$tmp[0]][] = $tmp[1];
		}
	}
}

$CLASSES = array_unique($CLASSES);
foreach($CLASSES as $CLASS) {
	echo "<p>From <b>$CLASS</b></p>";
	echo "<ul>";
	foreach($MEMBERS[$CLASS] as $MEMBER) {
		echo "<li>$MEMBER</li>";
	}
	echo "</ul>";
}

?>
<p><a href="javascript:window.close()">Close this window</a></p>
</body>
</html>