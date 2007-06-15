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

function getElementName($element) {
	if($element->nodeName == "elementSpec" || $element->nodeName == "classSpec") {
		return $element->getAttribute("ident");
	} else if($element->nodeName == "ref" || $element->nodeName == "rng:ref") {
		return $element->getAttribute("name");
	} else {
		return getElementName($element->parentNode);
	}
}

if(!isElement($element, $DOM)) {
	echo "$element is not in tei_all schema !!! \n";
	exit(0);
}

/* liste des pères possibles pour un élément */
echo "<html><head><title>Possible parents of $element</title></head><body>";
echo "<h2>Possible parents of $element</h2>";

/* direct access */
echo "<p>Direct access:</p>";
$DOM->getXPath($xpath);
$items = $xpath->query("//rng:ref[@name='$element']");
$PARENTS = array();
foreach($items as $item) {
	$PARENTS[] = getElementName($item->parentNode);
}
$PARENTS = array_unique($PARENTS);
foreach($PARENTS as $parent) {
	echo "<li><a href=\"parents.php?element=$parent\">$parent</a></li>";
}

/* via a model class */
echo "<p>Via a model class:</p>";

$DOM->getXPath($xpath);
$items = $xpath->query("//tei:*[@ident='$element']/tei:classes/tei:memberOf");
foreach($items as $item) {
	$class_name = $item->getAttribute("key");
	if(ereg("model\.", $class_name)) {
		$DOM->getXPath($xpath1);
		$items = $xpath1->query("//rng:ref[@name='$class_name']");
		$PARENTS = array();
		foreach($items as $item) {
			$PARENTS[] = getElementName($item->parentNode);
		}
		$PARENTS = array_unique($PARENTS);
		foreach($PARENTS as $parent) {
			echo "<li><a href=\"parents.php?element=$parent\">$parent</a></li>";
		}
	}
}

?>
<p><a href="javascript:window.close()">Close this window</a></p>
</body>
</html>