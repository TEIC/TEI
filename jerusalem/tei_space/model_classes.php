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

function isClass(&$class, $DOM) {
	$class = remove_sequences_from_classnames($class);
	$DOM->getXPath($xpath);
	$tmp = $xpath->query("//tei:classSpec[@ident='$class' and @type='model']" );
	if($tmp->length == 0) {
		return false;
	} else {
		return true;
	}
}

function remove_sequences_from_classnames($class) {
	if(ereg('_sequence', $class)) {
		$tab = explode('_sequence', $class);
		return $tab[0];
	} else {
		return $class;
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

if(!(isElement($element, $DOM) || isClass($element, $DOM))) {
	echo "$element is not in tei_all schema !!! \n";
	exit(0);
}

/* liste des pères possibles pour un élément */
echo "<html><head><title>Model classes of $element</title></head><body>";
echo "<h2>$element is member of </h2>";

/* elements & classes */
$DOM->getXPath($xpath);
$items = $xpath->query("//tei:*[@ident='$element']/tei:classes/tei:memberOf");
foreach($items as $item) {
	$class_name = $item->getAttribute("key");
	if(ereg("model\.", $class_name)) echo "<li><a href=\"model_classes.php?element=$class_name\">$class_name</a></li>";
}

?>
<p><a href="javascript:window.close()">Close this window</a></p>
</body>
</html>