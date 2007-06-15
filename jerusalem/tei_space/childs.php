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

function getAllChilds($element, $DOM, &$RESULTS, $in_optionality) {
	switch($element->nodeName) {
	case "zeroOrMore":
	case "optional":
	case "choice": {
		$childs = $element->childNodes;
		foreach($childs as $child) {
			getAllChilds($child, $DOM, $RESULTS, true);
		}
		break;
	}
	case "oneOrMore":
	case "group":
	case "content": {
		$childs = $element->childNodes;
		foreach($childs as $child) {
			getAllChilds($child, $DOM, $RESULTS, false);
		}
		break;
	}
	case "ref": {
		$nom = $element->getAttribute("name");
		if(ereg("model\.", $nom)) {
			$DOM->getXPath($xpath1);
			$classe = $xpath1->query("//tei:classSpec[@ident='$nom']")->item(0);
			getAllChilds($classe, $DOM, $RESULTS, $in_optionality);
		} else {
			$DOM->getXPath($xpath1);
			$item = $xpath1->query("//tei:elementSpec[@ident='$nom']")->item(0);
			getAllChilds($item, $DOM, $RESULTS, $in_optionality);
		}
		break;
	}
	case "elementSpec": {
		$monelement = array();
		$monelement['name'] = $element->getAttribute("ident");
		$monelement['optional'] = $in_optionality;
		$RESULTS[] = $monelement;
		break;
	}
	case "text": {
		$RESULTS[] = "TEXT";
		break;
	}
	case "rng:empty": {
		$RESULTS[] = "EMPTY";
		break;
	}
	case "s:pattern": {
		break;
	}
	case "classSpec": {
		$DOM->getXPath($xpath);
		$items = $xpath->query("//tei:*[tei:classes/tei:memberOf/@key='".$element->getAttribute("ident")."']");
		foreach($items as $item) {
			getAllChilds($item, $DOM, $RESULTS, $in_optionality);
		}
		break;
	}
	default : {
		echo "I don't know " . $element->nodeName . "\n";
	}
	}
}

/* liste des fils possibles pour un élément */
echo "<html><head><title>Possible children of $element</title></head><body>";
echo "<h2>Possible children of $element</h2>";

// if(ereg($_SERVER['PHP_SELF'], $_SERVER['HTTP_REFERER'])) echo "<p><a href=\"#\" onclick=\"javascript:history.back()\"><<-- go back</a></p>";

$RESULTS = array();
getAllChilds($xpath->query("//tei:elementSpec[@ident='$element']/tei:content")->item(0), $DOM, $RESULTS, false);

foreach($RESULTS as $CHILD) {
	switch($CHILD['name']) {
		case "EMPTY":
		case "TEXT":
			echo "<li>".$CHILD['name']."</li>";
			break;
		default : 
			switch($CHILD['optional']) {
				case "1": {
					echo "<li><a href=\"childs.php?element=".$CHILD['name']."\" title=\"".$CHILD['desc']."\">".$CHILD['name']."</a></li>";
					break;
				}
				default : {
					echo "<li><a style=\"color: red\" href=\"childs.php?element=".$CHILD['name']."\" title=\"".$CHILD['desc']."\">".$CHILD['name']."</a></li>";
				}
			}
	}
	
}

?>
<p>RED: element may be required !</p>
<p><a href="javascript:window.close()">Close this window</a></p>
</body>
</html>