<?php

define("JERUSALEM_HTDOCS", "/home/tei/roma_htdocs/");

class SanityChecker {

private $COMPUTING = array();
private $RESULTS = array();
private $DOM;
private $FILE_TMP_NAME;
private $ALL_ELEMENTS;
private $ALL_CLASSES;
private $PGB_CURRENT;
private $PARENTS;

public function __construct($dom_customization) {
	$this->loadProgressBar();
	$this->FILE_TMP_NAME = md5(time());
	$fp = fopen(JERUSALEM_HTDOCS."tmp/".$this->FILE_TMP_NAME.".odd", "w");
	fwrite($fp, $dom_customization->saveXML());
	fclose($fp);
	$this->updateProgressBar(3);
	exec("/usr/bin/roma --xsl=/home/tei/sourceforge/trunk/Stylesheets --localsource=/home/tei/sourceforge/trunk/P5/Source/Guidelines/en/guidelines-en.xml --compile ".JERUSALEM_HTDOCS."tmp/".$this->FILE_TMP_NAME.".odd /");
	$xml_input = implode("", file(JERUSALEM_HTDOCS."tmp/".$this->FILE_TMP_NAME.".odd.compiled"));
	$this->DOM = new romaDom($xml_input);
	$this->updateProgressBar(10);
	$this->DOM->getXPath($xpath);
	$this->ALL_ELEMENTS = $xpath->query("//tei:elementSpec");
	$this->ALL_CLASSES = $xpath->query("//tei:classSpec");
	$this->PARENTS = array();
}

private function deleteTemporary() {
	exec("rm ".JERUSALEM_HTDOCS."tmp/*.odd");
}

private function remove_sequences_from_classnames($class) {
	if(ereg('_sequence', $class)) {
		$tab = explode('_sequence', $class);
		return $tab[0];
	} else {
		return $class;
	}
}

private function getContent($input) {
	if($input->nodeName == "group" || $input->nodeName == "zeroOrMore" || $input->nodeName == "optional" || $input->nodeName == "oneOrMore") {
		$res = array();
		$childs = $input->childNodes;
		foreach($childs as $child) {
			if($child->nodeName == "ref") {
				$this->DOM->getXPath($xpath);
				$element = $xpath->query("//tei:elementSpec[@ident='".$child->getAttribute("name")."']")->item(0);
				$this->DOM->getXPath($xpath);
				$class = $xpath->query("//tei:classSpec[@ident='".$child->getAttribute("name")."']")->item(0);
				if(is_object($element)) $res[] = $element;
				if(is_object($class)) $res[] = $class;
			} else {
				$res[] = $child;
			}
		}
		return $res;
	} else if ($input->nodeName == "elementSpec") {
		$childs = $input->childNodes;
		foreach($childs as $child) {
			if($child->nodeName == "content") {
				return $child;
			}
		}
	} else if ($input->nodeName == "classSpec") {
		$res = array();
		$this->DOM->getXPath($xpath1);
		$items = $xpath1->query("//tei:*[tei:classes/tei:memberOf/@key='".$input->getAttribute("ident")."']");
		foreach($items as $item) {
			$res[] = $item;
		}
		return $res;
	}
}

private function isElement(&$element) {
	$this->DOM->getXPath($xpath);
	$tmp = $xpath->query("//tei:elementSpec[@ident='$element']" );
	if($tmp->length == 0) {
		return false;
	} else {
		return true;
	}
}

private function isClass(&$class) {
	$class = $this->remove_sequences_from_classnames($class);
	$this->DOM->getXPath($xpath);
	$tmp = $xpath->query("//tei:classSpec[@ident='$class' and @type='model']" );
	if($tmp->length == 0) {
		return false;
	} else {
		return true;
	}
}

private function computingStart($name) {
	if($name != "") $this->COMPUTING[$name] = true;
}

private function computingProgress($name) {
	if(trim($name) != "" && isset($this->COMPUTING[$name]) && $this->COMPUTING[$name]) {
		return true;
	} else {
		return false;
	}
}

private function computingStop($name) {
	$this->COMPUTING[$name] = false;
}

private function getParentItem($element) {
	if($element->nodeName == "elementSpec" || $element->nodeName == "classSpec") {
		return $element;
	} else {
		return $this->getParentItem($element->parentNode);
	}
}

private function getElementName($element) {
	if($element->nodeName == "elementSpec" || $element->nodeName == "classSpec") {
		return $element->getAttribute("ident");
	} else if($element->nodeName == "ref" || $element->nodeName == "rng:ref") {
		return $element->getAttribute("name");
	} else {
		return $this->getElementName($element->parentNode);
	}
}

private function inOptionnality($element) {
	if($element->nodeName == "zeroOrMore" || $element->nodeName == "optionnal" || $element->nodeName == "choice") {
		return true;
	} else {
		if(isset($element->parentNode)) {
			return $this->inOptionnality($element->parentNode);
		} else {
			return false;
		}
	}
}

private function verifElem(&$element, &$parent) {
$this->getParentItem($element);
	if(!is_object($element)) return null;
	$ident = $element->getAttribute("ident");
	$name = $element->nodeName;
	if($ident != "") $this->PARENTS[$element->getAttribute("ident")][] = $parent;
	if($ident != "" && isset($this->RESULTS[$ident])) {
		return $this->RESULTS[$ident];
	}
	if(!$this->computingProgress($ident)) {
		$this->increaseProgressBar();
		$this->computingStart($ident);
		//echo "verifElement: name=$name"; if($element->getAttribute("ident") != "") echo ", @ident=$ident"; if($element->getAttribute("name") != "") echo ", @name=".$element->getAttribute("name"); echo "\n";
		switch($name) {
		case "elementSpec": {
			$content = $this->getContent($element);
			$broken = false;
			foreach($content->childNodes as $content_item) {
				if(!$this->verifElem($content_item, &$element)) {
					$broken = true;
					$faulty = $content_item;
				}
			}
			if($broken) {
				if($content->childNodes->length == 0) $this->sanityCheckAddError($ident, " has NO VALID CONTENT and is used in ", $this->getElementName($parent), "");
				$this->RESULTS[$ident] = false;
				return false;
			}
			break;
		}
		case "classSpec": {
			$sequence = "";
			if(isset($parent) && $parent->nodeName == "ref") {
				$tmp = explode("_", $parent->getAttribute("name"));
				if(isset($tmp[1])) $sequence = $tmp[1];
			}
			$broken = false;
			$content = $this->getContent($element);
			$sequence_broken = false;
			$count = count($content);
			foreach($content as $content_item) {
				if(!$this->verifElem($content_item, &$parent)) {
					$count--;
					$sequence_broken = true;
				}
			}
			if($count == 0) {
				$this->sanityCheckAddWarning($ident, " is EMPTY  and is used in ", "", $this->getParentItem($parent)->getAttribute("ident"));
				$this->RESULTS[$ident] = false;
				return false;
			}
			if(($sequence == "sequence" || $sequence == "sequenceRepeatable") && $sequence_broken) {
				$this->sanityCheckAddWarning("$ident sequence is broken");
				$this->RESULTS[$ident] = false;
				return false;
			}
			break;
		}
		case "group": {
			$broken = false;
			foreach($element->childNodes as $content_item) {
				if(!$this->verifElem($content_item, &$element)) {
					$broken = true;
					$faulty = $content_item;
				}
			}
			if($broken) {
				if($this->inOptionnality($element)) {
					$this->sanityCheckAddWarning($this->getElementName($this->getParentItem($parent)), " group broken ", "", "");
				} else {
					$this->sanityCheckAddError($this->getElementName($this->getParentItem($parent)), " group broken ", "", "");
				}
				$this->RESULTS[$ident] = false;
				return false;
			}
			break;
		}
		case "oneOrMore": {
			$count = 0;
			foreach($element->childNodes as $content_item) {
				if($this->verifElem($content_item, &$element)) $count++;
				else $faulty = $content_item;
			}
			if($count == 0) {
				$this->sanityCheckAddError($this->getElementName($faulty), " is required at least once in ", $this->getElementName($this->getParentItem($parent)), " !");
				return false;
			}
			break;
		}
		case "zeroOrMore": {
			foreach($element->childNodes as $content_item) {
				$this->verifElem($content_item, &$element);
			}
			break;
		}
		case "choice": {
			$good_ones = $element->childNodes->length;
			foreach($element->childNodes as $content_item) {
				if(!$this->verifElem($content_item, &$element)) $good_ones--;
			}
			if($good_ones == 0) {
				$this->RESULTS[$ident] = false;
				return false;
			}
			break;
		}
		case "optional": {
			foreach($element->childNodes as $content_item) {
				$this->verifElem($content_item, &$element);
			}
			break;
		}
		case "text":
		case "rng:text":
		case "rng:empty":
		case "s:pattern":
		case "sch:pattern": {
			break;
		}
		case "rng:ref":
		case "ref": {
			$this->DOM->getXPath($xpath);
			if($this->isClass($element->getAttribute("name"))) {
				$el = $xpath->query("//tei:classSpec[@ident='".$this->remove_sequences_from_classnames($element->getAttribute("name"))."']")->item(0);	
			} else if ($this->isElement($element->getAttribute("name"))) {
				$el = $xpath->query("//tei:elementSpec[@ident='".$element->getAttribute("name")."']")->item(0);
			} else {
				$this->sanityCheckAddError($element->getAttribute("name"), " does NOT EXIST  and is used in ", $this->getParentItem($element)->getAttribute("ident"), ". It could be in ".$this->getParentItem($element)->getAttribute("module")." module.");
				return false;
			}
			if($el->nodeName != "") {
				$this->RESULTS[$ident] = $this->verifElem($el, &$element);
				//if(!$this->RESULTS[$ident]) $this->sanityCheckAddWarning("reference to ".$el->getAttribute("ident")." failed");
				return $this->RESULTS[$ident];
			}
			break;
		}
		default: {
			$this->sanityCheckAddError("I can't process $name");
			return false;
		}
		}
		$this->RESULTS[$ident] = true;
		$this->computingStop($ident);
		return true;
	} else {
		return true;
	}
}

public function loadProgressBar() {
	echo '<script type="text/javascript">';
	echo "showPgb();";
	echo '</script>';
	flush();
}

public function updateProgressBar($nPercentage) {
	echo '<script type="text/javascript">';
	echo "setPgb('pgbMain', '{$nPercentage}');";
	echo '</script>';
	flush();
	$this->PGB_CURRENT = $nPercentage;
}

public function increaseProgressBar() {
	$min = 10;
	$max = 100;
	$current = $this->PGB_CURRENT;
	$nb_el = $this->ALL_ELEMENTS->length;
	$nb_class = $this->ALL_CLASSES->length;
	$verified = count($this->RESULTS);
	$state = round(($verified/($nb_el+$nb_class))*($max-$min)) + $min;
	$this->updateProgressBar($state);
}

private function sanityCheckAddError($el_name, $prepend, $bold, $append) {
	echo '<script type="text/javascript">';
	echo "addError('".$el_name."', '".$prepend."', '".$bold."', '".$append."');";
	echo '</script>';
	flush();
}

private function sanityCheckAddWarning($el_name, $prepend, $bold, $append) {
	echo '<script type="text/javascript">';
	echo "addWarning('".$el_name."', '".$prepend."', '".$bold."', '".$append."');";
	echo '</script>';
	flush();
}

private function sanityCheckSchemaBroken() {
	echo '<script type="text/javascript">';
	echo "schemaBroken('Schema is broken !');";
	echo '</script>';
	flush();
}

/* is schema coherent ? */
public function pass1() {
	$this->DOM->getXPath($xpath);
	$roots = explode(" ", $xpath->query("//tei:schemaSpec")->item(0)->getAttribute("start"));
	if(trim($roots[0]) == "") {
		$roots = array();
		$roots[0] = "TEI";
	}
	foreach($roots as $root) {
		$tmp = $xpath->query("//tei:elementSpec[@ident='".$root."']")->item(0);
		$root_node = $this->DOM->rootNode;
		if(!$this->verifElem($tmp, $root_node)) $schema_broken = true;
	}
	if($schema_broken) {
		$this->sanityCheckSchemaBroken();
		return true;
	} else {
		return false;
	}
}

/* are all element reacheable from the root(s) ? */
public function pass2() {
	$res = true;
	$this->DOM->getXPath($xpath);
	foreach($this->ALL_ELEMENTS as $element) {
		if(!isset($this->RESULTS[$element->getAttribute("ident")])) {
			$res = false;
			$this->sanityCheckAddWarning($element->getAttribute("ident"), " is not reacheable from root", "", "");
		}
	}
	$this->updateProgressBar(100);
	return $res;
}

}

?>
