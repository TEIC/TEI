<?php

/**
 *Bernevig Ioan
 *i.bernevig@gmail.com 
 *Juin 2007
 **/  

/**
 *Les constantes
 *JERUSALEM_HTDOCS : le dossier où se trouvent les fichiers constituant le site
 *ROMA_SYSTEM : l'emplacement de l'outil binaire roma
 *Ces deux chemins doivent être de préférence absolus
 *Pour une utilisation sans la base de données, eXist, decommenter la ligne suivante en modifiant biensur les chemins
 * define("ROMA_SYSTEM", "/usr/bin/roma --xsl=/home/tei/sourceforge/trunk/Stylesheets --localsource=/home/tei/sourceforge/trunk/P5/Source/Guidelines/en/guidelines-en.xml")
 *Par défaut, l'outil Roma en ligne de commande utilise la base de données eXist tei.oucs.ox.ac.uk ! 
 **/
define("JERUSALEM_HTDOCS", "/home/tei/jerusalem/");
define("ROMA_SYSTEM", "/usr/bin/roma");

/**
 La classe SanityChecker vérifie la cohérence d'un schéma TEI.
 Elle se construit à partir d'un arbre DOM qui correspond au fichier ODD de personnalisation
 **/
class SanityChecker {
/**
 *COMPUTING    : tableau contenant les noms des éléments en cours de vérification
 *RESULTS      : tableau contenant les noms des éléments dont on connaît le résultat
 *DOM          : objet DOM qui représente le fichier flat ODD en mémoire
 *FILE_TMP_NAME: le nom temporaire du fichier ODD sur le disque dur
 *ALL_ELEMENTS : liste de tous les elements dans le schema (même ceux non joignables)
 *ALL_CLASSES  : liste de toutes les classes
 *PARENTS      : tableau associant à chaque nom d'élément le "dernier parent connu"
 *SCEH         : sanity checker error handler 
 **/         
public $COMPUTING = array();
public $RESULTS = array();
private $DOM;
private $FILE_TMP_NAME;
public $ALL_ELEMENTS;
public $ALL_CLASSES;
private $PARENTS;
private $SCEH;

/**
 La fonction constructeur
 Elle prend en paramètre l'arbre DOM du fichier de personnalisation ODD et le transforme en un arbre FLAT ODD
 **/
public function __construct($dom_customization) {
	$this->SCEH = new SanityCheckerErrorHandler($this);
	$this->FILE_TMP_NAME = md5(time());
	$fp = fopen(JERUSALEM_HTDOCS."tmp/".$this->FILE_TMP_NAME.".odd", "w");
	fwrite($fp, $dom_customization->saveXML());
	fclose($fp);
	$this->SCEH->updateProgressBar(3);
	exec(ROMA_SYSTEM." --compile ".JERUSALEM_HTDOCS."tmp/".$this->FILE_TMP_NAME.".odd /");
	$xml_input = implode("", file(JERUSALEM_HTDOCS."tmp/".$this->FILE_TMP_NAME.".odd.compiled"));
	$this->DOM = new romaDom($xml_input);
	$this->SCEH->updateProgressBar(10);
	$this->DOM->getXPath($xpath);
	$this->ALL_ELEMENTS = $xpath->query("//tei:elementSpec");
	$this->ALL_CLASSES = $xpath->query("//tei:classSpec");
	$this->PARENTS = array();
}

/**
 Fonction qui supprime les fichiers temporaires
 **/
private function deleteTemporary() {
	exec("rm ".JERUSALEM_HTDOCS."tmp/*.odd");
	exec("rm ".JERUSALEM_HTDOCS."tmp/*.compiled");
}

/**
 Fonction qui enlève les spécifications des séquences dans les noms des classes
 On se sert pour retrouver le nom de la classe dans une référence qui porte des précisions sur le type de séquence.
 **/
private function remove_sequences_from_classnames($class) {
	if(ereg('_sequence', $class)) {
		$tab = explode('_sequence', $class);
		return $tab[0];
	} else {
		return $class;
	}
}

/**
 La fonction getContent renvoie
 un tableau d'éléments si l'élément courant est un groupe, *, +, ?
 l'élément contenu si l'élément courant est un élément (au sens TEI)
 un tableau d'éléments si l'élément courant est une classe
 **/
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

/**
 La fonction isElement(element) renvoie si la la chaine passée en paramètres
 correspond ou pas à un élément dans l'arbre DOM
 IE: il existe au moins un élément elementSpec dont l'attribut @ident = element
 **/
private function isElement(&$element) {
	$this->DOM->getXPath($xpath);
	$tmp = $xpath->query("//tei:elementSpec[@ident='$element']" );
	if($tmp->length == 0) {
		return false;
	} else {
		return true;
	}
}

/**
 La fonction isClass(class) renvoie si la la chaine passée en paramètres
 correspond ou pas à une classe dans l'arbre DOM
 IE: il existe au moins un élément classSpec dont l'attribut @ident = class
 **/
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

/**
 La fonction computingStart(name) enregistre dans un tableau le fait que
 l'élément name est "en cours de calcul"
 IE: on se trouve à l'intérieur de la fonction verifElem(name)
 **/
private function computingStart($name) {
	if($name != "") $this->COMPUTING[$name] = true;
}

/**
 La fonction computingProgress(name) renvoie vrai si la fonction verifElem(name) 
 est en cours d'exécution sur l'élément dont le nom est name
 **/
private function computingProgress($name) {
	if(trim($name) != "" && isset($this->COMPUTING[$name]) && $this->COMPUTING[$name]) {
		return true;
	} else {
		return false;
	}
}

/**
 La fonction computingStop(name) enregistre dans un tableau le fait que
 l'élément name n'est plus "en cours de calcul"
 IE: la fonction verifElem(name) s'est terminée
 **/
private function computingStop($name) {
	$this->COMPUTING[$name] = false;
}

/**
 La fonction getParentItem(element) est une fonction récursive qui renvoie le
 premier élément "élément TEI" ou classe qui est père de l'élément en cours.
 Par exemple:
 <elementSpec ident="bla">
   <content>
     <group>
       <optional>
         <zeroOrMore>
           <ref name="truc"/>
 Supposons que l'élément courant est l'élément ref. getParentItem(element) ne
 renverra pas l'élément zeroOrMore qui est son parent direct mais l'élément
 <elementSpec ident="bla">.

 Cette fonction sert principalement dans l'affichage des messages d'erreur. Elle
 n'a aucun impact sur la vérification de la cohérence même.
 **/
private function getParentItem($element) {
	if($element->nodeName == "elementSpec" || $element->nodeName == "classSpec") {
		return $element;
	} else {
		return $this->getParentItem($element->parentNode);
	}
}

/**
 La fonction getElementName renvoie le nom de l'élément courant.
 Plus précisément:
 - la valeur de l'attribut @ident si l'élément courant est <elementSpec> ou <classSpec>
 - la valeur de l'attribut @name si l'élément courant est <ref> ou <rng:ref>
 - si on n'est ni dans un <elementSpec> ni dans un <classSpec> ni dans un <ref> ou
 un <rng:ref> la fonction s'appelle elle-même en passant en paramètre le père
 de l'élément courant.
 Cette fonction est récursive.
 **/
private function getElementName($element) {
	if($element->nodeName == "elementSpec" || $element->nodeName == "classSpec") {
		return $element->getAttribute("ident");
	} else if($element->nodeName == "ref" || $element->nodeName == "rng:ref") {
		return $element->getAttribute("name");
	} else {
		return $this->getElementName($element->parentNode);
	}
}

/**
 La fonction inOptionnality(element) renvoie si quelque part au dessus dans 
 l'hiérarchie il existe un élément qui ne soit pas obligatoire.
 IE: Il existe au moins un élément "zeroOrMore" OU "optionnal" OU "choice" 
 (quoi que pour choice, c'est un cas spécial) parmi les pères (et les pères
 des pères, et ainsi de suite) de l'élément element.
 **/
private function inOptionnality($recursion) {
	foreach($recursion as $recursion_item) {
		if($recursion_item->nodeName == "zeroOrMore" || $recursion_item->nodeName == "optional" || $recursion_item->nodeName == "choice") {
			return true;
		}
	}
	return false;
}

/**
 Ajoute l'élément courant à la liste de la récursion
 **/
private function addRecursion($recursion, $element) {
	$recursion[] = $element;
	return $recursion;
}

/**
 La fonction verifElem(element, parent) est la principale fonction de l'algorithme.
 Elle renvoie
 vrai : si l'élément passé en paramètre est satisfiable (ie: son contenu est satifiable)
 faux : si l'élément passé en paramètre n'est pas satifisable (ie: son contenu n'est pas satisfiable)

 Cette fonction est récursive. La premier appel à cette fonction se fait sur l'élément racine.
 Souvent, cet élément racine est l'élément <TEI> mais on peut avoir également d'autres racines possibles.
 Un schéma est "cassé" lorsque toutes les racines que ce schéma peut avoir sont cassées, IE
 la fonction verifElem renvoie faux pour chaque racine.

 Le principe de cette fonction est assez simple:
 Si l'élément a déjà été vérifié, on renvoie le résultat de la vérification précédente.
 Si l'élément est en cours de vérification, on le considère comme non satifiable jusqu'à preuve du contraire.

 Si l'élément courant est un élément: on vérifie tous les éléments qui sont dans son contenu. Si un seul de ces éléments est non satisfiable, l'élément courant est non satisfiable. Si l'élément n'a pas de contenu, c'est également une erreur.
 Si l'élément courant une une classe: on vérifie chaque élément membre de cette classe. Si à la fin de la vérification il ne reste plus aucun membre satisfiable dans cette classe, cette classe est considérée comme vide. Il est tenu compte des sequences, séquences répétables etc qui peuvent s'appliquer aux classes (cas particuliers).
 Si l'élément courant est une séquence d'éléments: on vérifie tous les éléments fils. Si un seul de ces éléments est non satisfiable, la séquence est considéré comme non satisfiable car la séquence a été interrompue.
 Si l'élément courant est un "zeroOrMore" ou "optionnal": On vérifie tous les éléments fils mais on ne tient pas compte du résultat. On renvoie vrai.
 Si l'élémen courant est un "oneOrMore" ou "choice": On vérifie tous les éléments fils. On renvoie vrai s'il y en a au moins un qui est satifisable, faux sinon.
 Si l'élément courant est un élément terminal comme "text", "rng:text", "rng:empty", "s:patter", "sch:pattern" on renvoie vrai.
 Si l'élément courant est une référence vers un élément ou une classe ("ref" ou "rng:ref"): on vérifie l'élément ou la classe correspondante. On renvoie vrai si l'élément sur lequel le pointeur pointe est satifiable, faux sinon.

 A chaque erreur trouvée, on doit renvoyer un message d'erreur. Le soucis est de savoir si l'erreur trouvée est une erreur ou un warning. C'est pour celà qu'on vérifie s'il y a une optionnalité parmi les éléments parents. S'il y a une optionnalité, cette optionnalité arretera la propagation de l'erreur, et on envoie un warning à l'utilisateur. S'il n'y a pas d'optionnalité parmi les éléments pères, l'erreur se propagera jusqu'à la racine et rendera le schéma incohérent de façon à n'avoir aucun document XML qui puisse le valide. Dans ce cas, on doit envoie le message sous la forme d'une erreur.
 **/
private function verifElem(&$element, &$parent, $recursion) {
$this->getParentItem($element);
	if(!is_object($element)) return null;
	$ident = $element->getAttribute("ident");
	$name = $element->nodeName;
	if($ident != "") $this->PARENTS[$element->getAttribute("ident")][] = $parent;
	if($ident != "" && isset($this->RESULTS[$ident])) {
		return $this->RESULTS[$ident];
	}
	if(!$this->computingProgress($ident)) {
		$this->SCEH->increaseProgressBar();
		//echo "verifElement: name=$name"; if($element->getAttribute("ident") != "") echo ", @ident=$ident"; if($element->getAttribute("name") != "") echo ", @name=".$element->getAttribute("name"); echo "\n";
		switch($name) {
		case "elementSpec": {
			$content = $this->getContent($element);
			$broken = false;
			$this->computingStart($ident);
			foreach($content->childNodes as $content_item) {
				if(!$this->verifElem($content_item, &$element, $this->addRecursion($recursion, $element))) {
					$broken = true;
					$faulty = $content_item;
				}
			}
			$this->computingStop($ident);
			if($broken) {
				if(!$this->computingProgress($ident) && $ident != $this->getElementName($faulty)) $this->SCEH->addError('Error', $ident, $this->getElementName($parent), 'has NO VALID CONTENT because '.$this->getElementName($faulty).' neither');
				//$this->RESULTS[$ident] = false;
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
			$this->computingStart($ident);
			foreach($content as $content_item) {
				if(!$this->verifElem($content_item, &$parent, $this->addRecursion($recursion, $element))) {
					$count--;
					$sequence_broken = true;
				}
			}
			$this->computingStop($ident);
			if($count == 0) {
				if(!$this->computingProgress($this->getElementName($element))) $this->SCEH->addError('Warning', $ident, $this->getParentItem($parent)->getAttribute("ident"), 'is empty');
				//$this->RESULTS[$ident] = false;
				return false;
			}
			if(($sequence == "sequence" || $sequence == "sequenceRepeatable") && $sequence_broken) {
				if(!$this->computingProgress($this->getElementName($element))) $this->SCEH->addError('Error', $ident, '', 'sequence broken');
				//$this->RESULTS[$ident] = false;
				return false;
			}
			break;
		}
		case "group": {
			$broken = false;
			foreach($element->childNodes as $content_item) {
				if(!$this->verifElem($content_item, &$element, $this->addRecursion($recursion, $element))) {
					$broken = true;
					$faulty = $content_item;
				}
			}
			if($broken) {
				if($this->inOptionnality($this->addRecursion($recursion, $element))) {
					if(!$this->computingProgress($this->getElementName($element))) $this->SCEH->addError('Warning', $this->getElementName($this->getParentItem($parent)), '', 'group broken' );
				} else {
					if(!$this->computingProgress($this->getElementName($element))) $this->SCEH->addError('Error', $this->getElementName($this->getParentItem($parent)), '', 'group broken' );
				}
				return false;
			}
			break;
		}
		case "oneOrMore": {
			$count = 0;
			foreach($element->childNodes as $content_item) {
				if($this->verifElem($content_item, &$element, $this->addRecursion($recursion, $element))) $count++;
				else $faulty = $content_item;
			}
			if($count == 0) {
				if(!$this->computingProgress($this->getElementName($element))) $this->SCEH->addError('Error', $this->getElementName($faulty), $this->getElementName($this->getParentItem($parent)), 'is required at least once' );
				return false;
			}
			break;
		}
		case "zeroOrMore": {
			foreach($element->childNodes as $content_item) {
				$this->verifElem($content_item, &$element, $this->addRecursion($recursion, $element));
			}
			break;
		}
		case "choice": {
			$good_ones = $element->childNodes->length;
			foreach($element->childNodes as $content_item) {
				if(!$this->verifElem($content_item, &$element, $this->addRecursion($recursion, $element))) $good_ones--;
			}
			if($good_ones == 0) {
				return false;
			}
			break;
		}
		case "optional": {
			foreach($element->childNodes as $content_item) {
				$this->verifElem($content_item, &$element, $this->addRecursion($recursion, $element));
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
				if($this->inOptionnality($this->addRecursion($recursion, $element))) {
				  $this->SCEH->addError('Warning', $element->getAttribute("name"), $this->getParentItem($element)->getAttribute("ident"), 'does not exist');
				} else {
					$this->SCEH->addError('Error', $element->getAttribute("name"), $this->getParentItem($element)->getAttribute("ident"), 'does not exist');
				}
				return false;
			}
			if($el->nodeName != "") {
				return $this->verifElem($el, &$element, $this->addRecursion($recursion, $element));
			}
			break;
		}
		default: {
			$this->sanityCheckAddError("I can't process $name");
			return false;
		}
		}
		if($ident != "") $this->RESULTS[$ident] = true;
		return true;
	} else {
		return false;
	}
}

/**
 Est-ce que le schéma est cohérent ?
 pass1() vérifie si toutes les racines du schéma sont satisfiables
**/
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
		if(!$this->verifElem($tmp, $root_node, array())) $schema_broken = true;
	}
	if($schema_broken) {
		$this->SCEH->sanityCheckSchemaBroken();
		return true;
	} else {
		$this->SCEH->sanityCheckSchemaOk();
		return false;
	}
}

/**
 Est-ce que tous les éléments sont joignables à partir des racines ?
 **/
public function pass2() {
	$res = true;
	$this->DOM->getXPath($xpath);
	foreach($this->ALL_ELEMENTS as $element) {
		if(!isset($this->COMPUTING[$element->getAttribute("ident")])) {
			$res = false;
			$this->SCEH->addError('Warning', $element->getAttribute("ident"), '', 'is not reacheable from root');
		}
	}
	$this->SCEH->updateProgressBar(95);
	return $res;
}

/**
 *Est-ce qu'il y a des cas où les éléments bouclent sur eux-mêmes
 *Un élément boucle sur lui même si
 * - il exist dans COMPUTING
 * - il n'existe pas dans RESULTS
 * - il n'existe pas dans la liste d'erreurs
 **/
public function pass3() {
/*	$res = true;
	$this->DOM->getXPath($xpath);
	foreach($this->COMPUTING as $item => $valeur) {
		if(!isset($this->RESULTS[Erro$item])) {
			$existe = false;
			foreach($this->SCEH->ERRORS as $error) {
				if($error['element'] == $item) $existe = true;
			}
			if(!$existe) $this->SCEH->addError('Error', $item, '', 'is looping');
		}
	}
*/
	$this->SCEH->updateProgressBar(100);
	return $res;
}

public function showErrors() {
	$this->SCEH->showErrors_2();
}

}

?>
