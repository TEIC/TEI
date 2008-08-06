<?php

/**
 *Bernevig Ioan
 *i.bernevig@gmail.com 
 *Juin 2007, 2008
 **/  

/**
 La classe SanityChecker vérifie la cohérence d'un schéma TEI.
 Elle se construit à partir d'un arbre DOM qui correspond au fichier ODD de personnalisation
 **/
class SanityChecker {
/**
 *ALL_ELEMENTS : array which contains all elements, their DOM representation, thein result if exists, their computing status ...
 *ALL_CLASSES  : exactly as the ALL_ELEMENTS array we have the model classes array, without the result
               : the same array stores attributes classes defined in the schema.
 *ELL_CLASSES  : model classes array which indicated for each model class the "mode" in which the class is processed (alternate, sequence, ...)
               : the same array indicated which attribute is defined in the schema
 *ALL_MACROS   : the same ...
 *CURRENT_PASS : integer representing the current sanity checker pass (1, 2, 3)
 *DOM          : DOM representation of the flat odd
 *PARENTS      : tableau associant à chaque nom d'élément le "dernier parent connu"
 *SCEH         : sanity checker error handler 
 **/         
public $ALL_ELEMENTS;
public $ALL_CLASSES;
public $ALL_MACROS;
public $ELL_CLASSES;
public $CURRENT_PASS = 0;
public $EXAMPLE_DOCUMENT;
private $DOM;
private $PARENTS;
private $SCEH;
private $SCHEMA_BROKEN = true;

/**
 La fonction constructeur
 Elle prend en paramètre l'arbre DOM du fichier de personnalisation ODD et le transforme en un arbre FLAT ODD
 **/
public function __construct($odd) {
	$this->SCEH = new SanityCheckerErrorHandler($this);
	$this->SCEH->updateStatus("Generating flat ODD");
	$this->SCEH->updateProgressBar(3);
  $this->DOM = new romaDom();	
	$odd->getOddDom($this->DOM);
	$this->SCEH->updateProgressBar(4);
  $this->xpath = new domxpath( $this->DOM );
	$this->SCEH->updateProgressBar(5);
	$this->xpath->registerNamespace( 'rng', 'http://relaxng.org/ns/structure/1.0' );
	$this->xpath->registerNamespace( 'tei', 'http://www.tei-c.org/ns/1.0' );
	$this->getAllElements();
	$this->getAllClasses();
	$this->getAllMacros();
	if(DEBUG) error_reporting(E_ALL);
	if(DEBUG) {
		echo "<pre>".htmlentities($this->DOM->saveXML())."</pre>";
	}
}

/*
 This function get all elements using an XPath query and stores them in a table
 */
private function getAllElements() {
	$this->ALL_ELEMENTS = array();
	$objets = $this->xpath->query("//tei:elementSpec");
	foreach($objets as $objet) {
		$monObjet = array("domNode"=>$objet, "parents"=>array());
		$ident = $objet->getAttribute('ident');
		$this->ALL_ELEMENTS[$ident] = $monObjet;
	}
}

/*
 This function get all macros using an XPath query and stores them in a table
 */
private function getAllMacros() {
	$this->ALL_MACROS = array();
	$objets = $this->xpath->query("//tei:macroSpec");
	foreach($objets as $objet) {
		$monObjet = array("domNode"=>$objet, "parents"=>array());
		$ident = $objet->getAttribute('ident');
		$this->ALL_MACROS[$ident] = $monObjet;
	}
}

/*
 Exactly the same as getAllElements() ...
 */
private function getAllClasses() {
	// Le tableau d'utilisation des classes
	$this->ELL_CLASSES = array();
	
	// Le tableau global des classes
	$this->ALL_CLASSES = array();
	$objets = $this->xpath->query("//tei:classSpec");
	foreach($objets as $objet) {
		$ident = $objet->getAttribute('ident');
		$type = $objet->getAttribute('type');
		$monObjet = array("domNode"=>$objet, "type"=>$type);
		if($type == 'atts')	{
			$this->ELL_CLASSES[$ident.'.attributes'] = $monObjet;
			$o_sons = $objet->childNodes;
			foreach($o_sons as $o_son) {
				if($o_son->localName == 'attList') foreach($o_son->childNodes as $o_attdef) if($o_attdef->localName == 'attDef') $this->ELL_CLASSES[$ident.'.attribute.'.$o_attdef->getAttribute('ident')] = array("domNode"=>$o_attdef, "result"=>true, "attribute"=>true);
			}
		}
		$this->ALL_CLASSES[$ident] = $monObjet;
	}
}

/**
 Fonction qui enlève les spécifications des séquences dans les noms des classes
 On se sert pour retrouver le nom de la classe dans une référence qui porte des précisions sur le type de séquence.
 **/
private function remove_sequences_from_classnames($class) {
	if(ereg('_alternation', $class) || ereg('_sequence', $class) || ereg('_sequenceOptional', $class) || ereg('_sequenceOptionalRepeatable', $class) || ereg('_sequenceRepeatable', $class)) {
		$tab = explode('_', $class);
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
	if($input->localName == "interleave" ||
		 $input->localName == "start" ||
		 $input->localName == "mixed" ||
		 $input->localName == "element" || 
		 $input->localName == "group" || 
		 $input->localName == "zeroOrMore" || 
		 $input->localName == "optional" || 
		 $input->localName == "oneOrMore" || 
		 $input->localName == "choice") {
		$res = array();
		$childs = $input->childNodes;
		foreach($childs as $child) {
			if($child->localName == "ref") {
				if($this->isElement($child->getAttribute("name")) || $this->isMacro($child->getAttribute("name"))) $res[] = $this->ALL_MACROS[$child->getAttribute("name")]['domNode'];
				else if ($this->isClass($child->getAttribute("name"))) $res[] = $this->ALL_CLASSES[$this->remove_sequences_from_classnames($child->getAttribute("name"))]['domNode']; 
			} else {
				$res[] = $child;
			}
		}
		return $res;
	} else if ($input->localName == "elementSpec" || $input->localName == "macroSpec") {
		$childs = $input->childNodes;
		foreach($childs as $child) {
			if($child->localName == "content") {
				return $child;
			}
		}
	} else if ($input->localName == "classSpec") {
		$res = array();
		$items = $this->xpath->query("//tei:*[tei:classes/tei:memberOf/@key='".$input->getAttribute("ident")."']");
		foreach($items as $item) {
			$res[] = $item;
		}
		return $res;
	}
	if(DEBUG) echo "getContent(".$input->localName.") echoue<br>";
}

/**
 La fonction isElement(element) renvoie si la la chaine passée en paramètres
 correspond ou pas à un élément dans l'arbre DOM
 IE: il existe au moins un élément elementSpec dont l'attribut @ident = element
 **/
private function isElement($element) {
	if(isset($this->ALL_ELEMENTS[$element])) {
		return true;
	} else {
		return false;
	}
}

private function isMacro($macro) {
	if(DEBUG) echo "isMacro($macro) <br>";
	if(isset($this->ALL_MACROS[$macro])) {
		if(DEBUG) echo "isMacro($macro) TRUE<br>";
		return true;
	} else {
		if(DEBUG) echo "isMacro($macro) FALSE<br>";
		return false;
	}
}

/**
 *Returns true if the string $class matches a classname in the ODD document
 *
 IE: there is at least one <classSpec> with the @ident attribute. The @type attribute can be "model" or "atts".
 **/
private function isClass($class) {
	if(DEBUG) echo "isClass($class) <br>";
	$class = $this->remove_sequences_from_classnames($class);
	if(isset($this->ALL_CLASSES[$class]) || isset($this->ELL_CLASSES[$class])) {
		if(DEBUG) echo "isClass($class) TRUE<br>";
		return true;
	} else {
		if(DEBUG) echo "isClass($class) FALSE<br>";
		return false;
	}
}

/**
 *Returns true if the name $attname matches an attribute name
 **/
private function isAttribute($attname) {
	if(DEBUG) echo "isAttribute($attname) <br>";
	if(isset($this->ELL_CLASSES[$attname]) && $this->ELL_CLASSES[$attname]['attribute']) {
		if(DEBUG) echo "isAttribute($attname) TRUE<br>";
		return true;
	} else {
		if(DEBUG) echo "isAttribute($attname) FALSE<br>";
		return false;
	}
}

/**
 Retourne la première grammaire (en "montant" dans la brache courante de l'arbre)
 **/
private function getFirstGrammar($element) {
	if(!$element->parentNode) return false;
	if($element->localName == 'grammar') return $element;
	else return $this->getFirstGrammar($element->parentNode);
}

/**
 *Returns all <define> elements in a given grammar
 **/
private function getDefinesInGrammar($element) {
	if($element->localName == 'define') {
		return array($element);
	} else {
		$res = array();
		if(!$element->childNodes) return $res;
		foreach($element->childNodes as $noeud) {
			$res = array_merge($res, $this->getDefinesInGrammar($noeud));
		}
		return $res;
	}
}

/**
 *Returns the <define> element that matches the rng:ref element (param)
 **/
private function getDefine($element) {
	if(is_object($element)) {
  	$noeuds = array();
  	$mode = false;
  	$premiere_grammaire = $this->getFirstGrammar($element);
  	if(!is_object($premiere_grammaire)) return false;
  	$items = $this->xpath->query("//node()", $premiere_grammaire);
  	$items = $this->getDefinesInGrammar($premiere_grammaire);
		foreach($items as $item) {
			if($element->getAttribute('name') == $item->getAttribute("name")) $noeuds[] = $item;
		}
		if(count($noeuds) == 1) return $noeuds[0];
		if(count($noeuds) > 1) {
			$nb_with_combine = 0;
			foreach($noeuds as $noeud) {
				if($noeud->getAttribute("combine")) {
					if(!$mode) $mode = $noeud->getAttribute("combine");
					if($noeud->getAttribute("combine") && $noeud->getAttribute("combine") != $mode) return false;
					$nb_with_combine++;
				}
			}
			if($nb_with_combine != count($noeuds) - 1) return false;
			/* Maintenant on peut commencer à les combiner :) */
			$o_define = $this->DOM->createElementNS('http://relaxng.org/ns/structure/1.0', 'define');
			$o_define->setAttribute('name', $element->getAttribute('name'));
			$o_mode = false;
			if($mode == 'choice') {
				$o_mode = $this->DOM->createElementNS('http://relaxng.org/ns/structure/1.0', 'choice');
			} else if($mode == 'interleave') {
				$o_mode = $this->DOM->createElementNS('http://relaxng.org/ns/structure/1.0', 'interleave');
			}
			if($o_mode) {
				foreach($noeuds as $noeud) {
					foreach($noeud->childNodes as $fils) {
						$o_mode->appendChild($fils);
					}
				}
				$o_define->appendChild($o_mode);
			}
			return $o_define;
		}
		return false;
	} else {
		return false;
	}
}

/**
 La fonction computingStart(name) enregistre dans un tableau le fait que
 l'élément name est "en cours de calcul"
 IE: on se trouve à l'intérieur de la fonction pass1_verifElem(name)
 **/
private function computingStart($name) {
	if($name != "") {
		if($this->isElement($name)) {
			$this->ALL_ELEMENTS[$name]['computing'] = 'started';
		} else if($this->isClass($name)) {
			$this->ELL_CLASSES[$name]['computing'] = 'started';
		} else if($this->isMacro($name)) {
			$this->ALL_MACROS[$name]['computing'] = 'started';
		}
	}
}

/**
 La fonction computingProgress(name) renvoie vrai si la fonction pass1_verifElem(name) 
 est en cours d'exécution sur l'élément dont le nom est name
 **/
private function computingProgress($name) {
	if(trim($name) != "" && (
		(isset($this->ALL_ELEMENTS[$name]['computing']) && $this->ALL_ELEMENTS[$name]['computing'] == 'started') ||
		(isset($this->ALL_MACROS[$name]['computing']) && $this->ALL_MACROS[$name]['computing'] == 'started') || 
		(isset($this->ALL_CLASSES[$name]['computing']) && $this->ALL_CLASSES[$name]['computing'] == 'started') ||
		(isset($this->ELL_CLASSES[$name]['computing']) && $this->ELL_CLASSES[$name]['computing'] == 'started')
		)) {
		return true;
	} else {
		return false;
	}
}

/**
 La fonction computingStop(name) enregistre dans un tableau le fait que
 l'élément name n'est plus "en cours de calcul"
 IE: la fonction pass1_verifElem(name) s'est terminée
 **/
private function computingStop($name) {
	if($name != "") {
		if($this->isElement($name)) {
			$this->ALL_ELEMENTS[$name]['computing'] = 'stopped';
		} else if($this->isClass($name)) {
			$this->ELL_CLASSES[$name]['computing'] = 'stopped';
		} else if($this->isMacro($name)) {
			$this->ALL_MACROS[$name]['computing'] = 'stopped';
		}
	}
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
   if($element->localName == "elementSpec" || $element->localName == "classSpec" || $element->localName == "element") {
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
	if(!is_object($element)) return false;
	if($element->localName == "elementSpec" || $element->localName == "classSpec") {
		return $element->getAttribute("ident");
	} else if($element->localName == "ref" || $element->localName == "element") {
		return $element->getAttribute("name");
	} else {
		return $this->getElementName($element->parentNode);
	}
}

/**
 La fonction pass1_verifElem(element, parent) est la principale fonction de
 l'algorithme.  Elle renvoie vrai : si l'élément passé en paramètre
 est satisfiable (ie: son contenu est satifiable) faux : si l'élément
 passé en paramètre n'est pas satifisable (ie: son contenu n'est pas
 satisfiable)

 Cette fonction est récursive. La premier appel à cette fonction se
 fait sur l'élément racine.  Souvent, cet élément racine est l'élément
 <TEI> mais on peut avoir également d'autres racines possibles.  Un
 schéma est "cassé" lorsque toutes les racines que ce schéma peut
 avoir sont cassées, IE la fonction pass1_verifElem renvoie faux pour chaque
 racine.

 Le principe de cette fonction est assez simple:

 Si l'élément a déjà été vérifié, on renvoie le résultat de la
 vérification précédente.
 
 Si l'élément est en cours de vérification, on le considère comme non
 satifiable jusqu'à preuve du contraire.

 Si l'élément courant est un élément: on vérifie tous les éléments qui
 sont dans son contenu. Si un seul de ces éléments est non
 satisfiable, l'élément courant est non satisfiable. Si l'élément n'a
 pas de contenu, c'est également une erreur.

 Si l'élément courant une une classe: on vérifie chaque élément membre
 de cette classe. Si à la fin de la vérification il ne reste plus
 aucun membre satisfiable dans cette classe, cette classe est
 considérée comme vide. Il est tenu compte des sequences, séquences
 répétables etc qui peuvent s'appliquer aux classes (cas
 particuliers).

 Si l'élément courant est une séquence d'éléments: on vérifie tous les
 éléments fils. Si un seul de ces éléments est non satisfiable, la
 séquence est considéré comme non satisfiable car la séquence a été
 interrompue.

 Si l'élément courant est un "zeroOrMore" ou "optionnal": On vérifie
 tous les éléments fils mais on ne tient pas compte du résultat. On
 renvoie vrai.

 Si l'élémen courant est un "oneOrMore" ou "choice": On vérifie tous
 les éléments fils. On renvoie vrai s'il y en a au moins un qui est
 satifisable, faux sinon.

 Si l'élément courant est un élément terminal comme "text",
 "rng:text", "rng:empty", "s:pattern", "sch:pattern" on renvoie vrai.

 Si l'élément courant est une référence vers un élément ou une classe
 ("ref" ou "rng:ref"): on vérifie l'élément ou la classe
 correspondante. On renvoie vrai si l'élément sur lequel le pointeur
 pointe est satifiable, faux sinon.

 A chaque erreur trouvée, on doit renvoyer un message d'erreur. Le
 soucis est de savoir si l'erreur trouvée est une erreur ou un
 warning. C'est pour celà qu'on vérifie s'il y a une optionnalité
 parmi les éléments parents. S'il y a une optionnalité, cette
 optionnalité arretera la propagation de l'erreur, et on envoie un
 warning à l'utilisateur. S'il n'y a pas d'optionnalité parmi les
 éléments pères, l'erreur se propagera jusqu'à la racine et rendera le
 schéma incohérent de façon à n'avoir aucun document XML qui puisse le
 valide. Dans ce cas, on doit envoie le message sous la forme d'une
 erreur.  **/ 

private function pass1_verifElem($element, $parent) { 
	if(!is_object($element)) return null; 
	if(DEBUG) {
		echo "pass1_element: nodeName=".$element->nodeName." localName=".$element->localName."<br>";
	}
	switch($element->localName) {
		case 'macroSpec':
 		case 'elementSpec':
 		case 'classSpec': $ident = $element->getAttribute("ident"); break;
 		case 'ref': $ident = $element->getAttribute("name"); break;
 		default: $ident = '';
	}
	
	if($ident != "") {
		if($this->isElement($ident)) {
			$this->ALL_ELEMENTS[$ident]['parents'][] = $parent;
			if(isset($this->ALL_ELEMENTS[$ident]['result'])) {
				if($this->CURRENT_PASS == 1) {
					$this->EXAMPLE_DOCUMENT .= '<'.$ident.' xmlns="http://www.tei-c.org/ns/1.0"></'.$ident.'>';
				}
				return $this->ALL_ELEMENTS[$ident]['result'];
			}
		} else if($this->isClass($ident)) {
			$this->ALL_CLASSES[$ident]['parents'][] = $parent;
			if(isset($this->ALL_CLASSES[$ident]['result'])) return $this->ALL_CLASSES[$ident]['result'];
		} else if($this->isMacro($ident)) {
			$this->ALL_MACROS[$ident]['parents'][] = $parent;
			if(isset($this->ALL_MACROS[$ident]['result'])) return $this->ALL_MACROS[$ident]['result'];
		}
	}
	
	if(!$this->computingProgress($ident)) {
		if($element->nodeName == '#text') return true;
		switch($element->localName) {
		case "macroSpec":
		case "elementSpec": {
			$content = $this->getContent($element);
			$broken = false;
			if($this->CURRENT_PASS == 1) {
				$this->EXAMPLE_DOCUMENT .= '<'.$ident.' xmlns="http://www.tei-c.org/ns/1.0">';
			}
			$this->computingStart($ident);
			foreach($content->childNodes as $content_item) {
				if(!$this->pass1_verifElem($content_item, $element)) {
					$this->computingStop($ident);
					if(DEBUG) echo "$ident FALSE because ".$content_item->localName."<br>";
					$this->EXAMPLE_DOCUMENT = '';
					return false;
				}
			}
			$this->computingStop($ident);
			if($this->CURRENT_PASS == 1) {
				$this->EXAMPLE_DOCUMENT .= '</'.$ident.'>';
			}
			break;
		}
		case "start":
		case "define":
		case "mixed":
		case "interleave":
		case "oneOrMore":
		case "element":
		case "group": {
			foreach($element->childNodes as $content_item) {
				if(!$this->pass1_verifElem($content_item, $element)) {
					return false;
				}
			}
			break;
		}
		case "choice": {
			$good_ones = $element->childNodes->length;
			foreach($element->childNodes as $content_item) {
				if($this->pass1_verifElem($content_item, $element)) return true;
			}
			return false;
			break;
		}
		case "grammar": {
			$start_mode = false;
			$starts_el = array();
			foreach($element->childNodes as $item) if($item->localName  == 'start') $starts_el[] = $item;
			if(count($starts_el) == 0) return false;
			if(count($starts_el) == 1) {
				$resultat = true;
				foreach($starts_el[0]->childNodes as $node) $resultat = $resultat && $this->pass1_verifElem($node, $element);
				return $resultat;
			} else if(count($starts_el) > 1) {
				foreach($starts_el as $el_start) {
					if(!$start_mode && $el_start->getAttribute('combine')) $start_mode = $el_start->getAttribute('combine');
					if($start_mode && $start_mode != $el_start->getAttribute('combine')) return false;
				}
				if($start_mode == 'choice') {
					$good = false;
					foreach($starts_el as $el_start) $good = $good || $this->pass1_verifElem($el_start, $element);
					return $good;
				} else if($start_mode == 'interleave') {
					$good = true;
					foreach($starts_el as $el_start) $good = $good && $this->pass1_verifElem($el_start, $element);
					return $good;
				}
				return false;
			}
			break;
		}
		case "notAllowed": {
			return false;
			break;
		}
		case "zeroOrMore":
		case "optional":
		case "text":
		case "empty":
		case "ns":
		case "attribute":
		case "value":
		case "list":
		case "valList":
		case "valItem":
		case "data":
		case "pattern": {
			break;
		}
		case "parentRef":
		case "ref": {
			$nom = $element->getAttribute("name");
			if(DEBUG) echo "pass1_ref: $nom<br />";
			if($this->computingProgress($nom)) {
				if(DEBUG) echo "<p color=red>on ne peut pas compter sur ".$nom." dans ".$this->getElementName($element)."</p>";
				return false;
			}
			if($this->isClass($nom)) {
				if(DEBUG) echo "pass1_ref_$nom is a class !! <br>";
				$a = $this->pass1_verifClass($nom, $element);
				return $a;
			} else if ($this->isElement($nom)) {
				return $this->pass1_verifElem($this->ALL_ELEMENTS[$nom]['domNode'], $element);
			} else if ($this->isMacro($nom)) {
				return $this->pass1_verifElem($this->ALL_MACROS[$nom]['domNode'], $element);
			} else if($noeud_define = $this->getDefine($element)) {
				return $this->pass1_verifElem($noeud_define, $element);
			} else if($this->isAttribute($nom)) {
				return true;
			} else {
				$this->SCEH->sanityCheckAddUndefined($nom, ' is used in ' , $this->getElementName($parent), ' and never defined.');
				if(DEBUG) echo "Where is the --$nom-- define ???<br />";
				return false;
			}
			break;
		}
		default: {
			if(DEBUG) echo "pass1: I can't process ". $element->localName."<br>";
			return false;
		}
	}
	} else {
		if(DEBUG) echo "(".$element->localName.") $ident false car en cours de calcul<br>";
		return false;
	}
	if($ident != "") {
		if($this->isElement($ident)) {
			$this->ALL_ELEMENTS[$ident]['result'] = true;
		} else if($this->isClass($ident)) {
			$this->ALL_CLASSES[$ident]['result'] = true;
		}
	}
	return true;
}

private function pass1_verifClass($nomclasse, $parent) {
	//Si la classe est en cours de vérification, on renvoie false (sans enregistrer le résultat
	if($this->computingProgress($nomclasse)) return false;
	
	// Paramètres d'initialisation
	$buffer = explode('_', $nomclasse);
	$nom = $buffer[0];
	
	//Si c'est une classe d'attributs, on renvoie true
	if(isset($this->ELL_CLASSES[$nom])) {
		if(DEBUG) echo "Je ne verifie pas class:$nom <br/>";
		return true;
	} 
	
	// Il s'agit d'une classe modèle
	$classe = $this->ALL_CLASSES[$nom]['domNode'];
	if(isset($buffer[1])) $mode = $buffer[1]; else $mode = false;
	if(!$mode) $mode = 'alternation';
	$this->computingStart($nom.'_'.$mode);

	// On regarde tous les modes possibles avec cette classe
	$modesAutorises = array("alternation", "sequence", "sequenceOptional", "sequenceOptionalRepeatable", "sequenceRepeatable");
	$att_generate = explode(' ', $classe->getAttribute('generate'));
	
	// Si aucune précision, tous les modes sont possibles
	if(count($att_generate) > 0 && $att_generate[0] == false) $att_generate = $modesAutorises;
	
	// On regarde si le mode voulu est possible
	$autorise = false;
	foreach($att_generate as $a) {
		if($mode == $a)	$autorise = true;
	}
	if(!$autorise) {
		//echo '<p style="color: red">Le mode '.$mode.' n\'est pas possible dans '.$nom.'. Choisissez parmi '; print_r($att_generate); echo '</p>';
		flush();
		return false;
	} 
	
	// On regarde pour chaque type de sequence si la classe est valide ou non
	$content = $this->getContent($classe);
	$count = count($content);
	switch($mode) {
		case 'alternation': {
			$bons = 0;
			foreach($content as $content_item) {
				if($this->isClass($content_item->getAttribute("ident"))) $result = $this->pass1_verifClass($content_item->getAttribute("ident"), $classe);
				else if($this->isElement($content_item->getAttribute("ident"))) $result = $this->pass1_verifElem($content_item, $classe);
				if($result) {
					$bons++;
					$this->ELL_CLASSES[$nom.'_'.$mode]['result'] = true;
					$this->computingStop($nom.'_'.$mode);
					return true;
				}
			}
			if($bons == 0) {
				//echo "Classe $nom vide<br>";
				return false;
			}
		}
		case 'sequence': 
		case 'sequenceRepeatable': {
			foreach($content as $content_item) {
				if($this->isClass($content_item->getAttribute("ident"))) $result = $this->pass1_verifClass($content_item->getAttribute("ident"), $classe);
				else if($this->isElement($content_item->getAttribute("ident"))) $result = $this->pass1_verifElem($content_item, $classe);
				if(!$result) {
					if(DEBUG) echo "======== ".$content_item->localName." brise la sequence de  ".$classe->localName."<br>";
					return false;
				}
			}
			break;
		}
		case 'sequenceOptional': 
		case 'sequenceOptionalRepeatable': break;
		default : {
			//echo "$mode <br>";
		}
	}
	$this->ELL_CLASSES[$nom.'_'.$mode]['result'] = true;
	$this->computingStop($nom.'_'.$mode);
	return true;
}

private function pass3_verifClass($nomclasse) {
	$buffer = explode('_', $nomclasse);
	$nom = $buffer[0];
	//Si c'est une classe d'attributs, on renvoie true
	if(isset($this->ELL_CLASSES[$nom])) {
		if(DEBUG) echo "Je ne verifie pas class:$nom <br/>";
		return true;
	} 
	$classe = $this->ALL_CLASSES[$nom]['domNode'];
	if(isset($buffer[1])) $mode = $buffer[1]; else $mode = false;
	if(!$mode) $mode = 'alternation';
	
	// Pour éviter les boucles
	if(isset($this->ALL_CLASSES[$nom]['reached'])) return true;	
	
	// Traitement de la classe
	//echo '<span style="color: red">'.$nom. "</span> ";flush();
	$this->ALL_CLASSES[$nom]['reached'] = true;
	$this->SCEH->increaseProgressBar();
	
	if($this->pass1_verifClass($nomclasse, false)) {
			$content = $this->getContent($this->ALL_CLASSES[$nom]['domNode']);
			foreach($content as $item) {
				if($this->isClass($item->getAttribute("ident"))) $this->pass3_verifClass($item->getAttribute("ident"), false);
				else if($this->isElement($item->getAttribute("ident"))) $this->pass3_verifElem($item, false);
			}
	}
	
}
private function pass3_verifElem($element) {
	if(!is_object($element) || $element->nodeName == '#text') return true;

	// Pour éviter les boucles
	if($element->localName == 'elementSpec' || $element->localName == 'classSpec' || $element->localName == 'macroSpec') $ident = $element->getAttribute('ident');
	else $ident = false;
	if($ident) {
		if(isset($this->ALL_CLASSES[$this->remove_sequences_from_classnames($ident)]['reached']) ||
		isset($this->ALL_ELEMENTS[$ident]['reached'])) return true;
	}
	
	// On enregistre le fait qu'on est passé par là si c'est un élément
	if($element->localName == "elementSpec" && $this->ALL_ELEMENTS[$element->getAttribute('ident')]['result']) {
		if(DEBUG) echo '<span style="color: magenta">'.$element->getAttribute('ident') . "</span> ";flush();
		$this->ALL_ELEMENTS[$element->getAttribute('ident')]['reached'] = true;
		$this->SCEH->increaseProgressBar();
	}
	
	// Si c'est une référence
	if($element->localName == 'ref' || $element->localName == 'parentRef') {
		if($this->isClass($element->getAttribute('name'))) {
			$this->pass3_verifClass($element->getAttribute('name'));
		}	else if($noeud_define = $this->getDefine($element)) {
				$this->pass3_verifElem($noeud_define, $element);
		} else if($this->isMacro($element->getAttribute('name'))) {
				$this->pass3_verifElem($this->ALL_MACROS[$element->getAttribute('name')]['domNode']);
		}	else if($this->isElement($element->getAttribute('name'))) {
			$this->pass3_verifElem($this->ALL_ELEMENTS[$element->getAttribute('name')]['domNode']);
		}
	} else {
		if($element->localName == 'elementSpec' || $element->localName == 'classSpec' || $element->localName == 'macroSpec') $ident = $element->getAttribute('ident');
		else $ident = false;
		$ident = $element->getAttribute('ident');
		if($ident && $this->isClass($ident)) {
			$this->pass1_verifClass($ident, false);
		} else if($this->pass1_verifElem($element, false) ) {
			$content = $this->getContent($element);
			switch($element->localName) {
				case 'macroSpec':
				case 'elementSpec': {
					foreach($content->childNodes as $content_item) $this->pass3_verifElem($content_item);
					break;
				}
				case 'grammar': {
					foreach($element->childNodes as $content_item) {
						if($content_item->localName  == 'start') {
							$fils = $content_item->childNodes;
							$resultat = true;
							foreach($fils as $fils_item) {
								$this->pass3_verifElem($fils_item, $content_item);
							}
							return $resultat;
						}
					}
					break;
				}
				case 'define':
				case 'classSpec':
				case 'oneOrMore':
				case 'choice':
				case 'optional':
				case 'zeroOrMore': 
				case 'element':
				case 'interleave':
				case 'mixed':
				case 'group': {
					$liste = $element->childNodes;
					foreach($liste as $content_item) {
						$this->pass3_verifElem($content_item);
					}
					break;
				}
				case 'text':
				case 'empty':
				case 'ns':
				case 'list':
				case 'attribute':
				case 'value':
				case 'valList':
				case 'valItem':
				case 'notAllowed':
				case 'data':
				case "pattern": {
					break;
				}
				default : echo "je ne sais pas faire ".$element->localName."<br>";flush();
			}
		}
	}
	return true;
}

/**
 Est-ce que le schéma est cohérent ?
 pass1() vérifie si toutes les racines du schéma sont satisfiables
 Seule une petite partie du document est vérifiée
**/
public function pass1() {
	$this->CURRENT_PASS = 1;
	$this->SCEH->updateStatus("PASS1 starts");
	$this->SCEH->updateProgressBar(51.1);
	$roots = explode(" ", $this->xpath->query("//tei:schemaSpec")->item(0)->getAttribute("start"));
	$this->SCEH->updateProgressBar(51.2);
	if(trim($roots[0]) == "") {
		$roots = array();
		$roots[0] = "TEI";
	}
	foreach($roots as $root) {
		$start = $this->ALL_ELEMENTS[$root]['domNode'];
    $root_node = $this->DOM->documentElement;
	 	if($this->pass1_verifElem($start, $root_node)) {
	 		$this->SCEH->updateStatus("PASS1 ends, element $root checked");
	 		$this->SCEH->sanityCheckSchemaOk();
	 		$this->SCHEMA_BROKEN = false;
	 		$_SESSION['model_document'] = $this->EXAMPLE_DOCUMENT;
			return true;
	 	}
	}
	
	$this->SCEH->updateStatus("<b>Schema is BROKEN</b>");
	$this->SCHEMA_BROKEN = true;
	$this->SCEH->sanityCheckSchemaBroken();
	return false;
}

/**
 On vérifie la satisfiabilité de chaque élément
 **/
public function pass2() {
	$this->CURRENT_PASS = 2;
	
	$this->SCEH->updateStatus("PASS2 starts, checking all elements");
	foreach($this->ALL_ELEMENTS as $cle => $element) {
		$this->ALL_ELEMENTS[$cle]['result'] = $this->pass1_verifElem($element['domNode'], $this->DOM->documentElement);
		$this->SCEH->updateStatus("<b>$cle</b> checked");
		$this->SCEH->increaseProgressBar();
	}
	$this->SCEH->updateStatus("PASS2 ends, all elements have been checked");
	
	return true;
}

/**
 *Elément joignable depuis la racine
 **/
public function pass3() {
	
	$this->CURRENT_PASS = 3;
	
	$this->SCEH->updateStatus("PASS3 starts");
	$roots = explode(" ", $this->xpath->query("//tei:schemaSpec")->item(0)->getAttribute("start"));

	if(trim($roots[0]) == "") {
		$roots = array();
		$roots[0] = "TEI";
	}

	foreach($roots as $root) {
	 	$this->pass3_verifElem($this->ALL_ELEMENTS[$root]['domNode']);
	}
	
	$this->SCEH->updateStatus("PASS3 ends, all roots have been tested");
}

/**
 *memberOf on classes which not exist
 *In fact, this is not a good idea. It offten happens that some element is member of a class form a module that the personalisation does not include it.
 **/
/*public function pass4() {
	
	$this->CURRENT_PASS = 4;
	
	$this->SCEH->updateStatus("PASS4 starts");
	$all_memberOf = $this->xpath->query("//tei:memberOf");
	foreach($all_memberOf as $item) {
		$key = $item->getAttribute("key");
		if(!$this->isClass($key)) {
			$this->SCEH->sanityCheckAddUndefined($key, ' is used in ' , $this->getElementName($item), ' and never defined.');
		}
	}
	$this->SCEH->updateStatus("PASS4 ends");
}*/

public function showErrors() {
	$this->SCEH->showErrors();
}

}

?>
