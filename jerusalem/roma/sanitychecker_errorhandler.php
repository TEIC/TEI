<?php

/**
 *Bernevig Ioan
 *i.bernevig@gmail.com 
 *Juin 2007
 **/
 
/**
 *Error sample:
 *[error: 0][element] = div
 *[error: 0][used_in] = p
 *[error: 0][problem] = does not exist
 *[error: 0][type] = warning
 **/      
 
class SanityCheckerErrorHandler {

	public $ERRORS;
	private $sc;

	/**
	 *Le constructeur
	 **/
	public function __construct(&$sc) {
		$this->ERRORS = array();
		$this->sc = $sc;
		$this->loadProgressBar();
	}

	/**
	 *Ajouter une erreur à la liste
	 **/
	public function addError($type, $element, $used_in, $problem) {
		$item = array();
		$item['type'] = $type;
		$item['element'] = $element;
		$item['problem'] = $problem;
		$item['used_in'] = $used_in;
		$this->ERRORS[] = $item;
	}

	/**
	 *Fonction qui trie les erreurs
	 **/
	private function getSortedErrors() {
		return $this->ERRORS;
	}

	/**
	 *Fonction qui affiche toutes les erreurs
	 **/
	public function showErrors_1() {
		$distinct_items = array();
		foreach($this->ERRORS as $error) $distinct_items[] = $error['element'];
		$distinct_items = array_unique($distinct_items);
		foreach($distinct_items as $element) {
			$used_in = array();
			$type = array();
			$problem = array();
			foreach($this->ERRORS as $error) {
				if($error['element'] == $element) {
					$used_in[] = $error['used_in'];
					$type[] = $error['type'];
					$problem[] = $error['problem'];	
				}
			}
			$type = $type[0];
			$used_in = array_unique($used_in);
			$problem = array_unique($problem);
			$bold = ' ';
			for($i=0; $i<count($used_in);$i++) {
				if($i != count($used_in) - 1) {
					$bold .= $used_in[$i] . ', ';
				} else {
					$bold .= $used_in[$i];
				}
			}
			$prepend = ' ';
			foreach($problem as $b) {
				$prepend .= $b . ' ';
			}
			if(!(count($used_in) == 1 && $used_in[0] == '')) $prepend .= ' and used in ';
			if($type == 'Error') {
				$this->sanityCheckAddError($element, $prepend, $bold, $append);
			} else {
				$this->sanityCheckAddWarning($element, $prepend, $bold, $append);
			}
		}
	}

	/**
	 *Fonction qui affiche toutes les erreurs
	 **/
	public function showErrors_2() {
		$used_items = array();
		foreach($this->ERRORS as $error) if($error['used_in'] != '') $used_items[] = $error['used_in'];
		$used_items = array_unique($used_items);
		$not_shown = array();
		foreach($used_items as $used_element) {
			$this->addElementContainer($used_element);
			foreach($this->ERRORS as $error) {
				if($error['used_in'] == $used_element) {
					if($error['type'] == 'Error') {
						$this->addElementContainerError($used_element, $error['element'], $error['problem']);
					} else {
						$this->addElementContainerWarning($used_element, $error['element'], $error['problem']);
					}
				} else {
					$not_shown[] = $error;
				}
			}
		}
		foreach($this->ERRORS as $error) {
			if($error['used_in'] == '') {
				if($error['type'] == 'Error') {
					$this->sanityCheckAddError($error['element'], $error['problem'], '', '');
				} else {
					$this->sanityCheckAddWarning($error['element'], $error['problem'], '', '');
				}
			}
		}
	}

	/**
	 *just for debugging
	 **/
	public function debug() {
		$sorted = $this->getSortedErrors();
		echo '<pre>';
		print_r($sorted);
		echo '</pre>';
	}

	/**
	 *Charge la barre de progression
	 **/
	public function loadProgressBar() {
		echo '<script type="text/javascript">';
		echo "showPgb();";
		echo '</script>';
		flush();
	}

	/**
	 *Met à jour le curseur de la barre de progression
	 **/
	public function updateProgressBar($nPercentage) {
		echo '<script type="text/javascript">';
		echo "setPgb('pgbMain', '{$nPercentage}');";
		echo '</script>';
		flush();
		$this->PGB_CURRENT = $nPercentage;
	}

	/**
	 *Incrémente de 1 la barre de progresion
	 **/
	public function increaseProgressBar() {
		$min = 10;
		$max = 100;
		$current = $this->PGB_CURRENT;
		if(isset($this->sc->ALL_ELEMENTS)) $nb_el = $this->sc->ALL_ELEMENTS->length + 1;
		if(isset($this->sc->ALL_CLASSES)) $nb_class = $this->sc->ALL_CLASSES->length + 1;
		$verified = count($this->sc->COMPUTING);
		$state = round(($verified/($nb_el+$nb_class))*($max-$min)) + $min;
		$this->updateProgressBar($state);
	}

	/**
	 *Ajoute le conteneur d'un élément "In element <bla> etc ..."
	 **/
	private function addElementContainer($el_name) {
		echo '<script type="text/javascript">';
		echo "addElementContainer('".$el_name."');";
		echo '</script>';
		flush();
	}

	/**
	 *Ajoute l'erreur dans le conteneur aproprié
	 **/
	private function addElementContainerError($used_element, $element, $problem) {
		echo '<script type="text/javascript">';
		echo "addElementContainerError('".$used_element."', '".$element."', '".$problem."');";
		echo '</script>';
		flush();
	}

	/**
	 *Ajoute l'erreur dans le conteneur aproprié
	 **/
	private function addElementContainerWarning($used_element, $element, $problem) {
		echo '<script type="text/javascript">';
		echo "addElementContainerWarning('".$used_element."', '".$element."', '".$problem."');";
		echo '</script>';
		flush();
	}

	/**
	 *Ajoute un message d'erreur
	 **/
	private function sanityCheckAddError($el_name, $prepend, $bold, $append) {
		echo '<script type="text/javascript">';
		echo "addError('".$el_name."', '".$prepend."', '".$bold."', '".$append."');";
		echo '</script>';
		flush();
	}

	/**
	*Ajoute un avertissement
	**/
	private function sanityCheckAddWarning($el_name, $prepend, $bold, $append) {
		echo '<script type="text/javascript">';
		echo "addWarning('".$el_name."', '".$prepend."', '".$bold."', '".$append."');";
		echo '</script>';
		flush();
	}

	/**
	*Ajoute le message qui dit que le schéma est invalide
	**/
	public function sanityCheckSchemaBroken() {
		echo '<script type="text/javascript">';
		echo "schemaBroken('Schema is broken !');";
		echo '</script>';
		flush();
	}

	/**
	*Ajoute le message qui dit que le schéma est valide
	**/
	public function sanityCheckSchemaOk() {
		echo '<script type="text/javascript">';
		echo "schemaOk('Schema is correct !');";
		echo '</script>';
		flush();
	}

}

?>
