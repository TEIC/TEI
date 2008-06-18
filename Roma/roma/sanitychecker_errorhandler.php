<?php

/**
 *Bernevig Ioan
 *i.bernevig@gmail.com 
 *Juin 2007, 2008
 **/
 
   
 
class SanityCheckerErrorHandler {

	private $sc;

	/**
	 *Le constructeur
	 **/
	public function __construct(&$sc) {
		$this->sc = $sc;
		$this->loadProgressBar();
	}

	/**
	 *Loads the progress bar
	 **/
	public function loadProgressBar() {
		echo '<script type="text/javascript">';
		echo "showPgb();";
		echo '</script>';
		flush();
	}

	/**
	 *Updates the progress bar
	 **/
	public function updateProgressBar($nPercentage) {
		echo '<script type="text/javascript">';
		echo "setPgb('pgbMain', '{$nPercentage}');";
		echo '</script>';
		flush();
		$this->PGB_CURRENT = $nPercentage;
	}

	/**
	 *Updates the status
	 **/
	public function updateStatus($status) {
		echo '<script type="text/javascript">';
		echo "
		var el = document.getElementById('status_div');
		el.innerHTML = '$status';
		";
		echo '</script>';
		flush();
	}

	/**
	 *updates the progressbar to the current status
	 **/
	public function increaseProgressBar() {
		if($this->sc->CURRENT_PASS == 3) $criteria = 'reached'; else $criteria = 'result';
		$min = 10;
		$max = 100;
		$verified = 0;
		$current = $this->PGB_CURRENT;
		if(isset($this->sc->ALL_ELEMENTS)) $nb_el = count($this->sc->ALL_ELEMENTS); else $nb_el = 999999;
		foreach($this->sc->ALL_ELEMENTS as $cle => $el) {
			if(isset($el[$criteria])) $verified++;
		}
		$state = round(($verified/($nb_el))*($max-$min)) + $min;
		$this->updateProgressBar($state);
	}

	/**
	 *Ajoute un message d'erreur
	 **/
	public function sanityCheckAddError($el_name, $prepend, $bold = false, $append = false) {
		echo '<script type="text/javascript">';
		echo "addError('".$el_name."', '".$prepend."', '".$bold."', '".$append."');";
		echo '</script>';
		flush();
	}

	/**
	*Ajoute un avertissement
	**/
	public function sanityCheckAddWarning($el_name, $prepend, $bold = false, $append = false) {
		echo '<script type="text/javascript">';
		echo "addWarning('".$el_name."', '".$prepend."', '".$bold."', '".$append."');";
		echo '</script>';
		flush();
	}

	/**
	*Affiche les erreurs 
	**/
	public function showErrors() {
	
		// Elements which cannot be used
		foreach($this->sc->ALL_ELEMENTS as $cle => $el) {
			if(isset($el['result']) && $el['result'] == false) $this->sanityCheckAddError($cle, " cannot be used");
		}
	
		// Elements never reached
		foreach($this->sc->ALL_ELEMENTS as $cle => $el) {
			if(!isset($el['reached']) && $el['result']) $this->sanityCheckAddWarning($cle, " is never reached from roots elements");
		}
		
		// Elements you can use
		echo '<script language="javascript">
		var tmp = document.getElementById(\'elements_you_can_use\');
		tmp.innerHTML = \'<span style="color: black">Elements you can use:</span> <br /> \'; 
		</script>';
		$ok_elements = array();
		foreach($this->sc->ALL_ELEMENTS as $cle => $el) {
			if($el['result'] == true) {
				$ok_elements[] = $cle;
			}
		}
		natcasesort($ok_elements);
		$html_buffer = '';
		foreach($ok_elements as $cle) {
			$html_buffer .= '<a class="href_element_you_can_use" href="http://www.tei-c.org/release/doc/tei-p5-doc/en/html/ref-'.$cle.'.html" target="_blank">'.$cle.'</a> ';
		}		
		echo '<script language="javascript">
		var tmp = document.getElementById(\'elements_you_can_use\');
		tmp.innerHTML = tmp.innerHTML + \' '.$html_buffer.' \'; 
		</script>';


		// Classes utilisables
		$ok_classes = array();
		foreach($this->sc->ELL_CLASSES as $cle => $el) {
			if(isset($el['result']) && !isset($el['attribute'])) {
				$tmp = explode('_', $cle);
				$ok_classes[$tmp[1]][] = $tmp[0];
			}
		}
		$buffer = '';
		foreach($ok_classes as $mode => $table) {
			$buffer .= '<p><u>In <b>'.$mode.'</b> mode:</u> <br />';
			natcasesort($table);
			foreach($table as $item) $buffer .= '<a class="href_element_you_can_use" href="http://www.tei-c.org/release/doc/tei-p5-doc/en/html/ref-'.$item.'.html" target="_blank">'.$item.'</a> ';
			$buffer .= '</p>';
		}
		echo '<script language="javascript">
		var tmp = document.getElementById(\'classes_you_can_use\');
		tmp.innerHTML = \'<span style="color: black">Model classes you can use:</span> <br /> '.$buffer.' \'; 
		</script>';
		
		
		
	}

	/**
	*Ajoute le message qui dit que le schéma est invalide
	**/
	public function sanityCheckSchemaBroken() {
		echo '<script type="text/javascript">';
		echo "schemaBroken('Schema is BROKEN !');";
		echo '</script>';
		flush();
	}

	/**
	*Ajoute le message qui dit que le schéma est valide
	**/
	public function sanityCheckSchemaOk() {
		echo '<script type="text/javascript">';
		echo "schemaOk('Schema is CORRECT !');";
		echo '</script>';
		flush();
	}

}

?>
