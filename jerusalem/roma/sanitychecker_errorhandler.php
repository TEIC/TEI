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

  private $ERRORS;
  private $sc;
  
  /**
   *Le constructeur
   **/     
  public function __construct($a) {
  	$this->ERRORS = array();
  	$this->sc = $a;
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
    *just for debugging
    **/       
   public function debug() {
    echo '<pre>';
    print_r($this->ERRORS);
    echo '</pre>';
   }
  
  /**
   Charge la barre de progression
   **/
  public function loadProgressBar() {
  	echo '<script type="text/javascript">';
  	echo "showPgb();";
  	echo '</script>';
  	flush();
  }
  
  /**
   Met à jour le curseur de la barre de progression
   **/
  public function updateProgressBar($nPercentage) {
  	echo '<script type="text/javascript">';
  	echo "setPgb('pgbMain', '{$nPercentage}');";
  	echo '</script>';
  	flush();
  	$this->PGB_CURRENT = $nPercentage;
  }
  
  /**
   Incrémente de 1 la barre de progresion
   **/
  public function increaseProgressBar() {
  	$min = 10;
  	$max = 100;
  	$current = $this->PGB_CURRENT;
  	if(is_object($this->sc) && is_object($this->sc->ALL_ELEMENTS)) {}
  	 $nb_el = $this->$sc->ALL_ELEMENTS->length;
  	 $nb_class = $this->sc->ALL_CLASSES->length;
  	}
  	$verified = count($this->sc->RESULTS);
  	$state = @round(($verified/($nb_el+$nb_class))*($max-$min)) + $min;
  	$this->updateProgressBar($state);
  }
  
  /**
   Ajoute un message d'erreur
   **/
  private function sanityCheckAddError($el_name, $prepend, $bold, $append) {
  	echo '<script type="text/javascript">';
  	echo "addError('".$el_name."', '".$prepend."', '".$bold."', '".$append."');";
  	echo '</script>';
  	flush();
  }
  
  /**
   Ajoute un avertissement
   **/
  private function sanityCheckAddWarning($el_name, $prepend, $bold, $append) {
  	echo '<script type="text/javascript">';
  	echo "addWarning('".$el_name."', '".$prepend."', '".$bold."', '".$append."');";
  	echo '</script>';
  	flush();
  }
  
  /**
   Ajoute le message qui dit que le schéma est invalide
   **/
  private function sanityCheckSchemaBroken() {
  	echo '<script type="text/javascript">';
  	echo "schemaBroken('Schema is broken !');";
  	echo '</script>';
  	flush();
  }



}

?>
