<script language="php">

require_once( 'roma/roma.php' );


switch( $_REQUEST[ 'option' ] )
  {
    case 'old':
      $oRoma = new roma( roma_startupOption_old );
      break;
    case 'new':
      $oRoma = new roma( roma_startupOption_new );
      break;
    default:
      $oRoma = new roma( '' );
      break;
  }

$oRoma->run();
</script>