function showPgb()
  {
    oPgb = document.getElementById( "Pgb" );

    if ( oPgb )
      {
        oPgb.style.visibility = 'visible';
      }
  }

function setPgb(pgbID, pgbValue) 
  {
    if (pgbObj = document.getElementById(pgbID))
      {
        pgbObj.width = pgbValue + '%'; // increase the progression by changing the width of the table
      }
    if (lblObj = document.getElementById(pgbID+'_label') )
      {
        lblObj.innerHTML = pgbValue + '%'; // change the label value
      }
  }
