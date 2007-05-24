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

function addError(el_name, prepend, in_bold, append) {
	var div = document.getElementById("error_div");
	var newdiv = document.createElement('div');
	newdiv.setAttribute('style', 'width: 600px; margin: 1px; background: darkred; font-size: 12px');
	
	var error_bold = document.createElement('b');
	error_bold.appendChild(document.createTextNode('Error'));

	var dots = document.createTextNode(': ');

	var element_name = document.createElement('b');
	element_name.appendChild(document.createTextNode(el_name));
	element_name.setAttribute('style', 'color: #aa8888');

	var bolded_text = document.createElement('b');
	bolded_text.appendChild(document.createTextNode(in_bold));

	newdiv.appendChild(error_bold);
	newdiv.appendChild(dots);
	newdiv.appendChild(element_name);
	newdiv.appendChild(document.createTextNode(prepend));
	newdiv.appendChild(bolded_text);
	newdiv.appendChild(document.createTextNode(append));

	div.appendChild(newdiv);
}

function addWarning(el_name, prepend, in_bold, append) {
	var div = document.getElementById("error_div");
	var newdiv = document.createElement('div');
	newdiv.setAttribute('style', 'width: 600px; margin: 1px; background: darkgreen; font-size: 12px');
	
	var error_bold = document.createElement('b');
	error_bold.appendChild(document.createTextNode('Warning'));

	var dots = document.createTextNode(': ');

	var element_name = document.createElement('b');
	element_name.appendChild(document.createTextNode(el_name));
	element_name.setAttribute('style', 'color: #aa8888');

	var bolded_text = document.createElement('b');
	bolded_text.appendChild(document.createTextNode(in_bold));

	newdiv.appendChild(error_bold);
	newdiv.appendChild(dots);
	newdiv.appendChild(element_name);
	newdiv.appendChild(document.createTextNode(prepend));
	newdiv.appendChild(bolded_text);
	newdiv.appendChild(document.createTextNode(append));

	div.appendChild(newdiv);
}

function schemaBroken(schema_broken_text) {
	div = document.getElementById("schema_broken_div");
	var sc = document.createTextNode(schema_broken_text);
	div.appendChild(sc);
}