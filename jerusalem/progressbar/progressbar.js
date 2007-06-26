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
	newdiv.setAttribute('style', 'width: 600px; margin: 1px; background: #ff6666; font-size: 12px');
	
	var error_bold = document.createElement('b');
	error_bold.appendChild(document.createTextNode('Error'));

	var dots = document.createTextNode(': ');

	var element_name = document.createElement('a');
	element_name.appendChild(document.createTextNode(el_name));
	element_name.setAttribute('style', 'color: black; font-weight: bold');
	element_name.setAttribute('href', 'http://tei.oucs.ox.ac.uk/Query/tag.xq?name='+el_name);

	var bolded_text = document.createElement('a');
	bolded_text.appendChild(document.createTextNode(in_bold));
	bolded_text.setAttribute('style', 'color: black; font-weight: bold; font-decoration: none');
	bolded_text.setAttribute('href', 'http://tei.oucs.ox.ac.uk/Query/tag.xq?name='+in_bold);

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
	newdiv.setAttribute('style', 'width: 600px; margin: 1px; background: lightgreen; font-size: 12px');
	
	var error_bold = document.createElement('b');
	error_bold.appendChild(document.createTextNode('Warning'));

	var dots = document.createTextNode(': ');

	var element_name = document.createElement('a');
	element_name.appendChild(document.createTextNode(el_name));
	element_name.setAttribute('style', 'color: black; font-weight: bold');
	element_name.setAttribute('href', 'http://tei.oucs.ox.ac.uk/Query/tag.xq?name='+el_name);

	var bolded_text = document.createElement('a');
	bolded_text.appendChild(document.createTextNode(in_bold));
	bolded_text.setAttribute('style', 'color: black; font-weight: bold; font-decoration: none');
	bolded_text.setAttribute('href', 'http://tei.oucs.ox.ac.uk/Query/tag.xq?name='+in_bold);

	newdiv.appendChild(error_bold);
	newdiv.appendChild(dots);
	newdiv.appendChild(element_name);
	newdiv.appendChild(document.createTextNode(prepend));
	newdiv.appendChild(bolded_text);
	newdiv.appendChild(document.createTextNode(append));

	div.appendChild(newdiv);
}

function addElementContainer(el_name) {
	var div = document.getElementById("error_div");
	var newdiv = document.createElement('div');
	newdiv.setAttribute('style', 'width: 600px; margin: 5px; padding: 5px; background: #dddd66; font-size: 12px; border: 1px solid red');
	newdiv.setAttribute('id', 'element_'+el_name);
	
	var element_name = document.createElement('a');
	element_name.appendChild(document.createTextNode('In ' + el_name));
	element_name.setAttribute('style', 'color: black; font-weight: bold');
	element_name.setAttribute('href', 'http://tei.oucs.ox.ac.uk/Query/tag.xq?name='+el_name);

	newdiv.appendChild(element_name);
	newdiv.appendChild(document.createElement('br'));

	div.appendChild(newdiv);
}

function addElementContainerError(used, element, problem) {
	var div = document.getElementById('element_'+used);
	var newdiv = document.createElement('div');
	newdiv.setAttribute('style', 'width: 590px; margin: 1px; background: #ddddff font-size: 12px; padding-left: 20px');
	var element_name = document.createElement('a');
	element_name.appendChild(document.createTextNode(element));
	element_name.setAttribute('style', 'color: black; font-weight: bold');
	element_name.setAttribute('href', 'http://tei.oucs.ox.ac.uk/Query/tag.xq?name='+element);
	newdiv.appendChild(element_name);
	newdiv.appendChild(document.createTextNode(' ' + problem));
	div.appendChild(newdiv);

}

function addElementContainerWarning(used, element, problem) {
	var div = document.getElementById('element_'+used);
	var newdiv = document.createElement('div');
	newdiv.setAttribute('style', 'width: 590px; margin: 1px; font-size: 12px; padding-left: 20px');
	var element_name = document.createElement('a');
	element_name.appendChild(document.createTextNode(element));
	element_name.setAttribute('style', 'color: black; font-weight: bold');
	element_name.setAttribute('href', 'http://tei.oucs.ox.ac.uk/Query/tag.xq?name='+element);
	newdiv.appendChild(element_name);
	newdiv.appendChild(document.createTextNode(' ' + problem));
	div.appendChild(newdiv);

}

function schemaBroken(schema_broken_text) {
	div = document.getElementById("schema_broken_div");
	var sc = document.createTextNode(schema_broken_text);
	div.appendChild(sc);
}

function schemaOk(schema_ok_text) {
	div = document.getElementById("schema_ok_div");
	var sc = document.createTextNode(schema_ok_text);
	div.appendChild(sc);
}
