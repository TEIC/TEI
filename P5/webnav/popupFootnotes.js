/* 
  This file provides popup functionality for footnotes and bibliography references
  in the Guidelines.
  
  Created by Martin Holmes, 2013-07 and 2014-07, free for all for everything.
 */
 
 // Make these variables global so we can use them everywhere.
 const footnotePopup = document.createElement('div'),
    footnotePopupContent = document.createElement('div'),
    closeBtn = document.createElement('button');

 let divBiblWithShadow = null;

document.addEventListener("DOMContentLoaded", function() {
  footnotePopup.setAttribute('id', 'footnotePopup');
  closeBtn.setAttribute('class', 'footnotePopupCloser');
  closeBtn.addEventListener('click', e => { e.target.parentNode.style.display = "none" });
  // "x" says nothing about what the button does; for screen readers we need a label.
  closeBtn.setAttribute('aria-label', "Close popup");
  closeBtn.appendChild(document.createTextNode('x'));
  footnotePopupContent.setAttribute('id', 'footnotePopupContent');
  footnotePopup.appendChild(closeBtn);
  footnotePopup.appendChild(footnotePopupContent);
  document.getElementsByTagName('body')[0].appendChild(footnotePopup);

  getBiblSource();

//Now we work through the links to footnotes.
  // Create an array of links to notes.
  let links = Array.from(document.querySelectorAll('a[href ^= "#Note"]'))
    // Filter out links at the bottom back to the content being notated.
    .filter(link => (! link.classList.contains('link_return')));
  // Add event listeners to show popup notes when each link is clicked.
  links.forEach(link => { link.addEventListener('click', showPopupFootnote) });
  
});

async function getBiblSource(){
  try{
    //Get the BIB file.
    const response = await fetch('BIB.html');
    const unparsedHtml = await response.text();
    //Parse it so we can get the bits we need.
    const parser = new DOMParser();
    const bibDoc = parser.parseFromString(unparsedHtml, 'text/html');
    //Find the bibls.
    const listBibls = bibDoc.querySelectorAll('ol.listBibl');
    //Create an element to attach them to.
    divBiblWithShadow = document.createElement('div');
    document.body.appendChild(divBiblWithShadow);
    divBiblWithShadow.style.display = 'none';

    //Create a shadow root.
    const shadow = divBiblWithShadow.attachShadow({mode: 'open'});
    listBibls.forEach(el => shadow.appendChild(el));

    //Now we're in a position to switch out the hrefs with 
    //onclick events.
    setupBiblPopups();
  } catch (err) {
    console.error('Error retrieving BIB.html:', err);
  }
}

function showPopupFootnote(event){
  event.preventDefault();
  /* The event's currentTarget is the element that has the event attached 
   (<a>), rather than the descendant element that fired the event (<sup>). */
  let el = event.currentTarget,
      footnoteId = el.getAttribute('href');
  if ( footnoteId === null ) {
    console.warn('Could not generate note content for anchor:');
    console.warn(el);
    return;
  }
  // When the close button is clicked, return focus back to the link.
  closeBtn.addEventListener('click', e => { el.focus() }, { once: true });
  // Take the '#' off the link ID.
  footnoteId = footnoteId.substring(1);
  let footnote = document.getElementById(footnoteId);
//If something is missing, we just default to original behaviour and jump to the footnote.
  if ((footnotePopup === null)||(footnotePopupContent === null)||(footnote === null)){
    document.location.hash = footnoteId;
    return;
  }
//Otherwise, we populate the popup with the content of the footnote, and show it.
  let cloneFootnote = footnote.cloneNode(true);
//We need to remove the id because it'll be a duplicate.
  cloneFootnote.setAttribute('id', '');
  // Also remove links back to the text because they are not needed in popups.
  cloneFootnote.querySelectorAll("a.link_return").forEach(a => {a.parentElement.removeChild(a)});
//Add it to the popup.
  clearContent(footnotePopupContent);
  footnotePopupContent.appendChild(cloneFootnote);
  footnotePopup.style.display = 'block';
  // Move focus to the close button, so screen readers have access to the note content.
  closeBtn.focus();

  // Position the popup near the footnote
  const footnoteY = el.getBoundingClientRect().top;
  const popupHeight = footnotePopup.getBoundingClientRect().height;
  footnotePopup.style.top = (footnoteY - popupHeight - 20) + 'px';
}

function clearContent(targetEl){
  if (targetEl == null){return;}
  for (var i=targetEl.childNodes.length-1; i>-1; i--){
    targetEl.removeChild(targetEl.childNodes[i]);
  }
} 

//Bind the escape key so that it hides the popup if it's showing.
document.addEventListener("keyup", function(e) {
  if(e.keyCode === 27) {
    if (document.getElementById('footnotePopup').style.display == 'block'){
      // Simulate a click on the close button, so that focus returns where it needs to. 
      closeBtn.click();
      e.preventDefault();
      e.stopPropagation();
    }
  }
})

//These functions set up and handle the display of bibliographical references
//as popups.

//This function finds all links to items in the bibliography and turns them
//into JS calls which retrieve the content which has been imported into 
//an iframe, and display it as a popup.
function setupBiblPopups () {
  var links = document.getElementsByTagName('a');
  for (var i=0; i<links.length; i++){
    var href = links[i].getAttribute('href');
    if (href && href.match(/BIB.html#/)){
      let biblId = href.split('#')[1];
      links[i].addEventListener('click', function(e) {
        e.preventDefault();
        showPopupBibl(e.target, biblId);
      });
      links[i].removeAttribute('href');
      //Add this so that we retain the appearance
      //even though the @href is gone.
      links[i].classList.add('popupLink');
    }
  }
}

//This function shows a bibl popup. It differs slightly from the function for note
//popups, so it is distinct.
function showPopupBibl(el, biblId){
  var bibl = null;
//We have to be cautious here; some browsers block access to the iframe 
//document contents from another document, especially when running locally.
  try{
    bibl = divBiblWithShadow.shadowRoot.getElementById(biblId);
  }
  catch(e){
    document.location = 'BIB.html#' + biblId;
    return;
  }
  
  if ((footnotePopup == null)||(footnotePopupContent == null)||(bibl == null)){
//If something is missing, we just default to original behaviour and jump to the bibliography.
    document.location = 'BIB.html#' + biblId;
    return;
  }
//Otherwise, we populate the popup with the content of the bibl, and show it.
  var biblContent = bibl.cloneNode(true);
  // Also remove links back to the text because they are not needed in popups.
  biblContent.querySelectorAll("a.link_return").forEach(function(a) {a.parentElement.removeChild(a)});
//Add it to the popup.
  clearContent(footnotePopupContent);
  footnotePopupContent.innerHTML = biblContent.innerHTML;
  footnotePopup.style.display = 'block';

  // Position the popup near the footnote
  const footnoteY = el.getBoundingClientRect().top;
  const popupHeight = footnotePopup.getBoundingClientRect().height;
  footnotePopup.style.top = (footnoteY - popupHeight - 20) + 'px';
}


