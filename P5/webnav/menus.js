// Pull menus live from TEI WordPress API
let ti = 0;
function list(data, top) {
  let result = '<ul';
  if (top) {
    result += ' id="menu" class="menu">';
  } else {
    result += ' class="sub-menu" aria-label="submenu">';
  }
  for (const item of data) {
    result += '<li onclick="window.location=\''+ item.url + '\'" tabindex="' + ti++ +'"><a href="' + item.url + '"';
      if ("children" in item) {
        result += ' aria-haspopup="true"';
      }
      result += '>' + item.title + '</a>';
    if ("children" in item) {
      result += list(item.children, false);
    }
    result += '</li>';
  }
  result += '</ul>';
  return result;
}
fetch("https://tei-c.org/wp-json/wp-api-menus/v2/menu-locations/primary")
  .then(response => response.json())
  .then(data => document.querySelector("#banner>nav").innerHTML = list(data, true));