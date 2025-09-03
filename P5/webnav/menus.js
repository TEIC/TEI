function list(data, lang) {
  let result = "";
  for (let item of data) {
    if (!item[lang]) {
      lang = "en";
    }
    if (item[lang].items) {
      result += `<li class="nav-item dropdown">`;
      result += `<a class="nav-link dropdown-toggle" href="#" id="${item.id}Menu" role="button" data-bs-toggle="dropdown" aria-haspopup="true" aria-expanded="false">${item[lang].name}</a>`;
      result += `<div class="dropdown-menu" aria-labelledby="${item.id}Menu">`;
        for (let subitem of item[lang].items) {
          result += `<a class="dropdown-item" href="${subitem.url}">${subitem.name}</a>`;
        }
      result += `</div></li>`;
    } else {
      result += `<li class="nav-item">`;
      result += `<a class="nav-link" href="${item[lang].url}">${item[lang].name}</a>`;
      result += `</li>`;
    }

  }
  return result;
}
function setSearch(target) {
  const input = document.getElementById("sitesearch");
  const query = document.getElementById("query_string");
  if (target == 'all') {
    input.setAttribute('value', 'https://tei-c.org');
    query.setAttribute('placeholder', 'Search');
  }
  if (target == 'guidelines') {
    input.setAttribute('value', `https://tei-c.org/release/doc/tei-p5-doc/${lang}/html/`);
    query.setAttribute('placeholder', 'Search Guidelines');
  }
}
const lang = window.location.pathname.replace(/.*\/([^\/]+)\/html.*/, "$1");
fetch("https://tei-c.org/data/menus.json")
  .then(response => response.json())
  .then(data => document.querySelector("#TEIMenu>ul").innerHTML = list(data, lang));