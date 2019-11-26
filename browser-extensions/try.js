// in chrome, as of 26/11/2019, this successfully inserts a string into clipboard
// in firefox, it doesn't, since additional permissions must be granted
function copyStringToClipboard (str) {
    // Temporäres Element erzeugen
          var el = document.createElement('textarea');
          // Den zu kopierenden String dem Element zuweisen
          el.value = str;
          // Element nicht editierbar setzen und aus dem Fenster schieben
          el.setAttribute('readonly', '');
          el.style = {position: 'absolute', left: '-9999px'};
          document.body.appendChild(el);
          // Text innerhalb des Elements auswählen
          el.select();
          // Ausgewählten Text in die Zwischenablage kopieren
          document.execCommand('copy');
          // Temporäres Element löschen
          document.body.removeChild(el);
}

// in PDF.js, this makes a string file:///home/user/Desktop/example.pdf to
// home/user/Desktop/example.pdf::[page-currently-shown-in-PDF.js]
document.addEventListener ("keydown", function (zEvent) {
          if (zEvent.ctrlKey && zEvent.altKey && zEvent.key === "e") {  // case sensitive
              console.log("hey");
              var link_str = String.prototype.concat(PDFViewerApplication.baseUrl).substr("file://".length).concat("::").concat(PDFViewerApplication.page);
              console.log(link_str);
              copyStringToClipboard(link_str);
          }
      });
