javascript:(function () {prompt('Copy link:', String.prototype.concat('[[pdfview:').concat(PDFViewerApplication.baseUrl.substr((PDFViewerApplication.baseUrl.split("//")[0].length + "//".length)).concat('::').concat(PDFViewerApplication.page).concat(']]')); }))();

javascript:(function () {prompt('Copy link:', String.prototype.concat('[[pdfview:').concat(PDFViewerApplication.baseUrl.substr((PDFViewerApplication.baseUrl.split("//")[0].length + "//".length))).concat('::').concat(PDFViewerApplication.page).concat(']]')); })();

// works with pdfview:https://.*.pdf
// and pdfview:/home/user/.*.pdf
javascript:(function () {var url = PDFViewerApplication.baseUrl; var cut_off_string_from_beg = String.prototype.concat(url.split('//')[0]).concat('//'); if (cut_off_string_from_beg != "file://") {cut_off_string_from_beg = ""} prompt("Copy link:", String.prototype.concat('[[pdfview:').concat(url.substr(cut_off_string_from_beg.length)).concat('::').concat(PDFViewerApplication.page).concat(']]'));})();
