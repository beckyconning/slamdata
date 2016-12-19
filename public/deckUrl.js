"use strict";
window.slamDataDeckUrl = function (options) {
  var queryParts = function () {
    var parts = [];
    var permissionTokenPart = "permissionTokens=" + options.permissionTokens.join(",");
    var stylesheetPart = "stylesheets=" + options.stylesheetUrls.map(encodeURIComponent).join(",");
    if (options.permissionTokens && options.permissionTokens.length) { queryParts.push(permissionTokenPart); }
    if (options.stylesheetUrls && options.stylesheetUrls.length) { queryParts.push(stylesheetPart); }
    return parts;
  };
  var queryString = "?" + queryParts().join("&");
  var varsParam = options.vars ? "/?vars=" + encodeURIComponent(JSON.stringify(options.vars)) : "";
  return options.slamDataUrl + queryString + "#" + options.deckPath + options.deckId + "/view" + varsParam;
};
