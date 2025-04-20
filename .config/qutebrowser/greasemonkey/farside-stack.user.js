// ==UserScript==
// @name         Farside Stack Exchange/Overflow redirector
// @version      2025.04.19
// @description  redirects Stack Exchange/Overflow pages to AnonymousOverflow using farside
// @author       deadendpl (oliwier.czerwi@proton.me)
// @run-at       document-start
// @match        https://stackoverflow.com/questions/*
// @match        https://*.stackexchange.com/questions/*
// ==/UserScript==

(function() {
  'use strict';

  var hostname = location.hostname;

  switch(true)
  {
    case(hostname == "stackoverflow.com"):
    location.host = location.host.replace(
      "stackoverflow.com",
      "farside.link/anonymousoverflow");
    location.replace("https://farside.link/anonymousoverflow" +
                     location.pathname);
    break;

    case(hostname.endsWith("stackexchange.com")):
    var parts = hostname.split(".");
    var path = "/anonymousoverflow/exchange/" +
        parts[0] + location.pathname;
    location.replace("https://farside.link" + path);
    break;
  }
})();
