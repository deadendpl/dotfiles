// ==UserScript==
// @name     Antifandom Redirector
// @author   Oliwier Czerwiński (oliwier.czerwi@proton.me), editor
// @version  2025.11.08
// @description redirects fandom.com->breezewiki.com
// @grant    none
// @match    https://*.fandom.com/*
// @run-at   document-start
// ==/UserScript==

(function() {
  try {
    var u = new URL(location.href);
    var hostParts = u.hostname.split('.');
    if(hostParts.length<3) {
      alert('Not a fandom subdomain');
      throw 0;
    }
    var sub = hostParts[0];
    var newUrl = 'https://farside.link/breezewiki/' +
        encodeURIComponent(sub) + u.pathname + u.search + u.hash;
    if(!location.hostname.includes("farside.link"))
      location.replace(newUrl);
  }
  catch(e) {}
}
)();
