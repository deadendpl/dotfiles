// ==UserScript==
// @name     Antifandom Redirector
// @author   Ezra Barrow (barrow@tilde.team)
// @author   Oliwier Czerwiński (oliwier.czerwi@proton.me), editor
// @version  0.2.3
// @description redirects fandom.com->breezewiki.com
// @grant    none
// @match    https://*.fandom.com/*
// @run-at   document-start
// ==/UserScript==
window.location.host = window.location.host.replace("fandom.com", "antifandom.com")
