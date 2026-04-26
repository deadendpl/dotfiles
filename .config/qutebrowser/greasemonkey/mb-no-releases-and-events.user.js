// ==UserScript==
// @name         MusicBrainz hide new releases and events
// @version      2026.04.25
// @description  the name says it all
// @author       Oliwier Czerwiński (oliwier.czerwi@proton.me)
// @match        https://musicbrainz.org/
// ==/UserScript==

(function() {
  'use strict';

  var stuff = document.getElementsByClassName("artwork-cont");
  var length = stuff.length;
  for(var i = 0; i < length; i++)
	stuff[i].parentElement.parentElement.remove();
})();
