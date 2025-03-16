// ==UserScript==
// @name         MusicBrainz hide new releases and events
// @version      2025.03.10
// @description  the name says it all
// @author       Oliwier Czerwiński (oliwier.czerwi@proton.me)
// @match        https://musicbrainz.org/
// ==/UserScript==

(function() {
  'use strict';

  var stuff = document.getElementsByTagName("h2");
  for(var i = 0; i < stuff.length; i++)
  {
	if(stuff[i].innerHTML == "Recently added releases" ||
	   stuff[i].innerHTML == "Recently added events")
	  stuff[i].parentElement.innerHTML = "";
	if(stuff[i].innerHTML == "Recently added releases" ||
	   stuff[i].innerHTML == "Recently added events")
	  stuff[i].parentElement.innerHTML = "";
  }
})();
