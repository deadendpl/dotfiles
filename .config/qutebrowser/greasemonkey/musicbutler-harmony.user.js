// ==UserScript==
// @name         Harmony button on MusicButler
// @version      2025.03.15
// @description  Add Harmony button under Spotify and Apple Music links on MusicButler
// @author       deadendpl
// @match        https://www.musicbutler.io/
// @match        https://www.musicbutler.io/artist-page/*
// @icon         https://www.musicbutler.io/static/favicon_package/favicon-32x32.png
// @grant        none
// ==/UserScript==

(function() {
  'use strict';

  var elements = document.getElementsByClassName("block");

  for(var i = 0; i < elements.length; i++)
    if(elements[i].tagName == 'A')
      if(elements[i].href.search("open.spotify.com") >= 0 ||
         elements[i].href.search("music.apple.com") >= 0)
      {
        var url = elements[i].href;
        var full_url = "https://harmony.pulsewidth.org.uk/release?url=" +
            url + "&gtin=&region=&musicbrainz=&deezer=&itunes=" +
            "&spotify=&tidal=&beatport=";
        var div = document.createElement("div");
        var a = document.createElement("a");
        a.href = full_url;

        var img = document.createElement("img");
        img.style.width = "60%";
        img.style.position = "relative";
        img.style.left = "20%";
        img.src = "https://harmony.pulsewidth.org.uk/favicon.svg";

        a.append(img);
        div.append(a);

        elements[i].append(div)
      }
})();
