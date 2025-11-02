// ==UserScript==
// @name         Harmony button on MusicButler
// @version      2025.10.29
// @description  Add Harmony button under Spotify and Apple Music links on MusicButler
// @author       deadendpl
// @match        https://www.musicbutler.io/
// @match        https://www.musicbutler.io/artist-page/*
// @icon         https://www.musicbutler.io/static/favicon_package/favicon-32x32.png
// @grant        none
// ==/UserScript==

(function() {
  'use strict';

  function setHarmonyAttribute(element)
  {
    element.setAttribute("harmony", "true");
  }

  function isHarmonyAttribute(element)
  {
    if(element.attributes.harmony)
      return true;

    return false;
  }

  function addHarmonyLinks()
  {
    var elements = document.getElementsByClassName("block");
    var length = elements.length;

    for(var element of elements)
      if(element.tagName == 'A')
        if(element.href.search("open.spotify.com") >= 0 ||
           element.href.search("music.apple.com") >= 0)
          if(! isHarmonyAttribute(element))
          {
            var url = element.href;
            var full_url = "https://harmony.pulsewidth.org.uk/release" +
                "?url=" + url +
                "&gtin=&region=&musicbrainz=&deezer=&itunes=" +
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

            element.append(div);
            setHarmonyAttribute(element);
          }
  }

  const observer = new MutationObserver((mutations) => {
    mutations.forEach((mutation) => {
      addHarmonyLinks(); // Call when new nodes are added
    });
  });

  // Main page and artist page have different ids for albums divs.
  // When it doesn't exist, it throws an error and exits the script.
  // Here, if an error occurs, a null is returned and the code still
  // runs
  try
  {
    observer.observe(document.querySelector("#artist-releases-group"), {
      childList: true,
      subtree: true
    });
  }
  catch(error)
  {
    null;
  }

  try
  {
    observer.observe(document.querySelector("#feed-releases-group"), {
      childList: true,
      subtree: true
    });
  }
  catch(error)
  {
    null;
  }

  addHarmonyLinks();
})();
