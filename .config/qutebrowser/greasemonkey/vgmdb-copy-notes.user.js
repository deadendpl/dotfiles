// ==UserScript==
// @name         Copy notes from VGMdb entry
// @version      2025.08.21
// @description  Adds a button that copies the notes of a VGMdb entry to clipboard.
// @author       Oliwier Czerwiński (oliwier.czerwi@proton.me)
// @match        https://vgmdb.net/album/*
// ==/UserScript==

(function() {
  'use strict';

  function copyNotes() {
    var text = document.getElementById("notes").innerText;
    navigator.clipboard.writeText(text);
  }

  var button = document.createElement("button");
  button.onclick = copyNotes;
  button.innerHTML = "<h3>Copy notes</h3>";
  button.style.backgroundColor = "#2f364f";
  button.style.border = "1px solid #2f364f";
  button.style.borderRadius = "20px";
  button.style.marginLeft = "5px";
  button.style.cursor = "pointer";

  document.getElementById("collapse_credits").parentElement.before(button);
})();
