// ==UserScript==
// @name         MusicBrainz remove new releases and events
// @version      2026.08.20
// @description  the name says it all
// @author       Oliwier Czerwiński (oliwier.czerwi@proton.me)
// @match        https://musicbrainz.org/
// @run-at       document-start
// @grant        none
// ==/UserScript==

(function() {
  'use strict';

  function removeFeatureColumns() {
    // Get all .feature-column elements
    const columns = document.querySelectorAll('.feature-column');

    // We expect "Recently added releases" and "Recently added events"
    // to be the last two. If we are sure they are the last two,
    // we can target them directly.
    if (columns.length > 6) {
      const last = columns[columns.length - 1];
      const secondLast = columns[columns.length - 2];

      last.remove();
      secondLast.remove();
    }
  }

  // Run on initial load
  removeFeatureColumns();

  // Observe changes to the DOM to handle dynamic loading
  const observer = new MutationObserver(() => {
    removeFeatureColumns();
  });

  observer.observe(document.documentElement, {
    childList: true,
    subtree: true
  });
})();
