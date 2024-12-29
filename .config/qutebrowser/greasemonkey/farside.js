// ==UserScript==
// @name        Farside
// @author      Ben Busby, Founder,
// @author      Schimon Jehudah, Adv.
// @author      Oliwier Czerwiński (oliwier.czerwi@proton.me), editor
// @license     AGPL3
// @namespace   link.farside
// @description A smart redirecting gateway for various frontend services
// @grant       none
// @run-at      document-start
// @match       *://bing.com/*
// @match       *://*.bing.com/*
// @match       *://goodreads.com/*
// @match       *://*.goodreads.com/*
// @match       *://genius.com/*
// @match       *://google.*/*
// @match       *://*.google.*/*
// @match       *://*.imdb.com/*
// @match       *://imgur.com/*
// @match       *://*.imgur.com/*
// @match       *://instagram.com/*
// @match       *://*.instagram.com/*
// @match       *://medium.com/*
// @match       *://*.medium.com/*
// @match       *://reddit.com/*
// @match       *://*.reddit.com/*
// @match       *://stackoverflow.com/*
// @match       *://*.tiktok.com/*
// @match       *://wikipedia.org/*
// @match       *://*.wikipedia.org/*
// @match       *://yahoo.com/*
// @match       *://*.yahoo.com/*
// @match       *://yandex.com/*
// @match       *://*.yandex.com/*
// @match       *://www.youtube.com/*
// @version     1.0.0
// @icon        data:image/png;base64,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
// ==/UserScript==

var hostname, pathname, path;

let url = new URL(location.href);
hostname = url.hostname;
pathname = url.pathname;

function pickParameters(para) {
  for (let i = 0; i < para.length; i++) {
    if (url.searchParams.get(para[i])) {
      para = url.searchParams.get(para[i]);
      url.searchParams.delete(para[i]);
      return para;
    };
  };
};

switch (true) {

  case (hostname.endsWith('translate.yandex.com')):
        // FIXME website blocks us from redirecting
        // function setTimeout() seems to not work
        // consider different approach/way
  case (hostname.endsWith('translate.google.com')):

  site = [
    // lingva
    'lgv,https://farside.link/lingva',
    // simplytranslate
    'smp,https://farside.link/simplytranslate'];
  break;

  // TODO bing mapquest moovit waze yandex
  // FIXME website blocks us from redirecting
  case (hostname.includes('google.') &&
        pathname.startsWith('/maps')):

  site = [
    // openstreetmap
    'osm,https://www.openstreetmap.org'];
  break;

  case (hostname.includes('yandex.') &&
        pathname.startsWith('/search')):
  case (hostname.startsWith('search.yahoo.')):
  case (hostname.includes('yahoo.') &&
        pathname.startsWith('/search')):
  case (hostname.includes('bing.') &&
        pathname.startsWith('/search')):
  // FIXME website blocks us from redirecting
  case (hostname.includes('google.') &&
        pathname.startsWith('/search')):

  site = [
    // searx
    'srx,https://farside.link/searx',
    // searxng
    'sng,https://farside.link/searxng',
    // whoogle
    'who,https://farside.link/whoogle'];
    // yacy
  break;

  case hostname.endsWith('imgur.com'):

  site = [
    // rimgo
    'rim,https://farside.link/rimgo'];
  break;

  case hostname.endsWith('instagram.com'):

  site = [
    // bibliogram
    'bib,https://farside.link/bibliogram'];
  break;

  case hostname.endsWith('medium.com'):

  site = [
    // scribe
    'scr,https://farside.link/scribe'];
  break;

  case hostname.endsWith('reddit.com'):

  site = [
    // libreddit
    'lib,https://farside.link/redlib'];
  break;

  case hostname.endsWith('twitter.com'):

  site = [
    // nitter
    'nit,https://farside.link/nitter'];
  break;

  case hostname.endsWith('tiktok.com'):

  site = [
    // proxitok
    'tik,https://farside.link/proxitok'];
  break;

  case hostname.endsWith('stackoverflow.com'):

  site = [
    // anonymousoverflow
    'sta,https://farside.link/anonymousoverflow'];
  break;

  case hostname.endsWith('wikipedia.org'):

  site = [
    // wikiless
    'wik,https://farside.link/wikiless'];
  break;

  case hostname.endsWith('goodreads.com'):

  site = [
    // biblioreads
    'bibr,https://biblioreads.eu.org',
    'bibr,https://biblioreads.vercel.app',
    'bibr,https://biblioreads.mooo.com',
    'bibr,https://bl.vern.cc',
    'bibr,https://biblioreads.lunar.icu',
    // 'bibr,https://read.whateveritworks.org',
    'bibr,https://biblioreads.privacyfucking.rocks',
    'bibr,https://read.seitan-ayoub.lol'];
  break;

  case hostname.endsWith('genius.com'):

  site = [
    // dumb
    'gen,https://farside.link/dumb'];
  break;

  case hostname.endsWith('imdb.com'):

  site = [
    // libremdb
    'lid,https://farside.link/libremdb'];
  break;

  case hostname.endsWith('youtube.com'):

  site = [
    // invidious
    'inv,https://farside.link/invidious',
    // piped
    // 'pip,https://farside.link/piped'
  ];
  break;
};

// Affairs Settlement Below
// Don't dare to give up nor give in

// site
site = site[Math.floor(Math.random()*site.length)];
name = site.split(',')[0];
site = site.split(',')[1];

// path
switch (name) {

  // consider function setTimeout()
  case 'bib':
    if (!pathname.includes('accounts/login')) {
      path = '/u' + pathname;
    } else {
      if (url.searchParams.get('next')) {
        para = ['next'];
        path = '/u' + pickParameters(para);
      };
    };
  break;

  case 'lgv':
    // extract text parameter
    paras = ['sl','tl','text']

    function setValue(para) {
      if (url.searchParams.get(para)) {
        para = [para];
        return pickParameters(para);
      } else { return 'auto' };
    }

    for (let i = 0; i < paras.length; i++) {
      paras[i] = setValue(paras[i]);
    }

    if (paras[2] === 'auto') { paras[2] = '' };
    path = '/' + paras[0] + '/' + paras[1] + '/' + paras[2];
  break;

  case 'osm':
    // extract coordinations
    if (pathname.includes('@')) {
      cord = pathname.split('/')[2].split('&')[0].slice(1);
      cord = cord.split(',');
    }
    path = '/#map=' + cord[2].slice(0, 1) +
           '/' + cord[0] + '/' + cord[1];
  break;

  case 'sng':
  case 'srx':
  case 'who':
    // extract search parameter
    para = ['q','text','p'];
    //pickParameters(paras);
    para = pickParameters(para); // works even when calling the function?
    path = '/search?q=' + para;
  break;

  case 'wik':
    // extract language code
    code = hostname.split('.');
    if (code.length === 3) {
      lang = code[0];
    }
    path = pathname + '?lang=' + lang;
  break;

  case 'ycy':
    // extract search parameter
    para = ['q','text','p'];
    //pickParameters(paras);
    para = pickParameters(para); // works even when calling the function?
    path = '/yacysearch.html?query=' + para;
  break;
}

if (!path) {
  path = pathname + url.search + url.hash;
}

location.href = site + path;
