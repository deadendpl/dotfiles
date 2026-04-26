// Config docs:
//
//   https://glide-browser.app/config
//
// API reference:
//
//   https://glide-browser.app/api
//
// Default config files can be found here:
//
//   https://github.com/glide-browser/glide/tree/main/src/glide/browser/base/content/plugins
//
// Most default keymappings are defined here:
//
//   https://github.com/glide-browser/glide/blob/main/src/glide/browser/base/content/plugins/keymaps.mts
//
// Try typing `glide.` and see what you can do!

// https://github.com/glide-browser/glide/discussions/127
// if keepassxc or other password managers don't work

glide.o.hint_size = "1rem";
// glide.o.hint_chars = "asdfghjhkl";

const prefs: Record<string, any> = {
  "toolkit.scrollbox.pagescroll.maxOverlapLines": 50,
  "toolkit.scrollbox.pagescroll.maxOverlapPercent": 50,
  "sidebar.verticalTabs": true,
  "sidebar.verticalTabs.dragToPinPromo.dismissed": true,
  "browser.theme.toolbar-theme": 2,
  // "browser.startup.homepage":
  // "file:///home/oliwier/.config/qutebrowser/start/start.html",
  "browser.display.use_system_colors": true,
  "signon.generation.enabled": false,
  "browser.ml.enable": false,
  "browser.newtabpage.enabled": false,
  "browser.download.useDownloadDir": false,
  "browser.startup.blankWindow": true,
  "browser.theme.windows.accent-color-in-tabs.enabled": true,
  // no looking for sponsor websites to add to the new tab page shortcuts
  "browser.topsites.contile.enabled": false,
  "browser.uidensity": 1,
  "font.name.monospace.x-western": "JetBrainsMono Nerd Font Mono",
  "font.name.sans-serif.x-western": "Ubuntu Nerd Font",
  "font.name.serif.x-western": "Ubuntu Nerd Font",
  "general.smoothScroll": false,
  "media.gmp-widevinecdm.enabled": true,
  "privacy.trackingprotection.enabled": true,
  "sidebar.verticalTabs": true,
  "signon.rememberSignons": false,
  "toolkit.scrollbox.smoothScroll": false,
  "ui.tooltip.delay_ms": 300,
};

for(const[key, value] of Object.entries(prefs))
  glide.prefs.set(key, value);

glide.keymaps.set("normal", "<leader>r", "config_reload");
// doesn't work
glide.keymaps.set("normal", "<A-gt>", "scroll_bottom");
glide.keymaps.set("normal", "<A-lt>", "scroll_top");
glide.keymaps.set("normal", "J", "tab_next");
glide.keymaps.set("normal", "K", "tab_prev");
glide.keymaps.set("normal", "H", "back");
glide.keymaps.set("normal", "L", "forward");
glide.keymaps.set("normal", "d", "tab_close");
glide.keymaps.set("normal", "r", "reload");
glide.keymaps.set("normal", "O", "tab_new");
glide.keymaps.set(["normal"], "o", () =>
  glide.keys.send("<C-l>", { skip_mappings: true }),
  { description: "Invoke URL bar in current tab" }
);
glide.keymaps.set("normal", "u", "tab_reopen");
glide.keymaps.set("normal", "<C-d>", "scroll_page_down");
glide.keymaps.set("normal", "<C-u>", "scroll_page_up");
glide.keymaps.set(["normal"], "gj", () =>
  glide.keys.send("<C-S-PageDown>", { skip_mappings: true }),
  { description: "Move the tab down" }
);
glide.keymaps.set(["normal"], "gk", () =>
  glide.keys.send("<C-S-PageUp>", { skip_mappings: true }),
  { description: "Move the tab up" }
);
glide.keymaps.set("normal", "'", "repeat");
glide.keymaps.set("normal", "/", () =>
  glide.keys.send("<C-f>", { skip_mappings: true }),
);

glide.keymaps.del("normal", "<C-j>");
glide.keymaps.del("normal", "<C-k>");
glide.keymaps.del("normal", "<A-h>");
glide.keymaps.del("normal", "<A-l>");
