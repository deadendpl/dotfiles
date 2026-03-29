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

glide.o.hint_size = "15px";

glide.prefs.set(
  "browser.startup.homepage",
  "file:///home/oliwier/.config/qutebrowser/start/start.html",
);

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
glide.keymaps.set("normal", "u", "tab_reopen");
// doesn't work
// glide.keymaps.set("normal", "<C-d>", () => {
//   glide.caret_move("down", 10);
// });

glide.keymaps.del("normal", "<C-j>");
glide.keymaps.del("normal", "<C-k>");
glide.keymaps.del("normal", "<A-h>");
glide.keymaps.del("normal", "<A-l>");
