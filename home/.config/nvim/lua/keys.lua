--[[ keys.lua ]]
local map = vim.api.nvim_set_keymap

-- remap the key used to leave insert mode
map('i', 'jk', '<ESC>', {})

-- Toggle nvim-tree
map('n', '<Leader>n', [[:NvimTreeToggle<CR>]], {})

-- Toggle more plugins
map('n', '<Leader>l', [[:IndentLinesToggle<CR>]], {})
map('n', '<Leader>t', [[:TagbarToggle<CR>]], {})
map('n', '<Leader>ff', [[:Telescope find_files<CR>]], {})
