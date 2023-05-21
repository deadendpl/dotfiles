--[[ keys.lua ]]
local map = vim.api.nvim_set_keymap

-- 'jk' in insert mode to leave insert mode
map('i', 'jk', '<ESC>', {})

-- nvim-tree
map('n', '<Leader>n', [[:NvimTreeToggle<CR>]], {})

-- indenting lines
map('n', '<Leader>l', [[:IndentLinesToggle<CR>]], {})

-- tagbar
map('n', '<Leader>t', [[:TagbarToggle<CR>]], {})

-- telescope (finding files)
map('n', '<Leader><Leader>', [[:Telescope find_files hidden=true<CR>]], {})

-- ':Explore' in a new split
map('n', '<leader>.', ':split <bar> wincmd J <bar> Explore<CR>', {noremap = true, silent = true})
