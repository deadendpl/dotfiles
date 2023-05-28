--[[ keys.lua ]]
local map = vim.api.nvim_set_keymap

vim.g.mapleader = " "
vim.g.localleader = "\\"

-- Unset arrow keys because why not? :)
map('n', '<Up>', '<NOP>', { noremap = true, silent = true })
map('n', '<Down>', '<NOP>', { noremap = true, silent = true })
map('n', '<Left>', '<NOP>', { noremap = true, silent = true })
map('n', '<Right>', '<NOP>', { noremap = true, silent = true })
map('i', '<Up>', '<NOP>', { noremap = true, silent = true })
map('i', '<Down>', '<NOP>', { noremap = true, silent = true })
map('i', '<Left>', '<NOP>', { noremap = true, silent = true })
map('i', '<Right>', '<NOP>', { noremap = true, silent = true })

-- 'jk' in insert mode to leave insert mode
map('i', 'jk', '<ESC>', {})

-- nvim-tree
map('n', '<Leader>n', [[:NvimTreeToggle<CR>]], {})

-- next buffer
map('n', '<Leader>bn', [[:bn<CR>]], {})

-- previous buffer
map('n', '<Leader>bp', [[:bp<CR>]], {})

-- MRU
map('n', '<Leader>fr', [[:MRU<CR>]], {})

-- indenting lines
map('n', '<Leader>l', [[:IndentLinesToggle<CR>]], {})

-- tagbar
map('n', '<Leader>t', [[:TagbarToggle<CR>]], {})

-- telescope (finding files)
map('n', '<Leader><Leader>', [[:Telescope find_files hidden=true<CR>]], {})

-- ':Explore' in a new split
map('n', '<leader>.', ':split <bar> wincmd J <bar> Explore<CR>', {noremap = true, silent = true})

-- neogit
map('n', '<leader>gg', [[:Neogit<CR>]], {})
