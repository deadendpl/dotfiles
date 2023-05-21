--[[ init.lua ]]

-- LEADER
-- These keybindings need to be defined before the first /
-- is called; otherwise, it will default to "\"
vim.g.mapleader = " "
vim.g.localleader = "\\"

-- IMPORTS
require('vars')      -- Variables
require('opts')      -- Options
require('keys')      -- Keymaps
require('plug')      -- Plugins

-- using system's clipboard
vim.o.clipboard = "unnamedplus"

-- nvim-tree
require('nvim-tree').setup{}

require('nvim-autopairs').setup{} -- Add this line

-- neogit (like magit)
local neogit = require('neogit')

neogit.setup {}

require('telescope').setup{
  defaults = {
    file_previewer = require'telescope.previewers'.vim_buffer_cat.new,
  }
}
