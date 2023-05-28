--[[ init.lua ]]

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

-- which key
require('which-key').setup{}

-- cool split resizer
require('focus').setup{}

-- neogit (like magit)
local neogit = require('neogit')

neogit.setup {}

require('telescope').setup{
  defaults = {
    file_previewer = require'telescope.previewers'.vim_buffer_cat.new,
  }
}
