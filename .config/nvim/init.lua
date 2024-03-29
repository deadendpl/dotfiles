local ensure_packer = function()
  local fn = vim.fn
  local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
    vim.cmd [[packadd packer.nvim]]
    return true
  end
  return false
end

local packer_bootstrap = ensure_packer()

return require('packer').startup(function(use)
  use 'wbthomason/packer.nvim'
  -- My plugins here
   use {
    'kyazdani42/nvim-tree.lua',                -- filesystem navigation
    requires = 'kyazdani42/nvim-web-devicons'  -- filesystem icons
  }

-- cursor jump
  use { 'DanilaMihailov/beacon.nvim' }

-- statusline
  use {
    'nvim-lualine/lualine.nvim',
    requires = {'kyazdani42/nvim-web-devicons',
                opt = true}
  }

-- which key
use {
  "folke/which-key.nvim",
  config = function()
    vim.o.timeout = true
    vim.o.timeoutlen = 150
    require("which-key").setup {
      -- your configuration comes here
      -- or leave it empty to use the default settings
    }
  end
}

-- themes
  use { 'Mofiqul/dracula.nvim', as = "dracula"}
  use { "catppuccin/nvim", as = "catppuccin" }
  use { 'uZer/pywal16.nvim', as = 'pywal16' }

-- dashboard
  use {
      'goolord/alpha-nvim',
      config = function ()
          require'alpha'.setup(require'alpha.themes.dashboard'.config)
      end
  }

-- org mode
use {'nvim-treesitter/nvim-treesitter'}
use {'nvim-orgmode/orgmode', config = function()
  require('orgmode').setup{}
end
}

-- minesweeper
use {'seandewar/nvimesweeper'}


-- [[ Dev ]]
  use {
    'nvim-telescope/telescope.nvim',                 -- fuzzy finder
    requires = { {'nvim-lua/plenary.nvim'} }
  }
  use { 'majutsushi/tagbar' }                        -- code structure
  use { 'Yggdroot/indentLine' }                      -- see indentation
  use { 'tpope/vim-fugitive' }                       -- git integration
  use { 'junegunn/gv.vim' }                          -- commit history
  use { 'windwp/nvim-autopairs' }

  use { 'NeogitOrg/neogit', requires = 'nvim-lua/plenary.nvim' } -- git


  -- Automatically set up your configuration after cloning packer.nvim
  -- Put this at the end after all plugins
  if packer_bootstrap then
    require('packer').sync()
  end

-- variables

local g = vim.g
g.t_co = 256
g.background = "dark"

-- options
local opt = vim.opt
local cmd = vim.api.nvim_command

-- [[ Context ]]
--opt.colorcolumn = '80'           -- str:  Show col for max line length
opt.number = true                -- bool: Show line numbers
--opt.relativenumber = true        -- bool: Show relative line numbers
opt.scrolloff = 4                -- int:  Min num lines of context
--opt.signcolumn = "yes"           -- str:  Show the sign column

-- [[ Filetypes ]]
opt.encoding = 'utf8'            -- str:  String encoding to use
opt.fileencoding = 'utf8'        -- str:  File encoding to use

-- [[ Theme ]]
opt.syntax = "ON"                -- str:  Allow syntax highlighting
opt.termguicolors = true         -- bool: If term supports ui color then enable

-- [[ Search ]]
opt.ignorecase = true            -- bool: Ignore case in search patterns
opt.smartcase = true             -- bool: Override ignorecase if search contains capitals
opt.incsearch = true             -- bool: Use incremental search
opt.hlsearch = true              -- bool: Highlight search matches

-- [[ Whitespace ]]
opt.expandtab = true             -- bool: Use spaces instead of tabs
--opt.shiftwidth = 4               -- num:  Size of an indent
--opt.softtabstop = 4              -- num:  Number of spaces tabs count for in insert mode
--opt.tabstop = 4                  -- num:  Number of spaces tabs count for

-- [[ Splits ]]
opt.splitright = true            -- bool: Place new window to right of current one
opt.splitbelow = true            -- bool: Place new window below the current one

-- theming
local latte = require("catppuccin.palettes").get_palette "latte"
local frappe = require("catppuccin.palettes").get_palette "frappe"
local macchiato = require("catppuccin.palettes").get_palette "macchiato"
local mocha = require("catppuccin.palettes").get_palette "mocha"


require('lualine').setup {
  options = {
    theme = 'pywal16-nvim',
  }
}

-- keybindings
local map = vim.api.nvim_set_keymap

vim.g.mapleader = " "
vim.g.localleader = "\\"

local wk = require("which-key")
wk.register({
  ["<leader>o"] = { name = "+orgmode" },
  ["<leader>f"] = { name = "+file" },
  ["<leader><leader>"] = { "<cmd>Telescope find_files hidden=true<cr>", "Find File" },
  ["<leader>ff"] = { "<cmd>Telescope find_files hidden=true<cr>", "Find File" },
  ["<leader>fr"] = { "<cmd>Telescope oldfiles<cr>", "Open Recent File" },
  ["<leader>fn"] = { "<cmd>enew<cr>", "New File" },

  ["<leader>b"] = { name = "+buffer" },
  ["<leader>bn"] = { [[:bn<CR>]], "Next buffer" },
  ["<leader>bp"] = { [[:bp<CR>]], "Previous buffer" },
  ["<leader>bk"] = { [[:bdelete<CR>]], "Kill buffer" },

  ["<leader>w"] = { name = "+split" },
  ["<leader>ws"] = { [[:sp<CR>]], "Horizontal split" },
  ["<leader>wv"] = { [[:vsp<CR>]], "Verical split" },
  ["<leader>ww"] = { [[<C-w>w]], "Next split" },
  ["<leader>wh"] = { [[<C-w>h]], "Right split" },
  ["<leader>wj"] = { [[<C-w>j]], "Lower split" },
  ["<leader>wk"] = { [[<C-w>k]], "Upper split" },
  ["<leader>wl"] = { [[<C-w>l]], "Left split" },
  ["<leader>wc"] = { [[:close<CR>]], "Kill split" },

  ["<leader>n"] = { [[:NvimTreeToggle<CR>]], "Nvim Tree" },
  ["<leader>l"] = { [[:IndentLinesToggle<CR>]], "Indenting lines" },
  ["<leader>t"] = { [[:TagbarToggle<CR>]], "Tagbar" },
  ["<leader>gg"] = { [[:Neogit<CR>]], "Neogit" },
  ["<leader>."] = { ":split <bar> wincmd J <bar> Explore<CR>", "File browser" },
})

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

-- startup

-- using system's clipboard
vim.o.clipboard = "unnamedplus"

-- nvim-tree
require('nvim-tree').setup{}

require('nvim-autopairs').setup{} -- Add this line

-- which key
require('which-key').setup{}

-- neogit (like magit)
local neogit = require('neogit')

neogit.setup {}

require('telescope').setup{
  defaults = {
    file_previewer = require'telescope.previewers'.vim_buffer_cat.new,
  }
}

-- org mode setup
-- Load custom treesitter grammar for org filetype
require('orgmode').setup_ts_grammar()

-- Treesitter configuration
require('nvim-treesitter.configs').setup {
  -- If TS highlights are not enabled at all, or disabled via `disable` prop,
  -- highlighting will fallback to default Vim syntax highlighting
  highlight = {
    enable = true,
    -- Required for spellcheck, some LaTex highlights and
    -- code block highlights that do not have ts grammar
    additional_vim_regex_highlighting = {'org'},
  },
  ensure_installed = {'org'}, -- Or run :TSUpdate org
}

require('orgmode').setup({
--  org_agenda_files = {'~/Dropbox/org/*', '~/my-orgs/**/*'},
--  org_default_notes_file = '~/Dropbox/org/refile.org',
})

local pywal16 = require('pywal16')
pywal16.setup()

end)
