-- [[ plug.lua ]]

return require('packer').startup(function(use)
  -- [[ Plugins Go Here ]]
  use {
    'kyazdani42/nvim-tree.lua',                -- filesystem navigation
    requires = 'kyazdani42/nvim-web-devicons'  -- filesystem icons
  }

-- MRU
use { 'vim-scripts/mru.vim' }

-- cursor jump
  use { 'DanilaMihailov/beacon.nvim' }               

-- statusline
  use {
    'nvim-lualine/lualine.nvim',
    requires = {'kyazdani42/nvim-web-devicons',
                opt = true}
  }

-- cool split resizing
use { "beauwilliams/focus.nvim", config = function() require("focus").setup() end }

-- which key
use {
  "folke/which-key.nvim",
  config = function()
    vim.o.timeout = true
    vim.o.timeoutlen = 150
    require("which-key").setup {
      -- your configuration comes here
      -- or leave it empty to use the default settings
      -- refer to the configuration section below
    }
  end
}


-- themes
  use { 'Mofiqul/dracula.nvim', as = "dracula"}
  use { "catppuccin/nvim", as = "catppuccin" }

-- dashboard
  use {
      'goolord/alpha-nvim',
      config = function ()
          require'alpha'.setup(require'alpha.themes.dashboard'.config)
      end
  }

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

  use { 'TimUntersberger/neogit', requires = 'nvim-lua/plenary.nvim' } -- git


end, {
  config = {
    package_root = vim.fn.stdpath('config') .. 'packages'
  }
})
