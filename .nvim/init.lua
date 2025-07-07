local lspconfig = require 'lspconfig'
local configs = require 'lspconfig.configs'

if not configs.kola_ls then
  configs.kola_ls = {
    default_config = {
      cmd = { '/home/tom/Desktop/Mordrag/kola/target/debug/kola-ls' },
      root_dir = lspconfig.util.root_pattern('.git'),
      filetypes = { 'kl' },
    },
  }
end

lspconfig.kola_ls.setup {}
