{ vimPlugins, vimPlugins', ... }:

let
  nvim-treesitter = vimPlugins.nvim-treesitter.withPlugins (p: [
    p.tree-sitter-agda
    p.tree-sitter-bash
    p.tree-sitter-c
    p.tree-sitter-c-sharp
    p.tree-sitter-cpp
    p.tree-sitter-css
    p.tree-sitter-go
    p.tree-sitter-html
    p.tree-sitter-java
    p.tree-sitter-javascript
    p.tree-sitter-jsdoc
    p.tree-sitter-json
    p.tree-sitter-julia
    p.tree-sitter-lua
    p.tree-sitter-nix
    p.tree-sitter-ocaml
    p.tree-sitter-php
    p.tree-sitter-python
    p.tree-sitter-ruby
    p.tree-sitter-rust
    p.tree-sitter-scala
    p.tree-sitter-swift
    p.tree-sitter-typescript
    p.tree-sitter-toml
    p.tree-sitter-tsx
    p.tree-sitter-vim
    p.tree-sitter-yaml
  ]);
in {
  plugins = [
    { repo = vimPlugins.vim-sensible.rtp; }
    {
      repo = vimPlugins.neovim-sensible.rtp;
      "if" = "has('nvim')";
    }
    {
      repo = vimPlugins.lualine-nvim.rtp;
      hook_post_source = ''
        lua <<EOF
        require 'lualine'.setup {
          options = {
            section_separators = "",
            component_separators = "",
          }
        }
        EOF
      '';
      "if" = "has('nvim')";
    }
    {
      repo = nvim-treesitter.rtp;
      hook_post_source = ''
        lua <<EOF
        require 'nvim-treesitter.configs'.setup {
          highlight = { enable = true },
          indent = { enable = true },
        }
        EOF
      '';
      "if" = "has('nvim')";
    }
    {
      repo = vimPlugins'.onedark-nvim.rtp;
      hook_add = ''
        lua <<EOF
          vim.g.onedark_transparent_background = true
          vim.g.onedark_toggle_style_keymap = "<Nop>"
        EOF
      '';
      hook_post_source = "colorscheme onedark";
    }
  ];
}
