{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  editorsCfg = config.modules.editors;
  cfg = editorsCfg.neovim;

  nvim_treesitter = (pkgs.vimPlugins.nvim-treesitter.withPlugins (p: [
    p.tree-sitter-bash
    p.tree-sitter-c
    p.tree-sitter-c-sharp
    p.tree-sitter-cpp
    p.tree-sitter-css
    p.tree-sitter-elisp
    p.tree-sitter-go
    p.tree-sitter-haskell
    p.tree-sitter-html
    p.tree-sitter-java
    p.tree-sitter-javascript
    p.tree-sitter-jsdoc
    p.tree-sitter-json
    p.tree-sitter-julia
    p.tree-sitter-lua
    p.tree-sitter-nix
    p.tree-sitter-norg
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
  ])).rtp;

  ginit_lua = pkgs.substituteAll {
    src = ../../config/neovim/lua/ginit.lua;
    dir = "lua";

    fontFamily = editorsCfg.fonts.code.family;
    fontSize = editorsCfg.fonts.code.size;
  };
  plugins_lua = pkgs.substituteAll {
    src = ../../config/neovim/lua/plugins.lua;
    dir = "lua";

    inherit nvim_treesitter;
    bufferline_nvim = pkgs.vimPlugins.bufferline-nvim.rtp;
    clever_f_vim = pkgs.vimPlugins.clever-f-vim.rtp;
    cmp_cmdline = pkgs.vimPlugins.cmp-cmdline.rtp;
    cmp_luasnip = pkgs.vimPlugins.cmp_luasnip.rtp;
    cmp_nvim_lsp = pkgs.vimPlugins.cmp-nvim-lsp.rtp;
    cmp_omni = pkgs.vimPlugins.cmp-omni.rtp;
    cmp_path = pkgs.vimPlugins.cmp-path.rtp;
    cmp_spell = pkgs.vimPlugins.cmp-spell.rtp;
    comment_nvim = pkgs.vimPlugins.comment-nvim.rtp;
    diffview_nvim = pkgs.vimPlugins.diffview-nvim.rtp;
    gitsigns_nvim = pkgs.vimPlugins.gitsigns-nvim.rtp;
    hop_nvim = pkgs.vimPlugins.hop-nvim.rtp;
    impatient_nvim = pkgs.vimPlugins.impatient-nvim;
    lspkind_nvim = pkgs.vimPlugins.lspkind-nvim.rtp;
    lua_dev_nvim = pkgs.vimPlugins.lua-dev-nvim.rtp;
    lualine_nvim = pkgs.vimPlugins.lualine-nvim.rtp;
    luasnip = pkgs.vimPlugins.luasnip.rtp;
    neogit = pkgs.vimPlugins.neogit.rtp;
    neorg = pkgs.vimPlugins.neorg.rtp;
    nvim_autopairs = pkgs.vimPlugins.nvim-autopairs.rtp;
    null_ls_nvim = pkgs.vimPlugins.null-ls-nvim.rtp;
    nvim_cmp = pkgs.vimPlugins.nvim-cmp.rtp;
    nvim_lspconfig = pkgs.vimPlugins.nvim-lspconfig.rtp;
    nvim_tree_lua = pkgs.vimPlugins.nvim-tree-lua.rtp;
    nvim_treesitter_refactor = pkgs.vimPlugins.nvim-treesitter-refactor.rtp;
    nvim_treesitter_textobjects =
      pkgs.vimPlugins.nvim-treesitter-textobjects.rtp;
    nvim_ts_autotag = pkgs.vimPlugins.nvim-ts-autotag.rtp;
    nvim_ts_context_commentstring =
      pkgs.vimPlugins.nvim-ts-context-commentstring.rtp;
    nvim_ts_rainbow = pkgs.vimPlugins.nvim-ts-rainbow.rtp;
    nvim_web_devicons = pkgs.vimPlugins.nvim-web-devicons.rtp;
    onedark_nvim = pkgs.vimPlugins.onedark-nvim.rtp;
    octo_nvim = pkgs.my.vimPlugins.octo-nvim.rtp;
    open_browser_vim = pkgs.vimPlugins.open-browser-vim.rtp;
    open_browser_github_vim = pkgs.vimPlugins.open-browser-github-vim.rtp;
    packer_nvim = pkgs.vimPlugins.packer-nvim.rtp;
    plenary_nvim = pkgs.vimPlugins.plenary-nvim.rtp;
    project_nvim = pkgs.vimPlugins.project-nvim.rtp;
    surround_nvim = pkgs.vimPlugins.surround-nvim.rtp;
    telescope_nvim = pkgs.vimPlugins.telescope-nvim.rtp;
    telescope_file_browser_nvim =
      pkgs.vimPlugins.telescope-file-browser-nvim.rtp;
    telescope_project_nvim = pkgs.vimPlugins.telescope-project-nvim.rtp;
    telescope_symbols_nvim = pkgs.vimPlugins.telescope-symbols-nvim.rtp;
    todo_comments_nvim = pkgs.vimPlugins.todo-comments-nvim.rtp;
    toggleterm_nvim = pkgs.vimPlugins.toggleterm-nvim.rtp;
    tokyonight_nvim = pkgs.vimPlugins.tokyonight-nvim.rtp;
    trouble_nvim = pkgs.vimPlugins.trouble-nvim.rtp;
    which_key_nvim = pkgs.vimPlugins.which-key-nvim.rtp;
  };
in {
  options.modules.editors.neovim = with types; {
    enable = mkBoolOpt false;

    pager.enable = mkBoolOpt false;

    manpager.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      neovim
      neovim-qt

      # octo.nvim
      gh
    ];

    home.dataFile = {
      "nvim/site/pack/packer/start/impatient.nvim".source =
        pkgs.vimPlugins.impatient-nvim;
      "nvim/site/pack/packer/opt/packer.nvim".source =
        pkgs.vimPlugins.packer-nvim.rtp;
      "nvim/site/pack/packer/start/which-key.nvim".source =
        pkgs.vimPlugins.which-key-nvim.rtp;
    };
    home.configFile = {
      "nvim/init.lua".source = "${configDir}/neovim/init.lua";
      "nvim/ginit.vim".text = ''
        lua require("ginit")
      '';
      "nvim/lua/ginit.lua".source = "${ginit_lua}/lua/ginit.lua";
      "nvim/lua/plugins.lua".source = "${plugins_lua}/lua/plugins.lua";
      "nvim/lua/config".source = "${configDir}/neovim/lua/config";
    };

    system.userActivationScripts.neovim = ''
      rm -f ''${XDG_CACHE_HOME:-~/.cache}/nvim/luacache \
            ''${XDG_CONFIG_HOME:-~/.config}/nvim/lua/packer_compiled.lua
    '';

    env = {
      PAGER = mkIf cfg.pager.enable "${pkgs.neovim}/bin/nvim -c PAGER -";
      MANPAGER = mkIf cfg.manpager.enable "${pkgs.neovim}/bin/nvim +'Man!'";
    };

    modules.shell.aliases = { vim = "nvim"; };
  };
}
