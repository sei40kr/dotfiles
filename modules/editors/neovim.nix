{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  editorsCfg = config.modules.editors;
  cfg = editorsCfg.neovim;

  nvim-treesitter = pkgs.vimPlugins.nvim-treesitter.withPlugins (p: [
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
  ]);
  ginit_lua = pkgs.runCommandLocal "ginit.lua" {} ''
    substitute ${../../config/neovim/lua/ginit.lua} $out \
      --subst-var-by fontFamily ${escapeShellArg editorsCfg.fonts.code.family} \
      --subst-var-by fontSize ${toString editorsCfg.fonts.code.size}
  '';
  plugins_lua = pkgs.runCommandLocal "plugins.lua" {} ''
    substitute ${../../config/neovim/lua/plugins.lua} $out \
      --subst-var-by bufferline_nvim               ${pkgs.vimPlugins.bufferline-nvim.rtp} \
      --subst-var-by cmp_cmdline                   ${pkgs.vimPlugins.cmp-cmdline.rtp} \
      --subst-var-by cmp_luasnip                   ${pkgs.vimPlugins.cmp_luasnip.rtp} \
      --subst-var-by cmp_nvim_lsp                  ${pkgs.vimPlugins.cmp-nvim-lsp.rtp} \
      --subst-var-by cmp_omni                      ${pkgs.vimPlugins.cmp-omni.rtp} \
      --subst-var-by cmp_path                      ${pkgs.vimPlugins.cmp-path.rtp} \
      --subst-var-by cmp_spell                     ${pkgs.vimPlugins.cmp-spell.rtp} \
      --subst-var-by comment_nvim                  ${pkgs.vimPlugins.comment-nvim.rtp} \
      --subst-var-by diffview_nvim                 ${pkgs.vimPlugins.diffview-nvim.rtp} \
      --subst-var-by gitsigns_nvim                 ${pkgs.vimPlugins.gitsigns-nvim.rtp} \
      --subst-var-by hop_nvim                      ${pkgs.vimPlugins.hop-nvim.rtp} \
      --subst-var-by impatient_nvim                ${pkgs.vimPlugins.impatient-nvim} \
      --subst-var-by lspkind_nvim                  ${pkgs.vimPlugins.lspkind-nvim.rtp} \
      --subst-var-by lua_dev_nvim                  ${pkgs.vimPlugins.lua-dev-nvim.rtp} \
      --subst-var-by lualine_nvim                  ${pkgs.vimPlugins.lualine-nvim.rtp} \
      --subst-var-by luasnip                       ${pkgs.vimPlugins.luasnip.rtp} \
      --subst-var-by mini_nvim                     ${pkgs.vimPlugins.mini-nvim.rtp} \
      --subst-var-by neogit                        ${pkgs.vimPlugins.neogit.rtp} \
      --subst-var-by neorg                         ${pkgs.vimPlugins.neorg.rtp} \
      --subst-var-by nvim_autopairs                ${pkgs.vimPlugins.nvim-autopairs.rtp} \
      --subst-var-by null_ls_nvim                  ${pkgs.vimPlugins.null-ls-nvim.rtp} \
      --subst-var-by nvim_cmp                      ${pkgs.vimPlugins.nvim-cmp.rtp} \
      --subst-var-by nvim_lspconfig                ${pkgs.vimPlugins.nvim-lspconfig.rtp} \
      --subst-var-by nvim_tree_lua                 ${pkgs.vimPlugins.nvim-tree-lua.rtp} \
      --subst-var-by nvim_treesitter               ${nvim-treesitter.rtp} \
      --subst-var-by nvim_treesitter_refactor      ${pkgs.vimPlugins.nvim-treesitter-refactor.rtp} \
      --subst-var-by nvim_treesitter_textobjects   ${pkgs.vimPlugins.nvim-treesitter-textobjects.rtp} \
      --subst-var-by nvim_ts_autotag               ${pkgs.vimPlugins.nvim-ts-autotag.rtp} \
      --subst-var-by nvim_ts_context_commentstring ${pkgs.vimPlugins.nvim-ts-context-commentstring.rtp} \
      --subst-var-by nvim_ts_rainbow               ${pkgs.vimPlugins.nvim-ts-rainbow.rtp} \
      --subst-var-by nvim_web_devicons             ${pkgs.vimPlugins.nvim-web-devicons.rtp} \
      --subst-var-by octo_nvim                     ${pkgs.vimPlugins.octo-nvim.rtp} \
      --subst-var-by open_browser_vim              ${pkgs.vimPlugins.open-browser-vim.rtp} \
      --subst-var-by open_browser_github_vim       ${pkgs.vimPlugins.open-browser-github-vim.rtp} \
      --subst-var-by packer_nvim                   ${pkgs.vimPlugins.packer-nvim.rtp} \
      --subst-var-by plenary_nvim                  ${pkgs.vimPlugins.plenary-nvim.rtp} \
      --subst-var-by project_nvim                  ${pkgs.vimPlugins.project-nvim.rtp} \
      --subst-var-by telescope_nvim                ${pkgs.vimPlugins.telescope-nvim.rtp} \
      --subst-var-by telescope_file_browser_nvim   ${pkgs.vimPlugins.telescope-file-browser-nvim.rtp} \
      --subst-var-by telescope_project_nvim        ${pkgs.vimPlugins.telescope-project-nvim.rtp} \
      --subst-var-by telescope_symbols_nvim        ${pkgs.vimPlugins.telescope-symbols-nvim.rtp} \
      --subst-var-by todo_comments_nvim            ${pkgs.vimPlugins.todo-comments-nvim.rtp} \
      --subst-var-by toggleterm_nvim               ${pkgs.vimPlugins.toggleterm-nvim.rtp} \
      --subst-var-by tokyonight_nvim               ${pkgs.vimPlugins.tokyonight-nvim.rtp} \
      --subst-var-by trouble_nvim                  ${pkgs.vimPlugins.trouble-nvim.rtp} \
      --subst-var-by vim_visual_multi              ${pkgs.vimPlugins.vim-visual-multi.rtp} \
      --subst-var-by which_key_nvim                ${pkgs.vimPlugins.which-key-nvim.rtp}
  '';
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
      "nvim/lua/ginit.lua".source = ginit_lua;
      "nvim/lua/plugins.lua".source = plugins_lua;
      "nvim/lua/config".source = "${configDir}/neovim/lua/config";
    };

    system.activationScripts.neovimClean = let
      xdg = config.home-manager.users.${config.user.name}.xdg;
    in ''
      rm -f ${xdg.cacheHome}/nvim/luacache \
            ${xdg.configHome}/nvim/lua/packer_compiled.lua
    '';

    env = {
      PAGER = mkIf cfg.pager.enable "${pkgs.neovim}/bin/nvim -c PAGER -";
      MANPAGER = mkIf cfg.manpager.enable "${pkgs.neovim}/bin/nvim +'Man!'";
    };

    modules.shell.aliases = { vim = "nvim"; };
  };
}
