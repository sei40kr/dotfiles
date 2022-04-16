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
      --subst-var-by impatient_nvim  ${pkgs.vimPlugins.impatient-nvim.rtp} \
      --subst-var-by nvim_treesitter ${nvim-treesitter.rtp} \
      --subst-var-by nvim_ts_rainbow ${pkgs.vimPlugins.nvim-ts-rainbow.rtp} \
      --subst-var-by packer_nvim     ${pkgs.vimPlugins.packer-nvim.rtp} \
      --subst-var-by which_key_nvim  ${pkgs.vimPlugins.which-key-nvim.rtp}
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

    home.configFile = {
      "nvim/init.lua".source = "${configDir}/neovim/init.lua";
      "nvim/ginit.vim".text = ''
        lua require("ginit")
      '';
      "nvim/lua/ginit.lua".source = ginit_lua;
      "nvim/lua/plugins.lua".source = plugins_lua;
      "nvim/lua/config".source = "${configDir}/neovim/lua/config";
    };
    home.dataFile = {
      "nvim/site/pack/packer/start/impatient.nvim".source =
        pkgs.vimPlugins.impatient-nvim;
      "nvim/site/pack/packer/opt/packer.nvim".source =
        pkgs.vimPlugins.packer-nvim.rtp;
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
