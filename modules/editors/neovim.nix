{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  cfg = config.modules.editors.neovim;

  toml = (pkgs.formats.toml { }).generate;
  dein_toml = toml "dein.toml" (import "${configDir}/neovim/dein_toml.nix" {
    inherit (pkgs) vimPlugins;
    vimPlugins' = pkgs.my.vimPlugins;
  });
  dein_lazy_toml = toml "dein_lazy.toml"
    (import "${configDir}/neovim/dein_lazy_toml.nix" {
      inherit (pkgs) vimPlugins;
      vimPlugins' = pkgs.my.vimPlugins;
    });
in {
  options.modules.editors.neovim = with types; {
    enable = mkBoolOpt false;

    pager.enable = mkBoolOpt false;

    manpager.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ neovim ];

    home.configFile = {
      "nvim/init.lua".source = "${configDir}/neovim/init.lua";
      "nvim/lua/dein.lua".text = ''
        local config_dir = vim.call('stdpath', 'config')
        local dein_cache_dir = (vim.call('stdpath', 'cache')) .. '/dein'

        vim.opt.runtimepath:append { '${pkgs.my.vimPlugins.dein-vim.rtp}' }

        if vim.call('dein#load_state', dein_cache_dir) == 1 then
          vim.call('dein#begin', dein_cache_dir)

          vim.call('dein#load_toml', config_dir .. '/dein.toml')
          vim.call('dein#load_toml', config_dir .. '/dein_lazy.toml', { lazy = 1 })

          vim.call('dein#end')
          vim.call('dein#save_state')
        end

        vim.cmd([[
          augroup dein
            autocmd!
            autocmd VimEnter * call dein#call_hook('post_source')
          augroup END
        ]])
      '';
      "nvim/dein.toml".source = dein_toml;
      "nvim/dein_lazy.toml".source = dein_lazy_toml;
      "nvim/doom.vim".source = "${configDir}/neovim/doom.vim";
    };

    system.userActivationScripts.neovim = ''
      : ''${XDG_CACHE_HOME:=''${HOME}/.cache}

      rm -f ''${XDG_CACHE_HOME}/nvim/dein/state_nvim.vim \
            ''${XDG_CACHE_HOME}/nvim/dein/cache_nvim
    '';

    env = {
      PAGER = mkIf cfg.pager.enable "${pkgs.neovim}/bin/nvim -c PAGER -";
      MANPAGER =
        mkIf cfg.manpager.enable "${pkgs.neovim}/bin/nvim -c MANPAGER -";
    };

    modules.shell.aliases = { vim = "nvim"; };
  };
}
