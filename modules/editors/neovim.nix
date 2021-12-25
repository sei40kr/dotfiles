{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  cfg = config.modules.editors.neovim;

  toml = (pkgs.formats.toml { }).generate;
  dein_toml = toml "dein.toml"
    (import "${configDir}/vim/dein_toml.nix" { inherit (pkgs) vimPlugins; });
  dein_lazy_toml = toml "dein_lazy.toml"
    (import "${configDir}/vim/dein_lazy_toml.nix" {
      inherit (pkgs) vimPlugins;
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
      "nvim/init.vim".text = ''
        let s:xdg_cache_home = !empty($XDG_CACHE_HOME) ? $XDG_CACHE_HOME : expand('~/.cache')
        let s:xdg_config_home = !empty($XDG_CONFIG_HOME) ? $XDG_CONFIG_HOME : expand('~/.config')

        set runtimepath+=${pkgs.my.vimPlugins.dein-vim.rtp}

        let s:dein_cache_dir = s:xdg_cache_home . '/dein'

        if dein#load_state(s:dein_cache_dir)
          call dein#begin(s:dein_cache_dir)

          call dein#load_toml(s:xdg_config_home . '/nvim/dein.toml', {'lazy': v:false})
          call dein#load_toml(s:xdg_config_home . '/nvim/dein_lazy.toml', {'lazy': v:true})

          call dein#end()
          call dein#save_state()
        endif

        autocmd VimEnter * call dein#call_hook('post_source')

        let g:mapleader = "\<Space>"
        exec 'source ' . s:xdg_config_home . '/nvim/common.vim'

        set ignorecase
        set smartcase
        set wrapscan

        " Use 24-bit (true-color) mode in Vim/Neovim
        if (has("termguicolors"))
          set termguicolors
        endif

        filetype plugin indent on
      '';
      "nvim/common.vim".source = "${configDir}/vim/common.vim";
      "nvim/dein.toml".source = dein_toml;
      "nvim/dein_lazy.toml".source = dein_lazy_toml;
    };

    system.userActivationScripts.dein-vim = ''
      : ''${XDG_CACHE_HOME:=''${HOME}/.cache}
      ( shopt -s nullglob; rm -f ''${XDG_CACHE_HOME}/dein/state_*.vim ''${XDG_CACHE_HOME}/dein/cache_* )
    '';

    env = {
      PAGER = mkIf cfg.pager.enable "${pkgs.neovim}/bin/nvim -c PAGER -";
      MANPAGER =
        mkIf cfg.manpager.enable "${pkgs.neovim}/bin/nvim -c MANPAGER -";
    };

    modules.shell.aliases = { vim = "nvim"; };
  };
}
