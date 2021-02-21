{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.dev.editors.neovim;
  dein-vim = pkgs.my.vimPlugins.dein-vim;
in {
  options.modules.dev.editors.neovim = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    enableVimPager = mkOption {
      type = types.bool;
      default = false;
    };

    enableVimManpager = mkOption {
      type = types.bool;
      default = true;
    };
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ neovim dein-vim ];

    home.file = {
      ".vim/common.vim".source = "${configDir}/vim/common.vim";
      ".vim/dein.toml".source = "${configDir}/vim/dein.toml";
      ".vim/dein_lazy.toml".source = "${configDir}/vim/dein_lazy.toml";
    };
    home.configFile."nvim/init.vim".text = ''
      let g:mapleader = "\<Space>"

      source ~/.vim/common.vim

      set clipboard+=unnamedplus
      set ignorecase
      set number
      set relativenumber
      set smartcase
      set wrapscan

      set runtimepath+=${dein-vim}/share/vim-plugins/dein-vim

      if dein#load_state('~/.cache/dein')
        call dein#begin('~/.cache/dein')

        call dein#load_toml('~/.vim/dein.toml', {'lazy': 0})
        call dein#load_toml('~/.vim/dein_lazy.toml', {'lazy': 1})

        call dein#end()
        call dein#save_state()
      endif
      if dein#check_install()
        call dein#install()
      endif

      " Use 24-bit (true-color) mode in Vim/Neovim
      if (has("termguicolors"))
        set termguicolors
      endif

      filetype plugin indent on
    '';
    env.PAGER = mkIf cfg.enableVimPager "${pkgs.neovim}/bin/nvim -c PAGER -";
    env.MANPAGER =
      mkIf cfg.enableVimManpager "${pkgs.neovim}/bin/nvim -c MANPAGER -";
    modules.shell.zsh.aliases.vim = "nvim";
  };
}
