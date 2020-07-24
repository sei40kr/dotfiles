{ config, lib, options, pkgs, ... }:

with lib;
let dein-vim = pkgs.my.vimPlugins.dein-vim;
in {
  options.modules.dev.editors.neovim.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.editors.neovim.enable {
    my.packages = with pkgs; [ neovim dein-vim ];

    my.home.home.file = {
      ".vim/common.vim".source = <config/vim/common.vim>;
      ".vim/dein.toml".source = <config/vim/dein.toml>;
      ".vim/dein_lazy.toml".source = <config/vim/dein_lazy.toml>;
    };
    my.home.xdg.configFile."nvim/init.vim".text = ''
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

      filetype plugin indent on
      syntax enable
    '';
    my.zsh.aliases.vim = "nvim";
  };
}
