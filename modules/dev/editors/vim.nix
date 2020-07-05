{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.editors.vim.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.editors.vim.enable {
    my.packages = with pkgs; [ vim ];

    my.home.home.file = {
      ".vim/common.vim".source = <config/vim/common.vim>;
      ".vim/vimrc".source = <config/vim/vimrc>;
    };
    my.env = { VIMINIT = "source ~/.vim/vimrc"; };
  };
}
