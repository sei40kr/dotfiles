{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.editors.neovim.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.editors.neovim.enable {
    my.packages = with pkgs; [ neovim ];

    my.home.home.file.".vim/common.vim".source = <config/vim/common.vim>;
    my.home.xdg.configFile."nvim/init.vim".source = <config/neovim/init.vim>;
    my.zsh.aliases.vim = "nvim";
  };
}
