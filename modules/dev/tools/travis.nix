{ config, lib, pkgs, ... }:

with lib; {
  options.modules.dev.tools.travis.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.tools.travis.enable {
    my.packages = with pkgs; [ travis ];

    modules.shell.zsh.zinitPluginsInit = ''
      zinit ice if'[[ -f "''${HOME}/.travis/travis.sh" ]]' wait'''
      zinit snippet "''${HOME}/.travis/travis.sh"
    '';
  };
}
