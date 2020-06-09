{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.shell.tools.bat.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.shell.tools.bat.enable {
    my = {
      packages = with pkgs; [ bat ];
      zsh.aliases.cat = "bat";
    };
  };
}
