{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.shell.bat.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.shell.bat.enable {
    my = {
      packages = with pkgs; [ bat ];
      zsh.aliases.cat = "bat";
    };
  };
}
