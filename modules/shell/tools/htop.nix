{ config, lib, pkgs, ... }:

with lib; {
  options.modules.shell.tools.htop.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.shell.tools.htop.enable {
    my = {
      packages = with pkgs; [ htop ];
      zsh.aliases.top = "htop";
    };
  };
}
