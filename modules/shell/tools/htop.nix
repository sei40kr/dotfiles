{ config, lib, pkgs, ... }:

with lib; {
  options.modules.shell.tools.htop.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.shell.tools.htop.enable {
    user.packages = with pkgs; [ htop ];
    modules.shell.aliases.top = "htop";
  };
}
