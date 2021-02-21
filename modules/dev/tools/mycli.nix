{ config, lib, pkgs, ... }:

with lib;
with lib.my; {
  options.modules.dev.tools.mycli.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.tools.mycli.enable {
    user.packages = with pkgs; [ mycli ];
    home.file.".myclirc".source = "${configDir}/mycli/myclirc";
  };
}
