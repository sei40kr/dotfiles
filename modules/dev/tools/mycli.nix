{ config, lib, pkgs, ... }:

with lib; {
  options.modules.dev.tools.mycli.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.tools.mycli.enable {
    my.packages = with pkgs; [ mycli ];
    my.home.home.file.".myclirc".source = <config/mycli/myclirc>;
  };
}
