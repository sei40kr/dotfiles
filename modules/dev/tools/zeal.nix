{ config, lib, pkgs, ... }:

with lib;
let dataHome = config.home-manager.users."${config.my.userName}".xdg.dataHome;
in {
  options.modules.dev.tools.zeal.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.tools.zeal.enable {
    modules.dev.editors.doomEmacs.variables.dash-docs-docsets-path =
      "${dataHome}/Zeal/Zeal/docsets";

    my.packages = with pkgs; [ zeal ];
  };
}
