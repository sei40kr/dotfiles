{ config, lib, pkgs, ... }:

with lib;
let dataHome = config.home-manager.users.${config.user.name}.xdg.dataHome;
in {
  options.modules.dev.tools.zeal.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.tools.zeal.enable {
    modules.dev.editors.doomEmacs.variables.dash-docs-docsets-path =
      "${dataHome}/Zeal/Zeal/docsets";

    user.packages = with pkgs; [ zeal ];
  };
}
