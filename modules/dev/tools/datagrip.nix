{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.tools.datagrip.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.tools.datagrip.enable {
    modules.dev.editors = {
      fonts.enable = mkForce true;
      ideavim.enable = mkForce true;
    };

    my.packages = with pkgs; [ jetbrains.datagrip ];
  };
}
