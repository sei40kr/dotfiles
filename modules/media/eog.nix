{ config, lib, pkgs, ... }:

with lib; {
  options.modules.media.eog.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.media.eog.enable {
    modules.desktop.backends.gsettingsDesktopSchemas = {
      enable = mkForce true;
      packages = with pkgs; [ gnome3.eog ];
    };

    my.packages = with pkgs; [ gnome3.eog ];
  };
}
