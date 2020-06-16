{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.media.eog.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.media.eog.enable {
    my.packages = with pkgs; [ gnome3.eog ];
  };
}
