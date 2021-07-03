{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.transmission;
in {
  options.modules.services.transmission.enable = mkBoolOpt false;

  config.services.transmission = {
    enable = cfg.enable;
    settings = {
      script-torrent-done-enabled = false;
      script-torrent-done-filename =
        "${pkgs.libcanberra-gtk3}/bin/canberra-gtk-play -i complete-download -d 'transmission torrent downloaded'";
      show-filterbar = false;
      show-statusbar = false;
    };
    openFirewall = true;
  };
}
