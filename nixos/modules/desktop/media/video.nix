{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.media.video;
in
{
  options.modules.desktop.media.video = {
    totem.enable = mkBoolOpt true;
    trimmer.enable = mkBoolOpt true;
  };

  config = {
    user.packages = with pkgs; [
      (mkIf cfg.totem.enable gnome.totem)
      (mkIf cfg.trimmer.enable my.video-trimmer)
    ];
  };
}
