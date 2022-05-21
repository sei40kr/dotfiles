{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.i18n.japanese;
in
{
  config = mkIf cfg.enable {
    i18n.inputMethod = {
      enabled = "fcitx5";
      fcitx5.addons = with pkgs; [ fcitx5-mozc ];
    };
  };
}
