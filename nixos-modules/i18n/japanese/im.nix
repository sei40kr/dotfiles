{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.i18n.japanese;
in
{
  config = mkIf cfg.enable {
    i18n.inputMethod = {
      enabled = "ibus";
      ibus = {
        engines = with pkgs.ibus-engines; [ mozc ];
        panel = "${pkgs.plasma5Packages.plasma-desktop}/lib/libexec/kimpanel-ibus-panel";
      };
    };
  };
}
