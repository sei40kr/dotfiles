{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.browsers.vivaldi;
in
{
  options.modules.desktop.browsers.vivaldi = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ vivaldi ];
  };
}
