{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;
  cfg = config.modules.desktop.browsers.vivaldi;
in
{
  options.modules.desktop.browsers.vivaldi = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable { user.packages = with pkgs; [ vivaldi ]; };
}
