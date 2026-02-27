{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.desktop.apps.raycast;
in
{
  options.modules.desktop.apps.raycast = {
    enable = mkEnableOption "Raycast";
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = pkgs.stdenv.isDarwin;
        message = "Raycast is only available on macOS.";
      }
    ];

    home.packages = lib.optionals pkgs.stdenv.isDarwin [ pkgs.raycast ];
  };
}
