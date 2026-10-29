{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.lang.flutter;
in
{
  options.modules.dev.lang.flutter = {
    enable = mkEnableOption "Flutter development environment";
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.flutter ];

    home.shellAliases = {
      fl = "flutter";
      flattach = "flutter attach";
      flb = "flutter build";
      flchnl = "flutter channel";
      flc = "flutter clean";
      fldvcs = "flutter devices";
      fldoc = "flutter doctor";
      flpub = "flutter pub";
      flget = "flutter pub get";
      flr = "flutter run";
      flrd = "flutter run --debug";
      flrp = "flutter run --profile";
      flrr = "flutter run --release";
      flupgrd = "flutter upgrade";
    };
  };
}
