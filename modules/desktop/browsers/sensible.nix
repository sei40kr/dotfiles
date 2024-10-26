{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  browsersCfg = config.modules.desktop.browsers;
  cfg = browsersCfg.sensible;

  sensible-browser = pkgs.writeShellScriptBin "sensible-browser" (
    if browsersCfg.chrome.enable then
      ''exec google-chrome-stable "$@"''
    else if browsersCfg.firefox.enable then
      ''exec firefox "$@"''
    else
      abort "sensible-browser: no browser enabled"
  );
in
{
  options.modules.desktop.browsers.sensible = {
    enable = mkEnableOption "sensible-browser";
  };

  config = mkIf cfg.enable { environment.systemPackages = [ sensible-browser ]; };
}
