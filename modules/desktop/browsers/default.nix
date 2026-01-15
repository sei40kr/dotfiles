{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf mkOption types;
  inherit (types) nullOr str;

  cfg = config.modules.desktop.browsers;

  sensible-browser = pkgs.writeShellScriptBin "sensible-browser" ''
    exec ${defaultBrowserCommand} "$@"
  '';

  defaultBrowserCommand =
    if cfg.firefox.enable then
      "firefox"
    else if cfg.chrome.enable then
      "google-chrome-stable"
    else
      null;
  defaultBrowserDesktopFile =
    if cfg.firefox.enable then
      "firefox.desktop"
    else if cfg.chrome.enable then
      "google-chrome.desktop"
    else
      null;
in
{
  imports = [
    ./chrome.nix
    ./firefox.nix
  ];

  options.modules.desktop.browsers = {
    defaultBrowser = mkOption {
      type = nullOr str;
      readOnly = true;
      default = defaultBrowserCommand;
      description = "The default browser to use, automatically determined by enabled browser modules";
    };
  };

  config = mkIf (cfg.defaultBrowser != null) {
    environment.systemPackages = [ sensible-browser ];

    environment.variables = {
      BROWSER = "sensible-browser";
    };

    xdg.mime.defaultApplications = {
      "text/html" = defaultBrowserDesktopFile;
      "x-scheme-handler/http" = defaultBrowserDesktopFile;
      "x-scheme-handler/https" = defaultBrowserDesktopFile;
    };
  };
}
