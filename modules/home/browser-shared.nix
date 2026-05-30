{
  config,
  lib,
  osConfig,
  pkgs,
  ...
}:

let
  inherit (lib)
    attrByPath
    mkIf
    mkOption
    types
    ;
  inherit (types) nullOr str;

  cfg = config.modules.desktop.browsers;

  chromeEnabled = attrByPath [ "modules" "desktop" "browsers" "chrome" "enable" ] false osConfig;

  defaultBrowserCommand =
    if cfg.edge.enable then
      "microsoft-edge"
    else if cfg.firefox.enable then
      "firefox"
    else if chromeEnabled then
      "google-chrome-stable"
    else
      null;
  defaultBrowserDesktopFile =
    if cfg.edge.enable then
      "microsoft-edge.desktop"
    else if cfg.firefox.enable then
      "firefox.desktop"
    else if chromeEnabled then
      "google-chrome.desktop"
    else
      null;
in
{
  imports = [
    ./firefox.nix
    ./edge.nix
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
    home.sessionVariables = {
      BROWSER = defaultBrowserCommand;
    };

    # xdg.mimeApps carries a Linux-only platform assertion in home-manager,
    # so guard it to keep the module evaluable on darwin (BROWSER stays set).
    xdg.mimeApps = mkIf pkgs.stdenv.hostPlatform.isLinux {
      enable = true;
      defaultApplications = {
        "text/html" = defaultBrowserDesktopFile;
        "x-scheme-handler/http" = defaultBrowserDesktopFile;
        "x-scheme-handler/https" = defaultBrowserDesktopFile;
      };
    };
  };
}
