{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf mkEnableOption optionalAttrs;
  cfg = config.modules.desktop.browsers.firefox;
in
{
  options.modules.desktop.browsers.firefox = {
    enable = mkEnableOption "Firefox";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      (makeDesktopItem {
        name = "firefox-private";
        desktopName = "Firefox (Private)";
        genericName = "Web Browser";
        exec = "firefox -P private %U";
        icon = "firefox";
        categories = [
          "Network"
          "WebBrowser"
        ];
        mimeTypes = [
          "text/html"
          "text/xml"
          "application/xhtml+xml"
          "application/vnd.mozilla.xul+xml"
          "x-scheme-handler/http"
          "x-scheme-handler/https"
        ];
        startupNotify = true;
        startupWMClass = "firefox";
        terminal = false;
        type = "Application";
      })
      (makeDesktopItem {
        name = "firefox-work";
        desktopName = "Firefox (Work)";
        genericName = "Web Browser";
        exec = "firefox -P work %U";
        icon = "firefox";
        categories = [
          "Network"
          "WebBrowser"
        ];
        mimeTypes = [
          "text/html"
          "text/xml"
          "application/xhtml+xml"
          "application/vnd.mozilla.xul+xml"
          "x-scheme-handler/http"
          "x-scheme-handler/https"
        ];
        startupNotify = true;
        startupWMClass = "firefox";
        terminal = false;
        type = "Application";
      })
    ];

    programs.firefox = {
      enable = true;
      package = pkgs.firefox.overrideAttrs (oldAttrs: {
        buildCommand =
          (oldAttrs.buildCommand or "")
          + ''
            rm -f $out/share/applications/firefox.desktop
          '';
      });
      policies = {
        ExtensionSettings =
          {
            # DeepL翻訳
            "firefox-extension@deepl.com" = {
              installation_mode = "normal_installed";
              install_url = "https://addons.mozilla.org/firefox/downloads/latest/firefox-extension@deepl.com/latest.xpi";
            };
            # Keepa
            "amptra@keepa.com" = {
              installation_mode = "normal_installed";
              install_url = "https://addons.mozilla.org/firefox/downloads/latest/amptra@keepa.com/latest.xpi";
            };
            # アマゾン注文履歴フィルタ
            "{7cf57bd8-e913-4560-95e1-2e7871fe3632}" = {
              installation_mode = "normal_installed";
              install_url = "https://addons.mozilla.org/firefox/downloads/latest/{7cf57bd8-e913-4560-95e1-2e7871fe3632}/latest.xpi";
            };
          }
          // optionalAttrs config.modules.desktop.apps.bitwarden.enable {
            # Bitwarden Password Manager
            "{446900e4-71c2-419f-a6a7-df9c091e268b}" = {
              installation_mode = "normal_installed";
              install_url = "https://addons.mozilla.org/firefox/downloads/latest/{446900e4-71c2-419f-a6a7-df9c091e268b}/latest.xpi";
            };
          }
          // optionalAttrs config.modules.dev.lang.web.enable {
            # Apollo Client Devtools
            "{a5260852-8d08-4979-8116-38f1129dfd22}" = {
              installation_mode = "normal_installed";
              install_url = "https://addons.mozilla.org/firefox/downloads/latest/{a5260852-8d08-4979-8116-38f1129dfd22}/latest.xpi";
            };
            # Lighthouse
            "{cf3dba12-a848-4f68-8e2d-f9fadc0721de}" = {
              installation_mode = "normal_installed";
              install_url = "https://addons.mozilla.org/firefox/downloads/latest/{cf3dba12-a848-4f68-8e2d-f9fadc0721de}/latest.xpi";
            };
            # React Developer Tools
            "@react-devtools" = {
              installation_mode = "normal_installed";
              install_url = "https://addons.mozilla.org/firefox/downloads/latest/@react-devtools/latest.xpi";
            };
            # Urql Devtools
            "{c11f3a69-f159-4708-b044-853066c2d2fe}" = {
              installation_mode = "normal_installed";
              install_url = "https://addons.mozilla.org/firefox/downloads/latest/{c11f3a69-f159-4708-b044-853066c2d2fe}/latest.xpi";
            };
          };
        SearchEngines = {
          Default = "Google";
        };
      };
      preferences = {
        "browser.urlbar.shortcuts.bookmarks" = false;
        "browser.urlbar.shortcuts.history" = false;
        "browser.urlbar.shortcuts.tabs" = false;
      };
    };
  };
}
