{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.browsers.chrome;
in
{
  options.modules.desktop.browsers.chrome = with types; {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = [
      (pkgs.google-chrome.override {
        # Use GTK4 to get IM support on Wayland
        # https://fcitx-im.org/wiki/Using_Fcitx_5_on_Wayland
        commandLineArgs = "--gtk-version=4";
      })
    ];

    environment.etc."opt/chrome/policies/managed/default.json".text = builtins.toJSON {
      ExtensionInstallForcelist = [
        "cofdbpoegempjloogbagkncekinflcnj" # DeepL翻訳
        "neebplgakaahbhdphmkckjjcegoiijjo" # Keepa
        "difoiogjjojoaoomphldepapgpbgkhkb" # Sider
        "jaikhcpoplnhinlglnkmihfdlbamhgig" # アマゾン注文履歴フィルタ
      ] ++ optionals config.modules.desktop.apps.bitwarden.enable [
        "nngceckbapebfimnlniiiahkandclblb" # Bitwarden
      ] ++ optionals config.modules.dev.lang.web.enable [
        "pbjjkligggfmakdaogkfomddhfmpjeni" # Accessibility Insights for Web
        "blipmdconlkpinefehnmjammfjpmpbjk" # Lighthouse
        "fmkadmapgofadopljbjfkapdkoienihi" # React Developer Tools
        "lmhkpmbekcpmknklioeibfkpmmfibljd" # Redux DevTools
      ];
    };
  };
}
