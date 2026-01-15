{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.desktop.browsers.chrome;
in
{
  options.modules.desktop.browsers.chrome = {
    enable = mkEnableOption "Google Chrome";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.google-chrome ];

    environment.etc."opt/chrome/policies/managed/default.json".text = builtins.toJSON {
      ExtensionInstallForcelist = [
        "cofdbpoegempjloogbagkncekinflcnj" # DeepL翻訳
        "neebplgakaahbhdphmkckjjcegoiijjo" # Keepa
        "jaikhcpoplnhinlglnkmihfdlbamhgig" # アマゾン注文履歴フィルタ
        "nngceckbapebfimnlniiiahkandclblb" # Bitwarden
        "pbjjkligggfmakdaogkfomddhfmpjeni" # Accessibility Insights for Web
        "blipmdconlkpinefehnmjammfjpmpbjk" # Lighthouse
        "fmkadmapgofadopljbjfkapdkoienihi" # React Developer Tools
        "lmhkpmbekcpmknklioeibfkpmmfibljd" # Redux DevTools
      ];
    };
  };
}
