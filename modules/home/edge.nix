{
  config,
  lib,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf optionals;
  cfg = config.modules.desktop.browsers.edge;
in
{
  options.modules.desktop.browsers.edge = {
    enable = mkEnableOption "Microsoft Edge";
  };

  config = mkIf cfg.enable {
    programs.microsoft-edge = {
      enable = true;
      extensions = [
        # DeepL Translate
        { id = "cofdbpoegempjloogbagkncekinflcnj"; }
        # Keepa - Amazon Price Tracker
        { id = "neebplgakaahbhdphmkckjjcegoiijjo"; }
        # YouTube to NotebookLM
        { id = "kobncfkmjelbefaoohoblamnbackjggk"; }
        # アマゾン注文履歴フィルタ
        { id = "jaikhcpoplnhinlglnkmihfdlbamhgig"; }
        # はてなブックマーク
        { id = "baniobjofkeeahdkdnpanannchdgblni"; }
        # Bitwarden Password Manager
        { id = "nngceckbapebfimnlniiiahkandclblb"; }
      ]
      ++ optionals config.modules.dev.lang.web.enable [
        # Apollo Client Devtools
        { id = "jdkknkkbebbapilgoeccciglkfbmbnfm"; }
        # Lighthouse
        { id = "blipmdconlkpinefehnmjammfjpmpbjk"; }
        # React Developer Tools
        { id = "fmkadmapgofadopljbjfkapdkoienihi"; }
        # Urql Devtools
        { id = "jdbinjkmfkbalmhjlbjkfmkmlhpmgcmj"; }
      ];
    };
  };
}
