{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;
  cfg = config.modules.desktop.media.documents.ebook;
in
{
  options.modules.desktop.media.documents.ebook = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ foliate ];

    home.dataFile."com.github.johnfactotum.Foliate/catalogs/catalogs.json".text = builtins.toJSON {
      catalogs = [
        {
          title = "Calibre-Web";
          uri = "https://calibre.yong-ju.me/opds";
          preview = "https://calibre.yong-ju.me/opds/new";
        }
        {
          title = "Gihyo Digital Publishing";
          uri = "https://gihyo.jp/dp/catalogs.opds";
          preview = "https://gihyo.jp/dp/new.opds";
        }
        {
          title = "O'Reilly Japan Ebook Store";
          uri = "https://www.oreilly.co.jp/ebook/catalogs.opds";
          preview = "https://www.oreilly.co.jp/ebook/new.opds";
        }
      ];
    };

    modules.desktop.dconf = {
      enable = true;
      settings = {
        "com/github/johnfactotum/Foliate" = {
          restore-last-file = false;
        };
      };
    };
  };
}
