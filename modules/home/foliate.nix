{
  config,
  lib,
  ...
}:

let
  inherit (builtins) toJSON;
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.desktop.media.foliate;
in
{
  options.modules.desktop.media.foliate = {
    enable = mkEnableOption "Foliate";
  };

  config = mkIf cfg.enable {
    programs.foliate = {
      enable = true;
      settings = {
        restore-last-file = false;
      };
    };

    xdg.dataFile."com.github.johnfactotum.Foliate/catalogs/catalogs.json".text = toJSON {
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
  };
}
