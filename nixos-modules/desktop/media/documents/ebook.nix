{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.media.documents.ebook;

  catalogs = if config.modules.services.calibre-web.enable then [{
    title = "Calibre-Web (local)";
    uri = "http://localhost:8083/opds";
    preview = "http://localhost:8083/opds/discover";
  }] else [{
    title = "Calibre-Web";
    uri = "https://calibre-web.yong-ju.me/opds";
    preview = "https://calibre-web.yong-ju.me/opds/discover";
  }];
in {
  config = mkIf cfg.enable {
    user.packages = with pkgs; [ foliate ];

    home.dataFile."com.github.johnfactotum.Foliate/catalogs/catalogs.json".text =
      builtins.toJSON { inherit catalogs; };

    modules.desktop.dconf = {
      enable = true;
      settings = {
        "com/github/johnfactotum/Foliate" = { restore-last-file = false; };
      };
    };
  };
}
