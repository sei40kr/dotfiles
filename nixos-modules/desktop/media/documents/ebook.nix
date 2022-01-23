{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.media.documents.ebook;

  catalogs = if config.modules.services.calibre-web.enable then [{
    title = "Calibre-Web (local)";
    uri = "http://localhost:8083/opds";
    preview = "http://localhost:8083/opds/discover";
  }] else
    [ ];
in {
  config = mkIf cfg.enable {
    user.packages = with pkgs; [ foliate ];

    home.dataFile."com.github.johnfactotum.Foliate/catalogs/catalogs.json".text =
      builtins.toJSON { inherit catalogs; };

    modules.desktop.dconf = {
      enable = true;
      settings = {
        "org/gnome/desktop/interface" = { restore-last-file = false; };
      };
    };
  };
}
