{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.media.documents.ebook;
in
{
  options.modules.desktop.media.documents.ebook = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ foliate ];

    home.dataFile."com.github.johnfactotum.Foliate/catalogs/catalogs.json".text =
      builtins.toJSON {
        catalogs = [{
          title = "Calibre-Web";
          uri = "https://calibre-web.yong-ju.me/opds";
          preview = "https://calibre-web.yong-ju.me/opds/discover";
        }];
      };

    modules.desktop.dconf = {
      enable = true;
      settings = {
        "com/github/johnfactotum/Foliate" = { restore-last-file = false; };
      };
    };
  };
}
