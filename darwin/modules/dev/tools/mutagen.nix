{ config, lib, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.tools.mutagen;
in
{
  config = mkIf cfg.enable {
    homebrew = {
      enable = true;
      taps = [ "mutagen-io/mutagen" ];
      brews = [
        "mutagen"
        (mkIf cfg.compose.enable "mutagen-compose")
      ];
    };
  };
}
