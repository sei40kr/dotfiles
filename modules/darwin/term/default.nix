{ config, lib, ... }:

with lib;
with lib.my;
let
  terminfoDirs = (map (p: "${p}/share/terminfo") config.environment.profiles)
    ++ [ "/usr/share/terminfo" ];
in {
  config = {
    environment = {
      pathsToLink = [ "/share/terminfo" ];

      etc.terminfo.source = "${config.system.path}/share/terminfo";

      variables.TERMINFO_DIRS = concatStringsSep ":" terminfoDirs;
    };
  };
}
