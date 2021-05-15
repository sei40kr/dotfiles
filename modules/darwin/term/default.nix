{ config, lib, ... }:

with lib;
with lib.my; {
  config = {
    environment.pathsToLink = [ "/share/terminfo" ];

    environment.variables.TERMINFO_DIRS = concatStringsSep ":"
      ((map (p: "${p}/share/terminfo") config.environment.profiles)
        ++ [ "/usr/share/terminfo" ]);
  };
}
