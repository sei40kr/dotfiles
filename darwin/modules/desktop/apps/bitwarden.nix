{ config, lib, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.bitwarden;
in
{
  options.modules.desktop.apps.bitwarden = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    homebrew = {
      enable = true;
      casks = [ "bitwarden" ];
    };
  };
}
