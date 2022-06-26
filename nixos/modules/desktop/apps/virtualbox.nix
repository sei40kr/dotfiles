{ config, lib, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.virtualbox;
in
{
  options.modules.desktop.apps.virtualbox = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    virtualisation.virtualbox.host = {
      enable = true;
      enableExtensionPack = true;
    };

    user.extraGroups = [ "vboxusers" ];
  };
}
