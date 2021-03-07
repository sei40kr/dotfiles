{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.gtk;
in {
  options.modules.desktop.gtk = with types; { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    home-manager.users.${config.user.name}.gtk = {
      enable = true;
      # Disable mouse paste
      gtk3.extraConfig.gtk-enable-primary-paste = false;
    };
  };
}
