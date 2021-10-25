{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  cfg = config.modules.desktop.mako;
in {
  options.modules.desktop.mako = with types; { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    assertions = [{
      assertion = config.modules.desktop.sway.enable;
      message =
        "The mako module requires 'modules.desktop.sway.enable = true'.";
    }];

    user.packages = with pkgs; [ mako ];

    home.configFile."mako/config".source = "${configDir}/mako/config";

    environment.etc."sway/config.d/startup/mako.conf".text = ''
      exec ${pkgs.mako}/bin/mako
    '';
  };
}
