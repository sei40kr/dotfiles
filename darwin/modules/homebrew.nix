{ config, lib, ... }:

with lib;
with lib.my;
let
  prefix = "/opt/homebrew";
in
{
  config = mkIf config.homebrew.enable {
    homebrew.onActivation.cleanup = "zap";

    home-manager.users.${config.user.name}.home = {
      sessionVariables = {
        HOMEBREW_PREFIX = prefix;
        HOMEBREW_CELLAR = "${prefix}/Cellar";
        HOMEBREW_REPOSITORY = prefix;
      };
      sessionPath = [ "${prefix}/bin" "${prefix}/sbin" ];
      sessionVariablesExtra = ''
        export MANPATH="/opt/homebrew/share/man''${MANPATH+:$MANPATH}:";
        export INFOPATH="/opt/homebrew/share/info:''${INFOPATH:-}";
      '';
    };
  };
}

