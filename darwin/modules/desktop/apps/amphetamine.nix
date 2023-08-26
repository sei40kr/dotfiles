{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.apps.amphetamine;
in
{
  options.modules.desktop.apps.amphetamine = {
    enable = mkBoolOpt false;

    ehnancer.enable = mkBoolOpt true;
  };

  config = mkIf cfg.enable {
    homebrew = {
      enable = true;
      masApps.Amphetamine = 937984704;
    };

    launchd.user.agents = mkIf cfg.ehnancer.enable {
      amphetamine-enhancer-cdmManager = {
        serviceConfig = {
          Label = "com.if.AmphetamineEnhancer-CDMManager";
          ProgramArguments = [ "/bin/sh" "${pkgs.my.amphetamine-enhancer}/Applications/Amphetamine Enhancer.app/Contents/Resources/amphetamine-enhancer-cdmManager.sh" ];
          StartInterval = 10;
        };
      };
      amphetamine-enhancer-allProcesses = {
        serviceConfig = {
          Label = "com.if.AmphetamineHelper-AllProcesses";
          ProgramArguments = [ "/bin/sh" "${pkgs.my.amphetamine-enhancer}/Applications/Amphetamine Enhancer.app/Contents/Resources/amphetamine-enhancer-allProcesses.sh" ];
          StartInterval = 10;
        };
      };
    };
  };
}
