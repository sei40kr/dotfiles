{
  config,
  lib,
  perSystem,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.desktop.apps.tinycast;
  app = "${perSystem.self.tinycast}/Applications/Tinycast Beta.app";
in
{
  options.modules.desktop.apps.tinycast = {
    enable = mkEnableOption "Tinycast";
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = pkgs.stdenv.hostPlatform.isDarwin;
        message = "Tinycast is only available on macOS.";
      }
    ];

    home.packages = [ perSystem.self.tinycast ];

    # Tinycast is an LSUIElement agent, so it has to stay resident for its
    # global hotkey to respond.
    launchd.agents.tinycast = {
      enable = true;
      config = {
        ProgramArguments = [ "${app}/Contents/MacOS/Tinycast Beta" ];
        RunAtLoad = true;
        KeepAlive = true;
      };
    };
  };
}
