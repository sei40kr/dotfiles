{
  config,
  lib,
  inputs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;

  cfg = config.modules.desktop.apps.luciole;
in
{
  imports = [ inputs.luciole.homeModules.default ];

  options.modules.desktop.apps.luciole = {
    enable = mkEnableOption "Luciole file manager";
  };

  config = mkIf cfg.enable {
    programs.luciole.enable = true;

    xdg.mimeApps = {
      enable = true;
      defaultApplications."inode/directory" = "dev.noctalia.luciole.desktop";
    };
  };
}
