{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
with lib.my;
let
  cfg = config.modules.editors.datagrip;
in
{
  options.modules.editors.datagrip = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      (jetbrains.plugins.addPlugins jetbrains.datagrip [
        "acejump"
        "github-copilot"
        "ideavim"
      ])
    ];

    modules.editors = {
      fonts.enable = true;
      ideavim.enable = true;
    };
  };
}
