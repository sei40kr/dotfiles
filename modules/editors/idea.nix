{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
with lib.my;
let
  cfg = config.modules.editors.idea;
in
{
  options.modules.editors.idea = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      (jetbrains.plugins.addPlugins jetbrains.idea-ultimate [
        "acejump"
        "github-copilot"
        "ideavim"
        "python"
        "rust"
      ])
    ];

    modules.editors = {
      fonts.enable = true;
      ideavim.enable = true;
    };
  };
}
