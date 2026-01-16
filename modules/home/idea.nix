{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.editors.idea;
in
{
  imports = [
    inputs.self.homeModules.editor-shared
    inputs.self.homeModules.ideavim
  ];

  options.modules.editors.idea = {
    enable = mkEnableOption "IntelliJ IDEA";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      (jetbrains.plugins.addPlugins jetbrains.idea [
        "acejump"
        # FIXME: NixOS/nixpkgs#400317 github-copilot fails to build
        # "github-copilot"
        "ideavim"
        "python"
        "rust"
      ])
    ];

    modules.editors.ideavim.enable = true;
  };
}
