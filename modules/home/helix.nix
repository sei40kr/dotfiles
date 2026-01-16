{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.editors.helix;
in
{
  imports = [ inputs.self.homeModules.editor-shared ];

  options.modules.editors.helix = {
    enable = mkEnableOption "Helix";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ evil-helix ];
  };
}
