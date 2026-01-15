{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.tools.ansible;
in
{
  options.modules.dev.tools.ansible = {
    enable = mkEnableOption "Ansible development environment";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      ansible
      ansible-lint
    ];
  };
}
