{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf optional;
  cfg = config.modules.dev.tools.azure;
in
{
  options.modules.dev.tools.azure = {
    enable = mkEnableOption "Azure CLI tools";

    kubelogin.enable = mkEnableOption "Azure Kubelogin";
  };

  config = mkIf cfg.enable {
    home.packages =
      with pkgs;
      [
        azure-cli
      ]
      ++ optional cfg.kubelogin.enable kubelogin;
  };
}
