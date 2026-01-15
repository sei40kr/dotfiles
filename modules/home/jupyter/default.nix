{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    mkEnableOption
    mkIf
    mkOption
    types
    ;
  cfg = config.modules.dev.tools.jupyter;

  kernels = pkgs.jupyter-kernel.create { definitions = cfg.kernels; };
in
{
  imports = [
    ./IRkernel.nix
    ./ijulia.nix
    ./ipykernel.nix
  ];

  options.modules.dev.tools.jupyter = {
    enable = mkEnableOption "Jupyter";

    kernels = mkOption {
      type = types.attrs;
      default = { };
      visible = false;
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ python3Packages.notebook ];

    home.file.".jupyter/jupyter_console_config.py".source = ./jupyter_console_config.py;
    xdg.dataFile."jupyter/kernels".source = "${kernels}/kernels";
  };
}
