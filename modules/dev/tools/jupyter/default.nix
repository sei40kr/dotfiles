{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    mkIf
    mkOption
    mkEnableOption
    types
    ;
  inherit (config.dotfiles) configDir;
  inherit (pkgs) stdenv;
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
    user.packages = with pkgs; [ python3Packages.notebook ];

    home.file.".jupyter/jupyter_console_config.py".source =
      "${configDir}/jupyter/jupyter_console_config.py";
    home.dataFile."jupyter/kernels".source = "${kernels}/kernels";
  };
}
