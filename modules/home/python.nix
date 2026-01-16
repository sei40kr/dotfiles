{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.lang.python;
in
{
  imports = [
    inputs.self.homeModules.editor-shared
    inputs.self.homeModules.zsh
  ];

  options.modules.dev.lang.python = {
    enable = mkEnableOption "Python development environment";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      python3
      uv
      ruff
    ];

    modules.editors.lspServers.basedpyright = rec {
      package = pkgs.basedpyright;
      command = "${package}/bin/basedpyright-langserver";
      args = [ "--stdio" ];
      filetypes = [ "python" ];
      rootMarkers = [
        "pyproject.toml"
        "setup.py"
        "setup.cfg"
        "requirements.txt"
        "Pipfile"
        "pyrightconfig.json"
        ".git"
      ];
    };

    home.sessionPath = [ "$HOME/.poetry/bin" ];

    programs.zsh.oh-my-zsh.plugins = [ "pip" ];
  };
}
