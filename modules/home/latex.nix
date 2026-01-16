{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.lang.latex;
in
{
  imports = [
    inputs.self.homeModules.editor-shared
  ];

  options.modules.dev.lang.latex = {
    enable = mkEnableOption "LaTeX development environment";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      texlive.combined.scheme-medium
      texlab
    ];

    modules.editors.lspServers.texlab = rec {
      package = pkgs.texlab;
      command = "${package}/bin/texlab";
      filetypes = [
        "tex"
        "plaintex"
        "bib"
      ];
      rootMarkers = [
        ".latexmkrc"
        ".git"
      ];
    };
  };
}
