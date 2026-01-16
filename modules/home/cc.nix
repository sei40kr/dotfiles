{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf optionals;
  cfg = config.modules.dev.lang.cc;
in
{
  imports = [
    inputs.self.homeModules.editor-shared
  ];

  options.modules.dev.lang.cc = {
    enable = mkEnableOption "C/C++ development environment";
  };

  config = mkIf cfg.enable {
    # TODO Install clang
    home.packages =
      with pkgs;
      [
        cpplint
      ]
      ++ (optionals pkgs.stdenv.isLinux [
        gcc
        gdb
        llvm
      ]);

    modules.editors.lspServers.ccls = rec {
      package = pkgs.ccls;
      command = "${package}/bin/ccls";
      filetypes = [
        "c"
        "cpp"
        "objc"
        "objcpp"
        "cuda"
      ];
      rootMarkers = [
        "compile_commands.json"
        ".ccls"
        ".git"
      ];
    };
  };
}
