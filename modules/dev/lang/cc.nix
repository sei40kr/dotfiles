{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
with lib.my;
let
  cfg = config.modules.dev.lang.cc;
in
{
  options.modules.dev.lang.cc = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    # TODO Install clang
    user.packages =
      with pkgs;
      [
        cpplint
      ]
      ++ (optionals pkgs.stdenv.isLinux [
        gcc10
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
