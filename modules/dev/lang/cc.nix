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
        ccls
        cpplint
      ]
      ++ (optionals pkgs.stdenv.isLinux [
        gcc10
        gdb
        llvm
      ]);
  };
}
