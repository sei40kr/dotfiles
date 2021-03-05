{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.cc;
in {
  options.modules.dev.cc = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    # TODO Install clang
    user.packages = with pkgs; [ ccls cpplint gcc10 gdb llvm_10 ];
  };
}
