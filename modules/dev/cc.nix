{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.cc.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.cc.enable {
    # TODO Install clang
    my.packages = with pkgs; [ llvm_10 gcc10 gdb ];
  };
}
