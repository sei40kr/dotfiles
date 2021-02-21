{ config, lib, pkgs, ... }:

with lib; {
  options.modules.dev.cc.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.cc.enable {
    modules.dev.editors.tools.packages = with pkgs; [ ccls cpplint ];

    # TODO Install clang
    user.packages = with pkgs; [ llvm_10 gcc10 gdb ];
  };
}
