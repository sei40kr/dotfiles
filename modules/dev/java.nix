{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.dev.java;
in {
  options.modules.dev.java = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    javaPackages = mkOption {
      type = with types; attrsOf package;
      default = { };
    };
  };

  config = mkIf cfg.enable {
    modules.dev.tools = {
      jenv = {
        inherit (cfg) javaPackages;

        enable = mkForce true;
      };
      maven.enable = mkForce true;
      gradle.enable = mkForce true;
      springBoot.enable = mkForce true;
    };

    my.packages = with pkgs; [ jdk11 ];
  };
}
