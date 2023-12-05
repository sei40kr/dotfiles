{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.lang.java;
in
{
  options.modules.dev.lang.java = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ jdk11 gradle maven ];

    modules.shell.aliases.mvnag = "mvn archetype:generate";
  };
}
