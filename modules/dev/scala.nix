{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.scala.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.scala.enable {
    modules.dev.tools.jenv = {
      enable = mkForce true;
      pluginsToEnable = [ "scala" "sbt" "maven" "gradle" ];
    };

    my.packages = with pkgs; [ scala sbt maven gradle scalafmt metals ];
  };
}
