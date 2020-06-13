{ config, lib, options, pkgs, ... }:

with lib; {
  options.modules.dev.scala.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.dev.scala.enable {
    my.packages = with pkgs; [ scala sbt maven gradle scalafmt metals ];
  };
}
