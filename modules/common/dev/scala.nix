{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.scala;
in {
  options.modules.dev.scala = {
    enable = mkBoolOpt false;
    bloop.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    # TODO scalastyle
    user.packages = with pkgs;
      ([ scala sbt gradle maven metals scalafmt ]
        ++ optionals cfg.bloop.enable [ bloop ]);

    home-manager.users.${config.user.name}.systemd.user.services.bloop =
      mkIf cfg.bloop.enable {
        Unit.Description = "Bloop Scala build server";
        Service = {
          Type = "simple";
          ExecStart = "${pkgs.bloop}/bin/bloop server";
          Restart = "always";
          Environment = [ "PATH=${makeBinPath [ pkgs.jdk ]}" ];
        };
      };
  };
}
