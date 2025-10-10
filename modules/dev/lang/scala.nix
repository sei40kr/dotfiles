{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf optionals makeBinPath;
  inherit (lib.my) mkBoolOpt;
  cfg = config.modules.dev.lang.scala;
in
{
  options.modules.dev.lang.scala = {
    enable = mkBoolOpt false;

    bloop.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    # TODO Install scala, sbt completions
    # TODO Install scalastyle
    user.packages =
      with pkgs;
      [
        scala
        sbt
        gradle
        maven
        scalafmt
      ]
      ++ (optionals cfg.bloop.enable [ bloop ]);

    home-manager.users.${config.user.name}.systemd.user.services.bloop = mkIf cfg.bloop.enable {
      Unit.Description = "Bloop Scala build server";
      Service = {
        Type = "simple";
        ExecStart = "${pkgs.bloop}/bin/bloop server";
        Restart = "always";
        Environment = [ "PATH=${makeBinPath [ pkgs.jdk ]}" ];
      };
    };

    modules.editors.lspServers.metals = rec {
      package = pkgs.metals;
      command = "${package}/bin/metals";
      filetypes = [ "scala" ];
      rootMarkers = [
        "build.sbt"
        "build.sc"
        "build.gradle"
        "pom.xml"
      ];
    };
  };
}
