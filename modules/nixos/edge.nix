{
  config,
  lib,
  ...
}:

let
  inherit (lib) any attrValues mkIf;
  edgeEnabled = any (user: user.modules.desktop.browsers.edge.enable or false) (
    attrValues (config.home-manager.users or { })
  );
in
{
  config = mkIf edgeEnabled {
    services.earlyoom.extraArgs = [
      "--avoid"
      "^msedge$"
    ];
  };
}
