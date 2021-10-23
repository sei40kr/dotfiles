{ config, inputs, lib, options, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) dir;
  inherit (inputs) agenix;
  secretsDir = "${dir}/secrets";
  secretsFile = "${secretsDir}/secrets.nix";
in {
  imports = [ agenix.nixosModules.age ];
  environment.systemPackages = [ agenix.defaultPackage.x86_64-linux ];

  age = {
    secrets = if pathExists secretsFile then
      mapAttrs' (name: _:
        nameValuePair (removeSuffix ".age" name) {
          file = "${secretsDir}/${name}";
          owner = mkDefault config.user.name;
        }) (import secretsFile)
    else
      { };
    sshKeyPaths = options.age.sshKeyPaths.default ++ (filter pathExists [
      "${config.user.home}/.ssh/id_ecdsa"
      "${config.user.home}/.ssh/id_rsa"
    ]);
  };
}
