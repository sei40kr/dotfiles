{ self, lib, ... }:

with builtins;
with lib; rec {
  dotFilesDir = toString ../.;
  modulesDir = "${dotFilesDir}/modules";
  configDir = "${dotFilesDir}/config";
  homeDir = "/home/${
      let user = getEnv "USER";
      in if elem user [ "" "root" ] then "sei40kr" else user
    }";
}
