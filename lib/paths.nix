{ self, lib, ... }:

with builtins;
with lib;
with lib.my; rec {
  dotFilesDir = toString ../.;
  modulesDir = "${dotFilesDir}/modules";
  configDir = "${dotFilesDir}/config";
}
