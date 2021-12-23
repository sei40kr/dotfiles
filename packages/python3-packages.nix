{ callPackage, lib, python3Packages, ... }:

with lib; rec {
  importlab = callPackage ./python-modules/importlab { };

  online-judge-api-client =
    callPackage ./python-modules/online-judge-api-client { };

  online-judge-tools = callPackage ./python-modules/online-judge-tools {
    inherit online-judge-api-client;
  };
}
