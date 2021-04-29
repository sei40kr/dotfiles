{ callPackage, fetchFromGitHub }:

let buildGradle = callPackage ./_gradle-env.nix { };
in buildGradle {
  envSpec = ./gradle-env.json;
  pname = "groovy-language-server";
  version = "unstable-20201212";

  src = fetchFromGitHub {
    owner = "prominic";
    repo = "groovy-language-server";
    rev = "adada2bea6df41ff2c490509caa3525f0b2e019c";
    sha256 = "1i2p1jqn9w0hc880xv9z5l729nv5l0571wfsqpqaxj432q24g7yv";
  };

  gradleFlags = [ "build" "-x" "test" ];

  installPhase = ''
    mkdir -p "''${out}/lib"
    cp -r build/libs/*-all.jar "''${out}/lib/groovy-language-server-all.jar"
  '';
}
