{ fetchFromGitHub, jdk, lib, stdenv, javaPackages ? { }, plugins ? [ ], ... }:

with lib;
stdenv.mkDerivation {
  pname = "jenv";
  version = "0.5.4";

  src = fetchFromGitHub {
    owner = "jenv";
    repo = "jenv";
    rev = "a6ad42b6a97e469b45fd4000971f1886d22ffc04";
    sha256 = "13b339ak2pmad7hlf9im8rv0rgyr8a207jh7vq1nnj8pf8hb5gfd";
  };

  dontBuild = true;

  installPhase = ''
    mkdir -p "''${out}/share/jenv"
    cp -r . "''${out}/share/jenv"

    mkdir -p "''${out}/share/jenv/plugins"
    for plugin in ${escapeShellArgs plugins}; do
      ln -s "''${out}/share/jenv/available-plugins/''${plugin}" \
            "''${out}/share/jenv/plugins/''${plugin}"
    done

    mkdir -p "''${out}/share/jenv/versions"
    declare -A versions
    versions=(
      ${
        concatStringsSep "\n"
        (mapAttrsToList (k: v: "[${escapeShellArg k}]=${escapeShellArg "${v}"}")
          javaPackages)
      }
    )
    for alias in "''${!versions[@]}"; do
      ln -s "''${versions[$alias]}" "''${out}/share/jenv/versions/''${alias}"
    done
    echo 'system' >"''${out}/share/jenv/version"

    function complete() {
      :
    }

    export JENV_ROOT="''${out}/share/jenv"
    export PATH="''${JENV_ROOT}/bin:''${PATH}"
    eval "$(jenv init - --no-rehash)"

    jenv rehash
  '';

  meta = {
    description = "Manage your Java environment";
    homepage = "http://www.jenv.be";
    license = licenses.mit;
    platforms = platforms.all;
  };
}
