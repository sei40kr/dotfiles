{ lib, pkgs, python3Packages, ... }:

with lib; {
  atcoder-tools = python3Packages.buildPythonApplication rec {
    pname = "atcoder-tools";
    version = "1.1.7.1";

    src = python3Packages.fetchPypi {
      inherit pname version;
      sha256 =
        "cfcee879e87b18f259c488e00553657d577f63dfea1afbf8b01b5401f4346238";
    };

    propagatedBuildInputs = with python3Packages; [
      beautifulsoup4
      colorama
      jinja2
      requests
      toml
    ];

    doCheck = false;

    meta = {
      license = licenses.mit;
      description =
        "Convenient modules & tools for AtCoder users, written in Python 3.5";
    };
  };

  protonvpn-cli = python3Packages.buildPythonApplication rec {
    pname = "protonvpn-cli";
    version = "2.2.4";

    src = python3Packages.fetchPypi {
      inherit version;
      pname = "protonvpn_cli";
      sha256 =
        "e018976f38a666b9f7318b3c63bbefb5cf97db94e7963ef6e4abd3c3acd54b89";
    };

    propagatedBuildInputs = with python3Packages; [
      docopt
      jinja2
      pythondialog
      requests
    ];

    doCheck = false;

    meta = {
      license = licenses.gpl3;
      description =
        "Linux command-line client for ProtonVPN. Written in Python.";
    };
  };
}
