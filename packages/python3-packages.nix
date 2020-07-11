{ config, lib, pkgs, ... }:

with lib; {
  atcoder-tools = pkgs.python3Packages.buildPythonApplication rec {
    pname = "atcoder-tools";
    version = "1.1.7.1";

    src = pkgs.python3Packages.fetchPypi {
      inherit pname version;
      sha256 =
        "cfcee879e87b18f259c488e00553657d577f63dfea1afbf8b01b5401f4346238";
    };

    propagatedBuildInputs = with pkgs.python3Packages; [
      beautifulsoup4
      colorama
      jinja2
      requests
      toml
    ];

    doCheck = false;

    meta = with lib; {
      license = licenses.mit;
      description =
        "Convenient modules & tools for AtCoder users, written in Python 3.5";
    };
  };
}
