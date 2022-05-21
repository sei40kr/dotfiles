{ buildPythonApplication
, appdirs
, beautifulsoup4
, colorlog
, fetchFromGitHub
, lib
, Mako
, online-judge-api-client
, online-judge-tools
, ply
, pyyaml
, requests
, setuptools
, toml
, ...
}:

buildPythonApplication rec {
  pname = "online-judge-template-generator";
  version = "4.8.1";

  src = fetchFromGitHub {
    owner = "online-judge-tools";
    repo = "template-generator";
    rev = "v${version}";
    sha256 = "1a4n3nxli3z6aqzq1r5pi5c1q6lg0p2vg89fmc2z3ndxaq7l8bbi";
  };

  propagatedBuildInputs = [
    appdirs
    beautifulsoup4
    colorlog
    Mako
    online-judge-api-client
    online-judge-tools
    ply
    pyyaml
    requests
    setuptools
    toml
  ];

  # Needs internet to run tests
  doCheck = false;

  meta = with lib; {
    description =
      "Analyze problems of competitive programming and automatically generate boilerplate";
    homepage = "https://github.com/online-judge-tools/template-generator";
    license = licenses.mit;
    maintainers = with maintainers; [ sei40kr ];
  };
}
