{ appdirs, beautifulsoup4, buildPythonPackage, colorlog, fetchFromGitHub, git
, jsonschema, lib, lxml, markdown, python, requests, substituteAll, toml }:

let
  pythonEnv =
    python.withPackages (ps: with ps; [ colorlog jinja2 markdown toml ]);
in buildPythonPackage rec {
  pname = "online-judge-api-client";
  version = "10.10.0";

  src = fetchFromGitHub {
    owner = "online-judge-tools";
    repo = "api-client";
    rev = "v${version}";
    sha256 = "0lmryqi0bv82v9k9kf1rzzq9zr83smpmy8ivzw4fk31hvpczp4fn";
  };

  patches = [ ./fix-paths.patch ];
  postPatch = ''
    substituteInPlace onlinejudge/service/library_checker.py \
      --replace @pythonInterpreter@ ${pythonEnv.interpreter}
  '';

  propagatedBuildInputs =
    [ appdirs beautifulsoup4 colorlog jsonschema lxml requests toml ];

  # Requires internet access
  doCheck = false;

  pythonImportsCheck = [ "onlinejudge" "onlinejudge_api" ];

  meta = with lib; {
    description = "Tools for various online judges. Download sample cases, generat additional test cases, test your code, and submit it.";
    homepage = "https://github.com/online-judge-tools/api-client";
    license = licenses.mit;
    maintainers = with maintainers; [ sei40kr ];
  };
}
