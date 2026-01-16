{
  lib,
  config,
  inputs,
  pkgs,
  ...
}:

let
  inherit (builtins) readFile;
  inherit (lib) mkIf;

  # FIXME: We should use the Julia from Nixpkgs stable release, but somehow it
  #  fails to build the IJulia package. So we use the unstable version for now.
  juliaEnv = pkgs.julia-stable-bin.withPackages [
    "IJulia"
    "Plots"
  ];
  # Run a command to get the package directory of IJulia
  ijulia = readFile (
    pkgs.runCommand "${juliaEnv.name}-ijulia-pkgdir" { buildInputs = [ juliaEnv ]; } ''
      ${juliaEnv}/bin/julia -e 'using IJulia; print(pkgdir(IJulia))' >$out
    ''
  );
in
{
  imports = [
    inputs.self.homeModules.julia
  ];

  config = mkIf config.modules.dev.lang.julia.enable {
    modules.dev.tools.jupyter.kernels.ijulia = {
      displayName = "Julia ${juliaEnv.julia.version}";
      argv = [
        "${juliaEnv}/bin/julia"
        "-i"
        "--color=yes"
        "${ijulia}/src/kernel.jl"
        "{connection_file}"
      ];
      language = "julia";
      interruptMode = "signal";
      logo32 = "${ijulia}/deps/logo-32x32.png";
      logo64 = "${ijulia}/deps/logo-64x64.png";
    };
  };
}
