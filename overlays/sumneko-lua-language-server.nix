(_self: super: {
  sumneko-lua-language-server = super.sumneko-lua-language-server.overrideAttrs
    (_: {
      version = "unstable-2022-01-11";

      src = super.fetchFromGitHub {
        owner = "sumneko";
        repo = "lua-language-server";
        rev = "141f5d98b593a4eb2c626e2219ac33d91a524a1d";
        sha256 = "sha256-8xqHAr/K7+jD5dDG/xZ6NHAmcQ39+BVpWo1mV5Eot3M=";
        fetchSubmodules = true;
      };
    });
})
