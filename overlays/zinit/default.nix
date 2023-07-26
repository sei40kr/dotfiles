_self: super: {
  zinit = super.zinit.overrideAttrs ({ patches ? [ ], installPhase, ... }: {
    patches = patches ++ [ ./no-doc-copy.patch ];

    installPhase = ''
      outdir="$out/share/$pname"

      # Zplugin's source files
      install -dm0755 "$outdir"
      # Installing backward compatibility layer
      install -m0644 zinit{,-side,-install,-autoload}.zsh "$outdir"
      install -m0755 share/git-process-output.zsh "$outdir"
      installManPage doc/zinit.1

      # Zplugin autocompletion
      installShellCompletion --zsh _zinit
    '';
  });
}
