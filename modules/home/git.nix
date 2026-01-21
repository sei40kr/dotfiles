{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    types
    mdDoc
    mkIf
    mkOption
    mkEnableOption
    ;
  cfg = config.modules.shell.git;
in
{
  options.modules.shell.git = {
    enable = mkEnableOption "Git";

    user = {
      name = mkOption {
        type = types.str;
        default = "Seong Yong-ju";
        description = mdDoc ''
          The name of the user.
        '';
      };

      email = mkOption {
        type = types.str;
        default = "sei40kr@gmail.com";
        description = mdDoc ''
          The email of the user.
        '';
      };

    };

    signing = {
      key = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = mdDoc ''
          The signing key to use for commits.
        '';
      };

      format = mkOption {
        type = types.enum [
          "openpgp"
          "ssh"
          "x509"
        ];
        default = "openpgp";
        description = mdDoc ''
          The signing format to use (gpg, ssh, or x509).
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ gitflow ];

    programs.git = {
      enable = true;
      signing = {
        inherit (cfg.signing) key format;
        signByDefault = cfg.signing.key != null;
      };
      settings = {
        branch = {
          autosetuprebase = "always";
          "master" = {
            merge = "refs/heads/master";
            remote = "origin";
          };
        };
        commit.verbose = true;
        core = {
          pager = "${pkgs.delta}/bin/delta";
          whitespace = "fix,-indent-with-non-tab,trailing-space,cr-at-eol";
        };
        diff = {
          compactionHeuristic = true;
          indentHeuristic = true;
        };
        difftool.prompt = false;
        fetch.prune = true;
        github.user = "sei40kr";
        interactive.diffFilter = "${pkgs.delta}/bin/delta --color-only";
        merge.conflictstyle = "diff3";
        rebase.autosquash = true;
        rerere.enabled = true;
        push.default = "current";
        stash.showPatch = true;
        url."ssh://git@github.com/sei40kr".insteadOf = "https://github.com/sei40kr";
        user = { inherit (cfg.user) email name; };
      };
      ignores = [
        # Emacs
        "*~"
        "\\#*\\#"
        "/.emacs.desktop"
        "/.emacs.desktop.lock"
        "*.elc"
        "auto-save-list"
        "tramp"
        ".\\#*"
        ".org-id-locations"
        "*_archive"
        "*_flymake.*"
        "/eshell/history"
        "/eshell/lastdir"
        "/elpa/"
        "*.rel"
        "/auto/"
        ".cask/"
        "dist/"
        "flycheck_*.el"
        "/server/"
        ".projectile"
        ".dir-locals.el"

        # IntelliJ
        ".idea/**/workspace.xml"
        ".idea/**/tasks.xml"
        ".idea/**/usage.statistics.xml"
        ".idea/**/dictionaries"
        ".idea/**/shelf"
        ".idea/**/contentModel.xml"
        ".idea/**/dataSources/"
        ".idea/**/dataSources.ids"
        ".idea/**/dataSources.local.xml"
        ".idea/**/sqlDataSources.xml"
        ".idea/**/dynamic.xml"
        ".idea/**/uiDesigner.xml"
        ".idea/**/dbnavigator.xml"
        ".idea/**/gradle.xml"
        ".idea/**/libraries"
        ".idea/**/sonarlint/"
        ".idea/**/mongoSettings.xml"
        "*.iws"
        "out/"
        ".idea_modules/"
        "atlassian-ide-plugin.xml"
        "com_crashlytics_export_strings.xml"
        "crashlytics.properties"
        "crashlytics-build.properties"
        "fabric.properties"
        ".idea/caches/build_file_checksums.ser"

        # macOS
        ".DS_Store"
        ".AppleDouble"
        ".LSOverride"
        "Icon"
        "._*"
        ".DocumentRevisions-V100"
        ".fseventsd"
        ".Spotlight-V100"
        ".TemporaryItems"
        ".Trashes"
        ".VolumeIcon.icns"
        ".com.apple.timemachine.donotpresent"
        ".AppleDB"
        ".AppleDesktop"
        "Network Trash Folder"
        "Temporary Items"
        ".apdisk"
      ];
      lfs.enable = true;
    };

    home.shellAliases = rec {
      g = "git";
      ga = "git add";
      gaa = "git add --all";
      gapa = "git add --patch";
      gau = "git add --update";
      gav = "git add --verbose";
      gam = "git am";
      gama = "git am --abort";
      gamc = "git am --continue";
      gamscp = "git am --show-current-patch";
      gams = "git am --skip";
      gap = "git apply";
      gapt = "git apply --3way";

      gb = "git branch";
      gba = "git branch --all";
      gbd = "git branch --delete";
      gbD = "git branch --delete --force";
      gbm = "git branch --move";
      gbnm = "git branch --no-merged";
      gbr = "git branch --remote";

      gco = "git checkout";
      gcor = "git checkout --recurse-submodules";
      gcb = "git checkout -b";
      gcB = "git checkout -B";

      gcp = "git cherry-pick";
      gcpa = "git cherry-pick --abort";
      gcpc = "git cherry-pick --continue";

      gclean = "git clean --interactive -d";
      gcl = "git clone --recurse-submodules";
      gclf = "git clone --recursive --shallow-submodules --filter=blob:none --also-filter-submodules";

      gcam = "git commit --all --message";
      gcas = "git commit --all --signoff";
      gcasm = "git commit --all --signoff --message";
      gcs = "git commit --gpg-sign";
      gcss = "git commit --gpg-sign --signoff";
      gcssm = "git commit --gpg-sign --signoff --message";
      gcmsg = "git commit --message";
      gcsm = "git commit --signoff --message";
      gc = "git commit --verbose";
      gca = "git commit --verbose --all";
      "gca!" = "git commit --verbose --all --amend";
      "gcan!" = "git commit --verbose --all --no-edit --amend";
      "gcans!" = "git commit --verbose --all --signoff --no-edit --amend";
      "gcann!" = "git commit --verbose --all --date=now --no-edit --amend";
      "gc!" = "git commit --verbose --amend";
      gcn = "git commit --verbose --no-edit";
      "gcn!" = "git commit --verbose --no-edit --amend";
      gcf = "git config --list";
      gcfu = "git commit --fixup";

      gd = "git diff";
      gdca = "git diff --cached";
      gdcw = "git diff --cached --word-diff";
      gds = "git diff --staged";
      gdw = "git diff --word-diff";

      gf = "git fetch";
      gfa = "git fetch --all --tags --prune --jobs=10";
      gfo = "git fetch origin";

      gg = "git gui citool";
      gga = "git gui citool --amend";

      ghh = "git help";

      glgg = "git log --graph";
      glgga = "git log --graph --decorate --all";
      glgm = "git log --graph --max-count=10";
      glo = "git log --oneline --decorate";
      glog = "git log --oneline --decorate --graph";
      gloga = "git log --oneline --decorate --graph --all";
      glg = "git log --stat";
      glgp = "git log --stat --patch";

      gm = "git merge";
      gma = "git merge --abort";
      gmc = "git merge --continue";
      gms = "git merge --squash";
      gmff = "git merge --ff-only";
      gmtl = "git mergetool --no-prompt";
      gmtlvim = "git mergetool --no-prompt --tool=vimdiff";

      gl = "git pull";
      gpr = "git pull --rebase";
      gprv = "git pull --rebase -v";
      gpra = "git pull --rebase --autostash";
      gprav = "git pull --rebase --autostash -v";

      gp = "git push";
      gpd = "git push --dry-run";
      "gpf!" = "git push --force";
      gpf = "git push --force-with-lease --force-if-includes";
      gpv = "git push --verbose";
      gpu = "git push upstream";

      grb = "git rebase";
      grba = "git rebase --abort";
      grbc = "git rebase --continue";
      grbi = "git rebase --interactive";
      grbo = "git rebase --onto";
      grbs = "git rebase --skip";

      grf = "git reflog";
      gr = "git remote";
      grv = "git remote --verbose";
      gra = "git remote add";
      grrm = "git remote remove";
      grmv = "git remote rename";
      grset = "git remote set-url";
      grup = "git remote update";

      grh = "git reset";
      gru = "git reset --";
      grhh = "git reset --hard";
      grhk = "git reset --keep";
      grhs = "git reset --soft";
      # TODO: Uncomment when fixed: nix-community/home-manager#8589
      # gpristine = "git reset --hard; git clean --force -dfx";
      # gwipe = "git reset --hard; git clean --force -df";

      grs = "git restore";
      grss = "git restore --source";
      grst = "git restore --staged";

      grev = "git revert";
      greva = "git revert --abort";
      grevc = "git revert --continue";

      grm = "git rm";
      grmc = "git rm --cached";

      gcount = "git shortlog --summary --numbered";
      gsh = "git show";
      gsps = "git show --pretty=short --show-signature";

      gstall = "git stash --all";
      gstaa = "git stash apply";
      gstc = "git stash clear";
      gstd = "git stash drop";
      gstl = "git stash list";
      gstp = "git stash pop";
      gsta = "git stash push";

      gst = "git status";
      gss = "git status --short";
      gsb = "git status --short --branch";

      gsi = "git submodule init";
      gsu = "git submodule update";

      gsd = "git svn dcommit";
      gsr = "git svn rebase";

      gsw = "git switch";
      gswc = "git switch --create";

      gta = "git tag --annotate";
      gts = "git tag --sign";

      gignore = "git update-index --assume-unchanged";
      gunignore = "git update-index --no-assume-unchanged";
      gwch = "git whatchanged -p --abbrev-commit --pretty=medium";

      gwt = "git worktree";
      gwta = "git worktree add";
      gwtls = "git worktree list";
      gwtmv = "git worktree move";
      gwtrm = "git worktree remove";

      gbs = "git bisect";
      gbsb = "git bisect bad";
      gbsg = "git bisect good";
      gbsn = "git bisect new";
      gbso = "git bisect old";
      gbsr = "git bisect reset";
      gbss = "git bisect start";

      gbl = "git blame -w";
      gstu = "${gsta} --include-untracked";
    };
  };
}
