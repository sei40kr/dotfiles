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
    ;
  inherit (lib.my) mkBoolOpt;
  cfg = config.modules.shell.git;

  gitignoreFile = pkgs.writeTextFile {
    name = "gitignore-global";
    text = ''
      # -*- mode: gitignore; -*-

      # Created by https://www.gitignore.io/api/emacs,macos,intellij
      # Edit at https://www.gitignore.io/?templates=emacs,macos,intellij

      ### Emacs ###
      *~
      \#*\#
      /.emacs.desktop
      /.emacs.desktop.lock
      *.elc
      auto-save-list
      tramp
      .\#*

      # Org-mode
      .org-id-locations
      *_archive

      # flymake-mode
      *_flymake.*

      # eshell files
      /eshell/history
      /eshell/lastdir

      # elpa packages
      /elpa/

      # reftex files
      *.rel

      # AUCTeX auto folder
      /auto/

      # cask packages
      .cask/
      dist/

      # Flycheck
      flycheck_*.el

      # server auth directory
      /server/

      # projectiles files
      .projectile

      # directory configuration
      .dir-locals.el

      ### Intellij ###
      # Covers JetBrains IDEs: IntelliJ, RubyMine, PhpStorm, AppCode, PyCharm, CLion, Android Studio and WebStorm
      # Reference: https://intellij-support.jetbrains.com/hc/en-us/articles/206544839

      # User-specific stuff
      .idea/**/workspace.xml
      .idea/**/tasks.xml
      .idea/**/usage.statistics.xml
      .idea/**/dictionaries
      .idea/**/shelf

      # Generated files
      .idea/**/contentModel.xml

      # Sensitive or high-churn files
      .idea/**/dataSources/
      .idea/**/dataSources.ids
      .idea/**/dataSources.local.xml
      .idea/**/sqlDataSources.xml
      .idea/**/dynamic.xml
      .idea/**/uiDesigner.xml
      .idea/**/dbnavigator.xml

      # Gradle
      .idea/**/gradle.xml
      .idea/**/libraries

      # Gradle and Maven with auto-import
      # When using Gradle or Maven with auto-import, you should exclude module files,
      # since they will be recreated, and may cause churn.  Uncomment if using
      # auto-import.
      # .idea/modules.xml
      # .idea/*.iml
      # .idea/modules

      # CMake
      cmake-build-*/

      # Mongo Explorer plugin
      .idea/**/mongoSettings.xml

      # File-based project format
      *.iws

      # IntelliJ
      out/

      # mpeltonen/sbt-idea plugin
      .idea_modules/

      # JIRA plugin
      atlassian-ide-plugin.xml

      # Cursive Clojure plugin
      .idea/replstate.xml

      # Crashlytics plugin (for Android Studio and IntelliJ)
      com_crashlytics_export_strings.xml
      crashlytics.properties
      crashlytics-build.properties
      fabric.properties

      # Editor-based Rest Client
      .idea/httpRequests

      # Android studio 3.1+ serialized cache file
      .idea/caches/build_file_checksums.ser

      ### Intellij Patch ###
      # Comment Reason: https://github.com/joeblau/gitignore.io/issues/186#issuecomment-215987721

      # *.iml
      # modules.xml
      # .idea/misc.xml
      # *.ipr

      # Sonarlint plugin
      .idea/sonarlint

      ### macOS ###
      # General
      .DS_Store
      .AppleDouble
      .LSOverride

      # Icon must end with two \r
      Icon

      # Thumbnails
      ._*

      # Files that might appear in the root of a volume
      .DocumentRevisions-V100
      .fseventsd
      .Spotlight-V100
      .TemporaryItems
      .Trashes
      .VolumeIcon.icns
      .com.apple.timemachine.donotpresent

      # Directories potentially created on remote AFP share
      .AppleDB
      .AppleDesktop
      Network Trash Folder
      Temporary Items
      .apdisk

      # End of https://www.gitignore.io/api/emacs,macos,intellij
    '';
  };
in
{
  options.modules.shell.git = {
    enable = mkBoolOpt false;

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
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      git
      gitflow
    ];

    programs.git = {
      enable = true;
      config = {
        branch = {
          autosetuprebase = "always";
          "master" = {
            merge = "refs/heads/master";
            remote = "origin";
          };
        };
        commit.verbose = true;
        core = {
          excludesFile = gitignoreFile;
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
      lfs.enable = true;
    };

    modules.shell.aliases = rec {
      # Basic Git commands
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

      # Branch operations
      gb = "git branch";
      gba = "git branch --all";
      gbd = "git branch --delete";
      gbD = "git branch --delete --force";
      gbm = "git branch --move";
      gbnm = "git branch --no-merged";
      gbr = "git branch --remote";

      # Checkout
      gco = "git checkout";
      gcor = "git checkout --recurse-submodules";
      gcb = "git checkout -b";
      gcB = "git checkout -B";

      # Cherry-pick
      gcp = "git cherry-pick";
      gcpa = "git cherry-pick --abort";
      gcpc = "git cherry-pick --continue";

      # Clean and clone
      gclean = "git clean --interactive -d";
      gcl = "git clone --recurse-submodules";
      gclf = "git clone --recursive --shallow-submodules --filter=blob:none --also-filter-submodules";

      # Commit
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

      # Diff
      gd = "git diff";
      gdca = "git diff --cached";
      gdcw = "git diff --cached --word-diff";
      gds = "git diff --staged";
      gdw = "git diff --word-diff";

      # Fetch
      gf = "git fetch";
      gfa = "git fetch --all --tags --prune --jobs=10";
      gfo = "git fetch origin";

      # GUI
      gg = "git gui citool";
      gga = "git gui citool --amend";

      # Help
      ghh = "git help";

      # Log
      glgg = "git log --graph";
      glgga = "git log --graph --decorate --all";
      glgm = "git log --graph --max-count=10";
      glo = "git log --oneline --decorate";
      glog = "git log --oneline --decorate --graph";
      gloga = "git log --oneline --decorate --graph --all";
      glg = "git log --stat";
      glgp = "git log --stat --patch";

      # Merge
      gm = "git merge";
      gma = "git merge --abort";
      gmc = "git merge --continue";
      gms = "git merge --squash";
      gmff = "git merge --ff-only";
      gmtl = "git mergetool --no-prompt";
      gmtlvim = "git mergetool --no-prompt --tool=vimdiff";

      # Pull
      gl = "git pull";
      gpr = "git pull --rebase";
      gprv = "git pull --rebase -v";
      gpra = "git pull --rebase --autostash";
      gprav = "git pull --rebase --autostash -v";

      # Push
      gp = "git push";
      gpd = "git push --dry-run";
      "gpf!" = "git push --force";
      gpf = "git push --force-with-lease --force-if-includes";
      gpv = "git push --verbose";
      gpu = "git push upstream";

      # Rebase
      grb = "git rebase";
      grba = "git rebase --abort";
      grbc = "git rebase --continue";
      grbi = "git rebase --interactive";
      grbo = "git rebase --onto";
      grbs = "git rebase --skip";

      # Remote
      grf = "git reflog";
      gr = "git remote";
      grv = "git remote --verbose";
      gra = "git remote add";
      grrm = "git remote remove";
      grmv = "git remote rename";
      grset = "git remote set-url";
      grup = "git remote update";

      # Reset
      grh = "git reset";
      gru = "git reset --";
      grhh = "git reset --hard";
      grhk = "git reset --keep";
      grhs = "git reset --soft";
      gpristine = "git reset --hard && git clean --force -dfx";
      gwipe = "git reset --hard && git clean --force -df";

      # Restore
      grs = "git restore";
      grss = "git restore --source";
      grst = "git restore --staged";

      # Revert
      grev = "git revert";
      greva = "git revert --abort";
      grevc = "git revert --continue";

      # Remove
      grm = "git rm";
      grmc = "git rm --cached";

      # Show and shortlog
      gcount = "git shortlog --summary --numbered";
      gsh = "git show";
      gsps = "git show --pretty=short --show-signature";

      # Stash
      gstall = "git stash --all";
      gstaa = "git stash apply";
      gstc = "git stash clear";
      gstd = "git stash drop";
      gstl = "git stash list";
      gstp = "git stash pop";
      gsta = "git stash push";

      # Status
      gst = "git status";
      gss = "git status --short";
      gsb = "git status --short --branch";

      # Submodule
      gsi = "git submodule init";
      gsu = "git submodule update";

      # SVN
      gsd = "git svn dcommit";
      gsr = "git svn rebase";

      # Switch
      gsw = "git switch";
      gswc = "git switch --create";

      # Tag
      gta = "git tag --annotate";
      gts = "git tag --sign";

      # Update index utilities
      gignore = "git update-index --assume-unchanged";
      gunignore = "git update-index --no-assume-unchanged";
      gwch = "git whatchanged -p --abbrev-commit --pretty=medium";

      # Worktree
      gwt = "git worktree";
      gwta = "git worktree add";
      gwtls = "git worktree list";
      gwtmv = "git worktree move";
      gwtrm = "git worktree remove";

      # Bisect
      gbs = "git bisect";
      gbsb = "git bisect bad";
      gbsg = "git bisect good";
      gbsn = "git bisect new";
      gbso = "git bisect old";
      gbsr = "git bisect reset";
      gbss = "git bisect start";

      # Miscellaneous
      gbl = "git blame -w";
      gstu = "${gsta} --include-untracked";
    };
  };
}
