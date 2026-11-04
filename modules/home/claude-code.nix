{
  config,
  perSystem,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    flatten
    getExe
    makeBinPath
    mapAttrs
    mapAttrs'
    mapAttrsToList
    mkEnableOption
    mkIf
    mkPackageOption
    nameValuePair
    optionalAttrs
    ;
  cfg = config.modules.ai.claude-code;
  aiCfg = config.modules.ai;
  mcpCfg = aiCfg.mcpServers;

  # Each module carries its own leading separator so an empty module leaves
  # no dangling one.
  separator = "[ │ ](fg:#565F89)";
  lightGray = "#A9B1D6";
  thresholds = {
    warn_threshold = 50.0;
    warn_style = "fg:#E0AF68";
    critical_threshold = 80.0;
    critical_style = "fg:#F7768E";
  };
  # usage_limits paints its whole segment in one threshold style, so raw
  # escapes recolor everything after the percentage.
  esc = builtins.fromJSON ''"\u001b"'';
  # nf-oct-git_branch
  branchIcon = builtins.fromJSON ''""'';
  usageFormat = "{pct}%${esc}[38;2;169;177;214m ({reset})";
  # Shared by cship and the starship modules it passes through.
  cshipConfig = (pkgs.formats.toml { }).generate "cship.toml" {
    cship = {
      lines = [
        "$cship.model$cship.effort$cship.context_window.used_tokens$git_branch$custom.jj"
        # usage_limits reserves `format`, so labels and the separator live in the line.
        # shortcut: labels stay when no usage data arrives, move them into the module if cship gains per-part styles.
        "[5h: ](fg:${lightGray})$cship.usage_limits.session${separator}[7d: ](fg:${lightGray})$cship.usage_limits.weekly${separator}$cship.account"
      ];
      model.style = "fg:${lightGray}";
      effort = {
        format = " [$value]($style)";
        style = "fg:#C0CAF5";
        low_style = "fg:#E0AF68";
        medium_style = "fg:#9ECE6A";
        xhigh_style = "fg:#BB9AF7";
        max_style = "fg:#F7768E";
      };
      context_window.used_tokens = {
        format = "${separator}[$value]($style)";
        style = "fg:${lightGray}";
      }
      // thresholds;
      account = {
        # TODO: Swap ❋ for nf-cod-claude once nixpkgs ships a Nerd Fonts release with it.
        format = "❋ {tier}";
        style = "fg:#D97757";
      };
      usage_limits = {
        five_hour_format = usageFormat;
        seven_day_format = usageFormat;
        style = "fg:#9ECE6A";
      }
      // thresholds;
    };
    git_branch = {
      format = "${separator}[${branchIcon} $branch]($style)";
      style = "fg:#BB9AF7";
      # jj colocated repos keep git HEAD detached; custom.jj covers them.
      only_attached = true;
    };
    custom.jj = {
      when = "jj root --ignore-working-copy";
      command = "jj log -r 'heads(::@ & bookmarks())' --no-graph --ignore-working-copy -T 'bookmarks.map(|b| b.name()).join(\" \")'";
      format = "${separator}[${branchIcon} $output]($style)";
      style = "fg:#BB9AF7";
      require_repo = false;
      shell = [ "sh" ];
    };
  };

  # Official LSP plugins per modules.dev.lang.<name>.
  lspPlugins = {
    cc = "clangd-lsp";
    go = "gopls-lsp";
    java = "jdtls-lsp";
    javascript = "typescript-lsp";
    kotlin = "kotlin-lsp";
    lua = "lua-lsp";
    python = "pyright-lsp";
    ruby = "ruby-lsp";
    rust = "rust-analyzer-lsp";
  };

  convertMcpServer =
    name:
    { transport, ... }@server:
    if transport == "stdio" then
      {
        inherit (server) command args env;
        type = "stdio";
      }
    else if transport == "sse" || transport == "http" then
      {
        inherit (server) url headers;
        type = transport;
      }
    else
      throw "Unknown transport type ${transport} for MCP server ${name}";
in
{
  options.modules.ai.claude-code = {
    enable = mkEnableOption "Claude Code";

    claudebox = {
      enable = mkEnableOption "claudebox";
      package = mkPackageOption perSystem.llm-agents-nix "claudebox" { };
    };

    cship = {
      enable = mkEnableOption "cship";
      package = mkPackageOption perSystem.self "cship" { };
    };
  };

  config = mkIf cfg.enable {
    programs.claude-code = {
      enable = true;
      package = perSystem.llm-agents-nix.claude-code;
      settings = {
        autoMemoryEnabled = false;
        # Drop Claude Code's built-in commit/PR workflow instructions from the
        # system prompt; `context` below replaces them with skill dispatch and
        # the standing rules.
        includeGitInstructions = false;
        env = {
          CLAUDE_CODE_IDE_SKIP_AUTO_INSTALL = "1";
          DISABLE_AUTOUPDATER = "1";
          # DISABLE_AUTOUPDATER above also turns off the plugin auto-update
          # pass; this re-enables it for the marketplaces declared below.
          FORCE_AUTOUPDATE_PLUGINS = "1";
        };
        # Skill repos are consumed through Claude Code's plugin mechanism
        # instead of modules.ai.skillPaths: enabled plugins that aren't cached
        # yet are fetched automatically after session start, and marketplaces
        # with autoUpdate follow their repos without a rebuild.
        extraKnownMarketplaces = {
          sei40kr = {
            source = {
              source = "github";
              repo = "sei40kr/skills";
            };
            autoUpdate = true;
          };
          anthropic-agent-skills = {
            source = {
              source = "github";
              repo = "anthropics/skills";
            };
            autoUpdate = true;
          };
          ponytail = {
            source = {
              source = "github";
              repo = "DietrichGebert/ponytail";
            };
            autoUpdate = false;
          };
          yomiyasu = {
            source = {
              source = "github";
              repo = "nanaism/yomiyasu";
            };
            autoUpdate = false;
          };
        };
        enabledPlugins = {
          "sei40kr-skills@sei40kr" = true;
          "document-skills@anthropic-agent-skills" = true;
          "example-skills@anthropic-agent-skills" = true;
          "claude-api@anthropic-agent-skills" = true;
          "academy-guide@anthropic-agent-skills" = false;
          "discernment-nudge@anthropic-agent-skills" = false;
          "ponytail@ponytail" = true;
          "yomiyasu@yomiyasu" = true;
        }
        // mapAttrs' (
          lang: plugin:
          nameValuePair "${plugin}@claude-plugins-official" config.modules.dev.lang.${lang}.enable
        ) lspPlugins;
        permissions = {
          allow =
            (map (cmd: "Bash(${cmd} *)") aiCfg.permissions.allowedCommandPrefixes)
            ++ (map (domain: "WebFetch(domain:${domain})") aiCfg.permissions.allowedFetchDomains)
            ++ (flatten (
              mapAttrsToList (name: server: map (tool: "mcp__${name}__${tool}") server.allowedTools) mcpCfg
            ))
            ++ [
              # A commit is a reversible, safe operation.
              "Bash(git commit:*)"
            ];
          ask = [
            # git push affects the remote and is hard to reverse, so always
            # confirm with the user before pushing.
            "Bash(git push:*)"
          ];
          deny =
            (map (cmd: "Bash(${cmd} *)") aiCfg.permissions.deniedCommandPrefixes)
            ++ (flatten (
              mapAttrsToList (name: server: map (tool: "mcp__${name}__${tool}") server.deniedTools) mcpCfg
            ))
            ++ [
              # Claude Code tends to reach for `git add .` / `git add -A` and
              # end up committing files that should not be committed. Force it
              # to stage files explicitly by name.
              "Bash(git add .)"
              "Bash(git add -A)"
            ];
        };
        preferredNotifChannel = "kitty";
        # Commit signing talks to gpg-agent and keyboxd over Unix sockets.
        # Linux seccomp cannot allow sockets by path, so allow them all.
        sandbox.network.allowAllUnixSockets = true;
        theme = "custom:tokyo-night";
        # Use the classic main-screen renderer so scrollback stays in the
        # terminal and can be operated via tmux.
        tui = "default";
      }
      // optionalAttrs cfg.cship.enable {
        statusLine = {
          type = "command";
          command = "STARSHIP_CONFIG=${cshipConfig} STARSHIP_SHELL=unknown PATH=${makeBinPath [ pkgs.starship ]}:$PATH ${getExe cfg.cship.package} --config ${cshipConfig}";
          padding = 0;
        };
      };
      mcpServers = mapAttrs convertMcpServer mcpCfg;

      # Standing version-control rules, written to ~/.claude/CLAUDE.md. The
      # commit and PR workflows live in the sei40kr/skills plugin; this block
      # only dispatches to them and states what must hold even when no skill
      # is running.
      context = ''
        # Version control

        Use `jj` when the repository has a `.jj` directory, even next to `.git`; otherwise `git`.

        Follow the skills instead of improvising: `sei40kr-skills:commit` for commits and amends, `open-pr` for pull requests.

        Whether or not a skill is running:
        - Commit the changes you made with the `sei40kr-skills:commit` skill as soon as the task is done, without asking; the user reviews your work as that commit's diff. Commit only hunks you wrote this session; leave the user's uncommitted changes (in `@` for jj) untouched unless they ask.
        - End every commit message with a `Co-Authored-By: <your model name> <noreply@anthropic.com>` trailer.
        - End every pull request body with `🤖 Generated with [Claude Code](https://claude.com/claude-code)`.
        - Push only when the user explicitly asks.
        - Never run destructive commands (`push --force`, `reset --hard`, `checkout .`, `restore .`, `clean -f`, `branch -D`, `jj abandon`, `jj restore`), change git/jj config, or skip hooks (`--no-verify`, `--no-gpg-sign`) unless the user asks for that exact action. Never force-push main/master; warn if asked.
        - Never run interactive commands: no `-i` flags, and pass `-m` to every command that would otherwise open an editor.
        - Never bulk-stage with `git add -A` or `git add .`; the harness denies them.
      '';
    };

    # Enable remote control by overriding the wrapper's telemetry-disabling env vars
    home.sessionVariables = {
      CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC = "";
      DISABLE_NON_ESSENTIAL_MODEL_CALLS = "";
      DISABLE_TELEMETRY = "";
    };

    # Claude Code writes TUI setting changes back to settings.json, which fails
    # on a read-only store symlink. Install a writable copy instead, merging
    # the Nix settings over the existing file so TUI-only keys survive.
    # shortcut: keys dropped from Nix stay in the file, delete them by hand.
    # TODO: Replace this with `programs.claude-code.mutableSettings = true` on
    # the next home-manager update (nix-community/home-manager#10031).
    home.file."${config.programs.claude-code.configDir}/settings.json".enable = false;

    home.activation.claudeCodeSettings = lib.hm.dag.entryAfter [ "linkGeneration" ] (
      let
        settingsFile = config.home.file."${config.programs.claude-code.configDir}/settings.json";
        target = "${config.programs.claude-code.configDir}/settings.json";
      in
      ''
        current='{}'
        if [[ -f ${target} ]]; then
          current=$(cat ${target})
        fi
        merged=$(echo "$current" | ${getExe pkgs.jq} -s '.[0] * .[1]' - ${settingsFile.source})
        run rm -f ${target}
        run mkdir -p ${dirOf target}
        if [[ ! -v DRY_RUN ]]; then
          echo "$merged" > ${target}
        fi
      ''
    );
    home.activation.claudeCodeDoctor = lib.hm.dag.entryAfter [ "claudeCodeSettings" ] ''
      invalid=$(cd ~ && ${config.programs.claude-code.finalPackage}/bin/claude doctor </dev/null 2>&1 \
        | ${pkgs.gawk}/bin/awk '/^Invalid settings$/ { p = 1 } p && /^$/ { exit } p') || true
      if [[ -n $invalid ]]; then
        warnEcho "claude doctor found invalid settings:"
        echo "$invalid" >&2
      fi
    '';

    home.file.".claude/themes/tokyo-night.json".text =
      let
        # tokyonight.nvim "night" palette, named as in tokyonight.nvim.
        c = {
          bg = "#1a1b26";
          bg_dark = "#16161e";
          bg_highlight = "#292e42";
          bg_visual = "#283457";
          fg = "#c0caf5";
          fg_dark = "#a9b1d6";
          fg_gutter = "#3b4261";
          dark5 = "#737aa2";
          blue = "#7aa2f7";
          cyan = "#7dcfff";
          magenta = "#bb9af7";
          magenta2 = "#ff007c";
          purple = "#9d7cd8";
          orange = "#ff9e64";
          yellow = "#e0af68";
          green = "#9ece6a";
          green1 = "#73daca";
          red = "#f7768e";
          diff_add = "#243e4a";
          diff_delete = "#4a272f";
          # delta extra's plus/minus-emph-style
          delta_add = "#2c5a66";
          delta_delete = "#713137";
          # terminal.*_bright
          blue_bright = "#8db0ff";
          magenta_bright = "#c7a9ff";
          yellow_bright = "#faba4a";
          green_bright = "#9fe044";
          red_bright = "#ff899d";
        };

        # Not in tokyonight.nvim: tinted 30% toward white.
        orange_shimmer = "#ffbb92";
        purple_shimmer = "#baa3e4";

        # The built-in dark theme's brand colors.
        claude = "#d77757";
        claude_shimmer = "#eb9f7f";
      in
      builtins.toJSON {
        name = "Tokyo Night";
        base = "dark";
        overrides = {
          text = c.fg;
          inverseText = c.bg;
          inactive = c.fg_dark;
          inactiveShimmer = c.fg;
          subtle = c.dark5;

          inherit claude;
          claudeShimmer = claude_shimmer;
          clawd_body = claude;
          clawd_background = c.bg;
          briefLabelClaude = claude;
          briefLabelYou = c.blue;
          claudeBlue_FOR_SYSTEM_SPINNER = c.blue;
          claudeBlueShimmer_FOR_SYSTEM_SPINNER = c.blue_bright;

          permission = c.blue;
          permissionShimmer = c.blue_bright;
          suggestion = c.blue;
          remember = c.blue;
          professionalBlue = c.blue;
          ide = c.blue;
          background = c.cyan;
          promptBorder = c.blue;
          promptBorderShimmer = c.blue_bright;

          planMode = c.green1;
          autoAccept = c.magenta;
          autoAcceptShimmer = c.magenta_bright;
          skill = c.magenta;
          merged = c.magenta;
          effortUltra = c.magenta;
          bashBorder = c.magenta2;
          fastMode = c.orange;
          fastModeShimmer = orange_shimmer;

          success = c.green;
          error = c.red;
          warning = c.yellow;
          warningShimmer = c.yellow_bright;
          chromeYellow = c.yellow;

          diffAdded = c.diff_add;
          diffRemoved = c.diff_delete;
          diffAddedWord = c.delta_add;
          diffRemovedWord = c.delta_delete;
          # green2/red1 blended into bg at 12%
          diffAddedDimmed = "#1f2c37";
          diffRemovedDimmed = "#31212a";

          userMessageBackground = c.bg_highlight;
          userMessageBackgroundHover = c.fg_gutter;
          composerSidebarBackground = c.bg_dark;
          selectionBg = c.bg_visual;
          bashMessageBackgroundColor = "#31182f"; # magenta2 10%
          memoryBackgroundColor = "#242d3c"; # cyan 10%

          rate_limit_fill = c.blue;
          rate_limit_empty = c.fg_gutter;

          red_FOR_SUBAGENTS_ONLY = c.red;
          blue_FOR_SUBAGENTS_ONLY = c.blue;
          green_FOR_SUBAGENTS_ONLY = c.green;
          yellow_FOR_SUBAGENTS_ONLY = c.yellow;
          purple_FOR_SUBAGENTS_ONLY = c.purple;
          orange_FOR_SUBAGENTS_ONLY = c.orange;
          pink_FOR_SUBAGENTS_ONLY = c.magenta2;
          cyan_FOR_SUBAGENTS_ONLY = c.cyan;

          rainbow_red = c.red;
          rainbow_orange = c.orange;
          rainbow_yellow = c.yellow;
          rainbow_green = c.green;
          rainbow_blue = c.blue;
          rainbow_indigo = c.purple;
          rainbow_violet = c.magenta;
          rainbow_red_shimmer = c.red_bright;
          rainbow_orange_shimmer = orange_shimmer;
          rainbow_yellow_shimmer = c.yellow_bright;
          rainbow_green_shimmer = c.green_bright;
          rainbow_blue_shimmer = c.blue_bright;
          rainbow_indigo_shimmer = purple_shimmer;
          rainbow_violet_shimmer = c.magenta_bright;
        };
      };

    home.packages = [
      (mkIf cfg.claudebox.enable cfg.claudebox.package)
    ];
  };
}
