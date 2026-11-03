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

    ccstatusline = {
      enable = mkEnableOption "ccstatusline";
      package = mkPackageOption perSystem.llm-agents-nix "ccstatusline" { };
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
      // optionalAttrs cfg.ccstatusline.enable {
        statusLine = {
          type = "command";
          command = "${cfg.ccstatusline.package}/bin/ccstatusline";
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

    home.activation.claudeCodeDoctor = lib.hm.dag.entryAfter [ "linkGeneration" ] ''
      invalid=$(cd ~ && ${config.programs.claude-code.finalPackage}/bin/claude doctor </dev/null 2>&1 \
        | ${pkgs.gawk}/bin/awk '/^Invalid settings$/ { p = 1 } p && /^$/ { exit } p') || true
      if [[ -n $invalid ]]; then
        warnEcho "claude doctor found invalid settings:"
        echo "$invalid" >&2
      fi
    '';

    home.file.".claude/themes/tokyo-night.json".text = builtins.toJSON {
      name = "Tokyo Night";
      base = "dark";
      overrides = {
        claude = "#7AA2F7";
        claudeShimmer = "#9fbcf9";
        text = "#C0CAF5";
        inactive = "#414868";
        subtle = "#2f344a";
        suggestion = "#7AA2F7";
        permission = "#7AA2F7";
        success = "#9ECE6A";
        error = "#F7768E";
        warning = "#E0AF68";
        diffAdded = "#343f34";
        diffRemoved = "#462d3b";
        diffAddedWord = "#4f6341";
        diffRemovedWord = "#723f50";
        diffAddedDimmed = "#25292b";
        diffRemovedDimmed = "#2c222e";
        promptBorder = "#414868";
        planMode = "#7DCFFF";
        autoAccept = "#9ECE6A";
        bashBorder = "#BB9AF7";
        fastMode = "#7AA2F7";
        userMessageBackground = "#24283b";
        bashMessageBackgroundColor = "#272537";
        red_FOR_SUBAGENTS_ONLY = "#F7768E";
        blue_FOR_SUBAGENTS_ONLY = "#7AA2F7";
        green_FOR_SUBAGENTS_ONLY = "#9ECE6A";
        yellow_FOR_SUBAGENTS_ONLY = "#E0AF68";
        purple_FOR_SUBAGENTS_ONLY = "#BB9AF7";
        orange_FOR_SUBAGENTS_ONLY = "#FF9E64";
        pink_FOR_SUBAGENTS_ONLY = "#BB9AF7";
        cyan_FOR_SUBAGENTS_ONLY = "#7DCFFF";
      };
    };

    home.packages = [
      (mkIf cfg.claudebox.enable cfg.claudebox.package)
      (mkIf cfg.ccstatusline.enable cfg.ccstatusline.package)
    ];

    xdg.configFile."ccstatusline/settings.json".text = mkIf cfg.ccstatusline.enable (
      builtins.toJSON {
        version = 3;
        lines = [
          [
            {
              id = "1";
              type = "model";
              color = "";
            }
            {
              id = "2";
              type = "separator";
            }
            {
              id = "3";
              type = "context-length";
              color = "";
            }
            {
              id = "6";
              type = "separator";
            }
            {
              id = "162c2f21-371b-48b0-a623-1ddba6c760b3";
              type = "context-percentage-usable";
              color = "";
            }
            {
              id = "7d86f33e-a857-484e-8dcc-f7daf6f08397";
              type = "separator";
            }
            {
              id = "b10d0d0e-ce27-427e-a66c-43d1888d11e5";
              type = "session-cost";
              color = "";
            }
          ]
          [ ]
          [ ]
        ];
        flexMode = "full-minus-40";
        compactThreshold = 60;
        colorLevel = 2;
        inheritSeparatorColors = false;
        globalBold = false;
        powerline = {
          enabled = false;
          separators = [ "" ];
          separatorInvertBackground = [ false ];
          startCaps = [ ];
          endCaps = [ ];
          autoAlign = false;
        };
      }
    );
  };
}
