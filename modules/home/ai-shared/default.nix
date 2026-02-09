{
  lib,
  config,
  inputs,
  pkgs,
  ...
}:

let
  inherit (lib)
    mkOption
    types
    ;

  # Common MCP tool permission options shared across all server types
  mcpToolPermissionOptions = {
    allowedTools = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = "Tool name patterns that AI agents can use without asking for permission";
      example = [
        "search_*"
        "get_*"
      ];
    };

    deniedTools = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = "Tool name patterns that AI agents should deny without asking for confirmation";
      example = [
        "delete_*"
        "create_*"
      ];
    };
  };

  # MCP server type definitions for each transport
  stdioServerType = types.submodule {
    options = {
      transport = mkOption {
        type = types.enum [ "stdio" ];
        default = "stdio";
        description = "Transport type (stdio for local process)";
      };

      package = mkOption {
        type = types.nullOr types.package;
        default = null;
        description = "The package providing the MCP server";
        example = "pkgs.nodejs";
      };

      command = mkOption {
        type = types.str;
        description = "Command to run the MCP server";
        example = "npx";
      };

      args = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "Arguments to pass to the MCP server command";
        example = [
          "-y"
          "@modelcontextprotocol/server-filesystem"
          "/home/user"
        ];
      };

      env = mkOption {
        type = types.attrsOf types.str;
        default = { };
        description = "Environment variables for the MCP server";
        example = {
          GITHUB_TOKEN = "ghp_xxxxxxxxxxxx";
        };
      };
    }
    // mcpToolPermissionOptions;
  };

  sseServerType = types.submodule {
    options = {
      transport = mkOption {
        type = types.enum [ "sse" ];
        default = "sse";
        description = "Transport type (SSE for Server-Sent Events)";
      };

      url = mkOption {
        type = types.str;
        description = "SSE endpoint URL";
        example = "http://localhost:8080/sse";
      };

      headers = mkOption {
        type = types.attrsOf types.str;
        default = { };
        description = "HTTP headers for authentication";
        example = {
          Authorization = "Bearer token123";
        };
      };
    }
    // mcpToolPermissionOptions;
  };

  httpServerType = types.submodule {
    options = {
      transport = mkOption {
        type = types.enum [ "http" ];
        default = "http";
        description = "Transport type (HTTP for REST API)";
      };

      url = mkOption {
        type = types.str;
        description = "HTTP endpoint URL";
        example = "http://localhost:3000/mcp";
      };

      headers = mkOption {
        type = types.attrsOf types.str;
        default = { };
        description = "HTTP headers for authentication";
        example = {
          Authorization = "Bearer token123";
        };
      };
    }
    // mcpToolPermissionOptions;
  };

  # Combined MCP server type with addCheck for disambiguation
  mcpServerType = types.oneOf [
    (types.addCheck stdioServerType (v: v.transport or "" == "stdio"))
    (types.addCheck sseServerType (v: v.transport or "" == "sse"))
    (types.addCheck httpServerType (v: v.transport or "" == "http"))
  ];

  cfg = config.modules.ai;
in
{
  options.modules.ai = {
    mcpServers = mkOption {
      type = types.attrsOf mcpServerType;
      default = { };
      description = "MCP servers configuration supporting stdio, SSE, and HTTP transports";
      example = {
        filesystem = {
          transport = "stdio";
          package = null;
          command = "npx";
          args = [
            "-y"
            "@modelcontextprotocol/server-filesystem"
            "/home/user"
          ];
        };

        github = {
          transport = "stdio";
          package = null;
          command = "npx";
          args = [
            "-y"
            "@modelcontextprotocol/server-github"
          ];
          env = {
            GITHUB_TOKEN = "ghp_xxxxxxxxxxxx";
          };
        };
      };
    };

    skillPaths = mkOption {
      type = types.listOf (types.either types.path types.str);
      default = [ ];
      description = "List of skill directories to be combined and provided to AI tools";
      example = [
        ./skills
        "/home/user/custom-skills"
      ];
    };

    permissions = {
      allowedCommandPrefixes = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "Shell command prefixes that AI agents can execute without asking for permission. Commands matching any of these prefixes are auto-approved. Consumer modules map these to tool-specific formats.";
        example = [
          "git diff"
          "git log"
          "npm test"
        ];
      };

      deniedCommandPrefixes = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "Shell command prefixes that AI agents should deny without asking for confirmation.";
        example = [
          "rm -rf"
          "curl"
        ];
      };

      allowedFetchDomains = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "Domains that AI agents can fetch without asking for permission";
        example = [
          "github.com"
          "docs.python.org"
          "developer.mozilla.org"
        ];
      };
    };

    _combinedSkillsPath = mkOption {
      type = types.nullOr types.path;
      internal = true;
      readOnly = true;
      default = pkgs.symlinkJoin {
        name = "ai-skills";
        paths = cfg.skillPaths;
      };
      description = "Combined skills directory created from skillPaths list";
    };
  };

  config = {
    modules.ai.skillPaths = [
      "${inputs.anthropics-skills}/skills"
      ././skills
    ];

    home.packages = lib.flatten (
      lib.mapAttrsToList (
        name: server: lib.optional (server ? package && server.package != null) server.package
      ) cfg.mcpServers
    );
  };
}
