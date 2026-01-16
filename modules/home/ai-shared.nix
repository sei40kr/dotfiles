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
    };
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
    };
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
    };
  };

  # Combined MCP server type
  mcpServerType = types.oneOf [
    stdioServerType
    sseServerType
    httpServerType
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
        ../../config/ai/skills
        "/home/user/custom-skills"
      ];
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
      ./../../config/ai/skills
    ];

    home.packages = lib.flatten (
      lib.mapAttrsToList (
        name: server: lib.optional (server ? package && server.package != null) server.package
      ) cfg.mcpServers
    );
  };
}
