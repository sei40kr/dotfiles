{
  lib,
  config,
  pkgs,
  ...
}:

let
  inherit (lib)
    mkOption
    mkEnableOption
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
        # Local stdio server
        filesystem = rec {
          transport = "stdio";
          package = pkgs.nodejs;
          command = "${package}/bin/npx";
          args = [
            "-y"
            "@modelcontextprotocol/server-filesystem"
            "/home/user"
          ];
        };

        # Local stdio server with env
        github = rec {
          transport = "stdio";
          package = pkgs.nodejs;
          command = "${package}/bin/npx";
          args = [
            "-y"
            "@modelcontextprotocol/server-github"
          ];
          env = {
            GITHUB_TOKEN = "ghp_xxxxxxxxxxxx";
          };
        };

        # SSE server
        remote-sse = {
          transport = "sse";
          url = "http://localhost:8080/sse";
          headers = {
            Authorization = "Bearer sk-xxxxxxxxxxxxxxxx";
          };
        };

        # HTTP server
        remote-http = {
          transport = "http";
          url = "http://localhost:3000/mcp";
          headers = {
            Authorization = "Bearer token123";
          };
        };
      };
    };
  };

  config = {
    environment.systemPackages = lib.flatten (
      lib.mapAttrsToList (
        name: server: lib.optional (server ? package && server.package != null) server.package
      ) cfg.mcpServers
    );
  };
}
