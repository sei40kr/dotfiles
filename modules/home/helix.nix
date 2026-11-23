{
  config,
  inputs,
  lib,
  perSystem,
  ...
}:

let
  inherit (lib)
    concatLists
    filter
    genAttrs
    mapAttrs
    mapAttrsToList
    mkEnableOption
    mkIf
    optionalAttrs
    recursiveUpdate
    unique
    zipAttrs
    ;

  editorsCfg = config.modules.editors;
  cfg = editorsCfg.helix;
  termColorschemesCfg = config.modules.term.colorschemes;

  # Neovim filetype -> Helix language name. Filetypes Helix has no language
  # for (cuda, objc, quarto, …) are left out, since Helix rejects unknown
  # languages.
  helixLanguages = {
    bash = "bash";
    bib = "bibtex";
    c = "c";
    cpp = "cpp";
    go = "go";
    gomod = "gomod";
    gotmpl = "gotmpl";
    gowork = "gowork";
    haskell = "haskell";
    javascript = "javascript";
    javascriptreact = "jsx";
    kotlin = "kotlin";
    lean = "lean";
    lua = "lua";
    nix = "nix";
    python = "python";
    quint = "quint";
    r = "r";
    rmd = "rmarkdown";
    rust = "rust";
    sh = "bash";
    terraform = "hcl";
    terraform-vars = "tfvars";
    tex = "latex";
    typescript = "typescript";
    typescriptreact = "tsx";
    vue = "vue";
  };

  # { <helix language> = [ <server name> … ]; }
  languageServers = mapAttrs (_: unique) (
    zipAttrs (
      concatLists (
        mapAttrsToList (
          name: server:
          map (ft: { ${helixLanguages.${ft}} = name; }) (filter (ft: helixLanguages ? ${ft}) server.filetypes)
        ) editorsCfg.lspServers
      )
    )
  );

  noOps = keys: genAttrs keys (_: "no_op");

  # Helix defaults LazyVim and Vim have no counterpart for
  resetKeys = {
    normal = {
      # Surround moved to gs
      m = noOps [
        "d"
        "r"
        "s"
      ];
      # Helix's sticky view mode, where Vim's Z has only ZZ/ZQ
      Z = noOps [
        "/"
        "?"
        "b"
        "backspace"
        "c"
        "C-b"
        "C-d"
        "C-f"
        "C-u"
        "down"
        "j"
        "k"
        "m"
        "N"
        "n"
        "pagedown"
        "pageup"
        "space"
        "t"
        "up"
        "z"
      ];
      space = noOps [
        "'"
        "?"
        "A-c"
        "a"
        "C"
        "D"
        "e"
        "E"
        "F"
        "G"
        "h"
        "j"
        "k"
        "p"
        "P"
        "r"
        "R"
        "S"
        "y"
        "Y"
      ];
    };
    select.m = noOps [
      "d"
      "r"
      "s"
    ];
  };

  # Vim's <C-w> keys that Helix's window mode lacks
  windowBindings = {
    c = "wclose";
    n = "hsplit_new";
    W = "rotate_view_reverse";
  };

  bindings = {
    normal = {
      H = "goto_previous_buffer";
      L = "goto_next_buffer";
      K = "hover";
      "C-h" = "jump_view_left";
      "C-j" = "jump_view_down";
      "C-k" = "jump_view_up";
      "C-l" = "jump_view_right";
      "C-s" = ":write";
      "C-w" = windowBindings;
      "[" = {
        b = "goto_previous_buffer";
        h = "goto_prev_change";
      };
      "]" = {
        b = "goto_next_buffer";
        h = "goto_next_change";
      };
      g = {
        I = "goto_implementation";
        K = "signature_help";
        c.c = "toggle_comments";
        # mini.surround
        s = {
          a = "surround_add";
          d = "surround_delete";
          r = "surround_replace";
        };
      };
      # The Z node stays sticky, so after closing one of several splits,
      # <Esc> leaves it.
      Z = {
        Z = ":write-quit";
        Q = ":quit!";
      };
      space = {
        space = "file_picker";
        "," = "buffer_picker";
        "/" = "global_search";
        "`" = "goto_last_accessed_file";
        minus = "hsplit";
        "|" = "vsplit";
        b = {
          b = "goto_last_accessed_file";
          d = ":buffer-close";
          o = ":buffer-close-others";
        };
        c = {
          a = "code_action";
          f = ":format";
          r = "rename_symbol";
        };
        # nvim-dap (LazyVim dap.core extra)
        d = {
          a = "dap_launch";
          b = "dap_toggle_breakpoint";
          B = "dap_edit_condition";
          c = "dap_continue";
          i = "dap_step_in";
          o = "dap_step_out";
          O = "dap_next";
          P = "dap_pause";
          t = "dap_terminate";
        };
        f = {
          b = "buffer_picker";
          f = "file_picker";
          F = "file_picker_in_current_directory";
          n = ":new";
        };
        g.s = "changed_file_picker";
        q.q = ":quit-all";
        s = {
          C = "command_palette";
          d = "workspace_diagnostics_picker";
          D = "diagnostics_picker";
          g = "global_search";
          j = "jumplist_picker";
          R = "last_picker";
          s = "symbol_picker";
          S = "workspace_symbol_picker";
        };
        u = {
          f = ":toggle auto-format";
          h = ":toggle lsp.display-inlay-hints";
          L = ":toggle line-number relative absolute";
          w = ":toggle soft-wrap.enable";
        };
        w = windowBindings // {
          d = "wclose";
        };
      };
    };
    select = {
      g = {
        c = "toggle_comments";
        # mini.surround
        s = {
          a = "surround_add";
          d = "surround_delete";
          r = "surround_replace";
        };
      };
      space.c.f = "format_selections";
    };
    insert = {
      j.k = "normal_mode";
      "C-k" = "signature_help";
      "C-s" = [
        "normal_mode"
        ":write"
      ];
    };
  };
in
{
  options.modules.editors.helix = {
    enable = mkEnableOption "Helix";
  };

  config = mkIf cfg.enable {
    programs.helix = {
      enable = true;
      # The fork's flake leaves the plugin system off
      package = perSystem.helix.default.overrideAttrs { cargoBuildFeatures = [ "steel" ]; };
      settings = {
        theme = termColorschemesCfg.themes.${termColorschemesCfg.active}.helix;
        editor = {
          line-number = "relative";
          cursorline = true;
          default-yank-register = "+";
          cursor-shape = {
            normal = "block";
            insert = "bar";
            select = "block";
          };
          kitty-keyboard-protocol = "disabled";
          lsp.display-inlay-hints = true;
          # LazyVim: diagnostics as virtual text at the end of every line
          end-of-line-diagnostics = "hint";
          inline-diagnostics.cursor-line = "disable";
        };
        keys = recursiveUpdate resetKeys bindings;
      };
      languages = {
        language-server = mapAttrs (
          _: server:
          {
            inherit (server) command args;
          }
          // optionalAttrs (server.initOptions != { } || server.settings != { }) {
            config = server.initOptions // server.settings;
          }
        ) editorsCfg.lspServers;
        language = mapAttrsToList (name: language-servers: {
          inherit name language-servers;
        }) languageServers;
      };
    };

    xdg.configFile."helix/helix.scm".text = ''
      (require "preview-hx/preview.scm")
      (provide preview preview-stop)
    '';
    xdg.configFile."helix/init.scm".text = ''
      (require (only-in "helix/keymaps.scm" keymap))
      (require (only-in "preview-hx/preview.scm" preview))

      ;; Plugin commands are bound here rather than in config.toml, where the
      ;; key help labels them "Undocumented plugin command"; a Steel keymap
      ;; shows the first line of their @doc instead.

      ;; preview.hx, for the lang.markdown extra's Markdown Preview
      (keymap (global) (normal (space (c (p ":preview")))))
    '';
    xdg.dataFile."steel/cogs/preview-hx".source = inputs.preview-hx;
  };
}
