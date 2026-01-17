{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  inherit (builtins) floor;
  inherit (lib) mkEnableOption mkIf;

  editorsCfg = config.modules.editors;
  cfg = editorsCfg.zed;
  termFont = config.modules.term.font;
in
{
  options.modules.editors.zed = {
    enable = mkEnableOption "Zed";
  };

  config = mkIf cfg.enable {
    programs.zed-editor = {
      enable = true;
      userSettings = {
        active_pane_modifiers = {
          inactive_opacity = 0.7;
        };
        bottom_dock_layout = "full";
        auto_update = false;
        buffer_font_family = editorsCfg.fonts.code.name;
        buffer_font_size = floor (editorsCfg.fonts.code.size / 72.0 * 96.0);
        buffer_font_fallbacks = [ "Symbols Nerd Font Mono" ];
        ui_font_family = editorsCfg.fonts.ui.name;
        ui_font_size = floor (editorsCfg.fonts.ui.size / 72.0 * 96.0);
        close_on_file_delete = true;
        cursor_blink = false;
        scrollbar = {
          diagnostics = "warning";
          axes = {
            horizontal = false;
          };
        };
        tab_bar = {
          show_nav_history_buttons = false;
        };
        tabs = {
          file_icons = true;
          git_status = true;
          show_close_button = "always";
        };
        toolbar = {
          quick_actions = false;
          selections_menu = false;
          agent_review = false;
        };
        ensure_final_newline_on_save = false;
        diagnostics = {
          inline = {
            enabled = true;
          };
        };
        git = {
          inline_blame = {
            delay_ms = 500;
          };
        };
        indent_guides = {
          coloring = "indent_aware";
        };
        hover_popover_enabled = false;
        inlay_hints = {
          enabled = true;
        };
        languages = {
          Nix = {
            language_servers = [
              "nil"
              "!nixd"
            ];
            formatter = {
              external = {
                command = "nixfmt";
                arguments = [
                  "-f"
                  "{buffer_path}"
                ];
              };
            };
          };
          Python = {
            language_servers = [
              "basedpyright"
              "!pyright"
            ];
          };
        };
        preview_tabs = {
          enabled = false;
        };
        search = {
          regex = true;
        };
        seed_search_query_from_cursor = "selection";
        use_smartcase_search = true;
        show_whitespaces = "none";
        soft_wrap = "editor_width";
        telemetry = {
          diagnostics = false;
          metrics = false;
        };
        terminal = {
          env = {
            ZED = "1";
          };
          font_family = termFont.name;
          font_size = floor (termFont.size / 72.0 * 96.0);
          line_height = {
            custom = 1;
          };
          toolbar = {
            breadcrumbs = false;
          };
          button = false;
        };
        theme = {
          mode = "dark";
          dark = "Tokyo Night";
          light = "One Light";
        };
        vim_mode = true;
        project_panel = {
          auto_reveal_entries = false;
        };
        agent = {
          version = "2";
          default_model = {
            provider = "copilot_chat";
            model = "gpt-4o";
          };
          inline_assistant_model = {
            provider = "copilot_chat";
            model = "gpt-4o";
          };
          commit_message_model = {
            provider = "copilot_chat";
            model = "gpt-4o";
          };
          thread_summary_model = {
            provider = "copilot_chat";
            model = "gpt-4o";
          };
          default_view = "text_thread";
        };
        vim = {
          use_smartcase_find = true;
        };
        features = {
          edit_prediction_provider = "copilot";
        };
      };
      userKeymaps = [
        {
          context = "vim_mode == normal || vim_mode == visual";
          bindings = {
            "g s a" = "vim::PushAddSurrounds";
            "space c a" = "editor::ToggleCodeActions";
            "space c f" = "editor::Format";
          };
        }
        {
          context = "vim_mode == normal";
          bindings = {
            "g s d" = "vim::PushDeleteSurrounds";
            "g s r" = "vim::PushChangeSurrounds";
          };
        }
        {
          context = "vim_mode == insert";
          bindings = {
            "ctrl-m" = "editor::Newline";
            "ctrl-shift-m" = "editor::Newline";
            "ctrl-k" = "editor::ShowSignatureHelp";
            "ctrl-h" = "editor::Backspace";
            "j k" = "vim::NormalBefore";
          };
        }
        {
          context = "Pane > (!Editor || vim_mode == normal)";
          bindings = {
            # FIXME: Conflicts with `vim::WindowTop`
            "shift-h" = "pane::ActivatePreviousItem";
            # FIXME: Conflicts with `vim::WindowBottom`
            "shift-l" = "pane::ActivateNextItem";
            "[b" = "pane::ActivatePreviousItem";
            "]b" = "pane::ActivateNextItem";
            "space -" = "pane::SplitDown";
            "space |" = "pane::SplitRight";
            "space w d" = "pane::CloseActiveItem";
            "space b l" = "pane::CloseItemsToTheLeft";
            "space b p" = "pane::TogglePinTab";
            "space b P" = "pane::CloseAllItems";
            "space b r" = "pane::CloseItemsToTheRight";
          };
        }
        {
          context = "AgentPanel || GitPanel || ProjectPanel || CollabPanel || OutlinePanel || ChatPanel || (VimControl && vim_mode == normal) || EmptyPane || SharedScreen || MarkdownPreview || KeyContextView || DebugPanel";
          bindings = {
            "ctrl-h" = "workspace::ActivatePaneLeft";
            "ctrl-j" = "workspace::ActivatePaneDown";
            "ctrl-k" = "workspace::ActivatePaneUp";
            "ctrl-l" = "workspace::ActivatePaneRight";
            "ctrl-up" = "vim::ResizePaneUp";
            "ctrl-down" = "vim::ResizePaneDown";
            "ctrl-left" = "vim::ResizePaneLeft";
            "ctrl-right" = "vim::ResizePaneRight";
            "space b b" = "tab_switcher::Toggle";
            "space `" = "tab_switcher::Toggle";
            "space b o" = "workspace::CloseInactiveTabsAndPanes";
            "space b shift-d" = "workspace::CloseAllItemsAndPanes";
            "space b d" = [
              "pane::CloseActiveItem"
              { close_pinned = false; }
            ];
            "space f n" = "workspace::NewFile";
            "space f t" = "workspace::NewTerminal";
            "space f T" = "workspace::OpenInTerminal";
            "space w m" = "workspace::ToggleZoom";
            "space c S" = "project_symbols::Toggle";
            "space x l" = "diagnostics::Deploy";
            "space space" = "file_finder::Toggle";
            "space ," = "tab_switcher::Toggle";
            "space /" = "pane::DeploySearch";
            "space f b" = "tab_switcher::Toggle";
            "space f f" = "file_finder::Toggle";
            "space e" = "project_panel::ToggleFocus";
            "space f e" = "project_panel::ToggleFocus";
            "space f p" = "projects::OpenRecent";
            "space s g" = "pane::DeploySearch";
            "space u shift-c" = "theme_selector::Toggle";
            "space o b" = "zed::OpenProjectTasks";
            "space o o" = "task::Spawn";
            "space q q" = "zed::Quit";
          };
        }
        {
          context = "Editor";
          bindings = {
            "ctrl-s" = "workspace::Save";
          };
        }
        {
          context = "Editor && edit_prediction";
          bindings = {
            "ctrl-i" = "editor::AcceptEditPrediction";
          };
        }
        {
          context = "Editor && (showing_code_actions || showing_completions)";
          bindings = {
            "ctrl-j" = "editor::ContextMenuNext";
            # FIXME: Conflicts with `editor::CutToEndOfLine`
            "ctrl-k" = "editor::ContextMenuPrevious";
          };
        }
        {
          context = "Editor && showing_code_actions";
          bindings = {
            "ctrl-m" = "editor::ConfirmCodeAction";
          };
        }
        {
          context = "Editor && showing_completions";
          bindings = {
            "ctrl-m" = "editor::ConfirmCompletion";
            "ctrl-shift-m" = "editor::ConfirmCompletionReplace";
            "ctrl-i" = "editor::ComposeCompletion";
          };
        }
        {
          context = "Editor && renaming";
          bindings = {
            "ctrl-m" = "editor::ConfirmRename";
          };
        }
        {
          context = "Editor && vim_mode == normal";
          bindings = {
            "space c d" = "editor::Hover";
            "]d" = "editor::GoToDiagnostic";
            "[d" = "editor::GoToPreviousDiagnostic";
            "space u w" = "editor::ToggleSoftWrap";
            "space u shift-l" = "editor::ToggleRelativeLineNumbers";
            "space u l" = "editor::ToggleLineNumbers";
            "space u shift-a" = "editor::ToggleTabBar";
            "space u g" = "editor::ToggleIndentGuides";
            "space u h" = "editor::ToggleInlayHints";
            "space g b" = "git::Blame";
            "space g s" = "git_panel::ToggleFocus";
            "space c s" = "outline_panel::ToggleFocus";
            "g d" = "editor::GoToDefinition";
            "g r" = "editor::FindAllReferences";
            "g shift-i" = "editor::GoToImplementation";
            "g y" = "editor::GoToTypeDefinition";
            "g shift-k" = "editor::ShowSignatureHelp";
            "space c a" = "editor::ToggleCodeActions";
            "space c r" = "editor::Rename";
            "space shift-e" = "pane::RevealInProjectPanel";
            "space f shift-e" = "pane::RevealInProjectPanel";
            "space s s" = "outline::Toggle";
            "space g h s" = "git::StageAndNext";
            "space g h r" = "git::Restore";
            "space g h shift-r" = "git::RestoreFile";
            "space g h u" = "git::UnstageAndNext";
          };
        }
        {
          context = "GitPanel && ChangesList";
          bindings = {
            "d" = "git::Diff";
            "c c" = "git::Commit";
            "c a" = "git::Amend";
            "f" = "git::Fetch";
            "p" = "git::Pull";
            "shift-p p" = "git::Push";
            "s" = "git::StageFile";
            "shift-s" = "git::StageAll";
            "u" = "git::UnstageFile";
            "x" = [
              "git::RestoreFile"
              { skip_prompt = false; }
            ];
            "shift-u" = "git::UnstageAll";
            "escape" = "git_panel::Close";
            "ctrl-[" = "git_panel::Close";
            "q" = "git_panel::Close";
          };
        }
        {
          context = "KeyContextView";
          bindings = {
            "q" = "pane::CloseActiveItem";
          };
        }
        {
          context = "Picker || menu";
          bindings = {
            "ctrl-m" = "menu::Confirm";
            "ctrl-i" = "menu::SelectNext";
            "ctrl-[" = "menu::Cancel";
            "ctrl-j" = "menu::SelectNext";
            # FIXME: Conflicts with `editor::CutToEndOfLine`
            "ctrl-k" = "menu::SelectPrevious";
          };
        }
        {
          context = "ProjectPanel && not_editing";
          bindings = {
            "a" = "project_panel::NewFile";
            "c" = "project_panel::Copy";
            "p" = "project_panel::Paste";
            "y" = "workspace::CopyRelativePath";
            "r" = "project_panel::Rename";
            "d" = "project_panel::Delete";
            "ctrl-m" = "project_panel::OpenPermanent";
            "o" = "project_panel::OpenWithSystem";
            "space /" = "project_panel::NewSearchInDirectory";
            "ctrl-t" = "workspace::OpenInTerminal";
            "shift-i" = "project_panel::ToggleHideGitIgnore";
            "alt-i" = "project_panel::ToggleHideGitIgnore";
            "ctrl-[" = "project_panel::ToggleFocus";
            "q" = "project_panel::ToggleFocus";
          };
        }
        {
          context = "vim_mode == normal && extension == md";
          bindings = {
            "space c p" = "markdown::OpenPreviewToTheSide";
          };
        }
      ];
      extensions = [
        "basedpyright"
        "biome"
        "dockerfile"
        "haskell"
        "just"
        "nix"
        "ruby"
        "sql"
        "tokyo-night"
        "basher"
        "graphql"
        "html"
        "lua"
        "nu"
        "ruff"
        "terraform"
        "yaml"
      ];
    };

    home.packages = with pkgs; [ nerd-fonts.symbols-only ];
  };
}
