#!/usr/bin/env bash

tmux set -g message-command-style 'bg=#16161e' \; \
     set -g message-style 'bg=#16161e' \; \
     set -g mode-style 'fg=#c0caf5,bg=#33467c' \; \
     set -g status-justify absolute-centre \; \
     set -g status-left '#[fg=#7aa2f7]▎#[default] #[fg=#7aa2f7,bold]#S#[default]  #{prefix_highlight}' \; \
     set -g status-right 'CPU: #{cpu_percentage}  Mem: #{ram_percentage}  #{online_status}  %m/%d %H:%M ' \; \
     set -g status-style 'fg=#c0caf5,bg=#16161e' \; \
     set -wg pane-active-border-style 'fg=#15161e' \; \
     set -wg pane-border-style 'fg=#15161e' \; \
     set -wg window-status-current-format '#[fg=#7aa2f7]▎#[default] #[fg=#7aa2f7,bold]#I#[default]  #[bold]#W#[default]  ' \; \
     set -wg window-status-current-style 'fg=default,bg=#1a1b26' \; \
     set -wg window-status-format '  #I  #W  ' \; \
     set -wg window-status-separator '' \; \
     set -wg window-status-style 'fg=#3b4261,bg=#1a1b26' \; \
     set -g @online_icon "#[fg=#73daca]●#[default]" \; \
     set -g @offline_icon "#[fg=#db4b4b]●#[default]" \; \
     set -g @prefix_highlight_fg '#15161e' \; \
     set -g @prefix_highlight_bg '#7aa2f7' \; \
     set -g @prefix_highlight_show_copy_mode on \; \
     set -g @prefix_highlight_copy_mode_attr 'fg=#15161e,bg=#7aa2f7' \; \
     set -g @prefix_highlight_output_prefix ' ' \; \
     set -g @prefix_highlight_output_suffix ' '

# vim: ft=bash
