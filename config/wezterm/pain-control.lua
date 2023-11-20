local M = {}

local wezterm = require("wezterm")
local act = wezterm.action

---@param config unknown
---@param opts { pane_resize: number? }
function M.apply_to_config(config, opts)
  local pane_resize = opts.pane_resize or 5

  local keys = {
    -- Navigation
    { key = "h", mods = "LEADER",      action = act.ActivatePaneDirection("Left") },
    { key = "h", mods = "LEADER|CTRL", action = act.ActivatePaneDirection("Left") },
    { key = "j", mods = "LEADER",      action = act.ActivatePaneDirection("Down") },
    { key = "j", mods = "LEADER|CTRL", action = act.ActivatePaneDirection("Down") },
    { key = "k", mods = "LEADER",      action = act.ActivatePaneDirection("Up") },
    { key = "k", mods = "LEADER|CTRL", action = act.ActivatePaneDirection("Up") },
    { key = "l", mods = "LEADER",      action = act.ActivatePaneDirection("Right") },
    { key = "l", mods = "LEADER|CTRL", action = act.ActivatePaneDirection("Right") },

    -- Resizing Panes
    {
      key = "h",
      mods = "LEADER|SHIFT",
      action = act.Multiple({
        act.AdjustPaneSize({ "Left", pane_resize }),
        act.ActivateKeyTable({ name = "resize_pane", one_shot = false, until_unknown = true }),
      }),
    },
    {
      key = "j",
      mods = "LEADER|SHIFT",
      action = act.Multiple({
        act.AdjustPaneSize({ "Down", pane_resize }),
        act.ActivateKeyTable({ name = "resize_pane", one_shot = false, until_unknown = true }),
      }),
    },
    {
      key = "k",
      mods = "LEADER|SHIFT",
      action = act.Multiple({
        act.AdjustPaneSize({ "Up", pane_resize }),
        act.ActivateKeyTable({ name = "resize_pane", one_shot = false, until_unknown = true }),
      }),
    },
    {
      key = "l",
      mods = "LEADER|SHIFT",
      action = act.Multiple({
        act.AdjustPaneSize({ "Right", pane_resize }),
        act.ActivateKeyTable({ name = "resize_pane", one_shot = false, until_unknown = true }),
      }),
    },

    -- Splitting Panes
    { key = "|", mods = "LEADER", action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }) },
    { key = "-", mods = "LEADER", action = act.SplitVertical({ domain = "CurrentPaneDomain" }) },

    -- Swapping Windows
    { key = "<", mods = "LEADER", action = act.MoveTabRelative(-1) },
    { key = ">", mods = "LEADER", action = act.MoveTabRelative(1) },
  }
  for _, key in ipairs(keys) do
    table.insert(config.keys, key)
  end

  if not config.key_tables then
    config.key_tables = {}
  end
  config.key_tables.resize_pane = {
    { key = "h",      mods = "SHIFT",          action = act.AdjustPaneSize({ "Left", pane_resize }) },
    { key = "j",      mods = "SHIFT",          action = act.AdjustPaneSize({ "Down", pane_resize }) },
    { key = "k",      mods = "SHIFT",          action = act.AdjustPaneSize({ "Up", pane_resize }) },
    { key = "l",      mods = "SHIFT",          action = act.AdjustPaneSize({ "Right", pane_resize }) },
    { key = "Escape", action = act.PopKeyTable },
  }
end

return M
