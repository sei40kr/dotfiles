local function bind(key, action, args)
  rex.bind("ctrl+t>" .. key, action, args)
end

-- tmux defaults
bind("c", "client.tab.new")
bind("n", "client.tab.next")
bind("p", "client.tab.previous")
bind(",", "client.tab.rename")
bind("shift+7", "client.tab.close") -- &
bind("x", "pane.close")
bind("z", "pane.zoom")
bind("[", "pane.copy_mode.enter")
bind("]", "pane.paste")
bind("shift+;", "client.palette.toggle") -- :
bind("s", "session.switch")
bind("shift+4", "session.rename") -- $
bind("shift+9", "session.previous") -- (
bind("shift+0", "session.next") -- )
for i = 1, 9 do
  bind(tostring(i), "client.tab.goto", { index = i })
end

-- tmux-sensible
bind("shift+r", "client.config.reload") -- R
bind("ctrl+t", "pane.send_key", { key = "ctrl+t" })
bind("ctrl+p", "client.tab.previous")
bind("ctrl+n", "client.tab.next")

-- tmux-pain-control
for key, direction in pairs({ h = "left", j = "down", k = "up", l = "right" }) do
  bind(key, "pane.focus." .. direction)
  bind("ctrl+" .. key, "pane.focus." .. direction)

  -- tmux's bind -r: each resize re-enters a one-shot mode, so HJKL keeps
  -- resizing without the prefix until another key is pressed.
  local action = "pane_resize_" .. direction
  rex.action({
    name = action,
    title = "Resize Pane " .. direction .. " (repeatable)",
    run = function()
      rex.client.queue("pane.resize", { direction = direction, amount = 5 })
      rex.client.queue("client.mode.enter", { name = "resize", once = true })
    end,
  })
  bind("shift+" .. key, action)
  rex.bind("resize/shift+" .. key, action)
end
for _, key in ipairs({ "shift+\\", "\\", "shift+5" }) do -- | \ %
  bind(key, "pane.split", { direction = "right" })
end
for _, key in ipairs({ "-", "shift+-", "shift+'" }) do -- - _ "
  bind(key, "pane.split", { direction = "down" })
end
bind("shift+,", "client.tab.move.backward") -- <
bind("shift+.", "client.tab.move.forward") -- >

-- workmux's agent dashboard: agents by status, to jump to one.
bind("ctrl+s", "client.sidebar.activity.toggle")
