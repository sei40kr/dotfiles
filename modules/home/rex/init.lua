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

-- Repository picker: open a ghq repository or the dotfiles as a session,
-- or go to the session already open on it.
local home = os.getenv("HOME")
-- `ghq root` is slow too; ghq itself defaults to ~/ghq.
local ghq_root = os.getenv("GHQ_ROOT") or home .. "/ghq"
local dotfiles
for _, dir in ipairs({ "/etc/dotfiles", home .. "/dotfiles" }) do
  local f = io.open(dir)
  if f then
    f:close()
    dotfiles = dir
    break
  end
end

rex.action({
  name = "repository_picker",
  title = "Open Repository…",
  run = function()
    local items, labels = {}, {}
    local function add(dir, label)
      labels[dir] = label
      local short = dir:sub(1, #home + 1) == home .. "/" and "~" .. dir:sub(#home + 1) or dir
      table.insert(items, { id = dir, title = label, subtitle = short, icon = "folder" })
    end

    if dotfiles then
      add(dotfiles, "dotfiles")
    end
    -- `ghq list` walks into every repository; ghq puts them at host/owner/repo,
    -- next to workmux's repo__worktrees.
    local r = rex.exec({
      "@bfs@", "-s", ghq_root, "-mindepth", "3", "-maxdepth", "3", "-type", "d", "-not", "-name", "*__worktrees",
    })
    for dir in (r and r.stdout or ""):gmatch("[^\n]+") do
      -- owner/repo, so same-named repositories of different owners stay apart.
      add(dir, dir:match("[^/]+/[^/]+$"))
    end

    local dir = rex.ui.pick({ title = "Open repository", placeholder = "Filter repositories", items = items })
    local label = labels[dir]
    if dir == nil then
      return
    end

    local list = rex.call("session.list")
    for _, s in ipairs(list and list.sessions or {}) do
      if s.label == label then
        rex.client.queue("session.select", { session_id = s.session_id })
        return
      end
    end
    local created, err = rex.call("session.create", {
      label = label,
      initial_windows = {
        { layout = rex.layout.block({ flavor = "com.superlogical.terminal.shell", options = { cwd = dir } }) },
      },
    })
    if created == nil then
      error(err, 0)
    end
    rex.client.queue("session.select", { session_id = created.session_id })
  end,
})
bind("g", "repository_picker")
