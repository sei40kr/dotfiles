// Opens a ghq repository or /etc/dotfiles as a space, or goes to its open one.

import { View, div } from "gpui-kit";
import { v_flex, h_flex, InputState, Input } from "gpui-base";
import { exec, home, run, spaces, panes, theme, shared, props, closeOverlay } from "herdr";

// shortcut: first 15 matches only, no scrolling.
const LIMIT = 15;

// The sidebar's (mods/spaces) avatar colors.
const MARK = { ansi6: "#22a99a", ansi3: "#e3814f", ansi4: "#5b8def", ansi5: "#8b7cf6", ansi2: "#3fae6a", ansi1: "#d9649a" };

// The space on a folder, placed as the sidebar does: its first terminal's cwd.
function openOn(dir) {
  const ps = panes();
  return spaces().find((s) => {
    if (s.machine) return false;
    const tabs = new Set(s.tabs.map((c) => c.id));
    return (ps.find((p) => tabs.has(p.tab) && p.cwd)?.cwd || s.folder) === dir;
  });
}

export default class RepositoryPicker extends View {
  init(_props, cx) {
    this.repos = ["/etc/dotfiles"];
    this.filter = "";
    this.at = 0;
    this.input = InputState.new({ placeholder: "Open a repository…" });
    this.input.on("change", (_e, cx) => {
      this.filter = this.input.value();
      this.at = 0;
      cx.notify();
    });
    this.input.on("submit", () => this.go(this.rows()[this.at]));
    this.input.focus();
    cx.timer.after(600, () => this.input.focus());
    this.handle = cx.focus_handle();
    cx.spawn(async (cx) => {
      try {
        const r = await exec("@ghq@", ["list", "-p"]);
        this.repos.push(...r.stdout.split("\n").filter(Boolean));
      } catch {}
      cx.notify();
    });
  }

  close() {
    closeOverlay(props().overlay);
  }

  rows() {
    const f = this.filter.toLowerCase();
    return this.repos.filter((dir) => dir.toLowerCase().includes(f)).slice(0, LIMIT);
  }

  go(dir) {
    if (!dir) return;
    this.close();
    const open = openOn(dir);
    if (open) run("space.select", { id: open.id });
    else run("space.new", { folder: dir });
  }

  // Gray when not open as a space.
  avatar(t, name, open, colors) {
    const c = open && (colors[open.id] ?? "ansi6");
    const fill = !c ? t.dim : c.startsWith("#") ? c : (MARK[c] ?? t[c]);
    return h_flex().size(18).flex_none().rounded(5).bg(fill).items_center().justify_center().text_size(10.5).font_bold().text_color("#ffffff").child([...name][0]?.toUpperCase() ?? "?");
  }

  render() {
    const t = theme();
    const rows = this.rows();
    this.at = Math.min(this.at, Math.max(0, rows.length - 1));
    const colors = shared("space.colors") ?? {};
    const h = home();
    const row = (dir, i) => {
      const name = dir.split("/").at(-1);
      return h_flex()
        .id(`gr-${i}`)
        .mx(4)
        .px(10)
        .h(32)
        .gap(10)
        .rounded(7)
        .items_center()
        .cursor_pointer()
        .when(i === this.at, (el) => el.bg(t.hover))
        .hover((s) => s.bg(t.hover))
        .on_click(() => this.go(dir))
        .child(this.avatar(t, name, openOn(dir), colors))
        .child(div().flex_none().max_w(260).truncate().text_color(t.foreground).child(name))
        .child(div().flex_1().min_w(0).truncate().text_size(11.5).text_color(t.dim).child(dir.startsWith(h + "/") ? "~" + dir.slice(h.length) : dir));
    };

    return v_flex()
      .id("repository-picker")
      .track_focus(this.handle)
      .w_full()
      .min_h(0)
      .p(18)
      .gap(10)
      .bg(t.popover)
      .text_size(13)
      .on_key_down((e, cx) => {
        if (e.key === "escape") return this.close();
        if (!rows.length) return;
        if (e.key === "down") this.at = (this.at + 1) % rows.length;
        else if (e.key === "up") this.at = (this.at + rows.length - 1) % rows.length;
        else return;
        cx.stop_propagation();
        cx.notify();
      })
      .child(
        h_flex().h(36).flex_none().px(10).items_center().rounded(9).border_1().border_color(t.accent).bg(t.background)
          .child(Input.new(this.input).flex_1().h_full().text_size(12.8).text_color(t.foreground)),
      )
      .child(v_flex().children(rows.map(row)));
  }
}
