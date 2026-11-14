// Picks an agent, like workmux's dashboard, as the sidebar's agents list shows them.

import { View, div, image, svg } from "gpui-kit";
import { v_flex, h_flex, InputState, Input } from "gpui-base";
import { agents, spaces, focused, run, theme, props, closeOverlay } from "herdr";

const RANK = { blocked: 4, done: 3, working: 2, idle: 1 };
const SECTION = { 4: "Needs you", 3: "Done", 2: "Working", 1: "Idle" };
const SECTION_STATUS = { 4: "blocked", 3: "done", 2: "working" };

const clean = (s) => (s ?? "").replace(/^[^\p{L}\p{N}]+/u, "").trim();

function duration(ms) {
  const m = Math.floor(ms / 60000);
  if (m < 1) return "<1m";
  if (m < 60) return `${m}m`;
  const h = Math.floor(m / 60);
  return h < 24 ? `${h}h` : `${Math.floor(h / 24)}d`;
}

function icon(p, size, color) {
  if (!p?.icon) return null;
  return p.tint ? svg(p.icon).size(size).flex_none().text_color(color) : image(p.icon).w(size).h(size).flex_none();
}

// A named tab, else a title that isn't a path, host or the program's name.
function detail(a, tab) {
  if (tab?.named && tab.name !== a.program?.name) return tab.name;
  const live = clean(a.title);
  const path = /^[~/]/.test((a.title ?? "").replace(/^[^\p{L}\p{N}~/]+/u, ""));
  return live && live !== a.program?.name && !path && !live.includes("@") && live.toLowerCase() !== a.program?.key ? live : null;
}

export default class AgentPicker extends View {
  init(_props, cx) {
    this.filter = "";
    // Start on the current tab's agent.
    const rows = this.rows();
    const tab = focused()?.tab;
    const here = rows.findIndex((a) => a.focused);
    this.at = Math.max(0, here >= 0 ? here : rows.findIndex((a) => a.tab === tab));
    this.input = InputState.new({ placeholder: "Go to an agent…" });
    this.input.on("change", (_e, cx) => {
      this.filter = this.input.value();
      this.at = 0;
      cx.notify();
    });
    this.input.on("submit", () => this.go(this.rows()[this.at]));
    this.input.focus();
    cx.timer.after(600, () => this.input.focus());
    this.handle = cx.focus_handle();
    cx.timer.every(30000, (cx) => cx.notify());
  }

  close() {
    closeOverlay(props().overlay);
  }

  rows() {
    const tabs = new Map(spaces().flatMap((s) => s.tabs).map((tab) => [tab.id, tab]));
    const f = this.filter.toLowerCase();
    return agents()
      .map((a) => ({ ...a, detail: detail(a, tabs.get(a.tab)), changed: tabs.get(a.tab)?.changed_at }))
      .filter((a) => [a.space_name, a.detail, a.program?.name].join(" ").toLowerCase().includes(f))
      .sort((x, y) => (RANK[y.status] ?? 0) - (RANK[x.status] ?? 0) || y.seq - x.seq);
  }

  go(a) {
    if (!a) return;
    this.close();
    run("pane.focus", { pane: a.pane });
  }

  status(t, st) {
    if (st === "working") return svg("mods/workspace-chrome/spinner.svg").size(12).mx(1).flex_none().text_color(t.working).spin(900);
    if (st === "blocked") return div().size(14).flex_none().rounded(4).bg(t.blocked + "26").text_size(10.5).font_bold().text_color(t.blocked).flex().items_center().justify_center().child("!");
    if (st === "idle") return div().size(7).mx(3).flex_none().rounded(4).border(1.5).border_color(t.idle ?? t.dim);
    return div().size(7).mx(3).flex_none().rounded(4).bg(st === "done" ? t.done : t.dim + "00");
  }

  render() {
    const t = theme();
    const rows = this.rows();
    this.at = Math.min(this.at, Math.max(0, rows.length - 1));
    const now = Date.now();
    const rank = (a) => RANK[a.status] ?? RANK.idle;

    const row = (a, i) => {
      const machine = a.address.includes("/") ? a.address.split("/")[0] : null;
      return h_flex()
        .id(`ap-${a.address}`)
        .mx(4)
        .px(10)
        .h(30)
        .gap(9)
        .rounded(7)
        .items_center()
        .cursor_pointer()
        .when(i === this.at, (el) => el.bg(t.hover))
        .hover((s) => s.bg(t.hover))
        .on_click(() => this.go(a))
        .child(icon(a.program, 16, t.foreground) ?? div().w(16).flex_none())
        .child(h_flex().flex_1().min_w(0).gap(5).items_center()
          .child(div().flex_none().max_w(240).truncate().text_color(a.status === "idle" && !a.focused ? t.muted : t.foreground).child(a.space_name))
          .when(Boolean(a.detail), (el) => el.child(div().flex_1().min_w(0).truncate().text_color(t.dim).child(`· ${a.detail}`))))
        .when(Boolean(machine), (el) => el.child(div().flex_none().text_size(10).text_color(t.dim).child(machine)))
        .when(Boolean(a.changed), (el) => el.child(div().flex_none().text_size(11).text_color(t.dim).child(duration(now - a.changed))))
        .child(h_flex().w(14).flex_none().justify_center().child(this.status(t, a.status)));
    };

    const list = [];
    rows.forEach((a, i) => {
      if (!i || rank(rows[i - 1]) !== rank(a)) {
        list.push(h_flex().px(14).pt(i ? 8 : 2).h(24).gap(6).items_center().text_size(11).font_medium()
          .child(div().text_color(t[SECTION_STATUS[rank(a)]] ?? t.dim).child(SECTION[rank(a)]))
          .child(div().text_color(t.dim).child(String(rows.filter((b) => rank(b) === rank(a)).length))));
      }
      list.push(row(a, i));
    });

    return v_flex()
      .id("agent-picker")
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
      .child(v_flex().id("ap-list").max_h(480).overflow_y_scroll()
        .children(list)
        .when(!rows.length, (el) => el.child(div().px(14).text_size(12).text_color(t.dim).child(this.filter ? "No agent matches." : "No agent status yet."))));
  }
}
