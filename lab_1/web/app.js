// Frontend of "Faculty display classes schedule".
// The page is built from /api/meta, so it knows nothing about concrete tables:
// fields, their types and reports all come from the Haskell side.

"use strict";

const state = { meta: null, options: {}, rows: [] };
const main = document.getElementById("main");

// ---------- helpers ----------

function el(tag, attrs = {}, ...children) {
  const node = document.createElement(tag);
  for (const [key, value] of Object.entries(attrs)) {
    if (value === undefined || value === null || value === false) continue;
    if (key.startsWith("on")) node.addEventListener(key.slice(2), value);
    else if (key === "class") node.className = value;
    else node.setAttribute(key, value === true ? "" : value);
  }
  for (const child of children.flat()) {
    if (child === null || child === undefined) continue;
    node.append(child instanceof Node ? child : document.createTextNode(String(child)));
  }
  return node;
}

async function api(path, options = {}) {
  const response = await fetch(path, {
    ...options,
    headers: options.body ? { "Content-Type": "application/json" } : undefined,
  });
  const data = await response.json().catch(() => ({ errors: ["Server returned an invalid response."] }));
  if (!response.ok) throw data.errors || ["Request failed."];
  return data;
}

let toastTimer;
function toast(message) {
  const box = document.getElementById("toast");
  box.textContent = message;
  box.hidden = false;
  clearTimeout(toastTimer);
  toastTimer = setTimeout(() => (box.hidden = true), 2500);
}

function errorBox(errors) {
  return el("div", { class: "errors" }, el("ul", {}, [].concat(errors).map((e) => el("li", {}, e))));
}

// Options of foreign key fields, cached per referenced table.
async function refOptions(table) {
  if (!state.options[table]) state.options[table] = await api(`/api/options/${table}`);
  return state.options[table];
}

function refLabel(table, id) {
  const option = (state.options[table] || []).find((o) => String(o.id) === String(id));
  return option ? option.label : id;
}

// ---------- navigation ----------

function renderNav() {
  const current = location.hash;
  const link = (href, text) => el("a", { href, class: "nav-link" + (href === current ? " active" : "") }, text);
  const nav = document.getElementById("nav");
  nav.replaceChildren(
    el("div", { class: "nav-group" },
      el("div", { class: "nav-label" }, "Data"),
      state.meta.tables.map((t) => link(`#/table/${t.key}`, t.title))),
    el("div", { class: "nav-group" },
      el("div", { class: "nav-label" }, "Reports"),
      state.meta.reports.map((r) => link(`#/report/${r.key}`, r.title))),
  );
  nav.querySelector(".active")?.scrollIntoView({ block: "nearest", inline: "center" });
}

function route() {
  const [, kind, key] = location.hash.split("/");
  renderNav();
  const table = state.meta.tables.find((t) => t.key === key);
  const report = state.meta.reports.find((r) => r.key === key);
  if (kind === "table" && table) return showTable(table);
  if (kind === "report" && report) return showReport(report);
  location.hash = `#/table/${state.meta.tables[0].key}`;
}

// ---------- tables ----------

function displayValue(field, value) {
  if (value === "") return el("span", { class: "muted" }, "—");
  if (field.kind === "ref") return refLabel(field.ref, value);
  if (field.kind === "enum") return el("span", { class: `pill ${value}` }, value);
  return value;
}

async function showTable(table) {
  main.replaceChildren(el("div", { class: "loading" }, "Loading…"));
  try {
    const refs = table.fields.filter((f) => f.kind === "ref").map((f) => refOptions(f.ref));
    const [data] = await Promise.all([api(`/api/tables/${table.key}`), ...refs]);
    state.rows = data.rows;
  } catch (errors) {
    main.replaceChildren(errorBox(errors));
    return;
  }

  const body = el("tbody");
  const count = el("span", { class: "count" });
  const search = el("input", { class: "search", type: "search", placeholder: "Search…", oninput: fill });

  function fill() {
    const query = search.value.trim().toLowerCase();
    const rows = state.rows.filter((row) =>
      !query || table.fields.some((f) => String(displayText(f, row.values[f.name])).toLowerCase().includes(query)));
    count.textContent = `${rows.length} of ${state.rows.length}`;
    body.replaceChildren(...rows.map((row) =>
      el("tr", { class: "clickable", onclick: () => openForm(table, row) },
        el("td", { class: "num muted" }, row.id),
        table.fields.map((f) => el("td", { class: f.kind === "number" ? "num" : "long" }, displayValue(f, row.values[f.name]))),
        el("td", { class: "actions" },
          el("button", { class: "btn small quiet-danger", onclick: (e) => { e.stopPropagation(); remove(table, row); } }, "Delete")))));
    if (!rows.length) body.append(el("tr", {}, el("td", { colspan: table.fields.length + 2, class: "empty" }, "No records.")));
  }

  main.replaceChildren(
    el("div", { class: "page-head" },
      el("h1", {}, table.title), count, el("div", { class: "spacer" }), search,
      el("button", { class: "btn primary", onclick: () => openForm(table, null) }, `Add ${table.entity}`)),
    el("div", { class: "card" },
      el("div", { class: "table-wrap" },
        el("table", {},
          el("thead", {}, el("tr", {}, el("th", {}, "ID"), table.fields.map((f) => el("th", {}, f.label)), el("th"))),
          body))),
  );
  fill();
}

function displayText(field, value) {
  return field.kind === "ref" ? refLabel(field.ref, value) : value;
}

async function remove(table, row) {
  if (!confirm(`Delete this ${table.entity} (id ${row.id})?`)) return;
  try {
    await api(`/api/tables/${table.key}/${row.id}`, { method: "DELETE" });
    delete state.options[table.key];
    toast("Deleted.");
    showTable(table);
  } catch (errors) {
    alert([].concat(errors).join("\n"));
  }
}

// ---------- form ----------

const modal = document.getElementById("modal");
const form = document.getElementById("modal-form");
let submitForm = null;

modal.addEventListener("click", (e) => {
  if (e.target === modal || e.target.closest("[data-close]")) closeForm();
});
document.addEventListener("keydown", (e) => { if (e.key === "Escape" && !modal.hidden) closeForm(); });
form.addEventListener("submit", (e) => { e.preventDefault(); if (submitForm) submitForm(); });

function closeForm() {
  modal.hidden = true;
  submitForm = null;
}

// Input control for a field description coming from the server.
async function control(field, value, required) {
  const attrs = { name: field.name, id: `f-${field.name}` };
  const empty = required ? [] : [el("option", { value: "" }, "—")];
  switch (field.kind) {
    case "ref": {
      const options = await refOptions(field.ref);
      return el("select", attrs, empty,
        required && value === "" ? el("option", { value: "", disabled: true, selected: true }, "Select…") : null,
        options.map((o) => el("option", { value: o.id, selected: String(o.id) === String(value) }, o.label)));
    }
    case "enum":
    case "choice":
      return el("select", attrs, empty,
        field.options.map((o) => el("option", { value: o, selected: o === value }, o)));
    case "number": return el("input", { ...attrs, type: "number", value });
    case "time": return el("input", { ...attrs, type: "time", value });
    case "date": return el("input", { ...attrs, type: "date", value });
    default: return el("input", { ...attrs, type: "text", value });
  }
}

function fieldBox(field, input) {
  return el("div", { class: "field" },
    el("label", { for: input.id }, field.label, field.required ? el("span", { class: "req" }, " *") : null),
    input);
}

async function openForm(table, row) {
  const values = row ? row.values : {};
  const inputs = await Promise.all(table.fields.map((f) => control(f, values[f.name] ?? "", f.required)));
  document.getElementById("modal-title").textContent = row ? `Edit ${table.entity} #${row.id}` : `New ${table.entity}`;
  const errors = document.getElementById("modal-errors");
  errors.hidden = true;
  document.getElementById("modal-fields").replaceChildren(...table.fields.map((f, i) => fieldBox(f, inputs[i])));
  modal.hidden = false;
  inputs[0]?.focus();

  submitForm = async () => {
    const data = Object.fromEntries(table.fields.map((f, i) => [f.name, inputs[i].value]));
    try {
      if (row) await api(`/api/tables/${table.key}/${row.id}`, { method: "PUT", body: JSON.stringify(data) });
      else await api(`/api/tables/${table.key}`, { method: "POST", body: JSON.stringify(data) });
      delete state.options[table.key];
      closeForm();
      toast("Saved.");
      showTable(table);
    } catch (list) {
      errors.replaceChildren(errorBox(list).firstChild);
      errors.hidden = false;
      errors.scrollIntoView({ block: "nearest" });
    }
  };
}

// ---------- reports ----------

async function showReport(report) {
  // a foreign key parameter without default starts with the first available row
  const initial = async (p) =>
    p.kind === "ref" && !p.default ? String((await refOptions(p.ref))[0]?.id ?? "") : p.default;
  const inputs = await Promise.all(report.params.map(async (p) => control(p, await initial(p), true)));
  const results = el("div");

  async function run() {
    const query = new URLSearchParams(report.params.map((p, i) => [p.name, inputs[i].value]));
    results.replaceChildren(el("div", { class: "loading" }, "Running…"));
    try {
      const data = await api(`/api/reports/${report.key}?${query}`);
      results.replaceChildren(...data.tables.map(reportTable));
    } catch (errors) {
      results.replaceChildren(errorBox(errors));
    }
  }

  main.replaceChildren(...[
    el("div", { class: "page-head" }, el("h1", {}, report.title)),
    report.params.length
      ? el("form", { class: "card params", onsubmit: (e) => { e.preventDefault(); run(); } },
          report.params.map((p, i) => fieldBox({ ...p, required: false }, inputs[i])),
          el("button", { class: "btn primary", type: "submit" }, "Show"))
      : null,
    results,
  ].filter(Boolean));
  run();
}

function reportTable(t) {
  const cell = (value, index) => {
    if (index === t.bar) {
      const pct = Math.max(0, Math.min(100, parseFloat(value) || 0));
      return el("td", {},
        el("div", { class: "bar" },
          el("div", { class: "bar-track" }, el("div", { class: "bar-fill", style: `width:${pct}%` })),
          el("span", { class: "bar-label" }, `${value}%`)));
    }
    if (["working", "repair", "off"].includes(value)) return el("td", {}, el("span", { class: `pill ${value}` }, value));
    return el("td", { class: /^[\d.]+$/.test(value) ? "num" : "long" }, value);
  };
  return el("div", { class: "card" },
    t.title ? el("div", { class: "card-title" }, t.title) : null,
    t.rows.length
      ? el("div", { class: "table-wrap" },
          el("table", {},
            el("thead", {}, el("tr", {}, t.headers.map((h) => el("th", {}, h)))),
            el("tbody", {}, t.rows.map((r) => el("tr", {}, r.map(cell))))))
      : el("div", { class: "empty" }, t.empty));
}

// ---------- start ----------

(async () => {
  try {
    state.meta = await api("/api/meta");
  } catch (errors) {
    main.replaceChildren(errorBox(errors));
    return;
  }
  window.addEventListener("hashchange", route);
  route();
})();
