import * as Plot from "https://cdn.jsdelivr.net/npm/@observablehq/plot@0.6/+esm";
import {csvParse} from "https://cdn.jsdelivr.net/npm/d3-dsv@3/+esm";

async function fetchCSV(url) {
  const t = await (await fetch(url, {credentials: "same-origin"})).text();
  return csvParse(t, d => {
    const out = {};
    for (const [k, v] of Object.entries(d)) {
      const n = +v;
      out[k] = v !== "" && Number.isFinite(n) ? n : v;
    }
    return out;
  });
}
async function fetchJSON(url) {
  const r = await fetch(url, {credentials: "same-origin"});
  if (!r.ok) throw new Error(`HTTP ${r.status} for ${url}`);
  return r.json();
}

const fmt = new Intl.NumberFormat(undefined, {maximumFractionDigits: 0});
const escape = s => String(s).replace(/[&<>"]/g, m => ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;'}[m]));

function renderPlot(el, data) {
  const type = el.dataset.type;
  const x = el.dataset.x;
  const y = el.dataset.y;

  const humanize = s => s.replace(/_/g, ' ').replace(/\b\w/g, c => c.toUpperCase());

  const base = {
    style: {
      background: "transparent",
      fontSize: "12px",
      fontFamily: "triplicate, monospace"
    },
    marginLeft: 60,
    marginRight: 20,
    marginTop: 20,
    marginBottom: type === "bar-rotate" ? 72 : 48,
    x: {
      tickFormat: d => String(d),
      line: true,
      label: humanize(x)
    },
    y: {
      grid: true,
      line: true,
      label: humanize(y)
    },
    color: {legend: false}
  };

  let marks = [];
  const tipOpts = {format: {x: false, y: true}};
  switch (type) {
    case "line":
      marks = [
        Plot.lineY(data, {x, y, stroke: "currentColor"}),
        Plot.dot(data, {x, y, fill: "currentColor", tip: tipOpts})
      ];
      break;
    case "bar-rotate":
      marks = [Plot.barY(data, {x, y, fill: "currentColor", tip: tipOpts})];
      break;
    case "bar":
      marks = [Plot.barY(data, {x, y, fill: "currentColor", tip: tipOpts})];
  }

  const chart = Plot.plot({...base, caption: el.dataset.title, marks});
  el.replaceChildren(chart);
}

async function renderMetrics(container) {
  const m = await fetchJSON(container.dataset.src);
  const items = [
    `<li><strong>Posts:</strong> ${fmt.format(m.posts)}</li>`,
    `<li><strong>Words (total):</strong> ${fmt.format(m.words_total)}</li>`,
    `<li><strong>Median words/post:</strong> ${fmt.format(m.median_words)}</li>`,
    `<li><strong>P90 words/post:</strong> ${fmt.format(m.p90_words)}</li>`,
    `<li><strong>Max days between posts:</strong> ${fmt.format(m.max_days_between_posts)}</li>`,
    `<li><strong>Median days between posts:</strong> ${fmt.format(m.median_days_between_posts)}</li>`,
    `<li><strong>Mean days between posts:</strong> ${fmt.format(m.mean_days_between_posts)}</li>`,
    `<li><strong>First post:</strong> ${String(m.first_post).split("T")[0]}</li>`,
    `<li><strong>Latest post:</strong> ${String(m.last_post).split("T")[0]}</li>`,
  ];
  container.innerHTML = `<ul class="metrics">${items.join("")}</ul>`;
}

async function renderAll() {
  const figs = [...document.querySelectorAll("[data-plot]")];
  await Promise.all(figs.map(async el => {
    try {
      const data = await fetchCSV(el.dataset.src);
      renderPlot(el, data);
    } catch (e) {
      console.error("Chart render failed", e);
    }
  }));

  await Promise.all([...document.querySelectorAll("[data-widget='metrics']")].map(renderMetrics));
}

function debounced(fn, ms=150) {
  let t; return (...a)=>{ clearTimeout(t); t = setTimeout(()=>fn(...a), ms); };
}

if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", renderAll);
} else {
  renderAll();
}

addEventListener("resize", debounced(renderAll, 200));
