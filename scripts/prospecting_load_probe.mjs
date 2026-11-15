#!/usr/bin/env node
/**
 * Browser stopwatch for Stock Prospecting first load.
 * Isolated local Shiny only. Does not change app behavior.
 *
 *   node scripts/prospecting_load_probe.mjs --url http://127.0.0.1:13848/ --out /tmp/run
 *     [--budget-ms 120000] [--expect-ranking 1] [--wait-jsonl PATH]
 *     [--wait-node catalog_prewarm] [--wait-phase end]
 */
import { spawn } from "node:child_process";
import { mkdirSync, writeFileSync, existsSync, readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { setTimeout as sleep } from "node:timers/promises";

function arg(name, fallback = undefined) {
  const i = process.argv.indexOf(`--${name}`);
  if (i >= 0 && process.argv[i + 1]) return process.argv[i + 1];
  return fallback;
}

const url = arg("url");
const outPrefix = arg("out");
const budgetMs = Number(arg("budget-ms", "120000"));
const expectRanking = arg("expect-ranking", "1") !== "0";
const waitJsonl = arg("wait-jsonl", "");
const waitNode = arg("wait-node", "");
const waitPhase = arg("wait-phase", "end");
const pollMs = Number(arg("poll-ms", "250"));

if (!url || !outPrefix) {
  console.error("Usage: prospecting_load_probe.mjs --url URL --out PREFIX");
  process.exit(2);
}
mkdirSync(dirname(resolve(outPrefix)), { recursive: true });

const chrome =
  process.env.CHROME_PATH ||
  ["/usr/local/bin/google-chrome", "/usr/bin/google-chrome", "/usr/bin/chromium-browser", "/usr/bin/chromium"].find(
    (p) => existsSync(p)
  );
if (!chrome) {
  console.error("prospecting_load_probe: Chrome/Chromium not found");
  process.exit(3);
}

const debugPort = Number(process.env.YNOW_CDP_PORT || 0) || 9330 + Math.floor(Math.random() * 400);
const userData = `/tmp/ynow-cdp-pload-${process.pid}`;
mkdirSync(userData, { recursive: true });
const child = spawn(
  chrome,
  [
    `--remote-debugging-port=${debugPort}`,
    `--user-data-dir=${userData}`,
    "--headless=new",
    "--disable-gpu",
    "--no-sandbox",
    "--disable-dev-shm-usage",
    "--window-size=1400,1000",
    "about:blank",
  ],
  { stdio: ["ignore", "ignore", "pipe"] }
);

async function waitWsUrl() {
  for (let i = 0; i < 40; i++) {
    try {
      const res = await fetch(`http://127.0.0.1:${debugPort}/json/version`);
      if (res.ok) {
        const j = await res.json();
        if (j.webSocketDebuggerUrl) return j.webSocketDebuggerUrl;
      }
    } catch {
      /* retry */
    }
    await sleep(250);
  }
  throw new Error("CDP endpoint not ready");
}

function jsonlHas(path, node, phase) {
  if (!path || !existsSync(path)) return false;
  const text = readFileSync(path, "utf8");
  return text.split("\n").some((line) => {
    if (!line.includes(`"${node}"`)) return false;
    try {
      const rec = JSON.parse(line);
      return rec.node === node && (!phase || rec.phase === phase);
    } catch {
      return false;
    }
  });
}

const SNAP = `(() => {
  const text = (sel) => {
    const el = document.querySelector(sel);
    return el ? (el.innerText || el.textContent || "").replace(/\\s+/g, " ").trim() : "";
  };
  const btn = document.querySelector("#lab_im_run_fscore");
  const ind = document.querySelector("#lab_im_refresh_ind_mcap");
  const shell = !!(document.querySelector("#bluechip_im_report") && document.querySelector("#lab_im_max_n"));
  const plot = document.querySelector("#lab_im_ind_mcap_treemap .js-plotly-plot, #lab_im_ind_mcap_treemap .plotly");
  const status = text("#lab_im_ind_mcap_status");
  const treemap = !!(plot && /\\d+/.test(status) && /chain|產業鏈|snapshot|快照|as of|資料時間/i.test(status));
  const lb = text("#lab_im_leaderboard");
  const body = (document.body && document.body.innerText || "").slice(0, 8000);
  const blob = lb + "\\n" + status + "\\n" + body;
  const catalogPrep = /Preparing universe catalog|正在準備候選宇宙/.test(blob);
  const rankingStarted = /ranking in progress|績優排行計算中|Evaluating…|評估中…|evaluation started|績優評估已在背景開始/.test(blob);
  const rankingDone = /Evaluated \\d+|已評估\\s*\\d+|Top 10 has no rows|年化估值漲幅有資料/.test(blob);
  const tab = document.querySelector('a[data-value="bluechip"]');
  const busyTab = !!(tab && (tab.classList.contains("ynow-tab-is-busy") || tab.getAttribute("aria-busy") === "true"));
  const shinyBusy = document.documentElement.classList.contains("shiny-busy") || document.body.classList.contains("shiny-busy");
  const btnText = btn ? (btn.innerText || "").replace(/\\s+/g, " ").trim() : "";
  const indText = ind ? (ind.innerText || "").replace(/\\s+/g, " ").trim() : "";
  const indBusy = /Updating ranking|更新排名中/.test(indText);
  const evalBusy = /Evaluating|評估中|Preparing universe|正在準備候選宇宙/.test(btnText);
  const interact = shell && !!btn && !btn.disabled && !document.querySelector(".shiny-disconnected-overlay");
  return {
    shell, treemap, interact, catalogPrep, rankingStarted, rankingDone,
    busyTab, shinyBusy, indBusy, evalBusy,
    status: status.slice(0, 220),
    lb: lb.slice(0, 280),
    btnText: btnText.slice(0, 120),
    indText: indText.slice(0, 80),
    build: document.documentElement.getAttribute("data-ynow-build") || ""
  };
})()`;

async function main() {
  const wsUrl = await waitWsUrl();
  const ws = new WebSocket(wsUrl);
  await new Promise((res, rej) => {
    ws.addEventListener("open", res);
    ws.addEventListener("error", rej);
  });

  let nextId = 1;
  const pending = new Map();
  ws.addEventListener("message", (ev) => {
    const msg = JSON.parse(ev.data);
    if (msg.id && pending.has(msg.id)) {
      const { resolve, reject } = pending.get(msg.id);
      pending.delete(msg.id);
      if (msg.error) reject(new Error(JSON.stringify(msg.error)));
      else resolve(msg.result);
    }
  });
  const browser = {
    send(method, params = {}, sessionId) {
      const id = nextId++;
      return new Promise((resolve, reject) => {
        pending.set(id, { resolve, reject });
        const payload = { id, method, params };
        if (sessionId) payload.sessionId = sessionId;
        ws.send(JSON.stringify(payload));
      });
    },
  };

  const { targetId } = await browser.send("Target.createTarget", { url: "about:blank" });
  const { sessionId } = await browser.send("Target.attachToTarget", { targetId, flatten: true });
  const send = (method, params = {}) => browser.send(method, params, sessionId);

  const wsFrames = [];
  ws.addEventListener("message", (ev) => {
    const msg = JSON.parse(ev.data);
    if (msg.sessionId !== sessionId || msg.method !== "Network.webSocketFrameReceived") return;
    const payload = msg.params?.response?.payloadData || "";
    wsFrames.push({
      t: Date.now(),
      opcode: msg.params?.response?.opcode,
      n: payload.length,
      head: payload.slice(0, 180),
    });
  });

  await send("Page.enable");
  await send("Runtime.enable");
  await send("Network.enable");
  await send("Page.navigate", { url });

  let homeReady = false;
  for (let i = 0; i < 80; i++) {
    const ready = await send("Runtime.evaluate", {
      expression: `!!(window.Shiny && document.querySelector('#ynow_home_title'))`,
      returnByValue: true,
    });
    if (ready?.result?.value) {
      homeReady = true;
      break;
    }
    await sleep(500);
  }
  if (!homeReady) throw new Error("Home not ready");

  if (waitJsonl && waitNode) {
    const deadline = Date.now() + 180000;
    while (Date.now() < deadline && !jsonlHas(waitJsonl, waitNode, waitPhase)) {
      await sleep(300);
    }
    if (!jsonlHas(waitJsonl, waitNode, waitPhase)) {
      throw new Error(`jsonl wait timeout ${waitNode}/${waitPhase}`);
    }
    await sleep(400);
  } else {
    await sleep(800);
  }

  await send("Runtime.evaluate", {
    expression: `(() => {
      const body = document.body;
      if (body && body.classList.contains("sidebar-collapse")) {
        const tog = document.querySelector(".sidebar-toggle");
        if (tog) tog.click();
      }
      return true;
    })()`,
    returnByValue: true,
  });
  await sleep(300);

  const clickRes = await send("Runtime.evaluate", {
    expression: `(() => {
      const el = document.querySelector('a[data-value="bluechip"]');
      if (!el) return { ok: false };
      el.click();
      return { ok: true, text: (el.innerText || "").slice(0, 80) };
    })()`,
    returnByValue: true,
  });
  if (!clickRes?.result?.value?.ok) throw new Error("bluechip click failed");
  const clickAt = Date.now();

  const polls = [];
  const marks = {};
  const mark = (key, snap) => {
    if (marks[key] == null) marks[key] = { t: Date.now(), s: (Date.now() - clickAt) / 1000, snap };
  };

  const deadline = clickAt + budgetMs;
  let idleStreak = 0;
  while (Date.now() < deadline) {
    const ev = await send("Runtime.evaluate", { expression: SNAP, returnByValue: true });
    const snap = ev?.result?.value || {};
    const s = (Date.now() - clickAt) / 1000;
    polls.push({ s: Math.round(s * 1000) / 1000, ...snap });
    if (snap.shell) mark("shell", snap);
    if (snap.treemap) mark("treemap", snap);
    if (snap.interact) mark("interact", snap);
    if (snap.catalogPrep) mark("catalog_prep", snap);
    if (snap.rankingStarted) mark("ranking_started", snap);
    if (snap.rankingDone) mark("ranking_completed", snap);
    // html.shiny-busy flickers on every flush. Tab aria-busy and button
    // labels are the settle signal (same caveat as the v28.48 stopwatch).
    const jobBusy = snap.busyTab || snap.evalBusy || snap.indBusy;
    if (snap.shell && !jobBusy) idleStreak += 1;
    else idleStreak = 0;
    if (idleStreak >= 4 && (!expectRanking || marks.ranking_completed)) {
      mark("busy_cleared", snap);
      break;
    }
    await sleep(pollMs);
  }

  const shot = await send("Page.captureScreenshot", { format: "png", fromSurface: true });
  writeFileSync(`${outPrefix}.png`, Buffer.from(shot.data, "base64"));

  const relFrames = wsFrames
    .filter((f) => f.t >= clickAt - 50)
    .map((f) => ({ s: Math.round((f.t - clickAt) / 10) / 100, n: f.n, head: f.head }));

  const summary = {
    url,
    click_at_unix_ms: clickAt,
    expect_ranking: expectRanking,
    budget_ms: budgetMs,
    build: polls.find((p) => p.build)?.build || "",
    marks: Object.fromEntries(Object.entries(marks).map(([k, v]) => [k, v.s])),
    last: polls[polls.length - 1] || null,
    n_polls: polls.length,
    n_ws_frames_after_click: relFrames.length,
  };
  writeFileSync(`${outPrefix}.summary.json`, JSON.stringify(summary, null, 2));
  writeFileSync(`${outPrefix}.polls.json`, JSON.stringify(polls));
  writeFileSync(`${outPrefix}.ws.json`, JSON.stringify(relFrames));
  console.log(JSON.stringify(summary.marks));
  ws.close();
  child.kill("SIGTERM");
}

main().catch(async (err) => {
  console.error("prospecting_load_probe:", err);
  try { child.kill("SIGKILL"); } catch { /* ignore */ }
  process.exit(1);
});
