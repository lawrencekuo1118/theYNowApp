#!/usr/bin/env node
/**
 * Headless Chrome CDP helper for verify-theynow.
 * Opens URL, optional click + wait, writes screenshot PNG + dom text dump.
 *
 * Usage:
 *   node verify-cdp-shot.mjs --url http://127.0.0.1:13838/ --out /path/prefix \
 *     [--click 'css'] [--wait 'css'] [--must-contain 'text'] [--sleep-ms N]
 */
import { spawn } from "node:child_process";
import { mkdirSync, writeFileSync, existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { setTimeout as sleep } from "node:timers/promises";

function arg(name, fallback = undefined) {
  const i = process.argv.indexOf(`--${name}`);
  if (i >= 0 && process.argv[i + 1]) return process.argv[i + 1];
  return fallback;
}

const url = arg("url");
const outPrefix = arg("out");
const clickSel = arg("click");
const waitSel = arg("wait");
const mustContain = arg("must-contain");
const sleepMs = Number(arg("sleep-ms", "2500"));

if (!url || !outPrefix) {
  console.error("Usage: verify-cdp-shot.mjs --url URL --out PREFIX [--click CSS] [--wait CSS] [--must-contain TEXT]");
  process.exit(2);
}

const outDir = dirname(resolve(outPrefix));
mkdirSync(outDir, { recursive: true });

const chrome =
  process.env.CHROME_PATH ||
  ["/usr/local/bin/google-chrome", "/usr/bin/google-chrome", "/usr/bin/chromium-browser", "/usr/bin/chromium"].find(
    (p) => existsSync(p)
  );
if (!chrome) {
  console.error("verify-cdp-shot: Chrome/Chromium not found");
  process.exit(3);
}

const debugPort = Number(process.env.YNOW_CDP_PORT || 0) || 9222 + Math.floor(Math.random() * 1000);
const userData = `/tmp/ynow-cdp-profile-${process.pid}`;
mkdirSync(userData, { recursive: true });

const chromeArgs = [
  `--remote-debugging-port=${debugPort}`,
  `--user-data-dir=${userData}`,
  "--headless=new",
  "--disable-gpu",
  "--no-sandbox",
  "--disable-dev-shm-usage",
  "--window-size=1400,900",
  "about:blank",
];

const child = spawn(chrome, chromeArgs, { stdio: ["ignore", "ignore", "pipe"] });

async function waitWsUrl(tries = 40) {
  for (let i = 0; i < tries; i++) {
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

function cdp(ws) {
  let nextId = 1;
  const pending = new Map();
  const eventHandlers = new Map();
  ws.addEventListener("message", (ev) => {
    const msg = JSON.parse(ev.data);
    if (msg.id && pending.has(msg.id)) {
      const { resolve, reject } = pending.get(msg.id);
      pending.delete(msg.id);
      if (msg.error) reject(new Error(JSON.stringify(msg.error)));
      else resolve(msg.result);
    } else if (msg.method) {
      const hs = eventHandlers.get(msg.method) || [];
      for (const h of hs) h(msg.params);
    }
  });
  return {
    send(method, params = {}) {
      const id = nextId++;
      return new Promise((resolve, reject) => {
        pending.set(id, { resolve, reject });
        ws.send(JSON.stringify({ id, method, params }));
      });
    },
    on(method, handler) {
      if (!eventHandlers.has(method)) eventHandlers.set(method, []);
      eventHandlers.get(method).push(handler);
    },
  };
}

async function main() {
  let wsUrl;
  try {
    wsUrl = await waitWsUrl();
  } catch (e) {
    console.error(String(child.stderr?.read?.() || e));
    child.kill("SIGKILL");
    process.exit(4);
  }

  const ws = new WebSocket(wsUrl);
  await new Promise((res, rej) => {
    ws.addEventListener("open", res);
    ws.addEventListener("error", rej);
  });
  const client = cdp(ws);

  const { targetId } = await client.send("Target.createTarget", { url: "about:blank" });
  const { sessionId } = await client.send("Target.attachToTarget", { targetId, flatten: true });

  const session = {
    send(method, params = {}) {
      const id = Math.floor(Math.random() * 1e9);
      return new Promise((resolve, reject) => {
        const onMsg = (ev) => {
          const msg = JSON.parse(ev.data);
          if (msg.sessionId === sessionId && msg.id === id) {
            ws.removeEventListener("message", onMsg);
            if (msg.error) reject(new Error(JSON.stringify(msg.error)));
            else resolve(msg.result);
          }
        };
        ws.addEventListener("message", onMsg);
        ws.send(JSON.stringify({ id, method, params, sessionId }));
      });
    },
  };

  await session.send("Page.enable");
  await session.send("Runtime.enable");
  await session.send("DOM.enable");

  await session.send("Page.navigate", { url });

  // Wait until Shiny is connected and Home brand exists (usable, not just HTTP 200).
  let shinyReady = false;
  for (let i = 0; i < 60; i++) {
    const ready = await session.send("Runtime.evaluate", {
      expression: `!!(window.Shiny && document.querySelector('#ynow_home_title'))`,
      returnByValue: true,
    });
    if (ready?.result?.value) {
      shinyReady = true;
      break;
    }
    await sleep(500);
  }
  if (!shinyReady) {
    writeFileSync(`${outPrefix}.error.txt`, "Shiny/#ynow_home_title not ready");
    throw new Error("Shiny UI not ready");
  }
  await sleep(Math.max(sleepMs, 1000));

  if (clickSel) {
    const clickRes = await session.send("Runtime.evaluate", {
      expression: `(() => {
        const el = document.querySelector(${JSON.stringify(clickSel)});
        if (!el) return { ok: false, reason: 'missing' };
        el.click();
        return { ok: true, text: (el.innerText || el.getAttribute('aria-label') || '').slice(0, 120) };
      })()`,
      returnByValue: true,
    });
    if (!clickRes?.result?.value?.ok) {
      writeFileSync(`${outPrefix}.error.txt`, `click failed for ${clickSel}: ${JSON.stringify(clickRes)}`);
      throw new Error(`click failed: ${clickSel}`);
    }
    await sleep(sleepMs);
  }

  if (waitSel) {
    let found = false;
    for (let i = 0; i < 30; i++) {
      const w = await session.send("Runtime.evaluate", {
        expression: `!!document.querySelector(${JSON.stringify(waitSel)})`,
        returnByValue: true,
      });
      if (w?.result?.value) {
        found = true;
        break;
      }
      await sleep(500);
    }
    if (!found) throw new Error(`wait timeout: ${waitSel}`);
  }

  const dump = await session.send("Runtime.evaluate", {
    expression: `(() => {
      const pick = (sel) => {
        const el = document.querySelector(sel);
        if (!el) return null;
        return (el.innerText || el.textContent || el.getAttribute('aria-label') || '').trim().slice(0, 500);
      };
      return {
        title: document.title,
        build: document.documentElement.getAttribute('data-ynow-build'),
        bodyLite: document.body.classList.contains('ynow-lite'),
        homeTitle: pick('#ynow_home_title'),
        aboutTitle: pick('.ynow-about-title'),
        macroTitle: pick('#ynow_macro_page_title'),
        corp: pick('#txt_corpname'),
        searchResults: pick('#search_results'),
        mktUs: document.querySelector('#ynow_mkt_btn_us')?.className || null,
        mktTw: document.querySelector('#ynow_mkt_btn_tw')?.className || null,
        langEn: document.querySelector('#ynow_lang_btn_en')?.className || null,
        langZh: document.querySelector('#ynow_lang_btn_zh')?.className || null,
        bodySnippet: (document.body && document.body.innerText || '').slice(0, 4000)
      };
    })()`,
    returnByValue: true,
  });

  const info = dump?.result?.value || {};
  writeFileSync(`${outPrefix}.dom.json`, JSON.stringify(info, null, 2));
  writeFileSync(
    `${outPrefix}.dom.txt`,
    [
      `title=${info.title}`,
      `build=${info.build}`,
      `homeTitle=${info.homeTitle}`,
      `aboutTitle=${info.aboutTitle}`,
      `macroTitle=${info.macroTitle}`,
      `corp=${info.corp}`,
      `--- body ---`,
      info.bodySnippet || "",
    ].join("\n")
  );

  if (mustContain) {
    const blob = `${info.aboutTitle || ""}\n${info.homeTitle || ""}\n${info.macroTitle || ""}\n${info.bodySnippet || ""}`;
    if (!blob.includes(mustContain)) {
      writeFileSync(`${outPrefix}.error.txt`, `must-contain missing: ${mustContain}`);
      throw new Error(`must-contain missing: ${mustContain}`);
    }
  }

  const shot = await session.send("Page.captureScreenshot", { format: "png", fromSurface: true });
  writeFileSync(`${outPrefix}.png`, Buffer.from(shot.data, "base64"));

  console.log(`verify-cdp-shot: wrote ${outPrefix}.png and ${outPrefix}.dom.txt`);
  ws.close();
  child.kill("SIGTERM");
  await sleep(300);
  try {
    child.kill("SIGKILL");
  } catch {
    /* ignore */
  }
}

main().catch(async (err) => {
  console.error("verify-cdp-shot:", err);
  try {
    child.kill("SIGKILL");
  } catch {
    /* ignore */
  }
  process.exit(1);
});
