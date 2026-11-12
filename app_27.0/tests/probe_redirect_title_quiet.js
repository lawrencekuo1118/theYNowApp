'use strict';

const fs = require('fs');
const path = require('path');
const vm = require('vm');

const jsPath = path.join(__dirname, '..', 'www', 'ynow_app.js');
const src = fs.readFileSync(jsPath, 'utf8');
const start = src.indexOf('window.__ynowRedirectTitleMinHoldMs = 750;');
const end = src.indexOf('/* Every modal open');
if (start < 0 || end < start) {
  throw new Error('redirect title helpers not found');
}
const chunk = src.slice(start, end);

function classList(initial) {
  const set = new Set(initial || []);
  return {
    add(name) { set.add(name); },
    remove(name) { set.delete(name); },
    contains(name) { return set.has(name); }
  };
}

function harness() {
  let now = 0;
  let seq = 1;
  const timers = [];
  const html = { classList: classList([]) };
  const body = { classList: classList(['ynow-auth-redirecting']) };
  let endedAt = null;
  let endedWhileBusy = false;

  function note() {
    const redirecting = body.classList.contains('ynow-auth-redirecting');
    if (!redirecting && endedAt == null) {
      endedAt = now;
      endedWhileBusy = html.classList.contains('shiny-busy');
    }
  }

  function setTimeoutFn(fn, ms) {
    const id = seq++;
    timers.push({ id: id, at: now + Number(ms || 0), fn: fn, interval: null });
    return id;
  }
  function setIntervalFn(fn, ms) {
    const id = seq++;
    const every = Number(ms || 0);
    timers.push({ id: id, at: now + every, fn: fn, interval: every });
    return id;
  }
  function clearTimer(id) {
    const i = timers.findIndex(function (t) { return t.id === id; });
    if (i >= 0) timers.splice(i, 1);
  }
  function nextTimer() {
    let best = null;
    for (const t of timers) {
      if (!best || t.at < best.at || (t.at === best.at && t.id < best.id)) best = t;
    }
    return best;
  }
  function flushUntil(t) {
    while (true) {
      const n = nextTimer();
      if (!n || n.at > t) break;
      now = n.at;
      if (n.interval != null) n.at = now + n.interval;
      else timers.splice(timers.indexOf(n), 1);
      n.fn();
      note();
    }
    now = t;
    note();
  }

  const window = {
    requestAnimationFrame(fn) { return setTimeoutFn(function () { fn(now); }, 0); },
    ynowVisibleAuthTab() { return 'lab'; }
  };
  const document = {
    documentElement: html,
    body: body,
    getElementById() { return null; },
    querySelector() { return null; },
    addEventListener() {}
  };
  const context = {
    window: window,
    document: document,
    setTimeout: setTimeoutFn,
    clearTimeout: clearTimer,
    setInterval: setIntervalFn,
    clearInterval: clearTimer,
    Date: { now() { return now; } }
  };
  vm.createContext(context);
  vm.runInContext(chunk, context);

  return {
    window: window,
    html: html,
    body: body,
    flushUntil: flushUntil,
    setNow(t) { now = t; },
    now() { return now; },
    redirecting() { return body.classList.contains('ynow-auth-redirecting'); },
    busy() { return html.classList.contains('shiny-busy'); },
    endedAt() { return endedAt; },
    endedWhileBusy() { return endedWhileBusy; }
  };
}

function snap(h) {
  return { t: h.now(), redirecting: h.redirecting(), busy: h.busy() };
}

function runGap() {
  const h = harness();
  h.setNow(241);
  h.window.__ynowTitleRedirectStartedAt = 241 - 10000;
  h.window.ynowScheduleEndRedirectTitle('lab');
  h.flushUntil(321);
  const atFinishCandidate = snap(h);
  h.flushUntil(400);
  h.html.classList.add('shiny-busy');
  const atNavBusy = snap(h);
  const during = [];
  let clearedDuringBusy = false;
  for (let t = 450; t <= 1100; t += 50) {
    h.flushUntil(t);
    const row = snap(h);
    during.push(row);
    if (!row.redirecting) clearedDuringBusy = true;
  }
  h.html.classList.remove('shiny-busy');
  h.flushUntil(2000);
  return {
    scheduleAt: 241,
    oldEndAt: 321,
    navBusyAt: 400,
    navBusyUntil: 1100,
    atFinishCandidate: atFinishCandidate,
    atNavBusy: atNavBusy,
    clearedDuringNavBusy: clearedDuringBusy,
    duringNavBusyRedirecting: during.every(function (row) { return row.redirecting; }),
    endedAt: h.endedAt(),
    endedWhileBusy: h.endedWhileBusy(),
    redirectingAfterIdle: h.redirecting()
  };
}

function runMinHold() {
  const h = harness();
  h.setNow(1000);
  h.window.__ynowTitleRedirectStartedAt = 1000;
  h.window.ynowScheduleEndRedirectTitle('lab');
  h.flushUntil(1480);
  const atQuietEnd = snap(h);
  h.flushUntil(1800);
  return {
    atQuietEnd: atQuietEnd,
    endedAt: h.endedAt(),
    redirectingAfter: h.redirecting()
  };
}

function runForce() {
  const h = harness();
  h.setNow(241);
  h.window.__ynowTitleRedirectStartedAt = 241 - 10000;
  h.html.classList.add('shiny-busy');
  h.window.ynowScheduleEndRedirectTitle('lab');
  h.flushUntil(241 + 12000);
  return {
    t: h.now(),
    redirecting: h.redirecting(),
    busy: h.busy(),
    endedAt: h.endedAt()
  };
}

const gap = runGap();
const minHold = runMinHold();
const force = runForce();
const ok = gap.atFinishCandidate.redirecting === true &&
  gap.atFinishCandidate.busy === false &&
  gap.atNavBusy.redirecting === true &&
  gap.atNavBusy.busy === true &&
  gap.clearedDuringNavBusy === false &&
  gap.duringNavBusyRedirecting === true &&
  gap.endedWhileBusy === false &&
  gap.endedAt > gap.navBusyUntil &&
  gap.redirectingAfterIdle === false &&
  force.redirecting === false &&
  force.endedAt != null &&
  minHold.atQuietEnd.redirecting === true &&
  minHold.endedAt === 1750 &&
  minHold.redirectingAfter === false;

const result = { ok: ok, gap: gap, minHold: minHold, forceEnd: force };
process.stdout.write(JSON.stringify(result, null, 2) + '\n');
if (!ok) process.exit(1);
