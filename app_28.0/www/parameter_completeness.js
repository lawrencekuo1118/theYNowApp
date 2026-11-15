(function () {
  'use strict';
  var items = [], assumptions = [], assumptionTitle = '', pendingFocus = null, scheduled = false;

  function clearRendered() {
    document.querySelectorAll('.ynow-param-missing-hint').forEach(function (node) { node.remove(); });
    document.querySelectorAll('.ynow-lite-assumption-footnotes').forEach(function (node) { node.remove(); });
  }

  function footnoteHost(field) {
    if (!field) return null;
    // Prefer the innermost tab pane that owns the control so notes sit at the
    // bottom of the visible Lite valuation / model sub-tab.
    var pane = field.closest('.tab-pane');
    if (pane) return pane;
    return field.closest('.content-wrapper') || field.closest('.tab-content') || null;
  }

  function renderFootnotes() {
    if (!assumptions.length) return;
    var byHost = {};
    assumptions.forEach(function (item) {
      var field = document.getElementById(item.id);
      var host = footnoteHost(field);
      if (!host) return;
      var key = host.id || '__anon__';
      if (!byHost[key]) byHost[key] = { host: host, hints: [] };
      if (item.hint) byHost[key].hints.push(item.hint);
    });
    Object.keys(byHost).forEach(function (key) {
      var pack = byHost[key];
      if (!pack.hints.length) return;
      var aside = document.createElement('aside');
      aside.className = 'ynow-lite-assumption-footnotes';
      aside.setAttribute('role', 'note');
      var title = document.createElement('span');
      title.className = 'ynow-lite-assumption-footnotes-title';
      title.textContent = assumptionTitle || 'Notes — Lite applied assumptions';
      aside.appendChild(title);
      var list = document.createElement('ol');
      pack.hints.forEach(function (hint) {
        var li = document.createElement('li');
        li.textContent = hint;
        list.appendChild(li);
      });
      aside.appendChild(list);
      pack.host.appendChild(aside);
    });
  }

  function applyHints() {
    scheduled = false;
    clearRendered();
    items.forEach(function (item) {
      var field = document.getElementById(item.id);
      if (!field) return;
      var group = field.closest('.form-group') || field.parentElement;
      if (!group) return;
      var hint = document.createElement('small');
      hint.className = 'help-block ynow-param-missing-hint';
      hint.setAttribute('role', 'status');
      hint.style.color = '#8a4b08';
      hint.textContent = item.hint;
      group.appendChild(hint);
    });
    renderFootnotes();
    focusPending();
  }
  function focusPending() {
    if (!pendingFocus) return;
    var field = document.getElementById(pendingFocus);
    if (!field) return;
    var panes = [], pane = field.closest('.tab-pane');
    while (pane) { panes.unshift(pane); pane = pane.parentElement.closest('.tab-pane'); }
    panes.forEach(function (p) {
      var link = document.querySelector('a[href="#' + p.id + '"]');
      if (link && window.jQuery) window.jQuery(link).tab('show');
    });
    field.scrollIntoView({block: 'center', behavior: 'smooth'});
    field.focus();
    pendingFocus = null;
  }
  function schedule() {
    if (scheduled) return;
    scheduled = true;
    window.requestAnimationFrame(applyHints);
  }
  function isSelfManagedNode(node) {
    return node.nodeType === 1 && (
      node.classList.contains('ynow-param-missing-hint') ||
      node.classList.contains('ynow-lite-assumption-footnotes')
    );
  }
  function start() {
    if (!window.Shiny) { window.setTimeout(start, 100); return; }
    Shiny.addCustomMessageHandler('ynowParameterGuidance', function (message) {
      items = message.items || [];
      assumptions = message.assumptions || [];
      assumptionTitle = message.assumptionTitle || '';
      applyHints();
    });
    // Ignore mutations made by the hints/footnotes themselves, avoiding an observer loop.
    new MutationObserver(function (changes) {
      var changed = changes.some(function (change) {
        return Array.prototype.some.call(change.addedNodes, function (node) {
          return node.nodeType === 1 && !isSelfManagedNode(node);
        });
      });
      if (changed) schedule();
    }).observe(document.body, {childList: true, subtree: true});
    document.addEventListener('click', function (event) {
      var button = event.target.closest('.ynow-fill-parameter');
      if (!button) return;
      event.preventDefault();
      pendingFocus = button.getAttribute('data-ynow-fill-id');
      if (document.body.classList.contains('ynow-lite') && typeof window.__ynowApplyLiteMode === 'function') {
        window.__ynowApplyLiteMode(false, {navigate: false});
      }
      if (typeof window.ynowGotoTab === 'function') window.ynowGotoTab(button.getAttribute('data-ynow-fill-tab'));
      window.setTimeout(focusPending, 150);
    });
    // Only a real keyboard/spinner edit may become a Full-mode manual fiscal
    // fact. Shiny's programmatic update events have isTrusted=false.
    document.addEventListener('change', function (event) {
      var field = event.target;
      if (!event.isTrusted || !field || !field.id || field.type === 'hidden') return;
      if (!/^((mod_(fcf|ddm|ri|pb|nav|rel|sotp)-)|bridge_)/.test(field.id)) return;
      var value = Number(field.value);
      if (!Number.isFinite(value) || !window.Shiny || !Shiny.setInputValue) return;
      Shiny.setInputValue('ynow_manual_financial',
        {id: field.id, value: value, nonce: Date.now()}, {priority: 'event'});
    }, true);
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
  else start();
})();
