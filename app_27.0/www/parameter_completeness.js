(function () {
  'use strict';
  var items = [], pendingFocus = null, scheduled = false;
  function applyHints() {
    scheduled = false;
    document.querySelectorAll('.ynow-param-missing-hint').forEach(function (node) { node.remove(); });
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
  function start() {
    if (!window.Shiny) { window.setTimeout(start, 100); return; }
    Shiny.addCustomMessageHandler('ynowParameterGuidance', function (message) {
      items = message.items || [];
      applyHints();
    });
    // Ignore mutations made by the hints themselves, avoiding an observer loop.
    new MutationObserver(function (changes) {
      var changed = changes.some(function (change) {
        return Array.prototype.some.call(change.addedNodes, function (node) {
          return node.nodeType === 1 && !node.classList.contains('ynow-param-missing-hint');
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
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
  else start();
})();
