/**
 * Ticker / Stock Code auto-uppercase (Full + Lite).
 * Case-insensitive typing → always display & Shiny value as uppercase English.
 * Numeric TW codes (e.g. 2330) are unchanged; letters/suffixes become A–Z.
 * Does not switch market on typing (US 4-digit → TW still Search-only).
 */
(function () {
  if (typeof document === 'undefined') return;
  if (document.documentElement.getAttribute('data-ynow-ticker-upper') === '1') return;
  document.documentElement.setAttribute('data-ynow-ticker-upper', '1');

  var TEXT_IDS = {
    sc: true,
    lab_cluster_focus: true,
    pa_ticker: true,
    pa_quick_ticker: true
  };

  var SELECTIZE_IDS = {
    beta_peers: true,
    beta_bench: true
  };

  function isTickerTextInput(el) {
    if (!el || !el.id) return false;
    if (TEXT_IDS[el.id]) return true;
    if (el.classList && el.classList.contains('ynow-ticker-input')) return true;
    return false;
  }

  function isTickerSelectizeTyping(el) {
    if (!el || el.tagName !== 'INPUT') return false;
    var id = el.id || '';
    // Selectize typing field: "{inputId}-selectized"
    if (id.slice(-11) === '-selectized') {
      var base = id.slice(0, -11);
      if (SELECTIZE_IDS[base]) return true;
    }
    var root = el.closest ? el.closest('.selectize-control') : null;
    if (!root) return false;
    for (var sid in SELECTIZE_IDS) {
      if (!Object.prototype.hasOwnProperty.call(SELECTIZE_IDS, sid)) continue;
      var sel = document.getElementById(sid);
      if (sel && root.contains(sel)) return true;
      // sibling structure: select then .selectize-control
      if (sel && sel.nextElementSibling === root) return true;
    }
    return false;
  }

  function forceUpper(el) {
    if (!el) return false;
    var v = el.value == null ? '' : String(el.value);
    var up = v.toUpperCase();
    if (up === v) return false;
    var start = el.selectionStart;
    var end = el.selectionEnd;
    el.value = up;
    try {
      if (typeof start === 'number' && typeof end === 'number') {
        el.setSelectionRange(start, end);
      }
    } catch (eCaret) {}
    return true;
  }

  // Capture phase so Shiny's bubble handler reads the uppercased value.
  document.addEventListener(
    'input',
    function (e) {
      var t = e && e.target;
      if (!t) return;
      if (isTickerTextInput(t) || isTickerSelectizeTyping(t)) {
        forceUpper(t);
      }
    },
    true
  );

  document.addEventListener(
    'blur',
    function (e) {
      var t = e && e.target;
      if (!t) return;
      if (isTickerTextInput(t) || isTickerSelectizeTyping(t)) {
        forceUpper(t);
      }
    },
    true
  );

  function hookSelectize(id) {
    var el = document.getElementById(id);
    if (!el || !el.selectize || el.selectize.__ynowTickerUpper) return;
    var s = el.selectize;
    s.__ynowTickerUpper = true;

    var prevCreate = s.settings.create;
    s.settings.create = function (input, callback) {
      var u = String(input == null ? '' : input).trim().toUpperCase();
      if (!u) return;
      if (typeof prevCreate === 'function') {
        return prevCreate.call(this, u, function (data) {
          if (!data) {
            if (typeof callback === 'function') callback();
            return;
          }
          if (typeof data === 'string') {
            data = { value: data.toUpperCase(), text: data.toUpperCase() };
          } else if (typeof data === 'object') {
            var vv = String(data.value != null ? data.value : u).toUpperCase();
            data.value = vv;
            data.text = String(data.text != null ? data.text : vv).toUpperCase();
          }
          if (typeof callback === 'function') callback(data);
        });
      }
      if (typeof callback === 'function') {
        callback({ value: u, text: u });
      } else {
        return { value: u, text: u };
      }
    };

    s.on('item_add', function (value) {
      var u = String(value == null ? '' : value).trim().toUpperCase();
      if (!u || u === value) return;
      try {
        s.removeItem(value, true);
        if (!s.options[u]) s.addOption({ value: u, text: u });
        s.addItem(u, true);
      } catch (eAdd) {}
    });
  }

  function hookAllSelectize() {
    for (var id in SELECTIZE_IDS) {
      if (Object.prototype.hasOwnProperty.call(SELECTIZE_IDS, id)) {
        try {
          hookSelectize(id);
        } catch (eHook) {}
      }
    }
  }

  function boot() {
    hookAllSelectize();
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', boot);
  } else {
    boot();
  }

  if (window.jQuery) {
    window.jQuery(document).on('shiny:connected shiny:sessioninitialized shiny:value', function () {
      setTimeout(hookAllSelectize, 0);
      setTimeout(hookAllSelectize, 200);
    });
  } else {
    setTimeout(hookAllSelectize, 500);
    setTimeout(hookAllSelectize, 1500);
  }

  // Expose for tests / other scripts
  window.ynowTickerUppercase = {
    textIds: Object.keys(TEXT_IDS),
    selectizeIds: Object.keys(SELECTIZE_IDS),
    forceUpper: forceUpper,
    hookSelectize: hookSelectize
  };
})();
