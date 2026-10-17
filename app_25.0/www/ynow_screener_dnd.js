/* Custom screen drag-and-drop (SortableJS). Quote-safe: no R embedding. */
(function () {
  'use strict';

  var STORAGE_KEY = 'ynow_scr_conditions_v1';
  var availSortable = null;
  var selectedSortable = null;
  var ready = false;

  function $(sel, root) {
    return (root || document).querySelector(sel);
  }

  function $all(sel, root) {
    return Array.prototype.slice.call((root || document).querySelectorAll(sel));
  }

  function parseCard(el) {
    if (!el || !el.getAttribute) return null;
    var id = el.getAttribute('data-scr-id') || '';
    var field = el.getAttribute('data-scr-field') || '';
    var op = el.getAttribute('data-scr-op') || 'gte';
    var valueEl = el.querySelector('.ynow-scr-card-value');
    var value = valueEl
      ? parseFloat(valueEl.value)
      : parseFloat(el.getAttribute('data-scr-value'));
    if (!id || !field) return null;
    if (!isFinite(value)) value = parseFloat(el.getAttribute('data-scr-value'));
    return { id: id, field: field, op: op, value: value };
  }

  function parkHint(list) {
    if (!list) return;
    var hint = list.querySelector('.ynow-scr-drop-hint');
    if (hint) list.appendChild(hint);
  }

  function emitPayload() {
    var list = $('#scr_selected_list');
    if (!list) return;
    parkHint(list);
    if (typeof Shiny === 'undefined' || !Shiny.setInputValue) return;
    var cards = $all('.ynow-scr-card', list).map(parseCard).filter(Boolean);
    list.classList.toggle('has-items', cards.length > 0);
    Shiny.setInputValue('scr_dnd_payload', JSON.stringify(cards), { priority: 'event' });
  }

  function selectedIds() {
    return $all('#scr_selected_list .ynow-scr-card').map(function (el) {
      return el.getAttribute('data-scr-id');
    });
  }

  function cloneCardFromTemplate(id) {
    var src = $('#scr_avail_list .ynow-scr-card[data-scr-id="' + id + '"]');
    if (!src) return null;
    var clone = src.cloneNode(true);
    clone.classList.remove('is-visible');
    clone.removeAttribute('style');
    var addBtn = clone.querySelector('.ynow-scr-card-add');
    if (addBtn) addBtn.parentNode.removeChild(addBtn);
    var actions = clone.querySelector('.ynow-scr-card-actions');
    if (actions) {
      var val = document.createElement('input');
      val.type = 'number';
      val.className = 'ynow-scr-card-value';
      val.step = 'any';
      val.value = clone.getAttribute('data-scr-value') || '';
      var rm = document.createElement('button');
      rm.type = 'button';
      rm.className = 'ynow-scr-card-remove';
      rm.setAttribute('aria-label', 'Remove');
      rm.textContent = 'x';
      actions.appendChild(val);
      actions.appendChild(rm);
    }
    return clone;
  }

  function addById(id) {
    if (!id) return;
    if (selectedIds().indexOf(id) >= 0) {
      emitPayload();
      return;
    }
    var list = $('#scr_selected_list');
    if (!list) return;
    var clone = cloneCardFromTemplate(id);
    if (!clone) return;
    var hint = list.querySelector('.ynow-scr-drop-hint');
    if (hint) list.insertBefore(clone, hint);
    else list.appendChild(clone);
    emitPayload();
  }

  function clearSelected() {
    var list = $('#scr_selected_list');
    if (!list) return;
    $all('.ynow-scr-card', list).forEach(function (el) {
      el.parentNode.removeChild(el);
    });
    emitPayload();
  }

  function setSelectedFromPayload(cards) {
    clearSelected();
    if (!Array.isArray(cards)) return;
    cards.forEach(function (c) {
      if (!c || !c.id) return;
      addById(c.id);
      var el = $('#scr_selected_list .ynow-scr-card[data-scr-id="' + c.id + '"]');
      if (el && isFinite(c.value)) {
        el.setAttribute('data-scr-value', String(c.value));
        var inp = el.querySelector('.ynow-scr-card-value');
        if (inp) inp.value = String(c.value);
      }
    });
    emitPayload();
  }

  function filterVisible(category, pack) {
    $all('#scr_avail_list .ynow-scr-card').forEach(function (el) {
      var cat = el.getAttribute('data-scr-category') || '';
      var pk = el.getAttribute('data-scr-pack') || '';
      var show = cat === category;
      if (show && category === 'masters') {
        show = !pack || pk === pack;
      }
      el.classList.toggle('is-visible', !!show);
    });
    var packRow = $('#scr_pack_row');
    if (packRow) {
      packRow.style.display = category === 'masters' ? '' : 'none';
    }
  }

  function setActiveCat(cat) {
    $all('.ynow-scr-cat-btn').forEach(function (btn) {
      btn.classList.toggle('is-active', btn.getAttribute('data-scr-cat') === cat);
    });
    var firstPack = null;
    if (cat === 'masters') {
      var packs = $all('.ynow-scr-pack-btn');
      packs.forEach(function (btn, i) {
        if (i === 0) firstPack = btn.getAttribute('data-scr-pack');
        btn.classList.toggle('is-active', i === 0);
      });
    }
    filterVisible(cat, firstPack);
  }

  function setActivePack(pack) {
    $all('.ynow-scr-pack-btn').forEach(function (btn) {
      btn.classList.toggle('is-active', btn.getAttribute('data-scr-pack') === pack);
    });
    filterVisible('masters', pack);
  }

  function bindSortables() {
    if (typeof Sortable === 'undefined') return;
    var avail = $('#scr_avail_list');
    var selected = $('#scr_selected_list');
    if (!avail || !selected) return;
    if (availSortable) {
      try { availSortable.destroy(); } catch (e) {}
      availSortable = null;
    }
    if (selectedSortable) {
      try { selectedSortable.destroy(); } catch (e) {}
      selectedSortable = null;
    }
    availSortable = Sortable.create(avail, {
      group: { name: 'ynow-scr', pull: 'clone', put: false },
      sort: false,
      animation: 150,
      draggable: '.ynow-scr-card.is-visible',
      filter: '.ynow-scr-card-add',
      preventOnFilter: false,
      onClone: function (evt) {
        var item = evt.clone;
        if (!item) return;
        item.classList.remove('is-visible');
        var addBtn = item.querySelector('.ynow-scr-card-add');
        if (addBtn) addBtn.parentNode.removeChild(addBtn);
        var actions = item.querySelector('.ynow-scr-card-actions');
        if (actions && !item.querySelector('.ynow-scr-card-value')) {
          var val = document.createElement('input');
          val.type = 'number';
          val.className = 'ynow-scr-card-value';
          val.step = 'any';
          val.value = item.getAttribute('data-scr-value') || '';
          var rm = document.createElement('button');
          rm.type = 'button';
          rm.className = 'ynow-scr-card-remove';
          rm.setAttribute('aria-label', 'Remove');
          rm.textContent = 'x';
          actions.appendChild(val);
          actions.appendChild(rm);
        }
      }
    });
    selectedSortable = Sortable.create(selected, {
      group: { name: 'ynow-scr', pull: true, put: true },
      animation: 150,
      draggable: '.ynow-scr-card',
      filter: '.ynow-scr-drop-hint,.ynow-scr-card-value,.ynow-scr-card-remove',
      preventOnFilter: false,
      onAdd: function (evt) {
        var id = evt.item && evt.item.getAttribute('data-scr-id');
        var dups = $all('#scr_selected_list .ynow-scr-card[data-scr-id="' + id + '"]');
        if (dups.length > 1) {
          evt.item.parentNode.removeChild(evt.item);
        }
        emitPayload();
      },
      onUpdate: emitPayload,
      onRemove: emitPayload,
      onStart: function () {
        selected.classList.add('ynow-scr-drop-active');
      },
      onEnd: function () {
        selected.classList.remove('ynow-scr-drop-active');
        emitPayload();
      }
    });
  }

  function bindClicks() {
    var root = $('#ynow_screener_dnd');
    if (!root || root.getAttribute('data-ynow-scr-bound') === '1') return;
    root.setAttribute('data-ynow-scr-bound', '1');
    root.addEventListener('click', function (ev) {
      var t = ev.target;
      if (!t) return;
      var catBtn = t.closest ? t.closest('.ynow-scr-cat-btn') : null;
      if (catBtn) {
        setActiveCat(catBtn.getAttribute('data-scr-cat'));
        return;
      }
      var packBtn = t.closest ? t.closest('.ynow-scr-pack-btn') : null;
      if (packBtn) {
        setActivePack(packBtn.getAttribute('data-scr-pack'));
        return;
      }
      var addBtn = t.closest ? t.closest('.ynow-scr-card-add') : null;
      if (addBtn) {
        var card = addBtn.closest('.ynow-scr-card');
        if (card) addById(card.getAttribute('data-scr-id'));
        return;
      }
      var rmBtn = t.closest ? t.closest('.ynow-scr-card-remove') : null;
      if (rmBtn) {
        var selCard = rmBtn.closest('.ynow-scr-card');
        if (selCard && selCard.parentNode) selCard.parentNode.removeChild(selCard);
        emitPayload();
        return;
      }
      if (t.id === 'scr_clear' || (t.closest && t.closest('#scr_clear'))) {
        clearSelected();
        return;
      }
      if (t.id === 'scr_save' || (t.closest && t.closest('#scr_save'))) {
        try {
          var list = $('#scr_selected_list');
          var cards = $all('.ynow-scr-card', list).map(parseCard).filter(Boolean);
          localStorage.setItem(STORAGE_KEY, JSON.stringify(cards));
          if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
            Shiny.setInputValue('scr_dnd_saved', Date.now(), { priority: 'event' });
          }
        } catch (e) {}
        return;
      }
    });
    root.addEventListener('change', function (ev) {
      var t = ev.target;
      if (t && t.classList && t.classList.contains('ynow-scr-card-value')) {
        var card = t.closest('.ynow-scr-card');
        if (card) card.setAttribute('data-scr-value', t.value);
        emitPayload();
      }
    });
  }

  function boot() {
    var root = $('#ynow_screener_dnd');
    if (!root) return;
    // Allow a second boot after lazy mount even if an earlier empty pass ran.
    if (ready && root.getAttribute('data-ynow-scr-booted') === '1') return;
    bindClicks();
    bindSortables();
    setActiveCat('masters');
    try {
      var raw = localStorage.getItem(STORAGE_KEY);
      if (raw) {
        var parsed = JSON.parse(raw);
        if (Array.isArray(parsed) && parsed.length) setSelectedFromPayload(parsed);
      }
    } catch (e) {}
    emitPayload();
    root.setAttribute('data-ynow-scr-booted', '1');
    ready = true;
  }

  function onReady(fn) {
    if (document.readyState === 'loading') {
      document.addEventListener('DOMContentLoaded', fn);
    } else {
      fn();
    }
  }

  function tryBoot() {
    if (typeof Sortable === 'undefined') return;
    var root = $('#ynow_screener_dnd');
    if (!root) return;
    if (ready && root.getAttribute('data-ynow-scr-booted') === '1') return;
    boot();
  }

  onReady(function () {
    if (typeof Sortable !== 'undefined') tryBoot();
    else {
      var tries = 0;
      var timer = setInterval(function () {
        tries += 1;
        if (typeof Sortable !== 'undefined') {
          clearInterval(timer);
          tryBoot();
        } else if (tries > 40) {
          clearInterval(timer);
        }
      }, 100);
    }
  });

  // Blue Chip page mounts screener via lazy renderUI — watch for the root node.
  if (typeof MutationObserver !== 'undefined') {
    var mo = new MutationObserver(function () {
      if ($('#ynow_screener_dnd') && (!ready || !$('#scr_avail_list'))) {
        tryBoot();
      }
    });
    onReady(function () {
      mo.observe(document.documentElement, { childList: true, subtree: true });
    });
  }

  if (typeof Shiny !== 'undefined' && Shiny.addCustomMessageHandler) {
    Shiny.addCustomMessageHandler('ynowScrDndClear', function () {
      clearSelected();
    });
    Shiny.addCustomMessageHandler('ynowScrDndSet', function (cards) {
      setSelectedFromPayload(cards || []);
    });
  } else {
    document.addEventListener('shiny:connected', function () {
      if (window.Shiny && Shiny.addCustomMessageHandler) {
        Shiny.addCustomMessageHandler('ynowScrDndClear', function () {
          clearSelected();
        });
        Shiny.addCustomMessageHandler('ynowScrDndSet', function (cards) {
          setSelectedFromPayload(cards || []);
        });
      }
      tryBoot();
      if (ready) emitPayload();
    });
  }
  document.addEventListener('shiny:value', function (ev) {
    if (ev && ev.target && ev.target.id && String(ev.target.id).indexOf('ynow_lazy_host_bluechip') === 0) {
      setTimeout(tryBoot, 0);
    }
  });
})();
