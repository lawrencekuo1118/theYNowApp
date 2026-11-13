/* Extracted from ynow_ui.R for Home first-paint: cacheable static asset (YN-120). */
        (function () {
          var TAB_PARENT = {
            nav_calculator: 'ynow_menu_cat_asset',
            pb_calculator: 'ynow_menu_cat_asset',
            dcf_calculator: 'ynow_menu_cat_income',
            ddm_calculator: 'ynow_menu_cat_income',
            ri_calculator: 'ynow_menu_cat_income',
            rel_mult_ev: 'ynow_menu_cat_relative',
            rel_multiples_calculator: 'ynow_menu_cat_relative',
            rel_mult_rev: 'ynow_menu_cat_relative',
            sotp_calculator: 'ynow_menu_cat_meta'
          };
          var LAST_BADGE_MAP = null;

          function clearRecBadges(a) {
            if (!a) return;
            a.querySelectorAll('small.ynow-sidebar-badge, small.ynow-rec-badge, .ynow-rec-slot').forEach(function (b) {
              b.remove();
            });
          }

          /* Wrap bare text so flex can shrink the label; otherwise badges clip */
          function ensureShrinkableLabel(a) {
            if (!a) return;
            var nodes = Array.prototype.slice.call(a.childNodes);
            for (var i = 0; i < nodes.length; i++) {
              var n = nodes[i];
              if (n.nodeType !== 3) continue;
              var t = String(n.textContent || '').replace(/\s+/g, ' ').trim();
              if (!t) continue;
              var span = document.createElement('span');
              span.className = 'ynow-menu-label';
              span.textContent = ' ' + t + ' ';
              a.replaceChild(span, n);
            }
          }

          function findChevron(a) {
            if (!a) return null;
            var kids = a.children;
            for (var i = 0; i < kids.length; i++) {
              var el = kids[i];
              if (!el.classList) continue;
              if (el.classList.contains('pull-right-container')) return el;
              if (el.classList.contains('pull-right') &&
                  (el.classList.contains('fa') || el.classList.contains('fas') ||
                   el.classList.contains('far') || el.classList.contains('glyphicon'))) {
                return el;
              }
              if (el.classList.contains('fa-angle-left') || el.classList.contains('fa-angle-down')) {
                return el;
              }
            }
            return null;
          }

          function setRecBadgeOnAnchor(a, role, labels) {
            if (!a) return;
            clearRecBadges(a);
            if (!role || role === 'reference') return;
            ensureShrinkableLabel(a);
            var lab = labels || {};
            var badge = document.createElement('small');
            var cls = 'ynow-rec-secondary';
            var text = lab.secondary || 'Secondary';
            if (role === 'primary') {
              cls = 'ynow-rec-primary';
              text = lab.primary || 'Recommend';
            } else if (role === 'reference') {
              cls = 'ynow-rec-reference';
              text = lab.reference || 'Reference';
            }
            badge.className = 'ynow-sidebar-badge ' + cls;
            badge.textContent = text;
            var slot = document.createElement('span');
            slot.className = 'ynow-rec-slot';
            slot.appendChild(badge);
            var chev = findChevron(a);
            if (chev) a.insertBefore(slot, chev);
            else a.appendChild(slot);
          }

          function setTabBadge(tab, role, labels) {
            var a = document.querySelector('.main-sidebar .sidebar-menu a[data-value="' + tab + '"]') ||
                    document.querySelector('.sidebar-menu a[data-value="' + tab + '"]');
            setRecBadgeOnAnchor(a, role || '', labels);
          }

          function setParentBadges(map, labels) {
            var rolesByParent = {
              ynow_menu_cat_asset: [],
              ynow_menu_cat_income: [],
              ynow_menu_cat_relative: [],
              ynow_menu_cat_meta: []
            };
            Object.keys(TAB_PARENT).forEach(function (tab) {
              var role = (map && map[tab] && map[tab].role) ? String(map[tab].role) : '';
              var pid = TAB_PARENT[tab];
              if (role && rolesByParent[pid]) rolesByParent[pid].push(role);
            });
            Object.keys(rolesByParent).forEach(function (pid) {
              var span = document.getElementById(pid);
              if (!span) return;
              var a = span.closest ? span.closest('a') : null;
              if (!a) a = span.parentElement;
              var roles = rolesByParent[pid] || [];
              var parentRole = '';
              if (roles.indexOf('primary') >= 0) parentRole = 'primary';
              else if (roles.indexOf('secondary') >= 0) parentRole = 'secondary';
              setRecBadgeOnAnchor(a, parentRole, labels);
            });
          }

          function applySidebarBadges(map) {
            LAST_BADGE_MAP = map || null;
            var labels = (map && map.labels) || {};
            var tabs = ['dcf_calculator', 'ddm_calculator', 'pb_calculator', 'rel_mult_ev', 'rel_multiples_calculator', 'rel_mult_rev', 'sotp_calculator', 'ri_calculator', 'nav_calculator'];
            tabs.forEach(function (t) {
              var role = (map && map[t] && map[t].role) ? String(map[t].role) : '';
              if (!role && map && map[t] && map[t].on) role = 'primary';
              setTabBadge(t, role, labels);
            });
            setParentBadges(map, labels);
          }

          function registerBadgeHandler() {
            if (!window.Shiny || !Shiny.addCustomMessageHandler) {
              setTimeout(registerBadgeHandler, 50);
              return;
            }
            Shiny.addCustomMessageHandler('ynowSidebarBadges', applySidebarBadges);
            function pingReady() {
              if (!(window.Shiny && Shiny.setInputValue)) {
                setTimeout(pingReady, 50);
                return;
              }
              Shiny.setInputValue('ynow_sidebar_badges_ready', Date.now(), {priority: 'event'});
            }
            pingReady();
            if (window.jQuery) {
              jQuery(document).on('shiny:connected shiny:sessioninitialized', function () {
                setTimeout(pingReady, 0);
                setTimeout(pingReady, 400);
                if (LAST_BADGE_MAP) applySidebarBadges(LAST_BADGE_MAP);
              });
            }
          }
          registerBadgeHandler();

          /* 美股／台股：搬到三線 toggle 正後方。
             Desktop: absolute left = toggle 右緣（無空隙）。
             Mobile: static flex sibling after toggle (black-bar leftmost chrome). */
          function placeMarketHeaderByToggle() {
            var navbar = document.querySelector('.main-header .navbar');
            var market = document.getElementById('ynow-market-header');
            if (!navbar || !market) return;
            var toggle = null;
            var kids = navbar.children;
            for (var i = 0; i < kids.length; i++) {
              if (kids[i].classList && kids[i].classList.contains('sidebar-toggle')) {
                toggle = kids[i];
                break;
              }
            }
            if (!toggle) toggle = navbar.querySelector('a.sidebar-toggle, .sidebar-toggle');
            if (!toggle) return;
            /* Always re-assert beside toggle — AdminLTE may steal the node back
               into .navbar-custom-menu / .navbar-nav after our first pass. */
            if (!(market.parentNode === navbar && market.previousElementSibling === toggle)) {
              navbar.insertBefore(market, toggle.nextSibling);
            }
            var mobile = window.innerWidth <= 767;
            if (mobile) {
              market.style.setProperty('left', 'auto', 'important');
              market.style.setProperty('right', 'auto', 'important');
              market.style.setProperty('top', 'auto', 'important');
              market.style.setProperty('position', 'static', 'important');
              market.style.setProperty('float', 'none', 'important');
              market.style.setProperty('display', 'flex', 'important');
              market.style.setProperty('align-items', 'center', 'important');
            } else {
              var leftPx = Math.round(toggle.offsetLeft + toggle.offsetWidth);
              if (!(leftPx > 0)) leftPx = Math.round(toggle.offsetWidth) || 44;
              market.style.left = leftPx + 'px';
              market.style.top = '0';
              market.style.position = '';
            }
            market.style.marginLeft = '0';
            market.style.paddingLeft = '0';
            try { placeMobileAppTitle(); } catch (eTitle) {}
          }
          window.ynowPlaceMarketHeaderByToggle = placeMarketHeaderByToggle;
          /* Mobile: app title viewport-centered in the gap; scale font to fit — never cover chrome. */
          function placeMobileAppTitle() {
            var logo = document.querySelector('.main-header .logo');
            if (!logo) return;
            var title = logo.querySelector('.ynow-app-title');
            var base = title ? title.querySelector('.ynow-app-title-base') : null;
            var fillInner = title ? title.querySelector('.ynow-app-title-fill-inner') : null;
            if (window.innerWidth > 767) {
              logo.style.left = '';
              logo.style.right = '';
              if (title) {
                title.style.maxWidth = '';
                title.style.fontSize = '';
              }
              if (base) base.style.fontSize = '';
              if (fillInner) fillInner.style.fontSize = '';
              return;
            }
            logo.style.left = '0px';
            logo.style.right = '0px';
            if (!title || !base) return;
            var mkt = document.getElementById('ynow-market-header');
            var custom = document.querySelector('.main-header .navbar-custom-menu');
            var ptsHdr = document.getElementById('ynow-pts-header');
            var langHdr = document.querySelector('.main-header .ynow-lang-header');
            var gap = 10;
            var leftClear = mkt ? Math.round(mkt.getBoundingClientRect().right) : 90;
            /* Right clear: protect locale + FYN/PYN pts + mark — title must not paint over them. */
            var rightEdge = window.innerWidth;
            var pushLeft = function (el) {
              if (!el) return;
              try {
                var cs = window.getComputedStyle(el);
                if (cs.display === 'none' || cs.visibility === 'hidden') return;
                var r = el.getBoundingClientRect();
                if (r.width > 0 && r.left > 0) rightEdge = Math.min(rightEdge, r.left);
              } catch (ePush) {}
            };
            pushLeft(ptsHdr);
            pushLeft(langHdr);
            pushLeft(document.querySelector('.main-header .ynow-auth-header'));
            if (custom) pushLeft(custom);
            var rightClear = Math.max(80, Math.round(window.innerWidth - rightEdge));
            var maxW = Math.max(64, window.innerWidth - leftClear - rightClear - gap * 2);
            title.style.maxWidth = maxW + 'px';
            title.style.overflow = 'hidden';
            title.style.textOverflow = 'clip';
            /* Prefer readable size; shrink proportionally so full version string stays visible */
            var fs = 16;
            var minFs = 9;
            base.style.fontSize = fs + 'px';
            if (fillInner) fillInner.style.fontSize = fs + 'px';
            title.style.fontSize = fs + 'px';
            var guard = 0;
            while (base.scrollWidth > maxW + 1 && fs > minFs && guard < 24) {
              fs -= 0.5;
              base.style.fontSize = fs + 'px';
              if (fillInner) fillInner.style.fontSize = fs + 'px';
              title.style.fontSize = fs + 'px';
              guard++;
            }
          }
          window.placeMobileAppTitle = placeMobileAppTitle;

          /* Header title = load progress bar (fills with boot / Shiny busy / withProgress) */
          document.title = 'The YNow App';

          (function initYnowTitleLoadBar() {
            var pct = 0;
            var firstIdleDone = false;
            var busyTimer = null;
            var completeTimer = null;
            function titleEl() {
              return document.getElementById('ynow_app_title') ||
                document.querySelector('.main-header .logo .ynow-app-title');
            }
            function setPct(n, opts) {
              opts = opts || {};
              n = Math.max(0, Math.min(100, Number(n) || 0));
              if (!opts.force && n < pct && pct >= 100 && firstIdleDone && !opts.busy) {
                return;
              }
              if (!opts.force && n < pct && !opts.busy) {
                /* allow only forward during boot; busy may reset */
                if (!(opts.allowDrop)) return;
              }
              pct = n;
              var el = titleEl();
              if (!el) return;
              el.style.setProperty('--ynow-load-pct', pct + '%');
              el.setAttribute('aria-valuenow', String(Math.round(pct)));
              if (pct < 100) {
                el.classList.add('is-loading');
                el.classList.remove('is-complete');
              }
            }
            function markComplete() {
              setPct(100, { force: true });
              var el = titleEl();
              if (!el) return;
              el.classList.remove('is-loading');
              el.classList.add('is-complete');
              el.setAttribute('aria-valuenow', '100');
              firstIdleDone = true;
            }
            function readShinyProgressPct() {
              var bar = document.querySelector('.shiny-progress .progress-bar');
              if (!bar) return null;
              var styleW = bar.style.width || '';
              var m = String(styleW).match(/([0-9.]+)[ ]*%/);
              if (m) return Math.max(0, Math.min(100, parseFloat(m[1])));
              try {
                var parent = bar.parentElement;
                if (parent && parent.clientWidth > 0) {
                  return Math.max(0, Math.min(100, (bar.clientWidth / parent.clientWidth) * 100));
                }
              } catch (e) {}
              return null;
            }
            function onBusy() {
              if (completeTimer) {
                clearTimeout(completeTimer);
                completeTimer = null;
              }
              var el = titleEl();
              if (el) {
                el.classList.add('is-loading');
                el.classList.remove('is-complete');
              }
              var sp = readShinyProgressPct();
              if (sp != null) {
                setPct(Math.max(8, sp), { force: true, busy: true, allowDrop: true });
              } else {
                setPct(Math.max(pct < 100 ? pct : 12, 12), { force: true, busy: true, allowDrop: true });
              }
              if (busyTimer) clearInterval(busyTimer);
              busyTimer = setInterval(function () {
                var p = readShinyProgressPct();
                if (p != null) {
                  setPct(Math.max(8, Math.min(98, p)), { force: true, busy: true, allowDrop: true });
                  return;
                }
                if (pct < 90) setPct(pct + 1.2, { force: true, busy: true });
              }, 180);
            }
            function onIdle() {
              if (busyTimer) {
                clearInterval(busyTimer);
                busyTimer = null;
              }
              setPct(100, { force: true });
              if (completeTimer) clearTimeout(completeTimer);
              completeTimer = setTimeout(markComplete, 320);
            }
            setPct(6, { force: true });
            function onDomReady() { setPct(28); }
            function onWinLoad() { setPct(48); }
            if (document.readyState === 'loading') {
              document.addEventListener('DOMContentLoaded', onDomReady);
            } else {
              onDomReady();
            }
            if (document.readyState === 'complete') {
              onWinLoad();
            } else {
              window.addEventListener('load', onWinLoad);
            }
            function bindShinyLoad() {
              if (!window.jQuery) {
                setTimeout(bindShinyLoad, 40);
                return;
              }
              var $doc = jQuery(document);
              $doc.on('shiny:connected', function () { setPct(66); });
              $doc.on('shiny:sessioninitialized', function () { setPct(82); });
              $doc.on('shiny:busy', onBusy);
              $doc.on('shiny:idle', onIdle);
              /* If already idle after connect, complete */
              setTimeout(function () {
                if (!firstIdleDone && pct >= 66) onIdle();
              }, 1800);
            }
            bindShinyLoad();
            if (typeof MutationObserver !== 'undefined') {
              var obs = new MutationObserver(function () {
                var p = readShinyProgressPct();
                if (p == null) return;
                var el = titleEl();
                if (el && el.classList.contains('is-loading')) {
                  setPct(Math.max(8, Math.min(99, p)), { force: true, busy: true, allowDrop: true });
                }
              });
              obs.observe(document.documentElement, {
                childList: true,
                subtree: true,
                attributes: true,
                attributeFilter: ['style', 'class']
              });
            }
          })();

          /* Sidebar tab label: animated 「…」 while that tab's feature is loading */
          (function initYnowSidebarTabBusyDots() {
            var sourcesByTab = {};
            var shinyArmTimer = null;
            var shinyTab = null;

            function remapTab(tab) {
              tab = String(tab || '');
              if (tab === 'business_breakdown_lab') return 'company_advance';
              return tab;
            }
            function currentTab() {
              if (typeof window.ynowCurrentSidebarTab === 'function') {
                try { return remapTab(window.ynowCurrentSidebarTab() || ''); } catch (e0) {}
              }
              var active = document.querySelector('.sidebar-menu li.active > a[data-value]');
              if (active) return remapTab(active.getAttribute('data-value') || '');
              var pane = document.querySelector('.tab-content > .tab-pane.active');
              if (pane && pane.id && pane.id.indexOf('shiny-tab-') === 0) {
                return remapTab(pane.id.slice('shiny-tab-'.length));
              }
              return '';
            }
            function tabAnchor(tab) {
              tab = remapTab(tab);
              if (!tab) return null;
              return document.querySelector('.main-sidebar .sidebar-menu a[data-value="' + tab + '"]') ||
                     document.querySelector('.sidebar-menu a[data-value="' + tab + '"]');
            }
            function ensureDots(a) {
              if (!a) return null;
              var existing = null;
              for (var i = 0; i < a.children.length; i++) {
                if (a.children[i].classList && a.children[i].classList.contains('ynow-tab-busy-dots')) {
                  existing = a.children[i];
                  break;
                }
              }
              if (existing) return existing;
              var dots = document.createElement('span');
              dots.className = 'ynow-tab-busy-dots';
              dots.setAttribute('aria-hidden', 'true');
              dots.innerHTML = '<span>.</span><span>.</span><span>.</span>';
              var before = null;
              for (var j = 0; j < a.children.length; j++) {
                var el = a.children[j];
                if (!el.classList) continue;
                if (el.classList.contains('ynow-rec-slot') ||
                    el.classList.contains('pull-right-container') ||
                    el.classList.contains('fa-angle-left') ||
                    el.classList.contains('fa-angle-down')) {
                  before = el;
                  break;
                }
              }
              if (before) a.insertBefore(dots, before);
              else a.appendChild(dots);
              return dots;
            }
            function syncHeaderLogoSpin() {
              var mark = document.getElementById('ynow_header_logo_home');
              if (!mark || !mark.classList) return;
              var tab = currentTab();
              var busy = !!(tab && sourcesByTab[tab] && Object.keys(sourcesByTab[tab]).length > 0);
              /* Pending nav/refresh: spin even before the pane switch settles. */
              if (!busy) {
                Object.keys(sourcesByTab).forEach(function (t) {
                  var s = sourcesByTab[t] || {};
                  if (s.refresh || s.nav) busy = true;
                });
              }
              mark.classList.toggle('ynow-logo-spinning', busy);
              if (!busy) mark.classList.remove('ynow-logo-spin-fast');
              try { mark.setAttribute('aria-busy', busy ? 'true' : 'false'); } catch (eSpin) {}
            }
            function releaseFastRefresh(tab) {
              var mark = document.getElementById('ynow_header_logo_home');
              if (!mark || !mark.classList || !mark.classList.contains('ynow-logo-spin-fast')) return;
              tab = remapTab(tab);
              if (!tab || tabHasOtherBusy(tab)) return;
              if (sourcesByTab[tab] && sourcesByTab[tab].refresh) {
                setTabBusy(tab, false, 'refresh');
              }
            }
            function tabHasOtherBusy(tab) {
              var src = (tab && sourcesByTab[tab]) ? sourcesByTab[tab] : {};
              var keys = Object.keys(src);
              for (var i = 0; i < keys.length; i++) {
                if (keys[i] !== 'refresh' && src[keys[i]]) return true;
              }
              return false;
            }
            function clearOtherTabBusy(keep) {
              keep = remapTab(keep);
              Object.keys(sourcesByTab).forEach(function (t) {
                if (keep && t === keep) return;
                if (!sourcesByTab[t]) return;
                delete sourcesByTab[t];
                applyVisual(t);
              });
            }
            function focusCurrentTabBusy(keep) {
              keep = remapTab(keep);
              if (keep) setTabBusy(keep, true, 'refresh');
              clearOtherTabBusy(keep);
            }
            function applyVisual(tab) {
              tab = remapTab(tab);
              var a = tabAnchor(tab);
              if (!a) {
                syncHeaderLogoSpin();
                return;
              }
              var src = sourcesByTab[tab] || {};
              var busy = Object.keys(src).length > 0;
              ensureDots(a);
              a.classList.toggle('ynow-tab-is-busy', busy);
              var li = a.closest ? a.closest('li') : a.parentElement;
              if (li && li.classList) li.classList.toggle('ynow-tab-is-busy', busy);
              if (busy) {
                try { a.setAttribute('aria-busy', 'true'); } catch (e1) {}
              } else {
                try { a.removeAttribute('aria-busy'); } catch (e2) {}
              }
              syncHeaderLogoSpin();
            }
            function setTabBusy(tab, on, source) {
              tab = remapTab(tab);
              source = String(source || 'server');
              if (!tab) return;
              var hadOther = tabHasOtherBusy(tab);
              if (!sourcesByTab[tab]) sourcesByTab[tab] = {};
              if (on) {
                sourcesByTab[tab][source] = true;
              } else {
                delete sourcesByTab[tab][source];
                if (!Object.keys(sourcesByTab[tab]).length) delete sourcesByTab[tab];
              }
              applyVisual(tab);
              if (!on && source !== 'refresh' && hadOther && !tabHasOtherBusy(tab)) {
                releaseFastRefresh(tab);
              }
            }
            function clearSource(source) {
              Object.keys(sourcesByTab).forEach(function (tab) {
                if (sourcesByTab[tab] && sourcesByTab[tab][source]) {
                  var hadOther = tabHasOtherBusy(tab);
                  delete sourcesByTab[tab][source];
                  if (!Object.keys(sourcesByTab[tab]).length) delete sourcesByTab[tab];
                  applyVisual(tab);
                  if (source !== 'refresh' && hadOther && !tabHasOtherBusy(tab)) {
                    releaseFastRefresh(tab);
                  }
                }
              });
            }
            function hasShinyProgress() {
              return !!document.querySelector('.shiny-progress, .shiny-progress-container .progress');
            }
            function onShinyBusy() {
              /* Show 「…」 as soon as the loading screen / busy state starts — no arm delay. */
              if (shinyArmTimer) {
                clearTimeout(shinyArmTimer);
                shinyArmTimer = null;
              }
              var tab = currentTab();
              if (!tab) return;
              shinyTab = tab;
              setTabBusy(tab, true, 'shiny');
            }
            function onShinyIdle() {
              if (shinyArmTimer) {
                clearTimeout(shinyArmTimer);
                shinyArmTimer = null;
              }
              if (shinyTab) {
                setTabBusy(shinyTab, false, 'shiny');
                shinyTab = null;
              }
              var idleTab = currentTab();
              var hadOther = tabHasOtherBusy(idleTab);
              clearSource('shiny');
              clearSource('nav');
              clearSource('progress');
              var markIdle = document.getElementById('ynow_header_logo_home');
              var fastIdle = !!(markIdle && markIdle.classList &&
                markIdle.classList.contains('ynow-logo-spin-fast'));
              /* Fast spin outlives an early shiny:idle that has not seen
                 feature work yet. It stops once that work drops off. */
              if (!fastIdle) clearSource('refresh');
              else if (hadOther && !tabHasOtherBusy(idleTab)) releaseFastRefresh(idleTab);
              syncHeaderLogoSpin();
            }
            function onSidebarTabClick(ev) {
              var t = ev.target;
              if (!t || !t.closest) return;
              var a = t.closest('.sidebar-menu a[data-value]');
              if (!a) return;
              var tab = remapTab(a.getAttribute('data-value') || '');
              if (!tab) return;
              /* Immediate feedback on the tab that is about to load. */
              setTabBusy(tab, true, 'nav');
            }
            function watchProgressDom() {
              if (typeof MutationObserver === 'undefined') return;
              var root = document.body || document.documentElement;
              if (!root || root.nodeType !== 1) return;
              var armed = false;
              var obs = new MutationObserver(function () {
                if (hasShinyProgress()) {
                  if (armed) return;
                  armed = true;
                  var tab = currentTab();
                  if (tab) setTabBusy(tab, true, 'progress');
                } else if (armed) {
                  armed = false;
                  clearSource('progress');
                }
              });
              try {
                obs.observe(root, { childList: true, subtree: true });
              } catch (eWatch) { /* body may be unavailable mid-parse */ }
            }
            function register() {
              if (!window.Shiny || !Shiny.addCustomMessageHandler) {
                setTimeout(register, 50);
                return;
              }
              Shiny.addCustomMessageHandler('ynowTabBusy', function (payload) {
                if (!payload) return;
                var tab = payload.tab;
                var busy = payload.busy === true || payload.busy === 'true' || payload.busy === 1;
                var n = payload.n != null && payload.n !== '' ? Number(payload.n) : NaN;
                var focusN = window.__ynowLogoFocusN || 0;
                if (!busy && focusN && isFinite(n) && n < focusN) return;
                setTabBusy(tab, busy, 'server');
              });
              Shiny.addCustomMessageHandler('ynowBtnBusy', function (payload) {
                if (!payload || !payload.id) return;
                var id = String(payload.id);
                var busy = payload.busy === true || payload.busy === 'true' || payload.busy === 1;
                var label = payload.label != null ? String(payload.label) : '';
                window.__ynowBtnBusy = window.__ynowBtnBusy || {};
                window.__ynowBtnBusy[id] = busy;
                var el = document.getElementById(id);
                if (!el) return;
                var span = null;
                if (payload.label_id) {
                  span = document.getElementById(String(payload.label_id));
                }
                if (!span) {
                  span = document.getElementById('ynow_' + id + '_label');
                }
                if (span) {
                  span.textContent = label;
                } else {
                  var icon = el.querySelector('i');
                  var iconHtml = icon ? icon.outerHTML + ' ' : '';
                  el.innerHTML = iconHtml + label;
                }
                el.classList.toggle('ynow-btn-is-busy', busy);
                try {
                  el.setAttribute('aria-busy', busy ? 'true' : 'false');
                } catch (eBusy) {}
              });
              if (window.jQuery) {
                jQuery(document).on('shiny:busy', onShinyBusy);
                jQuery(document).on('shiny:idle', onShinyIdle);
                jQuery(document).on('click', '.sidebar-menu a[data-value]', onSidebarTabClick);
                watchProgressDom();
              } else {
                setTimeout(register, 50);
              }
            }
            function registerLogoSpinHandler() {
              if (!window.Shiny || !Shiny.addCustomMessageHandler) {
                setTimeout(registerLogoSpinHandler, 50);
                return;
              }
              Shiny.addCustomMessageHandler('ynowHeaderLogoSpin', function (payload) {
                var mark = document.getElementById('ynow_header_logo_home');
                if (!mark || !mark.classList) return;
                var on = !!(payload && (payload.spin === true || payload.spin === 'true' || payload.spin === 1));
                var n = payload && payload.n != null && payload.n !== '' ? Number(payload.n) : 0;
                if (!isFinite(n)) n = 0;
                var focusN = window.__ynowLogoFocusN || 0;
                if (!on && focusN && n < focusN) return;
                var tab = payload && payload.tab ? remapTab(payload.tab) : currentTab();
                if (!on && mark.classList.contains('ynow-logo-spin-fast') && tabHasOtherBusy(tab)) return;
                if (on && tab) {
                  setTabBusy(tab, true, 'refresh');
                } else if (tab) {
                  setTabBusy(tab, false, 'refresh');
                } else {
                  mark.classList.toggle('ynow-logo-spinning', on);
                  try { mark.setAttribute('aria-busy', on ? 'true' : 'false'); } catch (eHs) {}
                }
                if (on && payload && payload.rate === 'fast') {
                  mark.classList.add('ynow-logo-spin-fast');
                }
              });
            }
            register();
            registerLogoSpinHandler();
            window.ynowSetSidebarTabBusy = setTabBusy;
            window.ynowSyncHeaderLogoSpin = syncHeaderLogoSpin;
            window.ynowClearOtherTabBusy = clearOtherTabBusy;
            window.ynowFocusCurrentTabBusy = focusCurrentTabBusy;
            /* After applyUiLocale rewrites menu HTML, restore busy-dot visuals. */
            window.ynowReapplyAllTabBusyVisuals = function () {
              Object.keys(sourcesByTab).forEach(function (tab) { applyVisual(tab); });
              syncHeaderLogoSpin();
            };
          })();

          /* Deploy refresh: poll ynow_build.json; never auto-reload / never force logout.
             Patch (+0.01): soft flame on title; click The YNow App to refresh.
             Major (vN → vN+1): same flame + must-click reminder for every signed-in session. */
          (function initYnowDeployRefresh() {
            var POLL_MS = 45000;
            var sessionBuild = null;
            var remoteBuild = null;
            var pollTimer = null;
            var LAST_UPDATE_STRINGS = null;
            var authObs = null;

            function titleEl() {
              return document.getElementById('ynow_app_title') ||
                document.querySelector('.main-header .logo .ynow-app-title');
            }
            function logoEl() {
              return document.querySelector('.main-header .logo');
            }
            function isSignedIn() {
              return !!(document.body && document.body.classList.contains('ynow-authed'));
            }
            /* Major = integer before decimal (v28.43 → 25), same as YNOW_VERSION_BUMP_MAJOR cut. */
            function displayMajor(version) {
              /* Use [0-9] not \d — this block lives in an R character string. */
              var m = String(version || '').trim().match(/^[vV]?([0-9]+)/);
              if (!m) return NaN;
              return parseInt(m[1], 10);
            }
            function isMajorBump(sessionVer, remoteVer) {
              var a = displayMajor(sessionVer);
              var b = displayMajor(remoteVer);
              return isFinite(a) && isFinite(b) && b > a;
            }
            window.ynowDisplayMajor = displayMajor;
            window.ynowIsMajorDisplayBump = isMajorBump;
            function readSessionBuild() {
              if (sessionBuild) return sessionBuild;
              var el = titleEl();
              var fromDom = el && el.getAttribute('data-ynow-build');
              if (fromDom) {
                sessionBuild = String(fromDom).trim();
                return sessionBuild;
              }
              sessionBuild = '';
              return sessionBuild;
            }
            function updateTipText(major) {
              var s = LAST_UPDATE_STRINGS || {};
              if (major) {
                return s.update_major_click ||
                  'Major update — click The YNow App to refresh';
              }
              return s.update_available_click ||
                'New version available — click The YNow App to refresh';
            }
            function ensureMajorReminderEl() {
              var el = document.getElementById('ynow_major_update_reminder');
              if (el) return el;
              el = document.createElement('div');
              el.id = 'ynow_major_update_reminder';
              el.className = 'ynow-major-update-reminder';
              el.setAttribute('role', 'alertdialog');
              el.setAttribute('aria-modal', 'true');
              el.setAttribute('aria-labelledby', 'ynow_major_update_title');
              el.setAttribute('aria-describedby', 'ynow_major_update_body');
              /* Build with DOM APIs — avoid quote conflicts inside the R HTML string. */
              var card = document.createElement('div');
              card.className = 'ynow-major-update-card';
              var titleP = document.createElement('p');
              titleP.id = 'ynow_major_update_title';
              titleP.className = 'ynow-major-update-title';
              var bodyP = document.createElement('p');
              bodyP.id = 'ynow_major_update_body';
              bodyP.className = 'ynow-major-update-body';
              var hintP = document.createElement('p');
              hintP.className = 'ynow-major-update-hint';
              hintP.setAttribute('data-ynow-major-hint', '1');
              card.appendChild(titleP);
              card.appendChild(bodyP);
              card.appendChild(hintP);
              el.appendChild(card);
              /* Reminder is informational only — refresh stays on flame title click. */
              el.addEventListener('click', function (ev) {
                var hitCard = ev.target && ev.target.closest
                  ? ev.target.closest('.ynow-major-update-card')
                  : null;
                if (!hitCard) {
                  ev.preventDefault();
                  ev.stopPropagation();
                  var t = titleEl();
                  if (t && typeof t.focus === 'function') {
                    try { t.focus(); } catch (eFocus) {}
                  }
                  return;
                }
                /* Card click: highlight flame title; do not reload here. */
                ev.preventDefault();
                ev.stopPropagation();
                var title = titleEl();
                if (title && typeof title.focus === 'function') {
                  try { title.focus(); } catch (eFocus2) {}
                }
              }, true);
              (document.body || document.documentElement).appendChild(el);
              return el;
            }
            function fillMajorReminderStrings() {
              var s = LAST_UPDATE_STRINGS || {};
              var root = document.getElementById('ynow_major_update_reminder');
              if (!root) return;
              var title = root.querySelector('#ynow_major_update_title');
              var body = root.querySelector('#ynow_major_update_body');
              var hint = root.querySelector('[data-ynow-major-hint]');
              if (title) {
                title.textContent = s.update_major_title || 'Major update available';
              }
              if (body) {
                body.textContent = s.update_major_body ||
                  ('A new major version is live. Click the flaming The YNow App title to refresh. ' +
                   'Your session stays signed in — we never auto-reload or force sign-out.');
              }
              if (hint) {
                hint.textContent = s.update_major_hint ||
                  'Click the flaming The YNow App title to refresh';
              }
            }
            function applyUpdateChrome() {
              var pending = !!(remoteBuild && sessionBuild && remoteBuild !== sessionBuild);
              var major = pending && isMajorBump(sessionBuild, remoteBuild);
              document.body.classList.toggle('ynow-update-available', pending);
              document.body.classList.toggle('ynow-update-major', !!major);
              if (major && isSignedIn()) {
                ensureMajorReminderEl();
                fillMajorReminderStrings();
              }
              var tip = updateTipText(!!major);
              var title = titleEl();
              var logo = logoEl();
              if (title) {
                if (pending) {
                  title.setAttribute('title', tip);
                  title.setAttribute('aria-label', tip);
                  title.setAttribute('role', 'button');
                  title.setAttribute('tabindex', '0');
                } else {
                  title.removeAttribute('title');
                  if (title.getAttribute('role') === 'button') {
                    title.setAttribute('role', 'progressbar');
                  }
                  title.removeAttribute('tabindex');
                }
              }
              if (logo) {
                if (pending) logo.setAttribute('title', tip);
                else logo.removeAttribute('title');
              }
            }
            window.ynowSetUpdateLocaleStrings = function (strings) {
              LAST_UPDATE_STRINGS = strings || null;
              applyUpdateChrome();
            };
            function markRemote(version) {
              var v = String(version || '').trim();
              if (!v) return;
              remoteBuild = v;
              applyUpdateChrome();
            }
            function fetchBuild() {
              var url = 'ynow_build.json?_=' + Date.now();
              return fetch(url, { cache: 'no-store', credentials: 'same-origin' })
                .then(function (res) {
                  if (!res || !res.ok) throw new Error('build fetch failed');
                  return res.json();
                })
                .then(function (data) {
                  if (data && data.version) markRemote(data.version);
                })
                .catch(function () { /* keep last known; no auto-reload */ });
            }
            function reloadForUpdate() {
              /* Hard navigation so the browser loads the new Shiny bundle / assets.
                 Intentional flame-title click only — never called from poll / auth. */
              try {
                var u = new URL(window.location.href);
                u.searchParams.set('_ynow_refresh', String(Date.now()));
                window.location.replace(u.toString());
              } catch (eReload) {
                window.location.reload();
              }
            }
            function onTitleActivate(ev) {
              if (!document.body.classList.contains('ynow-update-available')) return;
              var hit = ev.target && ev.target.closest
                ? ev.target.closest('#ynow_app_title, .main-header .logo')
                : null;
              if (!hit) return;
              /* Do not steal clicks from market/lang chrome inside the navbar. */
              if (ev.target.closest && ev.target.closest(
                '.ynow-market-header, .ynow-lang-header, .navbar-custom-menu, .sidebar-toggle'
              )) return;
              /* Reminder overlay must not intercept title clicks (title is above overlay). */
              ev.preventDefault();
              ev.stopPropagation();
              reloadForUpdate();
            }
            function bindClick() {
              if (document.documentElement.getAttribute('data-ynow-deploy-refresh') === '1') return;
              document.documentElement.setAttribute('data-ynow-deploy-refresh', '1');
              document.addEventListener('click', onTitleActivate, true);
              document.addEventListener('keydown', function (ev) {
                if (!document.body.classList.contains('ynow-update-available')) return;
                var t = ev.target;
                if (!t || t.id !== 'ynow_app_title') return;
                if (ev.key !== 'Enter' && ev.key !== ' ') return;
                ev.preventDefault();
                reloadForUpdate();
              }, true);
            }
            function bindAuthObserver() {
              if (authObs || !document.body || typeof MutationObserver === 'undefined') return;
              authObs = new MutationObserver(function () {
                /* Signed-in mid-session after a major deploy still needs the reminder. */
                applyUpdateChrome();
              });
              authObs.observe(document.body, {
                attributes: true,
                attributeFilter: ['class']
              });
            }
            function startPolling() {
              readSessionBuild();
              bindAuthObserver();
              fetchBuild();
              setTimeout(fetchBuild, 3000);
              setTimeout(fetchBuild, 10000);
              if (pollTimer) clearInterval(pollTimer);
              pollTimer = setInterval(fetchBuild, POLL_MS);
            }
            bindClick();
            if (document.readyState === 'loading') {
              document.addEventListener('DOMContentLoaded', startPolling);
            } else {
              startPolling();
            }
            document.addEventListener('visibilitychange', function () {
              if (!document.hidden) fetchBuild();
            });
            window.addEventListener('focus', function () { fetchBuild(); });
            function bindShinyHooks() {
              if (!window.jQuery) {
                setTimeout(bindShinyHooks, 50);
                return;
              }
              /* After reconnect (e.g. post-deploy), re-check build — still no auto-reload. */
              jQuery(document).on('shiny:connected shiny:reconnected', function () {
                fetchBuild();
              });
            }
            bindShinyHooks();
          })();

          function bindMarketModeButtons() {
            placeMarketHeaderByToggle();
            var stack = document.querySelector('#ynow-market-header .ynow-market-stack');
            if (!stack) return;
            if (stack.getAttribute('data-ynow-bound') === '1') return;
            stack.setAttribute('data-ynow-bound', '1');
            stack.addEventListener('click', function (ev) {
              var btn = ev.target && ev.target.closest ? ev.target.closest('.ynow-mkt-btn') : null;
              if (!btn || !stack.contains(btn)) return;
              var val = btn.getAttribute('data-value');
              if (!val) return;
              stack.querySelectorAll('.ynow-mkt-btn').forEach(function (b) {
                b.classList.toggle('active', b === btn);
              });
              if (window.Shiny && Shiny.setInputValue) {
                Shiny.setInputValue('market_mode_pick', val, {priority: 'event'});
                // Force Clustering Lab clear even if mode was already applied client-side
                Shiny.setInputValue('lab_cluster_clear_tick', Date.now(), {priority: 'event'});
              }
            });
            function pushInitial() {
              if (!(window.Shiny && Shiny.setInputValue)) {
                setTimeout(pushInitial, 50);
                return;
              }
              var active = stack.querySelector('.ynow-mkt-btn.active');
              var val = active ? active.getAttribute('data-value') : 'US';
              Shiny.setInputValue('market_mode_pick', val || 'US', {priority: 'event'});
            }
            pushInitial();
          }
          /* Language: stable custom buttons (no shinyWidgets re-render / mobile drift).
             Do not push an initial value — server + device_ui_locale own first paint;
             a premature EN push can race and lock out device zh-TW. */
          function bindLangModeButtons() {
            var stack = document.getElementById('ynow_lang_stack') ||
              document.querySelector('.ynow-lang-header .ynow-lang-stack');
            if (!stack) return;
            if (stack.getAttribute('data-ynow-bound') === '1') return;
            stack.setAttribute('data-ynow-bound', '1');
            stack.addEventListener('click', function (ev) {
              var btn = ev.target && ev.target.closest ? ev.target.closest('.ynow-lang-btn') : null;
              if (!btn || !stack.contains(btn)) return;
              var val = btn.getAttribute('data-value');
              if (!val) return;
              stack.querySelectorAll('.ynow-lang-btn').forEach(function (b) {
                b.classList.toggle('active', b === btn);
              });
              /* Sticky pending pick blocks mismatched (stale) websocket applies.
                 Cancel prior verify timers so an EN click's delayed check cannot
                 run after a later 繁中 click. */
              if (window.__ynowLocaleVerifyTimer) {
                clearTimeout(window.__ynowLocaleVerifyTimer);
                window.__ynowLocaleVerifyTimer = null;
              }
              window.__ynowLocaleUserPick = val;
              window.__ynowLocaleUserPickSeq = (window.__ynowLocaleUserPickSeq || 0) + 1;
              var pickSeq = window.__ynowLocaleUserPickSeq;
              try {
                document.documentElement.setAttribute('lang', val === 'zh-TW' ? 'zh-TW' : 'en');
              } catch (eLangOpt) {}
              /* Click-time boot apply: real menu strings (首頁／關於) without waiting
                 on ynowUiLocale. Server push still refreshes the full string map. */
              try {
                var bootRoot = window.__YNOW_UI_LOCALE_BOOT || {};
                var boot = bootRoot[val] || (val === 'zh-TW' ? bootRoot['zh-TW'] : bootRoot.en);
                if (boot && typeof applyUiLocale === 'function') {
                  var mkt = document.body.classList.contains('ynow-market-tw') ? 'TW' : 'US';
                  applyUiLocale({
                    locale: boot.locale || val,
                    market: mkt,
                    strings: boot.strings || {},
                    tabs: boot.tabs || {},
                    boxes: boot.boxes || []
                  }, {fromBoot: true});
                }
              } catch (eBoot) {}
              if (window.Shiny && Shiny.setInputValue) {
                Shiny.setInputValue('ui_locale_pick', val, {priority: 'event'});
                var expect = val;
                window.__ynowLocaleVerifyTimer = setTimeout(function () {
                  window.__ynowLocaleVerifyTimer = null;
                  if (pickSeq !== window.__ynowLocaleUserPickSeq) return;
                  var home = document.querySelector('.sidebar-menu a[data-value="home"]');
                  var about = document.querySelector('.sidebar-menu a[data-value="about"]');
                  var sp = home ? home.querySelector('.ynow-menu-label, span:not(.pull-right-container):not(.ynow-rec-slot)') : null;
                  var spA = about ? about.querySelector('.ynow-menu-label, span:not(.pull-right-container):not(.ynow-rec-slot)') : null;
                  var homeTxt = ((sp && sp.textContent) || (home && home.textContent) || '').replace(/\s+/g, ' ').trim();
                  var aboutTxt = ((spA && spA.textContent) || (about && about.textContent) || '').replace(/\s+/g, ' ').trim();
                  var needZh = expect === 'zh-TW';
                  var stringsOk = needZh
                    ? (/首頁/.test(homeTxt) && /關於/.test(aboutTxt))
                    : (/Home/i.test(homeTxt) && /About/i.test(aboutTxt));
                  if (stringsOk || !(window.Shiny && Shiny.setInputValue)) return;
                  Shiny.setInputValue('ui_locale_pick', expect, {priority: 'event'});
                  try {
                    var bootRoot2 = window.__YNOW_UI_LOCALE_BOOT || {};
                    var boot2 = bootRoot2[expect] || (expect === 'zh-TW' ? bootRoot2['zh-TW'] : bootRoot2.en);
                    if (boot2 && typeof applyUiLocale === 'function') {
                      var mkt2 = document.body.classList.contains('ynow-market-tw') ? 'TW' : 'US';
                      applyUiLocale({
                        locale: boot2.locale || expect,
                        market: mkt2,
                        strings: boot2.strings || {},
                        tabs: boot2.tabs || {},
                        boxes: boot2.boxes || []
                      }, {fromBoot: true});
                    }
                  } catch (eBoot2) {}
                }, 1200);
              }
            });
          }
          function armMarketHeaderPlacement() {
            try { placeMarketHeaderByToggle(); } catch (ePlace0) {}
            try { bindMarketModeButtons(); } catch (eBindM) {}
            try { bindLangModeButtons(); } catch (eBindL) {}
            if (typeof MutationObserver !== 'undefined') {
              try {
                var hdrNav = document.querySelector('.main-header .navbar');
                if (hdrNav && hdrNav.nodeType === 1 && !hdrNav.getAttribute('data-ynow-mkt-obs')) {
                  hdrNav.setAttribute('data-ynow-mkt-obs', '1');
                  var placeTimer = null;
                  var mktObs = new MutationObserver(function () {
                    if (placeTimer) clearTimeout(placeTimer);
                    placeTimer = setTimeout(function () {
                      try { placeMarketHeaderByToggle(); } catch (ePlace1) {}
                    }, 0);
                  });
                  /* Navbar child moves + custom-menu rebuilds that steal the market li. */
                  mktObs.observe(hdrNav, { childList: true, subtree: false });
                  var custom = document.querySelector('.main-header .navbar-custom-menu');
                  if (custom && custom.nodeType === 1) {
                    mktObs.observe(custom, { childList: true, subtree: true });
                  }
                }
              } catch (eObs) { /* never block placement timers */ }
            }
          }
          /* Register timers BEFORE arm so a MutationObserver throw cannot skip them. */
          [0, 50, 150, 250, 500, 1000, 2000, 4000].forEach(function (ms) {
            setTimeout(function () {
              try { placeMarketHeaderByToggle(); } catch (eT) {}
            }, ms);
          });
          /* Mobile: keep nudging until flags sit after the hamburger (AdminLTE
             sometimes rebuilds .navbar-nav and steals the market li back). */
          (function keepMarketBesideToggleOnMobile() {
            var tries = 0;
            var iv = setInterval(function () {
              tries += 1;
              try {
                if (typeof window.ynowPlaceMarketHeaderByToggle === 'function') {
                  window.ynowPlaceMarketHeaderByToggle();
                } else {
                  placeMarketHeaderByToggle();
                }
              } catch (eIv) {}
              var market = document.getElementById('ynow-market-header');
              var toggle = document.querySelector('.main-header .navbar > .sidebar-toggle') ||
                document.querySelector('.main-header .navbar .sidebar-toggle');
              var ok = !!(market && toggle && market.parentNode === toggle.parentNode &&
                market.previousElementSibling === toggle);
              if (ok) {
                document.documentElement.setAttribute('data-ynow-mkt-beside-toggle', '1');
                clearInterval(iv);
              } else if (tries >= 60) {
                clearInterval(iv);
              }
            }, 200);
          })();
          if (document.readyState === 'loading') {
            document.addEventListener('DOMContentLoaded', armMarketHeaderPlacement);
          } else {
            armMarketHeaderPlacement();
          }
          window.addEventListener('resize', function () {
            try { placeMarketHeaderByToggle(); } catch (eR) {}
            try { placeMobileAppTitle(); } catch (eR2) {}
          });
          if (window.jQuery) {
            jQuery(document).on(
              'shiny:connected shiny:sessioninitialized shiny:reconnected',
              function () {
                try { placeMarketHeaderByToggle(); } catch (eS) {}
              }
            );
          }
          document.addEventListener('click', function (ev) {
            var t = ev.target;
            if (t && t.closest && t.closest('.sidebar-menu .treeview > a')) {
              setTimeout(function () {
                if (LAST_BADGE_MAP) applySidebarBadges(LAST_BADGE_MAP);
              }, 0);
              setTimeout(function () {
                if (LAST_BADGE_MAP) applySidebarBadges(LAST_BADGE_MAP);
              }, 200);
            }
          });

          /* ---- UI locale (en / zh-TW) in-place chrome labels ---- */
          function setMenuLabel(tab, label) {
            var a = document.querySelector('.main-sidebar .sidebar-menu a[data-value="' + tab + '"]') ||
                    document.querySelector('.sidebar-menu a[data-value="' + tab + '"]');
            if (!a || !label) return;
            var icon = a.querySelector('i.fa:not(.pull-right), i.fas:not(.pull-right), i.far:not(.pull-right), i.glyphicon:not(.pull-right), i.ion:not(.pull-right)');
            var prc = a.querySelector('.pull-right-container');
            var angle = a.querySelector('i.fa.pull-right, i.fas.pull-right, i.far.pull-right, i.glyphicon.pull-right, i.fa-angle-left, i.fas.fa-angle-left');
            var slots = a.querySelectorAll('.ynow-rec-slot');
            var badges = a.querySelectorAll('small.ynow-sidebar-badge, small.ynow-rec-badge');
            var busyDots = a.querySelector('.ynow-tab-busy-dots');
            var wasBusy = a.classList.contains('ynow-tab-is-busy');
            var iconClone = icon ? icon.cloneNode(true) : null;
            var prcClone = prc ? prc.cloneNode(true) : null;
            var angleClone = (!prc && angle) ? angle.cloneNode(true) : null;
            var busyDotsClone = busyDots ? busyDots.cloneNode(true) : null;
            var slotClones = [];
            if (slots.length) {
              slots.forEach(function (s) { slotClones.push(s.cloneNode(true)); });
            } else {
              badges.forEach(function (b) {
                var slot = document.createElement('span');
                slot.className = 'ynow-rec-slot';
                slot.appendChild(b.cloneNode(true));
                slotClones.push(slot);
              });
            }
            a.innerHTML = '';
            if (iconClone) a.appendChild(iconClone);
            /* Full guest locks — same tab ids as ynow_sidebar_guest_lock_tabs() / Lite hide. */
            var loginTabs = window.__YNOW_SIDEBAR_GUEST_LOCK_TABS || {
              lab_notes: true,
              decision_checklist: true,
              mos_assumptions: true,
              price_alerts: true,
              asset_transmission: true,
              global_prediction: true
            };
            if (loginTabs[tab]) {
              var lock = document.createElement('i');
              lock.className = 'fas fa-lock ynow-menu-lock';
              lock.setAttribute('aria-hidden', 'true');
              a.appendChild(lock);
            }
            var lab = document.createElement('span');
            lab.className = 'ynow-menu-label';
            lab.textContent = ' ' + label + ' ';
            a.appendChild(lab);
            if (busyDotsClone) a.appendChild(busyDotsClone);
            slotClones.forEach(function (sc) { a.appendChild(sc); });
            if (prcClone) a.appendChild(prcClone);
            else if (angleClone) a.appendChild(angleClone);
            if (wasBusy) {
              a.classList.add('ynow-tab-is-busy');
              try { a.setAttribute('aria-busy', 'true'); } catch (eBusy) {}
              var liBusy = a.closest ? a.closest('li') : a.parentElement;
              if (liBusy && liBusy.classList) liBusy.classList.add('ynow-tab-is-busy');
            }
          }

          function setNavLinkLabel(a, label) {
            if (!a || !label) return;
            /* 勿覆寫含 Shiny 動態輸出的頁籤（如 FCFF／FCFE） */
            if (a.querySelector('.shiny-text-output, .shiny-html-output, .shiny-bound-output')) return;
            var icon = a.querySelector('i');
            var iconClone = icon ? icon.cloneNode(true) : null;
            a.innerHTML = '';
            if (iconClone) {
              a.appendChild(iconClone);
              a.appendChild(document.createTextNode(' '));
            }
            a.appendChild(document.createTextNode(label));
          }

          function applyTabLabels(tabMap) {
            if (!tabMap) return;
            document.querySelectorAll('.nav-tabs a[data-value], .navbar-nav a[data-value]').forEach(function (a) {
              var dv = a.getAttribute('data-value');
              if (dv && tabMap[dv]) setNavLinkLabel(a, tabMap[dv]);
            });
          }

          function applyBoxHeaders(specs) {
            if (!specs || !specs.length) return;
            document.querySelectorAll('.nav-tabs-custom > .nav-tabs > li.header').forEach(function (li) {
              var text = (li.textContent || '').replace(/\s+/g, ' ').trim();
              if (!text) return;
              for (var i = 0; i < specs.length; i++) {
                var sp = specs[i] || {};
                var aliases = sp.match || [];
                var hit = false;
                for (var j = 0; j < aliases.length; j++) {
                  var al = String(aliases[j] || '').replace(/\s+/g, ' ').trim();
                  /* Exact match only — substring hits remapped unrelated titles (e.g. Snapshot). */
                  if (al && text === al) { hit = true; break; }
                }
                if (!hit || !sp.label) continue;
                var icon = li.querySelector('i');
                var iconClone = icon ? icon.cloneNode(true) : null;
                li.innerHTML = '';
                if (iconClone) {
                  li.appendChild(iconClone);
                  li.appendChild(document.createTextNode(' '));
                }
                li.appendChild(document.createTextNode(sp.label));
                break;
              }
            });
          }

          function applyUiLocale(payload, opts) {
            /* opts.fromBoot: click-time slim boot maps; ignored for control flow
               because __ynowLocaleUserPick stays sticky after a manual pick. */
            opts = opts || {};
            var s = (payload && payload.strings) || {};
            var incoming = payload && payload.locale ? String(payload.locale) : 'en';
            var pendingPick = window.__ynowLocaleUserPick ? String(window.__ynowLocaleUserPick) : '';
            /* Drop stale websocket locale pushes that disagree with the sticky
               manual pick (e.g. late queued `en` after an early 繁中 click). */
            if (pendingPick && incoming !== pendingPick) {
              return;
            }
            /* First-paint zh guard: drop early server `en` until matching zh arrives
               (Accept-Language / device_ui_locale), so boot 繁中 is not overwritten.
               Boot applies themselves do not clear the guard. */
            var bootGuard = window.__ynowLocaleBootGuard
              ? String(window.__ynowLocaleBootGuard)
              : '';
            if (!pendingPick && bootGuard && incoming !== bootGuard) {
              return;
            }
            if (bootGuard && incoming === bootGuard && !opts.fromBoot) {
              window.__ynowLocaleBootGuard = null;
            }
            try {
              if (payload && payload.strings) {
                /* Boot slim maps must merge into any prior full push; replacing
                   wiped keys like sidebar_reorder_* and stuck the button in English. */
                if (opts.fromBoot && window.__ynowUiLocaleStrings &&
                    typeof window.__ynowUiLocaleStrings === 'object') {
                  var mergedBoot = {};
                  var prevS = window.__ynowUiLocaleStrings;
                  Object.keys(prevS).forEach(function (k) { mergedBoot[k] = prevS[k]; });
                  Object.keys(payload.strings).forEach(function (k) {
                    mergedBoot[k] = payload.strings[k];
                  });
                  window.__ynowUiLocaleStrings = mergedBoot;
                  s = mergedBoot;
                } else {
                  window.__ynowUiLocaleStrings = payload.strings;
                }
              }
            } catch (eStoreLoc) {}
            if (typeof window.ynowSetUpdateLocaleStrings === 'function') {
              window.ynowSetUpdateLocaleStrings(s);
            }
            var menu = {
              home: s.menu_home,
              macro_market: s.menu_macro_market,
              asset_transmission: s.menu_asset_transmission,
              global_prediction: s.menu_global_prediction,
              dashboard: s.menu_dashboard,
              company_advance: s.menu_company_advance,
              smart_analysis: s.menu_smart_analysis,
              get_started: s.menu_get_started,
              dcf_calculator: s.menu_dcf,
              ddm_calculator: s.menu_ddm,
              pb_calculator: s.menu_pb || s.menu_rel_book,
              rel_mult_ev: s.menu_rel_ev,
              rel_multiples_calculator: s.menu_rel_earn || s.menu_rel_multiples,
              rel_mult_rev: s.menu_rel_rev,
              sotp_calculator: s.menu_sotp,
              ri_calculator: s.menu_ri,
              nav_calculator: s.menu_nav,
              sensitivity: s.menu_ynow,
              lab_notes: s.menu_backtest,
              bluechip: s.menu_bluechip,
              hfv: s.menu_hfv,
              decision_checklist: s.menu_decision_checklist,
              mos_assumptions: s.menu_mos_assumptions,
              price_alerts: s.menu_price_alert,
              about: s.menu_about,
              settings: s.menu_settings
            };
            var menuSettings = document.getElementById('ynow_menu_settings');
            if (menuSettings && s.menu_settings) menuSettings.textContent = s.menu_settings;
            Object.keys(menu).forEach(function (k) {
              if (menu[k]) setMenuLabel(k, menu[k]);
            });
            if (typeof window.ynowReapplyAllTabBusyVisuals === 'function') {
              try { window.ynowReapplyAllTabBusyVisuals(); } catch (eBusyVis) {}
            }
            if (typeof window.ynowApplyAccessLocks === 'function') {
              try { window.ynowApplyAccessLocks(); } catch (eAccessLock) {}
            }
            var menuMacro = document.getElementById('ynow_menu_macro');
            if (menuMacro && s.menu_macro_market) menuMacro.textContent = s.menu_macro_market;
            var menuAtx = document.getElementById('ynow_menu_asset_transmission');
            if (menuAtx && s.menu_asset_transmission) menuAtx.textContent = s.menu_asset_transmission;
            var menuGp = document.getElementById('ynow_menu_global_prediction');
            if (menuGp && s.menu_global_prediction) menuGp.textContent = s.menu_global_prediction;
            var menuCompanyAdvance = document.getElementById('ynow_menu_company_advance');
            if (menuCompanyAdvance && s.menu_company_advance) {
              menuCompanyAdvance.textContent = s.menu_company_advance;
            }
            var headerLogoHome = document.getElementById('ynow_header_logo_home');
            var headerLogoTitle = (s.header_logo_refresh_title || s.header_logo_home_title || '');
            if (headerLogoHome && headerLogoTitle) {
              headerLogoHome.setAttribute('title', headerLogoTitle);
              headerLogoHome.setAttribute('aria-label', headerLogoTitle);
            }
            if (typeof window.ynowSyncSidebarReorderBtnLocale === 'function') {
              try { window.ynowSyncSidebarReorderBtnLocale(s); } catch (eReorderLoc) {}
            }
            var smartLab = document.getElementById('ynow_menu_smart');
            if (smartLab && s.menu_smart_analysis) smartLab.textContent = s.menu_smart_analysis;
            var menuBt = document.getElementById('ynow_menu_backtest');
            if (menuBt && s.menu_backtest) menuBt.textContent = s.menu_backtest;
            setBtText('ynow_bblab_page_title', 'bblab_page_title');
            setBtText('ynow_bblab_report_kicker', 'bblab_report_kicker');
            setBtText('ynow_bblab_page_sub', 'bblab_page_sub');
            setBtText('ynow_bblab_listed_only_notice', 'bblab_listed_only_notice');
            setBtText('ynow_bblab_listed_only_scope', 'bblab_listed_only_scope');
            setBtText('ynow_bblab_shared_ticker_hint', 'bblab_shared_ticker_hint');
            setBtText('ynow_bblab_company_label', 'bblab_company_label');
            setBtText('ynow_bblab_period_label', 'bblab_period_label');
            setBtText('ynow_bblab_statement_ccy_label', 'bblab_statement_ccy_label');
            setBtText('ynow_bblab_ch1_title', 'bblab_ch1_title');
            setBtText('ynow_bblab_ch1_help', 'bblab_ch1_help');
            setBtText('ynow_bblab_ch2_title', 'bblab_ch2_title');
            setBtText('ynow_bblab_ch2_help', 'bblab_ch2_help');
            setBtText('ynow_bblab_struct_title', 'bblab_struct_title');
            setBtText('ynow_bblab_struct_help', 'bblab_struct_help');
            setBtText('ynow_bblab_struct_biz_h', 'bblab_struct_biz_h');
            setBtText('ynow_bblab_struct_adj_h', 'bblab_struct_adj_h');
            setBtText('ynow_bblab_struct_conc_h', 'bblab_struct_conc_h');
            setBtText('ynow_bblab_bcg_title', 'bblab_bcg_title');
            setBtText('ynow_bblab_bcg_help', 'bblab_bcg_help');
            setBtText('ynow_bblab_ch3_title', 'bblab_ch3_title');
            setBtText('ynow_bblab_ch3_help', 'bblab_ch3_help');
            setBtText('ynow_bblab_ch3_current_label', 'bblab_ch3_current_label');
            setBtText('ynow_bblab_ch4_title', 'bblab_ch4_title');
            setBtText('ynow_bblab_ch4_help', 'bblab_ch4_help');
            setBtText('ynow_bblab_ch4_limited', 'bblab_ch4_limited');
            setBtText('ynow_bblab_ch5_title', 'bblab_ch5_title');
            setBtText('ynow_bblab_ch5_help', 'bblab_ch5_help');
            setBtText('ynow_bblab_ch6_title', 'bblab_ch6_title');
            setBtText('ynow_bblab_ch6_help', 'bblab_ch6_help');
            setBtText('ynow_bblab_recon_notes_title', 'bblab_recon_notes_title');
            setBtText('ynow_bblab_ch7_title', 'bblab_ch7_title');
            setBtText('ynow_bblab_summary_title', 'bblab_ch2_title');
            setBtText('ynow_bblab_chart_title', 'bblab_ch3_title');
            setBtText('ynow_bblab_cards_title', 'bblab_ch5_title');
            setBtText('ynow_bblab_recon_title', 'bblab_ch6_title');
            setBtText('ynow_bblab_sources_title', 'bblab_ch7_title');
            setBtText('ynow_bblab_sources_chrome', 'bblab_sources_chrome');
            setBtText('ynow_bblab_shared_title', 'bblab_shared_title');
            setBtText('ynow_bblab_expand_other', 'bblab_expand_other');
            setBtText('ynow_bblab_fallback_gm_label', 'bblab_fallback_gm_label');
            setBtText('ynow_bblab_geo_veto_why', 'bblab_geo_veto_why');
            setBtText('ynow_bblab_orders_title', 'bblab_orders_title');
            setBtText('ynow_bblab_orders_help', 'bblab_orders_help');
            setBtText('ynow_bblab_orders_qty_title', 'bblab_orders_qty_title');
            setBtText('ynow_bblab_orders_qty_help', 'bblab_orders_qty_help');
            setBtText('ynow_bblab_orders_from_filings_customers', 'bblab_orders_from_filings');
            setBtText('ynow_bblab_orders_from_filings_suppliers', 'bblab_orders_from_filings');
            setBtText('ynow_bblab_orders_disclosed_facts_customers', 'bblab_orders_disclosed_facts');
            setBtText('ynow_bblab_orders_disclosed_facts_suppliers', 'bblab_orders_disclosed_facts');
            setBtText('ynow_bblab_orders_markets_title', 'bblab_orders_markets_title');
            setBtText('ynow_bblab_orders_markets_help', 'bblab_orders_markets_help');
            setBtText('ynow_bblab_orders_customers_title', 'bblab_orders_customers_title');
            setBtText('ynow_bblab_orders_customers_help', 'bblab_orders_customers_help');
            setBtText('ynow_bblab_orders_suppliers_title', 'bblab_orders_suppliers_title');
            setBtText('ynow_bblab_orders_suppliers_help', 'bblab_orders_suppliers_help');
            setBtText('ynow_bblab_or_title', 'bblab_or_title');
            setBtText('ynow_bblab_supply_report_title', 'bblab_or_supply_report_title');
            setBtText('ynow_bblab_or_help', 'bblab_or_help');
            setBtText('ynow_bblab_or_start', 'bblab_or_start');
            setBtText('ynow_bblab_overlap_why', 'bblab_overlap_why');
            setBtText('ynow_macro_page_title', 'macro_page_title');
            setBtText('ynow_macro_page_sub', 'macro_page_sub');
            setBtText('ynow_macro_rf_title', 'macro_rf_title');
            setBtText('ynow_macro_htcdi_title', 'htcdi_title');
            var htcdiBox = document.getElementById('ynow_macro_htcdi_box');
            var htcdiAlert = htcdiBox ? String(htcdiBox.getAttribute('data-htcdi-alert') || '') : '';
            if (htcdiAlert.toLowerCase() === 'unavailable') {
              setBtText('ynow_macro_htcdi_hint', 'htcdi_unavailable');
            } else {
              setBtText('ynow_macro_htcdi_hint', 'htcdi_click_hint');
            }
            setBtText('ynow_macro_htcdi_disclosure', 'htcdi_disclosure');
            setBtText('ynow_macro_htcdi_overview_title', 'htcdi_overview_title');
            setBtText('ynow_macro_htcdi_formula', 'htcdi_formula_eq');
            setBtText('ynow_macro_htcdi_formula_parts', 'htcdi_formula_parts');
            setBtText('ynow_macro_htcdi_layer_title', 'htcdi_layer_title');
            setBtText('ynow_macro_htcdi_network_title', 'htcdi_network_title');
            setBtText('ynow_macro_htcdi_contagion_paths', 'htcdi_contagion_paths');
            setBtText('ynow_macro_htcdi_network_note', 'htcdi_network_note');
            setBtText('ynow_macro_htcdi_term_bridge', 'htcdi_term_bridge');
            setBtText('ynow_macro_htcdi_map_stmt', 'htcdi_term_map_stmt');
            setBtText('ynow_macro_htcdi_map_mkt', 'htcdi_term_map_mkt');
            setBtText('ynow_macro_htcdi_map_inf', 'htcdi_term_map_inf');
            setBtText('ynow_macro_htcdi_map_traj', 'htcdi_term_map_traj');
            setBtText('ynow_macro_htcdi_in_title', 'htcdi_in_composite_title');
            setBtText('ynow_macro_htcdi_in_help', 'htcdi_in_composite_help');
            setBtText('ynow_macro_htcdi_method_title', 'htcdi_method_title');
            setBtText('ynow_macro_index_hint', 'macro_index_chart_hint');
            setBtText('ynow_macro_ynow_index_hint', 'macro_ynow_index_chart_hint');
            setBtText('ynow_own_index_overlay_label', 'macro_own_index_overlay_label');
            setBtText('ynow_own_index_overlay_hint', 'macro_own_index_overlay_hint');
            ['ynow_index_title', 'ynow_index_chapter_title', 'ynow_index_rule',
             'ynow_index_chart_note', 'ynow_index_constituents',
             'ynow_index_col_ticker', 'ynow_index_col_name', 'ynow_index_col_mcap',
             'ynow_index_col_weight', 'ynow_index_col_last', 'ynow_index_col_chg'].forEach(function(id) {
              var el = document.getElementById(id);
              if (!el) return;
              var key = el.getAttribute('data-i18n') || id;
              setBtText(id, key);
            });
            setBtText('ynow_macro_tw_signal_title', 'macro_tw_signal_title');
            setBtText('ynow_macro_theme_title', 'macro_theme_title');
            setBtText('ynow_macro_theme_help', 'macro_theme_help');
            setBtText('ynow_macro_industry_label', 'macro_industry_label');
            setBtText('ynow_macro_concept_label', 'macro_concept_label');
            setBtText('ynow_macro_concept_cloud_hint', 'macro_concept_cloud_hint');
            setBtText('ynow_macro_period_label', 'macro_period_label');
            setBtText('ynow_macro_fx_lock', 'macro_fx_lock');
            setBtText('ynow_macro_bubble_title', 'macro_bubble_title');
            setBtText('ynow_macro_bubble_sub', 'macro_bubble_sub');
            setBtText('ynow_macro_bubble_topn_label', 'macro_bubble_topn_label');
            setBtText('ynow_macro_bubble_attr_label', 'macro_bubble_attr_label');
            setBtText('ynow_macro_bubble_conc_title', 'macro_bubble_conc_title');
            setBtText('ynow_macro_bubble_top_list_title', 'macro_bubble_top_list_title');
            setBtText('ynow_macro_bubble_conc_note', 'macro_bubble_conc_note');
            setBtText('ynow_macro_bubble_attr_title', 'macro_bubble_attr_title');
            setBtText('ynow_macro_bubble_buffett_title', 'macro_bubble_buffett_title');
            setBtText('ynow_macro_bubble_buffett_note', 'macro_bubble_buffett_note');
            /* Relative performance + Dynamic industry bubble share the same refresh label. */
            ['macro-refresh', 'macro-bubble_refresh'].forEach(function (rid) {
              var macroRefresh = document.getElementById(rid);
              if (macroRefresh && s.btn_macro_refresh) {
                var ri = macroRefresh.querySelector('i');
                var runIconHtml = ri ? ri.outerHTML + ' ' : '';
                macroRefresh.innerHTML = runIconHtml + s.btn_macro_refresh;
              }
            });
            /* Lazy Macro host may insert after this push (hasDom false). Re-apply
               once #ynow_macro_page_title appears so zh-TW chrome is not stuck English. */
            try {
              if (!document.getElementById('ynow_macro_page_title') &&
                  typeof MutationObserver !== 'undefined' &&
                  !window.__ynowMacroLocaleWatch) {
                window.__ynowMacroLocaleWatch = true;
                var macroWatchPayload = payload;
                var macroHost = document.getElementById('ynow_lazy_host_macro_market') ||
                  document.body;
                var macroMo = new MutationObserver(function () {
                  if (!document.getElementById('ynow_macro_page_title')) return;
                  macroMo.disconnect();
                  window.__ynowMacroLocaleWatch = false;
                  try { applyUiLocale(macroWatchPayload); } catch (eRe) {}
                });
                if (macroHost) {
                  macroMo.observe(macroHost, { childList: true, subtree: true });
                }
              }
            } catch (eMacroWatch) {}
            var catAsset = document.getElementById('ynow_menu_cat_asset');
            if (catAsset && s.menu_cat_asset) catAsset.textContent = s.menu_cat_asset;
            var catIncome = document.getElementById('ynow_menu_cat_income');
            if (catIncome && s.menu_cat_income) catIncome.textContent = s.menu_cat_income;
            var catRel = document.getElementById('ynow_menu_cat_relative');
            if (catRel && s.menu_cat_relative) catRel.textContent = s.menu_cat_relative;
            var catMeta = document.getElementById('ynow_menu_cat_meta');
            if (catMeta && s.menu_cat_meta) catMeta.textContent = s.menu_cat_meta;
            var menuRel = document.getElementById('ynow_menu_rel_multiples');
            if (menuRel && s.menu_rel_multiples) menuRel.textContent = s.menu_rel_multiples;
            var menuSotp = document.getElementById('ynow_menu_sotp');
            if (menuSotp && s.menu_sotp) menuSotp.textContent = s.menu_sotp;
            setBtText('ynow_rel_multiples_lead_title', 'rel_multiples_lead_title');
            setBtText('ynow_rel_multiples_lead_body', 'rel_multiples_lead_body');
            setBtText('ynow_rel_multiples_box_title', 'rel_multiples_box_title');
            setBtText('ynow_rel_multiples_tab_overview', 'rel_multiples_tab_overview');
            setBtText('ynow_rel_multiples_tab_equity', 'rel_multiples_tab_equity');
            setBtText('ynow_rel_multiples_tab_enterprise', 'rel_multiples_tab_enterprise');
            setBtText('ynow_rel_multiples_tab_bridge', 'rel_multiples_tab_bridge');
            setBtText('ynow_rel_param_matrix_title', 'rel_param_matrix_title');
            setBtText('ynow_rel_formula_equity', 'rel_formula_equity');
            setBtText('ynow_rel_multiples_btn_calc', 'rel_multiples_btn_calc');
            setBtText('ynow_rel_multiples_btn_sync', 'rel_multiples_btn_sync');
            setBtText('ynow_rel_mode_label', 'rel_mode_label');
            setBtText('ynow_rel_mode_help', 'rel_mode_help');
            setBtText('ynow_rel_formula_earnings', 'rel_formula_earnings');
            setBtText('ynow_rel_formula_enterprise', 'rel_formula_enterprise');
            setBtText('ynow_rel_formula_ps', 'rel_formula_ps');
            setBtText('ynow_rel_settings_pe_note', 'rel_settings_pe_note');
            setBtText('ynow_rel_settings_peg_note', 'rel_settings_peg_note');
            setBtText('ynow_rel_settings_ev_note', 'rel_settings_ev_note');
            setBtText('ynow_rel_settings_ps_note', 'rel_settings_ps_note');
            setBtText('ynow_rel_settings_bridge_note', 'rel_settings_bridge_note');
            setBtText('ynow_rel_multiples_bridge_heading', 'rel_multiples_bridge_heading');
            setBtText('ynow_rel_multiples_ps_rev_note', 'rel_multiples_ps_rev_note');
            setBtText('ynow_rel_multiples_pe_heading', 'rel_multiples_pe_heading');
            setBtText('ynow_rel_multiples_peg_heading', 'rel_multiples_peg_heading');
            setBtText('ynow_rel_multiples_ev_heading', 'rel_multiples_ev_heading');
            setBtText('ynow_rel_multiples_ps_heading', 'rel_multiples_ps_heading');
            setBtText('ynow_rel_multiples_pe_help', 'rel_multiples_pe_help');
            setBtText('ynow_rel_multiples_peg_help', 'rel_multiples_peg_help');
            setBtText('ynow_rel_multiples_ev_help', 'rel_multiples_ev_help');
            setBtText('ynow_rel_multiples_ps_help', 'rel_multiples_ps_help');
            setBtText('ynow_rel_multiples_arr_help', 'rel_multiples_arr_help');
            setBtText('ynow_rel_multiples_bridge_help', 'rel_multiples_bridge_help');
            setBtText('ynow_sotp_lead_title', 'sotp_lead_title');
            setBtText('ynow_sotp_lead_body', 'sotp_lead_body');
            setBtText('ynow_sotp_box_title', 'sotp_box_title');
            setBtText('ynow_sotp_tab_overview', 'sotp_tab_overview');
            setBtText('ynow_sotp_tab_segments', 'sotp_tab_segments');
            setBtText('ynow_sotp_tab_bridge', 'sotp_tab_bridge');
            setBtText('ynow_sotp_formula_banner', 'sotp_formula_banner');
            setBtText('ynow_sotp_settings_seg_note', 'sotp_settings_seg_note');
            setBtText('ynow_sotp_settings_bridge_note', 'sotp_settings_bridge_note');
            setBtText('ynow_sotp_bridge_heading', 'sotp_bridge_heading');
            setBtText('ynow_sotp_nonop_help', 'sotp_nonop_help');
            setBtText('ynow_sotp_btn_calc', 'sotp_btn_calc');
            setBtText('ynow_sotp_btn_sync', 'sotp_btn_sync');
            setBtText('ynow_sotp_btn_apply_mult', 'sotp_btn_apply_mult');
            setBtText('ynow_sotp_help', 'sotp_help');
            setBtText('ynow_sotp_bridge_help', 'sotp_bridge_help');
            var smartTitle = document.getElementById('ynow_smart_page_title');
            if (smartTitle && s.smart_page_title) smartTitle.textContent = s.smart_page_title;
            var smartChart = document.getElementById('ynow_smart_chart_title');
            if (smartChart && s.smart_chart_title) smartChart.textContent = s.smart_chart_title;
            document.querySelectorAll('.ynow-lite-toggle').forEach(function (liteToggle) {
              if (s.lite_toggle_title) liteToggle.setAttribute('title', s.lite_toggle_title);
              if (s.lite_toggle_aria) liteToggle.setAttribute('aria-label', s.lite_toggle_aria);
            });
            if (LAST_BADGE_MAP) {
              LAST_BADGE_MAP.labels = {
                primary: s.menu_badge_primary || (LAST_BADGE_MAP.labels && LAST_BADGE_MAP.labels.primary) || '推薦',
                secondary: s.menu_badge_secondary || (LAST_BADGE_MAP.labels && LAST_BADGE_MAP.labels.secondary) || '備選',
                reference: s.menu_badge_reference || (LAST_BADGE_MAP.labels && LAST_BADGE_MAP.labels.reference) || '參照'
              };
              applySidebarBadges(LAST_BADGE_MAP);
            }
            applyTabLabels((payload && payload.tabs) || {});
            applyBoxHeaders((payload && payload.boxes) || []);
            var methodTitle = document.getElementById('ynow_method_section_title');
            if (methodTitle && s.method_section_title) methodTitle.textContent = s.method_section_title;
            var methodFvNote = document.getElementById('ynow_method_matrix_fv_note');
            if (methodFvNote && s.method_matrix_fv_note) methodFvNote.textContent = s.method_matrix_fv_note;
            var methodRelNote = document.getElementById('ynow_method_matrix_rel_note');
            if (methodRelNote && s.method_matrix_rel_note) methodRelNote.textContent = s.method_matrix_rel_note;
            var hfvTitle = document.getElementById('ynow_hfv_page_title');
            if (hfvTitle && s.hfv_page_title) hfvTitle.textContent = s.hfv_page_title;
            var hfvSub = document.getElementById('ynow_hfv_page_sub');
            if (hfvSub && s.hfv_page_sub) hfvSub.textContent = s.hfv_page_sub;
            var dcTitle = document.getElementById('ynow_dc_page_title');
            if (dcTitle && s.dc_page_title) dcTitle.textContent = s.dc_page_title;
            var dcSub = document.getElementById('ynow_dc_page_sub');
            if (dcSub && s.dc_page_sub) dcSub.textContent = s.dc_page_sub;
            var dcBoxChecks = document.getElementById('ynow_dc_box_checks');
            if (dcBoxChecks && s.dc_box_checks) dcBoxChecks.textContent = s.dc_box_checks;
            var dcBoxSum = document.getElementById('ynow_dc_box_summary');
            if (dcBoxSum && s.dc_box_summary) dcBoxSum.textContent = s.dc_box_summary;
            var dcBoxSop = document.getElementById('ynow_dc_box_sop');
            if (dcBoxSop && s.dc_box_sop) dcBoxSop.textContent = s.dc_box_sop;
            var dcSopIntro = document.getElementById('ynow_dc_sop_intro');
            if (dcSopIntro && s.dc_sop_intro) dcSopIntro.textContent = s.dc_sop_intro;
            var hfvMethod = document.getElementById('ynow_hfv_sec_method');
            if (hfvMethod && s.hfv_sec_method) hfvMethod.textContent = s.hfv_sec_method;
            var hfvResults = document.getElementById('ynow_hfv_sec_results');
            if (hfvResults && s.hfv_sec_results) hfvResults.textContent = s.hfv_sec_results;
            var hfvCh1K = document.getElementById('ynow_hfv_ch1_kicker');
            if (hfvCh1K && s.hfv_ch1_kicker) hfvCh1K.textContent = s.hfv_ch1_kicker;
            var hfvCh1T = document.getElementById('ynow_hfv_ch1_title');
            if (hfvCh1T && s.hfv_ch1_title) hfvCh1T.textContent = s.hfv_ch1_title;
            var hfvCh2K = document.getElementById('ynow_hfv_ch2_kicker');
            if (hfvCh2K && s.hfv_ch2_kicker) hfvCh2K.textContent = s.hfv_ch2_kicker;
            var hfvCh2T = document.getElementById('ynow_hfv_ch2_title');
            if (hfvCh2T && s.hfv_ch2_title) hfvCh2T.textContent = s.hfv_ch2_title;
            var hfvCh2Lead = document.getElementById('ynow_hfv_ch2_lead');
            if (hfvCh2Lead && s.hfv_ch2_lead) hfvCh2Lead.textContent = s.hfv_ch2_lead;
            var hfvCh3K = document.getElementById('ynow_hfv_ch3_kicker');
            if (hfvCh3K && s.hfv_ch3_kicker) hfvCh3K.textContent = s.hfv_ch3_kicker;
            var hfvCh3T = document.getElementById('ynow_hfv_ch3_title');
            if (hfvCh3T && s.hfv_ch3_title) hfvCh3T.textContent = s.hfv_ch3_title;
            var hfvCh3Lead = document.getElementById('ynow_hfv_ch3_lead');
            if (hfvCh3Lead && s.hfv_ch3_lead) hfvCh3Lead.textContent = s.hfv_ch3_lead;
            var hfvCh4K = document.getElementById('ynow_hfv_ch4_kicker');
            if (hfvCh4K && s.hfv_ch4_kicker) hfvCh4K.textContent = s.hfv_ch4_kicker;
            var hfvCh4T = document.getElementById('ynow_hfv_ch4_title');
            if (hfvCh4T && s.hfv_ch4_title) hfvCh4T.textContent = s.hfv_ch4_title;
            var hfvCh4Lead = document.getElementById('ynow_hfv_ch4_lead');
            if (hfvCh4Lead && s.hfv_ch4_lead) hfvCh4Lead.textContent = s.hfv_ch4_lead;
            var hfvCh5K = document.getElementById('ynow_hfv_ch5_kicker');
            if (hfvCh5K && s.hfv_ch5_kicker) hfvCh5K.textContent = s.hfv_ch5_kicker;
            var hfvCh5T = document.getElementById('ynow_hfv_ch5_title');
            if (hfvCh5T && s.hfv_ch5_title) hfvCh5T.textContent = s.hfv_ch5_title;
            var hfvCh5Lead = document.getElementById('ynow_hfv_ch5_lead');
            if (hfvCh5Lead && s.hfv_ch5_lead) hfvCh5Lead.textContent = s.hfv_ch5_lead;
            var hfvOvNote = document.getElementById('ynow_hfv_overlay_vs_replay_note');
            if (hfvOvNote && s.hfv_overlay_vs_replay_note) hfvOvNote.textContent = s.hfv_overlay_vs_replay_note;
            var hfvParamInv = document.getElementById('ynow_hfv_param_inv_title');
            if (hfvParamInv && (s.hfv_param_inv_title || s.box_hfv_param_inventory)) {
              hfvParamInv.textContent = s.hfv_param_inv_title || s.box_hfv_param_inventory;
            }
            var hfvParamHelp = document.getElementById('ynow_hfv_param_inv_help');
            if (hfvParamHelp && s.hfv_param_inv_help) hfvParamHelp.textContent = s.hfv_param_inv_help;
            var hfvChart = document.getElementById('ynow_hfv_chart_gap');
            if (hfvChart && s.hfv_chart_gap) hfvChart.textContent = s.hfv_chart_gap;
            var hfvTable = document.getElementById('ynow_hfv_table_detail');
            if (hfvTable && s.hfv_table_detail) hfvTable.textContent = s.hfv_table_detail;
            var hfvDataNote = document.getElementById('ynow_hfv_method_data_note');
            if (hfvDataNote && s.hfv_method_data_note) hfvDataNote.textContent = s.hfv_method_data_note;
            var hfvMethodBody = document.getElementById('ynow_hfv_method_body');
            if (hfvMethodBody && s.hfv_method_body) hfvMethodBody.textContent = s.hfv_method_body;
            var hfvScMatrix = document.getElementById('ynow_hfv_sum_scenario_matrix');
            if (hfvScMatrix && s.hfv_sum_scenario_matrix) hfvScMatrix.textContent = s.hfv_sum_scenario_matrix;
            var hfvScThresh = document.getElementById('ynow_hfv_scenario_thresh_note');
            if (hfvScThresh && s.hfv_scenario_thresh_note) hfvScThresh.textContent = s.hfv_scenario_thresh_note;
            var fsIsHead = document.getElementById('ynow_fs_is_chart_heading');
            if (fsIsHead && s.tab_income_statement) fsIsHead.textContent = s.tab_income_statement;
            var fsCfHead = document.getElementById('ynow_fs_cf_chart_heading');
            if (fsCfHead && s.tab_cash_flow) fsCfHead.textContent = s.tab_cash_flow;
            var hfvSessTitle = document.getElementById('ynow_hfv_session_params_title');
            if (hfvSessTitle && s.hfv_session_params_title) hfvSessTitle.textContent = s.hfv_session_params_title;
            var hfvToolbar = document.getElementById('ynow_hfv_toolbar') || document.querySelector('.ynow-hfv-toolbar');
            if (hfvToolbar && s.hfv_toolbar_aria) hfvToolbar.setAttribute('aria-label', s.hfv_toolbar_aria);
            var hfvPageCtl = document.getElementById('ynow_hfv_page_controls');
            if (hfvPageCtl && s.hfv_page_controls_aria) hfvPageCtl.setAttribute('aria-label', s.hfv_page_controls_aria);
            var hfvOverlayCtl = document.getElementById('ynow_hfv_chart_overlay_controls');
            if (hfvOverlayCtl && s.hfv_chart_overlay_aria) hfvOverlayCtl.setAttribute('aria-label', s.hfv_chart_overlay_aria);
            var chartModelsLab = document.querySelector('label[for="bt_fv_models"]');
            if (chartModelsLab && s.hfv_chart_models_label) chartModelsLab.textContent = s.hfv_chart_models_label;
            var replayLab = document.querySelector('label[for="bt_fv_replay_model"]');
            if (replayLab && s.hfv_replay_model_label) replayLab.textContent = s.hfv_replay_model_label;
            var oosLab = document.querySelector('label[for="bt_fv_oos_mode"]');
            if (oosLab && s.hfv_oos_mode_label) oosLab.textContent = s.hfv_oos_mode_label;
            var freqLab = document.querySelector('label[for="bt_fv_analysis_freq"]');
            if (freqLab && s.hfv_analysis_freq_label) freqLab.textContent = s.hfv_analysis_freq_label;
            var labTitle = document.getElementById('ynow_lab_notes_title');
            if (labTitle && s.lab_notes_title) labTitle.textContent = s.lab_notes_title;
            var labSub = document.getElementById('ynow_lab_notes_sub');
            if (labSub && s.lab_notes_sub) labSub.textContent = s.lab_notes_sub;
            var btZone = document.getElementById('ynow_bt_zone_title');
            if (btZone && s.bt_zone_title) btZone.textContent = s.bt_zone_title;
            var btToolbar = document.getElementById('ynow_bt_toolbar') || document.querySelector('.ynow-backtest-toolbar');
            if (btToolbar && s.bt_toolbar_aria) btToolbar.setAttribute('aria-label', s.bt_toolbar_aria);
            var btNavCtl = document.getElementById('ynow_bt_nav_controls');
            if (btNavCtl && s.bt_nav_controls_aria) btNavCtl.setAttribute('aria-label', s.bt_nav_controls_aria);
            var navWinLab = document.querySelector('label[for="bt_nav_window"]');
            if (navWinLab && s.bt_nav_window_label) navWinLab.textContent = s.bt_nav_window_label;
            function setBtText(id, key) {
              var el = document.getElementById(id);
              if (el && s[key]) el.textContent = s[key];
            }
            /* MOS Assumptions body. ynowUiLocale strings; no ynowSetText. */
            var mosAssumpLocale = {
              ynow_mos_assump_page_title: 'mos_assump_page_title',
              ynow_mos_assump_page_sub: 'mos_assump_page_sub',
              ynow_mos_assump_b3_title: 'mos_assump_b3_title',
              ynow_mos_assump_b3_lead: 'mos_assump_b3_lead',
              ynow_mos_assump_goto_hfv: 'mos_assump_goto_hfv',
              ynow_mos_assump_b4_title: 'mos_assump_b4_title',
              ynow_mos_assump_b4_lead: 'mos_assump_b4_lead',
              ynow_mos_assump_goto_lab: 'mos_assump_goto_lab',
              ynow_mos_assump_b1_title: 'mos_assump_b1_title',
              ynow_mos_assump_b1_lead: 'mos_assump_b1_lead',
              ynow_mos_assump_goto_checklist: 'mos_assump_goto_checklist',
              ynow_mos_assump_b2_title: 'mos_assump_b2_title',
              ynow_mos_assump_b2_lead: 'mos_assump_b2_lead',
              ynow_mos_assump_goto_alerts: 'mos_assump_goto_alerts',
              ynow_mos_assump_chk_bear_base_label: 'dc_label_bear_base',
              ynow_mos_assump_chk_bear_base_hint: 'dc_hint_bear_base',
              ynow_mos_assump_bear_floor_label: 'dc_cond_bear_mos_floor',
              ynow_mos_assump_bear_floor_hint: 'dc_cond_hint_bear_mos_floor',
              ynow_mos_assump_chk_base_mos_label: 'dc_label_base_mos',
              ynow_mos_assump_chk_base_mos_hint: 'dc_hint_base_mos',
              ynow_mos_assump_base_floor_label: 'dc_cond_base_mos_floor',
              ynow_mos_assump_base_floor_hint: 'dc_cond_hint_base_mos_floor',
              ynow_mos_assump_alert_label: 'mos_assump_alert_label',
              ynow_mos_assump_alert_hint: 'mos_assump_alert_hint',
              ynow_mos_assump_hfv_near_label: 'mos_assump_hfv_near',
              ynow_mos_assump_hfv_cheap_label: 'mos_assump_hfv_cheap',
              ynow_mos_assump_hfv_rich_label: 'mos_assump_hfv_rich',
              ynow_mos_assump_hfv_flat_label: 'mos_assump_hfv_flat',
              ynow_mos_assump_hfv_strong_label: 'mos_assump_hfv_strong',
              ynow_mos_assump_hfv_hint: 'mos_assump_hfv_hint',
              ynow_mos_assump_hfv_risk: 'mos_assump_hfv_risk',
              ynow_mos_assump_wvg_label: 'bt_w_vg_label',
              ynow_mos_assump_wvg_hint: 'bt_w_vg_hint',
              ynow_mos_assump_ladder_title: 'mos_assump_ladder_title',
              ynow_mos_assump_lad_deep_label: 'mos_assump_lad_deep',
              ynow_mos_assump_lad_mid_label: 'mos_assump_lad_mid',
              ynow_mos_assump_lad_fair_label: 'mos_assump_lad_fair',
              ynow_mos_assump_lad_mild_label: 'mos_assump_lad_mild',
              ynow_mos_assump_ladder_hint: 'mos_assump_ladder_hint',
              ynow_mos_assump_strat_risk: 'mos_assump_strat_risk',
              /* Same generic gate title as Lab / Checklist / Alerts (not checklist kicker). */
              ynow_mos_assump_gate_title: 'auth_pa_gate_title',
              ynow_mos_assump_gate_body: 'mos_assump_gate_body',
              ynow_mos_assump_gate_signin_label: 'auth_sign_in',
              ynow_mos_assump_gate_signin_register_label: 'auth_register_btn'
            };
            Object.keys(mosAssumpLocale).forEach(function (id) {
              setBtText(id, mosAssumpLocale[id]);
            });
            setBtText('ynow_cookie_banner_body', 'cookie_banner_body');
            setBtText('ynow_cookie_settings_btn', 'cookie_settings_btn');
            setBtText('ynow_cookie_accept_all', 'cookie_accept_all');
            setBtText('ynow_cookie_reject_optional', 'cookie_reject_optional');
            setBtText('ynow_cookie_save', 'cookie_save');
            setBtText('ynow_cookie_panel_title', 'cookie_panel_title');
            setBtText('ynow_cookie_necessary_label', 'cookie_necessary_label');
            setBtText('ynow_cookie_necessary_hint', 'cookie_necessary_hint');
            setBtText('ynow_cookie_analytics_label', 'cookie_analytics_label');
            setBtText('ynow_cookie_analytics_hint', 'cookie_analytics_hint');
            setBtText('ynow_cookie_preferences_label', 'cookie_preferences_label');
            setBtText('ynow_cookie_preferences_hint', 'cookie_preferences_hint');
            setBtText('ynow_cookie_always_on', 'cookie_always_on');
            setBtText('ynow_cookie_reopen_label', 'cookie_reopen');
            setBtText('ynow_sidebar_copyright', 'sidebar_copyright');
            var cookiePin = document.getElementById('ynow_cookie_reopen');
            if (cookiePin && s.cookie_reopen) cookiePin.setAttribute('title', s.cookie_reopen);
            ['ynow_home_title', 'ynow_home_lead', 'ynow_home_method', 'ynow_home_method_lite',
             'ynow_home_market_k', 'ynow_home_market_d',
             'ynow_home_bluechip_k', 'ynow_home_bluechip_d',
             'ynow_home_company_k', 'ynow_home_company_d',
             'ynow_home_ynow_k', 'ynow_home_ynow_d',
             'ynow_home_value_k', 'ynow_home_value_d',
             'ynow_home_value_k_lite', 'ynow_home_value_d_lite',
             'ynow_home_decide_k', 'ynow_home_decide_d',
             'ynow_home_legal', 'ynow_home_legal_about'].forEach(function (id) {
              var el = document.getElementById(id);
              if (!el) return;
              var key = id.replace(/^ynow_/, '');
              setBtText(id, key);
            });
            setBtText('ynow_bt_ch1_kicker', 'bt_ch1_kicker');
            setBtText('ynow_bt_ch1_title', 'bt_ch1_title');
            setBtText('ynow_bt_ch2_kicker', 'bt_ch2_kicker');
            setBtText('ynow_bt_ch2_title', 'bt_ch2_title');
            setBtText('ynow_bt_ch2_lead', 'bt_ch2_lead');
            setBtText('ynow_bt_ch3_kicker', 'bt_ch3_kicker');
            setBtText('ynow_bt_ch3_title', 'bt_ch3_title');
            setBtText('ynow_bt_ch4_kicker', 'bt_ch4_kicker');
            setBtText('ynow_bt_ch4_title', 'bt_ch4_title');
            setBtText('ynow_bt_ch4_lead', 'bt_ch4_lead');
            setBtText('ynow_bt_exposure_title', 'bt_exposure_title');
            setBtText('ynow_bt_bh_gap_title', 'bt_bh_gap_title');
            setBtText('ynow_bt_mos_eff_title', 'bt_mos_eff_title');
            setBtText('ynow_bt_mos_eff_hint', 'bt_mos_eff_hint');
            setBtText('ynow_bt_fv_edge_title', 'bt_fv_edge_title');
            setBtText('ynow_bt_leg_fund_label', 'bt_leg_fund_label');
            setBtText('ynow_bt_leg_fund_body', 'bt_leg_fund_body');
            setBtText('ynow_bt_leg_sent_label', 'bt_leg_sent_label');
            setBtText('ynow_bt_leg_sent_body', 'bt_leg_sent_body');
            setBtText('ynow_bt_leg_bh_label', 'bt_leg_bh_label');
            setBtText('ynow_bt_leg_bh_body', 'bt_leg_bh_body');
            setBtText('ynow_bt_leg_bench_label', 'bt_leg_bench_label');
            setBtText('ynow_bt_leg_bench_body', 'bt_leg_bench_body');
            setBtText('ynow_bt_leg_hfv_note', 'bt_leg_hfv_note');
            setBtText('ynow_bt_sec_hold_gate', 'bt_sec_hold_gate');
            setBtText('ynow_bt_hold_gate_intro', 'bt_hold_gate_intro');
            setBtText('ynow_bt_kpi_filter_label', 'bt_kpi_filter_label');
            setBtText('ynow_bt_kpi_filter_hint', 'bt_kpi_filter_hint');
            setBtText('ynow_bt_sec_run_controls', 'bt_sec_run_controls');
            setBtText('ynow_bt_param_auto_hint', 'bt_param_auto_hint');
            setBtText('ynow_bt_refresh_params_hint', 'bt_refresh_params_hint');
            setBtText('ynow_bt_sec_strategy_params', 'bt_sec_strategy_params');
            setBtText('ynow_bt_params_gate_note', 'bt_params_gate_note');
            setBtText('ynow_bt_tab_fundamental', 'bt_tab_fundamental');
            setBtText('ynow_bt_tab_sentiment', 'bt_tab_sentiment');
            setBtText('ynow_bt_fund_intro', 'bt_fund_intro');
            setBtText('ynow_bt_w_vg_moved_title', 'bt_w_vg_moved_title');
            setBtText('ynow_bt_w_vg_moved_body', 'bt_w_vg_moved_body');
            setBtText('ynow_bt_mos_ladder_title', 'bt_mos_ladder_title');
            setBtText('ynow_bt_mos_ladder_body', 'bt_mos_ladder_body');
            setBtText('ynow_bt_sent_intro', 'bt_sent_intro');
            setBtText('ynow_bt_w_mom_hint', 'bt_w_mom_hint');
            setBtText('ynow_bt_w_rsi_hint', 'bt_w_rsi_hint');
            setBtText('ynow_bt_news_on', 'bt_news_on');
            setBtText('ynow_bt_news_hint', 'bt_news_hint');
            setBtText('ynow_bt_max_exp_hint', 'bt_max_exp_hint');
            setBtText('ynow_bt_min_exp_hint', 'bt_min_exp_hint');
            setBtText('ynow_bt_fit_bh_hint', 'bt_fit_bh_hint');
            setBtText('ynow_bt_sec_methodology', 'bt_sec_methodology');
            var recent = document.getElementById('ynow_recent_search_label');
            if (recent && s.recent_search) recent.textContent = s.recent_search;
            if (recent && s.recent_search_24h_hint && recent.getAttribute('data-ynow-signed-in') === '1') {
              recent.setAttribute('title', s.recent_search_24h_hint);
            }
            var scLab = document.querySelector('label[for="sc"]');
            if (scLab && s.ticker_label) scLab.textContent = s.ticker_label;
            var scInput = document.getElementById('sc');
            if (scInput) {
              var mktPh = (payload && payload.market) ? String(payload.market) : 'US';
              var scPh = (mktPh === 'TW')
                ? (s.ticker_placeholder_tw || 'e.g. 2330')
                : (s.ticker_placeholder_us || 'e.g. AAPL / TSM');
              scInput.setAttribute('placeholder', scPh);
              scInput.placeholder = scPh;
            }
            var yahooIndLab = document.getElementById('ynow_industry_info_yahoo');
            if (yahooIndLab && s.industry_info_yahoo) yahooIndLab.textContent = s.industry_info_yahoo;
            var indLab = document.querySelector('label[for="industry_choice"]');
            if (indLab) {
              indLab.style.display = 'none';
              indLab.textContent = '';
            }
            var sgrMethodTitle = document.getElementById('ynow_sgr_method_title');
            if (sgrMethodTitle && s.sgr_method_title) sgrMethodTitle.textContent = s.sgr_method_title;
            var sharedBridgeTitle = document.getElementById('ynow_shared_bridge_title');
            if (sharedBridgeTitle && s.shared_bridge_title) sharedBridgeTitle.textContent = s.shared_bridge_title;
            var sharedBridgeHelp = document.getElementById('ynow_shared_bridge_help');
            if (sharedBridgeHelp && s.shared_bridge_help) sharedBridgeHelp.textContent = s.shared_bridge_help;
            var sharedBridgeReset = document.getElementById('ynow_shared_bridge_reset');
            if (sharedBridgeReset && s.shared_bridge_reset) sharedBridgeReset.textContent = s.shared_bridge_reset;
            var fcfRevHelp = document.getElementById('ynow_fcf_revenue_shared_help');
            if (fcfRevHelp && s.fcf_revenue_shared_help) fcfRevHelp.textContent = s.fcf_revenue_shared_help;
            [['bridge_cash', 'shared_bridge_cash'], ['bridge_debt', 'shared_bridge_debt'],
             ['bridge_shares', 'shared_bridge_shares'], ['bridge_revenue', 'shared_bridge_revenue'],
             ['bridge_other_claims', 'param_other_claims_label']].forEach(function (pair) {
              var lab = document.querySelector('label[for="' + pair[0] + '"]');
              if (lab && s[pair[1]]) lab.textContent = s[pair[1]];
            });
            var lifeTitle = document.getElementById('ynow_lifecycle_stage_title');
            if (lifeTitle && s.lifecycle_stage_title) lifeTitle.textContent = s.lifecycle_stage_title;
            var lifeHelp = document.getElementById('ynow_lifecycle_stage_help');
            if (lifeHelp && s.lifecycle_stage_help) lifeHelp.textContent = s.lifecycle_stage_help;
            var sgrMethodHelp = document.getElementById('ynow_sgr_method_help');
            if (sgrMethodHelp && s.sgr_method_help) sgrMethodHelp.textContent = s.sgr_method_help;
            var sgrCustomLab = document.querySelector('label[for="sgr"]');
            if (sgrCustomLab && s.sgr_custom_label) sgrCustomLab.textContent = s.sgr_custom_label;
            var indOverviewTitle = document.getElementById('ynow_ind_overview_title');
            if (indOverviewTitle && s.industry_overview_title) indOverviewTitle.textContent = s.industry_overview_title;
            var valStatusPrefix = document.getElementById('ynow_val_status_prefix');
            if (valStatusPrefix && s.composite_status_prefix) valStatusPrefix.textContent = s.composite_status_prefix;
            var g1Help = document.getElementById('ynow_g_stage1_help');
            if (g1Help && s.g_stage1_help) g1Help.textContent = s.g_stage1_help;
            var ddmModeHelp = document.getElementById('ynow_ddm_mode_help');
            if (ddmModeHelp && s.ddm_mode_help) ddmModeHelp.textContent = s.ddm_mode_help;
            var ddmD0Help = document.getElementById('ynow_ddm_d0_help');
            if (ddmD0Help && s.ddm_d0_help) ddmD0Help.textContent = s.ddm_d0_help;
            var ddmSpmTip = document.getElementById('ynow_ddm_spm_tip');
            if (ddmSpmTip && s.ddm_spm_tip) ddmSpmTip.textContent = s.ddm_spm_tip;
            var ddmGSyncHelp = document.getElementById('ynow_ddm_g_sync_help');
            if (ddmGSyncHelp && s.ddm_g_sync_help) ddmGSyncHelp.textContent = s.ddm_g_sync_help;
            var ddmSpmGNote = document.getElementById('ynow_ddm_spm_g_note');
            if (ddmSpmGNote && s.ddm_spm_g_note) ddmSpmGNote.textContent = s.ddm_spm_g_note;
            var ddmFormulaGordon = document.getElementById('ynow_ddm_formula_gordon');
            if (ddmFormulaGordon && s.ddm_formula_gordon) ddmFormulaGordon.textContent = s.ddm_formula_gordon;
            var ddmFormulaSpm = document.getElementById('ynow_ddm_formula_spm');
            if (ddmFormulaSpm && s.ddm_formula_spm) ddmFormulaSpm.textContent = s.ddm_formula_spm;
            var ddmFormulaTwo = document.getElementById('ynow_ddm_formula_two_stage');
            if (ddmFormulaTwo && s.ddm_formula_two_stage) ddmFormulaTwo.textContent = s.ddm_formula_two_stage;
            var ddmOverviewHint = document.getElementById('ynow_ddm_overview_hint');
            if (ddmOverviewHint && s.ddm_overview_hint) ddmOverviewHint.textContent = s.ddm_overview_hint;
            var ddmD0Banner = document.getElementById('ynow_ddm_d0_banner');
            if (ddmD0Banner && s.ddm_d0_banner) ddmD0Banner.textContent = s.ddm_d0_banner;
            var ddmTwoStageHelp = document.getElementById('ynow_ddm_two_stage_help');
            if (ddmTwoStageHelp && s.ddm_two_stage_help) ddmTwoStageHelp.textContent = s.ddm_two_stage_help;
            var waccBoxTitle = document.getElementById('ynow_wacc_box_title');
            if (waccBoxTitle && s.wacc_box_title) waccBoxTitle.textContent = s.wacc_box_title;
            var waccHelp = document.getElementById('ynow_wacc_help');
            if (waccHelp && s.wacc_help) waccHelp.textContent = s.wacc_help;
            var rdBoxTitle = document.getElementById('ynow_rd_box_title');
            if (rdBoxTitle && s.rd_box_title) rdBoxTitle.textContent = s.rd_box_title;
            var rdIntLab = document.querySelector('label[for="rd_interest_expense"]');
            if (rdIntLab && s.rd_interest_label) rdIntLab.textContent = s.rd_interest_label;
            var rdDebtLab = document.querySelector('label[for="rd_interest_bearing_debt"]');
            if (rdDebtLab && s.rd_debt_label) rdDebtLab.textContent = s.rd_debt_label;
            var rdMinLab = document.querySelector('label[for="wacc_rd_min"]');
            if (rdMinLab && s.rd_min_label) rdMinLab.textContent = s.rd_min_label;
            var rdMaxLab = document.querySelector('label[for="wacc_rd_max"]');
            if (rdMaxLab && s.rd_max_label) rdMaxLab.textContent = s.rd_max_label;
            var useRdLab = document.getElementById('ynow_use_estimated_rd_label');
            if (useRdLab && s.use_estimated_rd_label) useRdLab.textContent = s.use_estimated_rd_label;
            var btnCalcRd = document.getElementById('calc_rd');
            if (btnCalcRd && s.btn_calc_rd) {
              var rdIcon = btnCalcRd.querySelector('i');
              var rdIconHtml = rdIcon ? rdIcon.outerHTML + ' ' : '';
              btnCalcRd.innerHTML = rdIconHtml + s.btn_calc_rd;
            }
            var btnCalcWacc = document.getElementById('calc_wacc');
            if (btnCalcWacc && s.btn_calc_wacc) {
              var wIcon = btnCalcWacc.querySelector('i');
              var wIconHtml = wIcon ? wIcon.outerHTML + ' ' : '';
              btnCalcWacc.innerHTML = wIconHtml + s.btn_calc_wacc;
            }
            function setBtnLabel(id, label) {
              if (!id || !label) return;
              var el = document.getElementById(id);
              if (!el) return;
              var ic = el.querySelector('i');
              el.innerHTML = (ic ? ic.outerHTML + ' ' : '') + label;
            }
            setBtnLabel('calc', s.btn_calc_dcf);
            setBtnLabel('mod_ddm-btn_calc_ddm', s.btn_calc_ddm);
            setBtnLabel('mod_ri-btn_calc_ri', s.btn_calc_ri);
            setBtnLabel('mod_pb-btn_calc_pb', s.btn_calc_pb);
            setBtnLabel('mod_nav-btn_calc_nav', s.btn_calc_nav);
            if (s.btn_reset_defaults) {
              document.querySelectorAll('.ynow-btn-reset').forEach(function (el) {
                var ic = el.querySelector('i');
                el.innerHTML = (ic ? ic.outerHTML + ' ' : '') + s.btn_reset_defaults;
              });
            }
            var riCalcHint = document.getElementById('mod_ri-ynow_ri_calc_hint');
            if (riCalcHint && s.ri_calc_hint) riCalcHint.textContent = s.ri_calc_hint;
            var riSettingsHint = document.getElementById('mod_ri-ynow_ri_settings_recalc_hint');
            if (riSettingsHint && s.ri_settings_recalc_hint) riSettingsHint.textContent = s.ri_settings_recalc_hint;
            var pbSettingsHint = document.getElementById('mod_pb-ynow_pb_settings_reset_hint');
            if (pbSettingsHint && s.pb_settings_reset_hint) pbSettingsHint.textContent = s.pb_settings_reset_hint;
            var navSettingsHint = document.getElementById('mod_nav-ynow_nav_settings_reset_hint');
            if (navSettingsHint && s.nav_settings_reset_hint) navSettingsHint.textContent = s.nav_settings_reset_hint;
            var capmBoxTitle = document.getElementById('ynow_capm_box_title');
            if (capmBoxTitle && s.capm_box_title) capmBoxTitle.textContent = s.capm_box_title;
            var ddmCapmBoxTitle = document.getElementById('ynow_ddm_capm_box_title');
            if (ddmCapmBoxTitle && s.ddm_capm_box_title) ddmCapmBoxTitle.textContent = s.ddm_capm_box_title;
            var ddmKeBoxTitle = document.getElementById('ynow_ddm_ke_box_title');
            if (ddmKeBoxTitle && s.ddm_ke_box_title) ddmKeBoxTitle.textContent = s.ddm_ke_box_title;
            var ddmKeHelp = document.getElementById('ynow_ddm_ke_help');
            if (ddmKeHelp && s.ddm_ke_help) ddmKeHelp.textContent = s.ddm_ke_help;
            var ddmKeBridgeTitle = document.getElementById('ynow_ddm_ke_bridge_title');
            if (ddmKeBridgeTitle && s.ddm_ke_bridge_title) ddmKeBridgeTitle.textContent = s.ddm_ke_bridge_title;
            var ddmKeBridgeHelp = document.getElementById('ynow_ddm_ke_bridge_help');
            if (ddmKeBridgeHelp && s.ddm_ke_bridge_help) ddmKeBridgeHelp.textContent = s.ddm_ke_bridge_help;
            var ddmUseEstKe = document.getElementById('ynow_ddm_use_estimated_ke_label');
            if (ddmUseEstKe && s.ddm_use_estimated_ke_label) ddmUseEstKe.textContent = s.ddm_use_estimated_ke_label;
            setBtnLabel('calc_ddm_ke', s.btn_calc_ddm_ke);
            setBtnLabel('calc_ddm_capm', s.btn_calc_ddm_capm);
            var syncGsLab = document.getElementById('ynow_sync_gs_beta_label');
            if (syncGsLab && s.sync_gs_beta_label) syncGsLab.textContent = s.sync_gs_beta_label;
            var ddmSyncGsLab = document.getElementById('ynow_ddm_sync_gs_beta_label');
            if (ddmSyncGsLab && s.sync_gs_beta_label) ddmSyncGsLab.textContent = s.sync_gs_beta_label;
            document.querySelectorAll('.ynow-beta-source-heading').forEach(function (el) {
              if (s.beta_source_heading) el.textContent = s.beta_source_heading;
            });
            document.querySelectorAll('.ynow-beta-rolling-help').forEach(function (el) {
              if (s.beta_rolling_help) el.textContent = s.beta_rolling_help;
            });
            document.querySelectorAll('.ynow-btn-sync-selected-beta').forEach(function (el) {
              if (!s.btn_sync_selected_beta) return;
              var ic = el.querySelector('i');
              el.innerHTML = (ic ? ic.outerHTML + ' ' : '') + s.btn_sync_selected_beta;
            });
            setBtnLabel('calc_capm', s.btn_calc_capm);
            setBtnLabel('calc_ddm_capm', s.btn_calc_ddm_capm || s.btn_calc_capm);
            document.querySelectorAll('.ynow-btn-calc-capm').forEach(function (el) {
              if (!s.btn_calc_capm) return;
              var ic = el.querySelector('i');
              /* Keep DDM CAPM button on its own key when present */
              var lab = (el.id === 'calc_ddm_capm' && s.btn_calc_ddm_capm) ? s.btn_calc_ddm_capm : s.btn_calc_capm;
              el.innerHTML = (ic ? ic.outerHTML + ' ' : '') + lab;
            });
            var rfLab = document.querySelector('label[for="capm_rf"]');
            if (rfLab && s.capm_rf_label) rfLab.textContent = s.capm_rf_label;
            var rmLab = document.querySelector('label[for="capm_rm"]');
            if (rmLab && s.capm_rm_label) rmLab.textContent = s.capm_rm_label;
            var ddmRfLab = document.querySelector('label[for="ddm_capm_rf"]');
            if (ddmRfLab && s.capm_rf_label) ddmRfLab.textContent = s.capm_rf_label;
            var ddmRmLab = document.querySelector('label[for="ddm_capm_rm"]');
            if (ddmRmLab && s.capm_rm_label) ddmRmLab.textContent = s.capm_rm_label;
            var useEstRe = document.getElementById('ynow_use_estimated_re_label');
            if (useEstRe && s.use_estimated_re_label) useEstRe.textContent = s.use_estimated_re_label;
            var explainCoreTitle = document.getElementById('ynow_explain_core_callout_title');
            if (explainCoreTitle && s.explain_core_callout_title) {
              explainCoreTitle.textContent = s.explain_core_callout_title;
            }
            var explainCoreBody = document.getElementById('ynow_explain_core_callout_body');
            if (explainCoreBody && s.explain_core_callout_body) {
              explainCoreBody.textContent = s.explain_core_callout_body;
            }
            var gsModelSel = document.getElementById('ynow_gs_model_selector_title');
            if (gsModelSel && s.gs_model_selector_title) gsModelSel.textContent = s.gs_model_selector_title;
            var gsBetaOv = document.getElementById('ynow_gs_beta_overview_help');
            if (gsBetaOv && s.gs_beta_overview_help) gsBetaOv.textContent = s.gs_beta_overview_help;
            var gsPeer = document.getElementById('ynow_gs_peer_unlever_help');
            if (gsPeer && s.gs_peer_unlever_help) gsPeer.textContent = s.gs_peer_unlever_help;
            var gsRoll = document.getElementById('ynow_gs_rolling_help');
            if (gsRoll && s.gs_rolling_help) gsRoll.textContent = s.gs_rolling_help;
            var rollSetTitle = document.getElementById('ynow_beta_rolling_settings_title');
            if (rollSetTitle && s.beta_rolling_settings_title) rollSetTitle.textContent = s.beta_rolling_settings_title;
            var rollCmpTitle = document.getElementById('ynow_beta_rolling_compare_title');
            if (rollCmpTitle && s.beta_rolling_compare_title) rollCmpTitle.textContent = s.beta_rolling_compare_title;
            var rollHelpBody = document.getElementById('ynow_beta_rolling_help_body');
            if (rollHelpBody && s.beta_rolling_help_body) rollHelpBody.textContent = s.beta_rolling_help_body;
            var benchLab = document.querySelector('label[for="beta_bench"]');
            if (benchLab && s.beta_bench_label) benchLab.textContent = s.beta_bench_label;
            var lookLab = document.querySelector('label[for="beta_lookback_months"]');
            if (lookLab && s.beta_lookback_label) lookLab.textContent = s.beta_lookback_label;
            setBtnLabel('calc_beta_est', s.btn_calc_beta_est);
            var buHead = document.getElementById('ynow_beta_bottomup_heading');
            if (buHead && s.beta_bottomup_heading) buHead.textContent = s.beta_bottomup_heading;
            var peersLab = document.querySelector('label[for="beta_peers"]');
            if (peersLab && s.beta_peers_label) peersLab.textContent = s.beta_peers_label;
            var buHelp = document.getElementById('ynow_beta_bottomup_help');
            if (buHelp && s.beta_bottomup_help) buHelp.textContent = s.beta_bottomup_help;
            setBtnLabel('calc_beta_bottomup', s.btn_calc_beta_bottomup);
            var unlHead = document.getElementById('ynow_beta_unlever_heading');
            if (unlHead && s.beta_unlever_heading) unlHead.textContent = s.beta_unlever_heading;
            var unlHelp = document.getElementById('ynow_beta_unlever_help');
            if (unlHelp && s.beta_unlever_help) unlHelp.textContent = s.beta_unlever_help;
            var manHead = document.getElementById('ynow_beta_manual_heading');
            if (manHead && s.beta_manual_heading) manHead.textContent = s.beta_manual_heading;
            var manHelp = document.getElementById('ynow_beta_manual_help');
            if (manHelp && s.beta_manual_help) manHelp.textContent = s.beta_manual_help;
            var funnelPageTitle = document.getElementById('ynow_funnel_page_title');
            if (funnelPageTitle && s.funnel_page_title) funnelPageTitle.textContent = s.funnel_page_title;
            var funnelPageSub = document.getElementById('ynow_funnel_page_sub');
            if (funnelPageSub && s.funnel_page_sub) funnelPageSub.textContent = s.funnel_page_sub;
            var funnelVerdictK = document.getElementById('ynow_funnel_verdict_kicker');
            if (funnelVerdictK && s.funnel_verdict_kicker) funnelVerdictK.textContent = s.funnel_verdict_kicker;
            var funnelVerdictT = document.getElementById('ynow_funnel_verdict_title');
            if (funnelVerdictT && s.funnel_verdict_title) funnelVerdictT.textContent = s.funnel_verdict_title;
            var funnelVerdictLead = document.getElementById('ynow_funnel_verdict_lead');
            if (funnelVerdictLead && s.funnel_verdict_lead) funnelVerdictLead.textContent = s.funnel_verdict_lead;
            var funnelRiskK = document.getElementById('ynow_funnel_risk_kicker');
            if (funnelRiskK && s.funnel_risk_kicker) funnelRiskK.textContent = s.funnel_risk_kicker;
            var funnelRiskT = document.getElementById('ynow_funnel_risk_title');
            if (funnelRiskT && s.funnel_risk_title) funnelRiskT.textContent = s.funnel_risk_title;
            var funnelRiskLead = document.getElementById('ynow_funnel_risk_lead');
            if (funnelRiskLead && s.funnel_risk_lead) funnelRiskLead.textContent = s.funnel_risk_lead;
            var funnelCh1K = document.getElementById('ynow_funnel_ch1_kicker');
            if (funnelCh1K && s.funnel_ch1_kicker) funnelCh1K.textContent = s.funnel_ch1_kicker;
            var funnelCh1T = document.getElementById('ynow_funnel_ch1_title');
            if (funnelCh1T && s.funnel_ch1_title) funnelCh1T.textContent = s.funnel_ch1_title;
            var funnelCh1Lead = document.getElementById('ynow_funnel_ch1_lead');
            if (funnelCh1Lead && s.funnel_ch1_lead) funnelCh1Lead.textContent = s.funnel_ch1_lead;
            var funnelCh2K = document.getElementById('ynow_funnel_ch2_kicker');
            if (funnelCh2K && s.funnel_ch2_kicker) funnelCh2K.textContent = s.funnel_ch2_kicker;
            var funnelCh2T = document.getElementById('ynow_funnel_ch2_title');
            if (funnelCh2T && s.funnel_ch2_title) funnelCh2T.textContent = s.funnel_ch2_title;
            var funnelCh2Lead = document.getElementById('ynow_funnel_ch2_lead');
            if (funnelCh2Lead && s.funnel_ch2_lead) funnelCh2Lead.textContent = s.funnel_ch2_lead;
            var funnelCh3K = document.getElementById('ynow_funnel_ch3_kicker');
            if (funnelCh3K && s.funnel_ch3_kicker) funnelCh3K.textContent = s.funnel_ch3_kicker;
            var funnelCh3T = document.getElementById('ynow_funnel_ch3_title');
            if (funnelCh3T && s.funnel_ch3_title) funnelCh3T.textContent = s.funnel_ch3_title;
            var funnelCh3Lead = document.getElementById('ynow_funnel_ch3_lead');
            if (funnelCh3Lead && s.funnel_ch3_lead) funnelCh3Lead.textContent = s.funnel_ch3_lead;
            if (s.notes_title) {
              document.querySelectorAll('.ynow-notes__title').forEach(function (el) {
                el.textContent = s.notes_title;
              });
            }
            if (s.notes_toggle_aria) {
              document.querySelectorAll('.ynow-notes__summary').forEach(function (el) {
                if (el.id === 'ynow_ms_dims_summary') return;
                if (el.id === 'ynow_ms_guide_summary') return;
                el.setAttribute('title', s.notes_toggle_aria);
                el.setAttribute('aria-label', s.notes_toggle_aria);
              });
            }
            var msGuideTitle = document.getElementById('ynow_ms_guide_title');
            if (msGuideTitle && s.box_model_guide) msGuideTitle.textContent = s.box_model_guide;
            var msGuideSummary = document.getElementById('ynow_ms_guide_summary');
            if (msGuideSummary && s.gs_ms_guide_toggle_aria) {
              msGuideSummary.setAttribute('title', s.gs_ms_guide_toggle_aria);
              msGuideSummary.setAttribute('aria-label', s.gs_ms_guide_toggle_aria);
            }
            var methodTabbox = document.getElementById('ynow_method_tabbox_title');
            if (methodTabbox && s.method_formulas_title) methodTabbox.textContent = s.method_formulas_title;
            var msDimsTitle = document.getElementById('ynow_ms_dims_title');
            if (msDimsTitle && s.gs_ms_dims_title) msDimsTitle.textContent = s.gs_ms_dims_title;
            var msDimsSummary = document.getElementById('ynow_ms_dims_summary');
            if (msDimsSummary && s.gs_ms_dims_toggle_aria) {
              msDimsSummary.setAttribute('title', s.gs_ms_dims_toggle_aria);
              msDimsSummary.setAttribute('aria-label', s.gs_ms_dims_toggle_aria);
            }
            var kpiJumpQoe = document.getElementById('ynow_kpi_jump_qoe');
            if (kpiJumpQoe && s.funnel_kpi_jump_qoe_aria) kpiJumpQoe.setAttribute('aria-label', s.funnel_kpi_jump_qoe_aria);
            var kpiJumpFs = document.getElementById('ynow_kpi_jump_fscore');
            if (kpiJumpFs && s.funnel_kpi_jump_fscore_aria) kpiJumpFs.setAttribute('aria-label', s.funnel_kpi_jump_fscore_aria);
            var kpiJumpAl = document.getElementById('ynow_kpi_jump_alerts');
            if (kpiJumpAl && s.funnel_kpi_jump_alerts_aria) kpiJumpAl.setAttribute('aria-label', s.funnel_kpi_jump_alerts_aria);
            var funnelSecMethod = document.getElementById('ynow_funnel_sec_method');
            if (funnelSecMethod && s.funnel_sec_method) funnelSecMethod.textContent = s.funnel_sec_method;
            var funnelMethodBody = document.getElementById('ynow_funnel_method_body');
            if (funnelMethodBody && s.funnel_method_body) funnelMethodBody.textContent = s.funnel_method_body;
            var funnelMethodCaveat = document.getElementById('ynow_funnel_method_caveat');
            if (funnelMethodCaveat && s.funnel_method_caveat) funnelMethodCaveat.textContent = s.funnel_method_caveat;
            var funnelFs = document.getElementById('ynow_funnel_fscore_list_title');
            if (funnelFs && s.funnel_fscore_list_title) funnelFs.textContent = s.funnel_fscore_list_title;
            var funnelMomTitle = document.getElementById('ynow_funnel_mom_box_title');
            if (funnelMomTitle && s.funnel_mom_box_title) funnelMomTitle.textContent = s.funnel_mom_box_title;
            var funnelMomIntro = document.getElementById('ynow_funnel_mom_intro');
            if (funnelMomIntro && s.funnel_mom_intro) funnelMomIntro.textContent = s.funnel_mom_intro;
            var funnelMomLogic = document.getElementById('ynow_funnel_mom_logic_title');
            if (funnelMomLogic && s.funnel_mom_logic_title) funnelMomLogic.textContent = s.funnel_mom_logic_title;
            var funnelMomCond1 = document.getElementById('ynow_funnel_mom_cond1');
            if (funnelMomCond1 && s.funnel_mom_cond1) funnelMomCond1.textContent = s.funnel_mom_cond1;
            var funnelMomCond1Lab = document.getElementById('ynow_funnel_mom_cond1_label');
            if (funnelMomCond1Lab && s.funnel_mom_cond1_label) funnelMomCond1Lab.textContent = s.funnel_mom_cond1_label;
            var funnelMomCond2 = document.getElementById('ynow_funnel_mom_cond2');
            if (funnelMomCond2 && s.funnel_mom_cond2) funnelMomCond2.textContent = s.funnel_mom_cond2;
            var funnelMomCond2Lab = document.getElementById('ynow_funnel_mom_cond2_label');
            if (funnelMomCond2Lab && s.funnel_mom_cond2_label) funnelMomCond2Lab.textContent = s.funnel_mom_cond2_label;
            var funnelMomBull = document.getElementById('ynow_funnel_mom_bull_rule');
            if (funnelMomBull && s.funnel_mom_bull_rule) funnelMomBull.textContent = s.funnel_mom_bull_rule;
            var funnelMomData = document.getElementById('ynow_funnel_mom_data_title');
            if (funnelMomData && s.funnel_mom_data_title) funnelMomData.textContent = s.funnel_mom_data_title;
            var funnelMomD1 = document.getElementById('ynow_funnel_mom_data_1');
            if (funnelMomD1 && s.funnel_mom_data_1) funnelMomD1.textContent = s.funnel_mom_data_1;
            var funnelMomD2 = document.getElementById('ynow_funnel_mom_data_2');
            if (funnelMomD2 && s.funnel_mom_data_2) funnelMomD2.textContent = s.funnel_mom_data_2;
            var funnelMomD3 = document.getElementById('ynow_funnel_mom_data_3');
            if (funnelMomD3 && s.funnel_mom_data_3) funnelMomD3.textContent = s.funnel_mom_data_3;
            var kpiBlue = document.getElementById('ynow_kpi_legend_blue');
            if (kpiBlue && s.kpi_legend_blue) kpiBlue.textContent = s.kpi_legend_blue;
            var kpiRed = document.getElementById('ynow_kpi_legend_red');
            if (kpiRed && s.kpi_legend_red) kpiRed.textContent = s.kpi_legend_red;
            var kpiBlack = document.getElementById('ynow_kpi_legend_black');
            if (kpiBlack && s.kpi_legend_black) kpiBlack.textContent = s.kpi_legend_black;
            var kpiWhite = document.getElementById('ynow_kpi_legend_white');
            if (kpiWhite && s.kpi_legend_white) kpiWhite.textContent = s.kpi_legend_white;
            var kpiFocus = document.getElementById('ynow_kpi_legend_focus');
            if (kpiFocus && s.kpi_legend_focus_metric) kpiFocus.textContent = s.kpi_legend_focus_metric;
            var fundProfLab = document.getElementById('ynow_fund_profile_label');
            if (fundProfLab && s.fund_profile_label) fundProfLab.textContent = s.fund_profile_label + '：';
            var fundBadge = document.getElementById('ynow_fund_profile_badge_text');
            if (fundBadge) {
              var pid = fundBadge.getAttribute('data-profile-id') || '';
              var pkey = pid ? ('fund_profile_' + pid) : '';
              if (pkey && s[pkey]) fundBadge.textContent = s[pkey];
            }
            if (typeof window.ynowSyncDownloadReportLabel === 'function') {
              window.ynowSyncDownloadReportLabel();
            } else {
              var dl = document.getElementById('download_report');
              if (dl && s.download_report) {
                /* Keep current icon (download vs lock); sync may swap after. */
                var icon = dl.querySelector('i');
                var iconHtml = icon ? icon.outerHTML + ' ' : '';
                dl.innerHTML = iconHtml + s.download_report;
                if (typeof window.ynowSyncSidebarDownloadIcon === 'function') {
                  window.ynowSyncSidebarDownloadIcon();
                }
              }
            }
            try {
              if (typeof window.ynowApplyPdfQuotaCopy === 'function') {
                window.ynowApplyPdfQuotaCopy(s);
              } else {
                var remEl = document.getElementById('ynow_pdf_quota_remaining');
                var rulesEl = document.getElementById('ynow_pdf_quota_rules');
                var liteOn = document.body && document.body.classList.contains('ynow-lite');
                if (liteOn) {
                  if (remEl && (s.pdf_quota_hint_lite || s.pdf_quota_hint)) {
                    remEl.textContent = s.pdf_quota_hint_lite || s.pdf_quota_hint;
                  }
                  if (rulesEl) rulesEl.textContent = '';
                } else if (remEl && s.pdf_quota_hint) {
                  remEl.textContent = s.pdf_quota_hint;
                }
              }
            } catch (ePdfQ) {}
            var snapQuota = document.getElementById('ynow_snapshot_download_quota');
            if (snapQuota) {
              var liteSnap = document.body && document.body.classList.contains('ynow-lite');
              var snapHint = liteSnap
                ? (s.snapshot_download_quota_hint_lite || s.snapshot_download_quota_hint)
                : (s.snapshot_download_quota_hint || '');
              if (snapHint) snapQuota.textContent = snapHint;
            }
            var snap = document.getElementById('ynow_snapshot_link_label');
            if (snap && s.snapshot_link) snap.textContent = s.snapshot_link;
            var snapTitle = document.getElementById('ynow_snapshot_page_title');
            if (snapTitle && s.snapshot_page_title) snapTitle.textContent = s.snapshot_page_title;
            var snapHelp = document.getElementById('ynow_snapshot_page_help');
            if (snapHelp && s.snapshot_page_help) snapHelp.textContent = s.snapshot_page_help;
            var snapHelpLite = document.getElementById('ynow_snapshot_page_help_lite');
            if (snapHelpLite && s.snapshot_page_help_lite) snapHelpLite.textContent = s.snapshot_page_help_lite;
            var snapDefaultsHelpLite = document.getElementById('ynow_snapshot_defaults_help_lite');
            if (snapDefaultsHelpLite && s.snapshot_defaults_help_lite) {
              snapDefaultsHelpLite.textContent = s.snapshot_defaults_help_lite;
            }
            var snapTabAudit = document.getElementById('ynow_snapshot_tab_audit');
            if (snapTabAudit && (s.snapshot_tab_audit || s.param_audit_title)) {
              snapTabAudit.textContent = s.snapshot_tab_audit || s.param_audit_title;
            }
            var snapTabCurrent = document.getElementById('ynow_snapshot_tab_current');
            if (snapTabCurrent && s.snapshot_tab_current) snapTabCurrent.textContent = s.snapshot_tab_current;
            var snapTabDefaults = document.getElementById('ynow_snapshot_tab_defaults');
            if (snapTabDefaults && s.snapshot_tab_defaults) snapTabDefaults.textContent = s.snapshot_tab_defaults;
            var snapDefaultsHelp = document.getElementById('ynow_snapshot_defaults_help');
            if (snapDefaultsHelp && s.snapshot_defaults_help) snapDefaultsHelp.textContent = s.snapshot_defaults_help;
            var snapDefaultsLocked = document.getElementById('ynow_snapshot_defaults_locked_msg');
            if (snapDefaultsLocked && s.snapshot_defaults_locked) {
              snapDefaultsLocked.textContent = s.snapshot_defaults_locked;
            }
            var dlSnapBtn = document.getElementById('ynow_download_snapshot_btn');
            if (dlSnapBtn && s.download_snapshot_btn) dlSnapBtn.textContent = s.download_snapshot_btn;
            var dlRestoreBtn = document.getElementById('ynow_download_param_restore_btn');
            if (dlRestoreBtn && s.download_param_restore_btn) dlRestoreBtn.textContent = s.download_param_restore_btn;
            var restoreTitle = document.getElementById('ynow_param_restore_title');
            if (restoreTitle && s.param_restore_title) restoreTitle.textContent = s.param_restore_title;
            var restoreHelp = document.getElementById('ynow_param_restore_help');
            if (restoreHelp && s.param_restore_help) restoreHelp.textContent = s.param_restore_help;
            var restoreFileLab = document.getElementById('ynow_param_restore_file_label');
            if (restoreFileLab && s.param_restore_file_label) restoreFileLab.textContent = s.param_restore_file_label;
            var restoreBtn = document.getElementById('ynow_param_restore_btn');
            if (restoreBtn && s.param_restore_btn) restoreBtn.textContent = s.param_restore_btn;
            var dlDefaultsBtn = document.getElementById('ynow_download_defaults_btn');
            if (dlDefaultsBtn && s.download_defaults_btn) dlDefaultsBtn.textContent = s.download_defaults_btn;
            var restoreBrowseRoot = document.getElementById('param_restore_file');
            if (restoreBrowseRoot) {
              var restoreBrowse = restoreBrowseRoot.closest('.form-group, .shiny-input-container');
              if (restoreBrowse) {
                var browseBtn = restoreBrowse.querySelector('.btn-file, .input-group-btn label, .input-group-btn .btn');
                if (browseBtn && s.param_restore_browse_btn) {
                  var onlyText = true;
                  browseBtn.childNodes.forEach(function (n) {
                    if (n.nodeType === 3 && String(n.textContent || '').trim()) {
                      n.textContent = ' ' + s.param_restore_browse_btn + ' ';
                      onlyText = false;
                    }
                  });
                  if (onlyText && !browseBtn.querySelector('i')) browseBtn.textContent = s.param_restore_browse_btn;
                }
                var ph = restoreBrowse.querySelector('input[type="text"]');
                if (ph && s.param_restore_placeholder) {
                  ph.setAttribute('placeholder', s.param_restore_placeholder);
                  ph.placeholder = s.param_restore_placeholder;
                }
              }
            }
            /* Market buttons show equal-size flag glyphs; locale only updates a11y labels. */
            var mktUs = document.getElementById('ynow_mkt_btn_us');
            if (mktUs && s.market_us) {
              mktUs.setAttribute('aria-label', s.market_us);
              mktUs.setAttribute('title', s.market_us);
            }
            var mktTw = document.getElementById('ynow_mkt_btn_tw');
            if (mktTw && s.market_tw) {
              mktTw.setAttribute('aria-label', s.market_tw);
              mktTw.setAttribute('title', s.market_tw);
            }
            var mktStack = document.querySelector('#ynow-market-header .ynow-market-stack');
            if (mktStack && s.market_hint) mktStack.setAttribute('aria-label', s.market_hint);
            var langStack = document.getElementById('ynow_lang_stack') ||
              document.querySelector('.ynow-lang-header .ynow-lang-stack');
            if (langStack && s.hdr_lang_label) langStack.setAttribute('aria-label', s.hdr_lang_label);
            var langZh = document.getElementById('ynow_lang_btn_zh');
            var langEn = document.getElementById('ynow_lang_btn_en');
            if (langZh && s.hdr_lang_zh) langZh.textContent = s.hdr_lang_zh;
            if (langEn && s.hdr_lang_en) langEn.textContent = s.hdr_lang_en;
            var loc = (payload && payload.locale) ? String(payload.locale) : 'en';
            document.querySelectorAll('.ynow-lang-stack .ynow-lang-btn').forEach(function (b) {
              b.classList.toggle('active', b.getAttribute('data-value') === loc);
            });
            var ccyFloat = document.querySelector('.ynow-ccy-float');
            if (ccyFloat && s.hdr_ccy_label) ccyFloat.setAttribute('aria-label', s.hdr_ccy_label);
            setBtText('ynow_hfv_rel_models_note', 'hfv_rel_models_note');
            setBtText('ynow_lab_im_methods_label', 'lab_im_methods_label');
            setBtText('ynow_lab_im_methods_abs_label', 'lab_im_methods_abs_label');
            setBtText('ynow_lab_im_methods_rel_label', 'lab_im_methods_rel_label');
            setBtText('ynow_lab_im_methods_locked_hint', 'lab_im_methods_locked_hint');
            setBtText('ynow_lab_im_method_nav', 'lab_im_method_nav');
            setBtText('ynow_lab_im_method_pb', 'lab_im_method_pb');
            setBtText('ynow_lab_im_method_dcf', 'lab_im_method_dcf');
            setBtText('ynow_lab_im_method_ddm', 'lab_im_method_ddm');
            setBtText('ynow_lab_im_method_ri', 'lab_im_method_ri');
            setBtText('ynow_lab_im_method_enterprise', 'lab_im_method_enterprise');
            setBtText('ynow_lab_im_method_earnings', 'lab_im_method_earnings');
            setBtText('ynow_lab_im_method_revenue', 'lab_im_method_revenue');
            setBtText('ynow_lab_im_method_book', 'lab_im_method_book');
            setBtText('ynow_menu_pb', 'menu_pb');
            setBtText('ynow_menu_rel_book', 'menu_rel_book');
            setBtText('ynow_lab_im_chains_label', 'lab_im_chains_label');
            setBtText('ynow_lab_im_chains_hint', 'lab_im_chains_hint');
            setBtText('ynow_lab_im_industries_label', 'lab_im_industries_label');
            setBtText('ynow_lab_im_industries_hint', 'lab_im_industries_hint');
            var hfvBenchLab = document.getElementById('ynow_hfv_show_bench_label');
            if (hfvBenchLab && s.hfv_show_bench) hfvBenchLab.textContent = s.hfv_show_bench;
            var convLab = document.querySelector('label[for="bt_fv_conv_window"]');
            if (convLab && s.hfv_conv_window_label) convLab.textContent = s.hfv_conv_window_label;
            var paHelp = document.getElementById('ynow_param_audit_help');
            if (paHelp && s.param_audit_help) paHelp.textContent = s.param_audit_help;
            var paPdfTitle = document.getElementById('ynow_param_audit_pdf_title');
            if (paPdfTitle && s.param_audit_pdf_title) paPdfTitle.textContent = s.param_audit_pdf_title;
            var paPdfHelp = document.getElementById('ynow_param_audit_pdf_help');
            if (paPdfHelp && s.param_audit_pdf_help) paPdfHelp.textContent = s.param_audit_pdf_help;
            var paPdfPages = document.getElementById('ynow_param_audit_pdf_pages_label');
            if (paPdfPages && s.param_audit_pdf_pages_label) paPdfPages.textContent = s.param_audit_pdf_pages_label;
            var paPdfBtn = document.getElementById('ynow_param_audit_pdf_btn');
            if (paPdfBtn && s.param_audit_pdf_btn) paPdfBtn.textContent = s.param_audit_pdf_btn;
            document.querySelectorAll('.ynow-pdf-page-lab').forEach(function (el) {
              var k = el.getAttribute('data-key');
              if (k && s[k]) el.textContent = s[k];
            });
            document.querySelectorAll('.ynow-pdf-pages-group-label').forEach(function (el) {
              var k = el.getAttribute('data-key');
              if (k && s[k]) el.textContent = s[k];
            });
            var testL = document.getElementById('ynow_test_link_label');
            if (testL && s.test_link) testL.textContent = s.test_link;
            var fb = document.getElementById('ynow_feedback_link_label');
            if (fb && s.feedback_link) fb.textContent = s.feedback_link;
            var testTitle = document.getElementById('ynow_testing_page_title');
            if (testTitle && s.testing_page_title) testTitle.textContent = s.testing_page_title;
            var testSub = document.getElementById('ynow_testing_page_sub');
            if (testSub && s.testing_page_sub) testSub.textContent = s.testing_page_sub;
            var testBoxBody = document.getElementById('ynow_testing_box_body');
            if (testBoxBody && s.testing_box_body) testBoxBody.textContent = s.testing_box_body;
            var ptsSysTitle = document.getElementById('ynow_pts_sysadmin_title');
            if (ptsSysTitle && s.pts_sysadmin_title) ptsSysTitle.textContent = s.pts_sysadmin_title;
            var ptsSysIntro = document.getElementById('ynow_pts_sysadmin_intro');
            if (ptsSysIntro && s.pts_sysadmin_intro) ptsSysIntro.textContent = s.pts_sysadmin_intro;
            var ptsSysEnter = document.getElementById('ynow_pts_sysadmin_enter_label');
            if (ptsSysEnter && s.pts_sysadmin_enter) ptsSysEnter.textContent = s.pts_sysadmin_enter;
            var ptsSysExit = document.getElementById('ynow_pts_sysadmin_exit_label');
            if (ptsSysExit && s.pts_sysadmin_exit) ptsSysExit.textContent = s.pts_sysadmin_exit;
            var ptsLoginUserLab = document.getElementById('ynow_pts_sysadmin_login_user_label');
            if (ptsLoginUserLab && s.pts_sysadmin_login_user) ptsLoginUserLab.textContent = s.pts_sysadmin_login_user;
            var ptsLoginPassLab = document.getElementById('ynow_pts_sysadmin_login_pass_label');
            if (ptsLoginPassLab && s.pts_sysadmin_login_pass) ptsLoginPassLab.textContent = s.pts_sysadmin_login_pass;
            var ptsViewAs = document.getElementById('ynow_pts_sysadmin_viewas_label');
            if (ptsViewAs && s.pts_sysadmin_viewas) ptsViewAs.textContent = s.pts_sysadmin_viewas;
            var ptsViewAsActive = document.getElementById('ynow_pts_sysadmin_viewas_active_label');
            if (ptsViewAsActive && s.pts_sysadmin_viewas_active) ptsViewAsActive.textContent = s.pts_sysadmin_viewas_active;
            var vaChromeActive = document.getElementById('ynow_viewas_chrome_active_label');
            if (vaChromeActive && s.pts_sysadmin_viewas_active) vaChromeActive.textContent = s.pts_sysadmin_viewas_active;
            var vaChromeHint = document.getElementById('ynow_viewas_chrome_hint');
            if (vaChromeHint && s.pts_sysadmin_viewas_chrome_hint) vaChromeHint.textContent = s.pts_sysadmin_viewas_chrome_hint;
            var vaChromeExit = document.getElementById('ynow_viewas_chrome_exit_label');
            if (vaChromeExit && s.pts_sysadmin_viewas_exit) vaChromeExit.textContent = s.pts_sysadmin_viewas_exit;
            var selfViewLab = document.getElementById('ynow_pts_sysadmin_self_view_label');
            if (selfViewLab && s.pts_sysadmin_self_view_label) selfViewLab.textContent = s.pts_sysadmin_self_view_label;
            var selfGuestLab = document.getElementById('ynow_pts_sysadmin_self_guest_label');
            if (selfGuestLab && s.pts_sysadmin_self_guest) selfGuestLab.textContent = s.pts_sysadmin_self_guest;
            var selfSignedLab = document.getElementById('ynow_pts_sysadmin_self_signed_in_label');
            if (selfSignedLab && s.pts_sysadmin_self_signed_in) selfSignedLab.textContent = s.pts_sysadmin_self_signed_in;
            var selfOmniLab = document.getElementById('ynow_pts_sysadmin_self_omniscient_label');
            if (selfOmniLab && s.pts_sysadmin_self_omniscient) selfOmniLab.textContent = s.pts_sysadmin_self_omniscient;
            var selfExitLab = document.getElementById('ynow_pts_sysadmin_self_exit_label');
            if (selfExitLab && s.pts_sysadmin_self_exit) selfExitLab.textContent = s.pts_sysadmin_self_exit;
            var selfHelp = document.getElementById('ynow_pts_sysadmin_self_help');
            if (selfHelp && s.pts_sysadmin_self_help) selfHelp.textContent = s.pts_sysadmin_self_help;
            var modeRemExit = document.getElementById('ynow_admin_mode_reminder_exit_label');
            if (modeRemExit) {
              var remMode = (document.getElementById('ynow_admin_mode_reminder') || {})
                .getAttribute && document.getElementById('ynow_admin_mode_reminder')
                .getAttribute('data-mode');
              if (remMode === 'view_as' && s.pts_sysadmin_viewas_exit) {
                modeRemExit.textContent = s.pts_sysadmin_viewas_exit;
              } else if (s.pts_sysadmin_self_exit) {
                modeRemExit.textContent = s.pts_sysadmin_self_exit;
              }
            }
            var modeRemEl = document.getElementById('ynow_admin_mode_reminder');
            if (modeRemEl && s.pts_sysadmin_mode_reminder_aria) {
              modeRemEl.setAttribute('aria-label', s.pts_sysadmin_mode_reminder_aria);
            }
            var ctrlPanel = document.getElementById('ynow_sysadmin_control_panel');
            if (ctrlPanel && s.pts_sysadmin_control_panel_aria) {
              ctrlPanel.setAttribute('aria-label', s.pts_sysadmin_control_panel_aria);
            }
            var adminOpsTab = document.getElementById('ynow_admin_ops_tab_label');
            if (adminOpsTab && s.admin_ops_tab) adminOpsTab.textContent = s.admin_ops_tab;
            var adminOpsControlsTab = document.getElementById('ynow_admin_ops_controls_tab_label');
            if (adminOpsControlsTab && s.admin_ops_controls_tab) {
              adminOpsControlsTab.textContent = s.admin_ops_controls_tab;
            }
            var adminOpsTitle = document.getElementById('ynow_admin_ops_title');
            if (adminOpsTitle && s.admin_ops_title) adminOpsTitle.textContent = s.admin_ops_title;
            var adminOpsHelp = document.getElementById('ynow_admin_ops_help');
            if (adminOpsHelp && s.admin_ops_help) adminOpsHelp.textContent = s.admin_ops_help;
            var adminOpsEmpty = document.getElementById('ynow_admin_ops_empty');
            if (adminOpsEmpty && s.admin_ops_empty) adminOpsEmpty.textContent = s.admin_ops_empty;
            var adminInspTab = document.getElementById('ynow_admin_insp_tab_label');
            if (adminInspTab && s.admin_insp_tab) adminInspTab.textContent = s.admin_insp_tab;
            var adminInspTitle = document.getElementById('ynow_admin_insp_title');
            if (adminInspTitle && s.admin_insp_title) adminInspTitle.textContent = s.admin_insp_title;
            var adminInspHelp = document.getElementById('ynow_admin_insp_help');
            if (adminInspHelp && s.admin_insp_help) adminInspHelp.textContent = s.admin_insp_help;
            var ptsColon = (payload && payload.locale === 'zh-TW') ? '：' : ':';
            var ptsHdrForLocale = document.getElementById('ynow-pts-header');
            var ptsSideForLocale = document.getElementById('ynow-pts-sidebar');
            var ptsGuestHidden = !!(
              (ptsHdrForLocale && ptsHdrForLocale.classList.contains('ynow-pts-hidden')) ||
              (ptsSideForLocale && ptsSideForLocale.classList.contains('ynow-pts-hidden'))
            );
            /* Do not refill FYN/PYN banner labels while guest-hidden. */
            if (!ptsGuestHidden) {
              var ptsFynLab = document.getElementById('ynow_pts_banner_fyn_label');
              if (ptsFynLab && s.pts_fyn_label) ptsFynLab.textContent = s.pts_fyn_label + ptsColon;
              var ptsPynLab = document.getElementById('ynow_pts_banner_pyn_label');
              if (ptsPynLab && s.pts_pyn_label) ptsPynLab.textContent = s.pts_pyn_label + ptsColon;
              var ptsSideFynLab = document.getElementById('ynow_pts_sidebar_fyn_label');
              if (ptsSideFynLab && s.pts_fyn_label) ptsSideFynLab.textContent = s.pts_fyn_label + ptsColon;
              var ptsSidePynLab = document.getElementById('ynow_pts_sidebar_pyn_label');
              if (ptsSidePynLab && s.pts_pyn_label) ptsSidePynLab.textContent = s.pts_pyn_label + ptsColon;
            }
            var atxTitle = document.getElementById('ynow_atx_page_title');
            if (atxTitle && s.atx_page_title) atxTitle.textContent = s.atx_page_title;
            var atxSub = document.getElementById('ynow_atx_page_sub');
            if (atxSub && s.atx_page_sub) atxSub.textContent = s.atx_page_sub;
            var atxBoxTitle = document.getElementById('ynow_atx_box_title');
            if (atxBoxTitle && s.atx_box_title) atxBoxTitle.textContent = s.atx_box_title;
            var atxBoxBody = document.getElementById('ynow_atx_box_body');
            if (atxBoxBody && s.atx_box_body) atxBoxBody.textContent = s.atx_box_body;
            var atxTabMap = document.getElementById('ynow_atx_tab_map');
            if (atxTabMap && s.atx_tab_map) atxTabMap.textContent = s.atx_tab_map;
            var gpPageTitle = document.getElementById('ynow_gp_page_title');
            if (gpPageTitle && s.gp_page_title) gpPageTitle.textContent = s.gp_page_title;
            var gpGateTitle = document.getElementById('ynow_gp_gate_title');
            if (gpGateTitle && s.gp_gate_title) gpGateTitle.textContent = s.gp_gate_title;
            var gpGateBody = document.getElementById('ynow_gp_gate_body');
            if (gpGateBody && s.gp_gate_body) gpGateBody.textContent = s.gp_gate_body;
            var gpGateSignin = document.getElementById('ynow_gp_gate_signin');
            if (gpGateSignin && s.gp_gate_signin) gpGateSignin.textContent = s.gp_gate_signin;
            var atxNodesTab = document.getElementById('ynow_atx_nodes_tab');
            if (atxNodesTab && s.atx_nodes_title) atxNodesTab.textContent = s.atx_nodes_title;
            var atxEdgesTab = document.getElementById('ynow_atx_edges_tab');
            if (atxEdgesTab && s.atx_edges_title) atxEdgesTab.textContent = s.atx_edges_title;
            var runBt = document.getElementById('run_bt');
            if (runBt && s.btn_run_bt) {
              var runIcon = runBt.querySelector('i');
              var runIconHtml = runIcon ? runIcon.outerHTML + ' ' : '';
              runBt.innerHTML = runIconHtml + s.btn_run_bt;
            }
            setBtnLabel('bt_kpi_filter', s.btn_bt_kpi_filter);
            setBtnLabel('bt_refresh_params', s.btn_bt_refresh_params);
            setBtnLabel('bt_fit_bh_preset', s.btn_bt_fit_bh);
            var labImRun = document.getElementById('lab_im_run_fscore');
            if (labImRun && s.btn_lab_im_run) {
              var labBusyMap = window.__ynowBtnBusy || {};
              var labRunLbl = labBusyMap['lab_im_run_fscore']
                ? (s.btn_lab_im_run_busy || s.btn_lab_im_run)
                : s.btn_lab_im_run;
              var labIcon = labImRun.querySelector('i');
              var labIconHtml = labIcon ? labIcon.outerHTML + ' ' : '';
              labImRun.innerHTML = labIconHtml + labRunLbl;
            }
            var tickerSearchBtn = document.getElementById('search');
            if (tickerSearchBtn && s.btn_ticker_search) {
              var tkBusyMap = window.__ynowBtnBusy || {};
              var tkLbl = tkBusyMap['search']
                ? (s.btn_ticker_search_busy || s.btn_ticker_search)
                : s.btn_ticker_search;
              var tkIcon = tickerSearchBtn.querySelector('i');
              var tkIconHtml = tkIcon ? tkIcon.outerHTML + ' ' : '';
              tickerSearchBtn.innerHTML = tkIconHtml + tkLbl;
            }
            var labMaxNLabel = document.getElementById('ynow_lab_im_max_n_label');
            if (labMaxNLabel && s.lab_im_max_n_label) labMaxNLabel.textContent = s.lab_im_max_n_label;
            var labMaxNCustom = document.getElementById('ynow_lab_im_max_n_custom_label');
            if (labMaxNCustom && s.lab_im_max_n_custom_label) labMaxNCustom.textContent = s.lab_im_max_n_custom_label;
            var labPoolRank = document.getElementById('ynow_lab_im_pool_rank_label');
            if (labPoolRank && s.lab_im_pool_rank_label) labPoolRank.textContent = s.lab_im_pool_rank_label;
            var labConcepts = document.getElementById('ynow_lab_im_concepts_label');
            if (labConcepts && s.lab_im_concepts_label) labConcepts.textContent = s.lab_im_concepts_label;
            var labDetailIntro = document.getElementById('ynow_lab_im_detail_intro');
            if (labDetailIntro && s.lab_im_detail_intro) labDetailIntro.textContent = s.lab_im_detail_intro;
            var labDetailVirt = document.getElementById('ynow_lab_im_detail_virt_hint');
            if (labDetailVirt && s.lab_im_detail_virt_hint) labDetailVirt.textContent = s.lab_im_detail_virt_hint;
            setBtText('ynow_lab_im_detail_box_title', 'lab_im_detail_box_title');
            var labLbMode = document.getElementById('ynow_lab_im_lb_mode_label');
            if (labLbMode && s.lab_im_lb_mode_label) labLbMode.textContent = s.lab_im_lb_mode_label;
            var labLbOverall = document.getElementById('ynow_lab_im_lb_mode_overall');
            if (labLbOverall && s.lab_im_lb_mode_overall) labLbOverall.textContent = s.lab_im_lb_mode_overall;
            var labLbByInd = document.getElementById('ynow_lab_im_lb_mode_by_ind');
            if (labLbByInd && s.lab_im_lb_mode_by_industry) labLbByInd.textContent = s.lab_im_lb_mode_by_industry;
            var labLbUndervalued = document.getElementById('ynow_lab_im_lb_mode_undervalued');
            if (labLbUndervalued && s.lab_im_lb_mode_undervalued) labLbUndervalued.textContent = s.lab_im_lb_mode_undervalued;
            var labIndMcapTitle = document.getElementById('ynow_lab_im_ind_mcap_title');
            if (labIndMcapTitle && s.lab_im_ind_mcap_title) labIndMcapTitle.textContent = s.lab_im_ind_mcap_title;
            var labIndMcapBadge = document.getElementById('ynow_lab_im_ind_mcap_badge');
            if (labIndMcapBadge && s.lab_im_ind_mcap_badge) labIndMcapBadge.textContent = s.lab_im_ind_mcap_badge;
            var labIndMcapHelp = document.getElementById('ynow_lab_im_ind_mcap_help');
            if (labIndMcapHelp && s.lab_im_ind_mcap_help) labIndMcapHelp.textContent = s.lab_im_ind_mcap_help;
            var bcClusterShellHint = document.getElementById('ynow_bc_cluster_shell_hint');
            if (bcClusterShellHint && s.bc_cluster_shell_hint) {
              bcClusterShellHint.textContent = s.bc_cluster_shell_hint;
            }
            var bcScreenerShellHint = document.getElementById('ynow_bc_screener_shell_hint');
            if (bcScreenerShellHint && s.bc_screener_shell_hint) {
              bcScreenerShellHint.textContent = s.bc_screener_shell_hint;
            }
            var labIndMcapTreemapLegend = document.getElementById('ynow_lab_im_ind_mcap_treemap_legend');
            if (labIndMcapTreemapLegend && s.lab_im_ind_mcap_treemap_legend) {
              labIndMcapTreemapLegend.textContent = s.lab_im_ind_mcap_treemap_legend;
            }
            var labIndMcapRefreshLabel = document.getElementById('ynow_lab_im_refresh_ind_mcap_label');
            if (labIndMcapRefreshLabel && s.btn_lab_im_refresh_ind_mcap) {
              var indBusyMap = window.__ynowBtnBusy || {};
              labIndMcapRefreshLabel.textContent = indBusyMap['lab_im_refresh_ind_mcap']
                ? (s.btn_lab_im_refresh_ind_mcap_busy || s.btn_lab_im_refresh_ind_mcap)
                : s.btn_lab_im_refresh_ind_mcap;
            }
            var labIndMcapRefreshBtn = document.getElementById('lab_im_refresh_ind_mcap');
            if (labIndMcapRefreshBtn && s.lab_im_refresh_ind_mcap_title) {
              labIndMcapRefreshBtn.setAttribute('title', s.lab_im_refresh_ind_mcap_title);
            }
            var labBoardsLabel = document.getElementById('ynow_lab_im_boards_label');
            if (labBoardsLabel && s.lab_im_boards_label) labBoardsLabel.textContent = s.lab_im_boards_label;
            var labBoardTwse = document.getElementById('ynow_lab_im_board_twse');
            if (labBoardTwse && s.lab_im_board_twse) labBoardTwse.textContent = s.lab_im_board_twse;
            var labBoardTpex = document.getElementById('ynow_lab_im_board_tpex');
            if (labBoardTpex && s.lab_im_board_tpex) labBoardTpex.textContent = s.lab_im_board_tpex;
            var labBoardEsb = document.getElementById('ynow_lab_im_board_esb');
            if (labBoardEsb && s.lab_im_board_esb) labBoardEsb.textContent = s.lab_im_board_esb;
            var labBoardsHint = document.getElementById('ynow_lab_im_boards_hint');
            if (labBoardsHint && s.lab_im_boards_hint) {
              labBoardsHint.textContent = s.lab_im_boards_hint;
              var labBoardsWrap = labBoardsHint.parentElement;
              if (labBoardsWrap) {
                labBoardsWrap.setAttribute('title', s.lab_im_boards_hint);
                var labBoardsLabels = labBoardsWrap.querySelectorAll('label');
                for (var bi = 0; bi < labBoardsLabels.length; bi++) {
                  labBoardsLabels[bi].setAttribute('title', s.lab_im_boards_hint);
                }
              }
            }
            var labLbHelp = document.getElementById('ynow_lab_im_lb_scope_help');
            if (labLbHelp && s.lab_im_lb_scope_help) labLbHelp.textContent = s.lab_im_lb_scope_help;
            var labGateLabel = document.getElementById('ynow_lab_im_gate_label');
            if (labGateLabel && s.lab_im_gate_label) labGateLabel.textContent = s.lab_im_gate_label;
            var labGateHint = document.getElementById('ynow_lab_im_gate_hint');
            if (labGateHint && s.lab_im_gate_hint) labGateHint.textContent = s.lab_im_gate_hint;
            var labNoAlertLabel = document.getElementById('ynow_lab_im_no_alert_label');
            if (labNoAlertLabel && s.lab_im_no_alert_label) labNoAlertLabel.textContent = s.lab_im_no_alert_label;
            var labNoAlertHint = document.getElementById('ynow_lab_im_no_alert_hint');
            if (labNoAlertHint && s.lab_im_no_alert_hint) labNoAlertHint.textContent = s.lab_im_no_alert_hint;
            var labImRunTitle = document.getElementById('lab_im_run_fscore');
            if (labImRunTitle && s.lab_im_run_title) labImRunTitle.setAttribute('title', s.lab_im_run_title);
            var annFs = document.getElementById('ynow_ann_fscore_crossref');
            if (annFs && s.ann_fscore_crossref) annFs.textContent = s.ann_fscore_crossref;
            var labUsListedLabel = document.getElementById('ynow_lab_im_us_listed_label');
            if (labUsListedLabel && s.lab_im_us_listed_label) labUsListedLabel.textContent = s.lab_im_us_listed_label;
            var labUsListedHint = document.getElementById('ynow_lab_im_us_listed_hint');
            if (labUsListedHint && s.lab_im_us_listed_hint) {
              labUsListedHint.textContent = s.lab_im_us_listed_hint;
              var labUsListedWrap = labUsListedHint.parentElement;
              if (labUsListedWrap) {
                labUsListedWrap.setAttribute('title', s.lab_im_us_listed_hint);
                var labUsListedLabels = labUsListedWrap.querySelectorAll('label');
                for (var uli = 0; uli < labUsListedLabels.length; uli++) {
                  labUsListedLabels[uli].setAttribute('title', s.lab_im_us_listed_hint);
                }
              }
            }
            var labAdrLabel = document.getElementById('ynow_lab_im_include_adr_label');
            if (labAdrLabel && s.lab_im_include_adr_label) labAdrLabel.textContent = s.lab_im_include_adr_label;
            var labAdrHint = document.getElementById('ynow_lab_im_include_adr_hint');
            if (labAdrHint && s.lab_im_include_adr_hint) labAdrHint.textContent = s.lab_im_include_adr_hint;
            var labEqLabel = document.getElementById('ynow_lab_im_eq_label');
            if (labEqLabel && s.lab_im_eq_label) labEqLabel.textContent = s.lab_im_eq_label;
            var labEqHint = document.getElementById('ynow_lab_im_eq_hint');
            if (labEqHint && s.lab_im_eq_hint) labEqHint.textContent = s.lab_im_eq_hint;
            var labEqExplainTitle = document.getElementById('ynow_lab_im_eq_explain_title');
            if (labEqExplainTitle && s.lab_im_eq_explain_title) labEqExplainTitle.textContent = s.lab_im_eq_explain_title;
            var labEqExplainBody = document.getElementById('ynow_lab_im_eq_explain_body');
            if (labEqExplainBody && s.lab_im_eq_explain_body) labEqExplainBody.textContent = s.lab_im_eq_explain_body;
            var labQueryTitle = document.getElementById('ynow_lab_im_query_title');
            if (labQueryTitle && s.lab_im_query_title) labQueryTitle.textContent = s.lab_im_query_title;
            var labEvalHint = document.getElementById('ynow_lab_im_eval_hint');
            if (labEvalHint && s.lab_im_eval_hint) labEvalHint.textContent = s.lab_im_eval_hint;
            var labRefreshLabel = document.getElementById('ynow_lab_im_refresh_label');
            if (labRefreshLabel && s.btn_lab_im_refresh) {
              var uniBusyMap = window.__ynowBtnBusy || {};
              labRefreshLabel.textContent = uniBusyMap['lab_im_refresh_universe']
                ? (s.btn_lab_im_refresh_busy || s.btn_lab_im_refresh)
                : s.btn_lab_im_refresh;
            }
            var labRefreshBtn = document.getElementById('lab_im_refresh_universe');
            if (labRefreshBtn && s.lab_im_refresh_title) labRefreshBtn.setAttribute('title', s.lab_im_refresh_title);
            var labDl = document.getElementById('lab_im_download_report');
            if (labDl && s.btn_lab_im_download) {
              var dlIcon = labDl.querySelector('i');
              var dlIconHtml = dlIcon ? dlIcon.outerHTML + ' ' : '';
              labDl.innerHTML = dlIconHtml + s.btn_lab_im_download;
              if (s.lab_im_download_title) labDl.setAttribute('title', s.lab_im_download_title);
            }
            var clusterBlurb = document.getElementById('ynow_lab_cluster_blurb');
            if (clusterBlurb && s.lab_cluster_blurb) clusterBlurb.textContent = s.lab_cluster_blurb;
            var clusterDisc = document.getElementById('ynow_lab_cluster_disclaimer');
            if (clusterDisc && s.lab_cluster_disclaimer) clusterDisc.textContent = s.lab_cluster_disclaimer;
            var clusterHint = document.getElementById('ynow_lab_cluster_hint');
            if (clusterHint && s.lab_cluster_hint) clusterHint.textContent = s.lab_cluster_hint;
            var clusterMode = document.getElementById('ynow_lab_cluster_mode_label');
            if (clusterMode && s.lab_cluster_mode_label) clusterMode.textContent = s.lab_cluster_mode_label;
            var clusterBetaHelp = document.getElementById('ynow_lab_cluster_beta_bins_help');
            if (clusterBetaHelp && s.lab_cluster_beta_bins_help) clusterBetaHelp.textContent = s.lab_cluster_beta_bins_help;
            var clusterBetaMem = document.getElementById('ynow_lab_cluster_beta_membership_label');
            if (clusterBetaMem && s.lab_cluster_beta_membership_label) clusterBetaMem.textContent = s.lab_cluster_beta_membership_label;
            var clusterBetaIqr = document.getElementById('ynow_lab_cluster_beta_show_iqr_label');
            if (clusterBetaIqr && s.lab_cluster_beta_show_iqr_label) clusterBetaIqr.textContent = s.lab_cluster_beta_show_iqr_label;
            var clusterK = document.getElementById('ynow_lab_cluster_k_label');
            if (clusterK && s.lab_cluster_k_label) clusterK.textContent = s.lab_cluster_k_label;
            var clusterX = document.getElementById('ynow_lab_cluster_x_label');
            if (clusterX && s.lab_cluster_x_label) clusterX.textContent = s.lab_cluster_x_label;
            var clusterY = document.getElementById('ynow_lab_cluster_y_label');
            if (clusterY && s.lab_cluster_y_label) clusterY.textContent = s.lab_cluster_y_label;
            var clusterFocus = document.getElementById('ynow_lab_cluster_focus_label');
            if (clusterFocus && s.lab_cluster_focus_label) clusterFocus.textContent = s.lab_cluster_focus_label;
            var clusterRun = document.getElementById('ynow_lab_cluster_run_label');
            if (clusterRun && s.btn_lab_cluster_run) {
              var clBusyMap = window.__ynowBtnBusy || {};
              clusterRun.textContent = clBusyMap['lab_cluster_run']
                ? (s.btn_lab_cluster_run_busy || s.btn_lab_cluster_run)
                : s.btn_lab_cluster_run;
            }
            var clusterMap = document.getElementById('ynow_lab_cluster_map_title');
            if (clusterMap && s.lab_cluster_map_title) clusterMap.textContent = s.lab_cluster_map_title;
            var clusterRadar = document.getElementById('ynow_lab_cluster_radar_title');
            if (clusterRadar && s.lab_cluster_radar_title) clusterRadar.textContent = s.lab_cluster_radar_title;
            var clusterTable = document.getElementById('ynow_lab_cluster_table_title');
            if (clusterTable && s.lab_cluster_table_title) clusterTable.textContent = s.lab_cluster_table_title;
            document.querySelectorAll('[data-i18n]').forEach(function (el) {
              var key = el.getAttribute('data-i18n');
              if (!key || s[key] == null || s[key] === '') return;
              if (el.children && el.children.length) return;
              el.textContent = s[key];
            });
            document.querySelectorAll('[data-i18n-ph]').forEach(function (el) {
              var key = el.getAttribute('data-i18n-ph');
              if (!key || s[key] == null || s[key] === '') return;
              var target = (el.matches && el.matches('input,textarea')) ? el : el.querySelector('input,textarea');
              if (target) target.setAttribute('placeholder', s[key]);
            });
            document.documentElement.setAttribute('lang', (payload && payload.locale) || 'en');
            var mkt = (payload && payload.market) ? String(payload.market) : 'US';
            document.body.classList.toggle('ynow-market-tw', mkt === 'TW');
            document.querySelectorAll('#ynow-market-header .ynow-mkt-btn').forEach(function (b) {
              b.classList.toggle('active', b.getAttribute('data-value') === mkt);
            });
            /* Keep __ynowLocaleUserPick sticky after the first manual click.
               Clearing it on server-confirm let a late queued `en` overwrite
               首頁／關於 after an early 繁中 apply had already succeeded. */
            try {
              document.documentElement.setAttribute('data-ynow-locale-ready', '1');
            } catch (eReady) {}
            if (typeof window.ynowRefreshLockGates === 'function') {
              try { window.ynowRefreshLockGates(); } catch (eGateRefresh) {}
            }
          }
          window.applyUiLocale = applyUiLocale;
          /* Preferred-locale first paint: apply slim boot map as soon as DOM exists
             so Chinese devices never flash English menus then switch. */
          (function ynowApplyBootPreferredLocale() {
            function run() {
              if (window.__ynowBootLocaleApplied) return;
              var pref = window.__ynowPreferredLocale || 'en';
              if (pref !== 'zh-TW') {
                window.__ynowBootLocaleApplied = true;
                try {
                  document.documentElement.setAttribute('data-ynow-locale-ready', '1');
                } catch (eReadyEn) {}
                return;
              }
              try {
                var bootRoot = window.__YNOW_UI_LOCALE_BOOT || {};
                var boot = bootRoot['zh-TW'] || bootRoot['zh-TW'];
                if (!boot || typeof applyUiLocale !== 'function') return;
                window.__ynowBootLocaleApplied = true;
                var mkt = (document.body && document.body.classList.contains('ynow-market-tw'))
                  ? 'TW' : 'US';
                applyUiLocale({
                  locale: boot.locale || 'zh-TW',
                  market: mkt,
                  strings: boot.strings || {},
                  tabs: boot.tabs || {},
                  boxes: boot.boxes || []
                }, {fromBoot: true});
              } catch (eBootPref) {}
            }
            if (document.readyState === 'loading') {
              document.addEventListener('DOMContentLoaded', run);
            } else {
              run();
            }
          })();

          function applyDcLocale(payload) {
            if (!payload) return;
            var hint = document.getElementById('ynow_dc_panel_hint');
            if (hint && payload.panel_hint) hint.textContent = payload.panel_hint;
            (payload.sections || []).forEach(function (sec) {
              var st = document.getElementById('ynow_dc_section_' + sec.id);
              if (st && sec.title) st.textContent = sec.title;
            });
            var items = payload.items || [];
            items.forEach(function (it) {
              var lab = document.getElementById('ynow_dc_label_' + it.id);
              if (lab && it.label) lab.textContent = it.label;
              var h = document.getElementById('ynow_dc_hint_' + it.id);
              if (h && it.hint) h.textContent = it.hint;
              var badge = document.getElementById('ynow_dc_badge_' + it.id);
              if (badge && payload.badge) badge.textContent = payload.badge;
              (it.conds || []).forEach(function (c) {
                var el = document.querySelector('label[for="' + c.input_id + '"]');
                if (el && c.label) el.textContent = c.label;
                var ch = c.hint_id ? document.getElementById(c.hint_id) : null;
                if (ch && c.hint) ch.textContent = c.hint;
              });
            });
          }
          var __ynowLocaleResyncTimer = null;
          function requestLocaleResync(reason) {
            if (!(window.Shiny && Shiny.setInputValue)) return;
            // Debounce: each push ships a large strings payload; flooding delays applyUiLocale.
            if (__ynowLocaleResyncTimer) clearTimeout(__ynowLocaleResyncTimer);
            __ynowLocaleResyncTimer = setTimeout(function () {
              __ynowLocaleResyncTimer = null;
              Shiny.setInputValue('ynow_locale_resync', Date.now(), {priority: 'event'});
            }, 120);
          }
          window.ynowRequestLocaleResync = requestLocaleResync;
          function registerLocaleHandler() {
            if (!window.Shiny || !Shiny.addCustomMessageHandler) {
              setTimeout(registerLocaleHandler, 50);
              return;
            }
            /* Shiny 1.14+ requires handler.length === 1. applyUiLocale(payload, opts)
               has arity 2 for click-time boot opts — register a unary wrapper so
               addCustomMessageHandler does not throw and abort this script (which
               would skip the Lite toggle IIFE below). */
            var addHandler = function (type, fn) {
              try {
                Shiny.addCustomMessageHandler(type, fn);
              } catch (eAdd) {
                /* Re-register on shiny:connected may hit already-added; keep first. */
              }
            };
            addHandler('ynowUiLocale', function (payload) {
              applyUiLocale(payload);
            });
            addHandler('ynowAuthLocale', function (payload) {
              if (!payload) return;
              var set = function (id, val) {
                var el = document.getElementById(id);
                if (el && val) el.textContent = val;
              };
              var setAll = function (sel, val) {
                if (!val) return;
                var nodes = document.querySelectorAll(sel);
                for (var i = 0; i < nodes.length; i++) nodes[i].textContent = val;
              };
              setAll('.ynow-auth-open-label', payload.sign_in);
              setAll('.ynow-auth-signout-label', payload.sign_out);
              setAll('.ynow-auth-register-label', payload.register_btn || payload.register);
              window.__ynowAuthCopy = payload;
              set('ynow_auth_header_label', payload.sign_in);
              set('ynow_auth_date_label', payload.sign_in);
              set('ynow_auth_home_label', payload.sign_in);
              set('ynow_auth_sign_out_label', payload.sign_out);
              set('ynow_auth_sign_out_date_label', payload.sign_out);
              set('ynow_auth_sign_out_home_label', payload.sign_out);
              set('ynow_auth_modal_title', payload.modal_title);
              set('ynow_auth_modal_help', payload.modal_help);
              set('ynow_auth_email_label', payload.email_label);
              set('ynow_auth_password_label', payload.password_label);
              set('ynow_auth_submit_label', payload.submit);
              set('ynow_auth_busy_label', payload.busy || payload.submit);
              set('ynow_auth_cancel_label', payload.cancel);
              set('ynow_auth_or_email', payload.or_email);
              set('ynow_auth_google_label', payload.google);
              set('ynow_auth_facebook_label', payload.facebook);
              set('ynow_auth_linkedin_label', payload.linkedin);
              set('ynow_auth_forgot_label', payload.forgot);
              set('ynow_auth_reset_help', payload.reset_help);
              set('ynow_auth_reset_password_label', payload.reset_password_label);
              set('ynow_auth_reset_password2_label', payload.reset_password2_label);
              set('ynow_auth_reset_submit_label', payload.reset_submit);
              set('ynow_auth_reset_back_label', payload.reset_back);
              set('ynow_auth_change_pw_title', payload.change_pw_title);
              set('ynow_auth_change_pw_help', payload.change_pw_help);
              set('ynow_auth_change_current_label', payload.change_pw_current);
              set('ynow_auth_change_new_label', payload.change_pw_new);
              set('ynow_auth_change_new2_label', payload.change_pw_confirm);
              set('ynow_auth_change_submit_label', payload.change_pw_submit);
              set('ynow_auth_register_label', payload.register_btn || payload.register);
              set('ynow_auth_register_header_label', payload.register_btn || payload.register);
              set('ynow_auth_register_home_label', payload.register_btn || payload.register);
              set('ynow_acct_gate_register_label', payload.register_btn || payload.register);
              set('ynow_auth_resend_label', payload.resend);
              set('ynow_menu_admin', payload.menu_admin);
              set('ynow_menu_admin_overview', payload.menu_admin_overview);
              set('ynow_menu_pts_ledger', payload.menu_pts_ledger);
              set('ynow_menu_settings', payload.menu_settings);
              set('ynow_settings_box_title', payload.settings_title);
              set('ynow_settings_intro', payload.settings_intro);
              set('ynow_settings_hide_locked_label', payload.settings_hide_locked);
              set('ynow_settings_hide_locked_help', payload.settings_hide_locked_help);
              set('ynow_settings_ui_locale_label', payload.settings_ui_locale);
              set('ynow_settings_market_label', payload.settings_market);
              set('ynow_settings_sidebar_compact_label', payload.settings_sidebar_compact);
              set('ynow_settings_sidebar_compact_help', payload.settings_sidebar_compact_help);
              set('ynow_settings_gain_loss_label', payload.settings_gain_loss);
              set('ynow_settings_gain_loss_help', payload.settings_gain_loss_help);
              set('ynow_settings_save_label', payload.settings_save);
              set('ynow_pts_topup_title', payload.pts_topup_title);
              set('ynow_pts_topup_copy', payload.pts_topup_copy);
              set('ynow_pts_topup_help', payload.pts_topup_help);
              set('ynow_pts_redeem_label', payload.pts_topup_redeem_label);
              set('ynow_pts_redeem_submit_label', payload.pts_topup_redeem_cta);
              set('ynow_pts_topup_cta_label', payload.pts_topup_cta);
              (function () {
                var inp = document.getElementById('ynow_pts_redeem_code');
                if (inp && payload.pts_topup_redeem_placeholder) {
                  inp.setAttribute('placeholder', payload.pts_topup_redeem_placeholder);
                }
              })();
              set('ynow_admin_box_title', payload.admin_title);
              set('ynow_admin_intro', payload.admin_intro);
              set('ynow_admin_delete_account_label', payload.delete_account);
              setAll('.ynow-auth-delete-account-label', payload.delete_account);
              set('ynow_auth_delete_modal_title', payload.delete_modal_title);
              set('ynow_auth_delete_modal_warn', payload.delete_modal_warn);
              set('ynow_auth_delete_modal_type', payload.delete_modal_type);
              set('ynow_auth_delete_confirm_label', payload.delete_modal_confirm);
              set('ynow_pts_box_title', payload.pts_box_title);
              set('ynow_pts_ledger_intro', payload.pts_ledger_intro);
              set('ynow_pa_gate_title', payload.pa_gate_title);
              set('ynow_pa_gate_signin_label', payload.sign_in);
              set('ynow_lab_gate_title', payload.pa_gate_title);
              set('ynow_lab_gate_signin_label', payload.sign_in);
              set('ynow_atx_gate_title', payload.pa_gate_title);
              set('ynow_atx_gate_signin_label', payload.sign_in);
              set('ynow_chk_gate_title', payload.pa_gate_title);
              set('ynow_chk_gate_signin_label', payload.sign_in);
              setAll('.ynow-auth-gate-title', payload.pa_gate_title);
              setAll('.ynow-gate-section-kicker', payload.gate_section_kicker || 'Member access');
              setAll('.ynow-member-gate-signin-label', payload.sign_in);
              var setFeature = function (feat, map) {
                if (!feat || !map) return;
                var roots = document.querySelectorAll('[data-ynow-gate-feature="' + feat + '"]');
                for (var ri = 0; ri < roots.length; ri++) {
                  var root = roots[ri];
                  var applySel = function (sel, val) {
                    if (!val) return;
                    var nodes = root.querySelectorAll(sel);
                    for (var ni = 0; ni < nodes.length; ni++) {
                      /* MOS Assumptions reuses feature=checklist for unlock, but keeps
                         its own body copy (#ynow_mos_assump_gate_body). */
                      if (sel === '.ynow-gate-body' &&
                          nodes[ni].id === 'ynow_mos_assump_gate_body') {
                        continue;
                      }
                      nodes[ni].textContent = val;
                    }
                  };
                  applySel('.ynow-gate-body', map.body);
                  applySel('.ynow-gate-promo-kicker', map.kicker);
                  applySel('.ynow-gate-promo-lead', map.lead);
                  applySel('.ynow-gate-promo-b1', map.b1);
                  applySel('.ynow-gate-promo-b2', map.b2);
                  applySel('.ynow-gate-promo-b3', map.b3);
                  applySel('.ynow-gate-demo-caption', map.demo_caption);
                }
                // Body ids on member gates live outside data-feature children sometimes —
                // also set known gate body element ids.
                if (feat === 'lab') set('ynow_lab_gate_body', map.body);
                if (feat === 'checklist') set('ynow_chk_gate_body', map.body);
                if (feat === 'alerts') set('ynow_pa_gate_body', map.body);
                if (feat === 'atx') set('ynow_atx_gate_body', map.body);
                if (feat === 'snap_defaults') set('ynow_snapshot_defaults_locked_msg', map.body);
              };
              setFeature('lab', {
                body: payload.gate_lab_body, kicker: payload.gate_lab_kicker,
                lead: payload.gate_lab_lead, b1: payload.gate_lab_b1,
                b2: payload.gate_lab_b2, b3: payload.gate_lab_b3,
                demo_caption: payload.gate_lab_demo_caption
              });
              setFeature('checklist', {
                body: payload.gate_checklist_body, kicker: payload.gate_checklist_kicker,
                lead: payload.gate_checklist_lead, b1: payload.gate_checklist_b1,
                b2: payload.gate_checklist_b2, b3: payload.gate_checklist_b3,
                demo_caption: payload.gate_checklist_demo_caption
              });
              /* Restore MOS Assumptions body after checklist feature stamp. */
              if (payload.mos_assump_gate_body) {
                set('ynow_mos_assump_gate_body', payload.mos_assump_gate_body);
              }
              setFeature('alerts', {
                body: payload.gate_alerts_body, kicker: payload.gate_alerts_kicker,
                lead: payload.gate_alerts_lead, b1: payload.gate_alerts_b1,
                b2: payload.gate_alerts_b2, b3: payload.gate_alerts_b3,
                demo_caption: payload.gate_alerts_demo_caption
              });
              setFeature('pdf', {
                body: payload.gate_pdf_body, kicker: payload.gate_pdf_kicker,
                lead: payload.gate_pdf_lead, b1: payload.gate_pdf_b1,
                b2: payload.gate_pdf_b2, b3: payload.gate_pdf_b3,
                demo_caption: payload.gate_pdf_demo_caption
              });
              setFeature('snap_defaults', {
                body: payload.gate_snap_defaults_body, kicker: payload.gate_snap_defaults_kicker,
                lead: payload.gate_snap_defaults_lead, b1: payload.gate_snap_defaults_b1,
                b2: payload.gate_snap_defaults_b2, b3: payload.gate_snap_defaults_b3,
                demo_caption: payload.gate_snap_defaults_demo_caption
              });
              setFeature('inner_lock', {
                body: payload.gate_inner_lock_body, kicker: payload.gate_inner_lock_kicker,
                lead: payload.gate_inner_lock_lead, b1: payload.gate_inner_lock_b1,
                b2: payload.gate_inner_lock_b2, b3: payload.gate_inner_lock_b3,
                demo_caption: payload.gate_inner_lock_demo_caption
              });
              setFeature('atx', {
                body: payload.gate_atx_body, kicker: payload.gate_atx_kicker,
                lead: payload.gate_atx_lead, b1: payload.gate_atx_b1,
                b2: payload.gate_atx_b2, b3: payload.gate_atx_b3,
                demo_caption: payload.gate_atx_demo_caption
              });
              setAll('.ynow-gate-faq-title', payload.gate_faq_title);
              setAll('.ynow-gate-faq-q1', payload.gate_faq_q1);
              setAll('.ynow-gate-faq-a1', payload.gate_faq_a1);
              setAll('.ynow-gate-faq-q2', payload.gate_faq_q2);
              setAll('.ynow-gate-faq-a2', payload.gate_faq_a2);
              setAll('.ynow-gate-faq-q3', payload.gate_faq_q3);
              setAll('.ynow-gate-faq-a3', payload.gate_faq_a3);
              setAll('.ynow-gate-faq-q4', payload.gate_faq_q4);
              setAll('.ynow-gate-faq-a4', payload.gate_faq_a4);
            });
            window.__ynowPdfQuotaState = window.__ynowPdfQuotaState || {
              state: 'guest',
              left: null,
              limit: 3,
              cost: 10,
              remaining: '',
              rules: '',
              snap: ''
            };
            window.__ynowQuotaBadges = window.__ynowQuotaBadges || [];
            window.__ynowQuotaServerSkewMs = window.__ynowQuotaServerSkewMs || 0;
            window.ynowFmtCountdown = function (endsAtIso) {
              if (!endsAtIso) return '';
              var endMs = Date.parse(endsAtIso);
              if (!isFinite(endMs)) return '';
              var nowMs = Date.now() + (window.__ynowQuotaServerSkewMs || 0);
              var sec = Math.floor((endMs - nowMs) / 1000);
              if (sec <= 0) return '';
              var h = Math.floor(sec / 3600);
              var m = Math.floor((sec % 3600) / 60);
              var s = sec % 60;
              var pad = function (n) { return (n < 10 ? '0' : '') + String(n); };
              if (h > 0) return h + ':' + pad(m) + ':' + pad(s);
              return m + ':' + pad(s);
            };
            window.ynowSyncDownloadReportLabel = function () {
              var dl = document.getElementById('download_report');
              if (!dl) return;
              var s = window.__ynowUiLocaleStrings || {};
              var base = s.download_report || 'Download Report (PDF)';
              var st = window.__ynowPdfQuotaState || {};
              var liteOn = document.body && document.body.classList.contains('ynow-lite');
              var label = base;
              if (!liteOn && st.state !== 'lite' &&
                  st.left != null && st.limit != null &&
                  isFinite(Number(st.left)) && isFinite(Number(st.limit))) {
                var fmt = s.quota_count_fmt || '(%s/%s)';
                var countTxt = String(fmt)
                  .replace('%s', String(st.left))
                  .replace('%s', String(st.limit));
                label = base + ' ' + countTxt;
              }
              var icon = dl.querySelector(':scope > i, i');
              var iconHtml = '';
              if (icon && icon.parentNode === dl) {
                iconHtml = icon.outerHTML + ' ';
              } else if (icon) {
                iconHtml = icon.outerHTML + ' ';
              }
              var ttl = dl.querySelector(':scope > .ynow-ttl-countdown');
              var ttlHtml = ttl ? ttl.outerHTML : '';
              dl.innerHTML = iconHtml + label + ttlHtml;
              if (typeof window.ynowSyncSidebarDownloadIcon === 'function') {
                window.ynowSyncSidebarDownloadIcon();
              }
            };
            window.ynowClearQuotaBadgeChrome = function () {
              document.querySelectorAll('.ynow-count-badge, .ynow-ttl-countdown').forEach(function (el) {
                /* Keep PDF label-embedded count; only remove Class C / maint nodes. */
                if (el.classList.contains('ynow-count-badge') &&
                    el.getAttribute('data-ynow-badge-src') === 'pdf') {
                  return;
                }
                if (el.parentNode) el.parentNode.removeChild(el);
              });
              document.querySelectorAll('.ynow-quota-badge-host').forEach(function (el) {
                el.classList.remove('ynow-quota-badge-host');
              });
            };
            window.ynowQuotaBadgeHosts = function (itemId) {
              var id = String(itemId || '');
              var hosts = [];
              if (id.indexOf('tab:') === 0) {
                var tab = id.slice(4);
                document.querySelectorAll('a[data-value]').forEach(function (a) {
                  if (a.getAttribute('data-value') === tab) hosts.push(a);
                });
              } else if (id.indexOf('report:') === 0) {
                var btn = document.getElementById(id.slice(7));
                if (btn) hosts.push(btn);
              } else if (id.indexOf('section:') === 0 || id.indexOf('output:') === 0) {
                document.querySelectorAll('[data-ynow-lock]').forEach(function (el) {
                  if (el.getAttribute('data-ynow-lock') === id) hosts.push(el);
                });
              } else if (id.indexOf('input:') === 0) {
                /* Retired catalog kind; keep DOM resolve for any stale client payload. */
                var inpLegacy = document.getElementById(id.slice(6));
                var fgLegacy = inpLegacy && (inpLegacy.closest('.form-group') ||
                  inpLegacy.closest('.shiny-input-container'));
                if (fgLegacy) hosts.push(fgLegacy);
              }
              return hosts;
            };
            window.ynowEnsureCountBadge = function (host, remaining, total, src) {
              if (!host || remaining == null || total == null) return;
              if (!isFinite(Number(remaining)) || !isFinite(Number(total))) return;
              var s = window.__ynowUiLocaleStrings || {};
              var fmt = s.quota_count_fmt || '(%s/%s)';
              var txt = String(fmt)
                .replace('%s', String(remaining))
                .replace('%s', String(total));
              /* Download Report PDF freemium already embeds count in the button label. */
              if (host.id === 'download_report' && src !== 'pdf') return;
              if (host.id === 'download_report' && src === 'pdf') {
                if (typeof window.ynowSyncDownloadReportLabel === 'function') {
                  window.ynowSyncDownloadReportLabel();
                }
                return;
              }
              var badge = null;
              var kids = host.children || [];
              var bi;
              for (bi = 0; bi < kids.length; bi++) {
                if (kids[bi].classList && kids[bi].classList.contains('ynow-count-badge')) {
                  badge = kids[bi];
                  break;
                }
              }
              if (!badge) {
                badge = document.createElement('span');
                badge.className = 'ynow-count-badge';
                host.appendChild(badge);
              }
              badge.setAttribute('data-ynow-badge-src', src || 'acl');
              badge.textContent = ' ' + txt;
              host.classList.add('ynow-quota-badge-host');
            };
            window.ynowEnsureTtlCountdown = function (host, endsAtIso, src) {
              if (!host || !endsAtIso) return;
              var txt = window.ynowFmtCountdown(endsAtIso);
              var el = null;
              var kids = host.children || [];
              var i;
              for (i = 0; i < kids.length; i++) {
                if (kids[i].classList && kids[i].classList.contains('ynow-ttl-countdown')) {
                  el = kids[i];
                  break;
                }
              }
              if (!txt) {
                if (el && el.parentNode) el.parentNode.removeChild(el);
                return;
              }
              if (!el) {
                el = document.createElement('span');
                el.className = 'ynow-ttl-countdown';
                host.appendChild(el);
              }
              el.setAttribute('data-ends-at', endsAtIso);
              el.setAttribute('data-ynow-badge-src', src || 'acl');
              el.setAttribute('aria-live', 'off');
              el.textContent = txt;
              host.classList.add('ynow-quota-badge-host');
            };
            window.ynowApplyQuotaBadges = function () {
              if (typeof window.ynowClearQuotaBadgeChrome === 'function') {
                window.ynowClearQuotaBadgeChrome();
              }
              var badges = window.__ynowQuotaBadges || [];
              badges.forEach(function (row) {
                if (!row || !row.id) return;
                var hosts = window.ynowQuotaBadgeHosts(row.id) || [];
                hosts.forEach(function (host) {
                  if (row.remaining != null && row.total != null) {
                    window.ynowEnsureCountBadge(host, row.remaining, row.total, 'acl');
                  }
                  if (row.ends_at) {
                    window.ynowEnsureTtlCountdown(host, row.ends_at, 'acl');
                  }
                });
              });
              /* Maintenance windows: live countdown to window_end. */
              var maint = window.__ynowMaintEntries || [];
              maint.forEach(function (row) {
                if (!row || !row.id || !row.window_end) return;
                var hosts = window.ynowQuotaBadgeHosts(row.id) || [];
                hosts.forEach(function (host) {
                  window.ynowEnsureTtlCountdown(host, row.window_end, 'maint');
                });
              });
              if (typeof window.ynowSyncDownloadReportLabel === 'function') {
                window.ynowSyncDownloadReportLabel();
              }
            };
            window.ynowTickQuotaCountdowns = function () {
              var expired = false;
              document.querySelectorAll('.ynow-ttl-countdown[data-ends-at]').forEach(function (el) {
                var ends = el.getAttribute('data-ends-at');
                var txt = window.ynowFmtCountdown(ends);
                if (!txt) {
                  expired = true;
                  if (el.parentNode) el.parentNode.removeChild(el);
                  return;
                }
                el.textContent = txt;
              });
              if (expired) {
                if (window.Shiny && Shiny.setInputValue) {
                  Shiny.setInputValue(
                    'ynow_quota_badge_refresh',
                    (window.__ynowQuotaBadgeRefreshClicks =
                      (window.__ynowQuotaBadgeRefreshClicks || 0) + 1),
                    { priority: 'event' }
                  );
                }
              }
            };
            if (!window.__ynowQuotaTickTimer) {
              window.__ynowQuotaTickTimer = setInterval(function () {
                if (typeof window.ynowTickQuotaCountdowns === 'function') {
                  window.ynowTickQuotaCountdowns();
                }
              }, 1000);
            }
            window.ynowApplyPdfQuotaCopy = function (localeBundle) {
              var st = window.__ynowPdfQuotaState || {};
              var s = localeBundle || window.__ynowUiLocaleStrings || {};
              var rem = document.getElementById('ynow_pdf_quota_remaining');
              var rules = document.getElementById('ynow_pdf_quota_rules');
              var snap = document.getElementById('ynow_snapshot_download_quota');
              var liteOn = document.body && document.body.classList.contains('ynow-lite');
              if (liteOn || st.state === 'lite') {
                if (rem) rem.textContent = s.pdf_quota_hint_lite || st.remaining || '';
                if (rules) rules.textContent = '';
                if (snap) {
                  snap.textContent = s.snapshot_download_quota_hint_lite || st.snap || '';
                }
                if (typeof window.ynowSyncDownloadReportLabel === 'function') {
                  window.ynowSyncDownloadReportLabel();
                }
                return;
              }
              if (st.remaining) {
                if (rem) rem.textContent = st.remaining;
                if (rules) rules.textContent = st.rules || '';
                if (snap && st.snap) snap.textContent = st.snap;
                if (typeof window.ynowSyncDownloadReportLabel === 'function') {
                  window.ynowSyncDownloadReportLabel();
                }
                return;
              }
              if (rem && s.pdf_quota_hint) rem.textContent = s.pdf_quota_hint;
              if (rules && s.pdf_quota_rules_fmt) {
                var cost = (st.cost != null) ? String(st.cost) : '10';
                rules.textContent = String(s.pdf_quota_rules_fmt).replace('%s', cost);
              }
              if (snap && s.snapshot_download_quota_hint) {
                snap.textContent = s.snapshot_download_quota_hint;
              }
              if (typeof window.ynowSyncDownloadReportLabel === 'function') {
                window.ynowSyncDownloadReportLabel();
              }
            };
            addHandler('ynowPdfQuota', function (payload) {
              if (!payload) return;
              window.__ynowPdfQuotaState = {
                state: payload.state || 'guest',
                left: payload.left,
                limit: payload.limit,
                cost: payload.cost,
                remaining: payload.remaining || '',
                rules: payload.rules || '',
                snap: payload.snap || ''
              };
              var rem = document.getElementById('ynow_pdf_quota_remaining');
              var rules = document.getElementById('ynow_pdf_quota_rules');
              var snap = document.getElementById('ynow_snapshot_download_quota');
              if (rem) rem.textContent = payload.remaining || '';
              if (rules) rules.textContent = payload.rules || '';
              if (snap && payload.snap != null) snap.textContent = payload.snap || '';
              if (typeof window.ynowSyncDownloadReportLabel === 'function') {
                window.ynowSyncDownloadReportLabel();
              }
            });
            addHandler('ynowPtsRedeemStatus', function (payload) {
              var el = document.getElementById('ynow_pts_redeem_status');
              if (!el || !payload) return;
              el.textContent = payload.text || '';
              if (payload.ok === true) el.setAttribute('data-ok', '1');
              else if (payload.ok === false) el.setAttribute('data-ok', '0');
              else el.setAttribute('data-ok', '');
            });
            addHandler('ynowPtsBanner', function (payload) {
              if (!payload) return;
              var pairs = [
                {
                  fynLab: 'ynow_pts_banner_fyn_label',
                  pynLab: 'ynow_pts_banner_pyn_label',
                  fynVal: 'ynow_pts_banner_fyn_value',
                  pynVal: 'ynow_pts_banner_pyn_value',
                  wrap: 'ynow-pts-header',
                  banner: 'ynow_pts_banner'
                },
                {
                  fynLab: 'ynow_pts_sidebar_fyn_label',
                  pynLab: 'ynow_pts_sidebar_pyn_label',
                  fynVal: 'ynow_pts_sidebar_fyn_value',
                  pynVal: 'ynow_pts_sidebar_pyn_value',
                  wrap: 'ynow-pts-sidebar',
                  banner: 'ynow_pts_sidebar_banner'
                }
              ];
              var i, el, wrap, banner;
              /* Guests: hide + clear DOM (do not map empty to em-dash placeholders). */
              if (payload.visible === false) {
                for (i = 0; i < pairs.length; i++) {
                  el = document.getElementById(pairs[i].fynLab); if (el) el.textContent = '';
                  el = document.getElementById(pairs[i].pynLab); if (el) el.textContent = '';
                  el = document.getElementById(pairs[i].fynVal); if (el) el.textContent = '';
                  el = document.getElementById(pairs[i].pynVal); if (el) el.textContent = '';
                  wrap = document.getElementById(pairs[i].wrap);
                  if (wrap) wrap.classList.add('ynow-pts-hidden');
                  banner = document.getElementById(pairs[i].banner);
                  if (banner) banner.setAttribute('aria-hidden', 'true');
                }
                return;
              }
              for (i = 0; i < pairs.length; i++) {
                el = document.getElementById(pairs[i].fynLab);
                if (el && payload.fyn_label) el.textContent = payload.fyn_label;
                el = document.getElementById(pairs[i].pynLab);
                if (el && payload.pyn_label) el.textContent = payload.pyn_label;
                el = document.getElementById(pairs[i].fynVal);
                if (el) el.textContent = (payload.fyn != null && payload.fyn !== '') ? String(payload.fyn) : '—';
                el = document.getElementById(pairs[i].pynVal);
                if (el) el.textContent = (payload.pyn != null && payload.pyn !== '') ? String(payload.pyn) : '—';
                wrap = document.getElementById(pairs[i].wrap);
                if (wrap) wrap.classList.remove('ynow-pts-hidden');
                banner = document.getElementById(pairs[i].banner);
                if (banner) banner.setAttribute('aria-hidden', 'false');
              }
            });
            /* Apply signed-in chrome without mutating real auth (view-as preview). */
            window.ynowApplySessionLikeAuthChrome = function (on, labels) {
              if (!document.body) return;
              var openLinks = document.querySelectorAll('.ynow-auth-open');
              var regLinks = document.querySelectorAll('.ynow-auth-register-open');
              var outLinks = document.querySelectorAll('.ynow-auth-signout');
              var i;
              for (i = 0; i < openLinks.length; i++) {
                openLinks[i].style.display = on ? 'none' : '';
              }
              for (i = 0; i < regLinks.length; i++) {
                regLinks[i].style.display = on ? 'none' : '';
              }
              for (i = 0; i < outLinks.length; i++) {
                outLinks[i].style.display = on ? '' : 'none';
              }
              if (labels && labels.label_sign_out) {
                var outLbls = document.querySelectorAll('.ynow-auth-signout-label');
                for (i = 0; i < outLbls.length; i++) outLbls[i].textContent = labels.label_sign_out;
              }
              if (typeof window.ynowSyncSysAdminFootVisibility === 'function') {
                window.ynowSyncSysAdminFootVisibility(!!on);
              }
              if (typeof window.ynowSyncSidebarDownloadIcon === 'function') {
                window.ynowSyncSidebarDownloadIcon();
              }
              if (typeof window.ynowSyncGuestSidebarLocked === 'function') {
                window.ynowSyncGuestSidebarLocked();
              }
            };
            window.ynowHideAdminModeReminder = function () {
              var el = document.getElementById('ynow_admin_mode_reminder');
              if (!el) return;
              el.classList.remove('is-visible');
              el.setAttribute('hidden', 'hidden');
              el.removeAttribute('data-mode');
            };
            window.ynowShowAdminModeReminder = function (opts) {
              opts = opts || {};
              var el = document.getElementById('ynow_admin_mode_reminder');
              if (!el) return;
              var title = String(opts.title || '');
              var body = String(opts.body || '');
              if (!title && !body) return;
              var titleEl = document.getElementById('ynow_admin_mode_reminder_title');
              var bodyEl = document.getElementById('ynow_admin_mode_reminder_body');
              var exitLab = document.getElementById('ynow_admin_mode_reminder_exit_label');
              if (titleEl) titleEl.textContent = title;
              if (bodyEl) bodyEl.textContent = body;
              if (exitLab && opts.exit_label) exitLab.textContent = String(opts.exit_label);
              if (opts.aria) el.setAttribute('aria-label', String(opts.aria));
              el.setAttribute('data-mode', String(opts.mode || ''));
              el.removeAttribute('hidden');
              el.classList.add('is-visible');
            };
            (function bindAdminModeReminderExit() {
              var btn = document.getElementById('ynow_admin_mode_reminder_exit');
              if (!btn || btn.__ynowBound) return;
              btn.__ynowBound = true;
              btn.addEventListener('click', function (ev) {
                if (ev && ev.preventDefault) ev.preventDefault();
                var el = document.getElementById('ynow_admin_mode_reminder');
                var mode = el ? String(el.getAttribute('data-mode') || '') : '';
                if (!(window.Shiny && Shiny.setInputValue)) return;
                if (mode === 'view_as') {
                  Shiny.setInputValue(
                    'ynow_pts_sysadmin_viewas_exit',
                    (window.__ynowViewAsExitClicks = (window.__ynowViewAsExitClicks || 0) + 1),
                    { priority: 'event' }
                  );
                } else {
                  Shiny.setInputValue(
                    'ynow_pts_sysadmin_self_exit',
                    (window.__ynowSelfExitClicks = (window.__ynowSelfExitClicks || 0) + 1),
                    { priority: 'event' }
                  );
                }
              });
            })();
            addHandler('ynowViewAsMode', function (payload) {
              if (!payload) return;
              var on = !!payload.active;
              if (on) {
                document.body.classList.add('ynow-view-as');
                /* Session-like Current App: unlock guest chrome as if this user is signed in. */
                document.body.classList.add('ynow-authed');
                window.__ynowViewAsSessionLike = true;
                window.ynowApplySessionLikeAuthChrome(true, {
                  label_sign_out: payload.label_sign_out || ''
                });
                if (typeof window.ynowShowAdminModeReminder === 'function') {
                  var vaTitle = payload.reminder_title || payload.active_label || '';
                  var vaBody = payload.reminder_body || payload.hint || '';
                  if (payload.public_id) {
                    vaBody = (vaBody ? (vaBody + ' ') : '') +
                      '(' + String(payload.public_id) + ')';
                  }
                  window.ynowShowAdminModeReminder({
                    mode: 'view_as',
                    title: vaTitle,
                    body: vaBody,
                    exit_label: payload.exit_label || payload.reminder_dismiss || '',
                    aria: payload.reminder_aria || ''
                  });
                }
              } else {
                document.body.classList.remove('ynow-view-as');
                window.__ynowViewAsSessionLike = false;
                /* Real AuthState restore is pushed by server; clear only if still fake. */
                if (!window.__ynowRealAuthed) {
                  document.body.classList.remove('ynow-authed');
                  window.ynowApplySessionLikeAuthChrome(false, null);
                } else {
                  window.ynowApplySessionLikeAuthChrome(true, null);
                }
                if (typeof window.ynowHideAdminModeReminder === 'function') {
                  window.ynowHideAdminModeReminder();
                }
              }
              var pidEl = document.getElementById('ynow_viewas_chrome_pid');
              if (pidEl) pidEl.textContent = on && payload.public_id ? String(payload.public_id) : '';
              var actEl = document.getElementById('ynow_viewas_chrome_active_label');
              if (actEl && payload.active_label) actEl.textContent = String(payload.active_label);
              var hintEl = document.getElementById('ynow_viewas_chrome_hint');
              if (hintEl && payload.hint) hintEl.textContent = String(payload.hint);
              var exitLab = document.getElementById('ynow_viewas_chrome_exit_label');
              if (exitLab && payload.exit_label) exitLab.textContent = String(payload.exit_label);
            });
            addHandler('ynowSysadminMode', function (payload) {
              if (!payload) return;
              if (payload.on) {
                document.body.classList.add('ynow-sysadmin-on');
                /* Re-sync foot link after unlock — self-view may have set ynow-authed. */
                if (typeof window.ynowSyncSysAdminFootVisibility === 'function') {
                  var keepAuthed = document.body.classList.contains('ynow-authed');
                  window.ynowSyncSysAdminFootVisibility(keepAuthed);
                }
                /* Lite omits snap_defaults entirely — do not force that locked tab. */
              } else {
                document.body.classList.remove('ynow-sysadmin-on');
                /* Leaving portal also clears self-view chrome classes. */
                document.body.classList.remove('ynow-self-view', 'ynow-omniscient',
                  'ynow-self-view-guest', 'ynow-self-view-signed-in');
                window.__ynowSelfViewMode = '';
                if (typeof window.ynowHideAdminModeReminder === 'function') {
                  window.ynowHideAdminModeReminder();
                }
                if (typeof window.ynowSyncSysAdminFootVisibility === 'function') {
                  var realOn = !!window.__ynowRealAuthed;
                  window.ynowSyncSysAdminFootVisibility(realOn);
                }
                try {
                  var activeSnap = document.querySelector('#snapshot_report .nav-tabs li.active a');
                  var onDefaults = activeSnap &&
                    activeSnap.getAttribute('data-value') === 'snap_defaults';
                  /* Leaving sysadmin: leave APP_DEFAULTS; Lite uses free Snapshot tabs. */
                  if (onDefaults) {
                    var fallback = document.querySelector('#snapshot_report a[data-value="snap_current"]') ||
                      document.querySelector('#snapshot_report a[data-value="snap_audit"]');
                    if (fallback) {
                      if (window.jQuery) window.jQuery(fallback).tab('show');
                      else if (typeof fallback.click === 'function') fallback.click();
                    }
                  }
                } catch (eOff) {}
              }
            });
            window.__ynowSelfViewMode = window.__ynowSelfViewMode || '';
            addHandler('ynowSelfViewMode', function (payload) {
              if (!payload) return;
              var mode = String(payload.mode || '').toLowerCase();
              var on = mode === 'guest' || mode === 'signed_in' || mode === 'omniscient';
              document.body.classList.remove(
                'ynow-self-view', 'ynow-omniscient',
                'ynow-self-view-guest', 'ynow-self-view-signed-in'
              );
              window.__ynowSelfViewMode = on ? mode : '';
              if (on) {
                document.body.classList.add('ynow-self-view');
                if (mode === 'guest') {
                  document.body.classList.add('ynow-self-view-guest');
                  document.body.classList.remove('ynow-authed');
                  window.__ynowSelfViewSessionLike = false;
                  window.ynowApplySessionLikeAuthChrome(false, null);
                } else if (mode === 'signed_in') {
                  document.body.classList.add('ynow-self-view-signed-in');
                  document.body.classList.add('ynow-authed');
                  window.__ynowSelfViewSessionLike = true;
                  window.ynowApplySessionLikeAuthChrome(true, {
                    label_sign_out: payload.label_sign_out || ''
                  });
                } else if (mode === 'omniscient') {
                  document.body.classList.add('ynow-omniscient');
                  document.body.classList.add('ynow-authed');
                  window.__ynowSelfViewSessionLike = true;
                  window.ynowApplySessionLikeAuthChrome(true, {
                    label_sign_out: payload.label_sign_out || ''
                  });
                }
                if (typeof window.ynowShowAdminModeReminder === 'function') {
                  window.ynowShowAdminModeReminder({
                    mode: mode,
                    title: payload.reminder_title || '',
                    body: payload.reminder_body || '',
                    exit_label: payload.exit_label || '',
                    aria: payload.reminder_aria || ''
                  });
                }
              } else {
                window.__ynowSelfViewSessionLike = false;
                if (typeof window.ynowHideAdminModeReminder === 'function' &&
                    !window.__ynowViewAsSessionLike) {
                  window.ynowHideAdminModeReminder();
                }
                /* Restore real auth chrome unless view-as is still faking it. */
                if (window.__ynowViewAsSessionLike) {
                  document.body.classList.add('ynow-authed');
                  window.ynowApplySessionLikeAuthChrome(true, null);
                } else if (!window.__ynowRealAuthed) {
                  document.body.classList.remove('ynow-authed');
                  window.ynowApplySessionLikeAuthChrome(false, null);
                } else {
                  document.body.classList.add('ynow-authed');
                  window.ynowApplySessionLikeAuthChrome(true, null);
                }
              }
              if (payload.exit_label) {
                var exitLab = document.getElementById('ynow_pts_sysadmin_self_exit_label');
                if (exitLab) exitLab.textContent = String(payload.exit_label);
              }
              if (typeof window.ynowApplyAccessLocks === 'function') {
                try { window.ynowApplyAccessLocks(); } catch (eAl) {}
              }
              if (typeof window.ynowSyncGuestSidebarLocked === 'function') {
                try { window.ynowSyncGuestSidebarLocked(); } catch (eGs) {}
              }
            });
            function ynowResolveLockGateMode(authed, unlockType, pointsCost) {
              if (!authed) return 'signin';
              var ut = unlockType ? String(unlockType) : '';
              var cost = parseInt(pointsCost, 10);
              if (ut === 'points' && isFinite(cost) && cost >= 1) return 'spend';
              return 'topup';
            }
            function ynowLockGateCtx(itemId) {
              var id = itemId || '';
              var row = null;
              var entries = window.__ynowAccessEntries || [];
              var i;
              for (i = 0; i < entries.length; i++) {
                if (entries[i] && String(entries[i].id) === id) { row = entries[i]; break; }
              }
              var unlockType = null;
              var pointsCost = null;
              if (row) {
                unlockType = row.unlock_type ? String(row.unlock_type) : null;
                if (row.points_cost != null && row.points_cost !== '') pointsCost = row.points_cost;
              }
              return {
                authed: !!(document.body && document.body.classList.contains('ynow-authed')),
                unlockType: unlockType,
                pointsCost: pointsCost,
                itemId: id,
                copy: window.__ynowUiLocaleStrings || {}
              };
            }
            function ynowLockGateFmt(fmt, a, b) {
              var s = String(fmt == null ? '' : fmt);
              s = s.replace('%s', a == null ? '' : String(a));
              if (arguments.length > 2) s = s.replace('%s', b == null ? '' : String(b));
              return s;
            }
            function ynowLockGateButton(kind, label, itemId) {
              var btn = document.createElement('button');
              btn.type = 'button';
              btn.className = 'btn ' + (kind === 'spend' ? 'btn-primary ynow-lock-spend' : 'btn-default ynow-lock-topup');
              btn.setAttribute('data-ynow-item', itemId || '');
              btn.textContent = label || '';
              return btn;
            }
            function ynowAdaptPostLoginLockGate(root, ctx) {
              if (!root) return 'signin';
              ctx = ctx || {};
              var mode = ynowResolveLockGateMode(!!ctx.authed, ctx.unlockType, ctx.pointsCost);
              var itemId = ctx.itemId || '';
              var copy = ctx.copy || window.__ynowUiLocaleStrings || {};
              root.setAttribute('data-ynow-gate-mode', mode);
              root.setAttribute('data-ynow-item', itemId);
              if (ctx.unlockType) root.setAttribute('data-ynow-unlock-type', String(ctx.unlockType));
              else root.removeAttribute('data-ynow-unlock-type');
              if (ctx.pointsCost != null && ctx.pointsCost !== '' && isFinite(parseInt(ctx.pointsCost, 10))) {
                root.setAttribute('data-ynow-points-cost', String(parseInt(ctx.pointsCost, 10)));
              } else {
                root.removeAttribute('data-ynow-points-cost');
              }
              root.classList.remove('ynow-lock-gate--signin', 'ynow-lock-gate--spend', 'ynow-lock-gate--topup');
              root.classList.add('ynow-lock-gate--' + mode);
              if (mode === 'signin') return mode;
              var kicker = copy.lock_gate_spend_kicker || 'Points unlock';
              var title = copy.lock_gate_spend_title || 'Unlock with FYN';
              var body = ynowLockGateFmt(
                copy.lock_gate_spend_body_fmt || 'Spend %s FYN to unlock this item on this account.',
                ctx.pointsCost
              );
              if (mode === 'topup') {
                kicker = copy.lock_gate_topup_kicker || 'Points top-up';
                title = copy.lock_gate_topup_title || 'Points top-up';
                body = copy.lock_gate_topup_body || 'This item stays locked on this account. Credit FYN on Points top-up, then come back.';
              }
              var nodes;
              var ni;
              nodes = root.querySelectorAll('.ynow-gate-section-kicker');
              for (ni = 0; ni < nodes.length; ni++) nodes[ni].textContent = kicker;
              nodes = root.querySelectorAll('.ynow-auth-gate-title');
              for (ni = 0; ni < nodes.length; ni++) nodes[ni].textContent = title;
              if (!nodes.length) {
                nodes = root.querySelectorAll('.ynow-hfv-chapter__title');
                for (ni = 0; ni < nodes.length; ni++) nodes[ni].textContent = title;
              }
              nodes = root.querySelectorAll('.ynow-gate-body');
              for (ni = 0; ni < nodes.length; ni++) nodes[ni].textContent = body;
              nodes = root.querySelectorAll('.ynow-gate-faq, .ynow-gate-promo');
              for (ni = 0; ni < nodes.length; ni++) nodes[ni].style.display = 'none';
              var pair = root.querySelector('.ynow-lock-gate-actions') ||
                root.querySelector('.ynow-auth-pair--gate') ||
                root.querySelector('.ynow-auth-pair');
              if (!pair) {
                pair = document.createElement('div');
                root.appendChild(pair);
              }
              pair.className = 'ynow-lock-gate-actions';
              pair.style.display = '';
              pair.style.visibility = '';
              pair.style.pointerEvents = '';
              while (pair.firstChild) pair.removeChild(pair.firstChild);
              if (mode === 'spend') {
                pair.appendChild(ynowLockGateButton(
                  'spend',
                  copy.lock_gate_spend_cta || 'Spend FYN',
                  itemId
                ));
                pair.appendChild(ynowLockGateButton(
                  'topup',
                  copy.lock_gate_topup_cta || 'Open Points top-up',
                  itemId
                ));
              } else {
                pair.appendChild(ynowLockGateButton(
                  'topup',
                  copy.lock_gate_topup_cta || 'Open Points top-up',
                  itemId
                ));
              }
              return mode;
            }
            function ynowRefreshLockGates() {
              var nodes = document.querySelectorAll('[data-ynow-gate-mode]');
              var i;
              for (i = 0; i < nodes.length; i++) {
                var itemId = nodes[i].getAttribute('data-ynow-item') || '';
                ynowAdaptPostLoginLockGate(nodes[i], ynowLockGateCtx(itemId));
              }
            }
            window.ynowResolveLockGateMode = ynowResolveLockGateMode;
            window.ynowAdaptPostLoginLockGate = ynowAdaptPostLoginLockGate;
            window.ynowRefreshLockGates = ynowRefreshLockGates;
            if (!document.documentElement.getAttribute('data-ynow-lock-spend-click')) {
              document.documentElement.setAttribute('data-ynow-lock-spend-click', '1');
              document.addEventListener('click', function (ev) {
                var t = ev.target;
                if (!t || !t.closest) return;
                var spend = t.closest('.ynow-lock-spend');
                var topup = t.closest('.ynow-lock-topup');
                if (!spend && !topup) return;
                ev.preventDefault();
                var authed = !!(document.body && document.body.classList.contains('ynow-authed'));
                if (window.ynowIsAuthed) authed = !!window.ynowIsAuthed();
                if (!authed) return;
                if (topup) {
                  var lite = document.body && document.body.classList.contains('ynow-lite');
                  /* Redeem form is #ynow_pts_topup_slot on Account overview, not the ledger tab. */
                  var overview = document.querySelector('.sidebar-menu a[data-value="account_admin"]');
                  if (!lite && overview && window.ynowGotoTab) {
                    window.ynowGotoTab('account_admin');
                    var slot = document.getElementById('ynow_pts_topup_slot');
                    if (slot && slot.scrollIntoView) {
                      try { slot.scrollIntoView({ block: 'nearest' }); } catch (eSlot) {}
                    }
                  }
                }
                if (spend && window.Shiny && Shiny.setInputValue) {
                  Shiny.setInputValue('ynow_lock_spend', {
                    item_id: spend.getAttribute('data-ynow-item') || '',
                    nonce: Date.now()
                  }, { priority: 'event' });
                }
              }, true);
            }
            function ynowEnsureAtxLockPromo(pane, itemId) {
              if (!pane) return;
              var kids = pane.children || [];
              for (var hi = 0; hi < kids.length; hi++) {
                if (kids[hi].classList && kids[hi].classList.contains('ynow-atx-lock-host')) {
                  ynowAdaptPostLoginLockGate(kids[hi], ynowLockGateCtx(itemId));
                  return;
                }
              }
              var tpl = document.getElementById('ynow_atx_lock_promo_tpl');
              if (!tpl) return;
              var host = tpl.cloneNode(true);
              host.removeAttribute('id');
              host.removeAttribute('hidden');
              host.classList.remove('ynow-atx-lock-tpl');
              host.classList.add('ynow-atx-lock-host');
              pane.insertBefore(host, pane.firstChild);
              ynowAdaptPostLoginLockGate(host, ynowLockGateCtx(itemId));
            }
            function ynowEnsureSidebarTabLockSurface(pane, itemId) {
              if (!pane) return;
              var kids = pane.children || [];
              for (var hi = 0; hi < kids.length; hi++) {
                if (kids[hi].classList && (
                  kids[hi].classList.contains('ynow-tab-lock-host') ||
                  kids[hi].classList.contains('ynow-atx-lock-host')
                )) {
                  var again = kids[hi].querySelector('.ynow-gate-box, [data-ynow-gate-feature]') || kids[hi];
                  ynowAdaptPostLoginLockGate(again, ynowLockGateCtx(itemId));
                  return;
                }
              }
              var tpl = document.getElementById('ynow_inner_tab_lock_promo_tpl');
              if (!tpl) return;
              var promo = tpl.cloneNode(true);
              promo.removeAttribute('id');
              promo.removeAttribute('hidden');
              promo.classList.remove('ynow-inner-tab-lock-tpl');
              promo.classList.add('ynow-hfv-chapter');
              promo.classList.add('ynow-member-gate');
              promo.classList.add('ynow-gate-box');
              promo.classList.add('ynow-member-gate--surface-overlay');
              var fade = document.createElement('div');
              fade.className = 'ynow-member-gate-fade-backdrop';
              fade.setAttribute('aria-hidden', 'true');
              var surface = document.createElement('div');
              surface.className = 'ynow-member-gate-surface ynow-member-gate-surface--checklist ynow-member-gate-surface--locked ynow-tab-lock-host';
              surface.appendChild(fade);
              surface.appendChild(promo);
              pane.insertBefore(surface, pane.firstChild);
              ynowAdaptPostLoginLockGate(promo, ynowLockGateCtx(itemId));
            }
            function ynowEnsureInnerLockPromo(pane, itemId) {
              if (!pane) return;
              var kids = pane.children || [];
              for (var hi = 0; hi < kids.length; hi++) {
                if (kids[hi].classList && kids[hi].classList.contains('ynow-inner-tab-lock-host')) {
                  ynowAdaptPostLoginLockGate(kids[hi], ynowLockGateCtx(itemId));
                  return;
                }
              }
              var tpl = document.getElementById('ynow_inner_tab_lock_promo_tpl');
              if (!tpl) return;
              var host = tpl.cloneNode(true);
              host.removeAttribute('id');
              host.removeAttribute('hidden');
              host.classList.remove('ynow-inner-tab-lock-tpl');
              host.classList.add('ynow-inner-tab-lock-host');
              pane.insertBefore(host, pane.firstChild);
              ynowAdaptPostLoginLockGate(host, ynowLockGateCtx(itemId));
            }
            /* Lock target is a block, input, or report inside a tab — not a whole sidebar page. */
            function ynowMarkLockedInnerTarget(el, itemId) {
              if (!el || !el.closest) return;
              var pane = el.closest('.tab-pane');
              if (!pane || !pane.id) return;
              var value = pane.id.indexOf('shiny-tab-') === 0 ? pane.id.slice(10) : '';
              var anchor = null;
              if (value) {
                var links = document.querySelectorAll('a[data-value]');
                for (var i = 0; i < links.length; i++) {
                  if (links[i].getAttribute('data-value') === value) { anchor = links[i]; break; }
                }
              }
              if (!anchor) anchor = document.querySelector('a[href="#' + pane.id + '"]');
              if (!anchor) return;
              var li = anchor.closest ? anchor.closest('li') : null;
              if (!li) return;
              var inSidebar = !!(anchor.closest && anchor.closest('.sidebar-menu'));
              if (inSidebar) {
                li.classList.add('ynow-access-locked-nav');
                li.classList.add('ynow-sidebar-locked');
              } else {
                li.classList.add('ynow-inner-tab-locked');
                if (!anchor.querySelector('.ynow-inner-tab-lock')) {
                  var lock = document.createElement('i');
                  lock.className = 'fas fa-lock ynow-menu-lock ynow-inner-tab-lock';
                  lock.setAttribute('aria-hidden', 'true');
                  anchor.insertBefore(lock, anchor.firstChild);
                }
              }
              pane.classList.add('ynow-inner-locked-pane');
              ynowEnsureInnerLockPromo(pane, itemId);
            }
            window.ynowApplyAccessLocks = function () {
              var entries = window.__ynowAccessEntries || [];
              var note = window.__ynowAccessLockNote || '';
              /* Omniscient self-view: lift every ACL lock/hide (entries ignored). */
              var omniscient = !!(document.body && document.body.classList.contains('ynow-omniscient'));
              if (omniscient) entries = [];
              document.querySelectorAll('i.ynow-access-lock, i.ynow-inner-tab-lock').forEach(function (el) {
                if (el.parentNode) el.parentNode.removeChild(el);
              });
              document.querySelectorAll('.ynow-inner-tab-lock-host, .ynow-atx-lock-host, .ynow-tab-lock-host').forEach(function (el) {
                if (el.parentNode) el.parentNode.removeChild(el);
              });
              var clearSel = [
                '.ynow-access-hidden',
                '.ynow-access-locked-nav',
                '.ynow-sidebar-locked',
                '.ynow-access-locked-input',
                '.ynow-access-locked-section',
                '.ynow-access-locked-btn',
                '.ynow-access-locked-pane',
                '.ynow-access-hidden-pane',
                '.ynow-access-locked',
                '.ynow-inner-tab-locked',
                '.ynow-inner-locked-pane',
                '.ynow-atx-locked-pane'
              ].join(',');
              document.querySelectorAll(clearSel).forEach(function (el) {
                el.classList.remove(
                  'ynow-access-hidden',
                  'ynow-access-locked-nav',
                  'ynow-sidebar-locked',
                  'ynow-access-locked-input',
                  'ynow-access-locked-section',
                  'ynow-access-locked-btn',
                  'ynow-access-locked-pane',
                  'ynow-access-hidden-pane',
                  'ynow-access-locked',
                  'ynow-inner-tab-locked',
                  'ynow-inner-locked-pane',
                  'ynow-atx-locked-pane'
                );
                el.removeAttribute('data-lock-note');
              });
              if (omniscient) {
                if (typeof window.ynowSyncGuestSidebarLocked === 'function') {
                  window.ynowSyncGuestSidebarLocked();
                }
                return;
              }
              entries.forEach(function (row) {
                if (!row || !row.id) return;
                var id = String(row.id);
                var effect = String(row.effect || 'hide').toLowerCase();
                if (effect !== 'lock') effect = 'hide';
                if (id.indexOf('tab:') === 0) {
                  var tab = id.slice(4);
                  document.querySelectorAll('a[data-value]').forEach(function (a) {
                    if (a.getAttribute('data-value') !== tab) return;
                    var li = a.closest('li');
                    if (!li) return;
                    if (effect === 'hide') li.classList.add('ynow-access-hidden');
                    else {
                      li.classList.add('ynow-access-locked-nav');
                      li.classList.add('ynow-sidebar-locked');
                    }
                  });
                  var pane = document.getElementById('shiny-tab-' + tab);
                  if (pane) {
                    if (effect === 'hide') {
                      pane.classList.add('ynow-access-hidden-pane');
                    } else {
                      pane.classList.add('ynow-access-locked-pane');
                      pane.setAttribute('data-lock-note', note);
                      if (tab === 'asset_transmission') {
                        pane.classList.add('ynow-atx-locked-pane');
                        ynowEnsureAtxLockPromo(pane, id);
                      } else if (typeof ynowEnsureSidebarTabLockSurface === 'function') {
                        ynowEnsureSidebarTabLockSurface(pane, id);
                      }
                    }
                  }
                } else if (id.indexOf('report:') === 0) {
                  var btn = document.getElementById(id.slice(7));
                  if (!btn) return;
                  if (effect === 'hide') btn.classList.add('ynow-access-hidden');
                  else {
                    btn.classList.add('ynow-access-locked-btn');
                    btn.setAttribute('title', note);
                    ynowMarkLockedInnerTarget(btn, id);
                  }
                } else if (id.indexOf('section:') === 0 || id.indexOf('output:') === 0) {
                  document.querySelectorAll('[data-ynow-lock]').forEach(function (el) {
                    if (el.getAttribute('data-ynow-lock') !== id) return;
                    if (effect === 'hide') el.classList.add('ynow-access-hidden');
                    else {
                      el.classList.add('ynow-access-locked-section');
                      el.setAttribute('data-lock-note', note);
                      ynowMarkLockedInnerTarget(el, id);
                    }
                  });
                } else if (id.indexOf('input:') === 0) {
                  /* Retired; stale payloads only. */
                  var inp = document.getElementById(id.slice(6));
                  var fg = inp && (inp.closest('.form-group') || inp.closest('.shiny-input-container'));
                  if (!fg) return;
                  if (effect === 'hide') fg.classList.add('ynow-access-hidden');
                  else {
                    fg.classList.add('ynow-access-locked-input');
                    fg.setAttribute('data-lock-note', note);
                    ynowMarkLockedInnerTarget(fg, id);
                  }
                }
              });
            document.querySelectorAll('.sidebar-menu li.ynow-access-locked-nav > a').forEach(function (a) {
              if (a.querySelector('.ynow-menu-lock')) {
                /* Ensure unified chrome class even when guest lock glyph already exists. */
                var li0 = a.closest ? a.closest('li') : a.parentElement;
                if (li0 && li0.classList) li0.classList.add('ynow-sidebar-locked');
                return;
              }
              var lock = document.createElement('i');
              lock.className = 'fas fa-lock ynow-menu-lock ynow-access-lock';
              lock.setAttribute('aria-hidden', 'true');
              var glyph = a.querySelector(':scope > i:not(.ynow-menu-lock):not(.pull-right)');
              if (glyph) glyph.insertAdjacentElement('afterend', lock);
              else a.insertBefore(lock, a.firstChild);
            });
              if (document.body &&
                  document.body.classList.contains('ynow-authed') &&
                  !document.body.classList.contains('ynow-lite') &&
                  !document.body.classList.contains('ynow-omniscient')) {
                var persistTabs = window.__YNOW_SIDEBAR_PERSIST_LOCK_TABS || {};
                Object.keys(persistTabs).forEach(function (ptab) {
                  if (!persistTabs[ptab]) return;
                  var ppane = document.getElementById('shiny-tab-' + ptab);
                  if (!ppane || ppane.classList.contains('ynow-access-locked-pane')) return;
                  ppane.classList.add('ynow-access-locked-pane');
                  ynowEnsureSidebarTabLockSurface(ppane, 'tab:' + ptab);
                });
              }
              if (typeof window.ynowSyncGuestSidebarLocked === 'function') {
                window.ynowSyncGuestSidebarLocked();
              }
            };
            /* Full guest locks share .ynow-sidebar-locked with ACL + persist (B) locks.
               After sign-in: unlock-after-login (A) tabs drop chrome; still-locked
               (ACL / persist B) keep guest-identical lock icon + gray — never hide. */
            window.ynowEnsureSidebarLockGlyph = function (a) {
              if (!a || a.querySelector('.ynow-menu-lock')) return;
              var lock = document.createElement('i');
              lock.className = 'fas fa-lock ynow-menu-lock';
              lock.setAttribute('aria-hidden', 'true');
              var glyph = a.querySelector(':scope > i:not(.ynow-menu-lock):not(.pull-right)');
              if (glyph) glyph.insertAdjacentElement('afterend', lock);
              else a.insertBefore(lock, a.firstChild);
            };
            window.ynowSyncGuestSidebarLocked = function () {
              var tabs = window.__YNOW_SIDEBAR_GUEST_LOCK_TABS || {
                lab_notes: true,
                decision_checklist: true,
                mos_assumptions: true,
                price_alerts: true,
                asset_transmission: true,
                global_prediction: true
              };
              /* Tier B / points-gated sidebar tabs: keep locked chrome after auth. */
              var persist = window.__YNOW_SIDEBAR_PERSIST_LOCK_TABS || {};
              var omniscient = !!(document.body && document.body.classList.contains('ynow-omniscient'));
              /* View-as / signed-in self-view / omniscient are signed-in previews — not guest-locked. */
              var guest = !!(document.body &&
                document.body.classList.contains('ynow-auth-feature') &&
                !document.body.classList.contains('ynow-lite') &&
                !document.body.classList.contains('ynow-authed') &&
                !document.body.classList.contains('ynow-view-as') &&
                !document.body.classList.contains('ynow-omniscient'));
              document.querySelectorAll('.sidebar-menu a[data-value]').forEach(function (a) {
                var tab = a.getAttribute('data-value') || '';
                var li = a.closest ? a.closest('li') : a.parentElement;
                if (!li || !li.classList) return;
                if (omniscient) {
                  /* Lift guest + persist B chrome; ACL already cleared by applyAccessLocks. */
                  li.classList.remove('ynow-sidebar-locked');
                  var glyph = a.querySelector('.ynow-menu-lock:not(.ynow-access-lock)');
                  if (glyph && glyph.parentNode && !li.classList.contains('ynow-access-locked-nav')) {
                    /* Keep ACL lock glyph only if somehow still marked; otherwise drop freemium glyph. */
                  }
                  if (tabs[tab] || persist[tab]) {
                    var g0 = a.querySelector(':scope > .ynow-menu-lock');
                    if (g0 && g0.parentNode) g0.parentNode.removeChild(g0);
                  }
                  return;
                }
                /* Still-locked after auth: ACL + persist B — same chrome as guest. */
                if (li.classList.contains('ynow-access-locked-nav') || persist[tab]) {
                  li.classList.add('ynow-sidebar-locked');
                  li.classList.remove('ynow-access-hidden');
                  if (typeof window.ynowEnsureSidebarLockGlyph === 'function') {
                    window.ynowEnsureSidebarLockGlyph(a);
                  }
                  return;
                }
                if (guest && tabs[tab]) {
                  li.classList.add('ynow-sidebar-locked');
                  if (typeof window.ynowEnsureSidebarLockGlyph === 'function') {
                    window.ynowEnsureSidebarLockGlyph(a);
                  }
                } else if (tabs[tab]) {
                  /* A unlock-after-login: drop locked chrome only (item stays visible). */
                  li.classList.remove('ynow-sidebar-locked');
                }
              });
              if (typeof window.ynowSyncSidebarPinLocks === 'function') {
                try { window.ynowSyncSidebarPinLocks(); } catch (ePinLock) {}
              }
            };
            addHandler('ynowAccessLocks', function (payload) {
              if (!payload) return;
              var entries = payload.entries;
              if (!entries || !entries.length) {
                /* Backward compat: plain ids list → hide */
                entries = (payload.ids || []).map(function (id) {
                  return { id: id, effect: 'hide' };
                });
              }
              window.__ynowAccessEntries = entries;
              window.__ynowAccessLocks = (payload.ids || entries.map(function (e) {
                return e && e.id;
              })).filter(Boolean);
              window.__ynowAccessLockNote = payload.note || '';
              window.__ynowQuotaBadges = payload.badges || [];
              if (payload.server_now) {
                var sn = Date.parse(payload.server_now);
                if (isFinite(sn)) window.__ynowQuotaServerSkewMs = sn - Date.now();
              }
              if (window.ynowApplyAccessLocks) window.ynowApplyAccessLocks();
              if (window.ynowApplyMaintenance) window.ynowApplyMaintenance();
              if (window.ynowApplyQuotaBadges) window.ynowApplyQuotaBadges();
            });
            /* System admin maintenance: separate from ACL / guest locks.
               Menu items are non-clickable; content shows 維護中 / Under maintenance. */
            window.ynowApplyMaintenance = function () {
              var entries = window.__ynowMaintEntries || [];
              var clearSel = [
                '.ynow-maintenance-nav',
                '.ynow-maintenance-pane',
                '.ynow-maintenance-item'
              ].join(',');
              document.querySelectorAll(clearSel).forEach(function (el) {
                el.classList.remove(
                  'ynow-maintenance-nav',
                  'ynow-maintenance-pane',
                  'ynow-maintenance-item'
                );
                el.removeAttribute('data-maint-note');
                el.removeAttribute('aria-disabled');
                el.removeAttribute('tabindex');
              });
              entries.forEach(function (row) {
                if (!row || !row.id) return;
                var id = String(row.id);
                var note = String(row.note || '');
                if (id.indexOf('tab:') === 0) {
                  var tab = id.slice(4);
                  document.querySelectorAll('a[data-value]').forEach(function (a) {
                    if (a.getAttribute('data-value') !== tab) return;
                    var li = a.closest ? a.closest('li') : a.parentElement;
                    if (!li || !li.classList) return;
                    li.classList.add('ynow-maintenance-nav');
                    a.setAttribute('aria-disabled', 'true');
                    a.setAttribute('tabindex', '-1');
                    if (note) li.setAttribute('data-maint-note', note);
                  });
                  var pane = document.getElementById('shiny-tab-' + tab);
                  if (pane) {
                    pane.classList.add('ynow-maintenance-pane');
                    if (note) pane.setAttribute('data-maint-note', note);
                  }
                } else if (id.indexOf('report:') === 0) {
                  var btn = document.getElementById(id.slice(7));
                  if (!btn) return;
                  btn.classList.add('ynow-maintenance-item');
                  if (note) btn.setAttribute('title', note.replace(/\n/g, ' '));
                  if (note) btn.setAttribute('data-maint-note', note);
                } else if (id.indexOf('section:') === 0 || id.indexOf('output:') === 0) {
                  document.querySelectorAll('[data-ynow-lock]').forEach(function (el) {
                    if (el.getAttribute('data-ynow-lock') !== id) return;
                    el.classList.add('ynow-maintenance-item');
                    if (note) el.setAttribute('data-maint-note', note);
                  });
                } else if (id.indexOf('input:') === 0) {
                  var inp = document.getElementById(id.slice(6));
                  var fg = inp && (inp.closest('.form-group') || inp.closest('.shiny-input-container'));
                  if (!fg) return;
                  fg.classList.add('ynow-maintenance-item');
                  if (note) fg.setAttribute('data-maint-note', note);
                }
              });
            };
            window.__ynowMaintRefreshTimer = null;
            window.ynowScheduleMaintRefresh = function (nextEndIso) {
              if (window.__ynowMaintRefreshTimer) {
                try { clearTimeout(window.__ynowMaintRefreshTimer); } catch (eClr) {}
                window.__ynowMaintRefreshTimer = null;
              }
              if (!nextEndIso) return;
              var endMs = Date.parse(nextEndIso);
              if (!isFinite(endMs)) return;
              var delay = Math.max(500, endMs - Date.now() + 400);
              /* Cap client wait; server invalidateLater also pushes. */
              if (delay > 120000) delay = 120000;
              window.__ynowMaintRefreshTimer = setTimeout(function () {
                window.__ynowMaintRefreshTimer = null;
                if (window.Shiny && Shiny.setInputValue) {
                  Shiny.setInputValue(
                    'ynow_maint_refresh',
                    (window.__ynowMaintRefreshClicks = (window.__ynowMaintRefreshClicks || 0) + 1),
                    { priority: 'event' }
                  );
                }
              }, delay);
            };
            addHandler('ynowMaintenance', function (payload) {
              if (!payload) payload = {};
              var entries = payload.entries || [];
              window.__ynowMaintEntries = entries;
              window.__ynowMaintIds = (payload.ids || entries.map(function (e) {
                return e && e.id;
              })).filter(Boolean);
              if (payload.server_now) {
                var sn2 = Date.parse(payload.server_now);
                if (isFinite(sn2)) window.__ynowQuotaServerSkewMs = sn2 - Date.now();
              }
              if (window.ynowApplyMaintenance) window.ynowApplyMaintenance();
              if (window.ynowApplyQuotaBadges) window.ynowApplyQuotaBadges();
              if (window.ynowScheduleMaintRefresh) {
                window.ynowScheduleMaintRefresh(payload.next_end || null);
              }
            });
            /* Hard block navigation clicks on maintenance menu items (capture). */
            if (!document.documentElement.getAttribute('data-ynow-maint-click-guard')) {
              document.documentElement.setAttribute('data-ynow-maint-click-guard', '1');
              document.addEventListener('click', function (ev) {
                var a = ev.target && ev.target.closest ? ev.target.closest('a') : null;
                if (!a) return;
                var li = a.closest ? a.closest('li.ynow-maintenance-nav') : null;
                if (!li) return;
                try { ev.preventDefault(); } catch (ePd) {}
                try { ev.stopPropagation(); } catch (eSp) {}
                return false;
              }, true);
            }
            document.addEventListener('shiny:inputchanged', function () {
              if (window.ynowApplyAccessLocks) window.ynowApplyAccessLocks();
              if (window.ynowApplyMaintenance) window.ynowApplyMaintenance();
            });
            document.addEventListener('shiny:value', function () {
              if (window.ynowApplyAccessLocks) window.ynowApplyAccessLocks();
              if (window.ynowApplyMaintenance) window.ynowApplyMaintenance();
            });
            (function bindViewAsChromeExit() {
              var btn = document.getElementById('ynow_viewas_chrome_exit');
              if (!btn || btn.__ynowBound) return;
              btn.__ynowBound = true;
              btn.addEventListener('click', function () {
                if (!window.Shiny || !Shiny.setInputValue) return;
                Shiny.setInputValue(
                  'ynow_pts_sysadmin_viewas_exit',
                  (window.__ynowViewAsExitClicks = (window.__ynowViewAsExitClicks || 0) + 1),
                  { priority: 'event' }
                );
              });
            })();
            /* Exit self view is a native actionButton on the control-panel bar. */
            addHandler('ynowAuthFeature', function (payload) {
              if (!payload) return;
              if (payload.on) document.body.classList.add('ynow-auth-feature');
              else document.body.classList.remove('ynow-auth-feature');
              if (typeof window.ynowSyncSidebarDownloadIcon === 'function') {
                window.ynowSyncSidebarDownloadIcon();
              }
              if (typeof window.ynowSyncGuestSidebarLocked === 'function') {
                window.ynowSyncGuestSidebarLocked();
              }
            });
            /* Forgot-password recovery return: swap modal to set-new-password panel. */
            addHandler('ynowAuthResetMode', function (payload) {
              var modal = document.getElementById('ynow_auth_modal');
              if (!modal) return;
              var on = !!(payload && payload.on);
              if (on) modal.classList.add('ynow-auth-mode-reset');
              else modal.classList.remove('ynow-auth-mode-reset');
              var copy = window.__ynowAuthCopy || {};
              var title = document.getElementById('ynow_auth_modal_title');
              if (title) {
                if (on && copy.modal_title_reset) title.textContent = copy.modal_title_reset;
                else if (!on && copy.modal_title) title.textContent = copy.modal_title;
              }
            });
            /* Immediate social OAuth: PKCE + location.assign (shared by custom
               message + client click capture so the popup/redirect feels instant). */
            window.ynowStartOAuthRedirect = function (payload) {
              if (!payload || !payload.url) return false;
              var go = function (url) {
                try { window.location.assign(String(url)); }
                catch (eGo) { window.location.href = String(url); }
              };
              var tab = (payload.restore_tab && String(payload.restore_tab)) || '';
              if (!tab && typeof window.ynowPersistPreAuthTab === 'function') {
                try { tab = String(window.ynowPersistPreAuthTab() || ''); } catch (ePre) {}
              }
              if (!tab && typeof window.ynowVisibleAuthTab === 'function') {
                try { tab = String(window.ynowVisibleAuthTab() || ''); } catch (eVis) {}
              }
              if (tab === 'testing') tab = 'home';
              var baseUrl = String(payload.url);
              var finish = function (url) { go(url); };
              try {
                if (tab) {
                  sessionStorage.setItem('ynow_pre_oauth_tab', tab);
                  try { localStorage.setItem('ynow_pre_oauth_tab_v1', tab); } catch (eLsO) {}
                }
                var verifier = (typeof window.ynowPkceVerifier === 'function')
                  ? window.ynowPkceVerifier() : '';
                if (!verifier || typeof window.ynowPkceChallenge !== 'function') {
                  finish(baseUrl);
                  return true;
                }
                sessionStorage.setItem('ynow_oauth_code_verifier', verifier);
                window.ynowPkceChallenge(verifier).then(function (challenge) {
                  var join = baseUrl.indexOf('?') >= 0 ? '&' : '?';
                  var url = baseUrl + join +
                    'code_challenge=' + encodeURIComponent(challenge) +
                    '&code_challenge_method=S256';
                  finish(url);
                }).catch(function () {
                  finish(baseUrl);
                });
              } catch (ePkce) {
                finish(baseUrl);
              }
              return true;
            };
            addHandler('ynowAuthOAuthRedirect', function (payload) {
              window.ynowStartOAuthRedirect(payload);
            });
            addHandler('ynowAuthOAuthBootstrap', function (payload) {
              window.__ynowOAuthBootstrap = payload || null;
            });
            if (!window.__ynowOAuthSocialCapture) {
              window.__ynowOAuthSocialCapture = true;
              document.addEventListener('click', function (ev) {
                var t = ev.target;
                if (!t || !t.closest) return;
                var hit = t.closest(
                  '#ynow_auth_google, #ynow_auth_facebook, #ynow_auth_linkedin, button.ynow-auth-social'
                );
                if (!hit) return;
                var boot = window.__ynowOAuthBootstrap;
                if (!boot || !boot.ready || !boot.providers) return;
                var id = hit.id || '';
                var provider = '';
                if (id === 'ynow_auth_google' || hit.classList.contains('ynow-auth-social--google')) {
                  provider = 'google';
                } else if (id === 'ynow_auth_facebook' || hit.classList.contains('ynow-auth-social--facebook')) {
                  provider = 'facebook';
                } else if (id === 'ynow_auth_linkedin' || hit.classList.contains('ynow-auth-social--linkedin')) {
                  provider = 'linkedin';
                }
                if (!provider) return;
                var meta = boot.providers[provider];
                if (!meta || !meta.url) return;
                /* Disabled in dashboard: let Shiny show the in-app note. */
                if (meta.enabled === false) return;
                try { ev.preventDefault(); } catch (ePd) {}
                try { ev.stopImmediatePropagation(); } catch (eSip) {}
                try { ev.stopPropagation(); } catch (eSp) {}
                window.ynowStartOAuthRedirect({ url: String(meta.url), restore_tab: '' });
              }, true);
            }
            /* Sys. Admin foot link: hide while signed in (CSS + inline, sign-out restores).
               Keep visible while system-admin portal is unlocked (self-view sets ynow-authed). */
            function ynowSyncSysAdminFootVisibility(loggedIn) {
              var el = document.getElementById('ynow_sidebar_test_btn');
              if (!el) return;
              var sysOn = !!(document.body && document.body.classList.contains('ynow-sysadmin-on'));
              if (loggedIn && !sysOn) {
                el.style.setProperty('display', 'none', 'important');
                el.style.setProperty('visibility', 'hidden', 'important');
                el.style.setProperty('pointer-events', 'none', 'important');
                el.setAttribute('aria-hidden', 'true');
                el.setAttribute('tabindex', '-1');
              } else {
                el.style.removeProperty('display');
                el.style.removeProperty('visibility');
                el.style.removeProperty('pointer-events');
                el.removeAttribute('aria-hidden');
                el.removeAttribute('tabindex');
              }
            }
            window.ynowSyncSysAdminFootVisibility = ynowSyncSysAdminFootVisibility;
            /* Sidebar PDF button: Full guest → lock icon; Lite / signed-in → download icon.
               Lite never locks Download Report (free surface). */
            function ynowSyncSidebarDownloadIcon() {
              var dl = document.getElementById('download_report');
              if (!dl || !document.body) return;
              var isLite = document.body.classList.contains('ynow-lite');
              var locked = !isLite &&
                document.body.classList.contains('ynow-auth-feature') &&
                !document.body.classList.contains('ynow-authed') &&
                !document.body.classList.contains('ynow-view-as');
              var icon = dl.querySelector('i');
              if (!icon) {
                icon = document.createElement('i');
                icon.setAttribute('role', 'presentation');
                if (dl.firstChild) {
                  dl.insertBefore(icon, dl.firstChild);
                  dl.insertBefore(document.createTextNode(' '), icon.nextSibling);
                } else {
                  dl.appendChild(icon);
                  dl.appendChild(document.createTextNode(' '));
                }
              }
              if (locked) {
                icon.className = 'fas fa-lock ynow-sidebar-dl-lock';
                icon.setAttribute('aria-label', 'lock icon');
              } else {
                icon.className = 'fas fa-download';
                icon.setAttribute('aria-label', 'download icon');
              }
            }
            window.ynowSyncSidebarDownloadIcon = ynowSyncSidebarDownloadIcon;
            try { ynowSyncSidebarDownloadIcon(); } catch (eDlIcon0) {}
            try {
              if (typeof window.ynowSyncGuestSidebarLocked === 'function') {
                window.ynowSyncGuestSidebarLocked();
              }
            } catch (eGuestLock0) {}
            addHandler('ynowAuthPostLogin', function (payload) {
              if (!payload) return;
              try {
                if (typeof window.ynowEndAuthBusy === 'function') window.ynowEndAuthBusy();
              } catch (eBusyEnd) {}
              if (!payload.ok) {
                try {
                  if (typeof window.ynowEndRedirectTitle === 'function') {
                    window.ynowEndRedirectTitle();
                  }
                } catch (eRedirFail) {}
              }
              var showRedirect = !!(payload.ok && !payload.quiet);
              var continueRedirect = !!(payload.ok && payload.quiet &&
                document.body && document.body.classList.contains('ynow-auth-redirecting'));
              if (showRedirect) {
                try {
                  if (typeof window.ynowBeginRedirectTitle === 'function') {
                    window.ynowBeginRedirectTitle();
                  }
                } catch (eRedirBeg) {}
              }
              /* Success: mark authed BEFORE tab restore / sidebar work so any
                 subsequent Sign-in / BS toggleModal open is vetoed. */
              if (payload.ok && document.body) {
                document.body.classList.add('ynow-authed');
                window.__ynowRealAuthed = true;
                ynowSyncSysAdminFootVisibility(true);
                if (typeof window.ynowSyncSidebarDownloadIcon === 'function') {
                  window.ynowSyncSidebarDownloadIcon();
                }
                if (typeof window.ynowSyncGuestSidebarLocked === 'function') {
                  window.ynowSyncGuestSidebarLocked();
                }
              }
              try {
                if (window.jQuery) jQuery('#ynow_auth_modal').modal('hide');
              } catch (eHide) {}
              var tab = '';
              var ticker = '';
              try {
                tab = String(
                  payload.restore_tab ||
                  sessionStorage.getItem('ynow_pre_oauth_tab') ||
                  localStorage.getItem('ynow_pre_oauth_tab_v1') ||
                  ''
                );
                ticker = String(payload.restore_ticker || '');
                sessionStorage.removeItem('ynow_oauth_code_verifier');
                sessionStorage.removeItem('ynow_pre_oauth_tab');
                localStorage.removeItem('ynow_pre_oauth_tab_v1');
              } catch (eClr) {}
              var liveTab = '';
              try {
                if (typeof window.ynowVisibleAuthTab === 'function') {
                  liveTab = String(window.ynowVisibleAuthTab() || '');
                }
              } catch (eLive) {}
              /* Quiet resume after_chrome must not yank off a tab already chosen
                 by non-quiet in-session / OAuth PostLogin on this page. */
              if (payload.ok && payload.quiet && window.__ynowAuthInSessionTab) {
                tab = '';
              }
              /* In-session login with empty restore: keep the active tab (gate /
                 modal / OAuth) instead of letting a later durable-prefs jump win. */
              if (payload.ok && !tab && !payload.quiet) {
                tab = liveTab || '';
              }
              if (payload.ok && !payload.quiet && liveTab &&
                  (tab === 'home' || tab === 'get_started') &&
                  liveTab !== tab) {
                tab = liveTab;
              }
              /* Do not actively click Home / Get Started on in-session success —
                 those defaults are often a stale capture; leaving the live pane
                 is safer than yanking Macro → Home. Resume (quiet) may still
                 restore durable Home when no in-session tab was chosen. */
              if (payload.ok && !payload.quiet &&
                  (tab === 'home' || tab === 'get_started')) {
                tab = '';
              }
              /* Sys. Admin is guest/ops-only: never restore testing after sign-in. */
              if (payload.ok && tab === 'testing') tab = 'home';
              if (payload.ok) {
                if (!payload.quiet) {
                  window.__ynowAuthInSessionTab = tab || liveTab || '1';
                }
                try { ynowLastUiWrite(tab || null, ticker || null); } catch (eUiW) {}
                if (tab) ynowApplyRestoredTab(tab);
                if (ticker) ynowApplyRestoredTicker(ticker);
              }
              /* Defer ScheduleEnd until ynowAuthRedirectReady (Signed-in toast /
                 after-chrome flush). Ending at PostLogin clears Redirecting…
                 before the toast and destination tab switch finish. */
              if (showRedirect || continueRedirect) {
                window.__ynowRedirectAwaitReady = true;
                window.__ynowRedirectPendingTab = tab || liveTab || '';
              }
            });
            addHandler('ynowAuthRedirectReady', function (payload) {
              if (!payload) payload = {};
              var awaiting = !!window.__ynowRedirectAwaitReady;
              var redirecting = !!(document.body &&
                document.body.classList.contains('ynow-auth-redirecting'));
              window.__ynowRedirectAwaitReady = false;
              if (!awaiting && !redirecting) return;
              var tab = '';
              try {
                tab = String(
                  (payload && payload.tab) ||
                  window.__ynowRedirectPendingTab ||
                  ''
                );
              } catch (eTab) { tab = ''; }
              window.__ynowRedirectPendingTab = null;
              if (!tab) {
                try {
                  if (typeof window.ynowVisibleAuthTab === 'function') {
                    tab = String(window.ynowVisibleAuthTab() || '');
                  }
                } catch (eLive) {}
              }
              try {
                if (typeof window.ynowBeginRedirectTitle === 'function') {
                  window.ynowBeginRedirectTitle();
                }
              } catch (eBeg) {}
              try {
                if (typeof window.ynowScheduleEndRedirectTitle === 'function') {
                  window.ynowScheduleEndRedirectTitle(tab || '');
                } else if (typeof window.ynowEndRedirectTitle === 'function') {
                  window.ynowEndRedirectTitle();
                }
              } catch (eEnd) {}
            });
            /* Collapse sidebar, then show login modal (Sign in / PA gate).
               Uses window.ynowRequestAuthModal (defined later). Shiny 1.14+
               requires handler.length === 1 — zero-arg handlers never register. */
            addHandler('ynowAuthOpenModal', function (_msg) {
              if (document.body && document.body.classList.contains('ynow-authed')) return;
              var mode = (_msg && _msg.mode) ? String(_msg.mode) : '';
              if (typeof window.ynowRequestAuthModal === 'function') {
                window.ynowRequestAuthModal(mode);
              } else {
                setTimeout(function () {
                  try {
                    if (document.body && document.body.classList.contains('ynow-authed')) return;
                    if (window.jQuery) jQuery('#ynow_auth_modal').modal('show');
                  } catch (eModal) {}
                }, 140);
              }
            });
            addHandler('ynowUserSettings', function (payload) {
              if (!document.body || !document.body.classList) return;
              var hide = !!(payload && payload.hide_locked_sidebar);
              var compact = !!(payload && payload.sidebar_compact);
              if (hide) document.body.classList.add('ynow-hide-locked-nav');
              else document.body.classList.remove('ynow-hide-locked-nav');
              if (compact) document.body.classList.add('ynow-sidebar-compact');
              else document.body.classList.remove('ynow-sidebar-compact');
              var gain = (payload && payload.gain_loss_scheme) ?
                String(payload.gain_loss_scheme) : 'auto';
              gain = gain.replace(/\s+/g, '_').toLowerCase();
              var allowedGain = {
                auto: 1, green_red: 1, red_green: 1, blue_red: 1, red_blue: 1
              };
              if (!allowedGain[gain]) gain = 'auto';
              document.body.setAttribute('data-ynow-gain-scheme', gain);
              if (typeof window.ynowSyncGuestSidebarLocked === 'function') {
                try { window.ynowSyncGuestSidebarLocked(); } catch (eSync) {}
              }
            });
            /* Durable browser resume: localStorage token survives tab/browser close.
               Cleared only on Sign out / revoke / expired resume. */
            var YNOW_AUTH_RESUME_KEY = 'ynow_auth_resume_v1';
            /* Last member UI surface (tab + ticker). Survives Sign out; used for
               early restore when a resume token is present. */
            var YNOW_LAST_UI_KEY = 'ynow_last_ui_v1';
            var ynowAuthResumeSent = false;
            var ynowAuthPersistWrite = function (token) {
              try {
                if (token) localStorage.setItem(YNOW_AUTH_RESUME_KEY, String(token));
                else localStorage.removeItem(YNOW_AUTH_RESUME_KEY);
              } catch (ePersist) {}
            };
            var ynowAuthPersistRead = function () {
              try { return localStorage.getItem(YNOW_AUTH_RESUME_KEY) || ''; }
              catch (eRead) { return ''; }
            };
            var ynowLastUiRead = function () {
              try {
                var raw = localStorage.getItem(YNOW_LAST_UI_KEY) || '';
                if (!raw) return { tab: '', ticker: '' };
                var o = JSON.parse(raw);
                return {
                  tab: o && o.tab ? String(o.tab) : '',
                  ticker: o && o.ticker ? String(o.ticker) : ''
                };
              } catch (eUi) { return { tab: '', ticker: '' }; }
            };
            var ynowLastUiWrite = function (tab, ticker) {
              try {
                var cur = ynowLastUiRead();
                var next = {
                  tab: (tab != null && String(tab)) ? String(tab) : (cur.tab || ''),
                  ticker: (ticker != null && String(ticker)) ? String(ticker) : (cur.ticker || '')
                };
                if (next.tab === 'testing') next.tab = 'home';
                localStorage.setItem(YNOW_LAST_UI_KEY, JSON.stringify(next));
              } catch (eW) {}
            };
            window.ynowLastUiWrite = ynowLastUiWrite;
            window.ynowLastUiRead = ynowLastUiRead;
            var ynowApplyRestoredTab = function (tab) {
              tab = String(tab || '');
              if (!tab || tab === 'testing') return;
              try {
                if (typeof window.ynowShowSidebarFootTab === 'function' &&
                    (tab === 'snapshot' || tab === 'feedback')) {
                  window.ynowShowSidebarFootTab(tab);
                } else if (typeof window.ynowGotoTab === 'function') {
                  window.ynowGotoTab(tab);
                } else if (window.Shiny && Shiny.setInputValue) {
                  Shiny.setInputValue('sidebar_tabs', tab, { priority: 'event' });
                }
              } catch (eTab) {}
            };
            var ynowApplyRestoredTicker = function (ticker) {
              var tk = String(ticker || '');
              if (!tk || !(window.Shiny && Shiny.setInputValue)) return;
              try {
                Shiny.setInputValue('ynow_auth_restore_ticker', tk, { priority: 'event' });
                var sc = document.getElementById('sc');
                if (sc) {
                  sc.value = tk;
                  Shiny.setInputValue('sc', tk, { priority: 'event' });
                  Shiny.setInputValue('ticker_typeahead', tk, { priority: 'event' });
                }
              } catch (eTk) {}
            };
            var ynowAuthResumeOffer = function () {
              if (ynowAuthResumeSent) return;
              if (!window.Shiny || !Shiny.setInputValue) return;
              /* OAuth return owns this boot. Resume's delayed durable last-tab
                 (often Home) must not race / overwrite ynow_pre_oauth_tab. */
              try {
                if (window.__ynowOAuthCallbackPushed) return;
                var href = String(window.location.href || '');
                if (/[?#&](code|access_token|error)=/.test(href)) return;
                if (sessionStorage.getItem('ynow_pre_oauth_tab') ||
                    localStorage.getItem('ynow_pre_oauth_tab_v1')) {
                  return;
                }
              } catch (eOauthSkip) {}
              var tok = ynowAuthPersistRead();
              if (!tok) return;
              ynowAuthResumeSent = true;
              /* Apply last UI early so first authed paint is closer to last screen. */
              try {
                var ui = ynowLastUiRead();
                if (ui.tab) ynowApplyRestoredTab(ui.tab);
              } catch (eEarly) {}
              try {
                if (typeof window.ynowBeginRedirectTitle === 'function') {
                  window.ynowBeginRedirectTitle();
                }
              } catch (eRedirResume) {}
              Shiny.setInputValue('ynow_auth_resume_token', String(tok), { priority: 'event' });
            };
            addHandler('ynowAuthPersist', function (payload) {
              if (!payload) return;
              if (payload.clear) {
                ynowAuthPersistWrite('');
                ynowAuthResumeSent = false;
                return;
              }
              if (payload.token) {
                ynowAuthPersistWrite(payload.token);
                ynowAuthResumeSent = true;
              }
            });
            /* Single-device: another tab/device replaced or cleared the resume key. */
            window.addEventListener('storage', function (ev) {
              try {
                if (!ev || ev.key !== 'ynow_auth_resume_v1') return;
                if (!document.body || !document.body.classList.contains('ynow-authed')) return;
                if (!window.Shiny || !Shiny.setInputValue) return;
                Shiny.setInputValue(
                  'ynow_auth_session_peer_takeover',
                  (window.__ynowPeerTakeover = (window.__ynowPeerTakeover || 0) + 1),
                  { priority: 'event' }
                );
              } catch (ePeer) {}
            });
            addHandler('ynowAuthSessionTaken', function (payload) {
              try {
                ynowAuthPersistWrite('');
                ynowAuthResumeSent = false;
                if (document.body) {
                  document.body.classList.remove('ynow-authed');
                  document.body.classList.remove('ynow-hide-locked-nav');
                  document.body.classList.remove('ynow-sidebar-compact');
                }
                if (typeof window.ynowSyncGuestSidebarLocked === 'function') {
                  window.ynowSyncGuestSidebarLocked();
                }
                if (typeof window.ynowSyncSidebarDownloadIcon === 'function') {
                  window.ynowSyncSidebarDownloadIcon();
                }
                var msg = (payload && payload.message) ? String(payload.message) : '';
                if (msg && window.Shiny && Shiny.notifications) {
                  /* Server also showNotification; client toast is best-effort. */
                }
              } catch (eTaken) {}
            });
            document.addEventListener('shiny:connected', function () {
              setTimeout(ynowAuthResumeOffer, 0);
            });
            document.addEventListener('shiny:sessioninitialized', function () {
              setTimeout(ynowAuthResumeOffer, 0);
            });
            addHandler('ynowAuthState', function (payload) {
              if (!payload) return;
              var realLoggedIn = !!(payload.logged_in);
              window.__ynowRealAuthed = realLoggedIn;
              var viewAs = !!(document.body && document.body.classList.contains('ynow-view-as'));
              var selfMode = window.__ynowSelfViewMode || '';
              /* Self-view guest forces guest chrome; signed_in / omniscient force member chrome. */
              var selfSigned = (selfMode === 'signed_in' || selfMode === 'omniscient');
              var selfGuest = (selfMode === 'guest');
              /* View-as / self signed-in keep session-like chrome even when ops admin is not email-authed. */
              var loggedIn = selfGuest ? false : (realLoggedIn || viewAs || selfSigned);
              if (loggedIn) document.body.classList.add('ynow-authed');
              else {
                document.body.classList.remove('ynow-authed');
                document.body.classList.remove('ynow-hide-locked-nav');
                document.body.classList.remove('ynow-sidebar-compact');
                document.body.setAttribute('data-ynow-gain-scheme', 'auto');
              }
              ynowSyncSysAdminFootVisibility(loggedIn);
              if (typeof window.ynowSyncSidebarDownloadIcon === 'function') {
                window.ynowSyncSidebarDownloadIcon();
              }
              if (typeof window.ynowSyncGuestSidebarLocked === 'function') {
                window.ynowSyncGuestSidebarLocked();
              }
              if (typeof window.ynowRefreshLockGates === 'function') {
                try { window.ynowRefreshLockGates(); } catch (eAuthGate) {}
              }
              var openLinks = document.querySelectorAll('.ynow-auth-open');
              var regLinks = document.querySelectorAll('.ynow-auth-register-open');
              var outLinks = document.querySelectorAll('.ynow-auth-signout');
              var i;
              for (i = 0; i < openLinks.length; i++) {
                openLinks[i].style.display = loggedIn ? 'none' : '';
              }
              for (i = 0; i < regLinks.length; i++) {
                regLinks[i].style.display = loggedIn ? 'none' : '';
              }
              for (i = 0; i < outLinks.length; i++) {
                outLinks[i].style.display = loggedIn ? '' : 'none';
              }
              if (payload.label_sign_in) {
                var openLbls = document.querySelectorAll('.ynow-auth-open-label');
                for (i = 0; i < openLbls.length; i++) openLbls[i].textContent = payload.label_sign_in;
              }
              if (payload.label_sign_out) {
                var outLbls = document.querySelectorAll('.ynow-auth-signout-label');
                for (i = 0; i < outLbls.length; i++) outLbls[i].textContent = payload.label_sign_out;
              }
              if (payload.label_register) {
                var regLbls = document.querySelectorAll('.ynow-auth-register-label');
                for (i = 0; i < regLbls.length; i++) regLbls[i].textContent = payload.label_register;
              }
              /* Leave Sys. Admin tab if still open after sign-in (link is CSS-hidden).
                 Skip while view-as — Exit user view owns return to the portal. */
              if (realLoggedIn && !viewAs && window.Shiny && Shiny.setInputValue) {
                try {
                  var active = document.querySelector(
                    '.sidebar-menu li.active > a[data-value], .sidebar-menu a.active[data-value]'
                  );
                  var footActive = document.querySelector(
                    '.ynow-sidebar-snapshot-foot a.active[data-value], ' +
                    '.ynow-sidebar-snapshot-foot a[aria-expanded="true"][data-value]'
                  );
                  var cur = (active && active.getAttribute('data-value')) ||
                    (footActive && footActive.getAttribute('data-value')) || '';
                  var hash = (window.location.hash || '').replace(/^#shiny-tab-/, '');
                  if (cur === 'testing' || hash === 'testing') {
                    Shiny.setInputValue('sidebar_tabs', 'home', { priority: 'event' });
                  }
                } catch (eLeaveTest) {}
              }
            });
            addHandler('ynowDcLocale', applyDcLocale);
            addHandler('ynowDcSopLocale', function (payload) {
              if (!payload) return;
              var set = function (id, val) {
                var el = document.getElementById(id);
                if (el && val) el.textContent = val;
              };
              set('ynow_dc_sop_intro', payload.intro);
              set('ynow_dc_box_sop', payload.box_sop);
              set('ynow_dc_sop_step1_title', payload.step1);
              set('ynow_dc_sop_step2_title', payload.step2);
              set('ynow_dc_sop_step3_title', payload.step3);
              set('ynow_dc_sop_ack_sgr_label', payload.ack_sgr);
              set('ynow_dc_sop_ack_sens_label', payload.ack_sens);
              set('ynow_dc_sop_ack_qual_label', payload.ack_qual);
            });
            addHandler('ynowCorpLogoColors', function (payload) {
              var root = document.documentElement;
              var c1 = payload && payload.c1 ? String(payload.c1) : '';
              var c2 = payload && payload.c2 ? String(payload.c2) : '';
              var mono = !!(payload && payload.mono);
              if (!c1) {
                root.style.removeProperty('--ynow-corp-c1');
                root.style.removeProperty('--ynow-corp-c2');
                document.body.classList.remove('ynow-corp-logo');
                document.body.classList.remove('ynow-corp-logo-mono');
                return;
              }
              if (!c2) c2 = c1;
              root.style.setProperty('--ynow-corp-c1', c1);
              root.style.setProperty('--ynow-corp-c2', c2);
              document.body.classList.add('ynow-corp-logo');
              if (mono || c1 === c2) {
                document.body.classList.add('ynow-corp-logo-mono');
              } else {
                document.body.classList.remove('ynow-corp-logo-mono');
              }
            });
            addHandler('ynowModelTheme', function (payload) {
              var tab = (payload && payload.tab) ? String(payload.tab) : '';
              /* Keys match Model Selector card accents (NAV/DCF/DDM/RI/P/B/Multiples/SOTP). */
              var map = {
                nav_calculator: 'nav',
                dcf_calculator: 'dcf',
                ddm_calculator: 'ddm',
                ri_calculator: 'ri',
                pb_calculator: 'pb',
                rel_mult_ev: 'multiples',
                rel_multiples_calculator: 'multiples',
                rel_mult_rev: 'multiples',
                sotp_calculator: 'sotp'
              };
              var key = map[tab] || null;
              ['nav', 'dcf', 'ddm', 'ri', 'pb', 'multiples', 'sotp'].forEach(function (k) {
                document.body.classList.toggle('ynow-theme-' + k, k === key);
              });
              document.body.classList.toggle('ynow-theme-model', !!key);
              document.body.classList.toggle('ynow-theme-bluechip', tab === 'bluechip');
              document.body.classList.toggle('ynow-theme-atx', tab === 'asset_transmission');
            });
            /* Models filter: lock methods with no primary names (Listed-style disabled, unchecked). */
            addHandler('ynowLabImMethodsLock', function (payload) {
              var avail = (payload && payload.available) ? payload.available.map(String) : [];
              var roots = [];
              var bridge = document.getElementById('lab_im_methods');
              if (bridge) roots.push(bridge);
              var ynq = document.getElementById('lab_ynq-wrap') || document.querySelector('.ynq-wrap');
              if (ynq) roots.push(ynq);
              if (!roots.length) return;
              roots.forEach(function (root) {
                var inputs = root.querySelectorAll('input[type=checkbox]');
                var changed = false;
                inputs.forEach(function (el) {
                  var val = String(el.value || '');
                  /* Only lock model keys; skip quality / ADR checkboxes in ynq. */
                  var known = (payload && payload.all) ? payload.all.map(String) : avail;
                  if (known.length && known.indexOf(val) < 0) return;
                  var ok = avail.indexOf(val) >= 0;
                  var label = el.closest('label');
                  if (label) label.classList.toggle('ynow-lab-im-method--locked', !ok);
                  if (!ok) {
                    if (el.checked) {
                      el.checked = false;
                      changed = true;
                    }
                    el.disabled = true;
                    el.setAttribute('aria-disabled', 'true');
                  } else {
                    el.disabled = false;
                    el.removeAttribute('aria-disabled');
                  }
                });
                if (changed && window.Shiny && window.jQuery) {
                  window.jQuery(root).trigger('change');
                }
              });
            });
          }
          registerLocaleHandler();
          /* Connect/sessioninit: register only. Full ~270KB map waits for first
             shiny:idle (ynow_locale_after_idle). Resync on reconnect recovers a
             dropped websocket message after the first map was already sent. */
          if (window.jQuery) {
            jQuery(document).on(
              'shiny:connected shiny:sessioninitialized',
              function () {
                registerLocaleHandler();
              }
            );
            jQuery(document).on('shiny:reconnected', function () {
              registerLocaleHandler();
              requestLocaleResync('shiny-reconnected');
            });
            jQuery(document).one('shiny:idle', function () {
              if (!(window.Shiny && Shiny.setInputValue)) return;
              if (window.__ynowLocaleAfterIdleSent) return;
              window.__ynowLocaleAfterIdleSent = true;
              Shiny.setInputValue('ynow_locale_after_idle', Date.now(), {priority: 'event'});
            });
          } else {
            document.addEventListener('shiny:connected', function () {
              registerLocaleHandler();
            });
            document.addEventListener('shiny:reconnected', function () {
              registerLocaleHandler();
              requestLocaleResync('shiny-reconnected');
            });
          }

          /* Device / browser language → initial UI locale (mapping done in R). */
          function pushDeviceUiLocale() {
            if (!(window.Shiny && Shiny.setInputValue)) {
              setTimeout(pushDeviceUiLocale, 50);
              return;
            }
            if (window.__ynowDeviceLocalePushed) return;
            window.__ynowDeviceLocalePushed = true;
            var tags = [];
            try {
              if (navigator.languages && navigator.languages.length) {
                for (var i = 0; i < navigator.languages.length; i++) {
                  var t = String(navigator.languages[i] || '').trim();
                  if (t) tags.push(t);
                }
              } else if (navigator.language) {
                var one = String(navigator.language || '').trim();
                if (one) tags.push(one);
              }
            } catch (eLang) {}
            Shiny.setInputValue('device_ui_locale', tags.join(','), {priority: 'event'});
          }
          pushDeviceUiLocale();

          /* OAuth return: Google/Facebook/Supabase redirect lands with ?code= or #access_token= */
          /* PKCE helpers for Supabase Auth authorize → token exchange */
          window.ynowPkceVerifier = function () {
            try {
              var a = new Uint8Array(32);
              (window.crypto || window.msCrypto).getRandomValues(a);
              var s = '';
              for (var i = 0; i < a.length; i++) s += String.fromCharCode(a[i]);
              return btoa(s).replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/g, '');
            } catch (eV) {
              return '';
            }
          };
          window.ynowPkceChallenge = function (verifier) {
            var data = new TextEncoder().encode(String(verifier || ''));
            return (window.crypto || window.msCrypto).subtle.digest('SHA-256', data).then(function (buf) {
              var bytes = new Uint8Array(buf);
              var s = '';
              for (var i = 0; i < bytes.length; i++) s += String.fromCharCode(bytes[i]);
              return btoa(s).replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/g, '');
            });
          };
          window.ynowVisibleAuthTab = function () {
            try {
              var pane = document.querySelector('.tab-content > .tab-pane.active');
              var paneTab = '';
              if (pane && pane.id && pane.id.indexOf('shiny-tab-') === 0) {
                paneTab = pane.id.slice('shiny-tab-'.length);
              }
              var foot = document.querySelector(
                '.ynow-sidebar-snapshot-foot a.active[data-value], ' +
                '.ynow-sidebar-snapshot-foot a[aria-expanded="true"][data-value]'
              );
              var a = document.querySelector(
                '.sidebar-menu li.active > a[data-value], .sidebar-menu a.active[data-value]'
              );
              var tab = paneTab ||
                (foot && foot.getAttribute('data-value')) ||
                (a && a.getAttribute('data-value')) || '';
              if (!tab) {
                var hash = (window.location.hash || '').replace(/^#shiny-tab-/, '');
                if (hash && hash !== 'testing') tab = hash;
              }
              if (tab === 'testing') tab = 'home';
              return tab || '';
            } catch (eVis) { return ''; }
          };
          window.ynowPersistPreAuthTab = function () {
            try {
              var tab = '';
              if (typeof window.ynowVisibleAuthTab === 'function') {
                tab = String(window.ynowVisibleAuthTab() || '');
              }
              if (tab) {
                sessionStorage.setItem('ynow_pre_oauth_tab', tab);
                /* localStorage backup: some IdP round-trips drop sessionStorage. */
                try { localStorage.setItem('ynow_pre_oauth_tab_v1', tab); } catch (eLs) {}
              }
              if (window.Shiny && Shiny.setInputValue) {
                Shiny.setInputValue('ynow_auth_restore_tab', tab || '', { priority: 'event' });
              }
              return tab;
            } catch (eP) { return ''; }
          };
          window.ynowBeginAuthBusy = function () {
            try {
              if (document.body) document.body.classList.add('ynow-auth-signing-in');
              var modal = document.getElementById('ynow_auth_modal');
              if (modal) {
                modal.classList.add('ynow-auth-signing-in');
                var busy = document.getElementById('ynow_auth_busy');
                if (busy) busy.setAttribute('aria-hidden', 'false');
              }
              var copy = window.__ynowAuthCopy || {};
              var lab = document.getElementById('ynow_auth_busy_label');
              if (lab && copy.busy) lab.textContent = String(copy.busy);
            } catch (eBusy) {}
          };
          window.ynowEndAuthBusy = function () {
            try {
              if (document.body) document.body.classList.remove('ynow-auth-signing-in');
              var modal = document.getElementById('ynow_auth_modal');
              if (modal) modal.classList.remove('ynow-auth-signing-in');
              var busy = document.getElementById('ynow_auth_busy');
              if (busy) busy.setAttribute('aria-hidden', 'true');
            } catch (eBusyEnd) {}
          };
          window.__ynowRedirectTitleMinHoldMs = 750;
          window.__ynowRedirectTitleQuietMs = 400;
          window.ynowBeginRedirectTitle = function () {
            try {
              var el = document.getElementById('ynow_app_title') ||
                document.querySelector('.main-header .logo .ynow-app-title');
              if (!el) return;
              var base = el.querySelector('.ynow-app-title-base');
              var fill = el.querySelector('.ynow-app-title-fill-inner');
              if (!window.__ynowTitleRedirectSaved) {
                var build = el.getAttribute('data-ynow-build') || '';
                var fallback = build ? ('The YNow App ' + build) : 'The YNow App';
                window.__ynowTitleRedirectSaved = {
                  base: (base && base.textContent) ? String(base.textContent) : fallback,
                  fill: (fill && fill.textContent) ? String(fill.textContent) : fallback,
                  aria: el.getAttribute('aria-label') || fallback
                };
              }
              var copy = window.__ynowAuthCopy || {};
              var s = window.__ynowUiLocaleStrings || {};
              var msg = copy.redirecting || s.auth_redirecting || 'Redirecting…';
              if (base) base.textContent = msg;
              if (fill) fill.textContent = msg;
              el.setAttribute('aria-label', msg);
              /* Force full gold fill so busy load-bar clip cannot hide Redirecting… */
              try { el.style.setProperty('--ynow-load-pct', '100%'); } catch (ePct) {}
              window.__ynowTitleRedirectStartedAt = Date.now();
              if (document.body) document.body.classList.add('ynow-auth-redirecting');
              if (window.__ynowTitleRedirectTimer) {
                clearTimeout(window.__ynowTitleRedirectTimer);
              }
              window.__ynowTitleRedirectTimer = setTimeout(function () {
                try { window.ynowEndRedirectTitle(); } catch (eAbandon) {}
              }, Math.max(12000, (Number(window.__ynowRedirectTitleMinHoldMs) || 750) + 200));
              try {
                if (typeof window.placeMobileAppTitle === 'function') {
                  window.placeMobileAppTitle();
                }
              } catch (ePlaceBeg) {}
            } catch (eBeg) {}
          };
          window.ynowEndRedirectTitle = function () {
            try {
              if (window.__ynowTitleRedirectPoll) {
                clearInterval(window.__ynowTitleRedirectPoll);
                window.__ynowTitleRedirectPoll = null;
              }
              if (window.__ynowTitleRedirectTimer) {
                clearTimeout(window.__ynowTitleRedirectTimer);
                window.__ynowTitleRedirectTimer = null;
              }
              if (window.__ynowTitleRedirectHoldTimer) {
                clearTimeout(window.__ynowTitleRedirectHoldTimer);
                window.__ynowTitleRedirectHoldTimer = null;
              }
              if (window.__ynowTitleRedirectBusyTimer) {
                clearTimeout(window.__ynowTitleRedirectBusyTimer);
                window.__ynowTitleRedirectBusyTimer = null;
              }
              if (window.__ynowTitleRedirectQuietTimer) {
                clearTimeout(window.__ynowTitleRedirectQuietTimer);
                window.__ynowTitleRedirectQuietTimer = null;
              }
              window.__ynowRedirectAwaitReady = false;
              window.__ynowRedirectPendingTab = null;
              var saved = window.__ynowTitleRedirectSaved;
              window.__ynowTitleRedirectSaved = null;
              window.__ynowTitleRedirectStartedAt = null;
              if (document.body) document.body.classList.remove('ynow-auth-redirecting');
              if (!saved) return;
              var el = document.getElementById('ynow_app_title') ||
                document.querySelector('.main-header .logo .ynow-app-title');
              if (!el) return;
              var base = el.querySelector('.ynow-app-title-base');
              var fill = el.querySelector('.ynow-app-title-fill-inner');
              var build = el.getAttribute('data-ynow-build') || '';
              var normal = saved.base || '';
              if (build && (!normal || /Redirecting|重新導向/.test(normal))) {
                normal = 'The YNow App ' + build;
              }
              if (!normal && build) normal = 'The YNow App ' + build;
              if (!normal) normal = 'The YNow App';
              if (base) base.textContent = normal;
              if (fill) fill.textContent = saved.fill || normal;
              el.setAttribute('aria-label', saved.aria || normal);
              try {
                if (typeof window.placeMobileAppTitle === 'function') {
                  window.placeMobileAppTitle();
                }
              } catch (ePlaceEnd) {}
            } catch (eEnd) {}
          };
          window.ynowScheduleEndRedirectTitle = function (tab) {
            try {
              var done = false;
              var minHold = Number(window.__ynowRedirectTitleMinHoldMs) || 750;
              var quietMs = Number(window.__ynowRedirectTitleQuietMs) || 400;
              var busyNow = function () {
                try {
                  return !!(document.documentElement &&
                    document.documentElement.classList.contains('shiny-busy'));
                } catch (eBusyNow) { return false; }
              };
              var clearQuiet = function () {
                if (window.__ynowTitleRedirectQuietTimer) {
                  clearTimeout(window.__ynowTitleRedirectQuietTimer);
                  window.__ynowTitleRedirectQuietTimer = null;
                }
              };
              var clearHold = function () {
                if (window.__ynowTitleRedirectHoldTimer) {
                  clearTimeout(window.__ynowTitleRedirectHoldTimer);
                  window.__ynowTitleRedirectHoldTimer = null;
                }
              };
              var quietArmed = false;
              var finish = function (forceEnd) {
                if (done) return;
                if (forceEnd) {
                  done = true;
                  quietArmed = false;
                  try { window.ynowEndRedirectTitle(); } catch (eForce) {}
                  return;
                }
                if (busyNow()) {
                  quietArmed = false;
                  clearQuiet();
                  clearHold();
                  if (window.__ynowTitleRedirectBusyTimer) {
                    clearTimeout(window.__ynowTitleRedirectBusyTimer);
                  }
                  window.__ynowTitleRedirectBusyTimer = setTimeout(function () {
                    finish(false);
                  }, 200);
                  return;
                }
                if (quietArmed) return;
                quietArmed = true;
                clearHold();
                if (window.__ynowTitleRedirectBusyTimer) {
                  clearTimeout(window.__ynowTitleRedirectBusyTimer);
                  window.__ynowTitleRedirectBusyTimer = null;
                }
                window.__ynowTitleRedirectQuietTimer = setTimeout(function () {
                  window.__ynowTitleRedirectQuietTimer = null;
                  quietArmed = false;
                  if (window.__ynowTitleRedirectBusyTimer) {
                    clearTimeout(window.__ynowTitleRedirectBusyTimer);
                    window.__ynowTitleRedirectBusyTimer = null;
                  }
                  if (done) return;
                  if (busyNow()) {
                    finish(false);
                    return;
                  }
                  var runEnd = function () {
                    if (busyNow()) { finish(false); return; }
                    done = true;
                    try { window.ynowEndRedirectTitle(); } catch (eFin) {}
                  };
                  var started = Number(window.__ynowTitleRedirectStartedAt) || Date.now();
                  var wait = Math.max(0, minHold - (Date.now() - started));
                  clearHold();
                  if (wait > 0) {
                    window.__ynowTitleRedirectHoldTimer = setTimeout(runEnd, wait);
                  } else {
                    runEnd();
                  }
                }, quietMs);
                window.__ynowTitleRedirectBusyTimer = setTimeout(function watchQuiet() {
                  if (done || !window.__ynowTitleRedirectQuietTimer) return;
                  if (busyNow()) {
                    finish(false);
                    return;
                  }
                  window.__ynowTitleRedirectBusyTimer = setTimeout(watchQuiet, 200);
                }, 200);
              };
              if (window.__ynowTitleRedirectTimer) {
                clearTimeout(window.__ynowTitleRedirectTimer);
                window.__ynowTitleRedirectTimer = null;
              }
              if (window.__ynowTitleRedirectPoll) {
                clearInterval(window.__ynowTitleRedirectPoll);
                window.__ynowTitleRedirectPoll = null;
              }
              if (window.__ynowTitleRedirectBusyTimer) {
                clearTimeout(window.__ynowTitleRedirectBusyTimer);
                window.__ynowTitleRedirectBusyTimer = null;
              }
              if (window.__ynowTitleRedirectQuietTimer) {
                clearTimeout(window.__ynowTitleRedirectQuietTimer);
                window.__ynowTitleRedirectQuietTimer = null;
              }
              if (window.__ynowTitleRedirectHoldTimer) {
                clearTimeout(window.__ynowTitleRedirectHoldTimer);
                window.__ynowTitleRedirectHoldTimer = null;
              }
              window.__ynowTitleRedirectTimer = setTimeout(function () {
                finish(true);
              }, Math.max(12000, minHold + 200));
              var target = String(tab || '');
              var settleThenFinish = function () {
                try {
                  if (window.requestAnimationFrame) {
                    window.requestAnimationFrame(function () {
                      setTimeout(function () { finish(false); }, 80);
                    });
                  } else {
                    setTimeout(function () { finish(false); }, 120);
                  }
                } catch (eSettle) {
                  setTimeout(function () { finish(false); }, 120);
                }
              };
              var n = 0;
              var tryMatch = function () {
                if (done) return true;
                if (busyNow()) return false;
                if (!target) {
                  settleThenFinish();
                  return true;
                }
                try {
                  var live = '';
                  if (typeof window.ynowVisibleAuthTab === 'function') {
                    live = String(window.ynowVisibleAuthTab() || '');
                  }
                  if (live === target) {
                    settleThenFinish();
                    return true;
                  }
                } catch (eMatch) {}
                return false;
              };
              if (tryMatch()) return;
              window.__ynowTitleRedirectPoll = setInterval(function () {
                if (busyNow()) return;
                n += 1;
                if (tryMatch() || n > 28) {
                  if (window.__ynowTitleRedirectPoll) {
                    clearInterval(window.__ynowTitleRedirectPoll);
                    window.__ynowTitleRedirectPoll = null;
                  }
                  if (!done && n > 28) settleThenFinish();
                }
              }, 70);
            } catch (eSched) {
              try { window.ynowEndRedirectTitle(); } catch (eSchedEnd) {}
            }
          };
          /* Every modal open (including shinyBS toggleModal) refreshes the tab
             capture so a prior Home Sign-in does not stick after navigating away. */
          if (!window.__ynowAuthShowPersist) {
            window.__ynowAuthShowPersist = true;
            var persistOnShow = function () {
              try {
                if (typeof window.ynowPersistPreAuthTab === 'function') {
                  window.ynowPersistPreAuthTab();
                }
              } catch (eShowPersist) {}
            };
            try {
              if (window.jQuery) {
                jQuery(document).on('show.bs.modal', '#ynow_auth_modal', persistOnShow);
              }
            } catch (eBindShow) {}
            document.addEventListener('show.bs.modal', function (ev) {
              var t = ev && ev.target;
              if (!t || t.id !== 'ynow_auth_modal') return;
              persistOnShow();
            }, true);
          }
          if (!window.__ynowAuthSubmitPersist) {
            window.__ynowAuthSubmitPersist = true;
            document.addEventListener('click', function (ev) {
              var t = ev.target;
              if (!t || !t.closest) return;
              var btn = t.closest(
                '#ynow_auth_submit, #ynow_auth_register, ' +
                '#ynow_auth_google, #ynow_auth_facebook, #ynow_auth_linkedin'
              );
              if (!btn) return;
              try {
                if (typeof window.ynowPersistPreAuthTab === 'function') {
                  window.ynowPersistPreAuthTab();
                }
              } catch (eSubPersist) {}
              try {
                if (typeof window.ynowBeginAuthBusy === 'function') {
                  window.ynowBeginAuthBusy();
                }
              } catch (eSubBusy) {}
              try {
                if (typeof window.ynowBeginRedirectTitle === 'function') {
                  window.ynowBeginRedirectTitle();
                }
              } catch (eSubRedir) {}
            }, true);
          }
          if (!window.__ynowAuthEnterPersist) {
            window.__ynowAuthEnterPersist = true;
            document.addEventListener('keydown', function (ev) {
              if (!ev || (ev.key !== 'Enter' && ev.keyCode !== 13)) return;
              var t = ev.target;
              if (!t || !t.closest) return;
              if (!t.closest('#ynow_auth_email, #ynow_auth_password')) return;
              try {
                if (typeof window.ynowPersistPreAuthTab === 'function') {
                  window.ynowPersistPreAuthTab();
                }
              } catch (eEntPersist) {}
              try {
                if (typeof window.ynowBeginAuthBusy === 'function') {
                  window.ynowBeginAuthBusy();
                }
              } catch (eEntBusy) {}
              try {
                if (typeof window.ynowBeginRedirectTitle === 'function') {
                  window.ynowBeginRedirectTitle();
                }
              } catch (eEntRedir) {}
            }, true);
          }
          function ynowParseQuery(qs) {
            var out = {};
            var raw = String(qs || '');
            if (raw.charAt(0) === '?' || raw.charAt(0) === '#') raw = raw.slice(1);
            raw.split('&').forEach(function (pair) {
              if (!pair) return;
              var i = pair.indexOf('=');
              var k = i < 0 ? pair : pair.slice(0, i);
              var v = i < 0 ? '' : pair.slice(i + 1);
              try {
                out[decodeURIComponent(String(k).split('+').join(' '))] =
                  decodeURIComponent(String(v).split('+').join(' '));
              } catch (eDec) {
                out[k] = v;
              }
            });
            return out;
          }
          function ynowPushOAuthCallback() {
            if (!(window.Shiny && Shiny.setInputValue)) {
              setTimeout(ynowPushOAuthCallback, 50);
              return;
            }
            if (window.__ynowOAuthCallbackPushed) return;
            var q = ynowParseQuery(window.location.search || '');
            var h = ynowParseQuery(window.location.hash || '');
            var access = h.access_token || q.access_token || '';
            var code = q.code || h.code || '';
            var state = q.state || h.state || '';
            var err = q.error || h.error || '';
            var authType = h.type || q.type || '';
            if (!access && !code && !err) return;
            window.__ynowOAuthCallbackPushed = true;
            if (access || code) {
              try {
                if (typeof window.ynowBeginRedirectTitle === 'function') {
                  window.ynowBeginRedirectTitle();
                }
              } catch (eOauthRedir) {}
            }
            var storedTab = '';
            var codeVerifier = '';
            try {
              storedTab = sessionStorage.getItem('ynow_pre_oauth_tab') || '';
              codeVerifier = sessionStorage.getItem('ynow_oauth_code_verifier') || '';
            } catch (eS) {}
            Shiny.setInputValue('ynow_auth_oauth_callback', {
              access_token: String(access || ''),
              refresh_token: String(h.refresh_token || q.refresh_token || ''),
              code: String(code || ''),
              state: String(state || ''),
              type: String(authType || ''),
              provider: String(q.provider || h.provider || ''),
              error: String(err || ''),
              code_verifier: String(codeVerifier || ''),
              restore_tab: String(storedTab || '')
            }, {priority: 'event'});
            try {
              var path = window.location.pathname || '/';
              if (window.history && history.replaceState) {
                history.replaceState(null, '', path);
              } else {
                window.location.hash = '';
              }
            } catch (eClear) {}
          }
          ynowPushOAuthCallback();
          document.addEventListener('shiny:connected', function () {
            ynowPushOAuthCallback();
          });

          /* ---- YNOW KPI cards: click / keyboard scroll to chapter anchors ---- */
          function ynowScrollFunnelAnchor(id) {
            if (!id) return false;
            var el = document.getElementById(id);
            if (!el) return false;
            try { el.scrollIntoView({ behavior: 'smooth', block: 'start' }); }
            catch (e) { try { el.scrollIntoView(true); } catch (e2) {} }
            return true;
          }
          function ynowOnFunnelKpiJump(ev) {
            var a = ev.target && ev.target.closest ? ev.target.closest('a.ynow-funnel-kpi-jump') : null;
            if (!a) return;
            var href = a.getAttribute('href') || '';
            if (href.charAt(0) !== '#') return;
            var id = href.slice(1);
            if (!ynowScrollFunnelAnchor(id)) return;
            ev.preventDefault();
            if (window.history && history.replaceState) {
              try { history.replaceState(null, '', href); } catch (e3) {}
            }
          }
          document.addEventListener('click', ynowOnFunnelKpiJump);
          document.addEventListener('keydown', function (ev) {
            if (ev.key !== ' ' && ev.key !== 'Spacebar') return;
            var a = ev.target && ev.target.closest ? ev.target.closest('a.ynow-funnel-kpi-jump') : null;
            if (!a) return;
            ev.preventDefault();
            a.click();
          });

          /* ---- Lite mode: sidebar logo click toggles body.ynow-lite + LITE badge ---- */
          (function () {
            /* Sidebar tabs Lite must leave (ynow_sidebar_lite_omit_tabs) + Sys. Admin foot. */
            var FULL_ONLY_TABS = (window.__YNOW_LITE_OMIT_TABS || []).slice();
            if (FULL_ONLY_TABS.indexOf('testing') < 0) FULL_ONLY_TABS.push('testing');
            if (!FULL_ONLY_TABS.length) {
              FULL_ONLY_TABS = [
                'get_started', 'nav_calculator', 'dcf_calculator', 'ddm_calculator',
                'ri_calculator', 'pb_calculator', 'rel_mult_ev', 'rel_multiples_calculator',
                'rel_mult_rev', 'sotp_calculator', 'hfv',
                'decision_checklist', 'mos_assumptions', 'lab_notes', 'price_alerts', 'testing', 'company_advance',
                'asset_transmission', 'global_prediction', 'account_admin', 'account_points', 'settings'
              ];
            }
            function remapLegacyTab(tab) {
              tab = String(tab || '');
              if (tab === 'business_breakdown_lab') return 'company_advance';
              if (tab === 'bluechip_lab') return 'bluechip';
              return tab;
            }
            window.ynowRemapLegacyTab = remapLegacyTab;
            function ynowTabAnchor(tab) {
              return document.querySelector('.sidebar-menu a[data-value="' + tab + '"]') ||
                     document.querySelector('a[data-value="' + tab + '"]');
            }
            function currentSidebarTab() {
              var active = document.querySelector('.sidebar-menu li.active > a[data-value]');
              if (active) return remapLegacyTab(active.getAttribute('data-value') || '');
              var pane = document.querySelector('.tab-content > .tab-pane.active');
              if (pane) {
                var id = pane.id || '';
                if (id.indexOf('shiny-tab-') === 0) {
                  return remapLegacyTab(id.slice('shiny-tab-'.length));
                }
                return remapLegacyTab(pane.getAttribute('data-value') || '');
              }
              return '';
            }
            var YNOW_STATEMENT_TABS = {
              dashboard: 1, smart_analysis: 1, company_advance: 1,
              dcf_calculator: 1, ddm_calculator: 1, pb_calculator: 1,
              nav_calculator: 1, ri_calculator: 1, sotp_calculator: 1,
              rel_mult_ev: 1, rel_multiples_calculator: 1, rel_mult_rev: 1,
              hfv: 1, decision_checklist: 1, lab_notes: 1, get_started: 1
            };
            function ynowStatementTab(tab) {
              return !!YNOW_STATEMENT_TABS[remapLegacyTab(tab)];
            }
            function ynowDropCompanyFetchChrome() {
              var panel = document.getElementById('shiny-notification-panel');
              if (panel) {
                var nodes = panel.querySelectorAll('.shiny-notification');
                for (var i = nodes.length - 1; i >= 0; i--) {
                  var t = nodes[i].textContent || '';
                  if (t.indexOf('正在取得') >= 0 || t.indexOf('Yahoo') >= 0 ||
                      t.indexOf('yfinance') >= 0 || t.indexOf('櫃買') >= 0) {
                    nodes[i].parentNode.removeChild(nodes[i]);
                  }
                }
              }
              var bars = document.querySelectorAll('.shiny-progress-container');
              for (var j = bars.length - 1; j >= 0; j--) {
                if (bars[j].parentNode) bars[j].parentNode.removeChild(bars[j]);
              }
            }
            window.ynowDropCompanyFetchChrome = ynowDropCompanyFetchChrome;
            document.addEventListener('click', function (ev) {
              var a = ev.target && ev.target.closest
                ? ev.target.closest('a[data-value]')
                : null;
              if (!a || !a.closest('.sidebar-menu, .main-sidebar')) return;
              var tab = remapLegacyTab(a.getAttribute('data-value') || '');
              if (tab && !ynowStatementTab(tab)) ynowDropCompanyFetchChrome();
            }, true);
            function gotoTab(tab) {
              tab = remapLegacyTab(tab);
              if (!tab) return;
              if (!ynowStatementTab(tab)) ynowDropCompanyFetchChrome();
              /* Signed-in members: Sys. Admin portal is guest/ops only.
                 Unlocked system-admin session (self-view sets ynow-authed) may stay. */
              if (tab === 'testing' && document.body &&
                  document.body.classList.contains('ynow-authed') &&
                  !document.body.classList.contains('ynow-sysadmin-on')) {
                tab = 'home';
              }
              if (window.Shiny && Shiny.setInputValue) {
                Shiny.setInputValue('sidebar_tabs', tab, {priority: 'event'});
              }
              var a = ynowTabAnchor(tab);
              if (a) {
                try { a.click(); } catch (e) {}
              }
              if (document.body && document.body.classList.contains('ynow-authed') &&
                  typeof window.ynowLastUiWrite === 'function') {
                try { window.ynowLastUiWrite(tab, null); } catch (eUi) {}
              }
            }
            window.ynowGotoTab = gotoTab;
            window.ynowCurrentSidebarTab = currentSidebarTab;
            function bindModelSelectorJump() {
              if (document.documentElement.getAttribute('data-ynow-ms-jump') === '1') return;
              document.documentElement.setAttribute('data-ynow-ms-jump', '1');
              document.addEventListener('click', function (ev) {
                var card = ev.target && ev.target.closest
                  ? ev.target.closest('.ynow-model-card[data-tab]')
                  : null;
                if (!card) return;
                ev.preventDefault();
                var tab = card.getAttribute('data-tab');
                if (tab) gotoTab(tab);
              });
              document.addEventListener('keydown', function (ev) {
                var card = ev.target && ev.target.closest
                  ? ev.target.closest('.ynow-model-card[data-tab]')
                  : null;
                if (!card) return;
                if (ev.key !== 'Enter' && ev.key !== ' ') return;
                ev.preventDefault();
                var tab = card.getAttribute('data-tab');
                if (tab) gotoTab(tab);
              });
            }
            bindModelSelectorJump();
            function refreshCurrentSidebarTab() {
              var tab = currentSidebarTab();
              if (!tab) tab = 'home';
              var mark = document.getElementById('ynow_header_logo_home');
              var focus = !!(mark && mark.classList && mark.classList.contains('ynow-logo-spinning'));
              window.__ynowLogoNonce = (window.__ynowLogoNonce || 0) + 1;
              var n = window.__ynowLogoNonce;
              if (focus) {
                window.__ynowLogoFocusN = n;
                if (mark && mark.classList) mark.classList.add('ynow-logo-spin-fast');
                if (typeof window.ynowFocusCurrentTabBusy === 'function') {
                  try { window.ynowFocusCurrentTabBusy(tab); } catch (eFocus) {}
                } else if (typeof window.ynowClearOtherTabBusy === 'function') {
                  try { window.ynowClearOtherTabBusy(tab); } catch (eClear) {}
                }
              }
              if (typeof window.ynowSetSidebarTabBusy === 'function') {
                try { window.ynowSetSidebarTabBusy(tab, true, 'refresh'); } catch (eBusy) {}
              } else if (mark && mark.classList) {
                mark.classList.add('ynow-logo-spinning');
                try { mark.setAttribute('aria-busy', 'true'); } catch (eA0) {}
              }
              if (focus && mark && mark.classList) mark.classList.add('ynow-logo-spin-fast');
              if (window.Shiny && Shiny.setInputValue) {
                Shiny.setInputValue('ynow_tab_refresh', {
                  tab: tab,
                  t: Date.now(),
                  focus: focus,
                  n: n
                }, { priority: 'event' });
              }
            }
            window.ynowRefreshCurrentSidebarTab = refreshCurrentSidebarTab;
            function bindHeaderLogoRefresh() {
              if (document.documentElement.getAttribute('data-ynow-header-logo-refresh') === '1') return;
              document.documentElement.setAttribute('data-ynow-header-logo-refresh', '1');
              /* Keep legacy flag so older tests still see a bind marker. */
              document.documentElement.setAttribute('data-ynow-header-logo-home', '1');
              document.addEventListener('click', function (ev) {
                var mark = ev.target && ev.target.closest
                  ? ev.target.closest('#ynow_header_logo_home, .ynow-header-logo-mark')
                  : null;
                if (!mark) return;
                // Currency controls sit under the same <li>; only the mark refreshes.
                if (ev.target && ev.target.closest && ev.target.closest('.ynow-ccy-float')) return;
                ev.preventDefault();
                refreshCurrentSidebarTab();
              });
              document.addEventListener('keydown', function (ev) {
                var t = ev.target;
                if (!t || t.id !== 'ynow_header_logo_home') return;
                if (ev.key !== 'Enter' && ev.key !== ' ') return;
                ev.preventDefault();
                refreshCurrentSidebarTab();
              });
            }
            bindHeaderLogoRefresh();
            /* Alias for older references / tests. */
            window.bindHeaderLogoHome = bindHeaderLogoRefresh;
            function remapLegacyHash() {
              var h = String(location.hash || '');
              if (h === '#shiny-tab-business_breakdown_lab') {
                gotoTab('company_advance');
                try { history.replaceState(null, '', '#shiny-tab-company_advance'); } catch (eH) {}
              }
            }
            function registerGotoTabHandler() {
              if (!window.Shiny || !Shiny.addCustomMessageHandler) {
                setTimeout(registerGotoTabHandler, 50);
                return;
              }
              Shiny.addCustomMessageHandler('ynowGotoTab', function (payload) {
                var tab = payload && payload.tab;
                if (tab) gotoTab(tab);
              });
              /* Search YNOW / TYNOW (/TYNOW): open Macro and expand the own-index KPI. */
              Shiny.addCustomMessageHandler('ynowMacroOpenIndex', function (payload) {
                var sym = payload && payload.symbol ? String(payload.symbol) : '';
                if (!sym) return;
                var tries = 0;
                function tryOpen() {
                  tries += 1;
                  var el = document.querySelector(
                    '.ynow-macro-kpi[data-macro-index="' + sym + '"], #ynow_macro_idx_' +
                      String(sym).toLowerCase().replace(/[^a-z0-9]+/g, '')
                  );
                  if (el && typeof el.click === 'function') {
                    el.click();
                    return;
                  }
                  if (window.Shiny && Shiny.setInputValue) {
                    try {
                      Shiny.setInputValue('macro-index_click', sym, {priority: 'event'});
                    } catch (eClick) {}
                  }
                  if (tries < 48) setTimeout(tryOpen, 250);
                }
                setTimeout(tryOpen, 400);
              });
            }
            registerGotoTabHandler();
            window.addEventListener('hashchange', remapLegacyHash);
            function applyLiteMode(on, opts) {
              opts = opts || {};
              var enabled = !!on;
              document.body.classList.toggle('ynow-lite', enabled);
              document.querySelectorAll('.ynow-lite-toggle').forEach(function (brand) {
                brand.setAttribute('aria-pressed', enabled ? 'true' : 'false');
              });
              try {
                window.sessionStorage.setItem('ynow_lite_mode', enabled ? '1' : '0');
              } catch (e0) {}
              if (window.Shiny && Shiny.setInputValue) {
                Shiny.setInputValue('ynow_lite_mode', enabled, {priority: 'event'});
              }
              if (enabled) {
                ensureLiteSnapshotFreeTab();
                ensureLiteCorpEventsTab();
              }
              try {
                if (typeof window.ynowSyncSidebarDownloadIcon === 'function') {
                  window.ynowSyncSidebarDownloadIcon();
                }
              } catch (eDlLite) {}
              try {
                if (typeof window.ynowApplyPdfQuotaCopy === 'function') {
                  window.ynowApplyPdfQuotaCopy(window.__ynowUiLocaleStrings || null);
                } else {
                  var pdfQuotaRem = document.getElementById('ynow_pdf_quota_remaining');
                  var pdfQuotaRules = document.getElementById('ynow_pdf_quota_rules');
                  var locBundle = window.__ynowUiLocaleStrings || null;
                  if (locBundle) {
                    if (enabled) {
                      if (pdfQuotaRem && (locBundle.pdf_quota_hint_lite || locBundle.pdf_quota_hint)) {
                        pdfQuotaRem.textContent = locBundle.pdf_quota_hint_lite || locBundle.pdf_quota_hint;
                      }
                      if (pdfQuotaRules) pdfQuotaRules.textContent = '';
                    } else if (pdfQuotaRem && locBundle.pdf_quota_hint) {
                      pdfQuotaRem.textContent = locBundle.pdf_quota_hint;
                    }
                  }
                }
                var snapQ = document.getElementById('ynow_snapshot_download_quota');
                var locB2 = window.__ynowUiLocaleStrings || null;
                if (snapQ && locB2) {
                  snapQ.textContent = enabled
                    ? (locB2.snapshot_download_quota_hint_lite || locB2.snapshot_download_quota_hint || '')
                    : (locB2.snapshot_download_quota_hint || '');
                }
              } catch (eQLite) {}
              remapLegacyHash();
              if (opts.navigate === false) return;
              var tab = currentSidebarTab();
              if (enabled) {
                if (FULL_ONLY_TABS.indexOf(tab) >= 0) gotoTab('smart_analysis');
              } else if (tab === 'smart_analysis') {
                gotoTab('dashboard');
              }
            }
            function ensureLiteSnapshotFreeTab() {
              if (!document.body.classList.contains('ynow-lite')) return;
              /* Lite keeps Snapshot free: prefer Current snapshot, then Manual adjustments. Never land on locked APP_DEFAULTS. */
              var active = document.querySelector('#snapshot_report .nav-tabs li.active a');
              var dv = active && active.getAttribute('data-value');
              if (dv === 'snap_audit' || dv === 'snap_current') return;
              var a = document.querySelector('#snapshot_report a[data-value="snap_current"]') ||
                document.querySelector('#snapshot_report a[data-value="snap_audit"]');
              if (!a) return;
              try {
                if (window.jQuery) window.jQuery(a).tab('show');
                else if (typeof a.click === 'function') a.click();
              } catch (eSnap) {}
            }
            window.ensureLiteSnapshotFreeTab = ensureLiteSnapshotFreeTab;
            /* Lite: FINANCIAL REPORT shows Board / CB only (ownership under CB dilution). */
            function ensureLiteCorpEventsTab() {
              if (!document.body.classList.contains('ynow-lite')) return;
              var active = document.querySelector('#dashboard_fin_report > .nav-tabs li.active a');
              var dv = active && active.getAttribute('data-value');
              if (dv === 'corp_events') return;
              var a = document.querySelector('#dashboard_fin_report a[data-value="corp_events"]');
              if (!a) return;
              try {
                if (window.jQuery) window.jQuery(a).tab('show');
                else if (typeof a.click === 'function') a.click();
              } catch (eCorp) {}
            }
            window.ensureLiteCorpEventsTab = ensureLiteCorpEventsTab;
            /* Legacy alias — older call sites / tests. */
            window.ensureLiteSnapshotDefaultsTab = ensureLiteSnapshotFreeTab;
            /* Foot links (Snapshot / Testing / Feedback) are outside .sidebar-menu.
               Activate the pane directly — do not a.click() the foot anchor (recursion
               with return false) and do not rely on Bootstrap after preventDefault. */
            function showSidebarFootTab(tab) {
              tab = remapLegacyTab(tab);
              if (!tab) return;
              /* Signed-in members: never open Sys. Admin (testing) via foot link.
                 Exception: portal already unlocked (ynow-sysadmin-on). */
              if (tab === 'testing' && document.body &&
                  document.body.classList.contains('ynow-authed') &&
                  !document.body.classList.contains('ynow-sysadmin-on')) {
                tab = 'home';
              }
              if (window.Shiny && Shiny.setInputValue) {
                Shiny.setInputValue('sidebar_tabs', tab, {priority: 'event'});
              }
              var pane = document.getElementById('shiny-tab-' + tab);
              if (pane) {
                try {
                  if (window.jQuery) {
                    jQuery(pane).addClass('active').siblings('.tab-pane').removeClass('active in');
                  } else {
                    var parent = pane.parentNode;
                    if (parent && parent.children) {
                      Array.prototype.forEach.call(parent.children, function (sib) {
                        if (sib !== pane && sib.classList) {
                          sib.classList.remove('active');
                          sib.classList.remove('in');
                        }
                      });
                    }
                    pane.classList.add('active');
                  }
                } catch (eShow) {}
              }
              try { history.replaceState(null, '', '#shiny-tab-' + tab); } catch (eH) {}
              if (tab === 'snapshot') {
                setTimeout(function () {
                  try {
                    if (window.ensureLiteSnapshotFreeTab) window.ensureLiteSnapshotFreeTab();
                  } catch (eSnap2) {}
                }, 0);
              }
            }
            window.ynowShowSidebarFootTab = showSidebarFootTab;
            function toggleLite() {
              applyLiteMode(!document.body.classList.contains('ynow-lite'));
            }
            function bindLiteToggle() {
              if (document.documentElement.getAttribute('data-ynow-lite-delegated') === '1') return;
              document.documentElement.setAttribute('data-ynow-lite-delegated', '1');
              document.addEventListener('click', function (ev) {
                var brand = ev.target && ev.target.closest ? ev.target.closest('.ynow-lite-toggle') : null;
                if (!brand) return;
                ev.preventDefault();
                ev.stopPropagation();
                toggleLite();
              }, true);
              document.addEventListener('keydown', function (ev) {
                var brand = ev.target && ev.target.closest ? ev.target.closest('.ynow-lite-toggle') : null;
                if (!brand) return;
                if (ev.key === 'Enter' || ev.key === ' ' || ev.keyCode === 13 || ev.keyCode === 32) {
                  ev.preventDefault();
                  toggleLite();
                }
              }, true);
            }
            function restoreLiteFromStorage() {
              var saved = null;
              try { saved = window.sessionStorage.getItem('ynow_lite_mode'); } catch (e1) {}
              applyLiteMode(saved === '1', {navigate: false});
              remapLegacyHash();
            }
            if (document.readyState === 'loading') {
              document.addEventListener('DOMContentLoaded', function () {
                bindLiteToggle();
                restoreLiteFromStorage();
              });
            } else {
              bindLiteToggle();
              restoreLiteFromStorage();
            }
            if (window.jQuery) {
              jQuery(document).on('shiny:connected shiny:sessioninitialized', function () {
                bindLiteToggle();
                restoreLiteFromStorage();
              });
            }
            window.__ynowApplyLiteMode = applyLiteMode;
          })();

          /* ---- Param audit (1A+2B): go to tab + highlight input ---- */
          function ynowClearParamHighlight() {
            document.querySelectorAll('.ynow-param-highlight').forEach(function (el) {
              el.classList.remove('ynow-param-highlight');
            });
          }
          function ynowFindInputEl(inputId) {
            if (!inputId) return null;
            var id = String(inputId);
            var el = document.getElementById(id);
            if (el) return el;
            el = document.querySelector('[id="' + id.replace(/"/g, '') + '"]');
            if (el) return el;
            var named = document.querySelectorAll('[name="' + id.replace(/"/g, '') + '"]');
            if (named && named.length) {
              return named[0].closest('.form-group, .shiny-input-container, .radio, .checkbox') || named[0];
            }
            return null;
          }
          function ynowGotoAndHighlight(tab, inputId) {
            ynowClearParamHighlight();
            tab = window.ynowRemapLegacyTab ? window.ynowRemapLegacyTab(tab) : tab;
            if (tab && window.Shiny && Shiny.setInputValue) {
              Shiny.setInputValue('sidebar_tabs', tab, {priority: 'event'});
            }
            var a = document.querySelector('.main-sidebar .sidebar-menu a[data-value="' + tab + '"]') ||
                    document.querySelector('.sidebar-menu a[data-value="' + tab + '"]') ||
                    document.querySelector('a[data-value="' + tab + '"]');
            if (a) {
              try { a.click(); } catch (e) {}
            }
            setTimeout(function () {
              var el = ynowFindInputEl(inputId);
              if (!el) return;
              var box = el.closest('.form-group, .shiny-input-container, .box-body > div, .radio, .checkbox') || el;
              box.classList.add('ynow-param-highlight');
              try { box.scrollIntoView({behavior: 'smooth', block: 'center'}); } catch (e2) {
                try { box.scrollIntoView(true); } catch (e3) {}
              }
              setTimeout(function () { box.classList.remove('ynow-param-highlight'); }, 4500);
            }, 350);
          }
          window.ynowGotoAndHighlight = ynowGotoAndHighlight;

          /* g ≥ WACC／Ke: expand sidebar, pulse Model Dashboard, jump to SGR */
          function ynowSidebarPushToggle() {
            return document.querySelector('.main-header .sidebar-toggle') ||
                   document.querySelector('a.sidebar-toggle') ||
                   document.querySelector('[data-toggle="push-menu"]');
          }
          function ynowEnsureSidebarOpen() {
            var body = document.body;
            if (!body) return;
            if (!body.classList.contains('sidebar-collapse')) return;
            var toggle = ynowSidebarPushToggle();
            if (toggle) {
              try { toggle.click(); } catch (e0) {}
            }
            // Fallback if AdminLTE click handler did not expand
            setTimeout(function () {
              if (body.classList.contains('sidebar-collapse')) {
                body.classList.remove('sidebar-collapse');
                try {
                  if (window.jQuery) {
                    jQuery('body').trigger('expanded.pushMenu');
                  }
                } catch (e1) {}
              }
            }, 80);
          }
          /* Sign-in: collapse sidebar before the login modal appears.
             Mobile AdminLTE uses body.sidebar-open (often WITH sidebar-collapse
             still set). Desktop uses sidebar-collapse alone. Do not early-return
             on sidebar-collapse when sidebar-open is present. */
          function ynowEnsureSidebarCollapsed() {
            var body = document.body;
            if (!body) return;
            var isMobile = false;
            try {
              isMobile = !!(window.matchMedia && window.matchMedia('(max-width: 767px)').matches);
            } catch (eMm) {
              isMobile = window.innerWidth <= 767;
            }
            var openMobile = body.classList.contains('sidebar-open');
            var collapsed = body.classList.contains('sidebar-collapse');
            // Already fully closed for this viewport.
            if (isMobile || openMobile) {
              if (!openMobile) return;
            } else if (collapsed) {
              return;
            }
            var toggle = ynowSidebarPushToggle();
            if (toggle) {
              try { toggle.click(); } catch (e0) {}
            }
            setTimeout(function () {
              if (isMobile || body.classList.contains('sidebar-open')) {
                body.classList.remove('sidebar-open');
                body.classList.remove('sidebar-collapse');
                try {
                  if (window.jQuery) {
                    jQuery('body').trigger('collapsed.pushMenu');
                  }
                } catch (e1) {}
              } else if (!body.classList.contains('sidebar-collapse')) {
                body.classList.add('sidebar-collapse');
                try {
                  if (window.jQuery) {
                    jQuery('body').trigger('collapsed.pushMenu');
                  }
                } catch (e2) {}
              }
            }, 80);
          }
          window.ynowEnsureSidebarCollapsed = ynowEnsureSidebarCollapsed;
          function ynowIsAuthed() {
            return !!(document.body && document.body.classList.contains('ynow-authed'));
          }
          window.ynowIsAuthed = ynowIsAuthed;
          try {
            if (typeof window.ynowSyncSidebarDownloadIcon === 'function') {
              window.ynowSyncSidebarDownloadIcon();
            }
          } catch (eDlIcon) {}
          /* Full guest click gate for proprietary downloads / capture exports.
             Ids from membership_locks catalog (window.__YNOW_GATED_DOWNLOAD_IDS);
             each has explicit A/B tier in __YNOW_GATED_DOWNLOAD_TIERS.
             Lite stays open (Download Report free). Public statement CSVs stay open. */
          document.addEventListener('click', function (ev) {
            if (!document.body) return;
            if (!document.body.classList.contains('ynow-auth-feature')) return;
            if (document.body.classList.contains('ynow-lite')) return;
            if (ynowIsAuthed()) return;
            var ids = (window.__YNOW_GATED_DOWNLOAD_IDS && window.__YNOW_GATED_DOWNLOAD_IDS.length)
              ? window.__YNOW_GATED_DOWNLOAD_IDS
              : [
                  'download_report',
                  'download_snapshot',
                  'download_param_restore',
                  'download_defaults',
                  'lab_im_download_report',
                  'bblab-export_data',
                  'bblab-export_chart',
                  'param_audit_pdf_go'
                ];
            var sel = ids.map(function (id) { return '#' + id; }).join(',');
            var node = ev.target && ev.target.closest ? ev.target.closest(sel) : null;
            if (!node) return;
            try { ev.preventDefault(); } catch (ePd) {}
            try { ev.stopPropagation(); } catch (eSp) {}
            try {
              if (typeof window.ynowRequestAuthModal === 'function') {
                window.ynowRequestAuthModal();
              } else if (typeof window.ynowShowAuthModal === 'function') {
                window.ynowShowAuthModal();
              }
            } catch (eModal) {}
          }, true);
          function ynowShowAuthModal() {
            // Already signed in: never reopen login modal (post-login sidebar race).
            if (ynowIsAuthed()) return;
            try {
              if (window.jQuery && jQuery.fn && jQuery.fn.modal) {
                jQuery('#ynow_auth_modal').modal('show');
                return;
              }
            } catch (eShow) {}
            var el = document.getElementById('ynow_auth_modal');
            if (el) {
              el.classList.add('in');
              el.style.display = 'block';
              el.setAttribute('aria-hidden', 'false');
              document.body.classList.add('modal-open');
            }
          }
          window.ynowShowAuthModal = ynowShowAuthModal;
          /* Open login modal from client (collapse sidebar first). Used by the
             Shiny custom message and by a capture-phase click on Sign in so the
             modal does not depend on a server round-trip that can be lost. */
          function ynowApplyAuthModalMode(mode) {
            var authMode = (mode === 'register') ? 'register' : 'signin';
            var modal = document.getElementById('ynow_auth_modal');
            if (modal) modal.setAttribute('data-ynow-auth-mode', authMode);
            var copy = window.__ynowAuthCopy || {};
            var title = document.getElementById('ynow_auth_modal_title');
            if (title) {
              var next = authMode === 'register'
                ? (copy.modal_title_register || copy.register_btn || '')
                : (copy.modal_title || '');
              if (next) title.textContent = next;
            }
            var pw = document.getElementById('ynow_auth_password');
            if (pw) {
              pw.setAttribute('autocomplete', authMode === 'register' ? 'new-password' : 'current-password');
            }
          }
          window.ynowApplyAuthModalMode = ynowApplyAuthModalMode;
          function ynowRequestAuthModal() {
            if (ynowIsAuthed()) return;
            var mode = (arguments.length && arguments[0]) ? arguments[0] : '';
            try { ynowApplyAuthModalMode(mode); } catch (eMode) {}
            try {
              if (typeof window.ynowPersistPreAuthTab === 'function') window.ynowPersistPreAuthTab();
            } catch (eTab) {}
            try { ynowEnsureSidebarCollapsed(); } catch (eCol) {}
            setTimeout(function () {
              try { ynowShowAuthModal(); } catch (eModal) {}
            }, 140);
          }
          window.ynowRequestAuthModal = ynowRequestAuthModal;
          // Veto Bootstrap modal-show after sign-in (shinyBS toggleModal open).
          // NOTE: this JS lives inside an R double-quoted string — never put
          // bare double-quotes in comments here or source(ynow_ui.R) fails.
          if (!window.__ynowAuthModalShowVeto) {
            window.__ynowAuthModalShowVeto = true;
            var vetoShow = function (e) {
              if (!ynowIsAuthed()) return;
              try { if (e && e.preventDefault) e.preventDefault(); } catch (ePd) {}
              try { if (e && e.stopImmediatePropagation) e.stopImmediatePropagation(); } catch (eSip) {}
              try {
                if (window.jQuery) {
                  var $m = jQuery('#ynow_auth_modal');
                  setTimeout(function () {
                    try { $m.modal('hide'); } catch (eH) {}
                  }, 0);
                }
              } catch (eHide) {}
              return false;
            };
            var forceHideIfAuthed = function () {
              if (!ynowIsAuthed()) return;
              try {
                if (window.jQuery) jQuery('#ynow_auth_modal').modal('hide');
              } catch (eH2) {}
            };
            try {
              if (window.jQuery) {
                jQuery(document).on('show.bs.modal', '#ynow_auth_modal', vetoShow);
                jQuery(document).on('shown.bs.modal', '#ynow_auth_modal', forceHideIfAuthed);
              }
            } catch (eBind) {}
            document.addEventListener('show.bs.modal', function (ev) {
              var t = ev && ev.target;
              if (!t || t.id !== 'ynow_auth_modal') return;
              vetoShow(ev);
            }, true);
          }
          if (!window.__ynowAuthOpenCapture) {
            window.__ynowAuthOpenCapture = true;
            document.addEventListener('click', function (ev) {
              var t = ev.target;
              if (!t || !t.closest) return;
              var regHit = t.closest(
                'a.ynow-auth-register-open, .ynow-auth-register-open, #ynow_auth_register_header_label, #ynow_auth_register_home_label'
              );
              if (regHit) {
                var regAuthed = ynowIsAuthed();
                var regDisp = '';
                try { regDisp = window.getComputedStyle(regHit).display || ''; } catch (eRd) {}
                if (regAuthed || regDisp === 'none' || regHit.offsetParent === null) {
                  try { ev.preventDefault(); } catch (ePdR) {}
                  try { ev.stopPropagation(); } catch (eSpR) {}
                  return;
                }
                ynowRequestAuthModal('register');
                return;
              }
              var hit = t.closest(
                'a.ynow-auth-open, .ynow-auth-open, #ynow_pa_gate_signin, #ynow_pa_gate_signin_label, #ynow_lab_gate_signin, #ynow_lab_gate_signin_label, #ynow_chk_gate_signin, #ynow_chk_gate_signin_label'
              );
              if (!hit) return;
              var authed = ynowIsAuthed();
              var disp = '';
              try { disp = window.getComputedStyle(hit).display || ''; } catch (eD) {}
              var invisible = authed || disp === 'none' || hit.offsetParent === null;
              if (invisible) {
                try { ev.preventDefault(); } catch (ePd) {}
                try { ev.stopPropagation(); } catch (eSp) {}
                return;
              }
              ynowRequestAuthModal();
            }, true);
          }
          function ynowClearSidebarGuide() {
            document.querySelectorAll('.ynow-sidebar-guide-pulse').forEach(function (el) {
              el.classList.remove('ynow-sidebar-guide-pulse');
            });
            document.querySelectorAll('.ynow-sidebar-guide-hint').forEach(function (el) {
              el.remove();
            });
          }
          function ynowPulseSidebarMenu(tab, hintText) {
            ynowClearSidebarGuide();
            tab = window.ynowRemapLegacyTab ? window.ynowRemapLegacyTab(tab) : tab;
            var a = document.querySelector('.main-sidebar .sidebar-menu a[data-value="' + tab + '"]') ||
                    document.querySelector('.sidebar-menu a[data-value="' + tab + '"]');
            if (!a) return;
            var li = a.closest('li') || a;
            li.classList.add('ynow-sidebar-guide-pulse');
            if (hintText) {
              var hint = document.createElement('small');
              hint.className = 'ynow-sidebar-guide-hint';
              hint.textContent = String(hintText);
              a.appendChild(hint);
            }
            try { li.scrollIntoView({behavior: 'smooth', block: 'nearest'}); } catch (e2) {}
            setTimeout(ynowClearSidebarGuide, 10000);
          }
          function ynowGuideModelDashboard(payload) {
            payload = payload || {};
            var tab = payload.tab || 'get_started';
            var inputId = payload.inputId || 'sgr';
            var lite = document.body && document.body.classList.contains('ynow-lite');
            // Lite hides Model Dashboard; keep toast-only (silent heal covers Lite calc).
            if (lite && tab === 'get_started') return;
            if (payload.openSidebar !== false) ynowEnsureSidebarOpen();
            setTimeout(function () {
              ynowPulseSidebarMenu(tab, payload.menuHint || '');
              ynowGotoAndHighlight(tab, inputId);
            }, 220);
          }
          window.ynowGuideModelDashboard = ynowGuideModelDashboard;
          (function registerGuideModelDashboard() {
            if (!window.Shiny || !Shiny.addCustomMessageHandler) {
              setTimeout(registerGuideModelDashboard, 50);
              return;
            }
            Shiny.addCustomMessageHandler('ynowGuideModelDashboard', ynowGuideModelDashboard);
          })();

          document.addEventListener('click', function (ev) {
            var btn = ev.target && ev.target.closest ? ev.target.closest('.ynow-param-goto') : null;
            if (!btn) return;
            ev.preventDefault();
            ynowGotoAndHighlight(btn.getAttribute('data-tab'), btn.getAttribute('data-input-id'));
          });

          /* ---- Param audit 2A: annotated page screenshots → PDF ---- */
          function ynowLoadScriptOnce(src) {
            return new Promise(function (resolve, reject) {
              if (document.querySelector('script[data-ynow-src="' + src + '"]')) {
                resolve();
                return;
              }
              var s = document.createElement('script');
              s.src = src;
              s.async = true;
              s.setAttribute('data-ynow-src', src);
              s.onload = function () { resolve(); };
              s.onerror = function () { reject(new Error('load fail: ' + src)); };
              document.head.appendChild(s);
            });
          }
          function ynowEnsurePdfLibs() {
            var h2c = 'https://cdn.jsdelivr.net/npm/html2canvas@1.4.1/dist/html2canvas.min.js';
            var jsp = 'https://cdn.jsdelivr.net/npm/jspdf@2.5.2/dist/jspdf.umd.min.js';
            return ynowLoadScriptOnce(h2c).then(function () { return ynowLoadScriptOnce(jsp); });
          }
          function ynowSleep(ms) {
            return new Promise(function (r) { setTimeout(r, ms); });
          }
          function ynowActivateTab(tab) {
            tab = window.ynowRemapLegacyTab ? window.ynowRemapLegacyTab(tab) : tab;
            if (typeof window.ynowShowSidebarFootTab === 'function' &&
                (tab === 'snapshot' || tab === 'testing' || tab === 'feedback')) {
              try { window.ynowShowSidebarFootTab(tab); } catch (eFoot) {}
              return;
            }
            if (window.Shiny && Shiny.setInputValue) {
              Shiny.setInputValue('sidebar_tabs', tab, {priority: 'event'});
            }
            var a = document.querySelector('.main-sidebar .sidebar-menu a[data-value="' + tab + '"]') ||
                    document.querySelector('.sidebar-menu a[data-value="' + tab + '"]') ||
                    document.querySelector('a[data-value="' + tab + '"]');
            if (a) {
              try { a.click(); } catch (e) {}
            }
          }
          function ynowTabPane(tab) {
            tab = window.ynowRemapLegacyTab ? window.ynowRemapLegacyTab(tab) : tab;
            return document.getElementById('shiny-tab-' + tab) ||
                   document.querySelector('.tab-pane[data-value="' + tab + '"]') ||
                   document.querySelector('.tab-content > .tab-pane.active');
          }
          function ynowActivateInnerTab(pane, inner) {
            if (!pane || !inner) return false;
            var key = String(inner).trim();
            if (!key) return false;
            var links = pane.querySelectorAll(
              '.nav-tabs a[data-toggle="tab"], .nav-tabs a[data-bs-toggle="tab"], ' +
              'a[data-toggle="tab"], a[data-bs-toggle="tab"]'
            );
            for (var i = 0; i < links.length; i++) {
              var a = links[i];
              var dv = (a.getAttribute('data-value') || '').trim();
              var href = (a.getAttribute('href') || '').replace(/^#/, '');
              var text = (a.textContent || '').replace(/\s+/g, ' ').trim();
              if (dv === key || href === key || text === key ||
                  text.indexOf(key) === 0 || key.indexOf(text) === 0) {
                try { a.click(); } catch (eClick) {}
                return true;
              }
            }
            return false;
          }
          async function ynowWaitForCapturePane(tab, maxMs) {
            var deadline = Date.now() + (maxMs || 4000);
            var lazyHost = document.getElementById('ynow_lazy_host_' + tab);
            while (Date.now() < deadline) {
              var pane = ynowTabPane(tab);
              if (pane && pane.classList.contains('active')) {
                if (!lazyHost) return pane;
                if (lazyHost.children && lazyHost.children.length > 0) return pane;
                if (lazyHost.querySelector && lazyHost.querySelector('*')) return pane;
              }
              await ynowSleep(120);
            }
            return ynowTabPane(tab);
          }
          function ynowFindInputBox(inputId) {
            var el = ynowFindInputEl(inputId);
            if (!el) return null;
            return el.closest('.form-group, .shiny-input-container, .radio, .checkbox, .box') || el;
          }
          function ynowAnnotateCanvas(canvas, pane, changes) {
            if (!canvas || !pane || !changes || !changes.length) return canvas;
            var ctx = canvas.getContext('2d');
            var paneRect = pane.getBoundingClientRect();
            var sx = canvas.width / Math.max(pane.scrollWidth || pane.clientWidth || 1, 1);
            var sy = canvas.height / Math.max(pane.scrollHeight || pane.clientHeight || 1, 1);
            changes.forEach(function (ch) {
              var box = ynowFindInputBox(ch.input_id);
              if (!box) return;
              var r = box.getBoundingClientRect();
              var x = (r.left - paneRect.left + pane.scrollLeft) * sx;
              var y = (r.top - paneRect.top + pane.scrollTop) * sy;
              var w = Math.max(r.width * sx, 8);
              var h = Math.max(r.height * sy, 8);
              ctx.save();
              ctx.strokeStyle = '#f39c12';
              ctx.lineWidth = Math.max(2, 3 * sx);
              ctx.strokeRect(x - 4 * sx, y - 4 * sy, w + 8 * sx, h + 8 * sy);
              var label = (ch.label || ch.input_id) + ': ' + (ch.baseline || '—') + ' → ' + (ch.current || '—');
              ctx.font = Math.max(11, Math.round(12 * sx)) + 'px sans-serif';
              ctx.fillStyle = 'rgba(243,156,18,0.92)';
              var tw = ctx.measureText(label).width + 10;
              var ty = Math.max(0, y - 18 * sy);
              ctx.fillRect(x - 4 * sx, ty, tw, 16 * sy);
              ctx.fillStyle = '#111';
              ctx.fillText(label, x, ty + 12 * sy);
              ctx.restore();
            });
            return canvas;
          }
          function ynowAddCanvasToPdf(pdf, canvas, title) {
            var pageW = pdf.internal.pageSize.getWidth();
            var pageH = pdf.internal.pageSize.getHeight();
            var margin = 28;
            var maxW = pageW - margin * 2;
            var maxH = pageH - margin * 2 - 24;
            var imgW = maxW;
            var imgH = canvas.height * (imgW / canvas.width);
            if (imgH > maxH) {
              imgH = maxH;
              imgW = canvas.width * (imgH / canvas.height);
            }
            pdf.setFontSize(11);
            pdf.setTextColor(40);
            if (title) pdf.text(String(title), margin, margin - 8);
            var data = canvas.toDataURL('image/jpeg', 0.92);
            pdf.addImage(data, 'JPEG', margin, margin + 4, imgW, imgH);
          }
          async function ynowRunParamAuditPdf(payload) {
            var status = document.getElementById('ynow_param_audit_pdf_status');
            var setStatus = function (t) { if (status) status.textContent = t || ''; };
            var strings = (payload && payload.strings) || {};
            try {
              setStatus(strings.busy || 'Capturing…');
              await ynowEnsurePdfLibs();
              var html2canvasFn = window.html2canvas;
              var jsPDF = (window.jspdf && window.jspdf.jsPDF) ? window.jspdf.jsPDF : window.jsPDF;
              if (!html2canvasFn || !jsPDF) throw new Error('libs missing');
              var pageSpecs = (payload && payload.pages) || [];
              var tabs = (payload && payload.tabs) || [];
              var changes = (payload && payload.changes) || [];
              var pageTitles = (payload && payload.page_titles) || {};
              if (!pageSpecs.length && tabs.length) {
                pageSpecs = tabs.map(function (t) {
                  return { id: t, tab: t, inner: '', annotate_tab: t, title: pageTitles[t] || t };
                });
              }
              if (!pageSpecs.length) {
                setStatus(strings.need_pages || 'Select pages');
                return;
              }
              var pdf = new jsPDF({ orientation: 'portrait', unit: 'pt', format: 'a4' });
              var margin = 40;
              pdf.setFontSize(16);
              pdf.text(payload.title || 'Parameter adjustments PDF', margin, 56);
              pdf.setFontSize(11);
              pdf.text((payload.ticker || '') + '  ·  ' + (payload.baseline_at || ''), margin, 78);
              pdf.setFontSize(10);
              var y = 100;
              pdf.text(strings.summary || 'Adjusted parameters:', margin, y);
              y += 16;
              if (!changes.length) {
                pdf.text(strings.no_changes || '(No manual adjustments vs baseline)', margin, y);
              } else {
                changes.forEach(function (ch) {
                  var line = (ch.section || '') + ' / ' + (ch.label || ch.input_id) +
                    ': ' + (ch.baseline || '—') + ' → ' + (ch.current || '—');
                  var lines = pdf.splitTextToSize(line, pdf.internal.pageSize.getWidth() - margin * 2);
                  if (y + lines.length * 12 > pdf.internal.pageSize.getHeight() - 40) {
                    pdf.addPage();
                    y = 48;
                  }
                  pdf.text(lines, margin, y);
                  y += lines.length * 12 + 4;
                });
              }
              var lazyTabs = { bluechip: 1, hfv: 1, lab_notes: 1, company_advance: 1,
                               macro_market: 1, asset_transmission: 1, testing: 1 };
              for (var i = 0; i < pageSpecs.length; i++) {
                var spec = pageSpecs[i] || {};
                var tab = spec.tab || spec.id || '';
                var inner = spec.inner || '';
                var annotateTab = spec.annotate_tab || tab;
                var title = spec.title || pageTitles[spec.id] || pageTitles[tab] || tab;
                setStatus((strings.busy || 'Capturing…') + ' (' + (i + 1) + '/' + pageSpecs.length + ')');
                ynowActivateTab(tab);
                var waitMs = lazyTabs[tab] ? 4500 : 900;
                var pane = await ynowWaitForCapturePane(tab, waitMs);
                if (!pane) continue;
                if (inner) {
                  ynowActivateInnerTab(pane, inner);
                  await ynowSleep(400);
                } else {
                  await ynowSleep(200);
                }
                try { pane.scrollTop = 0; } catch (e0) {}
                var canvas = await html2canvasFn(pane, {
                  scale: 1,
                  useCORS: true,
                  logging: false,
                  backgroundColor: '#ffffff',
                  windowWidth: Math.max(pane.scrollWidth, pane.clientWidth || 800),
                  height: Math.min(pane.scrollHeight || pane.clientHeight || 800, 5000)
                });
                var tabChanges = changes.filter(function (c) {
                  var ct = c.tab || '';
                  if (window.ynowRemapLegacyTab) ct = window.ynowRemapLegacyTab(ct);
                  return ct === annotateTab || ct === tab ||
                         (annotateTab === 'bluechip' && (c.tab === 'bluechip_lab' || ct === 'bluechip'));
                });
                ynowAnnotateCanvas(canvas, pane, tabChanges);
                pdf.addPage();
                ynowAddCanvasToPdf(pdf, canvas, title);
              }
              ynowActivateTab('snapshot');
              await ynowSleep(200);
              var fname = (payload.filename || 'YNow_param_audit') + '.pdf';
              pdf.save(fname);
              setStatus(strings.done || 'PDF downloaded.');
              if (window.Shiny && Shiny.setInputValue) {
                Shiny.setInputValue('param_audit_pdf_done', Date.now(), {priority: 'event'});
              }
            } catch (err) {
              console.error(err);
              setStatus(strings.err || 'PDF capture failed.');
              try { ynowActivateTab('snapshot'); } catch (e2) {}
            }
          }
          function registerParamAuditPdfHandler() {
            if (!window.Shiny || !Shiny.addCustomMessageHandler) {
              setTimeout(registerParamAuditPdfHandler, 50);
              return;
            }
            Shiny.addCustomMessageHandler('ynowParamAuditPdf', function (payload) {
              ynowRunParamAuditPdf(payload || {});
            });
          }
          registerParamAuditPdfHandler();
        })();
