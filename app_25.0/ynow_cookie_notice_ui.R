# ==========================================
# ynow_cookie_notice_ui.R -- site-wide cookie banner / panel / reopen + prefs JS
#
# C-STRUCT-02 seam 2: extracted from ynow_ui.R. Sourced by the UI facade with
# local = TRUE. CSS (.ynow-cookie-*) and applyUiLocale setBtText stay in
# ynow_ui.R. No valuation / Lite-lock / auth semantic change; banner stays
# ungated (no ynow-menu-lock).
# ==========================================

#' Cookie notice: banner, settings panel, reopen control, and client prefs script
.ynow_cookie_notice_ui <- function() {
  tagList(
    tags$div(
      id = "ynow_cookie_banner",
      class = "ynow-cookie-banner",
      role = "dialog",
      `aria-modal` = "false",
      `aria-labelledby` = "ynow_cookie_panel_title",
      hidden = "hidden",
      tags$p(
        id = "ynow_cookie_banner_body",
        class = "ynow-cookie-banner__body",
        paste0(
          "To improve your experience, please allow this site to use cookies. ",
          "Select Cookie settings to learn more and manage cookies on this site."
        )
      ),
      tags$div(
        class = "ynow-cookie-banner__actions",
        tags$button(
          id = "ynow_cookie_settings_btn",
          type = "button",
          class = "btn btn-default btn-sm",
          "Cookie settings"
        ),
        tags$button(
          id = "ynow_cookie_accept_all",
          type = "button",
          class = "btn btn-default btn-sm",
          "Accept all"
        ),
        tags$button(
          id = "ynow_cookie_reject_optional",
          type = "button",
          class = "btn btn-default btn-sm",
          "Reject optional"
        )
      ),
      tags$div(
        id = "ynow_cookie_panel",
        class = "ynow-cookie-panel",
        hidden = "hidden",
        tags$p(
          id = "ynow_cookie_panel_title",
          class = "ynow-cookie-panel__title",
          "Cookie settings"
        ),
        tags$label(
          class = "ynow-cookie-row",
          `for` = "ynow_cookie_necessary",
          tags$input(
            id = "ynow_cookie_necessary",
            type = "checkbox",
            checked = "checked",
            disabled = "disabled"
          ),
          tags$span(
            tags$span(id = "ynow_cookie_necessary_label", "Necessary cookies"),
            tags$span(id = "ynow_cookie_always_on", class = "ynow-cookie-always", "Always on"),
            tags$span(
              id = "ynow_cookie_necessary_hint",
              class = "ynow-cookie-hint",
              "Required for this site to work. Always on and cannot be turned off."
            )
          )
        ),
        tags$label(
          class = "ynow-cookie-row",
          `for` = "ynow_cookie_analytics",
          tags$input(id = "ynow_cookie_analytics", type = "checkbox"),
          tags$span(
            tags$span(id = "ynow_cookie_analytics_label", "Analytics cookies"),
            tags$span(
              id = "ynow_cookie_analytics_hint",
              class = "ynow-cookie-hint",
              "Optional. Help us understand how this site is used."
            )
          )
        ),
        tags$label(
          class = "ynow-cookie-row",
          `for` = "ynow_cookie_preferences",
          tags$input(id = "ynow_cookie_preferences", type = "checkbox"),
          tags$span(
            tags$span(id = "ynow_cookie_preferences_label", "Preference cookies"),
            tags$span(
              id = "ynow_cookie_preferences_hint",
              class = "ynow-cookie-hint",
              "Optional. Remember choices such as language on this device."
            )
          )
        ),
        tags$button(
          id = "ynow_cookie_save",
          type = "button",
          class = "btn btn-default btn-sm",
          "Save settings"
        )
      )
    ),
    tags$button(
      id = "ynow_cookie_reopen",
      type = "button",
      class = "btn btn-default btn-sm ynow-cookie-reopen",
      hidden = "hidden",
      title = "Cookie settings",
      icon("thumbtack"),
      tags$span(id = "ynow_cookie_reopen_label", "Cookie settings")
    ),
    tags$script(HTML('
      (function () {
        var KEY = "ynow_cookie_prefs";
        function readPrefs() {
          try {
            var raw = localStorage.getItem(KEY);
            if (raw) return JSON.parse(raw);
          } catch (e) {}
          var m = document.cookie.match(/(?:^|; )ynow_cookie_prefs=([^;]*)/);
          if (!m) return null;
          try { return JSON.parse(decodeURIComponent(m[1])); } catch (e2) { return null; }
        }
        function writePrefs(obj) {
          obj.necessary = true;
          obj.v = 1;
          var raw = JSON.stringify(obj);
          try { localStorage.setItem(KEY, raw); } catch (e) {}
          var secure = (location.protocol === "https:") ? "; Secure" : "";
          document.cookie = KEY + "=" + encodeURIComponent(raw) +
            "; path=/; max-age=31536000; SameSite=Lax" + secure;
        }
        function banner() { return document.getElementById("ynow_cookie_banner"); }
        function panel() { return document.getElementById("ynow_cookie_panel"); }
        function reopen() { return document.getElementById("ynow_cookie_reopen"); }
        function showBanner(openPanel) {
          var b = banner();
          if (!b) return;
          b.removeAttribute("hidden");
          var r = reopen();
          if (r) r.setAttribute("hidden", "hidden");
          var p = panel();
          if (!p) return;
          if (openPanel) p.removeAttribute("hidden");
          else p.setAttribute("hidden", "hidden");
        }
        function hideBanner() {
          var b = banner();
          if (b) b.setAttribute("hidden", "hidden");
          var p = panel();
          if (p) p.setAttribute("hidden", "hidden");
          var r = reopen();
          if (r) r.removeAttribute("hidden");
        }
        function applyToggles(prefs) {
          var a = document.getElementById("ynow_cookie_analytics");
          var p = document.getElementById("ynow_cookie_preferences");
          var n = document.getElementById("ynow_cookie_necessary");
          if (n) { n.checked = true; n.disabled = true; }
          if (a) a.checked = !!(prefs && prefs.analytics);
          if (p) p.checked = !!(prefs && prefs.preferences);
        }
        function choice(kind) {
          var a = document.getElementById("ynow_cookie_analytics");
          var p = document.getElementById("ynow_cookie_preferences");
          var analytics = kind === "all" ? true : (kind === "reject" ? false : !!(a && a.checked));
          var preferences = kind === "all" ? true : (kind === "reject" ? false : !!(p && p.checked));
          writePrefs({
            necessary: true,
            analytics: analytics,
            preferences: preferences,
            choice: kind
          });
          applyToggles(readPrefs());
          hideBanner();
        }
        function bind() {
          var prefs = readPrefs();
          applyToggles(prefs);
          if (prefs && prefs.choice) hideBanner();
          else showBanner(false);
          var settings = document.getElementById("ynow_cookie_settings_btn");
          if (settings) settings.addEventListener("click", function () {
            var p = panel();
            if (!p) return;
            if (p.hasAttribute("hidden")) p.removeAttribute("hidden");
            else p.setAttribute("hidden", "hidden");
          });
          var accept = document.getElementById("ynow_cookie_accept_all");
          if (accept) accept.addEventListener("click", function () { choice("all"); });
          var reject = document.getElementById("ynow_cookie_reject_optional");
          if (reject) reject.addEventListener("click", function () { choice("reject"); });
          var save = document.getElementById("ynow_cookie_save");
          if (save) save.addEventListener("click", function () { choice("custom"); });
          var again = reopen();
          if (again) again.addEventListener("click", function () { showBanner(true); });
        }
        if (document.readyState === "loading") {
          document.addEventListener("DOMContentLoaded", bind);
        } else {
          bind();
        }
      })();
    '))
  )
}
