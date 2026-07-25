# ==========================================
# ui.R - 前端介面設計
# ==========================================

# Backtest Zone：欄位下方小字說明
.bt_hint <- function(text) {
  tags$p(
    style = "margin: -6px 0 14px 0; font-size: 11.5px; line-height: 1.45; color: #777;",
    text
  )
}

.bt_section_intro <- function(text) {
  tags$p(style = "margin: 0 0 12px 0; font-size: 12.5px; color: #555; line-height: 1.5;", text)
}

#' Shared CAPM / Beta settings block (canonical IDs on DCF → WACC).
#' @param calc_id actionButton id
#' @param result_id htmlOutput id for CAPM result text
#' @param advanced_hint helpText pointing to Get Started Rolling / Unlevered
capm_beta_settings_ui <- function(title = "CAPM 估算 rₑ",
                                  calc_id = "calc_capm",
                                  result_id = "capm_result",
                                  advanced_hint = TRUE) {
  hints <- list(
    "Beta (β) 與 Get Started → BETA 雙向連動：選定來源的 β 會直接寫入此處；此處手動改 β 會回寫為「手動輸入」。",
    "勾選「套用產業平均值」等同於 Get Started 選「產業平均」；取消勾選會改回其他來源。"
  )
  if (isTRUE(advanced_hint)) {
    hints <- c(
      hints,
      "來源可選去槓桿 βᵤ（本公司／Bottom-Up）或未去槓桿估計（Summary／Rolling／產業平均）。"
    )
  }
  box(
    h4(title),
    numericInput("capm_rf", "無風險利率 Rf (%)", value = APP_DEFAULTS$capm_rf, step = 0.01),
    numericInput("capm_beta", "Beta (β)", value = APP_DEFAULTS$capm_beta, step = 0.01),
    checkboxInput(
      "use_industry_beta",
      tags$span(style = "font-weight: bold;", "套用產業平均值（Beta）"),
      value = isTRUE(APP_DEFAULTS$use_industry_beta)
    ),
    do.call(helpText, as.list(hints)),
    numericInput("capm_rm", "市場報酬率 Rm (%)", value = APP_DEFAULTS$capm_rm, step = 0.01),
    actionButton(calc_id, "估算 rₑ（CAPM）", class = "btn-primary"),
    tags$br(), htmlOutput(result_id)
  )
}

#' Pointer box when advanced Beta controls live on Get Started (CAPM is on WACC).
.beta_moved_to_get_started_box <- function(extra = NULL) {
  box(
    title = tagList(icon("info-circle"), "Beta 進階預估"),
    width = 12, status = "info", solidHeader = TRUE,
    helpText(
      "β 預估在",
      tags$b("Get Started"),
      "→「永續成長率 SGR 設定」下方的",
      tags$b("BETA"),
      "小分頁（可選去槓桿 βᵤ 或未去槓桿估計值，直接寫入 CAPM β）。",
      "CAPM（Rf／β／Rm）在",
      tags$b("DCF-Model → WACC"),
      "；勾選「採用估算 rₑ／Ke」時由該處 CAPM 驅動。"
    ),
    extra
  )
}

#' 純粹基本面 / Unlevered Beta（Get Started，置於 Rolling 上方）
#' 流程：①選 β_L → 去槓桿 ②（可選）Bottom-Up ③選要套用的 β（去槓桿或未去槓桿）→ 直接寫入 CAPM
beta_unlever_section_ui <- function() {
  tagList(
    fluidRow(
      valueBoxOutput("vbx_beta_unlever_firm", width = 4),
      valueBoxOutput("vbx_beta_unlever_bottomup", width = 4),
      valueBoxOutput("vbx_beta_relevered", width = 4)
    ),
    fluidRow(
      box(
        width = 5, status = "warning", solidHeader = FALSE,
        title = "① 本公司去槓桿",
        radioButtons(
          "beta_bl_source", "槓桿 Beta（β_L）來源",
          choices = c(
            "Finance Summary（Yahoo 5Y Monthly）" = "summary",
            "Rolling 估計（需先於 Rolling 分頁估計）" = "rolling",
            "自動（Summary → Rolling）" = "auto"
          ),
          selected = APP_DEFAULTS$beta_bl_source,
          inline = FALSE
        ),
        helpText(
          "常見做法：以 Yahoo Finance Summary β 為 β_L，再以本公司 D/E、稅率 T 去槓桿。",
          "T 取自 WACC「所得稅率 T (%)」；D/E = Total Debt ÷ 股權市值。"
        ),
        htmlOutput("beta_unlever_firm_result")
      ),
      box(
        width = 7, status = "warning", solidHeader = FALSE,
        title = "② Bottom-Up 同業平均（可選）",
        selectizeInput(
          "beta_peers",
          "同業／競爭對手代碼（可多選或自行輸入）",
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          options = list(
            create = TRUE,
            placeholder = "例如 INTC, AMD, AVGO …",
            plugins = list("remove_button"),
            maxItems = 15
          )
        ),
        helpText(
          "各同業以 Yahoo β_L 與該股 D/E 去槓桿後平均；稅率 T 共用 WACC。",
          "未填同業時，改以本頁產業基準 β 與產業負債比作參考值。"
        ),
        actionButton(
          "calc_beta_bottomup", "計算 Bottom-Up βᵤ",
          class = "btn-primary", icon = icon("calculator")
        ),
        tags$br(), tags$br(),
        htmlOutput("beta_bottomup_result"),
        tags$br(),
        tableOutput("beta_bottomup_peers_table")
      )
    ),
    fluidRow(
      box(
        width = 12, status = "warning", solidHeader = FALSE,
        title = "③ 採用哪一個 β → 直接寫入 CAPM",
        radioButtons(
          "beta_u_apply_source",
          "套用至 CAPM（選定值直接寫入，不經再槓桿）",
          choices = c(
            "【去槓桿 βᵤ】本公司 Unlevered" = "unlever_firm",
            "【去槓桿 βᵤ】Bottom-Up 平均" = "bottomup",
            "【未去槓桿 β_L】Finance Summary（Yahoo 5Y Monthly）" = "summary",
            "【未去槓桿 β_L】Rolling 估計（需先估計）" = "rolling",
            "【未去槓桿】產業平均 β" = "industry",
            "手動輸入 β（直接寫入 CAPM）" = "manual"
          ),
          selected = APP_DEFAULTS$beta_u_apply_source,
          inline = FALSE
        ),
        numericInput(
          "beta_u_manual",
          "手動 β（直接寫入 CAPM）",
          value = APP_DEFAULTS$beta_u_manual,
          min = 0, max = 5, step = 0.01
        ),
        helpText(
          "選定的 β 會直接寫入 DCF → WACC 的 CAPM β 並用於 rₑ（無論去槓桿或未去槓桿，皆不再槓桿）。",
          "變更來源或數值會自動同步；選「產業平均」會勾選 CAPM 的產業平均；選其他來源會取消勾選。",
          "在 CAPM 手動改 β 會回寫為「手動輸入」。Rolling 亦可在 Rolling 分頁估計後由此選用。"
        ),
        actionButton(
          "apply_beta_u_selected", "立即同步所選 β 至 CAPM",
          class = "btn-success", icon = icon("check")
        )
      )
    )
  )
}

#' Rolling β 預估（Get Started，置於 Unlevered 下方）
#' 常見設定：對 SPY／QQQ／IWM 做月末報酬迴歸；預設 5 年對齊 Yahoo。
beta_rolling_section_ui <- function() {
  tagList(
    fluidRow(
      valueBoxOutput("vbx_beta_summary", width = 4),
      valueBoxOutput("vbx_beta_industry", width = 4),
      valueBoxOutput("vbx_beta_estimated", width = 4)
    ),
    fluidRow(
      box(
        width = 5, status = "primary", solidHeader = TRUE,
        title = tagList(icon("sliders-h"), "預估設定"),
        selectizeInput(
          "beta_bench", "基準指數（Benchmark）",
          choices = c(
            "SPY (S&P 500 ETF)" = "SPY",
            "QQQ (Nasdaq-100 ETF)" = "QQQ",
            "IWM (Russell 2000 ETF)" = "IWM"
          ),
          selected = APP_DEFAULTS$beta_bench,
          options = list(
            create = TRUE,
            placeholder = "選常見指數，或自行輸入代碼…",
            maxItems = 1
          )
        ),
        selectInput(
          "beta_lookback_months", "回溯期間（月報酬）",
          choices = c(
            "3 年（36 個月）" = 36,
            "5 年（60 個月，對齊 Yahoo）" = 60,
            "7 年（84 個月）" = 84
          ),
          selected = as.character(APP_DEFAULTS$beta_lookback_months)
        ),
        # 技術門檻：固定預設，不另開冷門參數
        tags$div(
          style = "display:none;",
          numericInput(
            "beta_min_obs", "最少觀測月數",
            value = APP_DEFAULTS$beta_min_obs,
            min = 12, max = 60, step = 1
          )
        ),
        helpText(
          "β = Cov(Rᵢ, Rₘ) / Var(Rₘ)，優先月末報酬（對齊 Yahoo 5Y Monthly）；",
          "樣本不足時改用週報酬。此為未去槓桿（槓桿）β；亦可在 Unlevered 分頁③選「Rolling 估計」。"
        ),
        actionButton("calc_beta_est", "估計 Rolling β", class = "btn-primary", icon = icon("calculator")),
        tags$span(style = "display:inline-block; width: 8px;"),
        actionButton("apply_beta_est", "套用至 CAPM β（選 Rolling）", class = "btn-success", icon = icon("check")),
        tags$br(), tags$br(),
        htmlOutput("beta_est_result")
      ),
      box(
        width = 7, status = "info", solidHeader = TRUE,
        title = tagList(icon("exchange-alt"), "三來源比較"),
        tableOutput("beta_sources_table"),
        plotOutput("plt_beta_scatter", height = "320px")
      )
    )
  )
}

#' @deprecated use beta_unlever_section_ui + beta_rolling_section_ui
beta_advanced_tab_ui <- function() {
  tagList(
    beta_unlever_section_ui(),
    tags$hr(),
    beta_rolling_section_ui()
  )
}

.dcf_core_params_box <- function() {
  box(
    title = tagList(icon("seedling"), "永續成長率 SGR 設定"),
    width = 12, status = "warning", solidHeader = TRUE,
    tags$h5(tags$b("SGR 評價方法")),
    selectInput(
      "perpetual_g_method",
      NULL,
      choices = c(
        "總體經濟錨定（Macro）" = "macro",
        "基本面公式（Fundamental／SGR）" = "fundamental",
        "產業生命週期（Lifecycle）" = "lifecycle"
      ),
      selected = APP_DEFAULTS$perpetual_g_method
    ),
    helpText(
      "Macro：直接套用美國 10 年期公債 Rf。",
      "Fundamental：Retention×ROE（僅適合成熟穩健企業）。",
      "Lifecycle：依產業成熟度反推 g，可手動覆寫自動分類。"
    ),
    conditionalPanel(
      condition = "input.perpetual_g_method == 'lifecycle'",
      tags$h5(tags$b("生命週期檔位")),
      selectInput(
        "lifecycle_stage",
        NULL,
        choices = c(
          "自動偵測" = "auto",
          "夕陽／高度成熟（≈1.5–2%）" = "mature_sunset",
          "成熟科技巨頭（≈2.5–3%）" = "mature_tech",
          "高速成長→成熟（終值≈2.5%，建議 two-stage）" = "growth_to_mature",
          "一般成熟（≈2.5%）" = "mature_general"
        ),
        selected = APP_DEFAULTS$lifecycle_stage
      ),
      helpText("可覆寫自動偵測結果；影響終值 g 建議區間。")
    ),
    tags$h5(tags$b("估計依據")),
    uiOutput("txt_perpetual_g_reason"),
    tags$hr(style = "margin: 12px 0;"),
    tags$h5(tags$b("終值永續成長率（SGR）")),
    numericInput(
      "sgr",
      "SGR (%)",
      value = APP_DEFAULTS$sgr
    ),
    helpText("供 DCF／RI 終值使用（相對 WACC）；與 DDM 股利成長率分開。可由上方方法自動估計，亦可手動覆寫。")
  )
}

.core_params_location_box <- function() {
  box(
    title = tagList(icon("location-arrow"), "核心參數位置"),
    width = 12, status = "warning", solidHeader = TRUE,
    tags$ul(
      style = "margin:0; padding-left:18px; line-height:1.55;",
      tags$li("DCF／RI 終值 SGR：本頁上方「永續成長率 SGR 設定」"),
      tags$li("β 預估：本頁 BETA 小分頁（與 DCF → WACC 的 CAPM β 雙向連動）"),
      tags$li("CAPM（Rf／β／Rm）：DCF-Model → WACC"),
      tags$li("兩階段成長假設：DCF-Model → Overview（選 Two-Stage 時顯示）"),
      tags$li("CapEx／ΔNWC 前瞻佔營收比：DCF → FCFF 分頁（驅動預測表）"),
      tags$li("DDM 股利成長率：可在 DDM 分頁單獨覆寫")
    )
  )
}

.dcf_two_stage_params_box <- function() {
  conditionalPanel(
    condition = "input.dcf_mode == 'two_stage'",
    box(
      title = tagList(icon("layer-group"), "兩階段成長假設（DCF）"),
      width = 12, status = "warning", solidHeader = TRUE,
      tags$p(style = "margin: 0 0 6px 0; font-size: 12.5px; color: #555;", tags$b("第一階段｜高速成長")),
      numericInput("yr_stage1", "年數", value = APP_DEFAULTS$yr_stage1),
      numericInput("g_stage1", "成長率 g1 (%)", value = APP_DEFAULTS$g_stage1),
      numericInput("wacc_stage1", "折現率 WACC1 (%)", value = APP_DEFAULTS$wacc_stage1, step = 0.01),
      tags$p(style = "margin: 10px 0 6px 0; font-size: 12.5px; color: #555;", tags$b("第二階段｜永續成長")),
      helpText("第二階段成長率採用 Get Started 的 SGR；以下設定折現率。"),
      numericInput("wacc_stage2", "折現率 WACC2 (%)", value = APP_DEFAULTS$wacc_stage2, step = 0.01)
    )
  )
}

#' Shared bottom block: per-share param contribution / ±10% sensitivity (DCF-style).
.model_param_sensitivity_box <- function(title, table_id) {
  fluidRow(
    box(
      title = tagList(icon("percentage"), title),
      width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
      helpText(
        "一次只變動一個可設定參數（相對 ±10%），比較對每股估值的影響；",
        "「影響力%」= 兩側 |Δ估值%| 的平均，數值愈大代表該參數對最終估值愈敏感。"
      ),
      tags$div(
        style = "overflow-x:auto;",
        tableOutput(table_id)
      )
    )
  )
}

ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = "The YNow App v13",
    titleWidth = 250
  ),
  
  dashboardSidebar(
    width = 250,
    collapsed = FALSE,
    column(width = 12,
           sidebarSearchForm(textId = "txt_search", buttonId = "btn_search", label = "Search..."),
           column(width = 12, textOutput("today"),
                  hr()
           )
    ),
    
    # Must stay inside column(width=12): bare sidebarMenu after Bootstrap
    # floated cols collapses to width:0 / left:250 (menu invisible).
    # 「推薦」badges are patched in-place (no renderMenu remount).
    column(width = 12,
           sidebarMenu(
             id = "sidebar_tabs",
             menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-line")),
             menuItem("Get Started", tabName = "get_started", icon = icon("play-circle")),
             menuItem("DCF-Model", tabName = "dcf_calculator", icon = icon("calculator")),
             menuItem("DDM", tabName = "ddm_calculator", icon = icon("hand-holding-usd")),
             menuItem("P/B-Asset", tabName = "pb_calculator", icon = icon("landmark")),
             menuItem("RI-Model", tabName = "ri_calculator", icon = icon("gem")),
             menuItem("Sensitivity", tabName = "sensitivity", icon = icon("sliders-h")),
             menuItem("Backtest Zone", tabName = "backtest", icon = icon("vial")),
             menuItem("About", tabName = "about", icon = icon("info-circle"))
             # Snapshot 不放主選單（避免巢狀 li 被瀏覽器抬出隱藏）；改由底部捷徑切換
           ),
           hr()
    ),
    
    column(width = 12,
           h5("Recent Search:"),
           textOutput("recentsearch"),
           hr()
    ),
    
    column(width = 12,
           div(style = "padding: 10px; text-align: center; margin-top: 20px;",
               downloadButton("download_report", "下載完整分析報告 (PDF)", 
                              style = "width: 100%; font-weight: bold; background-color: #1a1a1a; color: #ffffff; border: 1px solid #000000; box-shadow: none; text-shadow: none;")
           )
    ),
    
    column(width = 12,
           div(style = "padding: 15px; border-radius: 5px; border-left: 4px",
               tags$b("Data Source:"), tags$br(),
               "This application integrates real-time financial data via web parsing and API resources, applying comprehensive models for valuation."
           )
    ),

    # Snapshot：側邊欄內容流最底部低調捷徑（勿用 absolute，會跑到搜尋框）
    column(
      width = 12,
      class = "ynow-sidebar-snapshot-foot",
      tags$a(
        href = "#shiny-tab-snapshot",
        `data-toggle` = "tab",
        `data-value` = "snapshot",
        class = "ynow-sidebar-snapshot-link",
        onclick = "Shiny.setInputValue('sidebar_tabs', 'snapshot', {priority: 'event'}); return false;",
        icon("camera", class = "fa-fw"),
        tags$span(" Snapshot")
      )
    )
  ),
  
  dashboardBody(
    withMathJax(),
    
    tags$head(
      tags$style(HTML('
        .main-header .logo { font-weight: bold; }
        #shiny-tab-get_started > h2 { font-weight: 800 !important; }
        /* 側邊欄：僅目前選取頁面粗體（勿固定加粗 Get Started） */
        .sidebar-menu > li > a {
          font-weight: 400 !important;
        }
        .sidebar-menu > li.active > a,
        .sidebar-menu > li.menu-open > a {
          font-weight: 700 !important;
        }
        /* Snapshot：側邊欄內容底部低調捷徑（正常文件流，避免 absolute 跑位） */
        .ynow-sidebar-snapshot-foot {
          margin-top: 8px !important;
          margin-bottom: 10px !important;
          padding: 0 10px !important;
          text-align: left !important;
        }
        .ynow-sidebar-snapshot-link {
          display: inline-block;
          font-size: 11px !important;
          font-weight: 400 !important;
          color: rgba(255,255,255,0.45) !important;
          text-decoration: none !important;
          padding: 2px 4px;
          border-radius: 3px;
          opacity: 0.75;
          line-height: 1.2;
        }
        .ynow-sidebar-snapshot-link:hover {
          color: rgba(255,255,255,0.85) !important;
          opacity: 1;
          background: rgba(255,255,255,0.06);
        }
        /* 主選單不應再出現 Snapshot */
        .sidebar-menu a[data-value="snapshot"] { display: none !important; }
        .sidebar-menu li:has(> a[data-value="snapshot"]) { display: none !important; }
        /* 公司全稱：允許換行，避免被切掉 */
        .ynow-corpname {
          font-weight: bold;
          color: #333333;
          margin: 0;
          line-height: 1.25;
          white-space: normal;
          overflow-wrap: anywhere;
          word-break: break-word;
        }
      ')),
      #region agent log
      tags$script(HTML("(function(){function dbg(h,m,d){fetch('http://127.0.0.1:7828/ingest/e3a0dcdf-71e1-4bba-855e-f942118bd315',{method:'POST',headers:{'Content-Type':'application/json','X-Debug-Session-Id':'dcee0e'},body:JSON.stringify({sessionId:'dcee0e',runId:'snapshot-layout-post',hypothesisId:h,location:'ynow_ui.R:snapshot_sidebar_dbg',message:m,data:d||{},timestamp:Date.now()})}).catch(function(){})}function box(el){if(!el)return null;var r=el.getBoundingClientRect(),cs=window.getComputedStyle(el);return{tag:el.tagName,className:String(el.className||'').slice(0,120),parentClass:el.parentElement?String(el.parentElement.className||'').slice(0,120):null,display:cs.display,position:cs.position,top:Math.round(r.top),bottom:Math.round(r.bottom),nestedLi:!!(el.querySelector&&el.querySelector(':scope > li'))}}function measure(){var foot=document.querySelector('.ynow-sidebar-snapshot-foot'),hidden=document.querySelectorAll('.ynow-snapshot-hidden'),menuSnap=[].slice.call(document.querySelectorAll('.sidebar-menu a[data-value=\\'snapshot\\']')),mainSb=document.querySelector('.main-sidebar'),fb=box(foot),mb=box(mainSb);dbg('H1_H3','snapshot_menu_visibility',{nHiddenWrappers:hidden.length,hidden:[].map.call(hidden,box),nMenuSnapshotAnchors:menuSnap.length,menuSnapVisible:menuSnap.map(function(a){var li=a.closest('li'),cs=li?window.getComputedStyle(li):null;return{text:(a.textContent||'').trim().slice(0,40),liDisplay:cs?cs.display:null,liHasHiddenClass:!!(li&&li.classList.contains('ynow-snapshot-hidden')),parentHasHiddenClass:!!(li&&li.parentElement&&li.parentElement.classList.contains('ynow-snapshot-hidden')),top:Math.round(a.getBoundingClientRect().top)}})});dbg('H2_H4','snapshot_foot_position',{foot:fb,mainSidebar:mb,footParentIsMainSidebar:!!(foot&&foot.parentElement&&foot.parentElement.classList.contains('main-sidebar')),nearTop:fb&&mb?(fb.top-mb.top)<80:null,nearBottom:fb&&mb?(mb.bottom-fb.bottom)<80:null})}if(document.readyState==='loading'){document.addEventListener('DOMContentLoaded',function(){setTimeout(measure,400)})}else{setTimeout(measure,400)}if(window.jQuery){$(document).on('shiny:sessioninitialized',function(){setTimeout(measure,600)})}})();")),
      #endregion
      # region agent log
      tags$script(HTML("
        (function () {
          function send(msg, data, hyp) {
            fetch('http://127.0.0.1:7828/ingest/e3a0dcdf-71e1-4bba-855e-f942118bd315', {
              method: 'POST',
              headers: {
                'Content-Type': 'application/json',
                'X-Debug-Session-Id': '8b7c54'
              },
              body: JSON.stringify({
                sessionId: '8b7c54',
                runId: 'post-inplace-badges',
                hypothesisId: hyp,
                location: 'ynow_ui.R:sidebar_badges',
                message: msg,
                data: data,
                timestamp: Date.now()
              })
            }).catch(function () {});
          }

          function setRecBadge(tab, on) {
            var a = document.querySelector('.sidebar-menu a[data-value=\"' + tab + '\"]');
            if (!a) return;
            var badge = a.querySelector('small.badge');
            if (!on) {
              if (badge) badge.remove();
              return;
            }
            if (!badge) {
              badge = document.createElement('small');
              a.appendChild(badge);
            }
            badge.className = 'badge pull-right bg-red';
            badge.textContent = '推薦';
          }

          function registerBadgeHandler() {
            if (!window.Shiny || !Shiny.addCustomMessageHandler) {
              setTimeout(registerBadgeHandler, 50);
              return;
            }
            Shiny.addCustomMessageHandler('ynowSidebarBadges', function (map) {
              var tabs = ['dcf_calculator', 'ddm_calculator', 'pb_calculator', 'ri_calculator'];
              var applied = {};
              tabs.forEach(function (t) {
                var on = !!(map && map[t] && map[t].on);
                setRecBadge(t, on);
                applied[t] = on;
              });
              send('sidebar_badges_patched', applied, 'H8');
            });
          }
          registerBadgeHandler();

          function arm() {
            var menu = document.querySelector('.sidebar-menu');
            if (!menu) { setTimeout(arm, 250); return; }
            var lastLen = menu.innerHTML.length;
            var childRebuilds = 0;
            var obs = new MutationObserver(function (muts) {
              var child = muts.some(function (m) {
                if (m.type !== 'childList' || (!m.addedNodes.length && !m.removedNodes.length)) return false;
                return Array.prototype.some.call(m.addedNodes, function (n) {
                  return n.nodeType === 1 && n.matches && n.matches('li');
                }) || Array.prototype.some.call(m.removedNodes, function (n) {
                  return n.nodeType === 1 && n.matches && n.matches('li');
                });
              });
              var len = menu.innerHTML.length;
              if (child) {
                childRebuilds += 1;
                var a = document.querySelector('.sidebar-menu li.active > a');
                send('sidebar_li_rebuild', {
                  childRebuilds: childRebuilds,
                  len: len,
                  prevLen: lastLen,
                  active: a ? a.getAttribute('data-value') : null
                }, 'H8');
              }
              lastLen = len;
            });
            obs.observe(menu, { childList: true, subtree: true });
            send('sidebar_observer_armed', { len: lastLen, inPlaceBadges: true }, 'H8');
          }
          if (document.readyState === 'loading') {
            document.addEventListener('DOMContentLoaded', arm);
          } else {
            arm();
          }
        })();
      ")),
      # endregion
      
      tags$style(HTML("
        .selectize-dropdown-content {
          max-height: 300px !important;
          overflow-y: auto !important;
        }
        .selectize-dropdown {
          max-height: 300px !important;
        }

        /* 主搜尋框預選清單：黑字白底 */
        #sc_ticker_suggest {
          position: absolute;
          z-index: 2000;
          left: 0;
          right: 0;
          top: 100%;
          margin-top: 2px;
          max-height: 260px;
          overflow-y: auto;
          background: #ffffff;
          border: 1px solid #cccccc;
          border-radius: 4px;
          box-shadow: 0 4px 10px rgba(0,0,0,0.12);
          display: none;
        }
        #sc_ticker_suggest .ynow-suggest-item {
          display: block;
          width: 100%;
          padding: 8px 12px;
          color: #000000 !important;
          background: #ffffff;
          border: 0;
          border-bottom: 1px solid #eeeeee;
          text-align: left;
          font-size: 13px;
          cursor: pointer;
        }
        #sc_ticker_suggest .ynow-suggest-item:hover,
        #sc_ticker_suggest .ynow-suggest-item:focus {
          background: #f2f2f2;
          color: #000000 !important;
          outline: none;
        }
        #sc_ticker_suggest .ynow-suggest-sym {
          font-weight: 700;
          color: #000000;
          margin-right: 8px;
        }
        #sc_ticker_suggest .ynow-suggest-lab {
          color: #222222;
          font-weight: 400;
        }
        .ynow-sc-wrap {
          position: relative;
          max-width: 400px;
        }
        
        .info-box .info-box-number {
          font-size: 150% !important;
          font-weight: bold;
        }

        /* Finance Summary 卡片網格 */
        .ynow-fs-wrap {
          margin-bottom: 14px;
        }
        .ynow-fs-section {
          margin-bottom: 16px;
        }
        .ynow-fs-section-title {
          font-size: 12px;
          font-weight: 700;
          letter-spacing: 0.06em;
          text-transform: uppercase;
          color: #666666;
          margin: 0 0 8px 0;
          padding-bottom: 4px;
          border-bottom: 1px solid #e5e5e5;
        }
        .ynow-fs-grid {
          display: grid;
          grid-template-columns: repeat(5, minmax(0, 1fr));
          gap: 10px;
        }
        @media (max-width: 992px) {
          .ynow-fs-grid { grid-template-columns: repeat(3, minmax(0, 1fr)); }
        }
        @media (max-width: 576px) {
          .ynow-fs-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }
        }
        .ynow-fs-card {
          background: linear-gradient(165deg, #fafafa 0%, #f0f0f0 100%);
          border: 1px solid #e0e0e0;
          border-left: 3px solid #222222;
          border-radius: 4px;
          padding: 10px 12px;
          min-height: 64px;
          display: flex;
          flex-direction: column;
          justify-content: center;
          transition: border-color 0.15s ease, background 0.15s ease;
        }
        .ynow-fs-card:hover {
          background: #ffffff;
          border-left-color: #555555;
        }
        .ynow-fs-label {
          font-size: 11px;
          font-weight: 600;
          color: #777777;
          line-height: 1.25;
          margin-bottom: 4px;
        }
        .ynow-fs-value {
          font-size: 15px;
          font-weight: 700;
          color: #111111;
          font-variant-numeric: tabular-nums;
          letter-spacing: -0.01em;
          line-height: 1.2;
          word-break: break-word;
        }

        /* Snapshot / HFV 摘要數字：維持可覆寫 class，尺寸回預設 */
        .ynow-kpi-stat-label {
          font-size: 11px !important;
          color: #666 !important;
          line-height: 1.25 !important;
          margin: 0 !important;
        }
        .ynow-kpi-stat-value {
          font-size: 18px !important;
          font-weight: 700 !important;
          line-height: 1.2 !important;
          margin: 0 !important;
          font-variant-numeric: tabular-nums;
        }
        .ynow-kpi-stat-note {
          font-size: 10px !important;
          color: #888 !important;
          margin-top: 2px !important;
          line-height: 1.25 !important;
        }
        .ynow-kpi-stat-params {
          font-size: 12px !important;
          line-height: 1.4 !important;
        }
        .ynow-kpi-hero-value {
          font-size: 22px !important;
          font-weight: 700 !important;
          line-height: 1.2 !important;
        }

        /* KPI：φ⁻¹ 等比例縮小 + 一列五個左排 */
        .ynow-kpi-grid {
          display: flex;
          flex-wrap: wrap;
          justify-content: flex-start;
          align-items: stretch;
          margin-left: -4px;
          margin-right: -4px;
          clear: both;
        }
        .ynow-kpi-grid > * {
          width: 20% !important;
          max-width: 20% !important;
          flex: 0 0 20%;
          float: none !important;
          padding-left: 4px;
          padding-right: 4px;
          box-sizing: border-box;
        }
        @media (max-width: 992px) {
          .ynow-kpi-grid > * {
            width: 33.333% !important;
            max-width: 33.333% !important;
            flex-basis: 33.333%;
          }
        }
        @media (max-width: 576px) {
          .ynow-kpi-grid > * {
            width: 50% !important;
            max-width: 50% !important;
            flex-basis: 50%;
          }
        }
        .ynow-kpi-grid .small-box {
          aspect-ratio: 1.618 / 1 !important;
          display: flex !important;
          flex-direction: column !important;
          justify-content: center !important;
          float: none !important;
          width: 100% !important;
          min-height: 74px !important;
          height: auto !important;
          border-radius: 5px !important;
          margin-bottom: 9px !important;
          box-shadow: 0 2px 4px rgba(0,0,0,0.05) !important;
        }
        .ynow-kpi-grid .small-box .inner {
          padding: 6px 9px !important;
          text-align: center !important;
        }
        .ynow-kpi-grid .small-box .inner h3 {
          font-size: clamp(14px, 2.6vw, 23px) !important;
          font-weight: 800 !important;
          margin: 0 0 5px 0 !important;
          white-space: nowrap;
          overflow: hidden;
          text-overflow: ellipsis;
        }
        .ynow-kpi-grid .small-box .inner p {
          font-size: clamp(10px, 0.75vw, 11px) !important;
          opacity: 0.9;
          font-weight: 500 !important;
          margin: 0 !important;
          line-height: 1.2 !important;
        }
        .ynow-kpi-grid .small-box .icon-large {
          font-size: 37px !important;
          top: 9px !important;
          right: 9px !important;
          opacity: 0.12 !important;
        }
        .ynow-kpi-section-title {
          font-size: 13px;
          font-weight: 700;
          color: #333;
          margin: 12px 0 6px 0;
        }

        /* Backtest：績效指標卡片（軟色調 + 左側色條，避免實心色塊） */
        .ynow-metric-grid {
          --ynow-metric-green: #2d8a57;
          --ynow-metric-green-tint: #eef7f1;
          --ynow-metric-red: #c0392b;
          --ynow-metric-red-tint: #faf0ef;
          --ynow-metric-violet: #5c5a8a;
          --ynow-metric-violet-tint: #f3f2f8;
          --ynow-metric-blue: #2f6f9f;
          --ynow-metric-blue-tint: #eef5fa;
          --ynow-metric-amber: #b7791f;
          --ynow-metric-amber-tint: #faf6ee;
          display: grid;
          grid-template-columns: repeat(3, minmax(0, 1fr));
          gap: 14px;
          margin: 0 0 4px 0;
        }
        @media (max-width: 992px) {
          .ynow-metric-grid { grid-template-columns: 1fr; }
        }
        .ynow-metric-card {
          background: #ffffff;
          border: 1px solid #e6e8eb;
          border-radius: 10px;
          box-shadow: 0 1px 3px rgba(0, 0, 0, 0.06);
          overflow: hidden;
          display: flex;
          flex-direction: column;
          min-height: 0;
          transition: box-shadow 0.15s ease, border-color 0.15s ease;
        }
        .ynow-metric-card:hover {
          box-shadow: 0 3px 10px rgba(0, 0, 0, 0.08);
          border-color: #d5d9de;
        }
        .ynow-metric-card--green {
          border-left: 4px solid var(--ynow-metric-green);
          background: linear-gradient(180deg, var(--ynow-metric-green-tint) 0%, #ffffff 42%);
        }
        .ynow-metric-card--red {
          border-left: 4px solid var(--ynow-metric-red);
          background: linear-gradient(180deg, var(--ynow-metric-red-tint) 0%, #ffffff 42%);
        }
        .ynow-metric-card--violet {
          border-left: 4px solid var(--ynow-metric-violet);
          background: linear-gradient(180deg, var(--ynow-metric-violet-tint) 0%, #ffffff 42%);
        }
        .ynow-metric-card--blue {
          border-left: 4px solid var(--ynow-metric-blue);
          background: linear-gradient(180deg, var(--ynow-metric-blue-tint) 0%, #ffffff 42%);
        }
        .ynow-metric-card--amber {
          border-left: 4px solid var(--ynow-metric-amber);
          background: linear-gradient(180deg, var(--ynow-metric-amber-tint) 0%, #ffffff 42%);
        }
        .ynow-metric-card--blue .ynow-metric-card__icon { background: var(--ynow-metric-blue); }
        .ynow-metric-card--amber .ynow-metric-card__icon { background: var(--ynow-metric-amber); }
        .ynow-metric-card--blue .ynow-metric-card__value { color: #1e4d6e; }
        .ynow-metric-card--amber .ynow-metric-card__value { color: #8a5a12; }
        /* 執行面板：避免 btn-block 蓋住下方說明文字 */
        .ynow-bt-run-panel .btn-block { margin-left: 0; margin-right: 0; }
        .ynow-bt-run-panel .ynow-bt-run-note {
          clear: both;
          display: block;
          position: relative;
          z-index: 1;
          margin: 12px 0 0 0;
          padding: 8px 10px;
          background: #fff8e8;
          border: 1px solid #f0e0b2;
          border-radius: 4px;
          font-size: 11.5px;
          line-height: 1.45;
          color: #6b5a2e;
        }
        .ynow-metric-card__body {
          padding: 14px 16px 12px 16px;
          display: flex;
          flex-direction: column;
          gap: 6px;
        }
        .ynow-metric-card__top {
          display: flex;
          align-items: center;
          gap: 10px;
        }
        .ynow-metric-card__icon {
          flex: 0 0 auto;
          width: 34px;
          height: 34px;
          border-radius: 8px;
          display: inline-flex;
          align-items: center;
          justify-content: center;
          font-size: 15px;
          color: #ffffff;
        }
        .ynow-metric-card--green .ynow-metric-card__icon { background: var(--ynow-metric-green); }
        .ynow-metric-card--red .ynow-metric-card__icon { background: var(--ynow-metric-red); }
        .ynow-metric-card--violet .ynow-metric-card__icon { background: var(--ynow-metric-violet); }
        .ynow-metric-card__label {
          font-size: 10px;
          font-weight: 600;
          color: #555555;
          line-height: 1.3;
          margin: 0;
        }
        .ynow-metric-card__value {
          font-size: clamp(26px, 3.2vw, 34px);
          font-weight: 800;
          font-variant-numeric: tabular-nums;
          letter-spacing: -0.02em;
          line-height: 1.1;
          margin: 0;
          color: #1a1a1a;
        }
        .ynow-metric-card--green .ynow-metric-card__value { color: #1f5c3a; }
        .ynow-metric-card--red .ynow-metric-card__value { color: #8e2a20; }
        .ynow-metric-card--violet .ynow-metric-card__value { color: #3f3d62; }
        .ynow-metric-card__caption {
          margin: 0;
          font-size: 10px;
          color: #6b7280;
          line-height: 1.35;
        }

        /* Backtest：策略參數 tabBox 輕量潤飾 */
        .ynow-bt-params .nav-tabs-custom > .nav-tabs {
          border-bottom-color: #e5e8eb;
        }
        .ynow-bt-params .nav-tabs-custom > .nav-tabs > li > a {
          border-radius: 6px 6px 0 0;
          font-size: 12.5px;
          font-weight: 600;
        }
        .ynow-bt-params .nav-tabs-custom > .tab-content {
          background: #fafbfc;
          border: 1px solid #e8ecef;
          border-top: 0;
          border-radius: 0 0 8px 8px;
          padding: 14px 16px 10px;
        }
        .ynow-bt-params .form-group {
          background: #ffffff;
          border: 1px solid #e8ecef;
          border-radius: 8px;
          padding: 10px 12px 6px;
          margin-bottom: 10px;
          box-shadow: 0 1px 2px rgba(0, 0, 0, 0.04);
        }
        .ynow-bt-params .form-group > label {
          font-size: 12.5px;
          font-weight: 600;
          color: #333;
        }

        /* 僅美化：情緒波動價值（寬螢幕四參數一列；按鈕獨立列） */
        .ynow-bt-mode-b .ynow-bt-mode-b-grid > [class*='col-'] {
          margin-bottom: 8px;
        }
        .ynow-bt-mode-b .ynow-bt-fit-row {
          clear: both;
          margin-top: 4px;
          padding-top: 12px;
          border-top: 1px solid #eee0b8;
        }
        .ynow-bt-mode-b .ynow-bt-fit-panel {
          padding: 12px 14px;
          background: #fcf8e3;
          border: 1px solid #f0e6b2;
          border-radius: 5px;
          font-size: 12px;
          color: #8a6d3b;
          line-height: 1.5;
        }
        .ynow-bt-mode-b .ynow-bt-fit-panel .btn {
          max-width: 320px;
        }

        /* 僅美化：回測驗證（寬螢幕三列） */
        .ynow-bt-validate .ynow-bt-validate-col {
          margin-bottom: 12px;
        }
        .ynow-bt-validate .ynow-bt-validate-col > h5 {
          margin: 0 0 6px 0;
          font-size: 13px;
        }
        .ynow-bt-validate .ynow-bt-validate-panel {
          height: 100%;
          padding: 10px 12px;
          background: #fafbfc;
          border: 1px solid #e8ecef;
          border-radius: 6px;
        }
        .ynow-bt-validate .ynow-bt-validate-panel .table {
          margin-bottom: 0;
          font-size: 12px;
        }
        .ynow-bt-plateau-table-wrap table {
          width: 100% !important;
          table-layout: auto !important;
        }
        .ynow-bt-hfv-wrap {
          position: relative;
        }
        .ynow-bt-hfv-controls {
          position: absolute;
          right: 10px;
          bottom: 10px;
          z-index: 10;
          max-width: 320px;
          min-width: 240px;
          padding: 10px 14px 10px 18px;
          background: rgba(255, 255, 255, 0.97);
          border: 1px solid #dde2e6;
          border-radius: 6px;
          box-shadow: 0 1px 6px rgba(0, 0, 0, 0.08);
          font-size: 11px;
          box-sizing: border-box;
          overflow: visible;
        }
        .ynow-bt-hfv-controls .form-group {
          margin-bottom: 0;
        }
        .ynow-bt-hfv-controls .control-label {
          display: block;
          font-size: 11px;
          font-weight: 600;
          margin-top: 0;
          margin-bottom: 10px;
          line-height: 1.4;
          white-space: normal;
        }
        .ynow-bt-hfv-controls .shiny-options-group {
          margin-top: 0;
          clear: both;
          display: grid;
          grid-template-columns: 1fr 1fr;
          column-gap: 14px;
          row-gap: 2px;
          align-items: start;
          padding-left: 4px;
        }
        .ynow-bt-hfv-controls .radio {
          margin-top: 0;
          margin-bottom: 4px;
        }
        .ynow-bt-hfv-controls .radio label {
          font-size: 11px;
          font-weight: normal;
          line-height: 1.35;
        }
        .ynow-bt-hfv-controls .checkbox {
          margin-top: 4px;
          margin-bottom: 4px;
          min-height: 18px;
          padding-left: 4px;
        }
        .ynow-bt-hfv-controls .checkbox:first-child {
          margin-top: 2px;
        }
        /* Bootstrap checkbox 以 margin-left:-20px 掛在 label 左側；勿壓低 padding-left */
        .ynow-bt-hfv-controls .checkbox label {
          font-size: 11px;
          font-weight: normal;
          line-height: 1.35;
          padding-left: 20px;
          display: inline-block;
          min-height: 18px;
        }
        .ynow-bt-hfv-controls .checkbox input[type='checkbox'] {
          position: absolute;
          margin-left: -20px;
          margin-top: 2px;
        }
        @media (max-width: 767px) {
          .ynow-bt-hfv-controls {
            position: static;
            max-width: none;
            margin-top: 8px;
          }
        }
        @media (max-width: 991px) {
          .ynow-bt-mode-b .ynow-bt-fit-panel .btn {
            max-width: none;
            width: 100%;
          }
        }
        
        /* 針對 search_results (產業資訊) 進行黑白主題與字體縮小；滿寬 */
        #search_results {
          width: 100% !important;
          display: block !important;
          box-sizing: border-box !important;
          white-space: pre-wrap !important;
          background-color: #1e1e1e !important;  /* 深黑色背景 */
          color: #eeeeee !important;             /* 淺白色文字 */
          font-size: 12px !important;            /* 縮小字體 */
          border: 1px solid #444444 !important;  /* 加上細緻的暗色邊框 */
          padding: 8px 12px !important;          /* 調整內邊距讓它扁平一點 */
          border-radius: 4px !important;         /* 圓角 */
          font-weight: 500 !important;
          line-height: 1.2 !important;
        }
        #search_results pre {
          width: 100% !important;
          display: block !important;
          margin: 0 !important;
          white-space: pre-wrap !important;
          background: transparent !important;
          border: none !important;
          color: inherit !important;
        }
      "))
    ),
    
    # ==========================================
    # 獨立的 sc 搜尋輸入框與按鈕區塊
    # ==========================================
    fluidRow(
      column(width = 12,
             titlePanel(h5("a lawrence kuo shiny app")),
             div(
               class = "ynow-sc-wrap",
               textInput("sc", "Ticker / Stock Code", value = APP_DEFAULTS$stock_code),
               uiOutput("sc_ticker_suggest_ui")
             ),
             tags$script(HTML("
               (function() {
                 /* Dropdown only while typing (not on focus/empty). */
                 var typingOpen = false;

                 function scValue() {
                   var inp = document.getElementById('sc');
                   return inp ? (inp.value || '') : '';
                 }

                 function hasTypedQuery() {
                   return scValue().trim().length > 0;
                 }

                 function showSuggest() {
                   var el = document.getElementById('sc_ticker_suggest');
                   if (!el) return;
                   if (typingOpen && hasTypedQuery() && el.children.length) {
                     el.style.display = 'block';
                   } else {
                     el.style.display = 'none';
                   }
                 }

                 function hideSuggest() {
                   typingOpen = false;
                   var el = document.getElementById('sc_ticker_suggest');
                   if (el) el.style.display = 'none';
                 }

                 $(document).on('input', '#sc', function() {
                   var v = $(this).val() || '';
                   Shiny.setInputValue('ticker_typeahead', v, {priority: 'event'});
                   typingOpen = v.trim().length > 0;
                   showSuggest();
                 });

                 $(document).on('blur', '#sc', function(e) {
                   var rt = e.relatedTarget;
                   var el = document.getElementById('sc_ticker_suggest');
                   if (el && rt && el.contains(rt)) return;
                   hideSuggest();
                 });

                 $(document).on('keydown', '#sc', function(e) {
                   if (e.key === 'Enter' || e.keyCode === 13) hideSuggest();
                 });

                 $(document).on('click', '#search', function() {
                   hideSuggest();
                 });

                 $(document).on('mousedown', '#sc_ticker_suggest .ynow-suggest-item', function(e) {
                   e.preventDefault();
                   var sym = $(this).data('symbol');
                   hideSuggest();
                   if (sym) {
                     $('#sc').val(sym).trigger('change');
                     Shiny.setInputValue('sc', sym, {priority: 'event'});
                   }
                 });

                 $(document).on('shiny:value', function(e) {
                   if (e.name === 'sc_ticker_suggest_ui') {
                     setTimeout(showSuggest, 0);
                   }
                 });
               })();
             "))
      )
    ),
    fluidRow(
      column(
        width = 4,
        actionButton("search", "Search", icon = icon("search"))
      ),
      column(
        width = 8,
        h2(textOutput("txt_corpname", inline = TRUE), class = "ynow-corpname")
      )
    ),
    fluidRow(
      column(
        width = 12,
        tags$div(
          style = "width: 100%; text-align: left; margin-top: 8px;",
          tags$p(
            "industry info from Yahoo",
            style = "font-size: 12px; color: #888; margin: 0 0 4px 0; font-weight: bold;"
          ),
          verbatimTextOutput("search_results")
        )
      )
    ),
    br(),
    
    fluidRow(
      infoBoxOutput("ibx_stockprice"),
      infoBoxOutput("ibx_marketcap"),
      infoBoxOutput("ibx_EPS")
    ),
    
    # 插入智能估值顧問的 UI 輸出點（由 decision 模組提供）
    
    tabItems(
      tabItem(
        tabName = "get_started",
        h2(tags$span("Get Started", style = "font-weight: 800 !important; letter-spacing: 0.02em;")),
        fluidRow(
          column(
            width = 12,
            pickerInput(
              inputId = "industry_choice",
              label = "Industry Standard Comparison",
              choices = industry_picker_choices(),
              selected = APP_DEFAULTS$industry_choice,
              options = list(`live-search` = TRUE, `size` = 12)
            )
          )
        ),
        fluidRow(
          box(
            title = tagList(icon("route"), "Model Selector｜估值模型推薦"),
            width = 12, status = "primary", solidHeader = TRUE,
            uiOutput("get_started_model_selector")
          )
        ),
        fluidRow(
          .dcf_core_params_box()
        ),
        tabBox(
          title = "BETA",
          width = "auto",
          tabPanel(
            "Unlevered βᵤ",
            icon = icon("industry"),
            helpText(
              "去槓桿：βᵤ = β_L / (1 + (1−T)·(D/E))，剔除財務槓桿後看營運風險。",
              "③ 可選去槓桿 βᵤ 或未去槓桿估計值（Summary／Rolling／產業平均），直接寫入 CAPM。",
              "「手動輸入」為列表最後一項；與 CAPM β 雙向連動。"
            ),
            beta_unlever_section_ui()
          ),
          tabPanel(
            "Rolling β",
            icon = icon("chart-area"),
            helpText(
              "以常見基準（預設 SPY，亦可 QQQ／IWM）估計未去槓桿 Rolling β。",
              "估計後可按「套用」或於 Unlevered 分頁③選「Rolling 估計」寫入 CAPM。"
            ),
            beta_rolling_section_ui()
          )
        ),
        fluidRow(
          .core_params_location_box()
        )
      ),

      tabItem(
        tabName = "snapshot",
        h2("Snapshot"),
        helpText("上方：目前 App 執行中參數；下方：系統載入時的預設參數表（APP_DEFAULTS）。兩者皆可下載 CSV。"),
        fluidRow(
          box(
            title = tagList(icon("camera"), "Current App Parameter Snapshot"),
            width = 12, status = "info", solidHeader = TRUE,
            div(style = "display:flex; justify-content:space-between; align-items:center; gap:12px; margin-bottom:10px;",
                uiOutput("snapshot_timestamp"),
                downloadButton("download_snapshot", "下載 Snapshot CSV", icon = icon("download"))
            ),
            dataTableOutput("snapshot_table")
          )
        ),
        fluidRow(
          box(
            title = tagList(icon("sliders-h"), "系統預設參數（APP_DEFAULTS）"),
            width = 12, status = "warning", solidHeader = TRUE,
            div(
              style = "display:flex; justify-content:space-between; align-items:center; gap:12px; margin-bottom:10px; flex-wrap:wrap;",
              tags$span(
                style = "font-size:12.5px; color:#666; line-height:1.45;",
                "App 啟動時寫入的預設值（含依預設產業／Rf 動態估出的項目）。與上方「目前參數」可能不同；欄位仍可在各分頁覆寫。"
              ),
              downloadButton("download_defaults", "下載 Defaults CSV", icon = icon("download"))
            ),
            dataTableOutput("defaults_table")
          )
        )
      ),

      tabItem(tabName = "dashboard",

              div(
                style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px; gap: 12px; flex-wrap: wrap;",
                tags$div(
                  style = "display:flex; align-items:center; gap:10px;",
                  actionButton(
                    "bt_kpi_filter", "回測濾鏡",
                    icon = icon("filter"),
                    class = "btn-sm",
                    style = "background-color: #1a5276; color: #ffffff; border: 1px solid #154360; font-size: 12px; padding: 6px 14px; border-radius: 4px; font-weight: 600;"
                  ),
                  uiOutput("bt_filter_badge")
                ),
                actionButton("btn_expand_all", "Expand All",
                             icon = icon("expand"),
                             class = "btn-sm",
                             style = "background-color: #222222; color: #ffffff; border: 1px solid #555555; font-size: 12px; padding: 4px 12px; border-radius: 4px;")
              ),
              uiOutput("bt_filter_detail"),
              
              tabBox(title = "FINANCIAL REPORT",
                     width = "auto",
                     
                     tabPanel("Finance Summary",
                              p("This section imports Finance Summaries from Yahoo Finance",
                                style = "margin-bottom: 12px; color: #666; font-size: 13px;"),
                              uiOutput("fs_summary_ui"),
                              downloadButton('FS_download', "Download Finance Summary")
                     ),
                     
                     tabPanel("Income Statement",
                              p("This section imports Income Statements from Yahoo Finance"),
                              
                              # 🌟 新增：Income Statement 下拉選單與互動圖表
                              selectInput("is_type", "Select Income Statement Metric",
                                          choices = c("Total Revenue", "Gross Profit", "EBITDA")),
                              plotlyOutput("is_plot"),
                              tags$hr(),
                              
                              dataTableOutput("tbIncomeStatement"), 
                              downloadButton('IS_download', "Download Income Statement")
                     ),
                     
                     tabPanel("Balance Sheet",
                              p("This section imports Balance Sheets from Yahoo Finance"),
                              plotlyOutput("bs_plot", height = "380px"),
                              tags$hr(),
                              dataTableOutput("tbBalanceSheet"),
                              downloadButton('BS_download', "Download Balance Sheet")
                     ),
                     
                     tabPanel("Cash Flow",
                              p("This section imports Cash Flow data from Yahoo Finance"),
                              plotlyOutput("cf_plot", height = "460px") %>% withSpinner(),
                              tags$hr(),
                              dataTableOutput("tbCashFlow"),
                              downloadButton('CF_download', "Download Cash Flow Data")
                     )
              ),
              
              uiOutput("dashboard_selected_industry"),
              
              tabBox(title = "PERFORMANCE",
                     width = "auto",
                     
                     tabPanel("KPI by Sheet", fluidRow(
                       column(width = 12,
                              tags$h4("Balance Sheet KPI", class = "ynow-kpi-section-title"),
                              div(class = "ynow-kpi-grid",
                                  valueBoxOutput(NS("kpi", "vbx_eqt_multiplier"), width = NULL)
                              ),
                              tags$h4("Income Statement KPI", class = "ynow-kpi-section-title"),
                              div(class = "ynow-kpi-grid",
                                  valueBoxOutput(NS("kpi", "vbx_net_profit_margin"), width = NULL),
                                  valueBoxOutput(NS("kpi", "vbx_gross_profit_margin"), width = NULL),
                                  valueBoxOutput(NS("kpi", "vbx_opex_ratio"), width = NULL),
                                  valueBoxOutput(NS("kpi", "vbx_rev_growth"), width = NULL),
                                  valueBoxOutput(NS("kpi", "vbx_gross_profit_growth"), width = NULL)
                              ),
                              tags$h4("Cash Flow KPI", class = "ynow-kpi-section-title"),
                              div(class = "ynow-kpi-grid",
                                  valueBoxOutput(NS("kpi", "vbx_op_cash_flow_growth"), width = NULL),
                                  valueBoxOutput(NS("kpi", "vbx_inv_cash_flow_growth"), width = NULL),
                                  valueBoxOutput(NS("kpi", "vbx_fin_cash_flow_growth"), width = NULL)
                              )
                       )
                     )),
                     
                     tabPanel("Crossover KPIs", fluidRow(
                       column(width = 12,
                              div(class = "ynow-kpi-grid",
                                  valueBoxOutput(NS("kpi", "vbx_ROA"), width = NULL),
                                  valueBoxOutput(NS("kpi", "vbx_ROE"), width = NULL),
                                  valueBoxOutput(NS("kpi", "vbx_asset_turnover"), width = NULL),
                                  valueBoxOutput(NS("kpi", "vbx_ocf_net_income"), width = NULL)
                              )
                       )
                     )),
                     
                     tabPanel("Annotation", fluidRow(
                       column(width = 12,
                              div(style = "margin-bottom: 20px; padding: 12px; background: #fdfdfd; border: 1px dashed #ccc; border-radius: 6px; display: flex; align-items: center; justify-content: center; font-size: 13px;",
                                  span(style = "font-weight: bold; margin-right: 15px;", "同業比較圖例:"),
                                  span(icon("circle", style = "color: #0073b7;"), " 高於標準 (Better) ", style = "margin-right: 15px;"),
                                  span(icon("circle", style = "color: #00a65a;"), " 符合標準 (Standard) ", style = "margin-right: 15px;"),
                                  span(icon("circle", style = "color: #dd4b39;"), " 低於標準 (Worse) ", style = "margin-right: 15px;"),
                                  span(icon("circle", style = "color: #333;"), " 無資料 / 錯誤")
                              )
                       ),
                       column(width = 12,
                              tableOutput("stable_indicator_table")
                       )
                     ))
              )
      ),
      
      # ==========================================
      # DDM 頁面設計 (升級雙分頁版)
      # ==========================================
      tabItem(tabName = "ddm_calculator",
              tabBox(title = "DIVIDEND DISCOUNT", width = "auto",
                     
                     # --- 分頁 1：DDM 估值主畫面 ---
                     tabPanel("DDM Overview", icon = icon("calculator"),
                              fluidRow(
                                column(width = 6,
                                       # 🌟 關鍵修復：統一加上 mod_ddm- 前綴
                                       numericInput("mod_ddm-d0", "今年發放股利 (D0)", value = APP_DEFAULTS$ddm_d0),
                                       numericInput(
                                         "mod_ddm-g",
                                         "股利永續成長率 g (%)",
                                         value = APP_DEFAULTS$ddm_g
                                       ),
                                       checkboxInput(
                                         "mod_ddm-sync_g",
                                         "與中央永續成長率（Get Started SGR）同步",
                                         value = isTRUE(APP_DEFAULTS$ddm_sync_central_g)
                                       ),
                                       helpText("勾選時跟隨中央 SGR；取消勾選後可單獨覆寫股利成長率（不必等於 FCFF 終值 g）。"),
                                       numericInput("mod_ddm-ke", "要求報酬率 (Ke) %", value = APP_DEFAULTS$ddm_ke),
                                       helpText("股利屬股權現金流，以 Ke（CAPM）折現；DCF 的 FCFF 則以 WACC 折現。Ke 與 DCF → WACC 的 CAPM 共用；Rolling／Unlevered 在 Get Started。"),
                                       checkboxInput(
                                         "ddm_use_estimated_re",
                                         "採用估算 Ke（來自 CAPM β）",
                                         value = isTRUE(APP_DEFAULTS$use_est_re)
                                       ),
                                       helpText("與 DCF→WACC「採用估算 rₑ」同步；勾選時 Ke 跟隨 CAPM。"),
                                       tags$div(style = "margin-top: 15px; margin-bottom: 15px;",
                                                actionButton("mod_ddm-btn_calc_ddm", "試算 DDM 合理股價", class = "btn-primary", icon = icon("calculator")),
                                                HTML("&nbsp;&nbsp;"), 
                                                actionButton("mod_ddm-reset_ddm", "回復預設", class = "btn-warning", icon = icon("refresh"))
                                       )
                                ),
                                column(width = 6,
                                       # 🌟 關鍵修復：對接後端的 ui_ddm_result
                                       uiOutput("mod_ddm-ui_ddm_result")      
                                )
                              )
                     ),
                     
                     # --- 分頁 2：D0 進階參數設定 ---
                     tabPanel("D0 Settings", icon = icon("cogs"),
                              fluidRow(
                                infoBoxOutput("mod_ddm-ibx_d0_scraped", width = 4),
                                infoBoxOutput("mod_ddm-ibx_d0_eps", width = 4),
                                infoBoxOutput("mod_ddm-ibx_d0_payout", width = 4)
                              ),
                              
                              fluidRow(
                                column(width = 12,
                                       div("實務上常需對 D0 進行平滑化或還原本業配息，避免單一年度特別股利或景氣循環造成估值失真。",
                                           style = "font-size: 15px; font-weight: bold; color: #2C3E50; margin-bottom: 15px; padding: 10px; background-color: #F2F4F4; border-radius: 8px;")
                                ),
                                
                                box(h4(tags$b("方法 1：目標配息率推算法")),
                                    p(helpText("適用於宣告改變股利政策，或未來獲利將發生重大變化的公司")),
                                    div("公式：預估 EPS × 目標配息率",
                                        style = "font-size: 18px; font-weight: bold; color: #2C3E50; text-align: center; margin-bottom: 15px; padding: 10px; background-color: #F2F4F4; border-radius: 8px;"),
                                    numericInput("mod_ddm-est_eps", "預估/最新 EPS (元)", value = NA, step = 0.01),
                                    numericInput("mod_ddm-est_payout", "目標配息率 Payout Ratio (%)", value = NA, min = 0, max = 100, step = 0.01),
                                    actionButton("mod_ddm-calc_d0_payout", "計算並套用 D0", class = "btn-primary"),
                                    tags$br(),
                                    htmlOutput("mod_ddm-txt_d0_payout_res")
                                ),
                                
                                box(h4(tags$b("方法 2：景氣循環平滑法")),
                                    p(helpText("適用於航運、原物料等景氣循環股。系統將自動從現金流量表抓取歷史配息來平均。")),
                                    numericInput("mod_ddm-cycle_years", "抓取過去幾年平均？", value = 5, min = 1, max = 10, step = 0.01),
                                    actionButton("mod_ddm-calc_d0_average", "計算並套用平均 D0", class = "btn-primary"),
                                    tags$br(),
                                    htmlOutput("mod_ddm-txt_d0_avg_res")
                                )
                              )
                     ),

                     # --- 分頁 3：指向 Get Started 的 Beta 單一來源 ---
                     tabPanel(
                       "Beta (β)",
                       icon = icon("chart-line"),
                       fluidRow(
                         column(
                           width = 12,
                           .beta_moved_to_get_started_box(
                             tagList(
                               htmlOutput("ddm_beta_ke_status"),
                               helpText("DDM Overview 的「採用估算 Ke」仍跟隨 DCF → WACC 的 CAPM。")
                             )
                           )
                         )
                       )
                     )
              ),
              .model_param_sensitivity_box(
                "DDM 公式參數：每股估值貢獻與敏感度",
                "mod_ddm-param_sensitivity_table"
              )
      ),
      
      # ==========================================
      # DCF Calculator 分頁
      # ==========================================
      tabItem(tabName = "dcf_calculator",
              tabBox(width = "auto",
                     tabPanel("", 
                              fluidRow(
                                column(width = 6,
                                       radioButtons("dcf_mode", "選擇 DCF 估值模型：",
                                                    choices = list(
                                                      "明確預測 + Gordon 終值" = "gordon",
                                                      "二階段成長法 (Two-Stage Model)" = "two_stage"
                                                    ),
                                                    selected = APP_DEFAULTS$dcf_mode),
                                       # WACC 改由 DCF → WACC 分頁／CAPM 同步；此處隱藏保留 input$id 供計算鏈使用
                                       tags$div(
                                         style = "display:none;",
                                         numericInput(
                                           "wacc_gordon", "折現率 WACC (%)",
                                           value = APP_DEFAULTS$wacc_gordon, step = 0.01
                                         )
                                       )
                                ),
                                column(width = 6, numericInput("years", "預測年數 n", value = APP_DEFAULTS$years, min = 1, max = 30))
                              )
                     )
              ),
              
              tabBox(title = "DISCOUNTED CASH FLOW", width = "auto",
                     tabPanel("DCF Overview",
                              fluidRow(
                                column(width = 12,
                                       fluidRow(
                                         infoBoxOutput("ibx_stock_value_dcf", width = 6),
                                         infoBoxOutput("ibx_enterprise_value_dcf", width = 6)
                                       )
                                )
                              ),
                              fluidRow(
                                column(width = 12,
                                       radioButtons(
                                         "dcf_chart_mode",
                                         "圖表顯示模式",
                                         choices = c(
                                           "單純模式（歷史＋預測 FCFF，無折現線）" = "simple",
                                           "顯示折現後價值（DCF）" = "with_dcf"
                                         ),
                                         selected = APP_DEFAULTS$dcf_chart_mode,
                                         inline = TRUE
                                       ),
                                       plotOutput("plt_dcf_trajectory", height = "420px"),
                                       h6(helpText("提示：圖含歷史 FCFF；切換模式可隱藏／顯示折現後 DCF 線。啟動時已自動計算，自訂參數後可再點試算。")),
                                       fluidRow(
                                         column(width = 6, actionButton("calc", "試算 DCF", class = "btn-success btn-block", style = "padding: 12px; font-weight: bold; font-size: 16px;")),
                                         column(width = 6, actionButton("reset_dcf", "回復預設", class = "btn-default btn-block", style = "padding: 12px; font-weight: bold; font-size: 16px;"))
                                       ),
                                       tags$div(style = "margin-top: 10px;", htmlOutput("vtxt_dcf_setting_details"))
                                )
                              )
                     ),
                     
                     tabPanel("DCF Calculation Details",
                              fluidRow(
                                column(width = 12,
                                       plotOutput("mod_fcf-fcf_plot", height = "350px"),
                                       htmlOutput("mod_fcf-txt_fcf_raw_data") 
                                ),
                                uiOutput("ui_data_validation") 
                              )
                     )
              ),
              
              tabBox(width = "auto",
                     tabPanel("Overview",
                              fluidRow(
                                infoBoxOutput("ibx_estimated_g", width = 6),
                                infoBoxOutput("ibx_sgr", width = 6),
                                
                                column(width = 12,
                                       plotOutput("plt_fcf_trend", height = "350px")
                                ),
                                column(
                                  width = 12,
                                  tags$div(style = "margin-top: 12px;", .dcf_two_stage_params_box())
                                )
                              )
                     ),
                     
                     # 在 ui.R 的某個 tabBox 或 navbarMenu 中：
                     fcf_projection_module_ui(id = "mod_fcf"),
                     
                     tabPanel("WACC",
                              icon = icon("balance-scale"),
                              fluidRow(
                                valueBoxOutput("vbx_equity_val", width = 4), # 股權市值 (E)
                                valueBoxOutput("vbx_debt_val", width = 4),   # 總負債 (D)
                                valueBoxOutput("vbx_tax_rate", width = 4)    # 有效稅率 (T)
                              ),
                              fluidRow(
                                infoBoxOutput("ibx_wacc", width = 4),
                                infoBoxOutput("ibx_rd", width = 4), 
                                infoBoxOutput("ibx_re", width = 4)
                              ),
                              
                              fluidRow(
                                div("WACC = E / (E + D) × rₑ + D / (E + D) × rᵈ × (1 - T)",
                                    style = "font-size: 18px; font-weight: bold; color: #2C3E50; text-align: center; margin-bottom: 15px; padding: 10px; background-color: #F2F4F4; border-radius: 8px;"),
                                box(h4("WACC 估算"),
                                    numericInput("wacc_re", "股權成本 rₑ (%)", value = APP_DEFAULTS$wacc_re, min = 0, step = 0.01),
                                    checkboxInput("use_estimated_re", "採用估算 rₑ（來自CAPM）", value = APP_DEFAULTS$use_est_re),
                                    numericInput("wacc_rd", "負債成本 rᵈ (%)", value = APP_DEFAULTS$wacc_rd, min = 0, step = 0.01),
                                    numericInput("wacc_tax", "所得稅率 T (%)", value = APP_DEFAULTS$wacc_tax, min = 0, max = 100, step = 0.01),
                                    actionButton("calc_wacc", "計算 WACC", class = "btn-primary"),
                                    tags$br(), htmlOutput("wacc_result")
                                ),
                                capm_beta_settings_ui(
                                  title = "CAPM 估算 rₑ",
                                  calc_id = "calc_capm",
                                  result_id = "capm_result",
                                  advanced_hint = TRUE
                                )
                              )
                     ),

                     tabPanel(
                       "Beta (β)",
                       icon = icon("chart-line"),
                       fluidRow(column(width = 12, .beta_moved_to_get_started_box()))
                     )
              ),
              .model_param_sensitivity_box(
                "DCF 公式參數：每股估值貢獻與敏感度",
                "dcf_param_sensitivity_table"
              )
      ),
      
      # 🌟 呼叫 RI 模型分頁介面
      ri_module_ui("mod_ri"),
      
      # 🌟 呼叫 P/B／資產估值分頁介面
      pb_asset_module_ui("mod_pb"),
      
      tabItem(tabName = "sensitivity",
              
              decision_ui("main_decision"),
              
              tabBox(title = "SENSITIVITY", width = "auto",
                     fluidRow(
                       column(
                         width = 12,
                         h4("敏感度分析矩陣 (Sensitivity Analysis)"),
                         p(helpText("軸心採用 Get Started／Dashboard 目前的 SGR 與 WACC；觀察鄰近組合下的每股內在價值變化。CapEx／ΔNWC 前瞻比率請至 DCF → FCFF 設定。"))
                       )
                     ),
                     fluidRow(
                       column(
                         width = 12,
                         div(
                           style = "width: 100%; overflow-x: auto;",
                           tags$style(HTML("#dcf_sensitivity_table table { width: 100% !important; table-layout: fixed; }")),
                           tableOutput("dcf_sensitivity_table")
                         )
                       )
                     ),
                     fluidRow(
                       column(
                         width = 12,
                         uiOutput("sensitivity_analysis_panel")
                       )
                     )
              )
      ),
      
      tabItem(tabName = "backtest",
              withMathJax(),
              h2("量化回測實驗室 (Backtest Zone)"),
              .bt_section_intro(
                "先看折現比較圖（基本面價值 vs 實際股價＝情緒波動價值 vs 大盤），再看策略淨值（倉位規則賺不賺錢）。Ke／WACC 採 Rolling β。"
              ),

              # 1) 折現比較圖置頂：基本面價值 vs 情緒波動價值(實際股價) vs 大盤
              fluidRow(
                box(
                  title = tagList(icon("balance-scale"), "折現比較：基本面價值 vs 情緒波動價值 vs 大盤"),
                  width = 12, status = "primary", solidHeader = TRUE,
                  .bt_section_intro(
                    "歷史各點：僅用當時可得財報＋Rolling β 設算「基本面價值」（不套用目前分頁參數）。僅折線末端最新點掛勾目前 APP 分頁設定（DCF／DDM／RI／P/B）；「情緒波動價值」＝該股歷史實際股價；並疊加大盤（右軸）。搜尋後即預覽股價／大盤；勾選右下角評價模型即計算並顯示基本面價值（預設不勾選）。"
                  ),
                  tags$div(
                    class = "ynow-bt-hfv-wrap",
                    uiOutput("bt_valuation_summary"),
                    plotlyOutput("bt_hfv_timeline", height = "420px") %>% withSpinner(),
                    tags$div(
                      class = "ynow-bt-hfv-controls",
                      checkboxGroupInput(
                        "bt_fv_models",
                        "回測用評價模型（可複選疊圖）",
                        inline = FALSE,
                        choices = c(
                          "DCF" = "dcf",
                          "DDM" = "ddm",
                          "RI" = "ri",
                          "P/B" = "pb"
                        ),
                        selected = character(0)
                      )
                    ),
                    uiOutput("bt_signal_explain")
                  )
                )
              ),

              # 2) 績效指標
              fluidRow(
                box(
                  title = tagList(icon("trophy"), "回測績效指標"),
                  width = 12, status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                  uiOutput("perf_metrics")
                )
              ),

              # 3) 淨值圖 + 執行面板
              fluidRow(
                box(
                  title = tagList(icon("chart-area"), "兩模式策略淨值比較"),
                  width = 8, status = "info", solidHeader = TRUE,
                  tags$div(
                    style = "margin: 0 0 10px 0; padding: 10px 12px; background: #f4f8fb; border-left: 4px solid #3c8dbc; font-size: 12px; color: #444; line-height: 1.55;",
                    tags$b("關聯與差異："),
                    "兩者共用「持倉回測條件」閘門，但倉位路徑不同——",
                    tags$b("純基本面價值"), " = MOS／Value Gap 映射的 Exp_A；",
                    tags$b("情緒波動價值"), " = Exp_A 與「動能／RSI 情緒目標」加權混合（情緒熱→偏滿倉，情緒冷→偏保守），",
                    "故淨值折線應可分開，而非重疊。上方折現圖的「情緒波動價值」則指實際股價，勿與本圖策略淨值混淆。"
                  ),
                  plotlyOutput("bt_equity_plot", height = "400px") %>% withSpinner(),
                  tags$ul(
                    style = "margin: 10px 0 0 0; padding-left: 18px; font-size: 12px; color: #666; line-height: 1.55;",
                    tags$li(tags$b("純基本面價值"), "（橘線）＝模式 A：持倉條件＋MOS 倉位 × 日報酬。"),
                    tags$li(tags$b("情緒波動價值"), "（藍線）＝模式 B：在 Exp_A 上混入動能／RSI 情緒目標；參數見「情緒波動價值」標籤。"),
                    tags$li(tags$b("該股買進持有"), "（綠）全程 100%；", tags$b("大盤"), "（灰虛）SPY。"),
                    tags$li("股價／合理價美元比較見上方折現圖，勿與淨值混比。")
                  )
                ),
                box(
                  title = tagList(icon("play-circle"), "執行面板"),
                  width = 4, status = "warning", solidHeader = TRUE,
                  class = "ynow-bt-run-panel",
                  checkboxInput(
                    "bt_param_auto",
                    "自動同步參數（換股時依財報推導）",
                    value = TRUE
                  ),
                  .bt_hint(
                    "模式開關：勾選後，搜尋／載入新公司時會自動覆寫持倉門檻、曝險／情緒權重，並對齊上方推薦估值模型。手動改參數會自動取消勾選。"
                  ),
                  actionButton(
                    "bt_refresh_params", "立即依目前公司重算一次",
                    icon = icon("sync"), class = "btn-default btn-block",
                    style = "margin-bottom: 10px;"
                  ),
                  .bt_hint(
                    "單次動作：立刻用目前公司財報重算門檻／權重（可在取消自動後使用，不想持續自動覆寫時按一次即可）。"
                  ),
                  actionButton(
                    "run_bt", "啟動量化回測",
                    class = "btn-warning btn-lg btn-block",
                    style = "margin-bottom: 0;"
                  ),
                  tags$div(
                    class = "ynow-bt-run-note",
                    "季頻再平衡 · Rolling β 折現 · 依所選評價模型 PIT 重建。"
                  ),
                  uiOutput("bt_run_status")
                )
              ),

              # 4) 策略參數設定（置於 Exposure／B&H 歸因上方，方便先調再對照）
              fluidRow(column(width = 12, uiOutput("bt_param_notes"))),

              fluidRow(
                tags$div(
                  class = "ynow-bt-params",
                  tabBox(
                    title = tagList(icon("sliders-h"), "策略參數設定"),
                    width = 12,
                    tabPanel(
                      title = tagList(icon("filter"), "持倉回測條件"),
                      .bt_section_intro("季頻再平衡日四項皆過才允許持倉；否則純基本面價值／情緒波動價值皆空手。"),
                      fluidRow(
                        column(3, tipify(numericInput("bt_net_margin", "淨利率門檻 (%)", 5),
                                         "自動模式取該公司歷史淨利率約一半。", placement = "top")),
                        column(3, tipify(numericInput("bt_rev_growth", "營收成長門檻 (%)", 25),
                                         "自動模式取歷史營收成長約一半。", placement = "top")),
                        column(3, tipify(numericInput("bt_eps_growth", "EPS／淨利成長門檻 (%)", 15),
                                         "自動模式取淨利成長約一半。", placement = "top")),
                        column(3, tipify(numericInput("bt_fcf_cv", "FCF 變異係數上限 (%)", 20),
                                         "自動模式取 FCF CV × 1.25。", placement = "top"))
                      )
                    ),
                    tabPanel(
                      title = tagList(icon("balance-scale"), "純基本面價值"),
                      .bt_section_intro(
                        "模式 A：Exp_A → 淨值圖橘線。依 MOS 分級決定基本面倉位；上方折現圖的「基本面價值」驅動 MOS。"
                      ),
                      fluidRow(
                        column(
                          6,
                          sliderInput("bt_w_vg", "MOS／Value Gap 權重（曝險）", 0, 1, 0.7, step = 0.01),
                          .bt_hint("越大越依 MOS 分級減碼；越小越接近固定中性倉位。")
                        ),
                        column(
                          6,
                          tags$div(
                            style = "margin-top: 8px; padding: 12px; background: #f4f8fb; border: 1px solid #d6e4f0; border-radius: 5px; font-size: 12px; color: #3c5a73; line-height: 1.55;",
                            tags$b("MOS 滯後曝險（基準圖）"), tags$br(),
                            "MOS≥30%→接近最大持股；≥10%→約 72%×上限；≥0%→約 44%×上限；≥−10%→約 17%×上限；否則空手。",
                            "（最大／最低持股與「貼近買進持有」在「情緒波動價值」標籤。）"
                          )
                        )
                      )
                    ),
                    tabPanel(
                      title = tagList(icon("bolt"), "情緒波動價值"),
                      tags$div(
                        class = "ynow-bt-mode-b",
                        .bt_section_intro(
                          "模式 B：在 Exp_A 上混入動能／RSI「情緒目標」（熱→偏滿倉、冷→偏保守），使藍線與橘線可分開。上方折現圖的「情緒波動價值」＝實際股價，語意不同。"
                        ),
                        # 寬螢幕四參數一列：動能 / RSI / 最大持股 / 最低持股
                        tags$div(
                          class = "ynow-bt-mode-b-grid",
                          fluidRow(
                            column(
                              3,
                              sliderInput("bt_w_mom", "動能相對權重", 0, 1, 0.4, step = 0.01),
                              .bt_hint("與 RSI 組成情緒分數，再與 Exp_A 混合。")
                            ),
                            column(
                              3,
                              sliderInput("bt_w_rsi", "RSI 相對權重", 0, 1, 0.3, step = 0.01),
                              .bt_hint("過熱降低情緒目標；超賣提高。")
                            ),
                            column(
                              3,
                              sliderInput("bt_max_exp", "最大持股上限", 0.5, 1, 0.9, step = 0.01),
                              .bt_hint("拉到 1.00 可消除結構性少倉，利於貼近買進持有。")
                            ),
                            column(
                              3,
                              sliderInput("bt_min_exp_pass", "通過條件後最低持股", 0, 0.4, 0, step = 0.01),
                              .bt_hint("持倉條件通過且非極度高估時的地板倉位。")
                            )
                          )
                        ),
                        # 按鈕獨立一列（不與滑桿並排）
                        tags$div(
                          class = "ynow-bt-fit-row",
                          fluidRow(
                            column(
                              12,
                              tags$div(
                                class = "ynow-bt-fit-panel",
                                actionButton(
                                  "bt_fit_bh_preset", "貼近買進持有",
                                  icon = icon("chart-line"),
                                  class = "btn-success",
                                  style = "font-weight:600;"
                                ),
                                tags$div(
                                  style = "margin-top:8px;font-size:11px;color:#666;",
                                  "一鍵：最大持股=100%、最低持股=40%、w_vg=0.35（弱化減碼）。會關閉自動同步。"
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              ),

              fluidRow(
                box(
                  title = tagList(icon("percentage"), "兩模式倉位軌跡（Exposure）"),
                  width = 6, status = "danger", solidHeader = TRUE, collapsible = TRUE,
                  uiOutput("bt_exposure_stats"),
                  plotlyOutput("bt_exposure_plot", height = "260px") %>% withSpinner()
                ),
                box(
                  title = tagList(icon("search-dollar"), "為何輸給 Buy & Hold？"),
                  width = 6, status = "warning", solidHeader = TRUE, collapsible = TRUE,
                  uiOutput("bt_bh_gap")
                )
              ),

              # 5) 回測驗證：保留 MOS／FV（策略訊號是否有效）；參數高原已移出（與 Sensitivity 重疊）
              fluidRow(
                tags$div(
                  class = "ynow-bt-validate",
                  box(
                    title = tagList(icon("flask"), "回測驗證：MOS 與 Fair Value"),
                    width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                    .bt_section_intro(
                      "用來檢查「低估是否伴隨較佳前瞻報酬」——這是回測策略能否成立的核心。參數敏感度請改看 Sensitivity 分頁（WACC×g 矩陣）。"
                    ),
                    fluidRow(
                      column(
                        6,
                        tags$div(
                          class = "ynow-bt-validate-col",
                          tags$div(
                            class = "ynow-bt-validate-panel",
                            tags$h5(tags$b("MOS 有效性")),
                            .bt_hint("依 MOS 分組統計 1Y／3Y／5Y 前瞻報酬：MOS 愈高是否報酬愈好？"),
                            tags$div(style = "overflow-x:auto;", tableOutput("bt_mos_table"))
                          )
                        )
                      ),
                      column(
                        6,
                        tags$div(
                          class = "ynow-bt-validate-col",
                          tags$div(
                            class = "ynow-bt-validate-panel",
                            tags$h5(tags$b("Fair Value 預測能力")),
                            uiOutput("bt_fv_edge"),
                            tags$div(style = "overflow-x:auto;", tableOutput("bt_fv_table"))
                          )
                        )
                      )
                    )
                  )
                )
              ),

              # 6) 方法論
              fluidRow(
                box(
                  title = tagList(icon("book"), "回測數據來源與計算過程（方法論註解）"),
                  width = 12, status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, collapsed = TRUE,
                  uiOutput("bt_methodology_notes"),
                  tags$div(
                    style = "margin-top: 12px;",
                    downloadButton(
                      "download_bt_methodology",
                      "下載方法論說明（Markdown）",
                      icon = icon("download"),
                      class = "btn-primary",
                      style = "font-weight: 600;"
                    )
                  )
                )
              )
      ),
      
      # ==========================================
      # ℹ️ About 分頁 (系統介紹與評價方法論)
      # ==========================================
      tabItem(tabName = "about",
              fluidRow(
                column(width = 12,
                       h2(tags$b("About The YNow App")),
                       p("The YNow App v13.0 以「先分類，再選模型；先推導，再校正；先給區間，再給單點」為方法論，整合自動化資料抓取、主／副估值模型（DCF／DDM／P/B／RI）與決策區間看板。"),
                       tags$hr()
                )
              ),
              
              fluidRow(
                column(width = 12,
                       h3(tags$b("Financial Fraud Red Flags (財務舞弊警訊)")),
                       p("本系統內建五項核心排雷機制，透過交叉比對現金流與獲利品質，自動偵測潛在的地雷股："),
                       tags$ul(
                         tags$li(tags$b("無自由現金流 (No FCF)："), "長期 FCF 為負，代表企業無法靠自身營運創造現金，需依賴外部融資。"),
                         tags$li(tags$b("無營業現金流 (No OCF)："), "OCF 為負是極度危險的訊號，代表核心本業正在失血。"),
                         tags$li(tags$b("獲利未實現 (OCF < Net Income)："), "俗稱「紙上富貴」，損益表雖然賺錢，但現金沒有實際流入公司，可能存在應收帳款作帳疑慮。"),
                         tags$li(tags$b("虛假獲利 (Net Income > 0 but OCF < 0)："), "最經典的舞弊特徵，強烈暗示獲利品質不佳。"),
                         tags$li(tags$b("高財務槓桿 (Debt/Equity > 2)："), "負債比過高，在升息循環或景氣下行時面臨極大的流動性風險。")
                       ),
                       tags$hr()
                )
              ),
              
              fluidRow(
                column(width = 12,
                       h3(tags$b("Valuation Methodology (評價方法論)")),
                       
                       p("在進行企業估值時，選擇正確的模型與計算數字一樣重要。以下是本系統支援的三大評價邏輯與其適用場景："),
                       
                       tabBox(title = "模型選擇決策指南", width = 12, side = "left",
                              
                              # Tab 1: 方法論比較矩陣 (表格)
                              tabPanel("Decision Matrix", icon = icon("table"),
                                       tags$div(style = "overflow-x: auto; margin-bottom: 18px;",
                                                HTML("<table class='table table-striped table-hover table-bordered' style='background-color: white;'>
                                                        <thead style='background-color: #2C3E50; color: white;'>
                                                          <tr>
                                                            <th>對照項目</th>
                                                            <th>DDM（股利 Gordon）</th>
                                                            <th>DCF（明確預測 + Gordon 終值）</th>
                                                          </tr>
                                                        </thead>
                                                        <tbody>
                                                          <tr>
                                                            <td><b>現金流</b></td>
                                                            <td>每股股利 D（股權請求權）</td>
                                                            <td>企業自由現金流 FCFF（全體資金提供者）</td>
                                                          </tr>
                                                          <tr>
                                                            <td><b>折現率</b></td>
                                                            <td>Ke（CAPM 股權成本）</td>
                                                            <td>WACC（加權平均資本成本）</td>
                                                          </tr>
                                                          <tr>
                                                            <td><b>成長率 g</b></td>
                                                            <td>股利永續成長率（可與中央 SGR 同步或覆寫）</td>
                                                            <td>FCFF 終值成長率 SGR（相對 WACC）</td>
                                                          </tr>
                                                          <tr>
                                                            <td><b>「Gordon」含義</b></td>
                                                            <td>整段估值：P₀ = D₁ / (Ke − g)</td>
                                                            <td>僅終值：TV = FCFₙ(1+g)/(WACC−g)；另加 n 年明確預測折現</td>
                                                          </tr>
                                                          <tr>
                                                            <td><b>輸出</b></td>
                                                            <td>直接為每股合理價</td>
                                                            <td>先得企業價值 EV，再加減淨現金／負債後 ÷ 股數</td>
                                                          </tr>
                                                        </tbody>
                                                      </table>")
                                       ),
                                       tags$div(style = "overflow-x: auto;",
                                                HTML("<table class='table table-striped table-hover table-bordered' style='background-color: white;'>
                                                        <thead style='background-color: #2C3E50; color: white;'>
                                                          <tr>
                                                            <th>考慮維度</th>
                                                            <th>股利折現模型 (DDM)</th>
                                                            <th>自由現金流 (FCFF / FCFE)</th>
                                                            <th>剩餘收益模型 (RI)</th>
                                                          </tr>
                                                        </thead>
                                                        <tbody>
                                                          <tr>
                                                            <td><b>主要資料來源</b></td>
                                                            <td>現金流量表（現金股利支付）</td>
                                                            <td>現金流量表（營運與資本支出）</td>
                                                            <td>損益表與資產負債表（淨利與權益）</td>
                                                          </tr>
                                                          <tr>
                                                            <td><b>投資者身分 / 觀點</b></td>
                                                            <td>少數股東（無決策與控制權）</td>
                                                            <td>控股股東 / 併購者（有決策權）</td>
                                                            <td>皆可（尤其適用於負 FCF）</td>
                                                          </tr>
                                                          <tr>
                                                            <td><b>企業發展階段</b></td>
                                                            <td>成熟期、穩健期（如公用事業）</td>
                                                            <td>成長期、擴張期（如科技股）</td>
                                                            <td>各階段皆可，尤其是資產密集型</td>
                                                          </tr>
                                                          <tr>
                                                            <td><b>對配息政策依賴度</b></td>
                                                            <td><span class='label label-danger'>極高</span></td>
                                                            <td><span class='label label-success'>低</span></td>
                                                            <td><span class='label label-success'>極低</span></td>
                                                          </tr>
                                                        </tbody>
                                                      </table>")
                                       )
                              ),
                              
                              # Tab 3: DDM 模型解說
                              tabPanel("Dividend Discount Model (DDM)", icon = icon("hand-holding-usd"),
                                       h4(tags$b("股利折現模型（股利 Gordon）")),
                                       p("DDM 將普通股價值視為未來現金股利的現值。現金流是股利、折現率是 Ke，與以 FCFF／WACC 為核心的 DCF 屬不同層級。"),
                                       tags$ul(
                                         tags$li(tags$b("$$P_0 = \\frac{D_1}{K_e - g} = \\frac{D_0 \\times (1 + g)}{K_e - g}$$"))
                                       ),
                                       p("股利成長率 g 可與中央終值 SGR 同步，亦可在 DDM 分頁單獨覆寫。基本面法可參考 $$g = ROE \\times Retention\\ Ratio$$，但不宜與 FCFF 終值 g 強制畫上等號。")
                              ),
                              
                              # Tab 2: DCF 模型解說
                              tabPanel("Discounted Cash Flow (DCF)", icon = icon("money-bill-wave"),
                                       h4(tags$b("自由現金流折現模型 (FCFF)")),
                                       p("DCF 關注企業造血能力：將未來 FCFF 以 WACC 折現得到企業價值，再橋接至股權價值與每股價格。本 app 的「Gordon」模式為明確預測期加上 Gordon 終值，而非單期 EV = FCF₁/(WACC−g)。"),
                                       tags$ul(
                                         tags$li(tags$b("$$FCFF = Net Income + D\\&A - \\Delta NWC - CapEx$$")),
                                         tags$li(tags$b("$$Enterprise\\ Value = \\sum \\frac{FCFF_t}{(1+WACC)^t} + \\frac{Terminal\\ Value}{(1+WACC)^n}$$")),
                                         tags$li(tags$b("$$Terminal\\ Value = \\frac{FCFF_n \\times (1 + g)}{WACC - g}$$"))
                                       ),
                                       p("兩階段模式則在高速成長期後，將終值成長率收斂至 SGR；約束條件為 g < WACC（不是 Ke）。")
                              )
                       ),
                       
                       uiOutput("main_decision-ui_valuation_compare")
                )
              )
      )
    )
  )
)
