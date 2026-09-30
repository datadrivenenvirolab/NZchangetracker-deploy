# Target Change Tracker — Shiny front end for target_change_tracker.R
# R/tracker.R is sourced automatically by shiny::runApp().
# Styling follows zerotracker.net (www/zerotracker.css, Franz Sans in www/fonts).

library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(lubridate)

CACHE_FILE <- file.path("cache", "panel.rds")

# What is analysed — actor types, start month, time step, tracked variables —
# is set in the ANALYSIS RULES block at the top of R/tracker.R. This file only
# presents it, so no rule is restated here.
VAR_LABELS <- TARGET_VAR_LABELS

ACTOR_PLURALS <- c(Company = "Companies", Country = "Countries",
                   Region = "Regions", City = "Cities")

pluralise <- function(x) ifelse(x %in% names(ACTOR_PLURALS), ACTOR_PLURALS[x], paste0(x, "s"))

scope_label <- function() {
  if (all(is.na(ANALYSIS_ACTOR_TYPES))) "All actor types"
  else paste(pluralise(ANALYSIS_ACTOR_TYPES), collapse = ", ")
}

scope_note <- function() {
  sprintf("%s · %s timesteps, %s onward.",
          if (all(is.na(ANALYSIS_ACTOR_TYPES))) "All actor types" else paste(scope_label(), "only"),
          if (ANALYSIS_MONTHLY) "monthly" else "per-snapshot",
          format(ANALYSIS_START, "%Y"))
}

# zerotracker.net palette
GRAPE    <- "#271039"   # darkGrape: header, headings
OCEAN    <- "#3b7cc3"   # oceanBlue
RED      <- "#e23b25"   # brightRed: primary buttons
MIDNIGHT <- "#2f4b7c"   # midnightBlue
SURFACE  <- "#ffffff"
INK      <- "#111827"
INK_2    <- "#374151"
MUTED    <- "#6b7280"
GRID     <- "#ededed"   # brandGray
AXIS     <- "#dbdbdb"   # circleGray
FONT     <- "'Franz Sans', system-ui, -apple-system, 'Segoe UI', sans-serif"

# Categorical order drawn from the site's chart hues (checked with the dataviz validator)
SERIES <- c("#30639c", "#c9852f", "#a05195", "#73a649",
            "#3b7cc3", "#e23b25", "#665191", "#d45087")
# Recycled so adding a variable to the rules block still gets a colour
VAR_COLORS <- setNames(rep_len(SERIES, length(VAR_LABELS)), names(VAR_LABELS))

fmt_value <- function(x) ifelse(is.na(x), "(blank)", as.character(x))
fmt_int   <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)

chart_theme <- function(p, x = list(), y = list(), ...) {
  axis <- list(gridcolor = GRID, linecolor = AXIS, zerolinecolor = AXIS,
               tickfont = list(color = MUTED, size = 11), automargin = TRUE)
  p %>%
    layout(paper_bgcolor = SURFACE, plot_bgcolor = SURFACE,
           font = list(family = FONT, color = INK_2, size = 12),
           hoverlabel = list(bgcolor = "#ffffff", bordercolor = AXIS,
                             font = list(family = FONT, color = INK)),
           margin = list(l = 8, r = 16, t = 8, b = 8),
           xaxis = modifyList(axis, x), yaxis = modifyList(axis, y), ...) %>%
    config(displaylogo = FALSE,
           modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))
}

nzt_value_box <- function(...) {
  value_box(..., theme = value_box_theme(bg = "#f9f9f9", fg = GRAPE))
}

# ── UI ────────────────────────────────────────────────────────

ui <- page_sidebar(
  title = tags$span(
    class = "nzt-brand",
    tags$img(src = "logo.svg", alt = "Net Zero Tracker logo", class = "nzt-logo"),
    tags$span(class = "nzt-app-name", "Target Change Tracker")
  ),
  window_title = "Target Change Tracker",
  theme = bs_theme(
    version = 5, bg = SURFACE, fg = INK, primary = RED, secondary = OCEAN,
    base_font    = font_collection("Franz Sans", "system-ui", "-apple-system", "Segoe UI", "sans-serif"),
    heading_font = font_collection("Franz Sans", "system-ui", "-apple-system", "Segoe UI", "sans-serif"),
    "link-color" = "#3385b7", "headings-color" = GRAPE
  ),
  fillable = FALSE,
  tags$head(tags$link(rel = "stylesheet", href = "zerotracker.css")),
  sidebar = sidebar(
    width = 330,
    tags$h6("1. Data"),
    actionButton("fetch", "Download latest export", icon = icon("cloud-arrow-down"),
                 class = "btn-ocean"),
    uiOutput("data_status"),
    tags$hr(class = "my-1"),
    tags$h6("2. Settings"),
    selectInput("start_month", "Analyse snapshots from",
                choices = c("Jan 2024" = "2024-01"), selected = "2024-01"),
    tags$small(class = "text-muted d-block mb-3", scope_note()),
    checkboxGroupInput("target_vars", "Tracked variables",
                       choiceNames = unname(VAR_LABELS), choiceValues = names(VAR_LABELS),
                       selected = names(VAR_LABELS)),
    actionButton("run", "Run tracker", icon = icon("play"), class = "btn-primary"),
    uiOutput("stale_note"),
    tags$hr(class = "my-1"),
    tags$h6("3. Download"),
    downloadButton("dl_changes", "Pre/post changes CSV", class = "btn-outline-grape")
  ),
  navset_card_underline(
    nav_panel(
      "Summary",
      uiOutput("summary_boxes"),
      card(card_header("Change events by variable"), uiOutput("plot_variable_ui")),
      card(card_header("Change events per month"),
           plotlyOutput("plot_monthly", height = "340px"))
    ),
    nav_panel(
      "Actor timeline",
      selectizeInput("entity", "Entity (entities with at least one change)", choices = NULL,
                     width = "100%", options = list(placeholder = "Type a name or id_code")),
      uiOutput("entity_header"),
      uiOutput("timeline_ui"),
      tags$h6(class = "mt-4", "Change events"),
      tableOutput("entity_events")
    )
  )
)

# ── Server ────────────────────────────────────────────────────

server <- function(input, output, session) {
  panel      <- reactiveVal(NULL)
  fetched_at <- reactiveVal(NULL)
  results    <- reactiveVal(NULL)

  current_settings <- reactive({
    start <- suppressWarnings(as.Date(paste0(input$start_month, "-01")))
    if (length(start) != 1 || is.na(start) || start < ANALYSIS_START) start <- ANALYSIS_START
    list(start_date  = start,
         actor_types = ANALYSIS_ACTOR_TYPES,
         target_vars = intersect(ALL_TARGET_VARS, input$target_vars))
  })

  set_panel <- function(p, when) {
    # Month dropdown: every month in the data from 2024 onward
    months <- sort(unique(floor_date(p$date, "month")))
    months <- months[months >= ANALYSIS_START]
    if (length(months)) {
      choices <- setNames(format(months, "%Y-%m"), format(months, "%b %Y"))
      sel     <- isolate(input$start_month)
      if (is.null(sel) || !sel %in% choices) sel <- unname(choices[1])
      updateSelectInput(session, "start_month", choices = choices, selected = sel)
    }
    fetched_at(when)
    panel(p)
  }

  if (file.exists(CACHE_FILE)) {
    cached <- tryCatch(readRDS(CACHE_FILE), error = function(e) NULL)
    if (!is.null(cached)) set_panel(cached$panel, cached$fetched_at)
  }

  observeEvent(input$fetch, {
    dir <- tempfile("nzt_")
    p <- withProgress(message = "Downloading ZeroTracker export…",
                      detail = "About 160 MB; this can take a minute.", value = 0.2, {
      tryCatch({
        csv <- download_export(dir)
        setProgress(0.7, message = "Reading and normalising…", detail = NULL)
        normalise_panel(read_export(csv))
      },
      error = function(e) {
        showNotification(paste("Download failed:", conditionMessage(e)),
                         type = "error", duration = NULL)
        NULL
      },
      finally = unlink(dir, recursive = TRUE))
    })
    req(p)
    when <- Sys.time()
    # The cache is a convenience, not a requirement: on a read-only deployment
    # the download still works, it just is not kept for the next session.
    tryCatch({
      dir.create(dirname(CACHE_FILE), showWarnings = FALSE)
      saveRDS(list(panel = p, fetched_at = when), CACHE_FILE)
    }, error = function(e) {
      showNotification(paste("Loaded, but could not update the cache:",
                             conditionMessage(e)), type = "warning", duration = 10)
    })
    set_panel(p, when)
    showNotification(sprintf("Loaded %s rows.", fmt_int(nrow(p))), type = "message")
  })

  run_tracker <- function() {
    p <- panel()
    req(p)
    s <- isolate(current_settings())
    res <- withProgress(message = "Detecting target changes…", value = 0.5, {
      tryCatch(compute_changes(p, s$start_date, s$actor_types, s$target_vars),
               error = function(e) {
                 showNotification(conditionMessage(e), type = "error", duration = 10)
                 NULL
               })
    })
    results(res)
    req(res)

    ents <- res$events %>%
      group_by(id_code) %>%
      summarise(name = coalesce(first(name), first(id_code)), n = n(), .groups = "drop") %>%
      arrange(desc(n), name)
    updateSelectizeInput(session, "entity", server = TRUE,
                         choices = setNames(ents$id_code,
                                            sprintf("%s (%s) · %d change%s", ents$name, ents$id_code,
                                                    ents$n, ifelse(ents$n == 1, "", "s"))),
                         selected = if (nrow(ents)) ents$id_code[1] else character(0))
  }

  observeEvent(input$run, run_tracker())
  observeEvent(panel(), run_tracker())

  # ── Sidebar status ──

  output$data_status <- renderUI({
    p <- panel()
    if (is.null(p)) {
      return(tags$small(class = "text-muted",
                        "No data loaded yet. The export is about 160 MB."))
    }
    tags$small(class = "text-muted",
               sprintf("%s rows · %s entities", fmt_int(nrow(p)), fmt_int(n_distinct(p$id_code))),
               tags$br(),
               sprintf("Snapshots %s to %s", format(min(p$date)), format(max(p$date))),
               tags$br(),
               sprintf("Downloaded %s", format(fetched_at(), "%d %b %Y, %H:%M")))
  })

  output$stale_note <- renderUI({
    res <- results()
    req(res)
    chosen <- c("start_date", "actor_types", "target_vars")
    if (identical(current_settings()[chosen], res$settings[chosen])) return(NULL)
    tags$small(class = "text-warning-emphasis",
               icon("circle-exclamation"), " Settings changed. Click Run tracker to update.")
  })

  # ── Summary ──

  need_results <- function() {
    validate(need(panel(), "Download the export to get started."))
    res <- results()
    validate(need(res, "Run the tracker to see results."))
    res
  }

  output$summary_boxes <- renderUI({
    res <- need_results()
    s <- res$settings
    layout_columns(
      fill = FALSE,
      nzt_value_box("Change events", fmt_int(nrow(res$events)), showcase = icon("arrow-right-arrow-left")),
      nzt_value_box("Entities with changes", fmt_int(res$n_entities), showcase = icon("building")),
      nzt_value_box("Rows in pre/post CSV", fmt_int(nrow(res$target_changes)), showcase = icon("table")),
      nzt_value_box("Scope", scope_label(),
                    sprintf("%s from %s · %d of %d variables",
                            if (ANALYSIS_MONTHLY) "Monthly" else "Per snapshot",
                            format(s$start_date, "%b %Y"),
                            length(s$target_vars), length(ALL_TARGET_VARS)),
                    showcase = icon("filter"))
    )
  })

  bar_output <- function(id, n) plotlyOutput(id, height = paste0(56 + 34 * max(n, 1), "px"))

  hbar <- function(labels, values, colors, hover_label = "change events") {
    o <- order(values)
    plot_ly(x = values[o], y = factor(labels[o], levels = labels[o]), type = "bar",
            orientation = "h", marker = list(color = colors[o]),
            text = fmt_int(values[o]), textposition = "outside", cliponaxis = FALSE,
            textfont = list(color = INK_2, size = 11),
            hovertemplate = paste0("%{y}<br>%{x:,} ", hover_label, "<extra></extra>")) %>%
      chart_theme(x = list(title = "", showgrid = TRUE, rangemode = "tozero"),
                  y = list(title = "", showgrid = FALSE),
                  bargap = 0.4, showlegend = FALSE)
  }

  output$plot_variable_ui <- renderUI({
    res <- need_results()
    validate(need(nrow(res$events) > 0, "No change events for these settings."))
    bar_output("plot_variable", nrow(res$by_variable))
  })

  output$plot_variable <- renderPlotly({
    res <- results()
    req(res, nrow(res$events) > 0)
    df <- res$by_variable
    hbar(unname(VAR_LABELS[df$variable]), df$change_events, rep(MIDNIGHT, nrow(df)))
  })

  output$plot_monthly <- renderPlotly({
    res <- need_results()
    validate(need(nrow(res$events) > 0, "No change events for these settings."))
    df <- res$events %>% mutate(month = floor_date(date, "month")) %>% count(month)

    plot_ly(df, x = ~month, y = ~n, type = "bar",
            marker = list(color = MIDNIGHT, line = list(color = SURFACE, width = 1)),
            hovertemplate = "%{y:,} change events<extra></extra>") %>%
      chart_theme(x = list(title = "", showgrid = FALSE, xhoverformat = "%b %Y"),
                  y = list(title = "Change events", rangemode = "tozero"),
                  bargap = 0.5, hovermode = "x unified", showlegend = FALSE)
  })

  # ── Actor timeline ──

  entity_data <- reactive({
    res <- need_results()
    validate(need(nrow(res$events) > 0, "No change events for these settings."),
             need(input$entity, "Pick an entity."))
    id <- input$entity
    list(res = res,
         st  = res$states %>% filter(id_code == id) %>% arrange(date),
         ev  = res$events %>% filter(id_code == id) %>% arrange(date),
         end = res$last_seen$last_seen[res$last_seen$id_code == id])
  })

  output$entity_header <- renderUI({
    d <- entity_data()
    last <- tail(d$st, 1)
    tags$div(
      class = "mb-2",
      tags$h5(class = "mb-0", coalesce(last$name, last$id_code)),
      tags$small(class = "text-muted",
                 paste(na.omit(c(last$id_code, last$actor_type, last$country)), collapse = " · "),
                 sprintf(" · %d change event%s · tracked %s to %s", nrow(d$ev),
                         ifelse(nrow(d$ev) == 1, "", "s"),
                         format(min(d$st$date)), format(d$end))),
      tags$div(
        class = "small mt-1", style = paste0("color:", INK_2),
        "Click a variable in the legend to show or hide its line; double-click to show only that one. ",
        "Filled dots mark changes (hover for before → after); hollow dots mark the first snapshot; ",
        "dotted stretches have no value."
      )
    )
  })

  output$timeline_ui <- renderUI({
    d <- entity_data()
    plotlyOutput("timeline", height = paste0(140 + 62 * length(d$res$settings$target_vars), "px"))
  })

  output$timeline <- renderPlotly({
    d     <- entity_data()
    vars  <- d$res$settings$target_vars
    lanes <- unname(VAR_LABELS[vars])
    x_end <- d$end
    span  <- max(as.numeric(x_end - min(d$st$date)), 30)

    p <- plot_ly()
    for (v in vars) {
      lane <- VAR_LABELS[[v]]
      col  <- VAR_COLORS[[v]]
      vals <- d$st[[v]]

      # Runs of an unchanged value for this variable
      is_start <- c(TRUE, changed(vals[-1], vals[-length(vals)]))
      run_x    <- d$st$date[is_start]
      run_v    <- vals[is_start]
      run_end  <- c(run_x[-1], x_end)
      seg <- function(keep) {
        idx <- which(keep)
        list(x = as.Date(unlist(lapply(idx, function(i) c(run_x[i], run_end[i], NA)))),
             y = rep(c(lane, lane, NA), length(idx)))
      }

      # Legend carrier: one solid line per variable; blank runs leave a gap
      solid <- list(x = as.Date(unlist(lapply(seq_along(run_x), function(i) c(run_x[i], run_end[i], NA)))),
                    y = unlist(lapply(seq_along(run_x), function(i) {
                      if (is.na(run_v[i])) c(NA, NA, NA) else c(lane, lane, NA)
                    })))
      p <- p %>% add_trace(x = solid$x, y = solid$y, type = "scatter", mode = "lines",
                           name = lane, legendgroup = v, showlegend = TRUE,
                           line = list(color = col, width = 3), hoverinfo = "skip")

      blank <- seg(is.na(run_v))
      if (length(blank$x)) {
        p <- p %>% add_trace(x = blank$x, y = blank$y, type = "scatter", mode = "lines",
                             name = lane, legendgroup = v, showlegend = FALSE,
                             line = list(color = col, width = 1.5, dash = "dot"), opacity = 0.6,
                             hoverinfo = "skip")
      }

      # First snapshot in range
      p <- p %>% add_trace(x = run_x[1], y = lane, type = "scatter", mode = "markers+text",
                           name = lane, legendgroup = v, showlegend = FALSE,
                           text = fmt_value(run_v[1]), textposition = "top right",
                           textfont = list(color = INK_2, size = 11, family = FONT),
                           marker = list(color = SURFACE, size = 9, line = list(color = col, width = 2)),
                           hovertext = sprintf("%s · %s<br>%s (first snapshot)",
                                               format(run_x[1], "%d %b %Y"), lane, fmt_value(run_v[1])),
                           hovertemplate = "%{hovertext}<extra></extra>", cliponaxis = FALSE)

      # Changes
      hit <- d$ev[[paste0("changed_", v)]]
      if (any(hit)) {
        cx <- d$ev$date[hit]
        new_v  <- fmt_value(d$ev[[v]][hit])
        prev_v <- fmt_value(d$ev[[paste0("prev_", v)]][hit])
        p <- p %>% add_trace(x = cx, y = rep(lane, length(cx)), type = "scatter", mode = "markers+text",
                             name = lane, legendgroup = v, showlegend = FALSE,
                             text = new_v, textposition = "top right",
                             textfont = list(color = INK_2, size = 11, family = FONT),
                             marker = list(color = col, size = 12, line = list(color = SURFACE, width = 2)),
                             hovertext = sprintf("%s · %s<br>%s → %s", format(cx, "%d %b %Y"),
                                                 lane, prev_v, new_v),
                             hovertemplate = "%{hovertext}<extra></extra>", cliponaxis = FALSE)
      }
    }

    all_on  <- list(list(visible = TRUE))
    all_off <- list(list(visible = "legendonly"))

    p %>%
      chart_theme(
        x = list(title = "", type = "date", showgrid = TRUE,
                 range = c(min(d$st$date) - span * 0.02, x_end + span * 0.12)),
        y = list(title = "", type = "category", categoryorder = "array",
                 categoryarray = rev(lanes), showgrid = FALSE, fixedrange = TRUE,
                 tickfont = list(color = GRAPE, size = 12)),
        showlegend = TRUE, hovermode = "closest",
        legend = list(orientation = "h", x = 0, y = 1.02, yanchor = "bottom",
                      font = list(color = INK_2, size = 12),
                      itemclick = "toggle", itemdoubleclick = "toggleothers"),
        updatemenus = list(list(
          type = "buttons", direction = "left", x = 1, xanchor = "right", y = 1.02, yanchor = "bottom",
          showactive = FALSE, bgcolor = "#f9f9f9", bordercolor = AXIS,
          font = list(color = GRAPE, size = 11),
          buttons = list(list(label = "Show all", method = "restyle", args = all_on),
                         list(label = "Hide all", method = "restyle", args = all_off))
        )),
        margin = list(l = 8, r = 16, t = 72, b = 8)
      )
  })

  output$entity_events <- renderTable({
    d <- entity_data()
    d$ev %>% transmute(`Change date` = format(date),
                       `Previous snapshot` = format(prev_date),
                       `What changed` = gsub("; ", "\n", change_detail, fixed = TRUE))
  }, striped = TRUE, width = "100%", sanitize.text.function = function(x) gsub("\n", "<br>", htmltools::htmlEscape(x)))

  # ── Download ──

  file_tag <- function(res) {
    types <- res$settings$actor_types
    scope <- if (all(is.na(types))) "all" else paste(tolower(gsub("[^A-Za-z]+", "", types)), collapse = "-")
    paste0(scope, "_", format(Sys.Date()))
  }

  output$dl_changes <- downloadHandler(
    filename = function() paste0("target_changes_prepost_NZT_", file_tag(results()), ".csv"),
    content  = function(file) {
      res <- results()
      validate(need(res, "Run the tracker first."))
      readr::write_csv(res$target_changes, file)
    }
  )
}

shinyApp(ui, server)
