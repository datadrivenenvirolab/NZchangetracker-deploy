# ──────────────────────────────────────────────────────────────
# Core logic of target_change_tracker.R, split into functions so the
# Shiny app can download once and re-run with different settings.
#
# Each step mirrors the script; tests/test_parity.R checks that the
# pre/post CSV is byte-identical to the script's output.
# ──────────────────────────────────────────────────────────────

library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)

# ══════════════════════════════════════════════════════════════
# ANALYSIS RULES — edit this block to change what is analysed.
#
# Everything the analysis depends on is defined here: the data source, the
# scope (which actor types, from when, at what time step), the variables whose
# changes count, and what counts as a blank. Nothing below this block, and
# nothing in app.R, hard-codes any of it.
# ══════════════════════════════════════════════════════════════

# Where the export is downloaded from. Update if the ZeroTracker link changes.
DATA_URL <- "https://app.zerotracker.net/download/4762384a-a57a-42dc-bcdf-3bdebe10e11a"

# Which actor types are analysed: e.g. "Company", c("Company", "Country"),
# or NA for every actor type in the export.
ANALYSIS_ACTOR_TYPES <- "Company"

# Earliest snapshot analysed. This month is the first baseline, so the first
# change can be detected in the month after it.
ANALYSIS_START <- as.Date("2024-01-01")

# TRUE keeps ANALYSIS_START itself. FALSE drops it, matching the original
# script's `date > START_DATE`; the parity tests pass FALSE explicitly.
ANALYSIS_INCLUSIVE_START <- TRUE

# TRUE collapses snapshots to one per calendar month (the latest in each), so
# changes are always measured month over month. FALSE compares every snapshot.
ANALYSIS_MONTHLY <- TRUE

# The variables whose changes are tracked, with the label shown in the app.
# Add or remove lines to change what counts as a change. The order here sets
# the order of the checkboxes, the chart and the timeline lanes.
TARGET_VAR_LABELS <- c(
  end_target                          = "End target",
  end_target_percentage_reduction     = "End target % reduction",
  end_target_baseline_year            = "End target baseline year",
  end_target_year                     = "End target year",
  interim_target                      = "Interim target",
  interim_target_percentage_reduction = "Interim target % reduction",
  interim_target_baseline_year        = "Interim target baseline year",
  interim_target_year                 = "Interim target year"
)

# Which of the tracked variables hold numbers; the rest are treated as text.
# Numbers are compared as numbers, so "2050" and "2050.0" are not a change.
NUMERIC_TARGET_VARS <- c(
  "end_target_percentage_reduction", "end_target_baseline_year", "end_target_year",
  "interim_target_percentage_reduction", "interim_target_baseline_year", "interim_target_year"
)

# Cell values that mean "no value". A value appearing or disappearing counts
# as a change, so this list decides which cells are blank in the first place.
BLANK_VALUES <- c("", "NA", "N/A", "-")

# Identifier columns carried through to the output.
ID_VARS <- c("id_code", "name", "actor_type", "country")

# ══════════════════════════════════════════════════════════════
# Derived from the block above — no need to edit these.
# ══════════════════════════════════════════════════════════════

ALL_TARGET_VARS <- names(TARGET_VAR_LABELS)
ALL_NUM_VARS    <- intersect(ALL_TARGET_VARS, NUMERIC_TARGET_VARS)
ALL_CHR_VARS    <- setdiff(ALL_TARGET_VARS, ALL_NUM_VARS)

# ── 1. Download & read data ───────────────────────────────────

# Downloads into dest_dir and returns the path of the CSV to read.
# The caller owns dest_dir and should delete it once the CSV is read.
download_export <- function(dest_dir, url = DATA_URL) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  old <- options(timeout = 600)
  on.exit(options(old))

  tmp <- file.path(dest_dir, "download")
  download.file(url, destfile = tmp, mode = "wb", quiet = TRUE)

  # Detect ZIP vs plain CSV by magic bytes
  magic <- readBin(tmp, "raw", n = 2)
  if (!identical(magic, as.raw(c(0x50, 0x4b)))) return(tmp)

  unzip(tmp, exdir = dest_dir)
  csv_path <- list.files(dest_dir, pattern = "\\.csv$",
                         full.names = TRUE, recursive = TRUE)[1]
  if (is.na(csv_path)) stop("The downloaded ZIP holds no CSV file.")
  csv_path
}

read_export <- function(csv_path) {
  # Read the header only, so we can pull just the columns we need
  hdr    <- names(read_csv(csv_path, n_max = 0, show_col_types = FALSE))
  lookup <- setNames(hdr, tolower(hdr))

  # The export has shipped with both Entity_type and actor_type over time
  if (!"actor_type" %in% names(lookup) && "entity_type" %in% names(lookup)) {
    lookup["actor_type"] <- lookup["entity_type"]
  }

  wanted  <- c("date", ID_VARS, ALL_TARGET_VARS)
  missing <- setdiff(wanted, names(lookup))
  if (length(missing)) {
    stop("Columns absent from the download: ", paste(missing, collapse = ", "))
  }

  # Everything read as text and coerced below, so that a snapshot where readr
  # happens to guess a different column type cannot masquerade as a change.
  raw <- read_csv(
    csv_path,
    col_select     = all_of(unname(lookup[wanted])),
    col_types      = cols(.default = col_character()),
    show_col_types = FALSE
  )
  names(raw) <- wanted
  raw
}

# ── 2. Normalise ──────────────────────────────────────────────

blank_to_na <- function(x) {
  x <- str_squish(x)
  ifelse(x %in% BLANK_VALUES, NA_character_, x)
}

normalise_panel <- function(raw) {
  raw %>%
    mutate(date = ymd(str_sub(date, 1, 10))) %>%
    filter(!is.na(date), !is.na(id_code)) %>%
    mutate(across(all_of(c(ID_VARS, ALL_CHR_VARS)), blank_to_na)) %>%
    mutate(across(all_of(ALL_NUM_VARS), ~ suppressWarnings(as.numeric(blank_to_na(.x)))))
}

# ── 3–5. Detect changes ───────────────────────────────────────

# NA-safe: a value appearing or disappearing counts as a change
changed <- function(new, old) {
  (is.na(new) != is.na(old)) | (!is.na(new) & !is.na(old) & new != old)
}

fmt <- function(x) ifelse(is.na(x), "NA", as.character(x))

# Joins, per row, the pieces whose flag is set, in column order.
# Equivalent to apply(flags, 1, function(r) paste(pieces[r], collapse = sep)).
paste_flagged <- function(flags, pieces, sep) {
  out <- rep(NA_character_, nrow(flags))
  for (j in seq_len(ncol(flags))) {
    hit   <- flags[, j]
    piece <- rep_len(pieces[[j]], nrow(flags))[hit]
    out[hit] <- ifelse(is.na(out[hit]), piece, paste(out[hit], piece, sep = sep))
  }
  out
}

# Every argument defaults to the ANALYSIS RULES block at the top of this file;
# pass a value only to override the rules for one call (the tests do this).
compute_changes <- function(panel,
                            start_date      = ANALYSIS_START,
                            actor_types     = ANALYSIS_ACTOR_TYPES,
                            target_vars     = ALL_TARGET_VARS,
                            inclusive_start = ANALYSIS_INCLUSIVE_START,
                            monthly         = ANALYSIS_MONTHLY) {
  target_vars <- intersect(ALL_TARGET_VARS, target_vars)
  if (!length(target_vars)) stop("Select at least one variable to track.")
  if (!length(actor_types)) actor_types <- NA

  if (!is.na(start_date)) {
    panel <- if (inclusive_start) panel %>% filter(date >= start_date)
             else                 panel %>% filter(date >  start_date)
  }

  if (!all(is.na(actor_types))) {
    panel <- panel %>% filter(str_to_lower(actor_type) %in% str_to_lower(actor_types))
    if (nrow(panel) == 0) stop("No rows left after filtering to: ", paste(actor_types, collapse = ", "))
  }

  panel <- panel %>%
    select(date, all_of(ID_VARS), all_of(target_vars)) %>%
    distinct()

  # One target state per entity per snapshot; keep the first if conflicting
  dupes <- panel %>% count(id_code, date) %>% filter(n > 1) %>% nrow()
  if (dupes > 0) {
    panel <- panel %>% arrange(id_code, date) %>% distinct(id_code, date, .keep_all = TRUE)
  }

  # One snapshot per calendar month: keep the latest within each month
  collapsed <- 0L
  if (monthly && nrow(panel)) {
    n_before <- nrow(panel)
    panel <- panel %>%
      arrange(id_code, date) %>%
      mutate(.month = floor_date(date, "month")) %>%
      group_by(id_code, .month) %>%
      slice_tail(n = 1) %>%
      ungroup() %>%
      mutate(date = .month) %>%
      select(-.month)
    collapsed <- n_before - nrow(panel)
  }

  last_seen <- panel %>%
    group_by(id_code) %>%
    summarise(last_seen = if (n()) max(date) else as.Date(NA), .groups = "drop")

  # Collapse to distinct target states. Only consecutive repeats are dropped,
  # so a change that is later reverted keeps both events.
  states <- panel %>%
    arrange(id_code, date) %>%
    mutate(.state = do.call(paste, c(across(all_of(target_vars),
                                            ~ ifelse(is.na(.x), "<NA>", as.character(.x))),
                                     sep = ""))) %>%
    group_by(id_code) %>%
    filter(row_number() == 1 | .state != lag(.state)) %>%
    ungroup() %>%
    select(-.state)

  # Diff consecutive states
  events <- states %>%
    arrange(id_code, date) %>%
    group_by(id_code) %>%
    mutate(prev_date = lag(date),
           across(all_of(target_vars), ~ lag(.x), .names = "prev_{.col}")) %>%
    ungroup() %>%
    filter(!is.na(prev_date))

  flags <- matrix(FALSE, nrow = nrow(events), ncol = length(target_vars),
                  dimnames = list(NULL, target_vars))
  for (v in target_vars) {
    flags[, v] <- changed(events[[v]], events[[paste0("prev_", v)]])
  }

  keep   <- rowSums(flags) > 0
  events <- events[keep, , drop = FALSE]
  flags  <- flags[keep, , drop = FALSE]

  details <- lapply(target_vars, function(v) {
    sprintf("%s: %s -> %s", v, fmt(events[[paste0("prev_", v)]]), fmt(events[[v]]))
  })

  events <- events %>%
    mutate(
      n_changed       = rowSums(flags),
      changed_columns = paste_flagged(flags, as.list(target_vars), "; "),
      change_summary  = sprintf(
        "For %s, changes observed in %s",
        coalesce(name, id_code),
        paste_flagged(flags, as.list(target_vars), ", ")
      ),
      change_detail   = paste_flagged(flags, details, "; ")
    ) %>%
    bind_cols(as_tibble(flags, .name_repair = ~ paste0("changed_", .x))) %>%
    arrange(id_code, date) %>%
    mutate(change_id = row_number())

  # Emit one row per state, two rows per change
  pre <- events %>%
    transmute(change_id,
              change_state = "pre",
              change_date  = date,
              date         = prev_date,
              id_code, name, actor_type, country,
              across(all_of(paste0("prev_", target_vars)),
                     .names = "{str_remove(.col, '^prev_')}"),
              n_changed, changed_columns, change_summary, change_detail)

  post <- events %>%
    transmute(change_id,
              change_state = "post",
              change_date  = date,
              date,
              id_code, name, actor_type, country,
              across(all_of(target_vars)),
              n_changed, changed_columns, change_summary, change_detail)

  target_changes <- bind_rows(pre, post) %>%
    arrange(change_id, match(change_state, c("pre", "post"))) %>%
    select(change_id, change_state, change_date, date,
           id_code, name, actor_type, country,
           all_of(target_vars),
           n_changed, changed_columns, change_summary, change_detail)

  list(
    target_changes = target_changes,
    events         = events,
    states         = states,
    last_seen      = last_seen,
    by_actor_type  = events %>% count(actor_type, name = "change_events") %>% arrange(desc(change_events)),
    by_variable    = tibble(variable = target_vars, change_events = colSums(flags)) %>%
                       arrange(desc(change_events)),
    n_entities     = n_distinct(events$id_code),
    dupes          = dupes,
    collapsed      = collapsed,
    settings       = list(start_date = start_date, actor_types = actor_types,
                          target_vars = target_vars,
                          inclusive_start = inclusive_start, monthly = monthly)
  )
}
