# Copyright 2021 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

well_report <- function(ows, report_dates = Sys.Date(), within = 7,
                        clean_cache = TRUE) {

  report_dates <- dates_check(report_dates)

  # Remove cache files not from today
  if(clean_cache) cache_clean()

  data_update()

  # Format obs wells just in case
  ows <- toupper(ows)
  ows <- str_trim(ows)
  if(any(o <- !str_detect(ows, "OW[0-9]{3}"))) {
    stop(glue("Some obs wells are not valid IDs (OW000): ",
              {glue_collapse(ows[o], sep = ',')}),
         call. = FALSE)
  }

  message(glue("- Fetching/cleaning obs well data ({length(ows)} wells)"))
  w_full <- well_prep(ows, water_year_start = 10, report_dates)
  w_dates <- well_dates(w_full, report_dates, within)
  message("- Summarizing historical statistics")
  w_hist <- well_hist(w_full)
  w_comp <- well_hist_compare(w_dates, w_full)
  message("- Summarizing percentiles")
  w_perc <- well_percentiles(w_comp)

  message("- Writing report")
  render("report.Rmd",
         params = list("w_full" = w_full, "w_hist" = w_hist,
                       "w_comp" = w_hist, "w_perc" = w_perc,
                       "w_dates" = w_dates, "report_dates" = report_dates,
                       "within" = within),
         output_dir = "reports", output_file = glue("report_{Sys.Date()}.pdf"))
}

dates_check <- function(report_dates) {

  report_dates <- suppressWarnings(as_date(report_dates))
  if(any(is.na(report_dates))) {
    stop("report_dates must be valid dates YYYY-MM-DD", call. = FALSE)
  } else if (any(report_dates > Sys.Date())) {
    stop("Cannot calculate reports for future dates", call. = FALSE)
  }

  sort(c(report_dates, report_dates - years(1)), decreasing = TRUE)
}


well_prep <- function(ows, water_year_start, report_dates) {
  w <- well_dl(ows) %>%
    mutate(data = map(data, well_clean,
                      water_year_start = water_year_start,
                      report_dates = report_dates)) %>%
    unnest(data) %>%
    select(-file)

  wy <- unique(w$WaterYear[w$Date == max(report_dates)])

  mutate(w, CurrentYear = WaterYear == wy)
}

wy_calc <- function(report_dates, water_year_start) {
  d <- max(report_dates)
  wy <- as_date(glue("{c(year(d) - 1, year(d))}-{water_year_start}-01"))
  if(month(d) >= water_year_start) wy <- wy + years(1)
 wy
}

well_dl <- function(ows) {
  tibble(ow = ows,
         file_url = glue("http://www.env.gov.bc.ca/wsd/data_searches/",
                     "obswell/map/data/{ow}-data.csv")) %>%
    left_join(cache_load(ows), by = "ow") %>%
    mutate(file = if_else(!is.na(file_local), as_glue(file_local), file_url)) %>%
    select(-file_local, -file_url, -date) %>%
    mutate(data = map2(file, ow, well_read))
}

well_read <- function(file, ow) {
  message(glue("   - {ow} ({if_else(str_detect(file, 'http'), 'online', 'local')})"))
  d <- read_csv(file, col_types = "Tncc", progress = FALSE)
  write_csv(d, glue("cache/{ow}_{Sys.Date()}.csv"))
  d
}

cache_load <- function(ows = NULL) {
  cache <- tibble(file_local = list.files("cache", full.names = TRUE),
                  ow = str_extract(basename(file_local), "OW[0-9]{3}"),
                  date = str_extract(basename(file_local), "[0-9]{4}-[0-9]{2}-[0-9]{2}")) %>%
    mutate(date = as_date(date))
  if(!is.null(ows)) cache <- filter(cache, ow %in% !!ows, date == Sys.Date())
  cache
}

cache_clean <- function() {
  cache_load() %>%
    filter(date != Sys.Date()) %>%
    pull(file_local) %>%
    unlink()
}

well_clean <- function(w, water_year_start, report_dates) {
  w %>%
    select(-myLocation) %>%
    mutate(Date = as_date(Time)) %>%
    group_by(Date) %>%
    # makes a day "Working" if any hour is "Working"
    mutate(Approval = case_when(any(Approval == "Working") ~ "Working",
                                TRUE ~ "Approved")) %>%
    # Create daily averages, keeping dates and approvals
    group_by(Date, Approval) %>%
    summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
    find_continuous() %>%
    complete(Date = report_dates) %>%
    # Fill in dates with missing values with NA and add various date columns
    fill_missing_dates(water_year_start = water_year_start) %>%
    add_date_variables(water_year_start = water_year_start) %>%
    # If filled with NA, make "Working" and categorize data for historic/recent
    mutate(Approval = if_else(is.na(Approval), "Working", Approval),
           water_year_start = water_year_start) %>%
    filter(DayofYear != 366)
}

find_continuous <- function(w) {
  first_date <- w %>%
    mutate(month = floor_date(Date, "month")) %>%
    group_by(month) %>%
    summarize(n = n()) %>%
    filter(n > 25) %>%
    slice(1) %>%
    pull(month)

  filter(w, Date > first_date)
}


well_meta <- function(w) {
  w %>%
    left_join(well_regions(ows), by = "ow") %>%
    left_join(data_load("aquifers"), by = c("aquifer_id", "ow"))
}

well_dates <- function(w_full, report_dates, within) {

  r <- tibble(report_dates = report_dates) %>%
    mutate(Date = map(report_dates, ~seq(. - days(within),
                                         . + days(within),
                                         by = "1 day"))) %>%
    unnest(Date)

  w_full %>%
    right_join(r, by = "Date") %>%
    group_by(ow, report_dates) %>%
    arrange(abs(Date - report_dates)) %>%
    mutate(keep = if_else(all(is.na(Value)), Date[1], Date[!is.na(Value)][1])) %>%
    filter(Date == keep) %>%
    ungroup() %>%
    select(-keep)
}

well_hist <- function(w_full) {
  w_full %>%
    filter(Approval == "Approved", !is.na(Value), !CurrentYear) %>%
    group_by(ow, DayofYear) %>%
    summarize(min = min(Value, na.rm = TRUE),
              max = max(Value, na.rm = TRUE),
              median = median(Value, na.rm = TRUE),
              mean = mean(Value, na.rm = TRUE),
              n_years = length(unique(WaterYear)),
              start_year = min(WaterYear),
              end_year = max(WaterYear),
              v = list(Value),
              p = list(ecdf(Value)), .groups = "drop")
}

well_hist_compare <- function(w_dates, w_full) {
  w_dates %>%
    left_join(well_hist(w_full), by = c("ow", "DayofYear")) %>%
    mutate(percentile = map2_dbl(p, Value, ~.x(.y)))
}

perc_match <- function(x, cols = "class") {
  perc_values[[cols]][x >= perc_values$low][sum(x >= perc_values$low)]
}

well_percentiles <- function(w_comp) {
  w_comp %>%
    well_meta() %>%
    select(ow, Date, report_dates, percentile, type) %>%
    filter(!is.na(percentile)) %>%
    mutate(class = map_chr(percentile, perc_match, cols = "nice")) %>%
    select(-percentile) %>%
    mutate(window = Date != report_dates) %>%
    group_by(report_dates, class, type) %>%
    summarize(n_class_type = n(),
              window = any(window),
              .groups = "drop") %>%
    complete(report_dates = unique(w_comp$report_dates), type = unique(type_values$type),
             class = perc_values$nice, fill = list(n_class_type = 0)) %>%
    group_by(report_dates, type) %>%
    mutate(n_total_type = sum(n_class_type)) %>%
    group_by(report_dates, class) %>%
    mutate(n_total_class = sum(n_class_type)) %>%
    ungroup() %>%
    group_by(report_dates) %>%
    mutate(n_total_date = sum(n_class_type),
           window = any(window, na.rm = TRUE)) %>%
    ungroup()
}

well_quantiles <- function(values) {
  perc_values %>%
    mutate(q_low = quantile(values, perc_values$low),
           q_high = quantile(values, perc_values$high)) %>%
    select(nice, q_low, q_high) %>%
    mutate(nice = factor(nice, levels = perc_values$nice))
}


well_regions <- function(ows) {
  data_load("wells_sf") %>%
    filter(ow %in% ows) %>%
    st_join(data_load("regions")) %>%
    st_drop_geometry() %>%
    mutate(area_name = if_else(area_name == "Coast Natural Resource Area",
                               region_name, area_name)) %>%
    select(-region_name)
}
