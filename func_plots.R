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


well_plots_base <- function(title = "", legend = "right", caption = NA) {
  p_values <- filter(perc_values, !class %in% c("p_max", "p_min"))
  g <- ggplot() +
    theme_bw() +
    theme(legend.title = element_blank(), legend.position = legend,
          legend.margin = margin(),
          legend.box.spacing = unit(5, "pt"),
          legend.spacing = unit(1, "pt")) +
    scale_y_reverse() +
    scale_x_date(expand = c(0, 0)) +
    scale_size_manual(values = setNames(plot_values$size, plot_values$type)) +
    scale_colour_manual(values = setNames(plot_values$colour, plot_values$type)) +
    scale_fill_manual(values = setNames(p_values$colour, p_values$nice)) +
    labs(x = "Day of Year", y = "DBG (m)", subtitle = title)

  if(!is.na(caption)) g <- g + labs(caption = caption)
  g
}

well_plot_perc <- function(full, hist, latest_date = NULL, legend = "right") {

  year <- full %>%
    filter(!is.na(Value)) %>%
    count(WaterYear) %>%
    filter(n >= 7) %>%   # Must have at least one week of data
    slice(n()) %>%
    pull(WaterYear)

  recent <- filter(full, WaterYear == year)

  if(nrow(recent) > 0) {
    origin <- recent$Date[recent$DayofYear == 1] - days(1)
    hist <- mutate(hist,
                   Date = as_date(DayofYear, origin = origin),
                   Approval = "Median",
                   q = map(v, well_quantiles, minmax = FALSE)) %>%
      unnest(q)

    data <- select(recent, Date, Approval, Value) %>%
      bind_rows(select(hist, Date, Approval, Value = median)) %>%
      mutate(Approval = factor(Approval,
                               levels = c("Median", "Approved", "Working")))

    title <- glue("Water Year {glue_collapse(unique(recent$CalendarYear), ' - ')}")
    range_name <- glue("Historical range of min & max ({hist$start_year[1]} - ",
                       "{hist$end_year[1]})")

    caption <- if_else(nrow(hist) > 0,
                       "Not enough non-missing data to calculate percentiles",
                       NA_character_)

    p <- well_plots_base(title = title, legend = legend, caption = caption)

    if(nrow(hist) > 0) {
      p <- p +
        geom_ribbon(data = hist, alpha = 0.35,
                    aes(x = Date, ymin = q_low, ymax = q_high, fill = nice))
    }
    p <- p +
      geom_line(data = data, aes(x = Date, y = Value, colour = Approval,
                                 size = Approval), na.rm = TRUE)

    if(!is.null(latest_date) &&
       nrow(latest_date) > 0 &&
       latest_date$Date %in% data$Date) {
      p <-  p +
        geom_point(data = latest_date, size = 4,
                   aes(x = Date, y = Value, shape = "Latest Date")) +
        guides(shape = guide_legend(override.aes = list(colour = "black",
                                                        fill = "black",
                                                        shape = 21)))
    }
  p

  } else {
    well_plots_base() +
      theme(axis.text = element_blank(), axis.ticks = element_blank()) +
      annotate(geom = "text", x = Sys.Date(), y = 1, label = "No Data")
  }
}

well_plot_hist <- function(full, hist, date_range, latest_date = NULL,
                           legend = "right", wrap_year = FALSE) {

  data <- full %>%
    select(Date, Approval, Value) %>%
    filter(Date >= date_range[1], Date < date_range[2]) %>%
    complete(Date = seq(date_range[1], date_range[2]-1, "1 day"),
             Approval = c("Working", "Approved"),
             fill = list(Value = NA)) %>%
    add_date_variables(water_year_start = full$water_year_start[1]) %>%
    left_join(select(hist, DayofYear, Median = median), by = "DayofYear") %>%
    pivot_wider(names_from = Approval, values_from = Value) %>%
    pivot_longer(cols = c(Approved, Working, Median),
                 names_to = "Approval", values_to = "Value") %>%
    mutate(Approval = fct_relevel(Approval, "Median"))

  yr_bin <- unique(data$WaterYear)
  yr_bin <- yr_bin[c(1, length(yr_bin)/2, length(yr_bin))]

  data <- mutate(data, yr_bin =
                   if_else(WaterYear <= yr_bin[2],
                           glue("Water Year {yr_bin[1]} - {yr_bin[2]}"),
                           glue("Water Year {yr_bin[2]+1} - {yr_bin[3]}")))

  p <- well_plots_base(title = "Historical Record", legend = legend) +
    geom_line(data = data,
              aes(x = Date, y = Value, colour = Approval, size = Approval),
              na.rm = TRUE) +
    facet_wrap(~ yr_bin, ncol = 1, scales = "free_x")

  if(!is.null(latest_date) &&
     nrow(latest_date) > 0 &&
     latest_date$Date %in% data$Date
     && latest_date$CurrentYear) {
    latest_date$yr_bin <- data$yr_bin[nrow(data)]
    p <- p +
      geom_point(data = latest_date, size = 4,
                 aes(x = Date, y = Value, shape = "Latest Date")) +
      guides(shape = guide_legend(override.aes = list(colour = "black",
                                                      fill = "black",
                                                      shape = 21)))
  }

  p
}


well_table_overview <- function(w_dates) {
  w_dates %>%
    well_meta() %>%
    mutate(ow = ow_link(ow),
           area_name = str_remove_all(area_name, "( Natural Resource )|(Region)|(Area)"),
           district_name = str_remove(district_name, " Natural Resource District"),
           Value = as.character(round(Value, 2)),
           Value = if_else(Date != report_dates & !is.na(Value),
                           as.character(glue("{Value}*")),
                           Value)) %>%
    arrange(area_name, district_name, ow, desc(Date)) %>%
    select(area_name, district_name, ow, Value, report_dates) %>%
    pivot_wider(names_from = "report_dates", values_from = "Value") %>%
    rename(Region = area_name, `Geographic\nArea` = district_name, `Obs.\nWell` = ow)
}

well_table_below_norm <- function(w_perc, window) {

  totals <- w_perc %>%
    select(-type) %>%
    filter(str_detect(class, "low")) %>%
    group_by(report_dates) %>%
    summarize(t = unique(n_total_date),
              n = sum(n_class_type),
              p = round(n / t * 100),
              p = if_else(is.nan(p), 0, p), .groups = "drop") %>%
    mutate(text = glue("{p}% ({n}/{t})"),
           text = if_else(t == 0, glue(""), text)) %>%
    select(report_dates, text) %>%
    arrange(desc(report_dates)) %>%
    mutate(type = "Across all types")

  w_perc %>%
    filter(str_detect(class, "low")) %>%
    group_by(report_dates, type) %>%
    summarize(t = unique(n_total_type), n = sum(n_class_type),
              p = round(n / t * 100),
              p = if_else(is.nan(p), 0, p),
              .groups = "drop") %>%
    mutate(text = glue("{p}% ({n}/{t})"),
           text = if_else(t == 0, glue(""), text)) %>%
    select(report_dates, type, text) %>%
    arrange(desc(report_dates)) %>%
    bind_rows(totals) %>%
    mutate(report_dates = if_else(report_dates %in% !!window,
                                  as.character(glue("{report_dates}*")),
                                  as.character(report_dates))) %>%
    pivot_wider(names_from = report_dates, values_from = text) %>%
    select(type, everything()) %>%
    rename(`Aquifer Type` = type)
}

well_table_status <- function(w_perc, perc_values, window) {
  w_perc %>%
    select(class, report_dates, n_total_class, n_total_date) %>%
    distinct() %>%
    arrange(desc(report_dates)) %>%
    pivot_longer(cols = contains("total"), names_to = "total", values_to = "n") %>%
    mutate(class = if_else(str_detect(total, "date"), "Across all classes", class),
           class = factor(class, levels = c(perc_values$nice, "Across all classes"))) %>%
    select(-total) %>%
    distinct() %>%
    mutate(report_dates = if_else(report_dates %in% !!window,
                                  as.character(glue("{report_dates}*")),
                                  as.character(report_dates))) %>%
    pivot_wider(names_from = report_dates, values_from = n) %>%
    arrange(class) %>%
    left_join(select(perc_values, colour, nice), by = c("class" = "nice")) %>%
    mutate(colour = replace_na(colour, "white")) %>%
    select(colour, class, everything())
}

well_table_summary <- function(w_dates, w_hist, perc_values) {

  t <- well_hist_compare(w_dates, w_hist)

  last_year <- t %>%
    filter(!CurrentYear) %>%
    select(ow, value_last_year = Value, report_dates) %>%
    mutate(report_dates = report_dates + years(1))

  t %>%
    well_meta() %>%
    arrange(ow, desc(Date)) %>%
    group_by(ow) %>%
    filter(CurrentYear) %>%
    mutate(keep = if_else(all(is.na(Value)), Date[1], Date[!is.na(Value)][1])) %>%
    filter(Date == keep) %>%
    left_join(last_year, by = c("ow", "report_dates")) %>%
    mutate(ow = ow_link(ow),
           class = map_chr(percentile, perc_match, cols = "nice"),
           Name = "",
           area_name = str_remove_all(area_name, "( Natural Resource )|(Region)|(Area)"),
           district_name = str_remove(district_name, "Natural Resource District"),
           Value = as.character(round(Value, 2)),
           Value = if_else(Approval == "Working" & !is.na(Value), as.character(glue("{Value}*")), Value),
           value_last_year = round(value_last_year, 2),
           class = replace_na(class, ""),
           percentile = if_else(is.na(percentile), glue(""), glue(" ({round((1 - percentile) * 100)}\\%)")),
           percentile = glue("{class}{percentile}"),
           n_years = case_when(percentile == "" ~ glue(""),
                               quality_hist == "fair" ~ glue("{n_years}**"),
                               TRUE ~ glue(n_years)),
           median = round(median, 2)) %>%
    ungroup() %>%
    arrange(area_name, district_name, ow) %>%
    mutate(bg_colour = set_names(perc_values$colour, perc_values$nice)[class],
           bg_colour = replace_na(bg_colour, "white"),
           txt_colour = set_names(perc_values$txt_colour, perc_values$nice)[class],
           txt_colour = replace_na(txt_colour, "black")) %>%
    select(bg_colour, txt_colour,
           Region = area_name, `Geographic\nArea` = district_name, Name,
           `Obs.\nWell` = ow, `Aquifer Type` = type,
           `Latest\nDate` = Date, `Latest\nValue` = Value,
           `Percentile Class` = percentile, `Perc.\nYears` = n_years,
           `Last Year's\nValue` = value_last_year, Median = median)
}
