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


well_plots_base <- function(title = "", legend = "right") {
  ggplot() +
    theme_bw() +
    theme(legend.title = element_blank(), legend.position = legend,
          legend.margin = margin(),
          legend.box.spacing = unit(5, "pt"),
          legend.spacing = unit(1, "pt")) +
    scale_y_reverse() +
    scale_x_date(expand = c(0, 0)) +
    scale_size_manual(values = setNames(plot_values$size, plot_values$type)) +
    scale_colour_manual(values = setNames(plot_values$colour, plot_values$type)) +
    scale_fill_manual(values = setNames(perc_values$colour, perc_values$nice)) +
    labs(x = "Day of Year", y = "DBG (m)", subtitle = title)
}

well_plot_perc <- function(full, hist, latest_date = NULL, legend = "right") {

  recent <- filter(full, WaterYear == max(WaterYear))
  if(nrow(recent) > 0) {
    origin <- recent$Date[recent$DayofYear == 1] - days(1)
    hist <- mutate(hist,
                   Date = as_date(DayofYear, origin = origin),
                   Approval = "Median",
                   q = map(v, well_quantiles)) %>%
      unnest(q)

    data <- select(recent, Date, Approval, Value) %>%
      bind_rows(select(hist, Date, Approval, Value = median)) %>%
      mutate(Approval = factor(Approval,
                               levels = c("Median", "Approved", "Working")))

    title <- glue("Water Year {glue_collapse(unique(recent$CalendarYear), ' - ')}")
    range_name <- glue("Historical range of min & max ({hist$start_year[1]} - ",
                       "{hist$end_year[1]})")

    p <- well_plots_base(title = title, legend = legend) +
      geom_ribbon(data = hist, alpha = 0.35,
                  aes(x = Date, ymin = q_low, ymax = q_high, fill = nice)) +
      geom_line(data = data, aes(x = Date, y = Value, colour = Approval,
                                 size = Approval), na.rm = TRUE)

    if(!is.null(latest_date) && latest_date$Date %in% data$Date) {
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

  data <- mutate(data, yr_bin = if_else(WaterYear <= yr_bin[2],
                                        glue("Water Year {yr_bin[1]} - {yr_bin[2]}"),
                                        glue("Water Year {yr_bin[2]+1} - {yr_bin[3]}")))

  p <- well_plots_base(title = "Historical Record", legend = legend) +
    geom_line(data = data,
              aes(x = Date, y = Value, colour = Approval, size = Approval),
              na.rm = TRUE) +
    facet_wrap(~ yr_bin, ncol = 1, scales = "free_x")

  if(!is.null(latest_date) &&
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


well_table_below_norm <- function(w, by) {





}
