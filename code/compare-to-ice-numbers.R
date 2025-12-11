library(tidyverse)

# read in ICE Core data
arrests <- arrow::read_feather(
  "https://github.com/deportationdata/ice/raw/29445e94454aa9cdb8d2a5cced2c2b5a64ed10da/data/arrests-latest.feather"
)
detainers <- arrow::read_feather(
  "https://github.com/deportationdata/ice/raw/259b7af52ddee745295152e9651813e4b366cf9b/data/detainers-latest.feather"
)
detention_stints <- arrow::read_feather(
  "https://github.com/deportationdata/ice/raw/c7e1d106fefb7b924114fcaf29776ae10634da1a/data/detention-stints-latest.feather"
)
removals <- arrow::read_feather(
  "https://github.com/deportationdata/ice/raw/44903d4c57695f1ef12062b93b7a2fe4418cbf3a/data/removals-latest.feather"
)

# verify

# figure 3
# FY 2024 ERO CIEP Arrests [by category] - sum to get total arrests
# "CIEP data is current through September 30, 2024"
arrests_report <- c(80999, 724, 29703, 2005) |> sum()

arrests_core <-
  arrests |>
  # calculate number of arrests in FY24
  filter(
    apprehension_date >= as.Date("2023-10-01") &
      apprehension_date <= as.Date("2024-09-30")
  ) |>
  count()

# figure 4
# FY 2024 ERO CIEP Detainers Issued [by category] - sum to get total detainers issued
# "CIEP data is current through September 30, 2024"
detainers_report <- c(144692, 324, 1732, 3016) |> sum()
detainers_core <-
  detainers |>
  # calculate number of detainers issued in FY24
  filter(
    detainer_prepare_date >= as.Date("2023-10-01") &
      detainer_prepare_date <= as.Date("2024-09-30")
  ) |>
  count()

# figure 5
# FY 2024 ERO CIEP Removals [by category] - sum to get total removals
# "CIEP data is current through September 30, 2024"
removals_ciep <- c(89495, 471, 180476, 1042) |> sum()
# figure 23
# FY 2019 – FY 2024 Overall ERO Removals by Fiscal Year
removals_ero <- c(271484) |> sum()

removals_core <-
  removals |>
  # remove duplicates
  filter(duplicate_likely == FALSE) |>
  # calculate number of removals in FY24
  filter(
    departed_date >= as.Date("2023-10-01") &
      departed_date <= as.Date("2024-09-30")
  ) |>
  summarize(n = n_distinct(unique_identifier)) 

# summary table of above numbers
comparison_summary_initial <-
  tibble::tibble(
    metric = c("Arrests", "Detainers", "Removals"),
    reported_count = c(arrests_report, detainers_report, removals_ciep),
    core_count = c(arrests_core$n, detainers_core$n, removals_core$n),
  ) |>
  mutate(
    difference = reported_count - core_count,
    percent_difference = difference / reported_count * 100
  )



# compare dtm 9-25-25 (last in FY25)
# https://ucla.app.box.com/index.php?rm=box_download_shared_file&shared_name=9d8qnnduhus4bd5mwqt7l95kz34fic2v&file_id=f_1998729580163

dtm_monthly_bookins_by_agency_fy25 <-
  tibble::tribble(
    ~Agency , ~Oct  , ~Nov  , ~Dec  , ~Jan  , ~Feb  , ~Mar  , ~Apr  , ~May  , ~Jun  , ~Jul  , ~Aug , ~Sep ,
    "Total", 23613, 21123, 22122, 21990, 21630, 22921, 22817, 28880, 36704, 31262, 32364, 24755,
    "CBP",   15004, 13543, 14238, 10237,  4252,  3798,  4602,  5238,  5114,  3805,  4056,  3462,
    "ICE",    8609,  7580,  7884, 11753, 17378, 19123, 18215, 23642, 31590, 27457, 28308, 21293
  ) |>
  filter(Agency == "Total") |>
  tidyr::pivot_longer(
    cols = -Agency,
    names_to = "month",
    values_to = "bookin_count"
  ) |>
  select(-Agency) |> 
  mutate(fiscal_year = 2025)

# compare to FY24 annual Detention Management spreadsheet
# https://ucla.app.box.com/index.php?rm=box_download_shared_file&shared_name=9d8qnnduhus4bd5mwqt7l95kz34fic2v&file_id=f_1836537358917

dtm_monthly_bookins_by_agency_fy24 <-
  tibble::tribble(
    ~Agency , ~Oct  , ~Nov  , ~Dec  , ~Jan  , ~Feb  , ~Mar  , ~Apr  , ~May  , ~Jun  , ~Jul  , ~Aug , ~Sep ,
    "Total" , 24118 , 17692 , 21085 , 20536 , 24431 , 22105 , 23974 , 28582 , 25034 , 23644 , 24706 , 22006 ,
    "CBP"   , 17278 , 10922 , 13349 , 13998 , 17044 , 14557 , 15533 , 20131 , 17677 , 15635 , 16516 , 14658  ,
    "ICE"   ,  6840 ,  6770 ,  7736 ,  6538 ,  7387 ,  7548 ,  8441 ,  8451 ,  7357 ,  8009 ,  8190 ,  7348
  ) |>
  filter(Agency == "Total") |>
  tidyr::pivot_longer(
    cols = -Agency,
    names_to = "month",
    values_to = "bookin_count"
  ) |>
  select(-Agency) |> 
  mutate(fiscal_year = 2024)

# calculate same stat from ICE Core data
core_monthly_bookins <-
  detention_stints |>
  filter(!is.na(unique_identifier)) |>
  distinct(unique_identifier, stay_book_in_date_time) |>
  filter(
    between(
      as.Date(stay_book_in_date_time),
      as.Date("2023-10-01"),
      as.Date("2025-07-28")
    )
  ) |>
  mutate(
    month = format(as.Date(stay_book_in_date_time), "%b"),
    fiscal_year = if_else(
      month %in% c("Oct", "Nov", "Dec"),
      as.integer(format(as.Date(stay_book_in_date_time), "%Y")) + 1,
      as.integer(format(as.Date(stay_book_in_date_time), "%Y"))
    )
  ) |>
  group_by(fiscal_year, month) |>
  summarise(
    bookin_count = n(),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = -c(fiscal_year, month),
    names_to = "name",
    values_to = "core_bookin_count"
  ) |>
  select(-name)

dtm_monthly_bookins_df <- 
  bind_rows(
  dtm_monthly_bookins_by_agency_fy24,
  dtm_monthly_bookins_by_agency_fy25
) |> 
  mutate(    
    year = if_else(
      month %in% c("Oct", "Nov", "Dec"),
      fiscal_year - 1,
      fiscal_year
    ),
    year_month = as.Date(glue::glue("{year}-{match(month, month.abb)}-01"))
  )

dtm_monthly_bookins_df |> 
  # keep through july 2025
  filter(
    year_month <= as.Date("2025-07-01")
  ) |>
  left_join(
    core_monthly_bookins,
    by = c("fiscal_year", "month")
  ) |>
  pivot_longer(
    cols = c(bookin_count, core_bookin_count),
    names_to = "data_source",
    values_to = "count"
  ) |> 
  mutate(
    data_source = case_when(
      data_source == "bookin_count" ~ "ICE detention\nmanagement spreadsheets",
      data_source == "core_bookin_count" ~ "This dataset"
    ),
    data_source = factor(data_source, levels = c("This dataset", "ICE detention\nmanagement spreadsheets"))
  ) |> 
  # order months
  ggplot(aes(x = year_month, y = count, color = data_source)) +
  geom_line() + 
  scale_color_grey() +
  # change y axis scale to thousands
  scale_y_continuous(labels = scales::comma) +
  # make x axis date format month-year
  scale_x_date(date_labels = "%b.\n%Y", date_breaks = "6 months") + 
  theme_minimal() + 
  labs(
    x = "Date",
    y = "Enforcement actions per week",
    color = "Data Source"
  ) +
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank(),
    # make y axis title further from scale
    axis.title.y = element_text(margin = margin(r = 10)),
    # add margin on righthand side of plot so label is shown
    plot.margin = margin(t = 5, r = 12, b = 5, l = 5)
  )

ggsave(filename = "figures/compare-counts-dtm.pdf", width = 6, height = 2.5)


# create a tibble to summarize every comparison (theirs, ours)
comparison_summary <-
  tibble::tibble(
    metric = c("Arrests", "Detainers", "Removals"),
    reported_count = c(arrests_report, detainers_report, removals_ciep),
    core_count = c(arrests_core$n, detainers_core$n, removals_core$n),
  ) |>
  bind_rows(
    left_join(
      dtm_monthly_bookins_df |> rename(reported_count = bookin_count),
      core_monthly_bookins |> rename(core_count = core_bookin_count),
      by = c("fiscal_year", "month")
    ) |>
      mutate(metric = str_c("Monthly Book-ins: ", year_month), .keep = "unused")
  ) |>
  mutate(
    difference = reported_count - core_count,
    percent_difference = difference / reported_count * 100
  )


# compare to 2012-2023 ICE Core data

arrests_12_23 <- arrow::read_feather(
  "https://github.com/deportationdata/ice/raw/refs/heads/main/data/ice-arrests-2012-2023.feather"
)
detentions_12_23 <- arrow::read_feather(
  "https://github.com/deportationdata/ice/raw/refs/heads/main/data/ice-detentions-2012-2023.feather"
)
removals_12_23 <- arrow::read_feather(
  "https://github.com/deportationdata/ice/raw/refs/heads/main/data/ice-removals-2012-2023.feather"
)

arrests_overlap <- 
  bind_rows(
    `current` = arrests,
    `2012-2023` = arrests_12_23,
    .id = "data_source"
  ) |> 
  filter(
    apprehension_date >= as.Date("2023-09-01") &
      apprehension_date <= as.Date("2023-09-30")
  ) |> 
  mutate(date = apprehension_date)

detentions_overlap <- 
  bind_rows(
    `current` = detention_stints,
    `2012-2023` = detentions_12_23 |> rename(book_in_date_time = detention_book_in_date),
    .id = "data_source"
  ) |> 
  filter(
    book_in_date_time >= as.Date("2023-09-01") &
      book_in_date_time <= as.Date("2023-09-30")
  ) |> 
  mutate(date = book_in_date_time)

removals_overlap <-
  bind_rows(
    `current` = removals,
    `2012-2023` = removals_12_23 |> rename(departed_date = departure_date),
    .id = "data_source"
  ) |>
  filter(
    departed_date >= as.Date("2023-09-01") &
      departed_date <= as.Date("2023-09-30")
  ) |> 
  mutate(date = departed_date)

compare_df_long <- 
bind_rows(
  "arrests" = arrests_overlap,
  "detentions" = detentions_overlap,
  "removals" = removals_overlap,
  .id = "type"
) |> 
  mutate(day = lubridate::floor_date(date, unit = "day")) |>
  count(type, data_source, day) 

compare_df_long |> 
  pivot_wider(id_cols = c(type, day), names_from = data_source, values_from = n) |> 
  mutate(difference = current - `2012-2023`,
         percent_difference = difference / `2012-2023` * 100) |> 
  group_by(type) |> 
  summarize(avg_diff = median(difference, na.rm = TRUE),
            avg_pct_diff = median(percent_difference, na.rm = TRUE))

compare_df_long |>
  mutate(
    type = case_when(
      type == "arrests" ~ "Arrests",
      type == "detentions" ~ "Detentions",
      type == "removals" ~ "Removals"
    ),
    data_source = factor(data_source, levels = c("current", "2012-2023"), labels = c("This dataset", "ACLU v. ICE (2012-2023)"))
  ) |> 
  ggplot(aes(x = day, y = n, color = data_source, line_width = data_source)) +
  geom_line() + 
  facet_grid(type ~ ., scales = "free_y") + 
  scale_color_grey() +
  theme_minimal() + 
  labs(
    x = "Date",
    y = "Enforcement actions per week",
    color = "Data Source"
  ) +
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank(),
    axis.title.y = element_text(margin = margin(r = 10)),
    # add margin on righthand side of plot so label is shown
    plot.margin = margin(t = 5, r = 12, b = 5, l = 5)
  )

ggsave(filename = "figures/compare-counts-aclu-ice.pdf", width = 6, height = 4)

