library(tidyverse)

# read in ICE Core data
arrests <- arrow::read_feather(
  "data/arrests-july2025.feather"
)
detainers <- arrow::read_feather(
  "data/detainers-july2025.feather"
)
detention_stints <- arrow::read_feather(
  "data/detention-stints-july2025.feather"
)
removals <- arrow::read_feather(
  "data/removals-july2025.feather"
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
    ~Agency , ~Oct  , ~Nov  , ~Dec  , ~Jan  , ~Feb  , ~Mar  , ~Apr  , ~May  , ~Jun  , ~Jul  , ~Aug  , ~Sep  ,
    "Total" , 23613 , 21123 , 22122 , 21990 , 21630 , 22921 , 22817 , 28880 , 36704 , 31262 , 32364 , 24755 ,
    "CBP"   , 15004 , 13543 , 14238 , 10237 ,  4252 ,  3798 ,  4602 ,  5238 ,  5114 ,  3805 ,  4056 ,  3462 ,
    "ICE"   ,  8609 ,  7580 ,  7884 , 11753 , 17378 , 19123 , 18215 , 23642 , 31590 , 27457 , 28308 , 21293
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
    ~Agency , ~Oct  , ~Nov  , ~Dec  , ~Jan  , ~Feb  , ~Mar  , ~Apr  , ~May  , ~Jun  , ~Jul  , ~Aug  , ~Sep  ,
    "Total" , 24118 , 17692 , 21085 , 20536 , 24431 , 22105 , 23974 , 28582 , 25034 , 23644 , 24706 , 22006 ,
    "CBP"   , 17278 , 10922 , 13349 , 13998 , 17044 , 14557 , 15533 , 20131 , 17677 , 15635 , 16516 , 14658 ,
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

dtm_removals_fy2425 <-
  tibble::tribble(
    ~pull_date   , ~removals ,
    "2023-12-26" ,     57129 ,
    "2024-01-12" ,     69741 ,
    "2024-01-22" ,     79928 ,
    "2024-02-05" ,     90861 ,
    "2024-02-20" ,    101726 ,
    "2024-03-04" ,    112576 ,
    "2024-03-18" ,    124633 ,
    "2024-04-01" ,    133803 ,
    "2024-04-15" ,    143811 ,
    "2024-04-29" ,    153872 ,
    "2024-05-13" ,    164350 ,
    "2024-05-13" ,    175693 ,
    "2024-06-10" ,    186373 ,
    "2024-06-24" ,    197713 ,
    "2024-07-08" ,    207747 ,
    "2024-08-05" ,    228444 ,
    "2024-08-19" ,    238660 ,
    "2024-09-03" ,    248739 ,
    "2023-10-10" ,     21529 ,
    "2024-10-28" ,     29817 ,
    "2023-11-13" ,     30606 ,
    "2023-11-27" ,     38079 ,
    "2023-12-11" ,     49103 ,
    "2024-12-23" ,     69546 ,
    "2025-01-06" ,     76939 ,
    "2025-01-21" ,     85769 ,
    "2025-01-21" ,     95474 ,
    "2025-02-18" ,    103868 ,
    "2025-02-18" ,    103868 ,
    "2025-03-03" ,    113541 ,
    "2025-03-17" ,    123122 ,
    "2025-03-31" ,    134500 ,
    "2025-04-14" ,    144963 ,
    "2025-04-28" ,    157948 ,
    "2025-05-12" ,    169956 ,
    "2025-05-27" ,    181360 ,
    "2025-06-09" ,    197016 ,
    "2025-06-23" ,    212645 ,
    "2025-07-07" ,    228282 ,
    "2025-07-21" ,    246287 ,
    "2025-08-04" ,    266377 ,
    "2025-08-18" ,    283295 ,
    "2025-09-02" ,    302192 ,
    "2025-09-15" ,    319980 ,
    "2024-10-28" ,     29817 ,
    "2024-11-12" ,     40658 ,
    "2024-11-25" ,     49991 ,
    "2024-12-09" ,     60595
  )

dtm_book_outs <-
  tibble::tribble(
    ~date        , ~release_reason , ~n_book_outs ,
    "2023-10-01" , "Total"         ,        20382 ,
    "2023-11-01" , "Total"         ,        19637 ,
    "2023-12-01" , "Total"         ,        20287 ,
    "2024-01-01" , "Total"         ,        19291 ,
    "2024-02-01" , "Total"         ,        22137 ,
    "2024-03-01" , "Total"         ,        24399 ,
    "2024-04-01" , "Total"         ,        23643 ,
    "2024-05-01" , "Total"         ,        25952 ,
    "2024-06-01" , "Total"         ,        23738 ,
    "2024-07-01" , "Total"         ,        24735 ,
    "2024-08-01" , "Total"         ,        23652 ,
    "2024-09-01" , "Total"         ,         4683 ,
    "2023-10-01" , "Bonded Out"    ,          941 ,
    "2023-11-01" , "Bonded Out"    ,          940 ,
    "2023-12-01" , "Bonded Out"    ,          981 ,
    "2024-01-01" , "Bonded Out"    ,          690 ,
    "2024-02-01" , "Bonded Out"    ,          852 ,
    "2024-03-01" , "Bonded Out"    ,         1085 ,
    "2024-04-01" , "Bonded Out"    ,         1084 ,
    "2024-05-01" , "Bonded Out"    ,         1021 ,
    "2024-06-01" , "Bonded Out"    ,          885 ,
    "2024-07-01" , "Bonded Out"    ,          968 ,
    "2024-08-01" , "Bonded Out"    ,         1046 ,
    "2024-09-01" , "Bonded Out"    ,          195 ,
    "2024-10-01" , "Total"         ,        21709 ,
    "2024-11-01" , "Total"         ,        20890 ,
    "2024-12-01" , "Total"         ,        20696 ,
    "2025-01-01" , "Total"         ,        19970 ,
    "2025-02-01" , "Total"         ,        17330 ,
    "2025-03-01" , "Total"         ,        19364 ,
    "2025-04-01" , "Total"         ,        21113 ,
    "2025-05-01" , "Total"         ,        26798 ,
    "2025-06-01" , "Total"         ,        29387 ,
    "2025-07-01" , "Total"         ,        30727 ,
    "2025-08-01" , "Total"         ,        32404 ,
    "2025-09-01" , "Total"         ,         7760 ,
    "2024-10-01" , "Bonded Out"    ,          909 ,
    "2024-11-01" , "Bonded Out"    ,          653 ,
    "2024-12-01" , "Bonded Out"    ,          806 ,
    "2025-01-01" , "Bonded Out"    ,          677 ,
    "2025-02-01" , "Bonded Out"    ,          803 ,
    "2025-03-01" , "Bonded Out"    ,         1791 ,
    "2025-04-01" , "Bonded Out"    ,         2512 ,
    "2025-05-01" , "Bonded Out"    ,         2438 ,
    "2025-06-01" , "Bonded Out"    ,         2479 ,
    "2025-07-01" , "Bonded Out"    ,         2050 ,
    "2025-08-01" , "Bonded Out"    ,          898 ,
    "2025-09-01" , "Bonded Out"    ,          125
  )

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
    year = format(as.Date(stay_book_in_date_time), "%Y"),
    fiscal_year = if_else(
      month %in% c("Oct", "Nov", "Dec"),
      as.integer(format(as.Date(stay_book_in_date_time), "%Y")) + 1,
      as.integer(format(as.Date(stay_book_in_date_time), "%Y"))
    )
  ) |>
  group_by(fiscal_year, year, month) |>
  summarise(
    bookin_count = n(),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = -c(fiscal_year, year, month),
    names_to = "name",
    values_to = "core_bookin_count"
  ) |>
  select(-name) |>
  mutate(
    year_month = as.Date(glue::glue(
      "{year}-{match(month, month.abb)}-01"
    ))
  )

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

dtm_monthly_bookouts_df <-
  dtm_book_outs |>
  mutate(
    date = as.Date(date),
    fiscal_year = if_else(
      month(date) >= 10,
      year(date) + 1,
      year(date)
    ),
    month = format(date, "%b")
  ) |>
  rename(year_month = date)

dtm_removals_df <-
  dtm_removals_fy2425 |>
  mutate(
    pull_date = as.Date(pull_date),
    fiscal_year = if_else(
      month(pull_date) >= 10,
      year(pull_date) + 1,
      year(pull_date)
    ),
    month = format(pull_date, "%b")
  ) |>
  rename(year_month = pull_date)


# calculate same stat from ICE Core data
core_monthly_bookouts <-
  detention_stints |>
  filter(!is.na(unique_identifier)) |>
  distinct(unique_identifier, stay_book_out_date_time) |>
  filter(
    between(
      as.Date(stay_book_out_date_time),
      as.Date("2023-10-01"),
      as.Date("2025-07-28")
    )
  ) |>
  mutate(
    month = format(as.Date(stay_book_out_date_time), "%b"),
    year = format(as.Date(stay_book_out_date_time), "%Y"),
    fiscal_year = if_else(
      month %in% c("Oct", "Nov", "Dec"),
      as.integer(format(as.Date(stay_book_out_date_time), "%Y")) + 1,
      as.integer(format(as.Date(stay_book_out_date_time), "%Y"))
    )
  ) |>
  group_by(fiscal_year, year, month) |>
  summarise(
    bookout_count = n(),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = -c(fiscal_year, year, month),
    names_to = "name",
    values_to = "core_bookout_count"
  ) |>
  select(-name) |>
  mutate(
    year_month = as.Date(glue::glue(
      "{year}-{match(month, month.abb)}-01"
    ))
  ) |>
  arrange(year_month)

core_monthly_bookouts_bondedout <-
  detention_stints |>
  filter(
    !is.na(unique_identifier),
    stay_release_reason %in% c("Bonded Out - Field Office", "Bonded Out - IJ")
  ) |>
  distinct(unique_identifier, stay_book_out_date_time) |>
  filter(
    between(
      as.Date(stay_book_out_date_time),
      as.Date("2023-10-01"),
      as.Date("2025-07-28")
    )
  ) |>
  mutate(
    month = format(as.Date(stay_book_out_date_time), "%b"),
    year = format(as.Date(stay_book_out_date_time), "%Y"),
    fiscal_year = if_else(
      month %in% c("Oct", "Nov", "Dec"),
      as.integer(format(as.Date(stay_book_out_date_time), "%Y")) + 1,
      as.integer(format(as.Date(stay_book_out_date_time), "%Y"))
    )
  ) |>
  group_by(fiscal_year, year, month) |>
  summarise(
    bookout_count = n(),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = -c(fiscal_year, year, month),
    names_to = "name",
    values_to = "core_bookout_count"
  ) |>
  select(-name) |>
  mutate(
    year_month = as.Date(glue::glue(
      "{year}-{match(month, month.abb)}-01"
    ))
  )

core_removals <-
  removals |>
  summarize(
    `n_2023-10-10` = sum(
      as.Date(departed_date) >= as.Date("2023-10-01") &
        as.Date(departed_date) <= as.Date("2023-10-10")
    ),
    `n_2023-11-13` = sum(
      as.Date(departed_date) >= as.Date("2023-10-01") &
        as.Date(departed_date) <= as.Date("2023-11-13")
    ),
    `n_2023-11-27` = sum(
      as.Date(departed_date) >= as.Date("2023-10-01") &
        as.Date(departed_date) <= as.Date("2023-11-27")
    ),
    `n_2023-12-11` = sum(
      as.Date(departed_date) >= as.Date("2023-10-01") &
        as.Date(departed_date) <= as.Date("2023-12-11")
    ),
    `n_2023-12-26` = sum(
      as.Date(departed_date) >= as.Date("2023-10-01") &
        as.Date(departed_date) <= as.Date("2023-12-26")
    ),
    `n_2024-01-12` = sum(
      as.Date(departed_date) >= as.Date("2023-10-01") &
        as.Date(departed_date) <= as.Date("2024-01-12")
    ),
    `n_2024-01-22` = sum(
      as.Date(departed_date) >= as.Date("2023-10-01") &
        as.Date(departed_date) <= as.Date("2024-01-22")
    ),
    `n_2024-02-05` = sum(
      as.Date(departed_date) >= as.Date("2023-10-01") &
        as.Date(departed_date) <= as.Date("2024-02-05")
    ),
    `n_2024-02-20` = sum(
      as.Date(departed_date) >= as.Date("2023-10-01") &
        as.Date(departed_date) <= as.Date("2024-02-20")
    ),
    `n_2024-03-04` = sum(
      as.Date(departed_date) >= as.Date("2023-10-01") &
        as.Date(departed_date) <= as.Date("2024-03-04")
    ),
    `n_2024-03-18` = sum(
      as.Date(departed_date) >= as.Date("2023-10-01") &
        as.Date(departed_date) <= as.Date("2024-03-18")
    ),
    `n_2024-04-01` = sum(
      as.Date(departed_date) >= as.Date("2023-10-01") &
        as.Date(departed_date) <= as.Date("2024-04-01")
    ),
    `n_2024-04-15` = sum(
      as.Date(departed_date) >= as.Date("2023-10-01") &
        as.Date(departed_date) <= as.Date("2024-04-15")
    ),
    `n_2024-04-29` = sum(
      as.Date(departed_date) >= as.Date("2023-10-01") &
        as.Date(departed_date) <= as.Date("2024-04-29")
    ),
    `n_2024-05-13` = sum(
      as.Date(departed_date) >= as.Date("2023-10-01") &
        as.Date(departed_date) <= as.Date("2024-05-13")
    ),
    `n_2024-05-13` = sum(
      as.Date(departed_date) >= as.Date("2023-10-01") &
        as.Date(departed_date) <= as.Date("2024-05-13")
    ),
    `n_2024-06-10` = sum(
      as.Date(departed_date) >= as.Date("2023-10-01") &
        as.Date(departed_date) <= as.Date("2024-06-10")
    ),
    `n_2024-06-24` = sum(
      as.Date(departed_date) >= as.Date("2023-10-01") &
        as.Date(departed_date) <= as.Date("2024-06-24")
    ),
    `n_2024-07-08` = sum(
      as.Date(departed_date) >= as.Date("2023-10-01") &
        as.Date(departed_date) <= as.Date("2024-07-08")
    ),
    `n_2024-08-05` = sum(
      as.Date(departed_date) >= as.Date("2023-10-01") &
        as.Date(departed_date) <= as.Date("2024-08-05")
    ),
    `n_2024-08-19` = sum(
      as.Date(departed_date) >= as.Date("2023-10-01") &
        as.Date(departed_date) <= as.Date("2024-08-19")
    ),
    `n_2024-09-03` = sum(
      as.Date(departed_date) >= as.Date("2023-10-01") &
        as.Date(departed_date) <= as.Date("2024-09-03")
    ),
    `n_2024-10-28` = sum(
      as.Date(departed_date) >= as.Date("2024-10-01") &
        as.Date(departed_date) <= as.Date("2024-10-28")
    ),
    `n_2024-10-28` = sum(
      as.Date(departed_date) >= as.Date("2024-10-01") &
        as.Date(departed_date) <= as.Date("2024-10-28")
    ),
    `n_2024-11-12` = sum(
      as.Date(departed_date) >= as.Date("2024-10-01") &
        as.Date(departed_date) <= as.Date("2024-11-12")
    ),
    `n_2024-11-25` = sum(
      as.Date(departed_date) >= as.Date("2024-10-01") &
        as.Date(departed_date) <= as.Date("2024-11-25")
    ),
    `n_2024-12-09` = sum(
      as.Date(departed_date) >= as.Date("2024-10-01") &
        as.Date(departed_date) <= as.Date("2024-12-09")
    ),
    `n_2024-12-23` = sum(
      as.Date(departed_date) >= as.Date("2024-10-01") &
        as.Date(departed_date) <= as.Date("2024-12-23")
    ),
    `n_2025-01-06` = sum(
      as.Date(departed_date) >= as.Date("2024-10-01") &
        as.Date(departed_date) <= as.Date("2025-01-06")
    ),
    `n_2025-01-21` = sum(
      as.Date(departed_date) >= as.Date("2024-10-01") &
        as.Date(departed_date) <= as.Date("2025-01-21")
    ),
    `n_2025-01-21` = sum(
      as.Date(departed_date) >= as.Date("2024-10-01") &
        as.Date(departed_date) <= as.Date("2025-01-21")
    ),
    `n_2025-02-18` = sum(
      as.Date(departed_date) >= as.Date("2024-10-01") &
        as.Date(departed_date) <= as.Date("2025-02-18")
    ),
    `n_2025-02-18` = sum(
      as.Date(departed_date) >= as.Date("2024-10-01") &
        as.Date(departed_date) <= as.Date("2025-02-18")
    ),
    `n_2025-03-03` = sum(
      as.Date(departed_date) >= as.Date("2024-10-01") &
        as.Date(departed_date) <= as.Date("2025-03-03")
    ),
    `n_2025-03-17` = sum(
      as.Date(departed_date) >= as.Date("2024-10-01") &
        as.Date(departed_date) <= as.Date("2025-03-17")
    ),
    `n_2025-03-31` = sum(
      as.Date(departed_date) >= as.Date("2024-10-01") &
        as.Date(departed_date) <= as.Date("2025-03-31")
    ),
    `n_2025-04-14` = sum(
      as.Date(departed_date) >= as.Date("2024-10-01") &
        as.Date(departed_date) <= as.Date("2025-04-14")
    ),
    `n_2025-04-28` = sum(
      as.Date(departed_date) >= as.Date("2024-10-01") &
        as.Date(departed_date) <= as.Date("2025-04-28")
    ),
    `n_2025-05-12` = sum(
      as.Date(departed_date) >= as.Date("2024-10-01") &
        as.Date(departed_date) <= as.Date("2025-05-12")
    ),
    `n_2025-05-27` = sum(
      as.Date(departed_date) >= as.Date("2024-10-01") &
        as.Date(departed_date) <= as.Date("2025-05-27")
    ),
    `n_2025-06-09` = sum(
      as.Date(departed_date) >= as.Date("2024-10-01") &
        as.Date(departed_date) <= as.Date("2025-06-09")
    ),
    `n_2025-06-23` = sum(
      as.Date(departed_date) >= as.Date("2024-10-01") &
        as.Date(departed_date) <= as.Date("2025-06-23")
    ),
    `n_2025-07-07` = sum(
      as.Date(departed_date) >= as.Date("2024-10-01") &
        as.Date(departed_date) <= as.Date("2025-07-07")
    ),
    `n_2025-07-21` = sum(
      as.Date(departed_date) >= as.Date("2024-10-01") &
        as.Date(departed_date) <= as.Date("2025-07-21")
    ),
    `n_2025-08-04` = sum(
      as.Date(departed_date) >= as.Date("2024-10-01") &
        as.Date(departed_date) <= as.Date("2025-08-04")
    ),
    `n_2025-08-18` = sum(
      as.Date(departed_date) >= as.Date("2024-10-01") &
        as.Date(departed_date) <= as.Date("2025-08-18")
    ),
    `n_2025-09-02` = sum(
      as.Date(departed_date) >= as.Date("2024-10-01") &
        as.Date(departed_date) <= as.Date("2025-09-02")
    ),
    `n_2025-09-15` = sum(
      as.Date(departed_date) >= as.Date("2024-10-01") &
        as.Date(departed_date) <= as.Date("2025-09-15")
    ),
    `n_2025-11-10` = sum(
      as.Date(departed_date) >= as.Date("2024-10-01") &
        as.Date(departed_date) <= as.Date("2025-11-10")
    ),
    `n_2025-11-28` = sum(
      as.Date(departed_date) >= as.Date("2024-10-01") &
        as.Date(departed_date) <= as.Date("2025-11-28")
    ),
    `n_2025-12-11` = sum(
      as.Date(departed_date) >= as.Date("2024-10-01") &
        as.Date(departed_date) <= as.Date("2025-12-11")
    ),
    `n_2025-12-26` = sum(
      as.Date(departed_date) >= as.Date("2024-10-01") &
        as.Date(departed_date) <= as.Date("2025-12-26")
    ),
    `n_2026-01-22` = sum(
      as.Date(departed_date) >= as.Date("2024-10-01") &
        as.Date(departed_date) <= as.Date("2026-01-22")
    )
  ) |>
  pivot_longer(cols = everything(), names_to = "week", values_to = "n") |>
  mutate(
    week = as.Date(week, format = "n_%Y-%m-%d"),
    fiscal_year = year(week) + if_else(month(week) >= 10, 1L, 0L),
    month = month(week, label = TRUE, abbr = TRUE)
  ) |>
  arrange(week) |>
  rename(year_month = week)

plot_df <-
  bind_rows(
    "dtm-bookin" = dtm_monthly_bookins_df |> rename(n = bookin_count),
    "dtm-bookout" = dtm_monthly_bookouts_df |> rename(n = n_book_outs),
    "core-bookin" = core_monthly_bookins |>
      rename(n = core_bookin_count) |>
      mutate(release_reason = "Total") |>
      mutate(year = as.integer(year)),
    "core-bookout" = core_monthly_bookouts |>
      rename(n = core_bookout_count) |>
      mutate(release_reason = "Total") |>
      mutate(year = as.integer(year)),
    "core-bookout" = core_monthly_bookouts_bondedout |>
      rename(n = core_bookout_count) |>
      mutate(release_reason = "Bonded Out") |>
      mutate(year = as.integer(year)),
    "dtm-removals" = dtm_removals_df |> rename(n = removals),
    "core-removals" = core_removals,
    .id = "type"
  ) |>
  separate(type, into = c("dataset", "event"), sep = "-") |>
  mutate(release_reason = replace_na(release_reason, "Total")) |>
  mutate(
    event = case_when(
      release_reason == "Bonded Out" ~ "bookout-bond",
      TRUE ~ event
    )
  )

plot_df |>
  # filter(event == "bookin") |>
  filter_out(
    str_detect(event, "bookout") &
      fiscal_year == 2024 &
      month == "Sep"
  ) |>
  filter(
    year_month <= as.Date("2025-07-01")
  ) |>
  mutate(
    dataset = case_when(
      dataset == "dtm" ~ "ICE detention\nmanagement spreadsheets",
      dataset == "core" ~ "This dataset"
    ),
    dataset = factor(
      dataset,
      levels = c("This dataset", "ICE detention\nmanagement spreadsheets")
    ),
    event = case_when(
      event == "bookin" ~ "Book-ins",
      event == "bookout" & release_reason == "Total" ~ "Book-outs\n(total)",
      event == "bookout-bond" &
        release_reason == "Bonded Out" ~ "Book-outs\n(bonded out)",
      event == "removals" ~ "Removals"
    )
  ) |>
  ggplot(aes(
    year_month,
    n,
    color = dataset,
    group = interaction(dataset, fiscal_year)
  )) +
  geom_line() +
  facet_grid(event ~ ., scales = "free_y") +
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

ggsave(filename = "figures/compare-counts-dtm.pdf", width = 6, height = 5)

# dtm_monthly_bookins_df |>
#   # keep through july 2025
#   filter(
#     year_month <= as.Date("2025-07-01")
#   ) |>
#   left_join(
#     core_monthly_bookins,
#     by = c("fiscal_year", "month")
#   ) |>
#   pivot_longer(
#     cols = c(bookin_count, core_bookin_count),
#     names_to = "data_source",
#     values_to = "count"
#   ) |>
#   mutate(
#     data_source = case_when(
#       data_source == "bookin_count" ~ "ICE detention\nmanagement spreadsheets",
#       data_source == "core_bookin_count" ~ "This dataset"
#     ),
#     data_source = factor(
#       data_source,
#       levels = c("This dataset", "ICE detention\nmanagement spreadsheets")
#     )
#   ) |>
#   # order months
#   ggplot(aes(x = year_month, y = count, color = data_source)) +
#   geom_line() +
#   scale_color_grey() +
#   # change y axis scale to thousands
#   scale_y_continuous(labels = scales::comma) +
#   # make x axis date format month-year
#   scale_x_date(date_labels = "%b.\n%Y", date_breaks = "6 months") +
#   theme_minimal() +
#   labs(
#     x = "Date",
#     y = "Enforcement actions per week",
#     color = "Data Source"
#   ) +
#   theme(
#     legend.position = "bottom",
#     axis.title.x = element_blank(),
#     # make y axis title further from scale
#     axis.title.y = element_text(margin = margin(r = 10)),
#     # add margin on righthand side of plot so label is shown
#     plot.margin = margin(t = 5, r = 12, b = 5, l = 5)
#   )

# ggsave(filename = "figures/compare-counts-dtm.pdf", width = 6, height = 2.5)

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
  "data/arrests-2012-2023.feather"
)
detentions_12_23 <- arrow::read_feather(
  "data/detentions-2012-2023.feather"
)
removals_12_23 <- arrow::read_feather(
  "data/removals-2012-2023.feather"
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
    `2012-2023` = detentions_12_23 |>
      rename(book_in_date_time = detention_book_in_date),
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
  pivot_wider(
    id_cols = c(type, day),
    names_from = data_source,
    values_from = n
  ) |>
  mutate(
    difference = current - `2012-2023`,
    percent_difference = difference / `2012-2023` * 100
  ) |>
  group_by(type) |>
  summarize(
    avg_diff = median(difference, na.rm = TRUE),
    avg_pct_diff = median(percent_difference, na.rm = TRUE)
  )

compare_df_long |>
  mutate(
    type = case_when(
      type == "arrests" ~ "Arrests",
      type == "detentions" ~ "Detentions",
      type == "removals" ~ "Removals"
    ),
    data_source = factor(
      data_source,
      levels = c("current", "2012-2023"),
      labels = c("This dataset", "ACLU v. ICE (2012-2023)")
    )
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
