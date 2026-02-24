library(dplyr)

removals <- arrow::read_feather("data/removals.feather") |> as_tibble()

removals |>
  filter(fiscal_year %in% c("FY24", "FY25")) |>
  select(pull_date, removals) |>
  datapasta::tribble_paste()

book_outs_by_reason <- arrow::read_feather(
  "data/book-outs-by-reason.feather"
) |>
  as_tibble()

book_outs_by_reason |>
  filter(fiscal_year %in% c("FY24", "FY25")) |>
  filter(release_reason %in% c("Total", "Bonded Out")) |>
  filter(pull_date %in% c("2024-09-03", "2025-09-02")) |>
  select(date, release_reason, n_book_outs) |>
  datapasta::tribble_paste()
