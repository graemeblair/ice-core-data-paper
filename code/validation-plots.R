# ---- Packages ----
library(tidyverse)
library(tidylog)
library(data.table)
library(here)
library(ggVennDiagram)

# ---- Options ----

options(scipen = 1000000)

# ---- Read in to temporary file ----

# read in ICE Core data
encounters <- arrow::read_feather(
  "data/encounters-july2025.feather"
)
arrests <- arrow::read_feather(
  "data/arrests-july2025.feather"
)
detainers <- arrow::read_feather(
  "data/detainers-july2025.feather"
)
detention_stays <- arrow::read_feather(
  "data/detention-stays-july2025.feather"
)
removals <- arrow::read_feather(
  "data/removals-july2025.feather"
)

# ---- Plot of set overlap ----

encounter_ids <- unique(encounters$unique_identifier)
detainer_ids <- unique(detainers$unique_identifier)
arrest_ids <- unique(arrests$unique_identifier)
detention_ids <- unique(detention_stays$unique_identifier)
removal_ids <- unique(removals$unique_identifier)

sets <- list(
  "encounters" = encounter_ids,
  "detainers" = detainer_ids,
  "arrests" = arrest_ids,
  "detentions" = detention_ids,
  "removals" = removal_ids
)

p0 <- ggVennDiagram(
  x = sets,
  force_upset = TRUE,
  order.set.by = "name",
  order.intersect.by = "size"
)

p1 <- ggVennDiagram(
  x = sets,
  force_upset = TRUE,
  order.set.by = "name",
  order.intersect.by = "size",
  nintersects = 10
)

ggsave(
  filename = here("figures", "id-upset.pdf"),
  plot = p1,
  dpi = 300,
  width = 8,
  height = 4,
  units = "in"
)

# We hand-edit "set size" x axis labels to fix overlap in resulting figure

# ---- Plot of weekly enforcement actions ----

dat1 <- encounters %>%
  filter(event_date >= "2023-09-04", event_date <= "2025-07-27") %>%
  mutate(week = floor_date(event_date, "week", week_start = "Monday")) %>%
  count(week) %>%
  mutate(type = "Encounters")

dat2 <- detainers %>%
  filter(
    detainer_prepare_date >= "2023-09-04",
    detainer_prepare_date <= "2025-07-27"
  ) %>%
  mutate(
    week = floor_date(detainer_prepare_date, "week", week_start = "Monday")
  ) %>%
  count(week) %>%
  mutate(type = "Detainers")

dat3 <- arrests %>%
  filter(
    apprehension_date >= "2023-09-04",
    apprehension_date <= "2025-07-27"
  ) %>%
  mutate(
    week = floor_date(apprehension_date, "week", week_start = "Monday")
  ) %>%
  count(week) %>%
  mutate(type = "Arrests")

dat4 <- detention_stays %>%
  filter(
    stay_book_in_date_time >= "2023-09-04",
    stay_book_in_date_time <= "2025-07-27"
  ) %>%
  mutate(
    week = floor_date(stay_book_in_date_time, "week", week_start = "Monday")
  ) %>%
  count(week) %>%
  mutate(type = "Detentions")

dat5 <- removals %>%
  filter(departed_date >= "2023-09-04", departed_date <= "2025-07-27") %>%
  mutate(week = floor_date(departed_date, "week", week_start = "Monday")) %>%
  count(week) %>%
  mutate(type = "Removals")

dat <- rbind(dat1, dat2, dat3, dat4, dat5)

levels = c("Encounters", "Detainers", "Arrests", "Detentions", "Removals")

p2 <- dat %>%
  mutate(type = factor(type, levels = levels)) %>%
  ggplot(aes(x = week, y = n, color = type)) +
  geom_line() +
  ylim(0, NA) +
  scale_color_viridis_d() +
  # change y axis scale to thousands
  scale_y_continuous(labels = scales::comma) +
  # make x axis date format month-year
  scale_x_date(date_labels = "%b.\n%Y", date_breaks = "6 months") +
  labs(
    x = "Date",
    y = "Enforcement actions per week",
    # title = "Weekly enforcement events & detention stay book-ins"
    color = "Table"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = here("figures", "weekly-enforcement-events.pdf"),
  plot = p2,
  dpi = 300,
  width = 8,
  height = 6,
  units = "in"
)

# END.
