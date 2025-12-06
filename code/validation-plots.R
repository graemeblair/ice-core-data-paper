# ---- Packages ----
library(tidyverse)
library(tidylog)
library(data.table)
library(here)
library(ggVennDiagram)

# ---- Read in to temporary file ----

# read in ICE Core data
encounters <- arrow::read_feather(
  "https://github.com/deportationdata/ice/raw/main/data/encounters-latest.feather"
)
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

# ---- Plot of set overlap ----

encounter_ids <- unique(encounters$unique_identifier)
detainer_ids <- unique(detainers$unique_identifier)
arrest_ids <- unique(arrests$unique_identifier)
detention_ids <- unique(detention_stints$unique_identifier)
removal_ids <- unique(removals$unique_identifier)

sets <- list("encounters" = encounter_ids,
             "detainers" = detainer_ids,
             "arrests" = arrest_ids,
             "detentions" = detention_ids,
             "removals" = removal_ids)

p0 <- ggVennDiagram(x = sets,
                    force_upset = TRUE,
                    order.set.by = "name",
                    order.intersect.by = "size"
)

p1 <- ggVennDiagram(x = sets,
                    force_upset = TRUE,
                    order.set.by = "name",
                    order.intersect.by = "size",
                    nintersects = 10
)

ggsave(filename="figures/id-upset.png", plot=p1, dpi=300, width=8, height=4, units="in")

# ---- Plot of weekly enforcement actions ----

dat1 <- encounters %>% 
  filter(event_date >= "2023-09-04",
         event_date <= "2025-07-27") %>% 
  mutate(week = floor_date(event_date, "week", week_start = "Monday")) %>% 
  count(week) %>% 
  mutate(type = "encounters")

dat2 <- detainers %>% 
  filter(detainer_prepare_date >= "2023-09-04",
         detainer_prepare_date <= "2025-07-27") %>% 
  mutate(week = floor_date(detainer_prepare_date, "week", week_start = "Monday")) %>% 
  count(week) %>% 
  mutate(type = "detainers")

dat3 <- arrests %>% 
  filter(apprehension_date >= "2023-09-04",
         apprehension_date <= "2025-07-27") %>% 
  mutate(week = floor_date(apprehension_date, "week", week_start = "Monday")) %>% 
  count(week) %>% 
  mutate(type = "arrests")

dat4 <- detention_stays_df %>% 
  filter(stay_book_in_date_time >= "2023-09-04",
         stay_book_in_date_time <= "2025-07-27") %>% 
  mutate(week = floor_date(stay_book_in_date_time, "week", week_start = "Monday")) %>% 
  count(week) %>% 
  mutate(type = "detentions")

dat5 <- removals %>% 
  filter(departed_date >= "2023-09-04",
         departed_date <= "2025-07-27") %>% 
  mutate(week = floor_date(departed_date, "week", week_start = "Monday")) %>% 
  count(week) %>% 
  mutate(type = "removals")

dat <- rbind(dat1, dat2, dat3, dat4, dat5)

p2 <- dat %>% 
  ggplot(aes(x = week, y = n, color = type)) +
  geom_line() +
  ylim(0, NA) +
  labs(title = "Weekly enforcement events & detention stay book-ins")

ggsave(filename="figures/weekly-enforcement-events.png", plot=p2, dpi=300, width=8, height=6, units="in")

# ---- Plot of weekly detention stay book-ins ----

# dat <- detention_stays_df %>% 
#   filter(stay_book_in_date_time >= "2023-09-04",
#          stay_book_in_date_time <= "2025-07-27") %>% 
#   mutate(week = floor_date(stay_book_in_date_time, "week", week_start = "Monday")) %>% 
#   count(week) %>% 
#   mutate(type = "detention stay book-ins")
# 
# p3 <- dat %>% 
#   ggplot(aes(x = week, y = n, color = type)) +
#   geom_line() +
#   ylim(0, NA) +
#   labs(title = "Weekly detention stay book-ins")
# 
# p3

# ---- Comparison across previous releases? ----

# END.