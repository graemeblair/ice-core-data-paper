# ---- Packages ----
library(tidyverse)
library(tidylog)
library(data.table)
library(here)
library(ggVennDiagram)

# ---- Options ----

options(scipen = 1000000)

# ---- Functions ----
source("code/functions/check_dttm_and_convert_to_date.R")
source("code/functions/is_not_blank_or_redacted.R")

# ---- Read in to temporary file ----

encounter_df <- arrow::read_feather("data/encounters-latest.feather")
detainer_df <- arrow::read_feather("data/detainers-latest.feather")
arrest_df <- arrow::read_feather("data/arrests-latest.feather")
detention_df <- arrow::read_feather("data/detention-stints-latest.feather")
detention_stays_df <- arrow::read_feather("data/detention-stays-latest.feather")
removal_df <- arrow::read_feather("data/removals-latest.feather")

# ---- Plot of set overlap ----

encounter_ids <- unique(encounter_df$unique_identifier)
detainer_ids <- unique(detainer_df$unique_identifier)
arrest_ids <- unique(arrest_df$unique_identifier)
detention_ids <- unique(detention_df$unique_identifier)
removal_ids <- unique(removal_df$unique_identifier)

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

p1

# We hand-edit "set size" x axis labels to fix overlap in resulting figure

ggsave(filename="id-upset.pdf", plot=p1, dpi=300, width=8, height=4, units="in")

# ---- Plot of weekly enforcement actions ----

dat1 <- encounter_df %>% 
  filter(event_date >= "2023-09-04",
         event_date <= "2025-07-27") %>% 
  mutate(week = floor_date(event_date, "week", week_start = "Monday")) %>% 
  count(week) %>% 
  mutate(type = "Encounters")

dat2 <- detainer_df %>% 
  filter(detainer_prepare_date >= "2023-09-04",
         detainer_prepare_date <= "2025-07-27") %>% 
  mutate(week = floor_date(detainer_prepare_date, "week", week_start = "Monday")) %>% 
  count(week) %>% 
  mutate(type = "Detainers")

dat3 <- arrest_df %>% 
  filter(apprehension_date >= "2023-09-04",
         apprehension_date <= "2025-07-27") %>% 
  mutate(week = floor_date(apprehension_date, "week", week_start = "Monday")) %>% 
  count(week) %>% 
  mutate(type = "Arrests")

dat4 <- detention_stays_df %>% 
  filter(stay_book_in_date_time >= "2023-09-04",
         stay_book_in_date_time <= "2025-07-27") %>% 
  mutate(week = floor_date(stay_book_in_date_time, "week", week_start = "Monday")) %>% 
  count(week) %>% 
  mutate(type = "Detentions")

dat5 <- removal_df %>% 
  filter(departed_date >= "2023-09-04",
         departed_date <= "2025-07-27") %>% 
  mutate(week = floor_date(departed_date, "week", week_start = "Monday")) %>% 
  count(week) %>% 
  mutate(type = "Removals")

dat <- rbind(dat1, dat2, dat3, dat4, dat5)

levels = c("Encounters", "Detainers", "Arrests", "Detentions", "Removals")

p2 <- dat %>% 
  mutate(type = factor(type, levels=levels)) %>% 
  ggplot(aes(x = week, y = n, color = type)) +
  geom_line() +
  ylim(0, NA) +
  scale_color_viridis_d() +
  # change y axis scale to thousands
  scale_y_continuous(labels = scales::comma) +
  # make x axis date format month-year
  scale_x_date(date_labels = "%b.\n%Y", date_breaks = "6 months") + 
  labs(x = "Date",
       y = "Enforcement actions per week",
      # title = "Weekly enforcement events & detention stay book-ins"
       color = "Table") +
  theme_minimal() +
  theme(legend.position = "bottom")

p2

ggsave(filename="weekly-enforcement-events.pdf", plot=p2, dpi=300, width=8, height=6, units="in")

# ---- Plot of weekly detention stay book-ins ----

dat <- detention_stays_df %>% 
  filter(stay_book_in_date_time >= "2023-09-04",
         stay_book_in_date_time <= "2025-07-27") %>% 
  mutate(week = floor_date(stay_book_in_date_time, "week", week_start = "Monday")) %>% 
  count(week) %>% 
  mutate(type = "detention stay book-ins")

p3 <- dat %>% 
  ggplot(aes(x = week, y = n, color = type)) +
  geom_line() +
  ylim(0, NA) +
  labs(title = "Weekly detention stay book-ins") +
  theme_minimal()

# ggsave(filename="weekly-detention-stay-book-ins.pdf", plot=p3, dpi=300, width=8, height=6, units="in")

# ---- Comparison across previous releases? ----

# END.