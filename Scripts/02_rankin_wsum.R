#############################################+
## Ordination analyses
## Valentina LM for the CostAction Team
## First draft: 2022-05-04
#############################################+

## ---- packages ----
library(tidyverse)
library(ggpubr)
# library(vegan)

## ---- functions ----
# not_all_na <- function(x) any(!is.na(x))
# not_any_na <- function(x) all(!is.na(x))
recode_plyr <- function(x) {
  as.numeric(plyr::mapvalues(x, from = c("Strongly disagree", "Disagree",
                                         "Neither agree nor disagree",
                                         "Agree", "Strongly agree"),
                             to = c(1,2,3,4,5)))
  # please note: 'I don't know' and 'N/A' -> NA
}

## ---- data and data tidying ----
temp <- read_delim("output/workshop_individual_ass.csv")[,-1]

## ---- data: recode to numeric ----
sapply(temp[,4:12], recode_plyr) -> scores_rec
bind_cols(temp[,1:3],as.data.frame(scores_rec)) -> scores_num

## ---- data:long formats ----
temp %>% 
  pivot_longer(
    cols = audience:improve_curation,
    names_to = "criterion",
    values_to = "rank"
  ) -> scores_tidy_long
scores_num %>% 
  pivot_longer(
    cols = audience:improve_curation,
    names_to = "criterion",
    values_to = "rank"
  ) -> scores_num_long



## ---- sum of scores ----
scores_num_long %>% 
  group_by(technology) %>% 
  summarise(sum.scores = sum(rank, na.rm = TRUE),
            no.coders = length(unique(coder))) %>% 
  mutate(w.rank = sum.scores / no.coders) %>% 
  arrange(desc(w.rank)) -> rank_sum_tech
write.csv(rank_sum_tech, "output/rank_sum_tech_workshop_individual_ass.csv")

## ---- scores per criteria ----
scores_num_long %>% 
  group_by(technology, criterion) %>% 
  summarise(sum.scores = sum(rank, na.rm = TRUE),
            no.coders = length(unique(coder))) -> dati
pivot_wider(dati, names_from = criterion,
              values_from = sum.scores) -> dati_wide

as.matrix(dati_wide[,3:11]) -> mat
as.numeric(dati_wide$no.coders) -> dev
plyr::aaply(mat, 2, "/", dev) -> weighted.scores
bind_cols(dati_wide[,1:2],
          as.data.frame(t(weighted.scores))) -> average_scores_criteria

# enose, acoustic analysis, open source hardware, collective intelligence, 
# data aggregation, social media mining
  