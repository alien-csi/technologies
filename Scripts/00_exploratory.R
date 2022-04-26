#############################################+
## Exploratory analyses
## Valentina LM for the CostAction Team
## First draft: 2022-04-25
#############################################+

## ---- packages ----
library(tidyverse)
library(ggpubr)

## ---- functions ----
not_all_na <- function(x) any(!is.na(x))
not_any_na <- function(x) all(!is.na(x))
recode_plyr <- function(x) {
  as.numeric(plyr::mapvalues(x, from = c("Strongly disagree", "Disagree",
                                         "Neither agree nor disagree",
                                         "Agree", "Strongly agree"),
                             to = c(1,2,3,4,5)))
}

## ---- data and data tidying ----
scores <- read_delim("Data/scores_rev.csv", delim = ";")
scores %>% 
  rename(date_time = 'Tijdstempel',
         coder = 'E-mailadres',
         technology = 'Select the technology you need to assess') %>% 
  select(-starts_with("Comments")) -> scores
distinct(scores["technology"]) -> techs
scores[ scores == "" ] <- NA

# temp <- vector("list", dim(techs)[1])
# i = 1
# while (i <= dim(techs)[1]) {
#   scores %>% 
#     filter(technology == techs[i,1]) %>% 
#     select(where(not_all_na)) -> temp[[i]]
  # colnames(temp[[i]]) <- c("date_time",
  #                          "coder",
  #                          "technology",
  #                          "audience",
  #                          "engagement_others",
  #                          "engagement_feedback",
  #                          "application",
  #                          "new_data",
  #                          "extend_data",
  #                          "improve_quality",
  #                          "improve_flow",
  #                          "improve_curation")
#   i = i + 1
# }

scores <- scores %>% 
  mutate(technology = case_when(technology == "Social media" ~ 
                                  "Social media have",
                                technology == "Social media mining" ~ 
                                  "Social media mining has",
                                technology == "3D technology to improve experience" ~ 
                                  "3D technology to improve CS experience",
                                TRUE ~ technology))

techs$technology[1] <- "Social media have"
techs$technology[35] <- "3D technology to improve CS experience"
techs$technology[39] <- "Social media mining has"

temp <- data.frame()

for(t in techs$technology){
  if(is.na(t)){
    next
  }
  sub <- scores %>% 
    filter(technology == t) %>% 
    select(date_time, 
           coder,
           technology,
           matches(t))
  
  colnames(sub) <- c("date_time",
                      "coder",
                      "technology",
                      "audience",
                      "engagement_others",
                      "engagement_feedback",
                      "application",
                      "new_data",
                      "extend_data",
                      "improve_quality",
                      "improve_flow",
                      "improve_curation")
  
  if(nrow(temp) == 0){
    temp <- sub
  }else{
    temp <- rbind(temp, sub)
  }
}



#bind_rows(temp) -> scores_tidy

## ---- data: recode to numeric ----
sapply(temp[,4:12], recode_plyr) -> scores_rec
bind_cols(temp[,1:3],scores_rec) -> scores_num

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



## ---- assessors (coders) per technology ----
temp %>% 
  group_by(technology) %>% 
  summarise(n = n()) -> summary_coders_tech

summary_coders_tech %>% 
  ggplot(aes(reorder(technology, n), n)) + 
  geom_col(aes(fill = n)) +
  scale_fill_gradient2(low = "red",
                       high = "blue",
                       midpoint = 4) +
  coord_flip() + 
  theme_minimal() + 
  # scale_x_continuous(breaks = c(2,4,6,8,10,12)) +
  labs(x = "Technology")


## ---- no. 'I don't know' per technology ----
# scores_tidy[scores_tidy == "I don't know"]
# scores_tidy[scores_tidy == "N/A"] 

scores_tidy_long %>% 
  group_by(technology, criterion) %>% 
  count(rank) %>% 
  filter(rank == "I don't know" | rank == "N/A") -> unknown_criteria

scores_tidy_long %>% 
  group_by(technology) %>% 
  count(rank) %>% 
  filter(rank == "I don't know" | rank == "N/A") %>%
  summarise(tot = sum(n)) -> unknown

unknown %>% 
  ggplot(aes(reorder(technology, tot), tot)) + 
  geom_col(aes(fill = tot)) +
  scale_fill_gradient2(low = "red",
                       high = "blue",
                       midpoint = 4) +
  coord_flip() + 
  theme_minimal() + 
  # scale_x_continuous(breaks = c(2,4,6,8,10,12)) +
  labs(x = "Technology")


## ---- basic stats ----
scores_num_long %>% 
  group_by(technology, criterion) %>% 
  summarise(min = min(rank, na.rm = TRUE),
            q1 = quantile(rank, 0.25, na.rm = TRUE),
            median = median(rank, na.rm = TRUE),
            mean = mean(rank, na.rm=TRUE),
            q3 = quantile(rank, 0.75, na.rm = TRUE),
            max = max(rank, na.rm = TRUE))

# grouped violinplots
scores_num_long %>% 
  filter(technology == techs[1,1]) %>% 
  ggplot(aes(x=criterion, y=rank)) + 
  geom_violin() 

# balloon plots
scores_num_long %>% 
  group_by(technology, criterion, rank) %>% 
  summarise(Freq=n()) %>% 
  filter(technology == techs[2,1]) -> prova
ggballoonplot(prova, x = "criterion", y = "rank",
              fill = "Freq", size = "Freq",
              ggtheme = theme_gray())


## ---- icc ----
library(irr)
scores_num %>% 
  filter(technology == techs[1,1]) %>% 
  select(audience:improve_curation) %>% 
  t() -> prova
icc(as.data.frame(prova), model="twoway", type="agreement")
kripp.alpha(t(prova), method = "ordinal")

