#############################################+
## Exploratory analyses
## Valentina LM for the CostAction Team
## First draft: 2022-04-25
#############################################+

## ---- packages ----
library(tidyverse)
library(ggpubr)
library(irr)
library(lme4)

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
#bind_rows(temp) -> scores_tidy

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



## ---- assessors (coders) per technology ----
temp %>% 
  group_by(technology) %>% 
  summarise(n = n()) -> summary_coders_tech
# write.csv(summary_coders_tech, "output/summary_coders_tech.csv")

summary_coders_tech %>% 
  ggplot(aes(reorder(technology, n), n)) + 
  geom_col(aes(fill = n)) +
  scale_fill_gradient2(low = "blue",
                       high = "red") +
  coord_flip() + 
  theme_minimal() +
  scale_y_continuous(breaks = c(2,4,6,8,10,12)) +
  labs(x = "Technology", y = "No. of coders")
# ggsave("summary_coders_tech.tiff",
#        dpi=300, compression = 'lzw')

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
  scale_fill_gradient2(low = "blue",
                       high = "red") +
  coord_flip() + 
  theme_minimal() + 
  scale_y_continuous(breaks = c(2,4,6,8,10,12)) +
  labs(x = "Technology", y = "No. of I don't know / N/A")
# ggsave("figs/summary_unknowns.tiff",
#        dpi=300, compression = 'lzw')


## ---- basic stats ----
# per criteria, regardless of technology
scores_num_long %>% 
  group_by(criterion) %>% 
  summarise(min = min(rank, na.rm = TRUE),
            q1 = quantile(rank, 0.25, na.rm = TRUE),
            median = median(rank, na.rm = TRUE),
            mean = mean(rank, na.rm=TRUE),
            q3 = quantile(rank, 0.75, na.rm = TRUE),
            max = max(rank, na.rm = TRUE))

scores_num_long %>% 
  group_by(technology, criterion) %>% 
  summarise(min = min(rank, na.rm = TRUE),
            q1 = quantile(rank, 0.25, na.rm = TRUE),
            median = median(rank, na.rm = TRUE),
            mean = mean(rank, na.rm=TRUE),
            q3 = quantile(rank, 0.75, na.rm = TRUE),
            max = max(rank, na.rm = TRUE))


## ---- stats and plots per criteria ----
dim(scores_num_long)
# regardless of technology
scores_num_long %>% 
  ggplot(aes(x=criterion, y=rank)) + 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  geom_violin() 
ggsave("figs/criteria_violin.tiff",
       dpi=300, compression = 'lzw')
# balloon plots
scores_num_long %>% 
  group_by(criterion, rank) %>% 
  summarise(Freq=n()) -> pp
# dim(pp)
ggballoonplot(pp, x = "criterion", y = "rank",
              fill = "Freq", size = "Freq",
              ggtheme = theme_gray()) +
  scale_y_continuous(limits = c(1,5))
ggsave("figs/criteria_balloon.tiff",
       dpi=300, compression = 'lzw')

scores_num %>%
  select(audience:improve_curation) %>%
  t() -> scores_matrix
# icc(as.data.frame(scores_matrix), type="consistency",
#     model="twoway", unit = "average") -> icc_crit
kripp.alpha(t(scores_matrix), method = "ordinal") -> kripp_alpha_crit
kripp_alpha_crit # doesn't make sense to measure agreement across technologies
kappam.fleiss(scores_matrix)

## ---- stats and plots per tech ----

i = 1

icc_value <- vector("numeric",dim(techs)[1])
icc_lbound <- vector("numeric",dim(techs)[1])
icc_ubound <- vector("numeric",dim(techs)[1])
kripp_value <- vector("numeric",dim(techs)[1])
fleiss_value <- vector("numeric",dim(techs)[1])
fleiss_z <- vector("numeric",dim(techs)[1])
fleiss_p <- vector("numeric",dim(techs)[1])
n <- vector("numeric",dim(techs)[1])
anova.s <- vector("list",dim(techs)[1])
p_anova <- vector("numeric",dim(techs)[1])

while (i <= dim(techs)[1]) {
  tech_plot <- techs[[i,1]]
  # # grouped violinplots
  # scores_num_long %>% 
  #   filter(technology == tech_plot) %>% 
  #   ggplot(aes(x=criterion, y=rank)) + 
  #   scale_x_discrete(guide = guide_axis(angle = 45)) +
  #   labs(title = tech_plot) +
  #   geom_violin() 
  # ggsave(paste("figs/",tech_plot,"_violin.tiff",sep=""),
  #        dpi=300, compression = 'lzw')
  # # balloon plots
  # scores_num_long %>% 
  #   group_by(technology, criterion, rank) %>% 
  #   summarise(Freq=n()) %>% 
  #   filter(technology == tech_plot) -> pp
  # # dim(pp)
  # ggballoonplot(pp, x = "criterion", y = "rank",
  #               fill = "Freq", size = "Freq",
  #               ggtheme = theme_gray()) +
  #   scale_y_continuous(limits = c(1,5)) +
  #   labs(title = tech_plot)
  # ggsave(paste("figs/",tech_plot,"_balloon.tiff",sep=""),
  #        dpi=300, compression = 'lzw')
  
  ## ---- icc, Krippendorf's , Fleiss ----
  scores_num %>% 
    filter(technology == tech_plot) %>% 
    select(audience:improve_curation) %>% 
    t() -> scores_matrix
  matrix <- na.omit(scores_matrix)
  if (dim(matrix)[1] != 0) {
    icc(as.data.frame(matrix), type="consistency",
        model="twoway", unit = "average") -> icc_tech
    kripp.alpha(t(matrix), method = "ordinal") -> kripp_alpha_tech
    kappam.fleiss(matrix) -> fleiss_tech
    
    ## ---- anova ----
    na.omit(scores_num_long) %>% 
      filter(technology == tech_plot) -> dataset_long
    lmer(rank ~ coder + (1 | criterion), data = dataset_long, REML=TRUE) -> mod
    # summary(lmer(rank ~ coder + (1 | criterion), data = dataset_long, REML=TRUE)) -> mod.summary
    anova(lmer(rank ~ coder + (1 | criterion), data = dataset_long, REML=TRUE)) -> anova.s[[i]]
    qf(p=.05, df1=8, df2=dim(mod@frame)[1]-9, lower.tail=FALSE) -> threshold
    # The degrees of freedom for the numerator are the degrees of freedom for the between group (k-1)
    # and the degrees of freedom for the denominator are the degrees of freedom 
    # for the within group (N-k)
    if (anova.s[[i]]$`F value` > threshold) p_anova[i] <- "< 0.05" else p_anova[i] <- NA

    icc_value[i] <- icc_tech$value
    icc_lbound[i] <- icc_tech$lbound
    icc_ubound[i] <- icc_tech$ubound
    kripp_value[i] <- kripp_alpha_tech$value
    fleiss_value[i] <- fleiss_tech$value
    fleiss_z[i] <- fleiss_tech$statistic
    fleiss_p[i] <- fleiss_tech$p.value
    n[i] <- fleiss_tech$raters
    
  } else {
    icc_value[i] <- NA
    icc_lbound[i] <- NA
    icc_ubound[i] <- NA
    kripp_value[i] <- NA
    fleiss_value[i] <- NA
    fleiss_z[i] <- NA
    fleiss_p[i] <- NA
    n[i] <- NA
  }
  
  i = i + 1
} 

data.frame(
  technology = techs[[1]],
  icc = icc_value,
  icc.lbound = icc_lbound,
  icc.ubound = icc_ubound,
  kripp = kripp_value,
  fleiss = fleiss_value,
  fleiss.z = fleiss_z,
  fleiss.p = fleiss_p,
  p.anova = p_anova,
  num = n
) -> irr_table_temp

left_join(irr_table_temp, summary_coders_tech) %>% 
  rename(no.coders = n) -> irr_table
write.csv(irr_table, "output/irr_table_new.csv")

## Fleiss: a significant p-value means the stat is significantly different from 0 (agreement)



#Fleiss kappa as an index of inter-rater agreement between m raters on categorical data
#kappam.fleiss(prova)
#scores_num

# piece of code taken from manageability analysis
# Interrater Reliability is the degree of agreement among raters. It is a score of how much homogeneity or consensus exists in the ratings given by various judges (wiki).
# # There are a number of statistics that can be used to determine inter-rater reliability, we will test:
# 1. Krippendorff's alpha
# 2. Cronbachs alfa
# 3. Cohen's Kappa

# # 1. Krippendorff's alpha
# 
# ## Per species

# # every row of the input matrix is one observer
# species_un <- unique(assessment_results$species)
# 
# per_species_erad <- as.data.frame(species_un)
# 
# for (i in 1:length(species_un)) {
#   set1 <- assessment_results %>% 
#     filter(species == species_un[i],
#            scenario == "eradication") %>%
#     select(assessor, criterium, score) %>% 
#     spread(criterium, score) %>% 
#     select(-assessor) %>% 
#     as.matrix()
#   
#   per_species_erad$kr_alpha[i] <- kripp.alpha(set1, method = "ordinal")$value 
# }
# 
# data.table(per_species_erad)
# 
# Acceptable levels only for three species: Ondatra zibethicus (0.787), Sciurus niger (0.700) and Gunnera tinctoria (0.690).
# 
# per_species_spread <- as.data.frame(species_un) %>% 
#   mutate(kr_alpha = NA)
# 
# for (i in 1:length(species_un)) {
#   set1 <- assessment_results %>% 
#     filter(species == species_un[i],
#            scenario == "spread limitation") %>%
#     select(assessor, criterium, score) %>% 
#     spread(criterium, score) %>% 
#     select(-assessor) %>% 
#     as.matrix()
#   
#   per_species_spread$kr_alpha[i] <- kripp.alpha(set1, method = "ordinal")$value 
# }
# 
# data.table(per_species_spread)
# 
# Acceptable levels only for two species: Gunnera tinctoria (0.846) en Ondatra zibethicus (0.680).
# 
# ## Per functional group
# 
# Doubtfull if this is allowed, since the same assesors scored multiple species.
# combo_un <- unique(assessment_results$combo)
# 
# per_combo <- as.data.frame(rep(combo_un,2))
# 
# for (i in 1:length(combo_un)) {
#   set1 <- assessment_results %>% 
#     filter(combo == combo_un[i],
#            scenario == "eradication") %>%
#     mutate(ID = paste(assessor, species)) %>% 
#     select(ID, criterium, score) %>% 
#     spread(criterium, score) %>% 
#     select(-ID) %>% 
#     as.matrix()
#   
#   per_combo$scen <- "eradication"
#   per_combo$kr_alpha[i] <- kripp.alpha(set1, method = "ordinal")$value
# }
# 
# for (i in 1:length(combo_un)) {
#   set1 <- assessment_results %>% 
#     filter(combo == combo_un[i],
#            scenario == "spread limitation") %>%
#     mutate(ID = paste(assessor, species)) %>% 
#     select(ID, criterium, score) %>% 
#     spread(criterium, score) %>% 
#     select(-ID) %>% 
#     as.matrix()
#   
#   per_combo$scen[i + 4] <- "spread limitation"
#   per_combo$kr_alpha[i + 4] <- kripp.alpha(set1, method = "ordinal")$value
# }
# 
# data.table(per_combo)
# 
# All very low, what is the effect of looking only at feasibility?
#   
# per_combo_fs <- as.data.frame(rep(combo_un,2))
# 
# for (i in 1:length(combo_un)) {
#   set1 <- assessment_results %>% 
#     filter(combo == combo_un[i],
#            scenario == "eradication") %>%
#     mutate(ID = paste(assessor, species)) %>% 
#     select(ID, criterium, score) %>% 
#     spread(criterium, score) %>%
#     mutate(feasibility = (acceptability + cost + effectiveness + impact +
#                             `likelihood of reintroduction` + practicality + 
#                             `window of opportunity`)/7) %>% 
#     select(feasibility) %>% 
#     as.matrix()
#   
#   per_combo_fs$scen <- "eradication"
#   per_combo_fs$kr_alpha[i] <- kripp.alpha(set1, method = "ordinal")$value
# }
# 
# for (i in 1:length(combo_un)) {
#   set1 <- assessment_results %>% 
#     filter(combo == combo_un[i],
#            scenario == "spread limitation") %>%
#     mutate(ID = paste(assessor, species)) %>% 
#     select(ID, criterium, score) %>% 
#     spread(criterium, score) %>%
#     mutate(feasibility = (acceptability + cost + effectiveness + impact +
#                             `likelihood of reintroduction` + practicality + 
#                             `window of opportunity`)/7) %>% 
#     select(feasibility) %>% 
#     as.matrix()
#   
#   per_combo_fs$scen[i + 4] <- "spread limitation"
#   per_combo_fs$kr_alpha[i + 4] <- kripp.alpha(set1, method = "ordinal")$value
# }
# 
# data.table(per_combo_fs)
# 
# Alpha is even lower, Krippendorff's alpha should be used to compare differnt measurements per observation.
# 
