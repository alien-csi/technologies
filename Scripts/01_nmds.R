#############################################+
## Ordination analyses
## Valentina LM for the CostAction Team
## First draft: 2022-04-26
#############################################+

#############################################+
## TO DOs
## sum of the scores per tech, weighted by the
## number of assessments
#############################################+

## ---- packages ----
library(tidyverse)
library(ggpubr)
library(vegan)

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



## ---- NMDS overall ----
dataset_tot <- na.omit(scores_num)
dataset_tot_NMDS=metaMDS(dataset_tot[,4:12],k=2,trymax=100)
ordiplot(dataset_tot_NMDS,type="n")
ordihull(dataset_tot_NMDS,groups=dataset_tot$technology,draw="polygon",col="grey90",label=T)
# ordiellipse(dataset_tot_NMDS,groups=dataset$technology,draw="polygon",col="grey90",label=T)
# orditorp(dataset_tot_NMDS,display="species")
# orditorp(example_NMDS,display="sites",col="red",air=0.01)

# # investigate the criteria that drive the technologies distribution pattern (intrinsic criteria)
# dataset.spp.fit <- envfit(dataset_tot_NMDS, dataset_tot, permutations = 999)
# head(dataset.spp.fit) # they are all signif. 



## ---- NMDS overall ggplots ----
dataset_tot_NMDS=metaMDS(dataset_tot[,4:12],k=2,trymax=100)
dataset.spp.fit <- envfit(dataset_tot_NMDS, dataset_tot, permutations = 999) # this fits species (criteria) vectors

site.scrs <- as.data.frame(scores(dataset_tot_NMDS, display = "sites")) #save NMDS results into dataframe
site.scrs <- cbind(site.scrs, Technology = dataset_tot$technology, coder=dataset_tot$coder) #add grouping variable "Technology" to dataframe
head(site.scrs) # x,y points for each site

spp.scrs <- as.data.frame(scores(dataset.spp.fit, display = "vectors")) #save criteria intrinsic values into dataframe
spp.scrs <- cbind(spp.scrs, Criteria = rownames(spp.scrs)) #add criteria names to dataframe
spp.scrs <- cbind(spp.scrs, pval = dataset.spp.fit$vectors$pvals) #add pvalues to dataframe so you can select species(criteria) which are significant
#spp.scrs<- cbind(spp.scrs, abrev = abbreviate(spp.scrs$Species, minlength = 6)) #abbreviate species names
sig.spp.scrs.overall <- subset(spp.scrs, pval<=0.05) #subset data to show species significant at 0.05
head(spp.scrs)

nmds.plot.overall <- ggplot(site.scrs, aes(x=NMDS1, y=NMDS2))+ #sets up the plot
  geom_point(aes(NMDS1, NMDS2, colour = factor(site.scrs$Technology), size = 2),
             show.legend = FALSE)+ #adds site points to plot, shape determined by Landuse, colour determined by Management
  coord_fixed()+
  theme_classic()+ 
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  labs(colour = "Technology") # add legend labels
  # theme(legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12), axis.text = element_text(size = 10)) # add legend at right of plot

nmds.plot.overall # + labs(title = "Basic ordination plot")
#Significant Species (Criteria)
nmds.plot.overall +
  geom_segment(data = sig.spp.scrs.overall, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3) + #add vector arrows of significant species
  ggrepel::geom_text_repel(data = sig.spp.scrs, aes(x=NMDS1, y=NMDS2, label = Criteria), cex = 3, direction = "both", segment.size = 0.25)+ #add labels for species, use ggrepel::geom_text_repel so that labels do not overlap
  labs(title = "Ordination with criteria vectors")


## ---- NMDS single techs ggplots ----
# based on overall NMDS

i = 1
while (i <= dim(techs)[1]) {
  tech_plot <- techs[[i,1]]
  site.scrs.s <- filter(site.scrs, Technology == tech_plot)
  
  nmds.plot <- ggplot(site.scrs.s, aes(x=NMDS1, y=NMDS2))+ #sets up the plot
    geom_point(aes(NMDS1, NMDS2, colour = factor(site.scrs.s$Technology), size = 2),
               show.legend = FALSE)+ #adds site points to plot, shape determined by Landuse, colour determined by Management
    coord_fixed()+
    theme_classic()+ 
    theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
    labs(colour = "Technology") +
    scale_x_continuous(limits = c(-0.5,0.9)) +
    scale_y_continuous(limits = c(-0.5,0.5))# add legend labels
  # theme(legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12), axis.text = element_text(size = 10)) # add legend at right of plot
  
  # nmds.plot +
  #   geom_segment(data = sig.spp.scrs.overall, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3) + #add vector arrows of significant species
  #   ggrepel::geom_text_repel(data = sig.spp.scrs.overall, aes(x=NMDS1, y=NMDS2, label = Criteria), cex = 3, direction = "both", segment.size = 0.25)+ #add labels for species, use ggrepel::geom_text_repel so that labels do not overlap
  #   labs(title = paste("Ordination with criteria vectors: ",tech_plot,sep=""))
  #   # geom_text(label=site.scrs.s$coder, hjust=0, vjust=0)
  
  nmds.plot +
    # geom_text(label=site.scrs.s$coder, hjust=0, vjust=0) +
    labs(title = paste("Ordination with criteria vectors: ",tech_plot,sep=""))
  ggsave(paste("figs/ordination_",tech_plot,".tiff",sep=""), 
                dpi=300, compression = 'lzw')
  
  i = i + 1
}
  
  
## ---- NMDS single techs ----
dataset_NMDS <- vector("list",dim(techs)[1])
dataset.spp.fit <- vector("list",dim(techs)[1])
dataset <- vector("list",dim(techs)[1])

i = 1
while (i <= dim(techs)[1]) {
  tech_plot <- techs[[i,1]]
  dataset_tot %>% 
    filter(technology == tech_plot) -> dataset[[i]]
  dataset_NMDS[[i]] = metaMDS(dataset[[i]][,4:12],k=2,trymax=100)
  # plot(example_NMDS)
  # orditorp(example_NMDS,display="sites",col="red",air=0.01)
  ordiplot(dataset_NMDS[[i]],type="n")
  ordihull(dataset_NMDS[[i]],groups=dataset[[i]]$technology,draw="polygon",col="grey90",label=T)
  # ordiellipse(dataset_NMDS,groups=dataset$technology,draw="polygon",col="grey90",label=T)
  # orditorp(example_NMDS,display="species")
  # orditorp(example_NMDS,display="sites")
  
  dataset.spp.fit[[i]] <- envfit(dataset_NMDS[[i]], dataset[[i]], permutations = 999)
  # head(dataset.spp.fit)
  
  i=i+1
}

# # investigate the criteria that drive the technologies distribution pattern (intrinsic criteria)
# tech_plot <- techs[[1,1]] # 6, 15, 33
# dataset_tot %>% 
#   filter(technology == tech_plot) -> dataset
# dataset_NMDS=metaMDS(dataset[,4:12],k=2,trymax=100)
# # plot(example_NMDS)
# # orditorp(example_NMDS,display="sites",col="red",air=0.01)
# ordiplot(dataset_NMDS,type="n")
# ordihull(dataset_NMDS,groups=dataset$technology,draw="polygon",col="grey90",label=T)
# # ordiellipse(dataset_NMDS,groups=dataset$technology,draw="polygon",col="grey90",label=T)
# orditorp(example_NMDS,display="species")
# dataset.spp.fit <- envfit(dataset_NMDS, dataset, permutations = 999)
# head(dataset.spp.fit)

i = 1
site.scrs <- as.data.frame(scores(dataset_NMDS[[i]], display = "sites")) #save NMDS results into dataframe
site.scrs <- cbind(site.scrs, Technology = dataset[[i]]$technology, coder=dataset[[i]]$coder) #add grouping variable "Technology" to dataframe
head(site.scrs) # x,y points for each site

spp.scrs <- as.data.frame(scores(dataset.spp.fit[[i]], display = "vectors")) #save criteria intrinsic values into dataframe
spp.scrs <- cbind(spp.scrs, Criteria = rownames(spp.scrs)) #add criteria names to dataframe
spp.scrs <- cbind(spp.scrs, pval = dataset.spp.fit[[i]]$vectors$pvals) #add pvalues to dataframe so you can select species(criteria) which are significant
#spp.scrs<- cbind(spp.scrs, abrev = abbreviate(spp.scrs$Species, minlength = 6)) #abbreviate species names
sig.spp.scrs <- subset(spp.scrs, pval<=0.05) #subset data to show species significant at 0.05
head(spp.scrs)

nmds.plot.s <- ggplot(site.scrs, aes(x=NMDS1, y=NMDS2, label=coder))+ #sets up the plot
  geom_point(aes(NMDS1, NMDS2, colour = factor(site.scrs$Technology), size = 2),
             show.legend = FALSE)+ #adds site points to plot, shape determined by Landuse, colour determined by Management
  geom_text(hjust=0, vjust=0)+
  coord_fixed()+
  theme_classic()+ 
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  labs(colour = "Technology") # add legend labels
# theme(legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12), axis.text = element_text(size = 10)) # add legend at right of plot

nmds.plot.s # + labs(title = "Basic ordination plot")
#Significant Species (Criteria)
nmds.plot.s +
  geom_segment(data = sig.spp.scrs, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3) + #add vector arrows of significant species
  ggrepel::geom_text_repel(data = sig.spp.scrs, aes(x=NMDS1, y=NMDS2, label = Criteria), cex = 3, direction = "both", segment.size = 0.25)+ #add labels for species, use ggrepel::geom_text_repel so that labels do not overlap
  labs(title = "Ordination with criteria vectors")

nmds.plot.s +
  labs(title = "Ordination with criteria vectors")




# function for ellipsess 
veganCovEllipse <- function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

# DA RIVEDERE
#data for ellipse, in this case using the management factor
df_ell.tech.criteria <- data.frame() #sets up a data frame before running the function.
for(g in levels(site.scrs$Technology)){
  df_ell.tech.criteria <- rbind(df_ell.tech.criteria, cbind(as.data.frame(with(site.scrs [site.scrs$Technology==g,],
                                                                                   veganCovEllipse(cov.wt(cbind(NMDS1,NMDS2),wt=rep(1/length(NMDS1),length(NMDS1)))$cov,center=c(mean(NMDS1),mean(NMDS2))))) ,Technology=g))
}

# data for labelling the ellipse
NMDS.mean.tech=aggregate(site.scrs[ ,c("NMDS1", "NMDS2")], 
                         list(group = site.scrs$Technology), mean)

# data for labelling the ellipse
NMDS.mean=aggregate(site.scrs[,c("NMDS1", "NMDS2")], 
                    list(group = site.scrs$Technology), mean)
nmds.plot+ 
  geom_path(data = df_ell.tech.criteria, aes(x = NMDS1, y = NMDS2, group = site.scrs$Technology)) #this is the ellipse, seperate ones by Site. 

