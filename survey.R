library(readxl)
library(dplyr)
assessors <- read_excel("Data/tech_assessors.xlsx")
head(assessors)
assessors_list <- na.omit(assessors$Assessors)
assessors_list
tech_list <- na.omit(assessors$"Approaches/technologies")
tech_list

as.data.frame(table(sample(tech_list, 520, replace = TRUE)))

list.ass <- vector("list", length(assessors_list))
i = 1
set.seed(4)
while (i <= length(tech_list)) {
  list.ass[[i]] <- sample(x = assessors_list, 10, replace = FALSE)
  i = i + 1
}

class(list.ass[[1]])
assignments_temp <- as.data.frame(do.call("rbind",list.ass))
assignments_temp %>%
  mutate(tecnologies = tech_list) -> assignments

as.data.frame(table(unlist(list.ass)))
