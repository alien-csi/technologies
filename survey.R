#'  This code performs a random assignment of 10 technologies per assessor and
#'  taking into account that each technology is evaluated by exactly 10
#'  assessors

library(readxl)
library(dplyr)
assessors <- read_excel("Data/tech_assessors.xlsx")
head(assessors)
assessors_list <- na.omit(assessors$Assessors)
assessors_list
tech_list <- na.omit(assessors$"Approaches/technologies")
tech_list


# n technologies per assessor
n <- 10
# max value allowed to st_deviation of n assessors per technology
st_dev_limit <- 2

st_dev <- 100 # initialize standard eviation
verbose <- FALSE # if TRUE more details are returned on screen

while (st_dev > st_dev_limit) {
  # create matrix with n rows as number of assessors and ncols as number of
  # technologies
  assignments <- matrix(
    rep(0, length(assessors_list)*length(tech_list)),
    nrow = length(assessors_list), 
    ncol = length(tech_list)
  )
  for (i in seq(assessors_list)) {
    if (verbose) {
      message(paste0("Assigning technologies to ",
                   assessors_list[i],
                   " (", i, "/", n, ")"))
    }
    technologies_sample <- sample(
      x = seq(length(tech_list)),
      size = n,
      replace = FALSE
    )
    assignments[i, technologies_sample] <- 1
  }
  st_dev <- sd(colSums(assignments))
  if (st_dev > st_dev_limit) {
    message(paste0(
      "Too high standard deviation: ", st_dev, " > ", st_dev_limit
      )
    )
  } else {
      message(paste0("Standard deviation (", st_dev, ")  is acceptable."))
    }
}

message("Technologies assigned to all assessors.")
message(paste0("Standard deviation: ", st_dev))

colnames(assignments) <- tech_list

assignments <- as_tibble(assignments)

assessors <- assessors %>% select(Assessors)

assignments <- assessors %>% bind_cols(assignments)

assignments
View(assignments)