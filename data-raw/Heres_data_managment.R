#############################################################
###Program name: Heres_data_managment_models
###Last Updated : 9-10-2024
###Purpose: Clean the raw Hers dataset and run the models for post-selection inference
#################################


# Load the library
library(readxl)
library(MASS)
library(tidyverse)
library(gtsummary)
library(janitor)
library(corrplot)
library(naniar)


# import the data
hersdata <- read_excel("data-raw/hersdata.xls")
hersdata <- clean_names(hersdata)

################# Data management ##############
str(hersdata)

# convert the categorical variables into factors
hersdata <- hersdata %>%
  mutate(
    ht = factor(ht, levels = c("placebo", "hormone therapy")),
    raceth = factor(raceth, levels = c("White", "African American", "Other")),
    nonwhite = factor(nonwhite, levels = c("no", "yes")),
    smoking = factor(smoking, levels = c("no", "yes")),
    drinkany = factor(drinkany, levels = c("no", "yes")),
    exercise = factor(exercise, levels = c("no", "yes")),
    physact = factor(physact, levels=c("much less active", "somewhat less active", "about as active", "somewhat more active", "much more active")),
    globrat= factor(globrat, levels=c("poor", "fair", "good", "very good", "excellent")),
    poorfair = factor(poorfair, levels = c("no", "yes")),
    medcond  = factor(medcond , levels =c(0,1), labels= c("no", "yes")),
    htnmeds  = factor(htnmeds , levels = c("no", "yes")),
    statins = factor(statins , levels = c("no", "yes")),
    diabetes = factor(diabetes , levels = c("no", "yes")),
    dmpills = factor(dmpills , levels = c("no", "yes")),
    insulin  = factor(insulin , levels = c("no", "yes")),

  )


# Look at the initial data with summary stats
hersdata  %>%  tbl_summary(
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} / {N} ({p}%)"
  ),
  digits = all_continuous() ~ 2,
  missing_text = "(Missing)"
)


# Look at the distribution of continuous variables
data_explore <- hersdata %>%  select(where(is.numeric))

for (col in 1:ncol(data_explore)) {
  print(
    ggplot(data_explore, aes_string(x = colnames(data_explore)[col])) +
      geom_histogram() +
      labs(x = colnames(data_explore)[col])
  )
}
#  glucose, glucose1 skewed

# correlation plots
corrplot(cor(data_explore,use='pairwise.complete.obs'))

###### Missing Data exploration ####
hersdata  %>% miss_var_summary()
miss_case_table(hersdata )


# only 7-8% is missing

# do complete cases
hers_comp = hersdata[complete.cases(hersdata), ]

# create a design matrix with std variables
x_raw= hers_comp   %>%  select(-hdl1,-nonwhite, -poorfair, -age10, -tchol1,-weight1, -bmi1,-waist1,-whr1,-glucose1,
                               -tchol,-tchol1,-ldl1,-tg1)
x.std_df = x_raw %>%
  mutate_if(is.numeric, scale)


# Check if variable is standadrized
x.std_df %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} / {N} ({p}%)"
    ),
    digits = all_continuous() ~ 4,
    missing_text = "(Missing)",
    type = list(
      c(age, weight, bmi, waist, whr, glucose, ldl, hdl, tg, sbp, dbp) ~ "continuous"
    )
  )

colnames(x.std_df) <- colnames(x_raw)


# Join the standardize matrix with outcome again
std_data <- cbind(hdl1=hers_comp$hdl1,x.std_df )
raw_data <- cbind(hdl1=hers_comp$hdl1,x_raw )

#  select the variables for analysis
#saveRDS(std_data , "hers_clean.rds")

usethis::use_data(raw_data, overwrite = TRUE)



