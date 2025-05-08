################################################
##### Generalized Synthetic Control Method #####
################################################

#################
# ---- SETUP ----
#################

rm(list = ls())

# Setting the working directory, make sure to replace filepath to where the .zip has been downloaded
wd <- "/Users/glpou/Documents/SCIENCESPO/M2/S4/Thesis/1.Package/EEC_03_20"
setwd(wd)  

# Packages & Libraries 
install.packages("gsynth")
install.packages("zoo")

library(gsynth)
library(zoo)

# Importing Data 
df <- read.csv("Data/Clean/gscm.csv")
df$datdeb_q <- as.yearqtr(df$datdeb_q)

################
# ---- GSCM ----
################

vars <- c("sexe", "married", "enfant", "salred", "salred_sq", 
          "age", "age_sq", "annees_etudes", "cat_naf10"
)
colSums(is.na(df[, vars]))

result <- gsynth(tplus ~ treatment + married + age + age_sq, 
                 data = df,
                 index = c("indiv_num", "datdeb_q"),
                 force = "two-way",      # fixed effects for units & time
                 CV = TRUE,
                 r = c(0, 5),            # latent factors (auto-tuned)
                 se = TRUE,
                 inference = "parametric"
)

summary(result)
plot(result)

