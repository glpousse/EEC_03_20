##################################################
# Two-Stage Imputation with Individual Anchoring #
##################################################

# ---- STEPS ---- 
# STEP 1: Estimating a Gradient Boosted Random Forest Regressors Model 
# STEP 2: Anchoring predictions to individual "true" observations

rm(list=ls())
setwd("/Users/glpou/Documents/SCIENCESPO/M2/S4/Thesis/1.Package/EEC_03_20")

# ---- LIBRARIES ----
library(xgboost)
library(haven)
library(tidyr)
library(dplyr)
library(fastDummies)
library(Matrix)

#––––––––––––––––––––––––––
# ---- STEP 1: XGBoost ----
#––––––––––––––––––––––––––

#################
# ---- SETUP ----
#################

df_XGB <- read_dta("Data/Clean/df_XGB.dta")

# Making sure all variables are numerical 
df_XGB$naf10     <- as.factor(df_XGB$naf10) 
df_XGB$datdeb    <- as.Date(df_XGB$datdeb, format = "%d%b%Y")
df_XGB$annee     <- as.numeric(format(df_XGB$datdeb, "%Y"))
df_XGB$mois      <- as.numeric(format(df_XGB$datdeb, "%m"))
df_XGB$jour      <- as.numeric(format(df_XGB$datdeb, "%d"))
df_XGB$jsemaine  <- as.numeric(format(df_XGB$datdeb, "%u"))
df_XGB$jannee    <- as.numeric(format(df_XGB$datdeb, "%j"))
df_XGB$trim      <- (df_XGB$mois - 1) %/% 3 + 1

# Desired hours can be cyclical according to the week or month
df_XGB$mois_sin <- sin(2 * pi * df_XGB$mois / 12)
df_XGB$mois_cos <- cos(2 * pi * df_XGB$mois / 12)

df_XGB$jsemaine_sin <- sin(2 * pi * df_XGB$jsemaine / 7)
df_XGB$jsemaine_cos <- cos(2 * pi * df_XGB$jsemaine / 7)

# Adding a season var (desired hours can be seasonal)
df_XGB$season <- factor(
  ifelse(df_XGB$mois %in% c(12, 1, 2), "Winter",
  ifelse(df_XGB$mois %in% c(3, 4, 5), "Spring",
  ifelse(df_XGB$mois %in% c(6, 7, 8), "Summer",
  "Autumn"))),
  levels = c("Spring", "Summer", "Autumn", "Winter")
)

# CATEGORICAL vars need to be turned to dummies? 
# A lot of these are numerical categories, I make them factor catgories first 
dep_val <- c(
  "1", "2", "2.1", "2.2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12",
  "13", "14", "15", "16", "17", "18", "19", "21", "22", "23", "24", "25", "26",
  "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39",
  "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52",
  "53", "54", "55", "56", "57", "58", "59", "60", "61", "62", "63", "64", "65",
  "66", "67", "68", "69", "70", "71", "72", "73", "74", "75", "76", "77", "78",
  "79", "80", "81", "82", "83", "84", "85", "86", "87", "88", "89", "90", "91",
  "92", "93", "94", "95"
) 
dep_lab <- c(
  "Ain", "Aisne", "Corse-du-Sud", "Haute-Corse", "Allier", "Alpes-de-Haute-Provence", 
  "Hautes-Alpes", "Alpes-Maritimes", "Ardèche", "Ardennes", "Ariège", "Aube", 
  "Aude", "Aveyron", "Bouches-du-Rhône", "Calvados", "Cantal", "Charente", 
  "Charente-Maritime", "Cher", "Corrèze", "Côte-d'Or", "Côtes-d'Armor", "Creuse", 
  "Dordogne", "Doubs", "Drôme", "Eure", "Eure-et-Loir", "Finistère", "Gard", 
  "Haute-Garonne", "Gers", "Gironde", "Hérault", "Ille-et-Vilaine", "Indre", 
  "Indre-et-Loire", "Isère", "Jura", "Landes", "Loir-et-Cher", "Loire", 
  "Haute-Loire", "Loire-Atlantique", "Loiret", "Lot", "Lot-et-Garonne", "Lozère", 
  "Maine-et-Loire", "Manche", "Marne", "Haute-Marne", "Mayenne", 
  "Meurthe-et-Moselle", "Meuse", "Morbihan", "Moselle", "Nièvre", "Nord", 
  "Oise", "Orne", "Pas-de-Calais", "Puy-de-Dôme", "Pyrénées-Atlantiques", 
  "Hautes-Pyrénées", "Pyrénées-Orientales", "Bas-Rhin", "Haut-Rhin", "Rhône", 
  "Haute-Saône", "Saône-et-Loire", "Sarthe", "Savoie", "Haute-Savoie", "Paris", 
  "Seine-Maritime", "Seine-et-Marne", "Yvelines", "Deux-Sèvres", "Somme", 
  "Tarn", "Tarn-et-Garonne", "Var", "Vaucluse", "Vendée", "Vienne", 
  "Haute-Vienne", "Vosges", "Yonne", "Territoire de Belfort", "Essonne", 
  "Hauts-de-Seine", "Seine-Saint-Denis", "Val-de-Marne", "Val-d'Oise"
)
df_XGB$dep <- factor(
    df_XGB$dep,
    levels = dep_val,
    labels = dep_lab
)
df_XGB$dimanc <- factor(
    df_XGB$dimanc,
    levels = c("1","2","3"),
    labels = c("Habituellement","Occasionnellement","Jamais" )
)
df_XGB$samedc <- factor(
    df_XGB$samedc,
    levels = c("1","2","3"),
    labels = c("Habituellement","Occasionnellement","Jamais" )
)
df_XGB$horaic <- factor(
    df_XGB$horaic,
    levels = c("1","2","3",'4'),
    labels = c("memes", "alternes", "variables", "Sans objet")
)
df_XGB$maisoc <- factor(
    df_XGB$maisoc,
    levels = c("1","2","3"),
    labels = c("Habituellement","Occasionnellement","Jamais" )
) 
pnai_codes <- c("NA", "10", "11", "12", "13", "14", "15", "21", "22", "23", "24", "25",
           "26", "27", "28", "29", "31", "32", "41", "42", "43", "44", "45", "46",
           "47", "48", "51", "52", "60", "61"
)
pnai_lab <- labels <- c("Non renseigné", "France", "Algérie", "Tunisie", "Maroc", "Autres pays d'Afrique", "Vietnam, Cambodge, Laos", "Italie", "Allemagne", "Belgique", "Pays-Bas", "Luxembourg", "Irlande", "Danemark", "Royaume-Uni", "Grèce", "Espagne", "Portugal", "Suisse", "Autriche", "Pologne", "Yougoslavie", "Turquie", "Norvège", "Suède", "Autres pays européens (y compris URSS)", "Etats-Unis, Canada", "Autres pays d'Amérique", "Autres pays ou apatrides", "personne étrangère de nationalité non précise")
df_XGB$pnai28 <- factor(
    df_XGB$pnai28,
    levels = pnai_codes,
    labels = pnai_lab
)
nivet_codes <- c("21", "22", "31", "32", "33", "41", "42", "43", "51", "52",  "61", "71", "72", "73")
nivet_lab <- c(  "Troisième cycle universitaire, grande école", "Deuxième cycle universitaire", "Premier cycle universitaire", "DUT, BTS", "Paramédical et social niveau bac+2", "Terminale générale", "Terminale technologique", "Terminale bac pro", "Seconde ou première", "Terminale CAP, BEP", "Troisième seule, CAP-BEP avant l'année terminale", "Sixième, cinquième, quatrième; enseignement spécialisé", "Classes primaires", "Autres cas")
df_XGB$nivet <- factor(
    df_XGB$nivet,
    levels = nivet_codes,
    labels = nivet_lab
)
naf10_codes <- c(
  "00", "BE", "FZ", "GI", "JZ", "KZ", "LZ", "MN", "RU"
)
naf10_lab <- c(
  "Non renseigné",
  "Industrie manufacturière, industries extractives et autres",
  "Construction",
  "Commerce de gros et de détail, transports, hébergement et restauration",
  "Information et communication",
  "Activités financières et d'assurance",
  "Activités immobilières",
  "Activités spécialisées, scientifiques et techniques et activités de services administratifs et de soutien",
  "Autres activités de services"
)
df_XGB$naf10 <- factor(
    df_XGB$naf10,
    levels = naf10_codes,
    labels = naf10_lab 
)

# Individual Level Summary Features
# indiv_stats <- df_XGB %>% 
#     group_by(indiv_num) %>%
#     summarise(
#         hplus_mean_indiv    = mean(hplus, na.rm = TRUE),
#         hplus_sd_indiv      = sd(hplus, na.rm = TRUE)
# )
# df_XGB <- left_join(df_XGB, indiv_stats, by = "indiv_num")

# Adding a lagged hplus (algo can know what was reported by the individual last time)
# df_XGB <- df_XGB %>%
#   arrange(indiv_num, datdeb) %>%
#   group_by(indiv_num) %>%
#   mutate(hplus_lag1 = lag(hplus)
# )

# Making DUMMIES from all factor vars
df_XGB <- dummy_cols(df_XGB,
    remove_first_dummy = TRUE,
    remove_selected_columns = TRUE
)

# Checking var types & making new df
var_classes <- sapply(df_XGB, class) 
write.csv(df_XGB, "Data/Clean/df_guims.csv", row.names = FALSE)

#####################
# ---- USING XGB ----
#####################

# Making sure the training sample is random
set.seed(123)
df_XGB <- df_XGB[sample(1:nrow(df_XGB)), ] 
missing_idx <- which(is.na(df_XGB$hplus))      # saving row indices on missing hplus for when I assign missing values later 

# A training datset and a datset I want to predict 
df_train    <- df_XGB[!is.na(df_XGB$hplus), ]
df_missing  <- df_XGB[is.na(df_XGB$hplus), ]

# Building Model Matrices 
X_train     <- as.matrix(df_train %>% select(-hplus, -ID, -datdeb)) # I'm training on a dataset without the var I want to predict? 
y_train     <- df_train$hplus
X_missing   <- as.matrix(df_missing %>% select(-hplus, -ID, -datdeb))

dtrain      <- xgb.DMatrix(data = X_train, label = y_train)
dmissing    <- xgb.DMatrix(data = X_missing)

# Training XGBoost with cross-validation
cv_model <- xgb.cv(
    data = dtrain,
    nrounds = 1000,                     # Took high nrounds and used the early stopping rounds option to try to max accuracy 
    eta = 0.1,                          # I could make it even slower with 0.01, should I? Lower value (slower) should make it more accurate but would it risk overfitting? 
    nfold = 10,                         # These are the amount of times I resample the data and test, right? 
    objective = "reg:squarederror",     # Going with simple OLS, following the reasoning of "simpler is better", any recs on other models? 
    max_depth = 6,                      # Max depth of each decision tree. From what I gather 6 is a good startiNg point. More than 10 gets deep, risk of overfitting.
    early_stopping_rounds = 10,         # If model doesn't improve after 10 consecutive rounds, stop there. 
    verbose = 0                         # I don't want to see the progress output. 
)

best_nrounds <- cv_model$best_iteration

# Training the final Model 
final_model <- xgboost(
  data = dtrain,
  nrounds = best_nrounds,
  objective = "reg:squarederror",
  eta = 0.1,
  max_depth = 6,
  verbose = 0
)

# > model_worth
#     iter train_rmse_mean train_rmse_std test_rmse_mean test_rmse_std
#    <num>           <num>          <num>          <num>         <num>
# 1:   580        4.597414     0.02855449       6.312216     0.2026866

# Before predicting hplus with the trained model, I want to understand if the model is good 
# MODEL WORTH: 
model_worth <- cv_model$evaluation_log[cv_model$best_iteration, ]
#  iter train_rmse_mean train_rmse_std test_rmse_mean test_rmse_std
#    <num>           <num>          <num>          <num>         <num>
# 1:   247        5.428084     0.02866554       6.396188     0.1961106

# Interpreting what the stats mean in general: 
# - It took 247 boosting rounds in the training stage to best predict hplus (the early stopping feature made it stop before 1000)
# - The average Root Mean Squared Error is 5.42 across the 10 training folds
# - SD of RMSE across training rounds is 0.0286
# - Avg. RMSE on validation folds (where I test the model) is 6.396
# - SD of test RMSE 

# Interpreting what the stats mean for my model: 
# - RMSE and test RMSE are pretty close (5.428 & 6.396), that's a good thing? It indicates I'm not overfitting? 
# - low SDs: model performs consistently throughout testing rounds

# These are the stats for non-missing hplus values in my OG dataset: 
# Variable |        Obs        Mean    Std. dev.       Min        Max
# -------------+---------------------------------------------------------
#        hplus |     46,547    42.44188    7.351367          0      99.59

# - my model RMSE (6.39) is lower than the SD of the mean in the OG data (7.35). This shows my model is successfully learning? 
#        - as in, if I were wanting to predict the mean, I'd be doing so with more accuracy that STATA's summarize function?
# - 
# SO THE MAIN QUESTION IS - SHOULD I TUNE THE HYPERPARAMTERS DIFFERENTLY OR AM I SATISFIED WITH THIS MODEL?

# Adding lagged hplus:
#     iter train_rmse_mean train_rmse_std test_rmse_mean test_rmse_std
#    <num>           <num>          <num>          <num>         <num>
# 1:    97        5.580198     0.02239446       6.129956       0.22231


# Plotting residuals
predicted_train <- predict(final_model, dtrain)
actual_train <- y_train
residuals_train <- actual_train - predicted_train
# Scatter
plot(predicted_train, residuals_train,
     xlab = "Predicted hplus",
     ylab = "Residuals",
     main = "Residual Plot")
abline(h = 0, col = "red", lty = 2)
# Hist 
hist(residuals_train,
     breaks = 50,
     main = "Histogram of Residuals",
     xlab = "Residuals"
)

#–––––––––––––––––––––––––––––––––––––––
# ---- STEP 2: Individual Anchoring ----
#–––––––––––––––––––––––––––––––––––––––

# Now that I have my predict values of hplus, I match the means of an individual's true value and te series of predicted hplus's he gets assigned.
df_XGB$hplus_hat <- NA
df_XGB$hplus_hat[missing_idx] <- predict(final_model, dmissing)
df_XGB <- df_XGB %>% 
    select(ID,hplus_final, hplus, hplus_hat, everything()) %>%
    arrange(ID
)
View(df_XGB[1:2100, ])

