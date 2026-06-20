# ==============================================================================
# rBasicAnalysisToolkit(rBAT) v1_0_0 by wisaimtiac

# ██████╗    /\   {}_{}   /\       A reusable collection of R-code snippets
# ██╔══██╗  /  \__(ö ö)__/  \      to clean, explore and analyze data from e.g.:
# ██████╔╝ /    ,_|\./|_,    \     psychological experiments or questionnaires.
# ██╔══██╗ ~~~~'  |   |  '~~~~     The script runs "out of the box" with 
# ██║  ██║        \___/            simulated data (section 2a), so testing can
# ╚═╝  ╚═╝        /   \            be done without an external dataset.

# To analyze your own data, remove comments on the import (section 2b)
# and comment out the data simulation block (section 2a) to skip it.

# ATTENTION:
# The comment "# RSH!" signifies where you should "Rename Something Here!",
# to make the script use variables, etc. matching your personal data & analyses.


# ------------------------------------------------------------------------------
# 1. SETUP & PACKAGES

pakete  <- c("tidyverse", "psych", "afex", "car", "pwr")
missingPack <- setdiff(pakete, rownames(installed.packages()))
if (length(missingPack) > 0) install.packages(missingPack)
invisible(lapply(pakete, library, character.only = TRUE))

set.seed(23)  # For reproducability.

theme_set(theme_minimal(base_size = 12)) # Presetting uniform plot-design once.


# ------------------------------------------------------------------------------
# 2a. SIMULATION OF EXAMPLE DATA (to preprocess and analyse your own data
#                                 go to section 2b and comment out this block)

n <- 200
dataRaw <- tibble(
  ID_ORIGINAL = 1:n,
  AGE_VAR     = round(rnorm(n, mean = 24, sd = 5)),
  GENDER_VAR  = sample(c(1, 2, 3), n, replace = TRUE, prob = c(.50, .45, .05)),
  GROUP_VAR   = sample(c("A", "B", "C"), n, replace = TRUE)
)
items_sim <- as_tibble(matrix( # Adding Likert-scale items (1-7)
  sample(1:7, n * 10, replace = TRUE), nrow = n, 
  dimnames = list(NULL, paste0("ITEM_", sprintf("%02d", 1:10)))
))
dataRaw <- bind_cols(dataRaw, items_sim)
dataRaw$AGE_VAR[sample(n, 5)] <- NA # Scatter a few missing values.


# ------------------------------------------------------------------------------
# 2b. IMPORTING YOUR OWN DATA  (remove comments to activate and put comments on 2a)

# Provide your CSV file:
# dataRaw <- read_csv2(
#   "YOUR_FILENAME_HERE.csv",                                              		# RSH!
#   na     = c("-9", "NA", ""),  # How are missings coded?
#   locale = locale(decimal_mark = ",", grouping_mark = ".")
# )

glimpse(dataRaw)


# ------------------------------------------------------------------------------
# 3. CLEANING & RECODING

# Function to revert inverted items - general, not fixed to 7 steps.
reversePol <- function(x, max, min = 1) (max + min) - x            				# RSH!

# List items, comprising a certain questionnaire (Item 3 used in reverted form).
# Defined once, reused everywhere.
list_of_items <- c("item01", "item02", "item03_R", "item04", "item05")			# RSH!

# 3.1 Choosing & renaming items/variables.
dataClean <- dataRaw %>%
  select(
    id     = ID_ORIGINAL,                                      					# RSH!
    age    = AGE_VAR,
    gender = GENDER_VAR,
    group = GROUP_VAR,
    num_range("ITEM_", 1:10, width = 2) # loop thorugh ITEM_01 to ITEM_10. 
  ) %>%
  # ITEM_01 -> item01, ... (unifying lower case renaming).                 	
  rename_with(~ str_replace(.x, "ITEM_", "item"), starts_with("ITEM_")) %>%     # RSH!
  
  # 3.2 Recoding: choice options as a factor + revert item 3. 
  mutate(
    genderF = factor(case_when(
      gender == 1 ~ "female",                    								# RSH!
      gender == 2 ~ "male", 
      gender == 3 ~ "diverse",    
      TRUE ~ NA_character_ # Convert unknown to real missing. 
    )),
    group   = factor(group),
    item03_R = reversePol(item03, max = 7) # 7-step Likert-Scale
  ) %>%
  
  # 3.3 Filtering (e.g.: only above certain age).
  filter(age >= 18) %>%                                							# RSH!
  
  # 3.4 Calculating Subscales -- vectorzied instead of rowwise(). 
  mutate(
    list_mean = rowMeans(across(all_of(list_of_items)), na.rm = TRUE),          # RSH!
    skala_sum  = rowSums(across(all_of(list_of_items)),  na.rm = TRUE)
  )

# na.rm = TRUE returns a mean, even though only a small number of
# items have been answered. More strict is a minimum number of valid items:     # RSH!
#   dataClean <- dataClean %>%
#     mutate(valide = rowSums(!is.na(across(all_of(list_of_items)))),
#            list_mean = if_else(valide >= 4, list_mean, NA_real_))
#                         # In this example, there would need to be at least 
#                         # **four** valid answers, for values to be included.


# ------------------------------------------------------------------------------
# 4. CALCULATING RELIABILITY OF (SUB)SCALE

# ATTENTION: Cronbachs Alpha. psych::alpha --> named explicitly, 
# because scales/ggplot2 have a function named alpha() as well.
psych::alpha(select(dataClean, all_of(list_of_items)))                        	# RSH!


# ------------------------------------------------------------------------------
# 5. DESCRIPTIVE STATISTICS

# Frequency & percentage table of categorial variable(s).
table(dataClean$genderF)                       									# RSH!
prop.table(table(dataClean$genderF))

# Metric characterists - Mean, SD, Min, Max, Skew etc.  
psych::describe(dataClean[, c("age", "list_mean")])                        		# RSH!

# Separated by levels of variable. 
psych::describeBy(dataClean$list_mean, group = dataClean$genderF)               # RSH!


# ------------------------------------------------------------------------------
# 6. DATENVISUALISIERUNG (ggplot2)

# Histogram (for checking distribution)
ggplot(dataClean, aes(x = list_mean)) +                                         # RSH!
  geom_histogram(fill = "steelblue", color = "white", bins = 30) +           
  labs(title = "Distribution of list of items X", x = "items value", y = "Frequency")

# Boxplot (for comparison between levels of variable)
ggplot(dataClean, aes(x = genderF, y = list_mean, fill = genderF)) +            # RSH!
  geom_boxplot() +
  labs(title = "list of items X by gender", x = "gender", y = "items value") +
  theme(legend.position = "none")

# Scatterplot + regression line (correlation) 
ggplot(dataClean, aes(x = age, y = list_mean)) +                                # RSH!
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x, color = "red") +
  labs(title = "Correlation of age & list of items X", x = "age", y = "items value")


# ------------------------------------------------------------------------------
# 7. INFERENTIAL STATISTICS

# --- A) Correlation                                              
cor.test(dataClean$age, dataClean$list_mean, use = "complete.obs")              # RSH!

# --- B1) t-Test
dat_tt <- dataClean %>%                                                     	# RSH!
  filter(genderF %in% c("female", "male")) %>%
  droplevels()

# --- B2) Homogenity of variance                    
car::leveneTest(list_mean ~ genderF, data = dat_tt)                            	# RSH!

# --- B3) Welch-Test
# (var.equal = FALSE) is default & safe choice, independent of Levene-result.
t.test(list_mean ~ genderF, data = dat_tt)   									# RSH!

# --- C) ANOVA (>2 groups) using afex                            
anova_model <- afex::aov_ez( # afex has informative print method.               # RSH!
  id = "id", dv = "list_mean", between = "group", data = dataClean
)
anova_model

# --- D) Linear & multiple regression                      
lm_model <- lm(list_mean ~ age + genderF, data = dataClean)                     # RSH!
summary(lm_model)

# To check requirements: shows all four diagnostic-plots at once.
par(mfrow = c(2, 2)); plot(lm_model); par(mfrow = c(1, 1))               		# RSH!
car::vif(lm_model)   # Multi collinearity (Values > ~5 are critical!)


# ------------------------------------------------------------------------------
# 8. POWER-ANALYSIS

# What sample size is required, to be able to possibly discover a middle strong 
# effect size (d = 0.5) for the t-test with 80 % power with alpha = .05?     
pwr::pwr.t.test(d = 0.5, sig.level = 0.05, power = 0.80, type = "two.sample")   # RSH!

