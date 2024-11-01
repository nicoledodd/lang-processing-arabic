## statistical significance modeling for eye tracking data


## libraries
library(tidyverse)
library(brms)

## Separate models-omnibus by region (IA)
## Dependent variable: first fixation, first pass, go past, total fixation,
## first pass regression (binary), first pass skip (binary)
## Independent variables: type (categorical)
## Control predictors: length (continuous), trial number (continuous)
## Random intercepts: participant, group number
## Random slopes: type by participant, type by group number
## Sum coding

# metric ~ type + length + trial_index + (1 + type | session_id) +
# (1 + type | group_number)



#### data prep -----------------------------------------------------------------
data <- read_csv("data/exp3-eye-tracking-clean.csv")

# filter out filler items
data <- data %>% 
  filter(!grepl('filler', data$item_type))

# create column w/ labels for omnibus testing
data$correct[data$correct == 1] <- 'correct'
data$correct[data$correct == 0] <- 'incorrect'

data <- data %>% 
  unite(type, item_type, correct, sep = '-')

# filter out SRC-incorrect
data <- data %>% 
  filter(type != 'SRC-incorrect')

# make categorical parameters factors
data <- data %>% 
  mutate(type = factor(type),
         session_id = factor(session_id),
         group_number = factor(group_number))

## type1 = ORC-correct
## type2 = ORC-incorrect

# separate by region
matrixSubj <- data %>% 
  filter(region == 'matrix-SUBJ')

relPron <- data %>% 
  filter(region == 'rel-PRON')

relVerb <- data %>% 
  filter(region == 'rel-VERB')

relClitic <- data %>% 
  filter(region == 'rel-CLITIC')

# includes SRC relVerbs which are the same as relVerbWhole 
relVerbWhole <- data %>% 
  filter(region == 'rel-VERB-whole' | (region == 'rel-VERB' & type == 'SRC-correct'))

relNP <- data %>% 
  filter(region == 'rel-NP')

matrixVerb <- data %>% 
  filter(region == 'matrix-VERB')

spillover1 <- data %>% 
  filter(region == 'spillover-1')

spillover2 <- data %>% 
  filter(region == 'spillover-2')

spillover3 <- data %>% 
  filter(region == 'spillover-3')



#### model prep ----------------------------------------------------------------
set.seed(10)
options(contrasts = c('contr.sum', 'contr.sum'))

contr.sum(1:3)




#### matrix-SUBJ ---------------------------------------------------------------
ff_ms <- brm(first_fixation ~ type + length + trial_index +
               (1 + type | session_id) +
               (1 + type | group_number),
             data = matrixSubj,
             chains = 4,
             cores = 4,
             family = gaussian,
             warmup = 1000,
             iter = 6000)

write_rds(ff_ms, "models-omnibus/first-fixation-matrixSubj-omnibus.RDS")
ff_ms_df <- as_tibble(fixef(ff_ms), rownames = "parameter")
ff_ms_df <- ff_ms_df %>%
  mutate(region = 'matrix-SUBJ',
         metric = 'first fixation',
         model = 'omnibus')

fp_ms <- brm(first_pass ~ type + length + trial_index +
               (1 + type | session_id) +
               (1 + type | group_number),
             data = matrixSubj,
             chains = 4,
             cores = 4,
             family = gaussian,
             warmup = 1000,
             iter = 6000)

write_rds(fp_ms, "models-omnibus/first-pass-matrixSubj-omnibus.RDS")
fp_ms_df <- as_tibble(fixef(fp_ms), rownames = "parameter")
fp_ms_df <- ff_ms_df %>%
  mutate(region = 'matrix-SUBJ',
         metric = 'first pass',
         model = 'omnibus')

gp_ms <- brm(go_past ~ type + length + trial_index +
               (1 + type | session_id) +
               (1 + type | group_number),
             data = matrixSubj,
             chains = 4,
             cores = 4,
             family = gaussian,
             warmup = 1000,
             iter = 6000)

write_rds(gp_ms, "models-omnibus/go-past-matrixSubj-omnibus.RDS")
gp_ms_df <- as_tibble(fixef(gp_ms), rownames = "parameter")
gp_ms_df <- gp_ms_df %>%
  mutate(region = 'matrix-SUBJ',
         metric = 'go past',
         model = 'omnibus')

td_ms <- brm(total_duration ~ type + length + trial_index +
               (1 + type | session_id) +
               (1 + type | group_number),
             data = matrixSubj,
             chains = 4,
             cores = 4,
             family = gaussian,
             warmup = 1000,
             iter = 6000)

write_rds(td_ms, "models-omnibus/total-duration-matrixSubj-omnibus.RDS")
td_ms_df <- as_tibble(fixef(td_ms), rownames = "parameter")
td_ms_df <- td_ms_df %>%
  mutate(region = 'matrix-SUBJ',
         metric = 'total duration',
         model = 'omnibus')

# filter out items that were skipped on first pass for first pass regression model
matrixSubj_fpr <- matrixSubj %>% 
  filter(first_pass_regression != '.')
# actually going to skip this particular model for matrixSubj since it's the first
# word and there's nowhere to regress to 

fps_ms <- brm(first_pass_skip ~ type + length + trial_index +
                (1 + type | session_id) +
                (1 + type | group_number),
              data = matrixSubj,
              chains = 4,
              cores = 4,
              family = bernoulli,
              warmup = 1000,
              iter = 6000)

write_rds(fps_ms, "models-omnibus/first-pass-skip-matrixSubj-omnibus.RDS")
fps_ms_df <- as_tibble(fixef(fps_ms), rownames = "parameter")
fps_ms_df <- fps_ms_df %>%
  mutate(region = 'matrix-SUBJ',
         metric = 'first pass skip',
         model = 'omnibus')


estimates <- bind_rows(ff_ms_df, fp_ms_df, gp_ms_df, td_ms_df, fps_ms_df)




#### rel-Pron ------------------------------------------------------------------
## removing length parameter here since they're always the same length
ff_rp <- brm(first_fixation ~ type + trial_index +
               (1 + type | session_id) +
               (1 + type | group_number),
             data = relPron,
             chains = 4,
             cores = 4,
             family = gaussian,
             warmup = 1000,
             iter = 6000)

write_rds(ff_rp, "models-omnibus/first-fixation-relPron-omnibus.RDS")
ff_rp_df <- as_tibble(fixef(ff_rp), rownames = "parameter")
ff_rp_df <- ff_rp_df %>%
  mutate(region = 'rel-PRON',
         metric = 'first fixation',
         model = 'omnibus')

fp_rp <- brm(first_pass ~ type + trial_index +
               (1 + type | session_id) +
               (1 + type | group_number),
             data = relPron,
             chains = 4,
             cores = 4,
             family = gaussian,
             warmup = 1000,
             iter = 6000)

write_rds(fp_rp, "models-omnibus/first-pass-relPron-omnibus.RDS")
fp_rp_df <- as_tibble(fixef(fp_rp), rownames = "parameter")
fp_rp_df <- ff_rp_df %>%
  mutate(region = 'rel-PRON',
         metric = 'first pass',
         model = 'omnibus')

gp_rp <- brm(go_past ~ type + trial_index +
               (1 + type | session_id) +
               (1 + type | group_number),
             data = relPron,
             chains = 4,
             cores = 4,
             family = gaussian,
             warmup = 1000,
             iter = 6000)

write_rds(gp_rp, "models-omnibus/go-past-relPron-omnibus.RDS")
gp_rp_df <- as_tibble(fixef(gp_rp), rownames = "parameter")
gp_rp_df <- gp_rp_df %>%
  mutate(region = 'rel-PRON',
         metric = 'go past',
         model = 'omnibus')

td_rp <- brm(total_duration ~ type + trial_index +
               (1 + type | session_id) +
               (1 + type | group_number),
             data = relPron,
             chains = 4,
             cores = 4,
             family = gaussian,
             warmup = 1000,
             iter = 6000)

write_rds(td_rp, "models-omnibus/total-duration-relPron-omnibus.RDS")
td_rp_df <- as_tibble(fixef(td_rp), rownames = "parameter")
td_rp_df <- td_rp_df %>%
  mutate(region = 'rel-PRON',
         metric = 'total duration',
         model = 'omnibus')

# filter out items that were skipped on first pass for first pass regression model
relPron_fpr <- relPron %>% 
  filter(first_pass_regression != '.')

fpr_rp <- brm(first_pass_regression ~ type + trial_index +
                (1 + type | session_id) +
                (1 + type | group_number),
              data = relPron_fpr,
              chains = 4,
              cores = 4,
              family = bernoulli,
              warmup = 1000,
              iter = 6000)

write_rds(fpr_rp, "models-omnibus/first-pass-regression-relPron-omnibus.RDS")
fpr_rp_df <- as_tibble(fixef(fpr_rp), rownames = "parameter")
fpr_rp_df <- fpr_rp_df %>%
  mutate(region = 'rel-PRON',
         metric = 'first pass regression',
         model = 'omnibus')

fps_rp <- brm(first_pass_skip ~ type + trial_index +
                (1 + type | session_id) +
                (1 + type | group_number),
              data = relPron,
              chains = 4,
              cores = 4,
              family = bernoulli,
              warmup = 1000,
              iter = 6000)

write_rds(fps_rp, "models-omnibus/first-pass-skip-relPron-omnibus.RDS")
fps_rp_df <- as_tibble(fixef(fps_rp), rownames = "parameter")
fps_rp_df <- fps_rp_df %>%
  mutate(region = 'rel-PRON',
         metric = 'first pass skip',
         model = 'omnibus')


estimates <- bind_rows(estimates, ff_rp_df, fp_rp_df, gp_rp_df, td_rp_df, 
                       fpr_rp_df, fps_rp_df)




#### rel-VERB ------------------------------------------------------------------
ff_rv <- brm(first_fixation ~ type + length + trial_index +
               (1 + type | session_id) +
               (1 + type | group_number),
             data = relVerb,
             chains = 4,
             cores = 4,
             family = gaussian,
             warmup = 1000,
             iter = 6000)

write_rds(ff_rv, "models-omnibus/first-fixation-relVerb-omnibus.RDS")
ff_rv_df <- as_tibble(fixef(ff_rv), rownames = "parameter")
ff_rv_df <- ff_rv_df %>%
  mutate(region = 'rel-VERB',
         metric = 'first fixation',
         model = 'omnibus')

fp_rv <- brm(first_pass ~ type + length + trial_index +
               (1 + type | session_id) +
               (1 + type | group_number),
             data = relVerb,
             chains = 4,
             cores = 4,
             family = gaussian,
             warmup = 1000,
             iter = 6000)

write_rds(fp_rv, "models-omnibus/first-pass-relVerb-omnibus.RDS")
fp_rv_df <- as_tibble(fixef(fp_rv), rownames = "parameter")
fp_rv_df <- ff_rv_df %>%
  mutate(region = 'rel-VERB',
         metric = 'first pass',
         model = 'omnibus')

gp_rv <- brm(go_past ~ type + length + trial_index +
               (1 + type | session_id) +
               (1 + type | group_number),
             data = relVerb,
             chains = 4,
             cores = 4,
             family = gaussian,
             warmup = 1000,
             iter = 6000)

write_rds(gp_rv, "models-omnibus/go-past-relVerb-omnibus.RDS")
gp_rv_df <- as_tibble(fixef(gp_rv), rownames = "parameter")
gp_rv_df <- gp_rv_df %>%
  mutate(region = 'rel-VERB',
         metric = 'go past',
         model = 'omnibus')

td_rv <- brm(total_duration ~ type + length + trial_index +
               (1 + type | session_id) +
               (1 + type | group_number),
             data = relVerb,
             chains = 4,
             cores = 4,
             family = gaussian,
             warmup = 1000,
             iter = 6000)

write_rds(td_rv, "models-omnibus/total-duration-relVerb-omnibus.RDS")
td_rv_df <- as_tibble(fixef(td_rv), rownames = "parameter")
td_rv_df <- td_rv_df %>%
  mutate(region = 'rel-VERB',
         metric = 'total duration',
         model = 'omnibus')

# filter out items that were skipped on first pass for first pass regression model
relVerb_fpr <- relVerb %>% 
  filter(first_pass_regression != '.')

fpr_rv <- brm(first_pass_regression ~ type + length + trial_index +
                (1 + type | session_id) +
                (1 + type | group_number),
              data = relVerb_fpr,
              chains = 4,
              cores = 4,
              family = bernoulli,
              warmup = 1000,
              iter = 6000)

write_rds(fpr_rv, "models-omnibus/first-pass-regression-relVerb-omnibus.RDS")
fpr_rv_df <- as_tibble(fixef(fpr_rv), rownames = "parameter")
fpr_rv_df <- fpr_rv_df %>%
  mutate(region = 'rel-VERB',
         metric = 'first pass regression',
         model = 'omnibus')

fps_rv <- brm(first_pass_skip ~ type + length + trial_index +
                (1 + type | session_id) +
                (1 + type | group_number),
              data = relVerb,
              chains = 4,
              cores = 4,
              family = bernoulli,
              warmup = 1000,
              iter = 6000)

write_rds(fps_rv, "models-omnibus/first-pass-skip-relVerb-omnibus.RDS")
fps_rv_df <- as_tibble(fixef(fps_rv), rownames = "parameter")
fps_rv_df <- fps_rv_df %>%
  mutate(region = 'rel-VERB',
         metric = 'first pass skip',
         model = 'omnibus')


estimates <- bind_rows(estimates, ff_rv_df, fp_rv_df, gp_rv_df, td_rv_df, 
                       fpr_rv_df, fps_rv_df)




#### rel-CLITIC ----------------------------------------------------------------
# ff_rc <- brm(first_fixation ~ type + length + trial_index +
#                (1 + type | session_id) +
#                (1 + type | group_number),
#              data = relClitic,
#              chains = 4,
#              cores = 4,
#              family = gaussian,
#              warmup = 1000,
#              iter = 6000)
# 
# write_rds(ff_rc, "models-omnibus/first-fixation-relClitic-omnibus.RDS")
# ff_rc_df <- as_tibble(fixef(ff_rc), rownames = "parameter")
# ff_rc_df <- ff_rc_df %>%
#   mutate(region = 'rel-CLITIC',
#          metric = 'first fixation',
#          model = 'omnibus')
# 
# fp_rc <- brm(first_pass ~ type + length + trial_index +
#                (1 + type | session_id) +
#                (1 + type | group_number),
#              data = relClitic,
#              chains = 4,
#              cores = 4,
#              family = gaussian,
#              warmup = 1000,
#              iter = 6000)
# 
# write_rds(fp_rc, "models-omnibus/first-pass-relClitic-omnibus.RDS")
# fp_rc_df <- as_tibble(fixef(fp_rc), rownames = "parameter")
# fp_rc_df <- ff_rc_df %>%
#   mutate(region = 'rel-CLITIC',
#          metric = 'first pass',
#          model = 'omnibus')
# 
# gp_rc <- brm(go_past ~ type + length + trial_index +
#                (1 + type | session_id) +
#                (1 + type | group_number),
#              data = relClitic,
#              chains = 4,
#              cores = 4,
#              family = gaussian,
#              warmup = 1000,
#              iter = 6000)
# 
# write_rds(gp_rc, "models-omnibus/go-past-relClitic-omnibus.RDS")
# gp_rc_df <- as_tibble(fixef(gp_rc), rownames = "parameter")
# gp_rc_df <- gp_rc_df %>%
#   mutate(region = 'rel-CLITIC',
#          metric = 'go past',
#          model = 'omnibus')
# 
# td_rc <- brm(total_duration ~ type + length + trial_index +
#                (1 + type | session_id) +
#                (1 + type | group_number),
#              data = relClitic,
#              chains = 4,
#              cores = 4,
#              family = gaussian,
#              warmup = 1000,
#              iter = 6000)
# 
# write_rds(td_rc, "models-omnibus/total-duration-relClitic-omnibus.RDS")
# td_rc_df <- as_tibble(fixef(td_rc), rownames = "parameter")
# td_rc_df <- td_rc_df %>%
#   mutate(region = 'rel-CLITIC',
#          metric = 'total duration',
#          model = 'omnibus')
# 
# # filter out items that were skipped on first pass for first pass regression model
# relClitic_fpr <- relClitic %>% 
#   filter(first_pass_regression != '.')
# 
# fpr_rc <- brm(first_pass_regression ~ type + length + trial_index +
#                 (1 + type | session_id) +
#                 (1 + type | group_number),
#               data = relClitic_fpr,
#               chains = 4,
#               cores = 4,
#               family = bernoulli,
#               warmup = 1000,
#               iter = 6000)
# 
# write_rds(fpr_rc, "models-omnibus/first-pass-regression-relClitic-omnibus.RDS")
# fpr_rc_df <- as_tibble(fixef(fpr_rc), rownames = "parameter")
# fpr_rc_df <- fpr_rc_df %>%
#   mutate(region = 'rel-CLITIC',
#          metric = 'first pass regression',
#          model = 'omnibus')
# 
# fps_rc <- brm(first_pass_skip ~ type + length + trial_index +
#                 (1 + type | session_id) +
#                 (1 + type | group_number),
#               data = relClitic,
#               chains = 4,
#               cores = 4,
#               family = bernoulli,
#               warmup = 1000,
#               iter = 6000)
# 
# write_rds(fps_rc, "models-omnibus/first-pass-skip-relClitic-omnibus.RDS")
# fps_rc_df <- as_tibble(fixef(fps_rc), rownames = "parameter")
# fps_rc_df <- fps_rc_df %>%
#   mutate(region = 'rel-CLITIC',
#          metric = 'first pass skip',
#          model = 'omnibus')
# 
# 
# estimates <- bind_rows(estimates, ff_rc_df, fp_rc_df, gp_rc_df, td_rc_df, 
#                        fpr_rc_df, fps_rc_df)




#### rel-VERB-whole ------------------------------------------------------------
ff_rvw <- brm(first_fixation ~ type + length + trial_index +
               (1 + type | session_id) +
               (1 + type | group_number),
             data = relVerbWhole,
             chains = 4,
             cores = 4,
             family = gaussian,
             warmup = 1000,
             iter = 6000)

write_rds(ff_rvw, "models-omnibus/first-fixation-relVerbWhole-omnibus.RDS")
ff_rvw_df <- as_tibble(fixef(ff_rvw), rownames = "parameter")
ff_rvw_df <- ff_rvw_df %>%
  mutate(region = 'rel-VERB-whole',
         metric = 'first fixation',
         model = 'omnibus')

fp_rvw <- brm(first_pass ~ type + length + trial_index +
               (1 + type | session_id) +
               (1 + type | group_number),
             data = relVerbWhole,
             chains = 4,
             cores = 4,
             family = gaussian,
             warmup = 1000,
             iter = 6000)

write_rds(fp_rvw, "models-omnibus/first-pass-relVerbWhole-omnibus.RDS")
fp_rvw_df <- as_tibble(fixef(fp_rvw), rownames = "parameter")
fp_rvw_df <- ff_rvw_df %>%
  mutate(region = 'rel-VERB-whole',
         metric = 'first pass',
         model = 'omnibus')

gp_rvw <- brm(go_past ~ type + length + trial_index +
               (1 + type | session_id) +
               (1 + type | group_number),
             data = relVerbWhole,
             chains = 4,
             cores = 4,
             family = gaussian,
             warmup = 1000,
             iter = 6000)

write_rds(gp_rvw, "models-omnibus/go-past-relVerbWhole-omnibus.RDS")
gp_rvw_df <- as_tibble(fixef(gp_rvw), rownames = "parameter")
gp_rvw_df <- gp_rvw_df %>%
  mutate(region = 'rel-VERB-whole',
         metric = 'go past',
         model = 'omnibus')

td_rvw <- brm(total_duration ~ type + length + trial_index +
               (1 + type | session_id) +
               (1 + type | group_number),
             data = relVerbWhole,
             chains = 4,
             cores = 4,
             family = gaussian,
             warmup = 1000,
             iter = 6000)

write_rds(td_rvw, "models-omnibus/total-duration-relVerbWhole-omnibus.RDS")
td_rvw_df <- as_tibble(fixef(td_rvw), rownames = "parameter")
td_rvw_df <- td_rvw_df %>%
  mutate(region = 'rel-VERB-whole',
         metric = 'total duration',
         model = 'omnibus')

# filter out items that were skipped on first pass for first pass regression model
relVerbWhole_fpr <- relVerbWhole %>% 
  filter(first_pass_regression != '.')

fpr_rvw <- brm(first_pass_regression ~ type + length + trial_index +
                (1 + type | session_id) +
                (1 + type | group_number),
              data = relVerbWhole_fpr,
              chains = 4,
              cores = 4,
              family = bernoulli,
              warmup = 1000,
              iter = 6000)

write_rds(fpr_rvw, "models-omnibus/first-pass-regression-relVerbWhole-omnibus.RDS")
fpr_rvw_df <- as_tibble(fixef(fpr_rvw), rownames = "parameter")
fpr_rvw_df <- fpr_rvw_df %>%
  mutate(region = 'rel-VERB-whole',
         metric = 'first pass regression',
         model = 'omnibus')

fps_rvw <- brm(first_pass_skip ~ type + length + trial_index +
                (1 + type | session_id) +
                (1 + type | group_number),
              data = relVerbWhole,
              chains = 4,
              cores = 4,
              family = bernoulli,
              warmup = 1000,
              iter = 6000)

write_rds(fps_rvw, "models-omnibus/first-pass-skip-relVerbWhole-omnibus.RDS")
fps_rvw_df <- as_tibble(fixef(fps_rvw), rownames = "parameter")
fps_rvw_df <- fps_rvw_df %>%
  mutate(region = 'rel-VERB-whole',
         metric = 'first pass skip',
         model = 'omnibus')


estimates <- bind_rows(estimates, ff_rvw_df, fp_rvw_df, gp_rvw_df, td_rvw_df, 
                       fpr_rvw_df, fps_rvw_df)




#### rel-NP --------------------------------------------------------------------
ff_rnp <- brm(first_fixation ~ type + length + trial_index +
                (1 + type | session_id) +
                (1 + type | group_number),
              data = relNP,
              chains = 4,
              cores = 4,
              family = gaussian,
              warmup = 1000,
              iter = 6000)

write_rds(ff_rnp, "models-omnibus/first-fixation-relNP-omnibus.RDS")
ff_rnp_df <- as_tibble(fixef(ff_rnp), rownames = "parameter")
ff_rnp_df <- ff_rnp_df %>%
  mutate(region = 'rel-NP',
         metric = 'first fixation',
         model = 'omnibus')

fp_rnp <- brm(first_pass ~ type + length + trial_index +
                (1 + type | session_id) +
                (1 + type | group_number),
              data = relNP,
              chains = 4,
              cores = 4,
              family = gaussian,
              warmup = 1000,
              iter = 6000)

write_rds(fp_rnp, "models-omnibus/first-pass-relNP-omnibus.RDS")
fp_rnp_df <- as_tibble(fixef(fp_rnp), rownames = "parameter")
fp_rnp_df <- ff_rnp_df %>%
  mutate(region = 'rel-NP',
         metric = 'first pass',
         model = 'omnibus')

gp_rnp <- brm(go_past ~ type + length + trial_index +
                (1 + type | session_id) +
                (1 + type | group_number),
              data = relNP,
              chains = 4,
              cores = 4,
              family = gaussian,
              warmup = 1000,
              iter = 6000)

write_rds(gp_rnp, "models-omnibus/go-past-relNP-omnibus.RDS")
gp_rnp_df <- as_tibble(fixef(gp_rnp), rownames = "parameter")
gp_rnp_df <- gp_rnp_df %>%
  mutate(region = 'rel-NP',
         metric = 'go past',
         model = 'omnibus')

td_rnp <- brm(total_duration ~ type + length + trial_index +
                (1 + type | session_id) +
                (1 + type | group_number),
              data = relNP,
              chains = 4,
              cores = 4,
              family = gaussian,
              warmup = 1000,
              iter = 6000)

write_rds(td_rnp, "models-omnibus/total-duration-relNP-omnibus.RDS")
td_rnp_df <- as_tibble(fixef(td_rnp), rownames = "parameter")
td_rnp_df <- td_rnp_df %>%
  mutate(region = 'rel-NP',
         metric = 'total duration',
         model = 'omnibus')

# filter out items that were skipped on first pass for first pass regression model
relNP_fpr <- relNP %>% 
  filter(first_pass_regression != '.')

fpr_rnp <- brm(first_pass_regression ~ type + length + trial_index +
                 (1 + type | session_id) +
                 (1 + type | group_number),
               data = relNP_fpr,
               chains = 4,
               cores = 4,
               family = bernoulli,
               warmup = 1000,
               iter = 6000)

write_rds(fpr_rnp, "models-omnibus/first-pass-regression-relNP-omnibus.RDS")
fpr_rnp_df <- as_tibble(fixef(fpr_rnp), rownames = "parameter")
fpr_rnp_df <- fpr_rnp_df %>%
  mutate(region = 'rel-NP',
         metric = 'first pass regression',
         model = 'omnibus')

fps_rnp <- brm(first_pass_skip ~ type + length + trial_index +
                 (1 + type | session_id) +
                 (1 + type | group_number),
               data = relNP,
               chains = 4,
               cores = 4,
               family = bernoulli,
               warmup = 1000,
               iter = 6000)

write_rds(fps_rnp, "models-omnibus/first-pass-skip-relNP-omnibus.RDS")
fps_rnp_df <- as_tibble(fixef(fps_rnp), rownames = "parameter")
fps_rnp_df <- fps_rnp_df %>%
  mutate(region = 'rel-NP',
         metric = 'first pass skip',
         model = 'omnibus')


estimates <- bind_rows(estimates, ff_rnp_df, fp_rnp_df, gp_rnp_df, td_rnp_df, 
                       fpr_rnp_df, fps_rnp_df)




#### matrix-VERB ---------------------------------------------------------------
ff_mv <- brm(first_fixation ~ type + length + trial_index +
                (1 + type | session_id) +
                (1 + type | group_number),
              data = matrixVerb,
              chains = 4,
              cores = 4,
              family = gaussian,
              warmup = 1000,
              iter = 6000)

write_rds(ff_mv, "models-omnibus/first-fixation-matrixVerb-omnibus.RDS")
ff_mv_df <- as_tibble(fixef(ff_mv), rownames = "parameter")
ff_mv_df <- ff_mv_df %>%
  mutate(region = 'matrix-VERB',
         metric = 'first fixation',
         model = 'omnibus')

fp_mv <- brm(first_pass ~ type + length + trial_index +
                (1 + type | session_id) +
                (1 + type | group_number),
              data = matrixVerb,
              chains = 4,
              cores = 4,
              family = gaussian,
              warmup = 1000,
              iter = 6000)

write_rds(fp_mv, "models-omnibus/first-pass-matrixVerb-omnibus.RDS")
fp_mv_df <- as_tibble(fixef(fp_mv), rownames = "parameter")
fp_mv_df <- ff_mv_df %>%
  mutate(region = 'matrix-VERB',
         metric = 'first pass',
         model = 'omnibus')

gp_mv <- brm(go_past ~ type + length + trial_index +
                (1 + type | session_id) +
                (1 + type | group_number),
              data = matrixVerb,
              chains = 4,
              cores = 4,
              family = gaussian,
              warmup = 1000,
              iter = 6000)

write_rds(gp_mv, "models-omnibus/go-past-matrixVerb-omnibus.RDS")
gp_mv_df <- as_tibble(fixef(gp_mv), rownames = "parameter")
gp_mv_df <- gp_mv_df %>%
  mutate(region = 'matrix-VERB',
         metric = 'go past',
         model = 'omnibus')

td_mv <- brm(total_duration ~ type + length + trial_index +
                (1 + type | session_id) +
                (1 + type | group_number),
              data = matrixVerb,
              chains = 4,
              cores = 4,
              family = gaussian,
              warmup = 1000,
              iter = 6000)

write_rds(td_mv, "models-omnibus/total-duration-matrixVerb-omnibus.RDS")
td_mv_df <- as_tibble(fixef(td_mv), rownames = "parameter")
td_mv_df <- td_mv_df %>%
  mutate(region = 'matrix-VERB',
         metric = 'total duration',
         model = 'omnibus')

# filter out items that were skipped on first pass for first pass regression model
matrixVerb_fpr <- matrixVerb %>% 
  filter(first_pass_regression != '.')

fpr_mv <- brm(first_pass_regression ~ type + length + trial_index +
                 (1 + type | session_id) +
                 (1 + type | group_number),
               data = matrixVerb_fpr,
               chains = 4,
               cores = 4,
               family = bernoulli,
               warmup = 1000,
               iter = 6000)

write_rds(fpr_mv, "models-omnibus/first-pass-regression-matrixVerb-omnibus.RDS")
fpr_mv_df <- as_tibble(fixef(fpr_mv), rownames = "parameter")
fpr_mv_df <- fpr_mv_df %>%
  mutate(region = 'matrix-VERB',
         metric = 'first pass regression',
         model = 'omnibus')

fps_mv <- brm(first_pass_skip ~ type + length + trial_index +
                 (1 + type | session_id) +
                 (1 + type | group_number),
               data = matrixVerb,
               chains = 4,
               cores = 4,
               family = bernoulli,
               warmup = 1000,
               iter = 6000)

write_rds(fps_mv, "models-omnibus/first-pass-skip-matrixVerb-omnibus.RDS")
fps_mv_df <- as_tibble(fixef(fps_mv), rownames = "parameter")
fps_mv_df <- fps_mv_df %>%
  mutate(region = 'matrix-VERB',
         metric = 'first pass skip',
         model = 'omnibus')


estimates <- bind_rows(estimates, ff_mv_df, fp_mv_df, gp_mv_df, td_mv_df, 
                       fpr_mv_df, fps_mv_df)




#### spillover-1 ---------------------------------------------------------------
ff_s1 <- brm(first_fixation ~ type + length + trial_index +
               (1 + type | session_id) +
               (1 + type | group_number),
             data = spillover1,
             chains = 4,
             cores = 4,
             family = gaussian,
             warmup = 1000,
             iter = 6000)

write_rds(ff_s1, "models-omnibus/first-fixation-spillover1-omnibus.RDS")
ff_s1_df <- as_tibble(fixef(ff_s1), rownames = "parameter")
ff_s1_df <- ff_s1_df %>%
  mutate(region = 'spillover-1',
         metric = 'first fixation',
         model = 'omnibus')

fp_s1 <- brm(first_pass ~ type + length + trial_index +
               (1 + type | session_id) +
               (1 + type | group_number),
             data = spillover1,
             chains = 4,
             cores = 4,
             family = gaussian,
             warmup = 1000,
             iter = 6000)

write_rds(fp_s1, "models-omnibus/first-pass-spillover1-omnibus.RDS")
fp_s1_df <- as_tibble(fixef(fp_s1), rownames = "parameter")
fp_s1_df <- ff_s1_df %>%
  mutate(region = 'spillover-1',
         metric = 'first pass',
         model = 'omnibus')

gp_s1 <- brm(go_past ~ type + length + trial_index +
               (1 + type | session_id) +
               (1 + type | group_number),
             data = spillover1,
             chains = 4,
             cores = 4,
             family = gaussian,
             warmup = 1000,
             iter = 6000)

write_rds(gp_s1, "models-omnibus/go-past-spillover1-omnibus.RDS")
gp_s1_df <- as_tibble(fixef(gp_s1), rownames = "parameter")
gp_s1_df <- gp_s1_df %>%
  mutate(region = 'spillover-1',
         metric = 'go past',
         model = 'omnibus')

td_s1 <- brm(total_duration ~ type + length + trial_index +
               (1 + type | session_id) +
               (1 + type | group_number),
             data = spillover1,
             chains = 4,
             cores = 4,
             family = gaussian,
             warmup = 1000,
             iter = 6000)

write_rds(td_s1, "models-omnibus/total-duration-spillover1-omnibus.RDS")
td_s1_df <- as_tibble(fixef(td_s1), rownames = "parameter")
td_s1_df <- td_s1_df %>%
  mutate(region = 'spillover-1',
         metric = 'total duration',
         model = 'omnibus')

# filter out items that were skipped on first pass for first pass regression model
spillover1_fpr <- spillover1 %>% 
  filter(first_pass_regression != '.')

fpr_s1 <- brm(first_pass_regression ~ type + length + trial_index +
                (1 + type | session_id) +
                (1 + type | group_number),
              data = spillover1_fpr,
              chains = 4,
              cores = 4,
              family = bernoulli,
              warmup = 1000,
              iter = 6000)

write_rds(fpr_s1, "models-omnibus/first-pass-regression-spillover1-omnibus.RDS")
fpr_s1_df <- as_tibble(fixef(fpr_s1), rownames = "parameter")
fpr_s1_df <- fpr_s1_df %>%
  mutate(region = 'spillover-1',
         metric = 'first pass regression',
         model = 'omnibus')

fps_s1 <- brm(first_pass_skip ~ type + length + trial_index +
                (1 + type | session_id) +
                (1 + type | group_number),
              data = spillover1,
              chains = 4,
              cores = 4,
              family = bernoulli,
              warmup = 1000,
              iter = 6000)

write_rds(fps_s1, "models-omnibus/first-pass-skip-spillover1-omnibus.RDS")
fps_s1_df <- as_tibble(fixef(fps_s1), rownames = "parameter")
fps_s1_df <- fps_s1_df %>%
  mutate(region = 'spillover-1',
         metric = 'first pass skip',
         model = 'omnibus')


estimates <- bind_rows(estimates, ff_s1_df, fp_s1_df, gp_s1_df, td_s1_df, 
                       fpr_s1_df, fps_s1_df)




#### spillover-2 ---------------------------------------------------------------
ff_s2 <- brm(first_fixation ~ type + length + trial_index +
               (1 + type | session_id) +
               (1 + type | group_number),
             data = spillover2,
             chains = 4,
             cores = 4,
             family = gaussian,
             warmup = 1000,
             iter = 6000)

write_rds(ff_s2, "models-omnibus/first-fixation-spillover2-omnibus.RDS")
ff_s2_df <- as_tibble(fixef(ff_s2), rownames = "parameter")
ff_s2_df <- ff_s2_df %>%
  mutate(region = 'spillover-2',
         metric = 'first fixation',
         model = 'omnibus')

fp_s2 <- brm(first_pass ~ type + length + trial_index +
               (1 + type | session_id) +
               (1 + type | group_number),
             data = spillover2,
             chains = 4,
             cores = 4,
             family = gaussian,
             warmup = 1000,
             iter = 6000)

write_rds(fp_s2, "models-omnibus/first-pass-spillover2-omnibus.RDS")
fp_s2_df <- as_tibble(fixef(fp_s2), rownames = "parameter")
fp_s2_df <- ff_s2_df %>%
  mutate(region = 'spillover-2',
         metric = 'first pass',
         model = 'omnibus')

gp_s2 <- brm(go_past ~ type + length + trial_index +
               (1 + type | session_id) +
               (1 + type | group_number),
             data = spillover2,
             chains = 4,
             cores = 4,
             family = gaussian,
             warmup = 1000,
             iter = 6000)

write_rds(gp_s2, "models-omnibus/go-past-spillover2-omnibus.RDS")
gp_s2_df <- as_tibble(fixef(gp_s2), rownames = "parameter")
gp_s2_df <- gp_s2_df %>%
  mutate(region = 'spillover-2',
         metric = 'go past',
         model = 'omnibus')

td_s2 <- brm(total_duration ~ type + length + trial_index +
               (1 + type | session_id) +
               (1 + type | group_number),
             data = spillover2,
             chains = 4,
             cores = 4,
             family = gaussian,
             warmup = 1000,
             iter = 6000)

write_rds(td_s2, "models-omnibus/total-duration-spillover2-omnibus.RDS")
td_s2_df <- as_tibble(fixef(td_s2), rownames = "parameter")
td_s2_df <- td_s2_df %>%
  mutate(region = 'spillover-2',
         metric = 'total duration',
         model = 'omnibus')

# filter out items that were skipped on first pass for first pass regression model
spillover2_fpr <- spillover2 %>% 
  filter(first_pass_regression != '.')

fpr_s2 <- brm(first_pass_regression ~ type + length + trial_index +
                (1 + type | session_id) +
                (1 + type | group_number),
              data = spillover2_fpr,
              chains = 4,
              cores = 4,
              family = bernoulli,
              warmup = 1000,
              iter = 6000)

write_rds(fpr_s2, "models-omnibus/first-pass-regression-spillover2-omnibus.RDS")
fpr_s2_df <- as_tibble(fixef(fpr_s2), rownames = "parameter")
fpr_s2_df <- fpr_s2_df %>%
  mutate(region = 'spillover-2',
         metric = 'first pass regression',
         model = 'omnibus')

fps_s2 <- brm(first_pass_skip ~ type + length + trial_index +
                (1 + type | session_id) +
                (1 + type | group_number),
              data = spillover2,
              chains = 4,
              cores = 4,
              family = bernoulli,
              warmup = 1000,
              iter = 6000)

write_rds(fps_s2, "models-omnibus/first-pass-skip-spillover2-omnibus.RDS")
fps_s2_df <- as_tibble(fixef(fps_s2), rownames = "parameter")
fps_s2_df <- fps_s2_df %>%
  mutate(region = 'spillover-2',
         metric = 'first pass skip',
         model = 'omnibus')


estimates <- bind_rows(estimates, ff_s2_df, fp_s2_df, gp_s2_df, td_s2_df, 
                       fpr_s2_df, fps_s2_df)




#### spillover-3 ---------------------------------------------------------------
ff_s3 <- brm(first_fixation ~ type + length + trial_index +
               (1 + type | session_id) +
               (1 + type | group_number),
             data = spillover3,
             chains = 4,
             cores = 4,
             family = gaussian,
             warmup = 1000,
             iter = 6000)

write_rds(ff_s3, "models-omnibus/first-fixation-spillover3-omnibus.RDS")
ff_s3_df <- as_tibble(fixef(ff_s3), rownames = "parameter")
ff_s3_df <- ff_s3_df %>%
  mutate(region = 'spillover-3',
         metric = 'first fixation',
         model = 'omnibus')

fp_s3 <- brm(first_pass ~ type + length + trial_index +
               (1 + type | session_id) +
               (1 + type | group_number),
             data = spillover3,
             chains = 4,
             cores = 4,
             family = gaussian,
             warmup = 1000,
             iter = 6000)

write_rds(fp_s3, "models-omnibus/first-pass-spillover3-omnibus.RDS")
fp_s3_df <- as_tibble(fixef(fp_s3), rownames = "parameter")
fp_s3_df <- ff_s3_df %>%
  mutate(region = 'spillover-3',
         metric = 'first pass',
         model = 'omnibus')

gp_s3 <- brm(go_past ~ type + length + trial_index +
               (1 + type | session_id) +
               (1 + type | group_number),
             data = spillover3,
             chains = 4,
             cores = 4,
             family = gaussian,
             warmup = 1000,
             iter = 6000)

write_rds(gp_s3, "models-omnibus/go-past-spillover3-omnibus.RDS")
gp_s3_df <- as_tibble(fixef(gp_s3), rownames = "parameter")
gp_s3_df <- gp_s3_df %>%
  mutate(region = 'spillover-3',
         metric = 'go past',
         model = 'omnibus')

td_s3 <- brm(total_duration ~ type + length + trial_index +
               (1 + type | session_id) +
               (1 + type | group_number),
             data = spillover3,
             chains = 4,
             cores = 4,
             family = gaussian,
             warmup = 1000,
             iter = 6000)

write_rds(td_s3, "models-omnibus/total-duration-spillover3-omnibus.RDS")
td_s3_df <- as_tibble(fixef(td_s3), rownames = "parameter")
td_s3_df <- td_s3_df %>%
  mutate(region = 'spillover-3',
         metric = 'total duration',
         model = 'omnibus')

# filter out items that were skipped on first pass for first pass regression model
spillover3_fpr <- spillover3 %>% 
  filter(first_pass_regression != '.')

fpr_s3 <- brm(first_pass_regression ~ type + length + trial_index +
                (1 + type | session_id) +
                (1 + type | group_number),
              data = spillover3_fpr,
              chains = 4,
              cores = 4,
              family = bernoulli,
              warmup = 1000,
              iter = 6000)

write_rds(fpr_s3, "models-omnibus/first-pass-regression-spillover3-omnibus.RDS")
fpr_s3_df <- as_tibble(fixef(fpr_s3), rownames = "parameter")
fpr_s3_df <- fpr_s3_df %>%
  mutate(region = 'spillover-3',
         metric = 'first pass regression',
         model = 'omnibus')

fps_s3 <- brm(first_pass_skip ~ type + length + trial_index +
                (1 + type | session_id) +
                (1 + type | group_number),
              data = spillover3,
              chains = 4,
              cores = 4,
              family = bernoulli,
              warmup = 1000,
              iter = 6000)

write_rds(fps_s3, "models-omnibus/first-pass-skip-spillover3-omnibus.RDS")
fps_s3_df <- as_tibble(fixef(fps_s3), rownames = "parameter")
fps_s3_df <- fps_s3_df %>%
  mutate(region = 'spillover-3',
         metric = 'first pass skip',
         model = 'omnibus')


estimates <- bind_rows(estimates, ff_s3_df, fp_s3_df, gp_s3_df, td_s3_df, 
                       fpr_s3_df, fps_s3_df)




# write_excel_csv(estimates, "model-estimates-all-omnibus.csv")
