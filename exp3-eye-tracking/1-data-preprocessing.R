## cleaning and preprocessing eye tracking data


library(tidyverse)
library(openxlsx)

#### Get stim IA labels for analysis -------------------------------------------

# read in data
stim <- read.xlsx('/stimuli/eyetracking-80/eyetracking-stimuli-master-ND22Aug2023.xlsx')

stim <- as_tibble(stim)

# filter down
stim <- stim %>%
  filter(`item-type` == 'SRC' | `item-type` == 'ORC' | `item-type` == 'filler-clitic' | `item-type` == 'filler-noclitic') %>%
  select(`item-number`, `matrix-SUBJ1`:`filler-spillover`) %>%
  mutate(`matrix-SUBJ2` = as.double(`matrix-SUBJ2`),
         `rel-NP2` = as.double(`rel-NP2`)) # read in as chr

# pivot longer
stim <- stim %>%
  pivot_longer(`matrix-SUBJ1`:`filler-spillover`, names_to = "IA_label",
               values_to = "CURRENT_FIX_INTEREST_AREA_INDEX",
               values_drop_na = TRUE) %>%
  rename(item_number = `item-number`)

# write to hand-correct IA labels
# write_excel_csv(stim, 'ia-labels.csv')


#### read in data --------------------------------------------------------------
data <- read.xlsx('data/fixation-report-26Oct2023.xlsx')
data2 <- read.xlsx('data/fixation-report-RCverb-14Nov2023.xlsx') # for rel-VERB-whole and filler-WORD-whole

labels <- read_csv('data/ia-labels.csv')
labels2 <- read_csv('data/ia-labels-relVerbWhole.csv')

# left join w/ IA info
data <- left_join(data, labels, by = join_by(item_number, CURRENT_FIX_INTEREST_AREA_INDEX))
data2 <- left_join(data2, labels2, by = join_by(item_number, CURRENT_FIX_INTEREST_AREA_INDEX))

# replace correctness column w/ 1s and 0s
data$IsAnswerCorrect[data$IsAnswerCorrect == 'TRUE'] <- 1
data2$IsAnswerCorrect[data2$IsAnswerCorrect == 'TRUE'] <- 1



#### prep data for Get Reading Measures ----------------------------------------

# # pivot wider for Get Reading Measures
# labels <- labels %>%
#   pivot_wider(names_from = 'IA_label', values_from = 'CURRENT_FIX_INTEREST_AREA_INDEX')
# 
# labels2 <- labels2 %>%
#   pivot_wider(names_from = 'IA_label', values_from = 'CURRENT_FIX_INTEREST_AREA_INDEX')
# 
# # left join w/ IA info
# data <- left_join(data, labels, by = 'item_number')
# data2 <- left_join(data2, labels2, by = 'item_number')
# 
# # rename CURRENT_FIX_INTEREST_AREA_INDEX to work with GRM2
# data <- data %>%
#   rename(CURRENT_FIX_INTEREST_AREA_ID = CURRENT_FIX_INTEREST_AREA_INDEX)
# 
# data2 <- data2 %>%
#   rename(CURRENT_FIX_INTEREST_AREA_ID = CURRENT_FIX_INTEREST_AREA_INDEX)
# 
# ## filter down for individual IAs
# 
# # matrix-SUBJ
# items <- data %>%
#   select(`RECORDING_SESSION_LABEL`, `TRIAL_INDEX`, `CURRENT_FIX_INDEX`,
#          `CURRENT_FIX_DURATION`, `CURRENT_FIX_INTEREST_AREA_INDEX`,
#          `IsAnswerCorrect`, `group_number`, `item_number`, `item_type`,
#          `list`, `matrix-SUBJ`) %>%
#   drop_na()
# 
# write_delim(items, delim = "\t", "data/measures/matrix-SUBJ.txt")
# 
# 
# # matrix-VERB
# items <- data %>%
#   select(`RECORDING_SESSION_LABEL`, `TRIAL_INDEX`, `CURRENT_FIX_INDEX`,
#          `CURRENT_FIX_DURATION`, `CURRENT_FIX_INTEREST_AREA_INDEX`,
#          `IsAnswerCorrect`, `group_number`, `item_number`, `item_type`,
#          `list`, `matrix-VERB`) %>%
#   drop_na()
# 
# write_delim(items, delim = "\t", "data/measures/matrix-VERB.txt")
# 
# 
# # rel-CLITIC
# items <- data %>%
#   select(`RECORDING_SESSION_LABEL`, `TRIAL_INDEX`, `CURRENT_FIX_INDEX`,
#          `CURRENT_FIX_DURATION`, `CURRENT_FIX_INTEREST_AREA_INDEX`,
#          `IsAnswerCorrect`, `group_number`, `item_number`, `item_type`,
#          `list`, `rel-CLITIC`) %>%
#   drop_na()
# 
# write_delim(items, delim = "\t", "data/measures/rel-CLITIC.txt")
# 
# items <- data %>%
#   filter(item_type == 'ORC') %>% 
#   select(`RECORDING_SESSION_LABEL`, `TRIAL_INDEX`, `CURRENT_FIX_INDEX`,
#          `CURRENT_FIX_DURATION`, `CURRENT_FIX_INTEREST_AREA_INDEX`,
#          `IsAnswerCorrect`, `group_number`, `item_number`, `item_type`,
#          `list`, `rel-VERB`) %>%
#   drop_na()
# 
# 
# # rel-NP
# items <- data %>%
#   select(`RECORDING_SESSION_LABEL`, `TRIAL_INDEX`, `CURRENT_FIX_INDEX`,
#          `CURRENT_FIX_DURATION`, `CURRENT_FIX_INTEREST_AREA_INDEX`,
#          `IsAnswerCorrect`, `group_number`, `item_number`, `item_type`,
#          `list`, `rel-NP`) %>%
#   drop_na()
# 
# write_delim(items, delim = "\t", "data/measures/rel-NP.txt")
# 
# 
# # rel-PRON
# items <- data %>%
#   select(`RECORDING_SESSION_LABEL`, `TRIAL_INDEX`, `CURRENT_FIX_INDEX`,
#          `CURRENT_FIX_DURATION`, `CURRENT_FIX_INTEREST_AREA_INDEX`,
#          `IsAnswerCorrect`, `group_number`, `item_number`, `item_type`,
#          `list`, `rel-PRON`) %>%
#   drop_na()
# 
# write_delim(items, delim = "\t", "data/measures/rel-PRON.txt")
# 
# 
# # rel-VERB
# items <- data %>%
#   select(`RECORDING_SESSION_LABEL`, `TRIAL_INDEX`, `CURRENT_FIX_INDEX`,
#          `CURRENT_FIX_DURATION`, `CURRENT_FIX_INTEREST_AREA_INDEX`,
#          `IsAnswerCorrect`, `group_number`, `item_number`, `item_type`,
#          `list`, `rel-VERB`) %>%
#   drop_na()
# 
# write_delim(items, delim = "\t", "data/measures/rel-VERB.txt")
# 
# 
# # spillover-1
# items <- data %>%
#   select(`RECORDING_SESSION_LABEL`, `TRIAL_INDEX`, `CURRENT_FIX_INDEX`,
#          `CURRENT_FIX_DURATION`, `CURRENT_FIX_INTEREST_AREA_INDEX`,
#          `IsAnswerCorrect`, `group_number`, `item_number`, `item_type`,
#          `list`, `spillover-1`) %>%
#   drop_na()
# 
# write_delim(items, delim = "\t", "data/measures/spillover-1.txt")
# 
# 
# # spillover-2
# items <- data %>%
#   select(`RECORDING_SESSION_LABEL`, `TRIAL_INDEX`, `CURRENT_FIX_INDEX`,
#          `CURRENT_FIX_DURATION`, `CURRENT_FIX_INTEREST_AREA_INDEX`,
#          `IsAnswerCorrect`, `group_number`, `item_number`, `item_type`,
#          `list`, `spillover-2`) %>%
#   drop_na()
# 
# write_delim(items, delim = "\t", "data/measures/spillover-2.txt")
# 
# 
# # spillover-3
# items <- data %>%
#   select(`RECORDING_SESSION_LABEL`, `TRIAL_INDEX`, `CURRENT_FIX_INDEX`,
#          `CURRENT_FIX_DURATION`, `CURRENT_FIX_INTEREST_AREA_INDEX`,
#          `IsAnswerCorrect`, `group_number`, `item_number`, `item_type`,
#          `list`, `spillover-3`) %>%
#   drop_na()
# 
# write_delim(items, delim = "\t", "data/measures/spillover-3.txt")
# 
# 
# # rel-VERB-whole
# items <- data2 %>%
#   select(`RECORDING_SESSION_LABEL`, `TRIAL_INDEX`, `CURRENT_FIX_INDEX`,
#          `CURRENT_FIX_DURATION`, `CURRENT_FIX_INTEREST_AREA_ID`,
#          `IsAnswerCorrect`, `group_number`, `item_number`, `item_type`,
#          `list`, `rel-VERB-whole`) %>% 
#   drop_na()
# 
# write_delim(items, delim = "\t", "data/measures/rel-VERB-whole.txt")
# 
# 
# # filler-WORD
# items <- data %>%
#   filter(item_type == 'filler-clitic' | item_type == 'filler-noclitic') %>%
#   select(`RECORDING_SESSION_LABEL`, `TRIAL_INDEX`, `CURRENT_FIX_INDEX`,
#          `CURRENT_FIX_DURATION`, `CURRENT_FIX_INTEREST_AREA_ID`,
#          `IsAnswerCorrect`, `group_number`, `item_number`, `item_type`,
#          `list`, `filler-WORD`) %>%
#   drop_na()
# 
# write_delim(items, delim = "\t", "data/measures/filler-WORD.txt")
# 
# 
# # filler-CLITIC
# items <- data %>%
#   filter(item_type == 'filler-clitic' | item_type == 'filler-noclitic') %>%
#   select(`RECORDING_SESSION_LABEL`, `TRIAL_INDEX`, `CURRENT_FIX_INDEX`,
#          `CURRENT_FIX_DURATION`, `CURRENT_FIX_INTEREST_AREA_ID`,
#          `IsAnswerCorrect`, `group_number`, `item_number`, `item_type`,
#          `list`, `filler-CLITIC`) %>%
#   drop_na()
# 
# write_delim(items, delim = "\t", "data/measures/filler-CLITIC.txt")
# 
# 
# # filler-WORD-whole
# items <- data2 %>%
#   filter(item_type == 'filler-clitic' | item_type == 'filler-noclitic') %>%
#   select(`RECORDING_SESSION_LABEL`, `TRIAL_INDEX`, `CURRENT_FIX_INDEX`,
#          `CURRENT_FIX_DURATION`, `CURRENT_FIX_INTEREST_AREA_ID`,
#          `IsAnswerCorrect`, `group_number`, `item_number`, `item_type`,
#          `list`, `filler-WORD-whole`) %>%
#   drop_na()
# 
# write_delim(items, delim = "\t", "data/measures/filler-WORD-whole.txt")
# 
# 
# # filler-spillover
# items <- data %>%
#   filter(item_type == 'filler-clitic' | item_type == 'filler-noclitic') %>%
#   select(`RECORDING_SESSION_LABEL`, `TRIAL_INDEX`, `CURRENT_FIX_INDEX`,
#          `CURRENT_FIX_DURATION`, `CURRENT_FIX_INTEREST_AREA_ID`,
#          `IsAnswerCorrect`, `group_number`, `item_number`, `item_type`,
#          `list`, `filler-spillover`) %>%
#   drop_na()
# 
# write_delim(items, delim = "\t", "data/measures/filler-spillover.txt")



#### add measures and labels to fixation report --------------------------------
matrix_SUBJ <- read_delim("data/measures/matrix-SUBJ-measures.txt", delim = "\t")
matrix_VERB <- read_delim("data/measures/matrix-VERB-measures.txt", delim = "\t")
rel_CLITIC <- read_delim("data/measures/rel-CLITIC-measures.txt", delim = "\t")
rel_NP <- read_delim("data/measures/rel-NP-measures.txt", delim = "\t")
rel_PRON <- read_delim("data/measures/rel-PRON-measures.txt", delim = "\t")
rel_VERB <- read_delim("data/measures/rel-VERB-measures.txt", delim = "\t")
spillover_1 <- read_delim("data/measures/spillover-1-measures.txt", delim = "\t")
spillover_2 <- read_delim("data/measures/spillover-2-measures.txt", delim = "\t")
spillover_3 <- read_delim("data/measures/spillover-3-measures.txt", delim = "\t")
rel_VERB_whole <- read_delim("data/measures/rel-VERB-whole-measures.txt", delim = "\t")
filler_WORD <- read_delim("data/measures/filler-WORD-measures.txt", delim = "\t")
filler_CLITIC <- read_delim("data/measures/filler-CLITIC-measures.txt", delim = "\t")
filler_spillover <- read_delim("data/measures/filler-spillover-measures.txt", delim = "\t")
filler_WORD_whole <- read_delim("data/measures/filler-WORD-whole-measures.txt", delim = "\t")

matrix_SUBJ <- matrix_SUBJ %>% 
  mutate(region = 'matrix-SUBJ') %>% 
  rename('CURRENT_FIX_INTEREST_AREA_INDEX' = `matrix-SUBJ`)

matrix_VERB <- matrix_VERB %>% 
  mutate(region = 'matrix-VERB') %>% 
  rename('CURRENT_FIX_INTEREST_AREA_INDEX' = `matrix-VERB`)

rel_CLITIC <- rel_CLITIC %>% 
  mutate(region = 'rel-CLITIC') %>% 
  rename('CURRENT_FIX_INTEREST_AREA_INDEX' = `rel-CLITIC`)

rel_NP <- rel_NP %>% 
  mutate(region = 'rel-NP') %>% 
  rename('CURRENT_FIX_INTEREST_AREA_INDEX' = `rel-NP`)

rel_PRON <- rel_PRON %>% 
  mutate(region = 'rel-PRON') %>% 
  rename('CURRENT_FIX_INTEREST_AREA_INDEX' = `rel-PRON`)

rel_VERB <- rel_VERB %>% 
  mutate(region = 'rel-VERB') %>% 
  rename('CURRENT_FIX_INTEREST_AREA_INDEX' = `rel-VERB`)

spillover_1 <- spillover_1 %>% 
  mutate(region = 'spillover-1') %>% 
  rename('CURRENT_FIX_INTEREST_AREA_INDEX' = `spillover-1`)

spillover_2 <- spillover_2 %>% 
  mutate(region = 'spillover-2') %>% 
  rename('CURRENT_FIX_INTEREST_AREA_INDEX' = `spillover-2`)

spillover_3 <- spillover_3 %>% 
  mutate(region = 'spillover-3') %>% 
  rename('CURRENT_FIX_INTEREST_AREA_INDEX' = `spillover-3`)

rel_VERB_whole <- rel_VERB_whole %>% 
  mutate(region = 'rel-VERB-whole',
         CURRENT_FIX_INTEREST_AREA_INDEX = 3)

# NB: these are different bc data processed with GRM2
filler_WORD <- filler_WORD %>% 
  rename(`CURRENT_FIX_INTEREST_AREA_INDEX` = `filler-WORD`,
         region = `REGION_LABEL`,
         `FIRST_PAST_FIXATION` = `FIRST_PASS_FIXATION`) %>% # just to play nice with others
  select(RECORDING_SESSION_LABEL, TRIAL_INDEX, IsAnswerCorrect, group_number, 
         item_number, item_type, list, CURRENT_FIX_INTEREST_AREA_INDEX, 
         FIRST_FIXATION_DURATION, GAZE_DURATION, REGRESSION_PATH_DURATION,
         TOTAL_DURATION, FIRST_PASS_REGRESSION, FIRST_PAST_FIXATION, region)

filler_CLITIC <- filler_CLITIC %>% 
  rename(`CURRENT_FIX_INTEREST_AREA_INDEX` = `filler-CLITIC`,
         region = `REGION_LABEL`,
         `FIRST_PAST_FIXATION` = `FIRST_PASS_FIXATION`) %>% # just to play nice with others
  select(RECORDING_SESSION_LABEL, TRIAL_INDEX, IsAnswerCorrect, group_number, 
         item_number, item_type, list, CURRENT_FIX_INTEREST_AREA_INDEX, 
         FIRST_FIXATION_DURATION, GAZE_DURATION, REGRESSION_PATH_DURATION,
         TOTAL_DURATION, FIRST_PASS_REGRESSION, FIRST_PAST_FIXATION, region)

filler_spillover <- filler_spillover %>% 
  rename(`CURRENT_FIX_INTEREST_AREA_INDEX` = `filler-spillover`,
         region = `REGION_LABEL`,
         `FIRST_PAST_FIXATION` = `FIRST_PASS_FIXATION`) %>% # just to play nice with others
  select(RECORDING_SESSION_LABEL, TRIAL_INDEX, IsAnswerCorrect, group_number, 
         item_number, item_type, list, CURRENT_FIX_INTEREST_AREA_INDEX, 
         FIRST_FIXATION_DURATION, GAZE_DURATION, REGRESSION_PATH_DURATION,
         TOTAL_DURATION, FIRST_PASS_REGRESSION, FIRST_PAST_FIXATION, region)

filler_WORD_whole <- filler_WORD_whole %>% 
  rename(`CURRENT_FIX_INTEREST_AREA_INDEX` = `filler-WORD`,
         region = `REGION_LABEL`,
         `FIRST_PAST_FIXATION` = `FIRST_PASS_FIXATION`) %>% # just to play nice with others
  select(RECORDING_SESSION_LABEL, TRIAL_INDEX, IsAnswerCorrect, group_number, 
         item_number, item_type, list, CURRENT_FIX_INTEREST_AREA_INDEX, 
         FIRST_FIXATION_DURATION, GAZE_DURATION, REGRESSION_PATH_DURATION,
         TOTAL_DURATION, FIRST_PASS_REGRESSION, FIRST_PAST_FIXATION, region)


metrics <- bind_rows(matrix_SUBJ, rel_PRON, rel_VERB, rel_CLITIC, rel_VERB_whole,
                     rel_NP, matrix_VERB, spillover_1, spillover_2, spillover_3, 
                     filler_WORD, filler_CLITIC, filler_WORD_whole, filler_spillover)



#### get arb words -------------------------------------------------------------
arb_words <- data %>% 
  group_by(item_number, CURRENT_FIX_INTEREST_AREA_INDEX, CURRENT_FIX_INTEREST_AREA_LABEL, IA_label) %>% 
  summarise() %>% 
  drop_na()

arb_words2 <- data2 %>% 
  group_by(item_number, CURRENT_FIX_INTEREST_AREA_INDEX, CURRENT_FIX_INTEREST_AREA_LABEL, IA_label) %>% 
  summarise() %>% 
  drop_na()

arb_words2 <- arb_words2 %>% 
  filter(IA_label == 'rel-VERB-whole' | IA_label == 'filler-WORD-whole')

arb_words_all <- bind_rows(arb_words, arb_words2)

arb_words_all <- arb_words_all %>% 
  rename(region = IA_label) # to play nice with metrics

# add to metrics df
metrics <- left_join(metrics, arb_words_all, by = join_by(item_number, CURRENT_FIX_INTEREST_AREA_INDEX,
                                                          region))


#### clean and add other data --------------------------------------------------
metrics <- metrics %>% 
  rename(session_id = RECORDING_SESSION_LABEL,
         trial_index = TRIAL_INDEX,
         correct = IsAnswerCorrect,
         IA_index = CURRENT_FIX_INTEREST_AREA_INDEX,
         first_fixation = FIRST_FIXATION_DURATION,
         first_pass = GAZE_DURATION,
         go_past = REGRESSION_PATH_DURATION,
         total_duration = TOTAL_DURATION,
         first_pass_regression = FIRST_PASS_REGRESSION,
         first_pass_skip = FIRST_PAST_FIXATION,
         arb = CURRENT_FIX_INTEREST_AREA_LABEL) %>% 
  mutate(session_id = str_replace_all(session_id, c('s' = 'S', 'v' = 'V'))) %>% 
  select(session_id, trial_index, item_type, item_number, group_number, list,
         IA_index, arb, region, first_fixation, first_pass, go_past, total_duration,
         first_pass_regression, first_pass_skip, correct) %>% 
  arrange(session_id, item_number, IA_index)

# add length (excluding whitespace between multi-word IAs)
metrics <- metrics %>% 
  mutate(length = nchar(str_replace_all(arb, " ", "")))



#### some minor tweaks ---------------------------------------------------------
## Get Reading Measures says that first pass fixation (i.e., first pass skip)
## will be 1 if the target region was fixated before any progressive text, and
## 0 if the region was skipped during first pass reading. Since the column is 
## labeled 'first pass skip', I'm going to switch these numbers so 1 indicates
## a skip and 0 indicates no skip

skips <- c()
for (i in seq_along(metrics$first_pass_skip)) {
  if (metrics$first_pass_skip[i] == '1') {
    skips <- append(skips, 0)
  }
  else {
    skips <- append(skips, 1)
  }
}

metrics <- metrics %>% 
  mutate(first_pass_skip = skips)



#### export --------------------------------------------------------------------
# write_excel_csv(metrics, "data/exp3-eye-tracking-clean.csv")
