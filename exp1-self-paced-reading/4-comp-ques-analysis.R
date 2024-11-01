## analyzing answers to comprehension questions answered by participants


library(tidyverse)
library(openxlsx)
library(brms)

#### import data ---------------------------------------------------------------
comp_questions <- read.xlsx('data/clean/comp questions/exp1-spr-comp-ques-clean.xlsx')

## filter out problem questions
## remove questions for group_numbers 8 and 9 due to coding errors
comp_questions <- comp_questions %>% 
  filter((group_number != '8') & (group_number != '9'))


#### summary stats -------------------------------------------------------------

# investigate if any group items had super low comp question answers
summary_group <- comp_questions %>% 
  group_by(group_number, ques_type) %>% 
  summarise(m = mean(correct), count = n()) %>% 
  arrange(m)
# group 31 was super low, everything else was above chance

# exclude super low comp question
comp_questions <- comp_questions %>% 
  filter(group_number != '31')

# accuracy by question type (clausal or sentence) after exclusions
summary_ques <- comp_questions %>% 
  group_by(ques_type) %>% 
  summarise(accuracy = (sum(correct)/length(correct))*100)


#### relative clause comprehension ---------------------------------------------

# filter down df
clausal <- comp_questions %>% 
  filter(ques_type == 'clausal')

length(unique(clausal$group_number))
# 17 questions

# number of trials for each condition
trials <- clausal %>% 
  group_by(item_type, correct) %>% 
  summarise(count = n())

# accuracy by group item
clausal_group <- clausal %>% 
  group_by(group_number) %>% 
  summarise(accuracy = mean(correct)) %>% 
  arrange(accuracy)

# are there big differences between SRCs and ORCs for group items with 
# less than 75% accuracy?
under75 <- clausal_group$group_number[clausal_group$accuracy <= 0.75]

under75_dif <- clausal %>% 
  filter(group_number %in% under75) %>% 
  group_by(group_number, item_type) %>% 
  summarise(accuracy = mean(correct)) %>% 
  arrange(group_number)
# in some cases, yes: 11, 13, 33
# in one case, ORC had more correct answers: 29

# investigate this pattern for all items
clausal_item <- clausal %>% 
  group_by(group_number, item_type) %>% 
  summarise(accuracy = mean(correct)) %>% 
  arrange(group_number)


# accuracy by clause and correct answer type
ques_type_summary <- clausal %>% 
  group_by(item_type, correct_ans) %>% 
  summarise(accuracy = mean(correct))



#### modeling ------------------------------------------------------------------
set.seed(2)
options(contrasts = c('contr.sum','contr.sum')) # set sum coding


# turn everything into a factor
clausal <- clausal %>% 
  mutate(participant_ID = factor(participant_ID),
         item_type = factor(item_type),
         correct = factor(correct),
         item_number = factor(item_number),
         group_number = factor(group_number),
         correct_ans = factor(correct_ans),
         stim_gender = factor(stim_gender))


correctness = brm(correct ~ item_type * correct_ans + 
                    (1 + item_type * correct_ans | participant_ID) + 
                    (1 + item_type | group_number),
                  data = clausal,
                  chains = 4,
                  cores = 4,
                  family = 'bernoulli',
                  warmup = 100,
                  iter = 1000)

correctness
# significance all around

write_rds(correctness, 'models/modelComp.RDS')

correctness_df <- as_tibble(fixef(correctness), rownames = "parameter")
correctness_df <- correctness_df %>%
  mutate(model = 'item type-correct ans')


# anything interesting when we throw gender into the mix?
correctnessGen = brm(correct ~ item_type * correct_ans * stim_gender + 
                    (1 + item_type * correct_ans * stim_gender | participant_ID) + 
                    (1 + item_type | group_number),
                  data = clausal,
                  chains = 4,
                  cores = 4,
                  family = 'bernoulli',
                  warmup = 100,
                  iter = 1000)

correctnessGen
# potentially a significant three-way interaction?

# figure out what portion of data falls within a given interval in the CI
model_fixef = fixef(correctnessGen)
tmp = round(colMeans(fixef(correctnessGen, summary = FALSE) > 0)*100,0)
model_fixef = cbind(model_fixef, "% > 0"= tmp)
model_fixef

# not significant

write_rds(correctnessGen, 'models/modelComp-gen.RDS')

correctnessGen_df <- as_tibble(fixef(correctnessGen), rownames = "parameter")
correctnessGen_df <- correctnessGen_df %>%
  mutate(model = 'item type-correct ans-stim gender')



## write estimates
estimates <- bind_rows(correctness_df, correctnessGen_df)
# write.csv(estimates, 'models/comp-ques-analysis-estimates.csv', row.names = FALSE)
