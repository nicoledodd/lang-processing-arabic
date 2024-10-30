## preprocessing and cleaning self-paced reading data


library(tidyverse)
library(openxlsx)

#### import data ---------------------------------------------------------------
csv_raw <- read.csv('data/raw/results-Prolific-4May2021', header = FALSE, fill = TRUE, 
                    comment.char = '#', 
                    col.names = c('Col1', 'Col2', 'Col3', 'Col4', 'Col5', 'Col6',
                                  'Col7', 'Col8', 'Col9', 'Col10', 'Col11', 'Col12'),
                    encoding = 'UTF-8')



#### process participant data --------------------------------------------------
## doing this first to write w/o cleaning; need to maintain all participant data w/o cuts

participant_data <- csv_raw %>%
  filter(Col3 == 'Form')

# rename columns
participant_data <- participant_data %>%
  rename('time' = 'Col1',
         'participant_ID' = 'Col2',
         'controller' = 'Col3',
         'ibex_item_number' = 'Col4', # this is unique to Ibex, not part of our numbering system
         'element_number' = 'Col5',
         'item_type' = 'Col6',
         'group_number' = 'Col7',
         'field_name' = 'Col8',
         'field_value' = 'Col9')

# get rid of unwanted columns
participant_data <- participant_data %>%
  select(participant_ID, field_name, field_value)

# filter out RTs for participant questionnaire
participant_data <- participant_data %>%
  filter(field_name != '_REACTION_TIME_')

# pivot wider
participant_data <- participant_data %>%
  pivot_wider(names_from = field_name, values_from = field_value)

# write
# write.xlsx(participant_data, 'data/clean/participants/participant-data-4May2021.xlsx')



#### process RT data -----------------------------------------------------------
rt_data <- csv_raw %>% 
  filter(Col3 == 'RLDashedSentence')

# rename columns
rt_data <- rt_data %>% 
  rename('time' = 'Col1',
         'participant_ID' = 'Col2',
         'controller' = 'Col3',
         'ibex_item_number' = 'Col4', # this is unique to Ibex, not part of our numbering system
         'element_number' = 'Col5',
         'item_type' = 'Col6',
         'group_number' = 'Col7',
         'word_number' = 'Col8',
         'word' = 'Col9',
         'RT' = 'Col10',
         'new_line' = 'Col11',
         'sentence' = 'Col12')

# remove excluded participants (after reviewing participant data)
bad_participants <- c('a0430e86faeda700a1c82a92f9b3de81',
                      'd9775d7512438c2102b63729a3c3bd2e')

rt_data <- rt_data %>% 
  filter(! participant_ID %in% bad_participants)

# filter out practice and filler items
rt_data <- rt_data %>% 
  filter((item_type == 'src') | (item_type == 'orc'))

# get rid of unwanted columns
rt_data <- rt_data %>% 
  select(participant_ID, word_number, word, RT, sentence)

# import stim types
stim_types <- read.xlsx('data/stim-types.xlsx')

rt_data <- left_join(rt_data, stim_types, by = 'sentence')



## fix problematic item_number 33 that won't match sentences by string
# get indices of problem rows
item33 <- rt_data[is.na(rt_data$item_type), ]
problems <- as.numeric(rownames(item33))

# write group number by index
rt_data$group_number[row.names(rt_data) %in% problems] <- 33

# write SRC vs ORC
word <- 'زارتها'
rt_data$item_type[(row.names(rt_data) %in% problems) & (grepl(word, rt_data$sentence))] <- 'ORC'
rt_data$item_type[(row.names(rt_data) %in% problems) & !(grepl(word, rt_data$sentence))] <- 'SRC'

# write item number
rt_data$item_number[(rt_data$group_number == 33) & (rt_data$item_type == 'ORC')] <- 69
rt_data$item_number[(rt_data$group_number == 33) & (rt_data$item_type == 'SRC')] <- 29



# make a unique item-word number
rt_data <- rt_data %>% 
  unite(item_word_number, item_number, word_number, sep = "-", remove = FALSE)

# add column for length
rt_data <- rt_data %>% 
  mutate(length = nchar(word, type = 'width'))

# add regions data
regions <- read.xlsx('data/regions.xlsx')

regions <- regions %>% 
  select(item_word_number, region) %>% # narrow down to only columns we want
  mutate(item_word_number = as.character(item_word_number)) # make char for join

rt_data <- rt_data %>% 
  left_join(regions, by = c('item_word_number'))




#### process comp question data ------------------------------------------------
comp_data <- csv_raw %>% 
  filter(Col3 == 'Question')

# rename columns
comp_data <- comp_data %>% 
  rename('time' = 'Col1',
         'participant_ID' = 'Col2',
         'controller' = 'Col3',
         'ibex_item_number' = 'Col4', # this is unique to Ibex, not part of our numbering system
         'element_number' = 'Col5',
         'item_type' = 'Col6',
         'group_number' = 'Col7',
         'question' = 'Col8',
         'answer' = 'Col9',
         'correct' = 'Col10',
         'RT' = 'Col11')

# remove excluded participants
comp_data <- comp_data %>% 
  filter(! participant_ID %in% bad_participants)

# filter out practice items
comp_data <- comp_data %>% 
  filter(item_type != 'practice')

# get rid of unwanted columns
comp_data <- comp_data %>% 
  select(participant_ID, item_type, question, answer, correct)

# fix item types to be compatible
comp_data$item_type[comp_data$item_type == 'src'] <- 'SRC'
comp_data$item_type[comp_data$item_type == 'orc'] <- 'ORC'
comp_data$item_type[comp_data$item_type == 'f-hf'] <- 'filler-HF'
comp_data$item_type[comp_data$item_type == 'f-lf'] <- 'filler-LF'

# add comp question item_type, correct ans, stim gender
ques_types <- read.xlsx('data/comp-ques-types.xlsx')
ques_types <- ques_types %>% select(-notes)

comp_data <- comp_data %>% 
  left_join(ques_types, by = c('question', 'item_type'))


## once again fix problematic items
# get indices of problem rows
items <- comp_data[is.na(comp_data$item_number), ]
problems <- as.numeric(rownames(items))

# two problem items this time, so do by key word
word1 <- 'استقبل'
comp_data$group_number[(row.names(comp_data) %in% problems) & (grepl(word1, comp_data$question))] <- 8
comp_data$ques_type[(row.names(comp_data) %in% problems) & (grepl(word1, comp_data$question))] <- 'clausal'
comp_data$correct_ans[(row.names(comp_data) %in% problems) & (grepl(word1, comp_data$question))] <- 'no'
comp_data$stim_gender[(row.names(comp_data) %in% problems) & (grepl(word1, comp_data$question))] <- 'F'
comp_data$item_number[(row.names(comp_data) %in% problems) & (grepl(word1, comp_data$question)) & (comp_data$item_type == 'SRC')] <- 7
comp_data$item_number[(row.names(comp_data) %in% problems) & (grepl(word1, comp_data$question)) & (comp_data$item_type == 'ORC')] <- 47

word2 <- 'الممثلة'
comp_data$group_number[(row.names(comp_data) %in% problems) & (grepl(word2, comp_data$question))] <- 33
comp_data$ques_type[(row.names(comp_data) %in% problems) & (grepl(word2, comp_data$question))] <- 'clausal'
comp_data$correct_ans[(row.names(comp_data) %in% problems) & (grepl(word2, comp_data$question))] <- 'no'
comp_data$stim_gender[(row.names(comp_data) %in% problems) & (grepl(word2, comp_data$question))] <- 'F'
comp_data$item_number[(row.names(comp_data) %in% problems) & (grepl(word2, comp_data$question)) & (comp_data$item_type == 'SRC')] <- 29
comp_data$item_number[(row.names(comp_data) %in% problems) & (grepl(word2, comp_data$question)) & (comp_data$item_type == 'ORC')] <- 69



## filter out problem questions
# remove questions for group_numbers 8 and 9 due to coding errors
comp_data <- comp_data %>% 
  filter((group_number != '8') & (group_number != '9'))


# check if anyone scored below 75% overall for exclusion
ques_summary <- comp_data %>% 
  filter(ques_type == 'comprehension') %>% # excluding clausal comprehension questions
  group_by(participant_ID) %>% 
  summarise(m = mean(correct)) %>% 
  arrange(m)
# 1 participant scored exactly 75% (895470cc7159b025b0226b4a9ae4544a); keep because still technically good


# remove filler questions after checking for exclusions 
comp_data <- comp_data %>% 
  filter((item_type != 'filler-HF') & (item_type != 'filler-LF'))

# write for analysis
# write.xlsx(comp_data, 'data/clean/comp questions/comp_data-NEW.xlsx')




#### combine RT and comp question data -----------------------------------------
all_data <- left_join(rt_data, comp_data, by = c('participant_ID', 'item_number', 'group_number', 'item_type'))

# manual first cut outliers
all_data <- all_data %>% 
  filter((RT >= 100) & (RT <= 2000))

# write for analysis
# write.csv(all_data, 'data/clean/rts/all_data-NEW.csv', row.names = FALSE)



