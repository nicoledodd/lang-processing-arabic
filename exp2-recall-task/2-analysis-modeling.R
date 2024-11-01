## analyzing and modeling recall data

library(tidyverse)
library(openxlsx)
library(brms)

#### Load & clean data ---------------------------------------------------------
data <- read.xlsx("data/clean/recall-data-10Nov2021.xlsx")

# clean data: filter out filler items, incomplete items, excluded stim
data <- data %>% 
  filter(!str_detect(stim, 'F')) %>% # remove filler items
  filter(!str_detect(stim, 'P')) # remove practice items
  
data <- data %>% 
  filter(`total-correct` != 'x') %>% # remove incomplete items
  filter(stim != 'SRC31' & stim != 'ORC31' & stim != 'SRC45' & stim != 'ORC45') %>% # remove excluded stim
  rename(c('totalCorrect' = 'total-correct',
           'clauseTypeCorrect' = 'clause-type-correct')) %>% # rename problematic columns
  select(-Block) %>% # get rid of Block column
  mutate(totalCorrect = as.integer(totalCorrect)) %>% # coerce totalCorrect into int
  mutate(clauseTypeCorrect = as.integer(clauseTypeCorrect)) # coerce clauseTypeCorrect into int

# split RC type and item number
data <- data %>% 
  separate(stim, into = c('stim', 'item'), 
           sep = '(?=\\d+)', # use look ahead to retain separator values
           extra = 'merge') # retain more than one digit

# turn item into int
data <- data %>% 
  mutate(item = as.integer(item))


#### Descriptive stats ---------------------------------------------------------

# overall error rate
meanCorrectness <- mean(data$totalCorrect)
meanClauseCorrectness <- mean(data$clauseTypeCorrect, na.rm = TRUE)

# get counts by item type
trials <- data %>% 
  group_by(stim, clauseTypeCorrect) %>% 
  summarise(count = n())

# summarise by RC type
meanByRC <- data %>% 
  group_by(stim) %>% 
  summarise(meanCorrect = mean(totalCorrect),
            meanClauseCorrect = mean(clauseTypeCorrect))

# prop of errors as ORC -> SRC vs SRC -> ORC
wrong <- data %>% 
  filter(clauseTypeCorrect == 0)

propWrong <- mean(wrong$ORCtoSRC)




#### Logistic regressions ------------------------------------------------------

# calculate centered verb length
verb_by_item <- data %>% 
  group_by(stim, item, verbLen) %>% 
  summarise()

m <- mean(verb_by_item$verbLen)
  
verb_by_item <- verb_by_item %>% 
  mutate(verbLenCent = (verbLen - m))

# left join by stim, item
data <- left_join(data, verb_by_item, by = c('stim', 'item', 'verbLen'))

# convert everything into factors
data <- data %>% 
  mutate(stim = as.factor(stim),
         totalCorrect = as.factor(totalCorrect),
         clauseTypeCorrect = as.factor(clauseTypeCorrect),
         item = as.factor(item),
         verbLenSL = as.factor(verbLenSL))

# set sum coding
set.seed(2)
options (contrasts = c('contr.sum','contr.sum'))


## MODEL 1: overall correctness as a function of clause type
model1 = brm(totalCorrect ~ stim + (1 + stim | prolific_ID) + (1 + stim | item),
                      data = data,
                      family = 'bernoulli',
                      chains = 4,
                      cores = 2,
                      warmup = 100,
                      iter = 1000,
                      thin = 4)


# output & inspect model
# write_rds(model1, 'models/model1.RDS')

model1 <- readRDS('models/model1.RDS')
model1


## MODEL 2: clause correctness as a function of clause type
model2 = brm(clauseTypeCorrect ~ stim + (1 + stim | prolific_ID) + (1 + stim | item),
             data = data,
             family = 'bernoulli',
             chains = 4,
             cores = 2,
             warmup = 100,
             iter = 1000,
             thin = 4)


## output & inspect model
#write_rds(model2, 'models/model2.RDS')

model2 <- readRDS('models/model2.RDS')
model2


## MODEL 3: overall correctness as a function of clause type + RC verb length
# short = verb length of 3 or 4; long = verb length of 5, 6, or 7
model3 = brm(totalCorrect ~ stim + verbLen + (1 + stim + verbLen | prolific_ID) + (1 + stim | item),
             data = data,
             family = 'bernoulli',
             chains = 4,
             cores = 2,
             warmup = 200,
             iter = 2000,
             thin = 4)


## output & inspect model
# write_rds(model3, 'models/model3.RDS')

model3 <- readRDS('models/model3.RDS')
model3


## MODEL 4: clause correctness as a function of clause type + RC verb length
# short = verb length of 3 or 4; long = verb length of 5, 6, or 7
model4 = brm(clauseTypeCorrect ~ stim + verbLenSL + (1 + stim + verbLenSL | prolific_ID) + (1 + stim | item),
             data = data,
             family = 'bernoulli',
             chains = 4,
             cores = 2,
             warmup = 100,
             iter = 1000,
             thin = 4)


## output & inspect model
# write_rds(model4, 'models/model4.RDS')

model4 <- readRDS('models/model4.RDS')
model4


## MODEL 5: overall correctness as a function of clause type + RC verb length and their interaction
# short = verb length of 3 or 4; long = verb length of 5, 6, or 7
## INSIGNIFICANT
model5 = brm(totalCorrect ~ stim * verbLenSL + (1 + stim * verbLenSL | prolific_ID) + (1 + stim | item),
             data = data,
             family = 'bernoulli',
             chains = 4,
             cores = 2,
             warmup = 100,
             iter = 1000,
             thin = 4)


## output & inspect model
# write_rds(model5, 'models/model5.RDS')

model5 <- readRDS('models/model5.RDS')
model5


## MODEL 6: clause correctness as a function of clause type + RC verb length and their interaction
# short = verb length of 3 or 4; long = verb length of 5, 6, or 7
## INSIGNIFICANT
model6 = brm(clauseTypeCorrect ~ stim * verbLenSL + (1 + stim * verbLenSL | prolific_ID) + (1 + stim | item),
             data = data,
             family = 'bernoulli',
             chains = 4,
             cores = 2,
             warmup = 100,
             iter = 1000,
             thin = 4)


## output & inspect model
# write_rds(model6, 'models/model6.RDS')

model6 <- readRDS('models/model6.RDS')
model6


## MODEL 7: overall correctness as a function of clause type + RC verb length (continuous and centered) 
## and their interaction
model7 = brm(totalCorrect ~ stim * verbLenCent + 
               (1 + stim * verbLenCent | prolific_ID) + 
               (1 + stim | item),
             data = data,
             family = 'bernoulli',
             chains = 4,
             cores = 4,
             warmup = 100,
             iter = 1000,
             thin = 4)

## output & inspect model
write_rds(model7, 'models/model7.RDS')

model7


## MODEL 8: clause correctness as a function of clause type + RC verb length (continuous and centered) 
## and their interaction
model8 = brm(clauseTypeCorrect ~ stim * verbLenCent + 
               (1 + stim * verbLenCent | prolific_ID) + 
               (1 + stim | item),
             data = data,
             family = 'bernoulli',
             chains = 4,
             cores = 4,
             warmup = 100,
             iter = 1000,
             thin = 4)

## output & inspect model
# write_rds(model8, 'models/model8.RDS')

model8 <- readRDS('models/model8.RDS')

model8


## figure out what portion of data falls within a given interval in the CI
model_fixef = fixef(model8)
tmp = round(colMeans(fixef(model8, summary = FALSE) > 0)*100,0)
model_fixef = cbind(model_fixef, "% > 0"= tmp)
model_fixef




#### Plot proportion of correctness as a function of verb length by clause type

## get mean correctness by verbLen and type
totalCorrect <- data %>% 
  group_by(stim, verbLen) %>% 
  summarise(prop = mean(totalCorrect))

clauseCorrect <- data %>% 
  group_by(stim, verbLen) %>% 
  summarise(prop = mean(clauseTypeCorrect))


## plot
ggplot(totalCorrect, aes(x = verbLen, y = prop)) +
  geom_col(width = 0.5) +
  facet_wrap(~ stim) + 
  scale_y_continuous(labels = scales::percent_format()) +
  ylab("Proportion of correct answers") +
  xlab("Verb length") +
  theme(legend.title = element_blank())

ggplot(clauseCorrect, aes(x = verbLen, y = prop)) +
  geom_col(width = 0.5) +
  facet_wrap(~ stim) + 
  scale_y_continuous(labels = scales::percent_format()) +
  ylab("Proportion of correct answers") +
  xlab("Verb length") +
  theme(legend.title = element_blank())
