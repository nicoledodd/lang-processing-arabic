## analyze self-paced reading data using cplDataAnalysis package 
## (not publicly available)


library(tidyverse)
library(cplDataAnalysis)

#### import data ---------------------------------------------------------------
rt_data <- read.csv('data/clean/rts/exp1-spr-rts-clean.csv', header = TRUE)

# fill in 0s for items with excluded comp questions (group numbers 8 and 9)
rt_data$correct[is.na(rt_data$correct)] <- 0

# make everything factors
rt_data <- rt_data %>% 
  mutate(item_type = as.factor(item_type),
         participant_ID = as.factor(participant_ID),
         item_number = as.factor(item_number))

## analyze
spr <- analyze.spr(rt_data, 
            factors = c('item_type'),
            region.list = c('1', '2', '3', '4', '5', '6', '7'),
            subj.name = 'participant_ID',
            item.name = 'item_number',
            correct.name = 'correct',
            filler.name = NULL,
            region.name = 'region',
            rt.name = 'RT',
            wordlen.name = 'length',
            use.res = TRUE, # TRUE if you want residual RTs
            res.correct = FALSE, # do not use only correct items to calculate residuals
            analyze.correct = FALSE, # do not only analyze correct items
            do.first.cut = FALSE, # did first cut during data cleaning
            do.plot = TRUE)



#### get residuals for modeling ------------------------------------------------
resids <- spr[[6]]

# write w resids
# write.csv(resids, 'data/clean/rts/exp1-spr-rts-clean-resids.csv', row.names = FALSE)



