## statistical modeling of self-paced reading data


library(tidyverse)
library(brms)
library(cplDataAnalysis)


#### read in data --------------------------------------------------------------
rt_data <- read.csv('data/clean/rts/exp1-spr-rts-clean-resids.csv', header = TRUE)


#### prep data -----------------------------------------------------------------
rt_data <- rt_data %>%
  filter(region != 'N/A') %>%
  select(participant_ID, item_word_number, word_number, item_number:item_type, length, region, correct, res.rt)

# sum across region 1
region_data <- rt_data %>%
  group_by(participant_ID, item_number, group_number, item_type, region, correct) %>%
  summarise(residRT = sum(res.rt))

# make correct binary again
region_data$correct[region_data$correct == 'TRUE'] <- 1
region_data$correct[region_data$correct == 'FALSE'] <- 0

## make things factors
region_data <- region_data %>%
  mutate(participant_ID = factor(participant_ID),
         item_type = factor(item_type, levels = c('ORC', 'SRC')))




#### MODELING ------------------------------------------------------------------
# set sum coding
set.seed(2)
options(contrasts = c('contr.sum', 'contr.sum'))

 
#### REGIONS 2-4 SUMMED --------------------------------------------------------

region234 <- region_data %>% 
  filter(region == '2' | region == '3' | region == '4') %>% 
  group_by(participant_ID, group_number, item_type) %>% 
  summarise(avRT = sum(residRT))


model234 <- brm(avRT ~ item_type + 
                    (1 + item_type | participant_ID) + 
                    (1 + item_type | group_number),
                  data = region234,
                  chains = 4,
                  cores = 4,
                  family = gaussian,
                  warmup = 200,
                  iter = 2000)

model234

# item_type is borderline significant
# figure out what portion of data falls within a given interval in the CI
model_fixef = fixef(model234)
tmp = round(colMeans(fixef(model234, summary = FALSE) > 0)*100,0)
model_fixef = cbind(model_fixef, "% > 0"= tmp)
model_fixef

# 95%

write_rds(model234, 'model-estimates/modelRegions234-new.RDS')
model234_df <- as_tibble(fixef(model234), rownames = "parameter")
model234_df <- model234_df %>%
  mutate(region = '234',
         model = 'all items')



## only correct items
region234_corr <- region_data %>% 
  filter(region == '2' | region == '3' | region == '4') %>% 
  filter(correct == 1) %>% 
  group_by(participant_ID, group_number, item_type) %>% 
  summarise(avRT = sum(residRT))


model234_c <- brm(avRT ~ item_type + 
                    (1 + item_type | participant_ID) + 
                    (1 + item_type | group_number),
                  data = region234_corr,
                  chains = 4,
                  cores = 4,
                  family = gaussian,
                  warmup = 200,
                  iter = 2000)

model234_c

# item_type is borderline significant
# figure out what portion of data falls within a given interval in the CI
model_fixef = fixef(model234_c)
tmp = round(colMeans(fixef(model234_c, summary = FALSE) > 0)*100,0)
model_fixef = cbind(model_fixef, "% > 0"= tmp)
model_fixef

# 94% - not significant

write_rds(model234_c, 'model-estimates/modelRegions234-correct-new.RDS')
model234_c_df <- as_tibble(fixef(model234_c), rownames = "parameter")
model234_c_df <- model234_c_df %>%
  mutate(region = '234',
         model = 'correct items')


#### REGION 1 ------------------------------------------------------------------

region1 <- region_data %>% 
  filter(region == '1')

model1 <- brm(residRT ~ item_type + 
                (1 + item_type | participant_ID) + 
                (1 + item_type | group_number),
              data = region1,
              chains = 4,
              cores = 4,
              family = gaussian,
              warmup = 200,
              iter = 2000)

model1
# not significant

write_rds(model1, 'model-estimates/modelRegion1-new.RDS')
model1_df <- as_tibble(fixef(model1), rownames = "parameter")
model1_df <- model1_df %>%
  mutate(region = '1',
         model = 'all items')


## only correct items
region1_corr <- region1 %>% 
  filter(correct == '1')

model1_c <- brm(residRT ~ item_type + 
                  (1 + item_type | participant_ID) + 
                  (1 + item_type | group_number),
                data = region1_corr,
                chains = 4,
                cores = 4,
                family = gaussian,
                warmup = 200,
                iter = 2000)

model1_c
# not significant

write_rds(model1_c, 'model-estimates/modelRegion1-correct-new.RDS')
model1_c_df <- as_tibble(fixef(model1_c), rownames = "parameter")
model1_c_df <- model1_c_df %>%
  mutate(region = '1',
         model = 'correct items')



#### REGION 2 ------------------------------------------------------------------

region2 <- region_data %>% 
  filter(region == '2')

model2 <- brm(residRT ~ item_type + 
                (1 + item_type | participant_ID) + 
                (1 + item_type | group_number),
              data = region2,
              chains = 4,
              cores = 4,
              family = gaussian,
              warmup = 200,
              iter = 2000)

model2
# not significant

write_rds(model2, 'model-estimates/modelRegion2-new.RDS')
model2_df <- as_tibble(fixef(model2), rownames = "parameter")
model2_df <- model2_df %>%
  mutate(region = '2',
         model = 'all items')



## only correct items
region2_corr <- region2 %>% 
  filter(correct == '1')

model2_c <- brm(residRT ~ item_type + 
                  (1 + item_type | participant_ID) + 
                  (1 + item_type | group_number),
                data = region2_corr,
                chains = 4,
                cores = 4,
                family = gaussian,
                warmup = 200,
                iter = 2000)

model2_c
# not significant

write_rds(model2_c, 'model-estimates/modelRegion2-correct-new.RDS')
model2_c_df <- as_tibble(fixef(model2_c), rownames = "parameter")
model2_c_df <- model2_c_df %>%
  mutate(region = '2',
         model = 'correct items')



#### REGION 3 ------------------------------------------------------------------

region3 <- region_data %>% 
  filter(region == '3')

model3 <- brm(residRT ~ item_type + 
                (1 + item_type | participant_ID) + 
                (1 + item_type | group_number),
              data = region3,
              chains = 4,
              cores = 4,
              family = gaussian,
              warmup = 200,
              iter = 2000)

model3
# significant

write_rds(model3, 'model-estimates/modelRegion3-new.RDS')
model3_df <- as_tibble(fixef(model3), rownames = "parameter")
model3_df <- model3_df %>%
  mutate(region = '3',
         model = 'all items')


## only correct items
region3_corr <- region3 %>% 
  filter(correct == '1')

model3_c <- brm(residRT ~ item_type + 
                  (1 + item_type | participant_ID) + 
                  (1 + item_type | group_number),
                data = region3_corr,
                chains = 4,
                cores = 4,
                family = gaussian,
                warmup = 200,
                iter = 2000)

model3_c
# significant

write_rds(model3_c, 'model-estimates/modelRegion3-correct-new.RDS')
model3_c_df <- as_tibble(fixef(model3_c), rownames = "parameter")
model3_c_df <- model3_c_df %>%
  mutate(region = '3',
         model = 'correct items')



#### REGION 4 ------------------------------------------------------------------

region4 <- region_data %>% 
  filter(region == '4')

model4 <- brm(residRT ~ item_type + 
                (1 + item_type | participant_ID) + 
                (1 + item_type | group_number),
              data = region4,
              chains = 4,
              cores = 4,
              family = gaussian,
              warmup = 200,
              iter = 2000)

model4
# not significant

write_rds(model4, 'model-estimates/modelRegion4-new.RDS')
model4_df <- as_tibble(fixef(model4), rownames = "parameter")
model4_df <- model4_df %>%
  mutate(region = '4',
         model = 'all items')


## only correct items
region4_corr <- region4 %>% 
  filter(correct == '1')

model4_c <- brm(residRT ~ item_type + 
                  (1 + item_type | participant_ID) + 
                  (1 + item_type | group_number),
                data = region4_corr,
                chains = 4,
                cores = 4,
                family = gaussian,
                warmup = 200,
                iter = 2000)

model4_c
# not significant

write_rds(model4_c, 'model-estimates/modelRegion4-correct-new.RDS')
model4_c_df <- as_tibble(fixef(model4_c), rownames = "parameter")
model4_c_df <- model4_c_df %>%
  mutate(region = '4',
         model = 'correct items')




#### REGION 5 ------------------------------------------------------------------

region5 <- region_data %>% 
  filter(region == '5')

model5 <- brm(residRT ~ item_type + 
                (1 + item_type | participant_ID) + 
                (1 + item_type | group_number),
              data = region5,
              chains = 4,
              cores = 4,
              family = gaussian,
              warmup = 200,
              iter = 2000)

model5
# not significant

write_rds(model5, 'model-estimates/modelRegion5-new.RDS')
model5_df <- as_tibble(fixef(model5), rownames = "parameter")
model5_df <- model5_df %>%
  mutate(region = '5',
         model = 'all items')


## only correct items
region5_corr <- region5 %>% 
  filter(correct == '1')

model5_c <- brm(residRT ~ item_type + 
                  (1 + item_type | participant_ID) + 
                  (1 + item_type | group_number),
                data = region5_corr,
                chains = 4,
                cores = 4,
                family = gaussian,
                warmup = 200,
                iter = 2000)

model5_c
# not significant

write_rds(model5_c, 'model-estimates/modelRegion5-correct-new.RDS')
model5_c_df <- as_tibble(fixef(model5_c), rownames = "parameter")
model5_c_df <- model5_c_df %>%
  mutate(region = '5',
         model = 'correct items')




#### REGION 6 ------------------------------------------------------------------

region6 <- region_data %>% 
  filter(region == '6')

model6 <- brm(residRT ~ item_type + 
                (1 + item_type | participant_ID) + 
                (1 + item_type | group_number),
              data = region6,
              chains = 4,
              cores = 4,
              family = gaussian,
              warmup = 200,
              iter = 2000)

model6
# not significant

write_rds(model6, 'model-estimates/modelRegion6-new.RDS')
model6_df <- as_tibble(fixef(model6), rownames = "parameter")
model6_df <- model6_df %>%
  mutate(region = '6',
         model = 'all items')


## only correct items
region6_corr <- region6 %>% 
  filter(correct == '1')

model6_c <- brm(residRT ~ item_type + 
                  (1 + item_type | participant_ID) + 
                  (1 + item_type | group_number),
                data = region6_corr,
                chains = 4,
                cores = 4,
                family = gaussian,
                warmup = 200,
                iter = 2000)

model6_c
# not significant

model_fixef = fixef(model6_c)
tmp = round(colMeans(fixef(model6_c, summary = FALSE) > 0)*100,0)
model_fixef = cbind(model_fixef, "% > 0"= tmp)
model_fixef

write_rds(model6_c, 'model-estimates/modelRegion6-correct-new.RDS')
model6_c_df <- as_tibble(fixef(model6_c), rownames = "parameter")
model6_c_df <- model6_c_df %>%
  mutate(region = '6',
         model = 'correct items')




#### REGION 7 ------------------------------------------------------------------

region7 <- region_data %>% 
  filter(region == '7')

model7 <- brm(residRT ~ item_type + 
                (1 + item_type | participant_ID) + 
                (1 + item_type | group_number),
              data = region7,
              chains = 4,
              cores = 4,
              family = gaussian,
              warmup = 200,
              iter = 2000)

model7
# not significant

write_rds(model7, 'model-estimates/modelRegion7-new.RDS')
model7_df <- as_tibble(fixef(model7), rownames = "parameter")
model7_df <- model7_df %>%
  mutate(region = '7',
         model = 'all items')


## only correct items
region7_corr <- region7 %>% 
  filter(correct == '1')

model7_c <- brm(residRT ~ item_type + 
                  (1 + item_type | participant_ID) + 
                  (1 + item_type | group_number),
                data = region7_corr,
                chains = 4,
                cores = 4,
                family = gaussian,
                warmup = 200,
                iter = 2000)

model7_c
# not significant

write_rds(model7_c, 'model-estimates/modelRegion7-correct-new.RDS')
model7_c_df <- as_tibble(fixef(model7_c), rownames = "parameter")
model7_c_df <- model7_c_df %>%
  mutate(region = '7',
         model = 'correct items')



#### write model estimates
estimates <- bind_rows(model234_df, model234_c_df, model1_df, model1_c_df,
                       model2_df, model2_c_df, model3_df, model3_c_df,
                       model4_df, model4_c_df, model5_df, model5_c_df,
                       model6_df, model6_c_df, model7_df, model7_c_df)

# write.csv(estimates, 'model-estimates-all.csv', row.names = FALSE)




#### POST-HOC: RC verb region w/o control for length ---------------------------
## to match similar analysis w eye tracking data

# prep data
region_data <- rt_data %>%
  filter(region != 'N/A') %>%
  select(participant_ID, item_word_number, word_number, item_number:item_type, length, region, correct, RT)

# can skip summing bc Region 2 (RC verb) is never more than one word region

# make correct binary again
region_data$correct[region_data$correct == 'TRUE'] <- 1
region_data$correct[region_data$correct == 'FALSE'] <- 0

# make things factors
region_data <- region_data %>%
  mutate(participant_ID = factor(participant_ID),
         item_type = factor(item_type, levels = c('ORC', 'SRC')))

# set sum coding
set.seed(4)
options(contrasts = c('contr.sum', 'contr.sum'))


#### modeling
region2 <- region_data %>% 
  filter(region == '2')

# model raw RTs w/o word length to see if similar effect as with eye tracking
model2_raw <- brm(RT ~ item_type + 
                    (1 + item_type | participant_ID) + 
                    (1 + item_type | group_number),
                  data = region2,
                  chains = 4,
                  cores = 4,
                  family = gaussian,
                  warmup = 1000,
                  iter = 5000)

model2_raw
# significant; ORCs read longer than SRCs

write_rds(model2_raw, 'model-estimates/modelRegion2-new-raw.RDS')
model2_raw_df <- as_tibble(fixef(model2_raw), rownames = "parameter")
model2_raw_df <- model2_raw_df %>%
  mutate(region = '2',
         model = 'all items-raw')



## only correct items
region2_corr <- region2 %>% 
  filter(correct == '1')

model2_raw_c <- brm(RT ~ item_type + 
                      (1 + item_type | participant_ID) + 
                      (1 + item_type | group_number),
                    data = region2_corr,
                    chains = 4,
                    cores = 4,
                    family = gaussian,
                    warmup = 1000,
                    iter = 5000)

model2_raw_c
# also significant; ORCs read longer than SRCs

write_rds(model2_raw_c, 'model-estimates/modelRegion2-correct-new-raw.RDS')
model2_raw_c_df <- as_tibble(fixef(model2_raw_c), rownames = "parameter")
model2_raw_c_df <- model2_raw_c_df %>%
  mutate(region = '2',
         model = 'correct items-raw')


estimates <- bind_rows(model2_raw_df, model2_raw_c_df)
# write.csv(estimates, 'model-estimates-Region2-nolen.csv', row.names = FALSE)
