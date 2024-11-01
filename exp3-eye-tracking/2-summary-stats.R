## descriptive statistics and visualizations for eye tracking data


library(tidyverse)
library(openxlsx)
library(plotrix)

#### read in data --------------------------------------------------------------
data <- read_csv("data/exp3-eye-tracking-clean.csv")

trials <- data %>% 
  filter(item_type == 'SRC' | item_type == 'ORC') %>% 
  group_by(session_id, trial_index, item_type, item_number, group_number, correct) %>% 
  summarise()



#### accuracy rates for SRCs vs ORCs -------------------------------------------
overall <- trials %>% 
  group_by(item_type) %>% 
  summarise(accuracy = (mean(correct) * 100))

# get number of correct SRCs
sum(trials$correct[trials$item_type == 'SRC'] == 1)

# get number of correct ORCs
sum(trials$correct[trials$item_type == 'ORC'] == 1)

# get number of incorrect ORCs
sum(trials$correct[trials$item_type == 'ORC'] == 0)

# get number of incorrect SRCs
sum(trials$correct[trials$item_type == 'SRC'] == 0)

# accuracy by participant
accuracy <- trials %>% 
  group_by(session_id, item_type) %>% 
  summarise(accuracy = (mean(correct) * 100))

# accuracy by participant for ORCs
accuracy_orcs <- accuracy %>% 
  filter(item_type == 'ORC')

# accuracy by participant for SRCs
accuracy_srcs <- accuracy %>% 
  filter(item_type == 'SRC')

# mins and maxes
min(accuracy_orcs$accuracy)
max(accuracy_orcs$accuracy)
sort(accuracy_orcs$accuracy)

# number of participants who got less than 50% correct on ORCs
accuracy_less50 <- accuracy_orcs %>% 
  filter(accuracy < 50)
length(unique(accuracy_less50$session_id))

min(accuracy_srcs$accuracy)
max(accuracy_srcs$accuracy)
sort(accuracy_srcs$accuracy)

#### plot accuracy by participant and item type
accuracy_plot <- ggplot(accuracy, aes(item_type, accuracy, fill = item_type)) +
  geom_boxplot() +
  geom_hline(yintercept = 84.4, linetype = "dashed", size = 1) + # add line to show overall accuracy mean
  theme_classic() +
  scale_fill_brewer(palette = "Blues") +
  xlab("") +
  ylab("Accuracy by participant") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme(axis.text.x = element_text(size = 10, color = "black"),
        axis.title.y = element_text(size = 11, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"),
        legend.position = "none")
accuracy_plot




#### fixations on the RP clitic ------------------------------------------------
# filter down to smaller dataset
clitics <- data %>% 
  filter(region == 'rel-CLITIC')

# summarize fixation metrics by character and correctness condition
clitics_summary <- clitics %>% 
  group_by(arb, correct) %>% 
  summarise(mean_ff = mean(first_fixation),
            mean_fp = mean(first_pass),
            mean_gp = mean(go_past),
            mean_td = mean(total_duration),
            p_fps = mean(first_pass_skip),
            num_items = length(trial_index))


# get number of overall skips
clitics_skipped <- clitics %>% 
  filter(total_duration == '0') # 0 means was fully skipped during trial

clitics_summary2 <- clitics_skipped %>%
  group_by(arb, correct) %>% 
  summarise(num_skipped = length(total_duration))

# combine
clitics_summary <- left_join(clitics_summary, clitics_summary2)

# calculate probability of overall skip
clitics_summary <- clitics_summary %>% 
  mutate(p_s = num_skipped / num_items)


# summarize first pass regressions (have to do separately to exclude .)
clitics_fpr <- clitics %>% 
  filter(first_pass_regression != '.') %>% 
  mutate(first_pass_regression = as.numeric(first_pass_regression)) # was char thanks to .

clitics_summary3 <- clitics_fpr %>% 
  group_by(arb, correct) %>% 
  summarise(p_fpr = mean(first_pass_regression))


# combine
clitics_summary <- left_join(clitics_summary, clitics_summary3)

# clitics_summary <- clitics_summary %>% 
#   mutate(item_type = 'ORC')

# arrange columns
clitics_summary <- clitics_summary %>% 
  select(arb:mean_td, p_fpr, p_fps, p_s, num_items, num_skipped)

# pivot longer
clitics_summary <- clitics_summary %>% 
  pivot_longer(mean_ff:num_skipped, names_to = 'metric', values_to = 'value')


#### write
# write.xlsx(clitics_summary, "summary-stats/RP-summary-correctness-24Apr2024.xlsx")


#### fixations on filler item clitics ------------------------------------------
# filter down to smaller dataset
clitics_filler <- data %>% 
  filter(region == 'filler-CLITIC')

clitics_filler_skipped <- clitics_filler %>% 
  filter(total_duration == '0') # 0 means was fully skipped during trial


# summarize fixation metrics & skips
clitics_filler_summary <- clitics_filler %>% 
  group_by(arb) %>% 
  summarise(mean_ff = mean(first_fixation),
            se_ff = std.error(first_fixation),
            mean_fp = mean(first_pass),
            se_fp = std.error(first_pass),
            mean_gp = mean(go_past),
            se_gp = std.error(go_past),
            mean_td = mean(total_duration),
            se_td = std.error(total_duration),
            p_fps = mean(first_pass_skip))


# summarize overall skips
clitics_filler_summary2 <- clitics_filler_skipped %>%
  group_by(arb) %>% 
  summarise(p_s = length(clitics_filler_skipped$total_duration)/length(clitics_filler$total_duration))


# summarize first pass regressions (have to do separately to exclude .)
clitics_filler_fpr <- clitics_filler %>% 
  filter(first_pass_regression != '.') %>% 
  mutate(first_pass_regression = as.numeric(first_pass_regression)) # was char thanks to .

clitics_filler_summary3 <- clitics_filler_fpr %>% 
  group_by(arb) %>% 
  summarise(p_fpr = mean(first_pass_regression))


# combine
clitics_filler_summary <- left_join(clitics_filler_summary, clitics_filler_summary2)
clitics_filler_summary <- left_join(clitics_filler_summary, clitics_filler_summary3)

clitics_filler_summary <- clitics_filler_summary %>% 
  mutate(item_type = 'filler')



#### make giant df for ORC clitics and filler clitics
clitics_df <- bind_rows(clitics_summary, clitics_filler_summary)



#### write
# write.xlsx(clitics_df, "summary-stats/clitics-summary-22Nov2023.xlsx")



#### correctness rates over time -----------------------------------------------

# get correctness by participant and item
correct <- data %>% 
  filter(item_type == 'ORC' | item_type == 'SRC') %>% 
  group_by(session_id, trial_index, item_number, item_type, correct) %>% 
  summarise()

# overall correctness over time
time <- correct %>% 
  group_by(trial_index) %>% 
  summarise(m = mean(correct))

# write.csv(time, 'summary-stats/correctness-over-time.csv', row.names = FALSE)


# correctness over time by clause type
time_clause <- correct %>% 
  group_by(item_type, trial_index) %>% 
  summarise(m = mean(correct))

# write.csv(time_clause, 'summary-stats/correctness-over-time-byclause.csv', row.names = FALSE)



# get correctness for the first three ORCs each participant sees
correct_orcs <- correct %>% 
  filter(item_type == 'ORC') %>% 
  group_by(session_id, trial_index) %>% 
  summarise(correct = correct) %>% 
  filter(row_number() == '1' | row_number() == '2' | row_number() == '3')

# make column for first through third ORC trial
correct_orcs <- correct_orcs %>% 
  mutate(ORC_trial_index = rep(1:3))

# summarise
correct_orcs_summary <- correct_orcs %>% 
  group_by(ORC_trial_index) %>% 
  summarise(m = mean(correct))

# write.csv(correct_orcs_summary, 'summary-stats/correctness-ORC-trial-index.csv', row.names = FALSE)



#### first half second half analysis for ORCs ----------------------------------
correct <- correct %>% 
  mutate(half = rep('second'))

correct$half[correct$trial_index <= 50] <- 'first'

# get count of items
counts <- correct %>% 
  group_by(half, item_type, correct) %>% 
  summarise(count = n())

halves <- correct %>% 
  filter(item_type == 'ORC') %>% 
  group_by(half) %>% 
  summarise(m = mean(correct))


## by participant
halves_p <- correct %>% 
  filter(item_type == 'ORC') %>% 
  group_by(half, session_id) %>% 
  summarise(m = mean(correct))

# pivot wider
halves_p <- halves_p %>% 
  pivot_wider(names_from = half, values_from = m)

# left join with averages
ORC_average <- correct %>% 
  filter(item_type == 'ORC') %>% 
  group_by(session_id) %>% 
  summarise(overall = mean(correct))

halves_p <- left_join(halves_p, ORC_average) %>% 
  arrange(overall)

# write.csv(halves_p, 'summary-stats/correctness-ORC-halves.csv', row.names = FALSE)
