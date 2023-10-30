### load packages

library("tidyverse")
library("qualtRics")
library("plyr")

### load data from Pilot A

data_pilot_a <- read_survey("/Users/bendix/Desktop/krauss2003_rescue/data/pilotA.csv")

### tidy data

tidy_data_pilot_a = data_pilot_a |>
  select(Finished, "Duration (in seconds)", Q7, Q9, Q10, Q20, Q14, Q15, Q43) |> 
  unite(switch, c(Q10, Q20), na.rm = T) |>
  mutate(switch = case_when(switch == 'switch to door 2' | switch == 'switch to the last remaining door' ~ TRUE,
                         TRUE ~ FALSE)) |> 
  dplyr::rename(wins_cases_if_stays = Q7, wins_cases_if_switches = Q9, already_familiar = Q14, already_knew_answer = Q15, own_answer = Q43) |> 
  mutate(already_familiar = case_when(already_familiar == 'I was familiar with this game' ~ TRUE,
                                      TRUE ~ FALSE)) |> 
  mutate(already_knew_answer = case_when(already_knew_answer == 'I already knew what the correct answer should be' ~ TRUE,
                                         TRUE ~ FALSE)) |> 
  mutate(own_answer = case_when(own_answer == 'I arrived at the answer on my own' ~ TRUE,
                                TRUE ~ FALSE)) |> 
  mutate(control = case_when(is.na(wins_cases_if_stays) ~ TRUE,
                             TRUE ~ FALSE))

view(tidy_data_pilot_a)

# keep only rows corresponding to participants who finished the survey, weren't familiar with the scenario, and completed the questionnaire on their own

relevant_tidy_data_pilot_a = tidy_data_pilot_a |>
  filter(Finished == T) |> 
  filter(already_familiar == F) |> 
  filter(already_knew_answer == F) |> 
  filter(own_answer == T) |> 
  select(control, switch)

view(relevant_tidy_data_pilot_a)

# let's pretend we've coded the justifications (we'll generate values randomly for pilot A)

relevant_tidy_data_pilot_a = relevant_tidy_data_pilot_a |> 
  mutate(correct_justification = case_when(switch == TRUE ~ sample(c(TRUE, FALSE), size = 1, replace = TRUE),
         TRUE ~ FALSE))

view(relevant_tidy_data_pilot_a)

### Analysis

# generate contingency table that displays how many participants in the control and experimental condition provided a correct justification

table <- with(
  relevant_tidy_data_pilot_a,
  table(correct_justification, control))

table

proportions_pilot_a = data.frame(Condition=c("Control","Experimental"),
                                 Proportion=c((table[2, 2]/
                                                 (table[2, 2] + table[1, 2])), # the first proportion is that of correct justifications given in control condition
                                              (table[2,1]/
                                                 (table[2, 1] + table[1, 1])))) # the second proportion is that of correct justifications given in experimental condition

proportions_pilot_a

# perform Cohen's h test

library("pwr")

h <- ES.h(
  proportions_pilot_a$Proportion[1], # the first proportion is that of correct justifications given in control condition
  proportions_pilot_a$Proportion[2] # the second proportion is that of correct justifications given in experimental condition
)
h

# perform Fisher's exact test

fisher_exact_test <- fisher.test(as.matrix(table))
fisher_exact_test

# Plots of results

library(ggplot2)

# Plot from pilot A

plot_pilot_a <- ggplot(data=proportions_pilot_a, aes(x=Condition, y=(Proportion)*100)) +
  geom_bar(stat="identity") +
  scale_x_discrete(breaks=c("Control", "Experimental"),
                   labels=c("Control", "Experimental")) + ylim(0, 100) +
  labs(title = "Pilot A results", x = "Condition", y = "Percentage correct (%)")

plot_pilot_a

# Plot from the original study

proportions_original_study = data.frame(Condition=c("Control","Experimental"),
                       Proportion=c(.3, .38))

plot_original_study <- ggplot(data=proportions_original_study, aes(x=Condition, y=(Proportion)*100)) +
  geom_bar(stat="identity") +
  scale_x_discrete(breaks=c("Control","Experimental"),
                   labels=c("Control", "Experimental")) + ylim(0, 100) +
  labs(title = "Original study results", x = "Condition", y = "Percentage correct (%)")

plot_original_study
