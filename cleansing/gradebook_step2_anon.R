# Intro ---------------------------------------------------------------

# This script cleanses the Moodle quiz data. It creates 'pg_data_gb' used in 'pg_analysis.R'.

# Load the required packages.
library(tidyverse)
library(dplyr)
library(readr)

# Import post-class quiz ------------------------------------------------------------------
gradebook <- read_csv("data/cleansed/gradebook.csv", 
                      col_types = cols(X1 = col_skip()))

# Convert quiz names to factors for easier ordering on plots.
quiz <- gradebook %>% 
        filter(str_detect(assessment, "Post-class quiz")) %>% 
        filter(!str_detect(assessment, "Exam review"))

names(quiz)[4] <- "topic"

# Filter out the data for the first introductory course topic for which there was a quiz but no survey.
quiz <- quiz %>% 
        filter(topic != "Quiz: Topic 1 - Post-class quiz")

quiz <- quiz %>% 
        mutate(topic = fct_recode(topic,
                                       "Phonemic transcription" = "Quiz: Topic 2 - Post-class quiz",
                                       "Articulatory phonetics 1: The speech chain" = "Quiz: Topic 3 - Post-class quiz",
                                       "Articulatory phonetics 2: POA & MOA" = "Quiz: Topic 4 - Post-class quiz", 
                                       "Consonants 1: English" = "Quiz: Topic 5 - Post-class quiz",
                                       "Consonants 2: Polish" = "Quiz: Topic 6 - Post-class quiz",
                                       "Vowels 1: English" = "Quiz: Topic 7 - Post-class quiz",
                                       "Vowels 2: Polish" = "Quiz: Topic 8 - Post-class quiz",
                                       "Allophones 1: Obstruents" = "Quiz: Topic 9 - Post-class quiz",
                                       "Allophones 2: Other consonants" = "Quiz: Topic 10 - Post-class quiz",
                                       "Allophones 3: Vowels" = "Topic 11 - Post-class quiz",
                                       "Connected speech processes" = "Quiz: Topic 12 - Post-class quiz",
                                       "Word stress" = "Quiz: Topic 13 - Post-class quiz",
                                       "Rhythm" = "Quiz: Topic 14 - Post-class quiz",
                                       "Weak forms" = "Quiz: Topic 15 - Post-class quiz",
                                       "Intonation 1: Pitch, tones, tone unit" = "Quiz: Topic 16 - Post-class quiz",
                                       "Intonation 2: Functions of intonation" = "Quiz: Topic 17 - Post-class quiz",
                                       "The syllable" = "Topic 18 - Post-class quiz",
                                       "Phonotactics" = "Quiz: Topic 19 - Post-class quiz",
                                       "General British vs. General American" = "Topic 20 - Post-class quiz",
                                       "Acoustic phonetics" = "Quiz: Topic 21 - Post-class quiz"
                                       )
               )


quiz_order <- c(
        "Phonemic transcription",
        "Articulatory phonetics 1: The speech chain",
        "Articulatory phonetics 2: POA & MOA", 
        "Consonants 1: English",
        "Consonants 2: Polish",
        "Vowels 1: English",
        "Vowels 2: Polish",
        "Allophones 1: Obstruents",
        "Allophones 2: Other consonants",
        "Allophones 3: Vowels",
        "Connected speech processes",
        "Word stress",
        "Rhythm",
        "Weak forms",
        "Intonation 1: Pitch, tones, tone unit",
        "Intonation 2: Functions of intonation",
        "The syllable",
        "Phonotactics",
        "General British vs. General American",
        "Acoustic phonetics"
)

quiz$topic <- quiz$topic %>% 
        factor(levels = quiz_order)

# For the purpose of this analysis, we're treating the "not_submitted" label (and any other missing data) as if the learner didn't submit the quiz on time (i.e. didn't show up for it), which means a score of 0.
missing_quiz <- is.na(quiz$assess_score_first_perc)

quiz$assess_score_first_perc[missing_quiz] <- 0

summary(quiz$assess_score_first_perc)

# Import pre-class preparation --------------------------------------------
# Do the same as above but for pre-class lessons. These are first-attempt scores.
preclass <- gradebook %>% 
        filter(str_detect(assessment, "Lesson")) %>% 
        filter(!str_detect(assessment, "Class summary"))

names(preclass)[4] <- "topic"
names(preclass)[6] <- "preclass_score_avg"

preclass <- preclass %>% 
        mutate(topic = fct_recode(topic,
                                       "Phonemic transcription" = "Lesson: Topic 2 - Pre-class preparation",
                                       "Articulatory phonetics 1: The speech chain" = "Lesson: Topic 3 - Pre-class preparation",
                                       "Articulatory phonetics 2: POA & MOA" = "Lesson: Topic 4 - Pre-class preparation", 
                                       "Consonants 1: English" = "Lesson: Topic 5 - Pre-class preparation",
                                       "Consonants 2: Polish" = "Lesson: Topic 6 - Pre-class preparation",
                                       "Vowels 1: English" = "Lesson: Topic 7 - Pre-class preparation",
                                       "Vowels 2: Polish" = "Lesson: Topic 8 - Pre-class preparation",
                                       "Allophones 1: Obstruents" = "Lesson: Topic 9 - Pre-class preparation",
                                       "Allophones 2: Other consonants" = "Lesson: Topic 10 - Pre-class preparation",
                                       "Allophones 3: Vowels" = "Lesson: Topic 11 - Pre-class preparation",
                                       "Connected speech processes" = "Lesson: Topic 12 - Pre-class preparation",
                                       "Word stress" = "Lesson: Topic 13 - Pre-class preparation",
                                       "Rhythm" = "Lesson: Topic 14 - Pre-class preparation",
                                       "Weak forms" = "Lesson: Topic 15 - Pre-class preparation",
                                       "Intonation 1: Pitch, tones, tone unit" = "Lesson: Topic 16 - Pre-class preparation",
                                       "Intonation 2: Functions of intonation" = "Lesson: Topic 17 - Pre-class preparation",
                                       "The syllable" = "Lesson: Topic 18 - Pre-class preparation",
                                       "Phonotactics" = "Lesson: Topic 19 - Pre-class preparation",
                                       "General British vs. General American" = "Lesson: Topic 20 - Pre-class preparation",
                                       "Acoustic phonetics" = "Lesson: Topic 21 - Pre-class preparation"
        )
        )

preclass_order <- c(
        "Phonemic transcription",
        "Articulatory phonetics 1: The speech chain",
        "Articulatory phonetics 2: POA & MOA", 
        "Consonants 1: English",
        "Consonants 2: Polish",
        "Vowels 1: English",
        "Vowels 2: Polish",
        "Allophones 1: Obstruents",
        "Allophones 2: Other consonants",
        "Allophones 3: Vowels",
        "Connected speech processes",
        "Word stress",
        "Rhythm",
        "Weak forms",
        "Intonation 1: Pitch, tones, tone unit",
        "Intonation 2: Functions of intonation",
        "The syllable",
        "Phonotactics",
        "General British vs. General American",
        "Acoustic phonetics"
)

preclass$topic <- preclass$topic %>% 
        factor(levels = preclass_order)

# For the purpose of this analysis, we treat no attempt on pre-class preparation as a score of zero.
missing_preclass <- is.na(preclass$preclass_score_avg)

preclass$preclass_score_avg[missing_preclass] <- 0

summary(preclass$preclass_score_avg)

# Clean up.
preclass <- preclass %>%
        select(user_id, learner_id, topic, preclass_score_avg)

# Join quiz and pre-class.
pg_data_gb <- left_join(preclass, quiz)


# Select sample for paper -----------------------------------------------------------
# Show only rows for only those learners who completed the course evaluation survey.

completed_ceval <- read_csv("data/cleansed/who_completed_eval.csv")

completed_ceval <- completed_ceval$x

pg_data_gb <- pg_data_gb %>%
        filter(learner_id %in% completed_ceval)

pg_data_gb %>% group_by(learner_id) %>% count()

summary(pg_data_gb)