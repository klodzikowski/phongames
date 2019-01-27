# Intro ---------------------------------------------------------------

# This is the main script that you need to run in order to produce the plots and statistical tests.

# Load the required packages.
library(tidyverse)
library(dplyr)
library(readr)
library(Hmisc)
library(jtools) # For creating interaction plots.
library(car) # For running Levene's test.


# Import and cleanse ------------------------------------------------------------------

# Import the raw survey resposes.
surv_resp <- read_csv("data/cleansed/survey_responses.csv", 
                      col_types = cols(X1 = col_skip()))

# Import user info, so that we can identify the right sample for analysis.
users_for_survey <- read_csv("data/raw/mdl_user.csv", 
                             col_types = cols(X1 = col_skip()))

# Join the two datasets.
surv_resp <- left_join(surv_resp, users_for_survey)


# Rename the existing factor levels. What we have now as survey title is "Topic X", "Topic Y", etc. We'd like to know not only the topic number but also what was discussed, so let's add that manually.

surv_resp <- surv_resp %>% 
        mutate(surv_title = fct_recode(surv_title,
                   "Phonemic transcription" = "Topic 2 - Survey",
                   "Articulatory phonetics 1: The speech chain" = "Topic 3 - Survey",
                   "Articulatory phonetics 2: POA & MOA" = "Topic 4 - Survey", 
                   "Consonants 1: English" = "Topic 5 - Survey",
                   "Consonants 2: Polish" = "Topic 6 - Survey",
                   "Vowels 1: English" = "Topic 7 - Survey",
                   "Vowels 2: Polish" = "Topic 8 - Survey",
                   "Allophones 1: Obstruents" = "Topic 9 - Survey",
                   "Allophones 2: Other consonants" = "Topic 10 - Survey",
                   "Allophones 3: Vowels" = "Topic 11 - Survey",
                   "Connected speech processes" = "Topic 12 - Survey",
                   "Word stress" = "Topic 13 - Survey",
                   "Rhythm" = "Topic 14 - Survey",
                   "Weak forms" = "Topic 15 - Survey",
                   "Intonation 1: Pitch, tones, tone unit" = "Topic 16 - Survey",
                   "Intonation 2: Functions of intonation" = "Topic 17 - Survey",
                   "The syllable" = "Topic 18 - Survey",
                   "Phonotactics" = "Topic 19 - Survey",
                   "General British vs. General American" = "Topic 20 - Survey",
                   "Acoustic phonetics" = "Topic 21 - Survey",
                   "Course evaluation" = "Course evaluation survey"
                   )
               )


# Convert topic names to factors for easier ordering on plots.
topic_order <- c(
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
        "Acoustic phonetics",
        "Course evaluation" 
        )

surv_resp$surv_title <- surv_resp$surv_title %>% factor(levels = topic_order)

# Do a similar factor conversion for some of the responses.
surv_resp <- surv_resp %>% 
        mutate(surv_scale_row_text = fct_recode(surv_scale_row_text,
        "Online pre-class prep: Videos" = "Pre-class Moodle lessons: Videos",
        "Online pre-class prep: Activities" = "Pre-class Moodle lessons: Activities",
        "Online pre-class prep: Readings" = "Pre-class Moodle lessons: Supplementary readings",
        "In-class teacher's support" = "Real-life classes: Teacher's explanations",
        "In-class worksheets: Cheat sheet" = "Real-life classes: Worksheet cheat sheet",
        "In-class worksheets: Exercises" = "Real-life classes: Worksheet exercises",
        "In-class board games" = "Real-life classes: Board games",
        "Post-class quizzes" = "Post-class quizzes",
        "Post-class office hours" = "Office hours"
        )
        )


# Fig 4 Perception overall ----------------------------------
# Take the "Course evaluation survey".
glimpse(surv_resp)
summary(surv_resp)

surv_resp_coeval <- surv_resp %>%
        filter(surv_title == "Course evaluation")

# The following line shows that 29 learners completed the course evaluation survey. There were 4 learners who completed the survey but were excluded from the main study (because they did not participate in all recording sessions). These are the learners that can be filtered out. Their learner_ids are: P002, P020, P080, P081.
who_completed_eval <- surv_resp_coeval %>% 
        count(learner_id)

who_completed_eval_list <- who_completed_eval$learner_id

write.csv(who_completed_eval_list, file = "data/cleansed/who_completed_eval.csv")

# Select responses to question about the perceived usefulness of course tools. Select only the required columns. 
tools_overall_raw <- surv_resp_coeval %>%
        filter(surv_q_title == "Eval-3-ToolsOverall") %>% 
        select(1:3, 5:9, 14, 13, 15, 17) 

# In the survey, the question about worksheets was split into two to distinguish between the practice part of the worksheet (usually 3-5 pages of exercises) and the "cheat sheet" (a one page overview of the main 'rules' for a given topis, e.g. stress rules). Both were rated similarly highly, so we'll merge them here to simplify the plot. 

tools_overall <- tools_overall_raw %>% 
        select(1:3, 9:10) %>% 
        spread(surv_scale_row_text, surv_scale_cell_val) %>% 
        mutate(`In-class worksheets` = (`In-class worksheets: Cheat sheet` + `In-class worksheets: Exercises`) / 2) %>% 
        select(-c(11:12)) %>% 
        gather(surv_scale_row_text, surv_scale_cell_val, -c(1:3))

# Reorder the tools names, so when we plot them later, the horizontal axis lists the tools ordered from those typically **used first** in the weekly course cycle on the left to those typically **used last** on the right.

tool_order <- c(
        "Online pre-class prep: Videos",
        "Online pre-class prep: Activities",
        "Online pre-class prep: Readings",
        "In-class teacher's support",
        "In-class worksheets",
        "In-class board games",
        "Post-class quizzes",
        "Post-class office hours"
)

tools_overall$surv_scale_row_text <- tools_overall$surv_scale_row_text %>% factor(levels = tool_order)

# Check the distribution of the data. Looks like the scores are highly skewed to the left. 
# Note: Throughout this analysis, data collected through a Likert-scale questionnaire is treated like interval data. We made this decision following Norman' 2010 paper "Likert scales, levels of measurement and the “laws” of statistics". The values at 4.5 are due to the merger of two questions about worksheets (see the comment above).

summary(tools_overall)

ggplot(data = tools_overall, 
       mapping = aes(surv_scale_cell_val)) + 
        geom_histogram()

# Calculate the avg usefulness of each tool, and then visualise it. The distribution of learner ratings is not normal, so for stat_summary let's go with "smean.cl.boot".

usefulness <- tools_overall %>% 
        group_by(surv_scale_row_text) %>% 
        do(data.frame(rbind(smean.cl.boot(.$surv_scale_cell_val, conf.int = .95, B = 10000))))

ggplot(data = usefulness,
       mapping = aes(
               x = surv_scale_row_text, 
               y = Mean,
               color = surv_scale_row_text),
       fig.width = 6, 
       fig.asp = 0.618
       ) +
        geom_point(
                mapping = aes(
                        size = 1, 
                        stroke = 0)
                ) +
        geom_errorbar(
                mapping = aes(
                        ymin = Lower, 
                        ymax = Upper,
                        width = 0.3
                        )
                ) +
        theme(legend.position = "none") +
        coord_cartesian(ylim = c(1,5)) +
        scale_y_continuous(labels = c("1" = "(1) Not useful at all", "2" = "(2) Rather not useful", "3" = "(3) Neutral", "4" = "(4) Somewhat useful", "5" = "(5) Very useful")) +
        theme(
                axis.text.x = element_text(size = 11, angle = 45, vjust = 1, hjust = 1),
                axis.text.y = element_text(size = 11, angle = 0),
                axis.title.x = element_text(face = "bold", size = 13),
                axis.title.y = element_text(face = "bold", size = 13)) +
        labs(
                x = "Teaching aids used in the course",
                y = "Learners' perceived usefulness\n"
        )

ggsave("figs/fig4_surv_usefulness.pdf", height = 6, width = 7)
ggsave("figs/fig4_surv_usefulness.png", height = 6, width = 7)


# Fig 1 In-class engagement -------------------------
# Select the required data for all surveys apart from the "Course evaluation survey". Select responses to question about engagement.

engagement <- surv_resp %>% 
        filter(str_detect(surv_q_title, "Engagement")) %>% 
        filter(!is.na(surv_scale_cell_val))

# Select only the required columns. 
engagement <- engagement %>% 
        select(1:3, 5:9, 13, 15, 17)  

# Filter out those learners who didn't complete the course evaluation survey.
engagement <- engagement %>% 
       filter(learner_id %in% who_completed_eval_list)

engagement %>% count(learner_id)

# Check the distribution of the data. Looks like the scores are slightly skewed to the left.
summary(engagement)

ggplot(data = engagement, 
       mapping = aes(surv_scale_cell_val)) + 
        geom_histogram()


# Note that in a couple of cases a learner said they were absent and therefore their answers to questions regarding their class preparation / engagement were not representative. These have been corrected manually in the Moodle database itself (changed to NA).


# Calculate the avg engagement for each class, and then visualise it. The distribution of learner ratings is not normal, so for stat_summary let's go with "smean.cl.boot". 

engagement_avg <- engagement %>% 
        group_by(surv_title) %>%
        do(data.frame(rbind(smean.cl.boot(.$surv_scale_cell_val, conf.int = .95, B = 10000))))

names(engagement_avg) <- c("surv_title", "engage_avg", "engage_lower", "engage_upper")

# Add two metadata columns: one that states if the topic was covered in the 1st or 2nd semester, and another that states if the topic was accompanied by a board game.
engagement_avg$semester <- c("1st", "1st", "1st", "1st", "1st", "1st", "1st", "1st", "1st", "1st", "1st", "2nd", "2nd", "2nd", "2nd", "2nd", "2nd", "2nd", "2nd", "2nd")

engagement_avg$game <- c("no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "yes", "no", "yes", "no", "no", "no", "yes", "no", "no")

# Calculate the avg. engagement across the entire semester, 1st semester, and 2nd semester.
mean(engagement_avg$engage_avg) # Entire semester.

mean(engagement_avg$engage_avg[1:11]) # Only 1st semester.

mean(engagement_avg$engage_avg[12:20]) # Only 2nd semester.

# Calculate score spread.
eng_over35 <- engagement_avg %>% filter(engage_avg >= 3 & engage_avg < 4) %>% nrow()
eng_over35 / 20

# Visualise the avg engagement for each class.
ggplot(data = engagement_avg,
       mapping = aes(
               x = surv_title, 
               y = engage_avg,
               shape = game
               ),
       fig.width = 6, 
       fig.asp = 0.618
       ) +
        # scale_colour_brewer(type = "qual", palette = 6, direction = -1) +
        geom_point(
                mapping = aes(
                        size = 1, 
                        stroke = 0
                        )
        ) +
        geom_errorbar(
                mapping = aes(
                        ymin = engage_lower, 
                        ymax = engage_upper,
                        width = 0.3
                )
        ) +
        theme(legend.position = "none") +
        coord_cartesian(ylim = c(1,5)) +
        scale_y_continuous(labels = c("1" = "(1) Very disengaged", "2" = "(2) Somewhat disengaged", "3" = "(3) Neutral", "4" = "(4) Somewhat engaged", "5" = "(5) Very engaged")) +
        labs(
                x = "Course topics",
                y = "Learners' self-reported class engagement"
                # caption = "Whiskers show 95% bootstrap confidence intervals"
        ) +
        theme(
                axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
                axis.text.y = element_text(size = 9, angle = 0),
                axis.title.x = element_text(face = "bold", size = 12),
                axis.title.y = element_text(face = "bold", size = 12))

ggsave("figs/fig1_class_engagement.pdf", height = 5.8, width = 7)
ggsave("figs/fig1_class_engagement.png", height = 5.8, width = 7)


# Which game was the best? -------------------------------------------------------
# The findings from this section are that learners rated the usefulness of each game similarly. The following plot will not be showed in the paper. 

# Select responses to question about the perceived usefulness of each game. 
pg_useful <- surv_resp_coeval %>%
        filter(surv_q_title == "Eval-4-Game-Useful") %>% 
        select(1:3, 6, 7, 14, 13) 

# Here's a quick plot visualising the results.
summary(pg_useful)

ggplot(data = pg_useful, 
       mapping = aes(surv_scale_cell_val)) + 
        geom_histogram()

# Calculate the avg usefulness of each tool, and then visualise it. The distribution of learner ratings is not normal, so for stat_summary let's go with "smean.cl.boot".

pg_useful_boot <- pg_useful %>% 
        group_by(surv_scale_row_text) %>% 
        do(data.frame(rbind(smean.cl.boot(.$surv_scale_cell_val, conf.int = .95, B = 10000))))

# For additional reporting, if needed. Note the result obtained from "pg_useful_boot" is ~4.3, which is almost the same as the ~4.2 obtained from the "usefulness" table (where learners rated the usefulness of board games compared to other tools (as opposed to comparing just each of the three games).
mean(pg_useful_boot$Mean)

# Visualise the results.
ggplot(data = pg_useful_boot,
       mapping = aes(
               x = surv_scale_row_text, 
               y = Mean,
               color = surv_scale_row_text),
       fig.width = 6, 
       fig.asp = 0.618
) +
        geom_point(
                mapping = aes(
                        size = 1, 
                        stroke = 0)
        ) +
        geom_errorbar(
                mapping = aes(
                        ymin = Lower, 
                        ymax = Upper,
                        width = 0.3
                )
        ) +
        theme(legend.position = "none") +
        coord_cartesian(ylim = c(1,5)) +
        scale_y_continuous(labels = c("1" = "(1) Not useful at all", "2" = "(2) Rather not useful", "3" = "(3) Neutral", "4" = "(4) Somewhat useful", "5" = "(5) Very useful")) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
        labs(
                x = "Board game",
                y = "Learners' perceived usefulness of a game",
                caption = "Whiskers show 95% bootstrap confidence intervals"
        )

# ggsave("figs/eval_which_game.pdf", width = 8)
# ggsave("figs/eval_which_game.png", width = 8)


# Replay ------------------------------------------------------------------
# Select responses to question about game replay rate. 
pg_replay <- surv_resp_coeval %>%
        filter(surv_q_title == "Eval-4b-Game-Replay") %>% 
        select(1:3, 6, 7, 14, 13) %>% 
        filter(!is.na(surv_scale_row_text))

# We'd like to calculate the percentage of learners who answered "Yes" to the question "Do you think Topic X will help you in mastering English pronunciation?". In this dataset, "1" means "Yes". However, "No" is coded as "2", not "0". Let's change all "2" to "0", so that we can calculate the percentage of "Yes" easier.
pg_replay$surv_scale_cell_val <- gsub(2, 0, pg_replay$surv_scale_cell_val) 

# The operation above changed the col type to <char>, so let's bring <int> back.
pg_replay$surv_scale_cell_val <- as.integer(pg_replay$surv_scale_cell_val)

# The following table shows the replay rate for each game.
pg_replay_per_game <- pg_replay %>% 
        group_by(surv_scale_row_text) %>% 
        summarise(percent_yes = mean(surv_scale_cell_val))

# The following table shows the percentage of learners who replayed a given game.
pg_replay_per_learner <- pg_replay %>% 
        group_by(learner_id) %>% 
        summarise(percent_yes = mean(surv_scale_cell_val))

# Replay before exam
# Select responses to question about game replay rate. 
pg_replay_ex <- surv_resp_coeval %>%
        filter(surv_q_title == "Eval-8-Exam-Prep") %>% 
        select(1:3, 6, 7, 16) 

pg_replay_ex_filtered <- pg_replay_ex %>% 
        filter(str_detect(surv_q_text_answer, "game"))


# Fig 5 Game vs Worksheet? -------------------------------------------------------
# Select responses to question about the perceived usefulness of each game. 
pg_vs_work <- surv_resp_coeval %>%
        filter(surv_q_title == "Eval-6-Game-Worksheet") %>% 
        select(1:3, 6, 7, 14, 13) 

pg_vs_work$surv_scale_row_text <- as.factor(pg_vs_work$surv_scale_row_text)

# Here's a quick plot visualising the results.
summary(pg_vs_work)

ggplot(data = pg_vs_work, 
       mapping = aes(surv_scale_row_text)) + 
        stat_count()

pg_vs_work %>% 
        count(surv_scale_row_text) %>% 
        mutate(perc = n / nrow(pg_vs_work)) -> pg_vs_work_perc

# Visualise the results.
ggplot(pg_vs_work_perc,
       aes(surv_scale_row_text, perc),
       fig.width = 6, fig.asp = 0.618) +
        scale_y_continuous(labels = scales::percent) +
        coord_cartesian(ylim = c(0, 1)) +
        geom_col() +
        #theme(legend.position = "none") +
        #scale_fill_brewer(type = "qual", palette = 6, direction = -1) +
        labs(
                x = "\nIf you could choose between spending 30 minutes on doing extra \nworksheet exercises or playing board games, what would you prefer to do?",
                y = "Percentage of learners\n"
        ) +
        theme(
                axis.title.x = element_text(face = "bold", size = 14),
                axis.title.y = element_text(face = "bold", size = 14),
                axis.text.x = element_text(size = 12, angle = 0, hjust = .5, vjust = .5),
                axis.text.y = element_text(size = 12, angle = 0, hjust = .5, vjust = .5)
        )

ggsave("figs/fig5_pg_vs_work.pdf")
ggsave("figs/fig5_pg_vs_work.png")


# Fig 2 Topic usefulness -------------------------------------------------

# In this section, we'll visualise the self-reported engagement for each class against the self-reported usefulness of a given topic.

# Select the right surveys and learners.
engagement_help <- surv_resp %>% 
        filter(str_detect(surv_q_title, "Pron usefulness")) %>%
        filter(learner_id %in% who_completed_eval_list)

# We'd like to calculate the percentage of learners who answered "Yes" to the question "Do you think Topic X will help you in mastering English pronunciation?". In this dataset, "1" means "Yes". However, "No" is coded as "2", not "0". Let's change all "2" to "0", so that we can calculate the percentage of "Yes" easier.
engagement_help$surv_scale_cell_val <- gsub(2, 0, engagement_help$surv_scale_cell_val) 

# The operation above changed the col type to <char>, so let's bring <int> back.
engagement_help$surv_scale_cell_val <- as.integer(engagement_help$surv_scale_cell_val)

# The following table shows the percentage of learners who thought that a given topic would help them in mastering their English pronunciation. 0 = 0%, and 1 = 100%.        
engagement_help_perc <- engagement_help %>% 
        group_by(surv_title) %>% 
        filter(!is.na(surv_scale_cell_val)) %>% 
        summarise(percent_yes = mean(surv_scale_cell_val))

# Calculate the avg. usefulness across the entire semester, 1st semester, and 2nd semester.
mean(engagement_help_perc$percent_yes) # Entire year

mean(engagement_help_perc$percent_yes[1:11]) # Only 1st semester.

mean(engagement_help_perc$percent_yes[12:20]) # Only 2nd semester.

# Add two metadata columns: one that states if the topic was covered in the 1st or 2nd semester, and another that states if the topic was accompanied by a board game.
engagement_help_perc$semester <- c("1st", "1st", "1st", "1st", "1st", "1st", "1st", "1st", "1st", "1st", "1st", "2nd", "2nd", "2nd", "2nd", "2nd", "2nd", "2nd", "2nd", "2nd")

engagement_help_perc$game <- c("no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "no", "yes", "no", "yes", "no", "no", "no", "yes", "no", "no")
        
# Here's a quick plot visualising the results.
ggplot(engagement_help_perc,
       aes(surv_title, percent_yes, fill = semester),
       fig.width = 6, fig.asp = 0.618) +
        scale_y_continuous(labels = scales::percent) +
        geom_col() +
        #theme(legend.position = "none") +
        # scale_fill_brewer(type = "qual", palette = 1, direction = -1) +
        scale_fill_grey(start = 0.2, end = 0.6, aesthetics = "fill") +
        labs(
                x = "Course topics in the first and second semester",
                y = "\n\n\nPercentage of learners who agreed a given topic would \nhelp them in mastering their English pronunciation\n",
                fill = "Semester"
        ) +
        theme(
                axis.text.x = element_text(size = 11, angle = 45, vjust = 1, hjust = 1),
                axis.text.y = element_text(size = 11, angle = 0),
                axis.title.x = element_text(face = "bold", size = 14),
                axis.title.y = element_text(face = "bold", size = 14))

ggsave("figs/fig2_topic_useful.pdf")
ggsave("figs/fig2_topic_useful.png")



# Create the mod dataset -------------------------------------------------------
# In this section, we'll create the 'master dataset' to which we'll add all the variables required for modelling. We'll call this dataset "pg_data" (phongames_data).


# Add the "in-class engagement" variable by taking a subset of the in-class engagement table.
engage <- engagement %>%
        select(1, 2, 3, 5, 7, 9)

names(engage)[6] <- "class_engagement"

# Add the "topic usefulness" variable by taking a subset of the topic usefulness table.
engagement_help$surv_scale_cell_val <- as.factor(engagement_help$surv_scale_cell_val)

topic_useful <- engagement_help %>% 
        select(1, 2, 3, 6, 8, 13)

names(topic_useful)[6] <- "was_topic_useful"

# Join the two tables. 
pg_data <- left_join(topic_useful, engage, by = c("surv_att_id", "user_id", "learner_id", "surv_title"))


# Add the "does the topic have a board game" variable by merging a sheet with topic metadata.
topic_metadata <- read_csv("data/raw/epp_topic_metadata.csv", 
                col_types = cols(has_bg = col_factor(levels = c("no", "yes")),
                                 semester = col_factor(levels = c("1st", "2nd"))))

pg_data <- left_join(pg_data, topic_metadata) 

# Clean up by renaming and reordering the columns.
pg_data <- pg_data %>% 
        select(topic = surv_title, semester, topic_has_bg = has_bg, learner_id, user_id, surv_att_id, was_topic_useful, class_engagement)

pg_data$topic <- pg_data$topic %>% factor(levels = topic_order)
summary(pg_data$topic)


# Add the "pre-class preparation score" and "post-class quiz score" variables.
source("cleansing/gradebook_step2_anon.R")

pg_data_gb_sel <- pg_data_gb %>% 
        select(1:4, 6)

# Encode factors in the two datasets before merging them -- just to make sure the join goes right.
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

pg_data_gb_sel$topic <- pg_data_gb_sel$topic %>% factor(levels = quiz_order)
pg_data$topic <- pg_data$topic %>% factor(levels = quiz_order)

pg_data_gb_sel$user_id <- as.character(pg_data_gb_sel$user_id)
pg_data$user_id <- as.character(pg_data$user_id)

pg_data_gb_sel$learner_id <- as.character(pg_data_gb_sel$learner_id)
pg_data$learner_id <- as.character(pg_data$learner_id)


# Join and clean up the mess.
pg_data <- left_join(pg_data, pg_data_gb_sel)
summary(pg_data)


# Add learner metadata.
learner_demog_pg <- read_csv("data/cleansed/learner_demog_pg.csv")
pg_data <- left_join(pg_data, learner_demog_pg)

pg_data$epp_teacher <- as.factor(pg_data$epp_teacher)
pg_data$pron_teacher <- as.factor(pg_data$pron_teacher)
pg_data$group <- as.factor(pg_data$group)
pg_data$user_id <- as.factor(pg_data$user_id)
pg_data$learner_id <- as.factor(pg_data$learner_id)
pg_data$sex <- as.factor(pg_data$sex)
pg_data$lex_score <- as.double(pg_data$lex_score)
pg_data$adapt_score <- as.double(pg_data$adapt_score)
pg_data$tonedeaf_score <- as.double(pg_data$tonedeaf_score)
pg_data$rhythm_score <- as.double(pg_data$rhythm_score)
pg_data$matura_writ <- as.double(pg_data$matura_writ)
pg_data$matura_oral <- as.double(pg_data$matura_oral)

# Inspect the final dataset
glimpse(pg_data)
summary(pg_data)


# Demographics ------------------------------------------------------------
pg_data %>% 
        group_by(learner_id) %>%
        summarise(avg_age = mean(age)) %>% 
        summarise(avg_age2 = mean(avg_age))

pg_data %>% 
        group_by(learner_id) %>%
        summarise(avg_years_of_eng = mean(years_of_eng)) %>% 
        summarise(avg_years_of_eng2 = mean(avg_years_of_eng))

sex <- pg_data %>% 
        group_by(learner_id) %>%
        count(sex)


# Engagement Mod  -------------------------------------------------
# How is in-class engagement associated with topic usefulness and presence of a board game?

q1mod1 <- lm(class_engagement ~ group + topic_has_bg + was_topic_useful + preclass_score_avg + sex + matura_oral + matura_writ, data = pg_data)

summary(q1mod1)

leveneTest(pg_data$class_engagement, pg_data$group)
leveneTest(pg_data$class_engagement, pg_data$was_topic_useful)
leveneTest(pg_data$class_engagement, pg_data$topic_has_bg)


# Quiz Mod ------------------------------------------------------------------
# How is the use of in-class board games associated with post-class quiz performance?

# Expectedly, pre-class prep tends to increase quiz score.
q2mod <- lm(assess_score_first_perc ~ group + preclass_score_avg + was_topic_useful + topic_has_bg + class_engagement + sex + matura_oral + matura_writ, data = pg_data)

summary(q2mod)

# This is an interesting plot that shows how the potential effect of board games differs according to class engagement. But it may be too complicated for the typical teacher audience, so we won't include it in the paper.
interact_plot(q2mod, pred = "preclass_score_avg", modx = "topic_has_bg", mod2 = "class_engagement", plot.points = TRUE, interval = TRUE, int.width = 0.95)

# Instead, we'll go with this plot.
interact_plot(q2mod, pred = "class_engagement", modx = "topic_has_bg", plot.points = TRUE, interval = TRUE, int.width = 0.95,
              x.label = "Self-reported in-class engagement", y.label = "\nPost-class quiz score",
              legend.main = "Topic with a \nboard game?") +
        theme(
                axis.text.x = element_text(size = 11),
                axis.text.y = element_text(size = 11),
                axis.title.x = element_text(face = "bold", size = 13),
                axis.title.y = element_text(face = "bold", size = 13))

ggsave("figs/fig3_q2mod.pdf", height = 5, width = 7)
ggsave("figs/fig3_q2mod.png", height = 5, width = 7)

ggplot(data = pg_data, aes(
        y = assess_score_first_perc,
        x = preclass_score_avg,
        color = topic_has_bg)) +
        geom_point() +
        geom_smooth(method = "lm", se = TRUE)

ggplot(data = pg_data, aes(
        y = assess_score_first_perc,
        x = preclass_score_avg,
        color = was_topic_useful)) +
        geom_point() +
        geom_smooth(method = "lm", se = TRUE)


# Test Mod ------------------------------------------------------------------
# How is the use of in-class board games associated with midterm test performance?
epp_test_exam_games <- read_csv("data/cleansed/epp_test_exam_games.csv")

epp_test_exam_games$user_id <- as.character(epp_test_exam_games$user_id)
epp_test_exam_games$learner_id <- as.character(epp_test_exam_games$learner_id)

pg_data <- left_join(pg_data, epp_test_exam_games)

# Model with WS on midterm test.
pg_data_ws_test <- pg_data %>% 
        filter(topic == "Word stress")

q3mod_ws_test <- lm(word_stress_test_score ~ group + preclass_score_avg + was_topic_useful + class_engagement + assess_score_first_perc + sex + matura_oral + matura_writ, data = pg_data_ws_test)

summary(q3mod_ws_test)

# Model with WF on midterm test.
pg_data_wf_test <- pg_data %>% 
        filter(topic == "Weak forms")

q3mod_wf_test <- lm(weak_forms_test_score ~ group + preclass_score_avg + class_engagement + assess_score_first_perc + sex + matura_oral + matura_writ, data = pg_data_wf_test)

summary(q3mod_wf_test)


# Exam Mod ------------------------------------------------------------------
# How is the use of in-class board games associated with exam performance?

# Model with WS on final exam.
pg_data_ws_exam <- pg_data %>% 
        filter(topic == "Word stress")

q4mod_ws_exam <- lm(word_stress_exam_score ~ group + preclass_score_avg + was_topic_useful + class_engagement + assess_score_first_perc + sex + matura_oral + matura_writ, data = pg_data_ws_exam)

summary(q4mod_ws_exam)

# Model with WF on final exam.
pg_data_wf_exam <- pg_data %>% 
        filter(topic == "Weak forms")

q4mod_wf_exam <- lm(weak_forms_exam_score ~ group + preclass_score_avg + class_engagement + assess_score_first_perc + sex + matura_oral + matura_writ, data = pg_data_wf_exam)

summary(q4mod_wf_exam)

# Model with Phonotactics on final exam.
pg_data_pho_exam <- pg_data %>% 
        filter(topic == "Phonotactics")

q4mod_pho_exam <- lm(phonotactics_exam_score ~ group + preclass_score_avg + class_engagement + assess_score_first_perc + sex + matura_oral + matura_writ, data = pg_data_pho_exam)

summary(q4mod_pho_exam)


# Post-class survey comments-------------------------

# Select the required columns and filter out the rows with NAs (no comments).
class_comments <- surv_resp %>% 
        filter(str_detect(surv_q_title, "Comments")) %>% 
        select(1:4, 6, 8, 16, 17) %>% 
        filter(!is.na(surv_q_text_answer))

# Some counts.
class_comments %>% count(learner_id)

# Filter out those learners who didn't complete the course evaluation survey.
class_comments <- class_comments %>% 
        filter(learner_id %in% who_completed_eval_list)

class_comments$surv_q_text_answer

# Additionally, one comment regarding board games in the Word Stress survey: "The board game was a great idea,it really helped me to memorise the rules!".