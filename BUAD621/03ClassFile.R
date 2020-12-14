## CLASS EXERCISE#1:  Do this in a new script window File -> New Script
## Question:  Does a certain race dominate a movie (in terms of words spoken)?  Does the dominant `Race` differ across the movies?

lotr_tidy %>%
  group_by(Film, Race) %>%
  summarize(wordsSpoken = sum(Words)) %>%
  arrange(Film,desc(wordsSpoken))


## CLASS EXERCISE#2:  Do this in a new script window File -> New Script
## MAKE TWO TIDY DATAFRAMES (df'S): 
## #1: df OF FILM/CHARACTER/WORD COUNT OBSERVATIONS
## ANSWER THE FOLLOWING QUESTIONS USING DPLYR CODE:
#     1. WHO ARE THE TOP 5 CHARACTERS BY WORD COUNT FOR EACH MOVIE
#     2. WHO ARE THE TOP 5 CHARACTERS BY WORD COUNT ACROSS ALL MOVIES
## #2: df OF ALL CHARACTERS AND THEIR RACE
##    3. USE LEFT JOIN - DPLYR FUNCTIONS
##       TO FIND THE TOP 5 HOBBITS BY WORD COUNT ACROSS ALL MOVIES

# ----------------------------------------


lotr_dat %>%
  group_by(Film,Character) %>%
  summarize(totalWords = sum(Words)) %>%
  arrange(Film, desc(totalWords)) %>%
  top_n(3)

lotr_dat %>%
  group_by(Character) %>%
  summarize(totalWords = sum(Words)) %>%
  arrange(desc(totalWords)) %>%
  top_n(5)

lotr_dat %>%
  filter(Race == "Hobbit") %>%
  group_by(Character) %>%
  summarize(totalWords = sum(Words)) %>%
  arrange(desc(totalWords)) %>%
  top_n(5)
