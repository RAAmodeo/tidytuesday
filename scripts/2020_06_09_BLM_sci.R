# Set-up ----
# load packages
pacman::p_load(magrittr, tidyverse, readr, skimr, tidymodels)

# define color-blindness-friendly color palette
# (option w grey from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# read in data ----
firsts <- read_csv(here::here("data",
                              "2020",
                              "2020-06-09",
                              "firsts.csv"))

sci <- read_csv(here::here("data",
                           "2020",
                           "2020-06-09",
                           "science.csv"))

# Because these datasets are relatively small,
# I will run skim() on each set & perform visual EDA

# eda firsts ----
skim(firsts)
# Impressions:
## only 3 missing data points in the entire set.
## can esily convert some var to factors

firsts <- firsts %>%
  mutate(gender = if_else(gender == "Female African American Firsts", "female", "unspecified"),
         gender = as.factor(gender),
         category = as.factor(category))#,
         # cite = extract(person, ))

# repeats <- nrow(firsts) - nrow(as.data.frame(unique(firsts$person))) - 3
#           #3 for the numer of empty cells. had trouble coding the count
#           #would rather not hardcode the missingness count

ggplot(firsts,
       aes(year, category)) +
  geom_point(aes(color = gender), alpha = 0.4)+
  scale_colour_manual(values=cbPalette) +
  ggtitle("Milestones in Black Americans Breaking Barriers",
          subtitle = "Arranged by year and category")

d <- firsts %>%
  subset(gender == "female")

ggplot(d,
       aes(year, category)) +
  geom_point(size = 3, alpha = 0.4) +
  ggtitle("Milestones of Black American Women Breaking Barriers")


# eda sci ----
sci <- sci %>%
  separate(name,
           c('last_name', 'first_name'),
           sep = ',',
           extra = "merge") %>% 
  mutate(first_name = trimws(first_name))

sci <- sci %>%
  mutate(occupation_s = as.list(strsplit(occupation_s, ";")))

# notes ----
# A simple visual inspection of these data sets shows that the "gender" &
# "person" variables are compromised and not suitable for
# immediate, accurate interpretation as I was dismayed to see in most of
# the visualizations of this #TidyTuesday set.

# I asked more of a coding question on this related #TidyTuesday post:
# https://twitter.com/jpflores_31/status/1272338604782579715
