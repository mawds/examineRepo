# Uses https://github.com/lorenzwalthert/gitsum
# Analyse r-novice-gapminder logs to see which episodes
# get the most traffic

library(gitsum)
library(tidyverse)
library(forcats)
library(plotly)
gitdir <- "../r-novice-gapminder/" 
# init_gitsum(gitdir)
tbl <- parse_log_detailed(gitdir)

tbl %>% group_by(author_name) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n)) %>% 
  filter(author_name == "David Mawdsley")

# Total number of commits
tbl %>% unnest_log() %>% 
  set_changed_file_to_latest_name() %>% 
  filter( grepl("^_episodes_rmd/", changed_file) ) %>% 
  filter( grepl("\\.Rmd$", changed_file )) %>%
  mutate(shortname = basename(changed_file)) %>% 
  ggplot(aes(x = shortname)) + geom_bar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

tbl %>% unnest_log() %>% 
  set_changed_file_to_latest_name() %>% 
  filter( grepl("^_episodes_rmd/", changed_file) ) %>% 
  filter( grepl("\\.Rmd$", changed_file )) %>%
  mutate(shortname = basename(changed_file)) %>% 
  filter(edits > 1) %>% # Remove trivial changes
  ggplot(aes(x = date, y = shortname, color = log(edits))) + geom_point(size = .5)
 

tbl %>% unnest_log() %>% 
  set_changed_file_to_latest_name() %>% 
  filter( grepl("^_episodes_rmd/", changed_file) ) %>% 
  filter( grepl("\\.Rmd$", changed_file )) %>%
  mutate(shortname = basename(changed_file)) %>% 
  ggplot(aes(x = date, y = sqrt(insertions))) +
  geom_bar(stat = "identity", color = "green") + 
  geom_bar(aes(y = -sqrt(deletions)), color = "red", stat = "identity") +
  facet_grid(shortname ~ .) +  
  theme_minimal() +
  theme(strip.text.y = element_text(angle = 0))

