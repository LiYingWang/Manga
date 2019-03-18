# install.packages("devtools")
# devtools::install_github("hadley/rvest")

library(tidyverse)
library(tidytext)
library(rvest)
library(data.table)
library(ggbeeswarm)
library(ggridges)
top_manga <-
  read_html("https://www.anime-planet.com/manga/top-manga")

# download data from URL
info_manga <-
  top_manga %>%
  html_nodes("td.tableYear,
             .tableTitle,
             td.tableRank") %>%
  html_text()
#--------------------------------------------
# tidy the data...
df_manga_top_60 <-
  matrix(info_manga,
         ncol = 3,
         byrow = TRUE) %>%
  as_data_frame() %>%
  rename_at(vars(c('V1', 'V2', 'V3')),
            ~ c('rank', 'title', 'year')) %>%
  # covert chracter to numeric
  mutate(rank = as.numeric(rank),
         # make subset for every rank 10
         # covert numeric to chracter
         rank_groups = as.character(cut_interval(
           1:nrow(.),
           n = 10,
           labels = FALSE
         ))) %>%
  # make sure they are in order by rank
  arrange(rank) %>%
  # keep only top 60
  slice(1:60)

# add value to blank cells
df_manga_top_60$year[df_manga_top_60$year==""] <- "2018"  

# plot the top 60 manga by year
library(ggrepel)
ggplot(df_manga_top_60,
       aes(year, rank)) +
  geom_point() +
  geom_text_repel(aes(label = title,
                      color = rank_groups)) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Top 60 Manga from anime-planet.com") +
  scale_y_reverse(limits = c(60, 1), 
                  breaks = c(seq(60,1,by = -10), 1))

#ggsave("top60-manga.png")

# bar plot by count
ggplot(df_manga_top_60) +
  geom_bar(aes(year, fill = rank_groups), position= 'dodge') +
  scale_y_continuous(limits = c(0,6), breaks = c(seq(0,6,by = 2), 6))

# bar plot by proportion
ggplot(df_manga_top_60) +
  geom_bar(aes(year, y = ..prop.., fill = rank_groups), position= 'fill')

# polar plot
bar <- ggplot(df_manga_top_60) + 
  geom_bar(aes(year, fill = rank_groups), 
    show.legend = TRUE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()

# for categorical variable, transform the chr to factor and reverse the order
cate <-
df_manga_top_60 %>% 
  mutate(rank_groups = factor(rank_groups),
         rank_groups = factor(rank_groups, levels = rev(levels(rank_groups))),
         top10 = ifelse(rank_groups == "1", 'top10', 'not top10'))

ggplot(cate) +
  geom_count(aes(year, y = rank_groups, color = ..n..))+
  guides(color = 'legend') +
  scale_color_gradient(high = "#3f0081", low = "#8f7ee2")
  
# original: high = "#132B43", low = "#56B1F7"; purple: high = "#3f0081", low = "	#8f7ee2"

# boxplot

#---------------------stats-----------------------
# plot the top manga by read/reading/want to read

# get page for each story
top_manga_urls <- 
  top_manga %>% 
  html_nodes("td a") %>%
  html_attr('href') 

top_manga_urls <-       
  str_glue("https://www.anime-planet.com{top_manga_urls}")

# we will go to the page for each story and get the user stats
manga_story_stats <- 
map(top_manga_urls[1:10],
    ~.x %>% 
      read_html()  %>% 
      html_nodes(".status2 .slCount"))

# tidy the stats
map(manga_story_stats, 
    ~.x %>% 
      html_text())

#--------------------tags-------------------------
# go to the page for each story and get the tags
manga_story_tags <- 
  map(top_manga_urls,
      ~.x %>% 
        read_html() %>% 
        html_nodes(".tags a") %>% 
        html_text() %>% 
        tibble(text = .))

# convert the lists of tables to one big table 
df <- 
  rbindlist(manga_story_tags, idcol = 'rank') %>% 
  mutate(rank = as.character(rank))

# combine manga top 100 info and tags   
df_manga_t100_tags <-
  matrix(info_manga,
         ncol = 3,
         byrow = TRUE) %>%
  as_data_frame() %>%
  rename_at(vars(c('V1', 'V2', 'V3')),
            ~ c('rank', 'title', 'year')) %>% 
  mutate(year = ifelse(year == "", NA, year)) %>% 
  mutate_all(any_vars(replace_na(.,"2018"))) %>% 
  left_join(df) %>% 
  mutate(text = ifelse(text == "Manhua", "Manhwa", text)) %>% 
  mutate(text = ifelse(text == "Shounen", "Seinen", text))

# ploting the most common tags from top 100 manga
tags_all <-
  df_manga_t100_tags %>% 
  count(text, sort = TRUE)

tags_com <-
  tags_all %>%  
  filter(n > 11) %>%
  filter(!text %in% c("Manhwa", "Full Color", 
                      "Webtoon", "Light Novel", 
                      "Adapted to Anime")) %>% 
  mutate(text = reorder(text, n)) 

ggplot(tags_com,
       aes(text, n)) +
  geom_col() +
  theme_minimal() +
  coord_flip() +
  labs(y = "frequency", x ="genre", title = "Top 100 Manga: Popular genre")

#-----------------ratings vs genre---------------------
# extract common tags to a list
list_com_tags <-
  pull(tags_com, text)

# filter those common tags from the full dataset
rate_tags <-
  df_manga_t100_tags %>% 
  mutate(rank = as.numeric(rank)) %>% 
  filter(text %in% list_com_tags) 

# box plot
ggplot(rate_tags,
       aes(reorder(text, -rank),
           rank)) +
  geom_boxplot() +
  #geom_quasirandom(alpha = 0.8) +
  geom_beeswarm(alpha = 0.7) +
  theme_minimal() +
  scale_y_reverse(limits = c(100, 1), 
                  breaks = c(seq(100,1,by = -10), 1)) +
  labs(y = "rank", x ="genre", title = "Rank for Popular genre in Top 100 manga")

# ridge plot for rank
ggplot(rate_tags, aes(rank, text)) +
  geom_density_ridges(
    jittered_points = TRUE, position = "raincloud",
    alpha = 0.7, scale = 0.9) +
    scale_x_log10() +
  theme_ridges()

# ridge plot for year
rate_tags %>%
  mutate(year = as.numeric(year)) %>% 
  ggplot(aes(year, text)) +
  geom_density_ridges(jittered_points = TRUE,
                      alpha = 0.7) +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_ridges()

# plot barplot for years by commen tags
rate_tags %>% 
  ggplot() + 
  geom_bar(aes(year)) +
  theme_minimal() +
  facet_wrap(~text)

#----------------------patterns of tags------------------
library(scales)
# tidy the data to have to rank groups for comparison 
manga_t100_tags_groups <-
df_manga_t100_tags %>% 
  mutate(rank = as.numeric(rank), year = as.numeric(year)) %>% 
  mutate(rank_2groups = ifelse(rank < 51, "1-50", "51-100"),
         year_2groups = ifelse(year < 2010, "before 2010", "after 2010"))

# plot the comparision of genre frequency of manga for different rating groups 
fre_rank_groups <- 
  manga_t100_tags_groups %>%
  count(rank_2groups, text) %>%
  group_by(rank_2groups) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(rank_2groups, proportion) %>% 
  gather(rank_2groups, proportion, `1-50`)

ggplot(fre_rank_groups, 
       aes(x = proportion, y = `51-100`, color = abs(`51-100` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.5, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = text), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.01), low = "darkslategray4", high = "red") +
  facet_wrap(~rank_2groups) +
  theme(legend.position="none") +
  labs(y = "Top 51-100", x = NULL)

# plot the comparision of genre frequency of manga for different time periods
fre_year_groups <- 
  manga_t100_tags_groups %>%
  count(year_2groups, text) %>%
  group_by(year_2groups) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(year_2groups, proportion) %>% 
  gather(year_2groups, proportion, `after 2010`)

ggplot(fre_year_groups, 
       aes(x = proportion, y = `before 2010`, color = abs(`before 2010` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.5, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = text), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.01), low = "darkslategray4", high = "red") +
  facet_wrap(~year_2groups) +
  theme(legend.position="none") +
  labs(y = "before 2010", x = NULL)




