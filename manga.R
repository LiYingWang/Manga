# install.packages("devtools")
# devtools::install_github("hadley/rvest")

library(tidyverse)
library(rvest)
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
# plot the top 30 manga by year and rank

# tidy the data...
df_manga_top_30 <-
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
  # keep only top 30
  slice(1:30)

# plot the top 30 manga by year
library(ggrepel)
ggplot(df_manga_top_30,
       aes(year, rank)) +
  geom_point() +
  geom_text_repel(aes(label = title,
                      color = rank_groups)) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Top 30 Manga from anime-planet.com")

ggsave("top30-manga.png")

#--------------------------------------------
# plot the top manga by read/reading/want to read

# get page for each story
top_manga_urls <- 
  top_manga %>% 
  html_nodes("td a") %>%
  html_attr('href') 

top_manga_urls <-       
  str_glue("https://www.anime-planet.com/{top_manga_urls}")

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




