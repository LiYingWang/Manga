# install.packages("devtools")
# devtools::install_github("hadley/rvest")

library(rvest)
library(tidyverse)
top_manga <- read_html("https://www.anime-planet.com/manga/top-manga")

# download data from URL
info_manga <- top_manga %>%
  html_nodes("td.tableYear , .tableTitle , td.tableRank") %>%
  html_text()

# convert the vector into dataframe
df_manga <- matrix(info_manga, ncol=3, byrow=TRUE) %>% 
  as.data.frame(stringsAsFactors=FALSE) %>% 
  plyr::rename(c("V1" = "rank", "V2" = "title", "V3" = "year"))

# make subset for every rank 10 
groups <- cut_interval(1:nrow(df_manga), n = 10, labels = FALSE)
df_manga_groups <-
  df_manga %>% 
  mutate(rank_groups = as.character(groups)) %>% 
  mutate(rank = as.numeric(rank))
  
# plot the top 30 manga by years
ggplot(slice(df_manga_groups, 1:30), aes(year, rank))+
  geom_point()+
  geom_text(aes(label=title, 
                color= rank_groups, 
                hjust= "inward", vjust= "inward", 
                check_overlap = TRUE))+
  theme(legend.position="none")+
  ggtitle("Top 30 Manga") +
  theme(plot.title = element_text(hjust=0.5))

ggsave("top30-manga.png")

# bar plot
df_manga_groups %>% 
  ggplot(aes(x = year)) +
  geom_bar() +
  facet_wrap(~as.numeric(rank_groups)) +
  theme_minimal()
