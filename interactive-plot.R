library(plotly)

manga_top_100_inter <-
  df_manga_top_100 %>% 
  pivot_wider(names_from = type,
              values_from = rank)

top_100 <- plot_ly(manga_top_100_inter, 
                  x = ~ manga, 
                  y = ~ year, 
                  name = "manga", 
                  type = 'scatter',
                  mode = "markers", 
                  text = ~title,
                  marker = list(color = "pink"),
                  hovertemplate = paste('rank: %{x}', '<br>%{text}<br>'),
                  texttemplate = '%{text}', textposition = 'outside')

top_100 <- top_100 %>% add_trace(x = ~ `light novel`, 
                               y = ~ year, 
                               name = "light novel",
                               type = 'scatter',
                               mode = "markers", 
                               text = ~title,
                               marker = list(color = "blue"),
                               hovertemplate = paste('rank: %{x}', '<br>%{text}<br>'),
                               texttemplate = '%{text}', textposition = 'outside')

top_100 <- top_100 %>% layout(
  title = "Top 100 Manga and Light Novel",
  xaxis = list(title = "rank"),
  margin = list(l = 100))
