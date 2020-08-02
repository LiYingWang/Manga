library(plotly)

manga_top_50_inter <-
  df_manga_top_100 %>% 
  slice(1:50) %>% 
  pivot_wider(names_from = type,
              values_from = rank)

top_50 <- plot_ly(manga_top_50_inter, 
                  x = ~ manga, 
                  y = ~ year, 
                  name = "mange", 
                  type = 'scatter',
                  mode = "markers", 
                  text = ~title,
                  marker = list(color = "pink"),
                  hovertemplate = paste('rank: %{x}', '<br>%{text}<br>'),
                  texttemplate = '%{text}', textposition = 'outside')

top_50 <- top_50 %>% add_trace(x = ~ `light novel`, 
                               y = ~ year, 
                               name = "light novel",
                               type = 'scatter',
                               mode = "markers", 
                               text = ~title,
                               marker = list(color = "blue"),
                               hovertemplate = paste('rank: %{x}', '<br>%{text}<br>'),
                               texttemplate = '%{text}', textposition = 'outside')

top_50 <- top_50 %>% layout(
  title = "Top 50 Manga and Light Novel",
  xaxis = list(title = "rank"),
  margin = list(l = 100))
