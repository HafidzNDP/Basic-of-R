iris %>% 
  # second layer: aesthetics
  ggplot(aes(
    x = Sepal.Length, 
    y = Sepal.Width, 
    color = Species
  )) +
  # third layer: geometric form
  geom_point() + 
  # add grey theme
  theme_minimal() + 
  labs(
    title = "Iris Plot",
    subtitle = "Based on Fisher Dataset",
    x = "Sepal Length",
    y = "Sepal Width",
    caption = "Plot made by Hadimaster",
    tag = "Clustering Example"
  )
