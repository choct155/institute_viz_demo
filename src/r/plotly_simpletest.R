install.packages("plotly")

library(plotly)
p <- plot_ly(midwest, x=~percollege, color=~state, type = "box")

p

Sys.setenv("plotly_username" = "chexyu")
Sys.setenv("plotly_api_key" = "AHRjBvsviGdqtdaZYXgy")

api_create(p, filename = "r-simple-test")