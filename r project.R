library(tidyverse)
library(ggplot2)

view(mpg)

data(mpg)

mpg %>%
  group_by(manufacturer) %>%
  summarise(avg_mpg = mean(hwy)) %>%
  arrange(desc(avg_mpg))

mpg %>%
  group_by(manufacturer) %>%
  summarise(avg_mpg = mean(hwy)) %>%
  ggplot(aes(x = reorder(manufacturer, -avg_mpg), y = avg_mpg)) +
  geom_col() +
  coord_flip() +
  labs(title = "Average MPG by Manufacturer",
       x = "Manufacturer",
       y = "Average MPG")

mpg %>%
  group_by(drv) %>%
  summarise(avg_mpg = mean(hwy)) %>%
  arrange(desc(avg_mpg))

mpg %>%
  group_by(drv) %>%
  summarise(avg_mpg = mean(hwy)) %>%
  ggplot(aes(x = reorder(drv, -avg_mpg), y = avg_mpg)) +
  geom_col() +
  coord_flip() +
  labs(title = "Average MPG by Drive Train",
       x = "Drive Train",
       y = "Average MPG")

mpg %>%
  ggplot(aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Trend between Engine Displacement and MPG",
       x = "Engine Displacement",
       y = "MPG")

mpg %>%
  ggplot(aes(x = year, y = hwy)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Trend of MPG across Years",
       x = "Year",
       y = "MPG")  


mpg$trans <- as.character(mpg$trans)
mpg$trans[mpg$trans == "auto(l3)"] <- "Auto"
mpg$trans[mpg$trans == "auto(l4)"] <- "Auto"
mpg$trans[mpg$trans == "auto(l5)"] <- "Auto"
mpg$trans[mpg$trans == "auto(l6)"] <- "Auto"
mpg$trans[mpg$trans == "manual(l3)"] <- "Manual"
mpg$trans[mpg$trans == "manual(l4)"] <- "Manual"
mpg$trans[mpg$trans == "manual(l5)"] <- "Manual"
mpg$trans[mpg$trans == "manual(l6)"] <- "Manual" 

mpg %>%
  ggplot(aes(x = displ, y = hwy, color = trans)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Trend between Engine Displacement and MPG based on Transmission",
       x = "Engine Displacement",
       y = "MPG",
       color = "Transmission")







































































































































































































