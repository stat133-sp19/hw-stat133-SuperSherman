---
title: "workout"
output: html_document
---
```{r}
library(ggplot2)
library(dplyr)
library(grid)
library(jpeg)

thompson_scatterplot = ggplot(data = thompson) + geom_point(aes(x = x, y = y, color = shot_made_flag))
green_scatterplot = ggplot(data = green) + geom_point(aes(x = x, y = y, color = shot_made_flag))
durant_scatterplot = ggplot(data = durant) + geom_point(aes(x = x, y = y, color = shot_made_flag))
iguodala_scatterplot = ggplot(data = iguodala) + geom_point(aes(x = x, y = y, color = shot_made_flag))
curry_scatterplot = ggplot(data = curry) + geom_point(aes(x = x, y = y, color = shot_made_flag))
court_file <- "../image/nba-court.jpg"
court_image <- rasterGrob(readJPEG(court_file),width = unit(1,"npc"),height = unit(1, "npc"))
klay_shot_chart <- ggplot(data = thompson) +annotation_custom(court_image, -250, 250, -50, 420) +
geom_point(aes(x = x, y = y, color = shot_made_flag)) +ylim(-50, 420) +ggtitle('Shot Chart: Klay Thompson (2016 season)') +
theme_minimal()
pdf(file = '../image/klay-thompson-shot-chart.pdf', width = 6.5, height = 5, onefile = TRUE)
klay_shot_chart
dev.off()

green_shot_chart <- ggplot(data = green) +annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +ylim(-50, 420) +ggtitle('Shot Chart: Draymond Green (2016 season)') +
  theme_minimal()
pdf(file = '../image/draymond-green-shot-chart.pdf', width = 6.5, height = 5, onefile = TRUE)
green_shot_chart
dev.off()


durant_shot_chart <- ggplot(data = durant) +annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +ylim(-50, 420) +ggtitle('Shot Chart: Kevin Durant (2016 season)') +
  theme_minimal()
pdf(file = '../image/kevin-durant-shot-chart.pdf', width = 6.5, height = 5, onefile = TRUE)
durant_shot_chart
dev.off()

iguodala_shot_chart <- ggplot(data = iguodala) +annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +ylim(-50, 420) +ggtitle('Shot Chart: ANdre Iguodala(2016 season)') +
  theme_minimal()
pdf(file = '../image/andre-igoudala-shot-chart.pdf', width = 6.5, height = 5, onefile = TRUE)
iguodala_shot_chart
dev.off()

curry_shot_chart <- ggplot(data = curry) +annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +ylim(-50, 420) +ggtitle('Shot Chart: Stephen Curry (2016 season)') +
  theme_minimal()
pdf(file = '../image/stephen-curry-shot-chart.pdf', width = 6.5, height = 5, onefile = TRUE)
curry_shot_chart
dev.off()


pdf(file = '../image/gsw-shot-chart.pdf', width = 8, height = 7, onefile = TRUE)
ggplot(data = players) +annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +ylim(-50, 420) +ggtitle('Shot Chart: Golden States (2016 season)') +
  theme_minimal() +
  facet_wrap(~name)
dev.off()

```


