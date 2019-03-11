---
title: "Untitled"
output: html_document
---
```{r}
library(ggplot2)
library(dplyr)
curry<- read.csv('../data/stephen-curry.csv', stringsAsFactors = FALSE)
thompson<- read.csv('../data/klay-thompson.csv', stringsAsFactors = FALSE)
durant<- read.csv('../data/kevin-durant.csv', stringsAsFactors = FALSE)
green<- read.csv('../data/draymond-green.csv', stringsAsFactors = FALSE)
iguodala<- read.csv('../data/andre-iguodala.csv', stringsAsFactors = FALSE)
iguodala$name = "igoudala"
thompson$name = "thompson"
green$name = "green"
durant$name = "durant"
curry$name = "curry"
iguodala$shot_made_flag[which(iguodala$shot_made_flag == "y")] <- "made shot"
iguodala$shot_made_flag[which(iguodala$shot_made_flag == "n")] <- "missed shot"
green$shot_made_flag[which(green$shot_made_flag == "y")] <- "made shot"
green$shot_made_flag[which(green$shot_made_flag == "n")] <- "missed shot"
thompson$shot_made_flag[which(thompson$shot_made_flag == "y")] <- "made shot"
thompson$shot_made_flag[which(thompson$shot_made_flag == "n")] <- "missed shot"
durant$shot_made_flag[which(durant$shot_made_flag == "y")] <- "made shot"
durant$shot_made_flag[which(durant$shot_made_flag == "n")] <- "missed shot"
curry$shot_made_flag[which(curry$shot_made_flag == "y")] <- "made shot"
curry$shot_made_flag[which(curry$shot_made_flag == "n")] <- "missed shot"
iguodala$minute = (iguodala$period-1)*12 + (12-iguodala$minutes_remaining)
green$minute = (green$period-1)*12 + (12-green$minutes_remaining)
durant$minute = (durant$period-1)*12 + (12-durant$minutes_remaining)
thompson$minute = (thompson$period-1)*12 + (12-thompson$minutes_remaining)
curry$minute = (curry$period-1)*12 + (12-curry$minutes_remaining)
sink(file = '../output/andre-iguodala-summary.txt')
summary(iguodala)
sink()
sink(file = '../output/draymond-green-summary.txt')
summary(green)
sink()
sink(file = '../output/klay-thompson-summary.txt')
summary(thompson)
sink()
sink(file = '../output/kevin-durant-summary.txt')
summary(durant)
sink()
sink(file = '../output/stephen-curry-summary.txt')
summary(curry)
sink()
players <- rbind(curry, durant,green,iguodala,thompson)
write.csv(players, file = '../data/shots-data.csv')
sink(file = '../output/shots-data-summary.txt')
summary(players)
sink()

```

