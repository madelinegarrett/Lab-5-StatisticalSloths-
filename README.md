# Lab-5-StatisticalSloths-

---
title: "Lab5_StatsSloths"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
library(tidyverse)
milk <- read.csv('state_milk_production.csv')
head(milk)
```

# Distribution of the Yearly Milk Production for: 
### Madeline's Birth Year 

```{r}
milk <- milk %>%
  mutate(milk_million = milk_produced/1000000)
milk2000 <- milk %>%
  filter(year == 2000)
ggplot(data = milk2000, aes(x = milk_million)) +
  geom_histogram(bins = 100) + 
  ggtitle('Histogram of milk produced in 2000 by state')

```

### Madeline's Birth Year Summary: 

Mean: 
```{r}
sum_mean <- milk2000 %>%
  group_by(state) %>%
  summarise(Average = mean(milk_million)) %>%
  as_tibble()
sum_mean
```

Median: 
```{r}
sum_median <- milk2000 %>%
  group_by(state) %>%
  summarise(Median = median (milk_million)) %>%
  as_tibble()
sum_median
```

Max: 
```{r}
milk2000 %>%
  group_by(state) %>%
  summarise(
    max = max(milk_million)
    )

```

### Kevin's Birth Year 

```{r}
milk <- milk %>%
  mutate(milk_million = milk_produced/1000000)
milk2001 <- milk %>%
  filter(year == 2001)
```
### Kevin's Birth Year Summary: 

Average Milk: 
```{r}
milk2001 %>%
  summarise(average = mean(milk_million)) %>%
  as_tibble()
```
Median Milk By State: 
```{r}
milk2001 %>%
  summarise(median = median (milk_million)) %>%
  as_tibble()
```
Most Milk: 
```{r}
milk2001 %>%
  summarise(max = max(milk_million))
```
Least Milk: 
```{r}
milk2001 %>%
  summarise(min = min(milk_million))
```
### Katie's Birth Year:
```{r}
milk <- milk %>%
  mutate(milk_million = milk_produced/1000000)
milk1999 <- milk %>%
  filter(year == 1999)
ggplot(data = milk1999, aes(x = milk_million)) +
  geom_histogram(bins = 100) +
  ggtitle('Histogram of Milk Produced in 1999 by State')
```

### Katie's Birth Year Summary:
Average Milk:
```{r}
sum_average <- milk1999 %>%
  summarise(average = mean(milk_million)) %>%
  as_tibble()
sum_average
```
Median Milk by State: 
```{r}
median.sum <- milk1999 %>%
  group_by(state) %>%
  summarise(median = mean(milk_million)) %>%
  as.tibble()
median.sum
```
Most Milk by State:
```{r}
max.milk <- milk1999 %>%
  group_by(state) %>%
  summarise(max = max(milk_million))
max.milk
```
Least Milk by State:
```{r}
min.milk <- milk1999 %>%
  group_by(state) %>%
  summarise(min = min(milk_million))
min.milk
```
### The year when the most milk was produced in the United States: 2014
```{r}
most.milk <- milk %>%
  group_by(year) %>%
  summarise(max = max(milk_million)) %>%
  arrange(desc(max))
most.milk
```
