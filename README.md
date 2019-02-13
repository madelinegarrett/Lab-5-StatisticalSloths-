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

# Team Graph 

```{r}
milksub <- milk %>%
  filter(state %in% c('New York','Colorado','Minnesota','North Carolina','Oregon')) %>%
  select(state, year, milk_million)

milkavg <- milk %>%
  mutate()
  

ggplot(data = milksub, aes(x = year, y = milk_million, color = state)) +
  geom_point()+
  ggtitle('Pounds of milk over time by state') +
  xlab('Year') +
  ylab('Milk Produced (Million lb)')
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

Average Milk by State: 
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
