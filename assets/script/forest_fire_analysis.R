---
title: "Forest Fire Analysis"
date: "2023 December"
output:
  pdf_document: default
  word_document: default
  html_document: default
---

<br>

**Project Objective:**

We will not use any type of modeling on this project, but rather we'll
focus on visualising it.

---

**About the dataset:**

`X` : X-axis spatial coordinate within the Montesinho park map: 1 to 9

`Y` : Y-axis spatial coordinate within the Montesinho park map: 2 to 9
`month`: Month of the year: 'jan' to 'dec'

`day` : Day of the week: 'mon' to 'sun'

`FFMC` : Fine Fuel Moisture Code index from the FWI system: 18.7 to 96.20

`DMC` : Duff Moisture Code index from the FWI system: 1.1 to 291.3

`DC` : Drought Code index from the FWI system: 7.9 to 860.6

`ISI` : Initial Spread Index from the FWI system: 0.0 to 56.10

`temp` : Temperature in Celsius degrees: 2.2 to 33.30

`RH` : Relative humidity in percentage: 15.0 to 100

`wind` : Wind speed in km/h: 0.40 to 9.40

`rain` : Outside rain in mm/m2 : 0.0 to 6.4

`area` : The burned area of the forest (in ha): 0.00 to 1090.84

<br>

**Key insights from the columns:**

-   A single row corresponds to the location of a fire and some
    characteristics about the fire itself.

-   Higher water presence is typically associated with less fire spread,
    therefore we can expect the water-related variables (`DMC` and
    `rain`) are associated with `area`.
    
---

<br>

**Import required libraries/packages:**

```{r}
library(ggplot2)
library(tidyr)
library(readr)
library(dplyr)
```
<br>

 **Load data:**

```{r}
setwd("C:\\Users\\S\\Desktop\\R_test")
df <- read_csv("forestfires.csv", show_col_types = FALSE)
df
```
<br>

**Pre-Processing Data: Organise month and date in the correct order:**

We can see that values in `month` and `date` are not in the right order.
We will arrange them in the correct order to facilitate intuitive
representation and analysis.

```{r}
# Check the order of values

df %>% pull(month) %>% unique
df %>% pull(day) %>% unique
```

```{r}
# Arrange values in the correct order:

df <- df %>%
  mutate(month_reordered = factor(month, levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")), day_reordered = factor(day, levels = c("mon", "tue", "wed", "thu", "fri", "sat", "sun"))
  )
```

```{r}
# Check if the values of 'Month' and 'day' are ordered properly: 

df %>% pull(month_reordered) %>% unique
df %>% pull(day_reordered) %>% unique 
```
<br>

**When do most forest fires occur?:**

To manage the forest fires, we must understand the pattern forest fires.
To do so, we will find more about the frequency of fire occurrence by
month and day, respectively.

```{r}
# Fire occurrence by month

df_occurence_month <- df %>% 
  group_by(month_reordered) %>%
  summarize(count = n())

df_occurence_month %>%
  ggplot(aes(x=count, y=month_reordered))+
  geom_bar(stat = "identity")+
  labs(
    title="Frequency of fire occurrence by month",
    x= "Frequency",
    y= "Month"
  )

```

```{r}
# Fire occurrence by day 

df_occurrence_day  <- df %>% 
  group_by(day_reordered) %>%
  summarize(count = n())

df_occurrence_day %>%
  ggplot(aes(x=count, y=day_reordered))+
  geom_bar(stat = "identity")+
  labs(
    title="Frequency of forest fire occurence by day",
    x= "Frequency", 
    y= "Day"
  )
```
<br>

**Observations:**

-   August and September see more forest fires than other months.

-   Weekend have more fires (Friday, Saturday, and Sunday).

```{r}
# Further analysis: Total number of fires for each combination of 'month_reordered' and 'day_reordered'

df_month_day <- df %>%
  group_by(month_reordered, day_reordered) %>%
  summarize(total = n())
df_month_day
```
<br> 

**How each of the other 8 variables (`FFMC` \~ `rain`) relates to month?:**

For this analysis we chose `month` as our main variable as it can vary a
lot between seasons.

To find relationship between month and the 8 other variables, we will
first need to pivot the data into a longer dimension to make it easier
to plot.

```{r}
# Pivoting the data

df_pivoted <- df%>%
  pivot_longer(cols= c(FFMC, DMC, DC, ISI, temp, RH, wind, rain),
               names_to = "column",
               values_to = "value"
               )
df_pivoted 
```
<br>

**Examining Forest Fire Severity:**

The `area` contains data on the number of hectares of forest that burned
during the forest fire. We will use this variable as an indicator of the
severity of fire.

We will use scatter plot To learn about relationships between the area
burnt and the 8 variables.

```{r}
# Using scatter plot to find relationships between the area burnt and the 8 variables

df_pivoted%>% 
    ggplot(aes(x = value, y = area))+
    geom_point()+
    facet_wrap(vars(column), scale = "free_x")+ 
    labs(
        title = "Relationships between FFMC ~ rain and area burned",
        x = "Value of each variable (FFMC ~ rain)",
        y = "Area (in hectars)"
    )
```

**Observations:**

-   The outliers in the plots represent fires that caused inordinate
    amounts of damage compared to the other fires.
    
<br>

**Outliers:**

From the scatter plot above, we noticed some outliers of values of the 8
different variables (`FFMC` \~ `rain`). We will investigate further by
employing summary statistics and histogram as for through analysis.

```{r}
# Summary stat

summary_stat_area <- df_pivoted %>%
  summarize(
    count = n(),
    sum_val = sum(area),
    min_val = min(area),
    max_val = max(area),
    med_val = median(area),
    avg = mean(area),
    upper_quartile_75 = quantile(area, probs = 0.75),
    upper_quartile_90 = quantile(area, probs = 0.9)
  )

# Convert summary statistics to a data frame

summary_table <- as.data.frame(t(summary_stat_area))
summary_table
```

-   From the summary statistics, we can notice that there is a huge gap
    between `avg` and `max`.

-   `upper_quartile_75` of 6.57 is less affected by the outlier.

-   We increased the upper quartile to 90%. Likewise,
    `upper_quartile_90` is still less affected by the outlier.

<br> 

**To more clearly visualise relationships between variables, we filtered \`area' values except for rows with very high values of area:**

```{r}
### answer from solutions - which I still have no idea of

df_pivoted %>% 
  filter(area < 300) %>% 
  ggplot(aes(x = value, y = area)) +
  geom_point() +
  facet_wrap(vars(column), scales = "free_x") +
  labs(
    title = "Relationships between other variables and area burned (area < 300)",
    x = "Value of column",
    y = "Area burned (hectare)")
```

