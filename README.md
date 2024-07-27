
# Forest Fire Analysis

2023 December


<br><br>

**Project Objective:** To analyse fire occurrence data to uncover patterns and relationships between various factors such as month, area, rain, fire severity, and so on.

<br>

**Techniques used:**

    Data Preprocessing:
        -	Arrange values (e.g., month and date) in the correct order for intuitive analysis.
        -	Pivot the data into a longer format to make it easier to plot (for scatter plots).
        
    Data Visualisation using ggplot:
        - 	Create a histogram to understand the pattern of fire occurrences by month.
        -	Use a scatter plot to find relationships between the variable 'month' and other variables (area, rain, etc.) and fire severity.
        -	Identify outliers through summary statistics from the scatter plot.
        -	Remove outliers to better visualize relationships between variables.


---

<br>

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

**Note:**

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
![image](https://github.com/user-attachments/assets/1d6de4a9-44e1-4c47-ab0f-d474ed3c6b54)

<br><br>

**Pre-Processing Data: Organise month and date in the correct order:**

We can see that values in `month` and `date` are not in the right order.
We will arrange them in the correct order to facilitate intuitive
representation and analysis.

```{r}
# Check the order of values

df %>% pull(month) %>% unique
```
![image](https://github.com/user-attachments/assets/ec0433ff-787e-47b2-954e-17f379512dbd)

<br>

```{r}
df %>% pull(day) %>% unique
```
![image](https://github.com/user-attachments/assets/28f8fde3-0e1f-4793-95b5-5d502008ebeb)

<br>

```{r}
# Arrange values in the correct order:

df <- df %>%
  mutate(month_reordered = factor(month, levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")), day_reordered = factor(day, levels = c("mon", "tue", "wed", "thu", "fri", "sat", "sun"))
  )
```
![image](https://github.com/user-attachments/assets/19463c87-76e4-4b8c-93d5-23a033f147e5)

<br>

```{r}
# Check if the values of 'Month' and 'day' are ordered properly: 

df %>% pull(month_reordered) %>% unique
```
![image](https://github.com/user-attachments/assets/eaacc56d-44f0-45f7-8c1b-2df2d239133f)

<br>

```{r}
df %>% pull(day_reordered) %>% unique 
```
![image](https://github.com/user-attachments/assets/3585e018-d4f3-4d50-9a8f-da5a8015e117)

<br><br>

**When do most forest fires occur?:**

To manage the forest fires, we must understand the pattern forest fires. Let's find more about the frequency of fire occurrence by month and day, respectively.

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
![image](https://github.com/user-attachments/assets/36cf05a3-5c99-4d91-a16e-c01cdf726d7b)

<br>

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

![image](https://github.com/user-attachments/assets/7e6aa488-0b87-416e-b052-e069a439f577)

**Observations:**

-   August and September see more forest fires than other months.

-   Weekend have more fires (Friday, Saturday, and Sunday).

<br><br>

```{r}
# Further analysis: Total number of fires for each combination of 'month_reordered' and 'day_reordered'

df_month_day <- df %>%
  group_by(month_reordered, day_reordered) %>%
  summarize(total = n())
df_month_day
```
![image](https://github.com/user-attachments/assets/4e5c8511-a029-4cf6-8d85-908a46dc3bfd)

```{r}
df_month_day
```
![image](https://github.com/user-attachments/assets/ee3d188b-15ce-4b9c-b08c-8bb21acc9b8e)

<br><br>

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
![image](https://github.com/user-attachments/assets/c27607df-0b95-42a4-be9a-bf014f245dce)

<br><br>

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
![image](https://github.com/user-attachments/assets/d4fff9f3-b9e2-434e-949d-9e6ee16f14a2)

**Observations:**

-   The outliers in the plots represent fires that caused inordinate
    amounts of damage compared to the other fires.

<br><br>

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

![image](https://github.com/user-attachments/assets/b43ed945-1793-4360-949c-5527253d139d)

-   From the summary statistics, we can notice that there is a huge gap
    between `avg` and `max`.

-   `upper_quartile_75` of 6.57 is less affected by the outlier.

-   We increased the upper quartile to 90%. Likewise,
    `upper_quartile_90` is still less affected by the outlier.

<br><br>

**To better visualise relationships between variables, we filtered \`area' values except for rows with very high values of area:**

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
![image](https://github.com/user-attachments/assets/80ee4cb5-a7ff-4622-aba8-8f442311396f)
