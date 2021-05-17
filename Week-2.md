# Data Exploration:

This data is from Inside Airbnb (www.insideairbnb.com) (Cox, 2021). In
this section we will preform some explanatory statistics of the Month
February 2021.

``` r
library(broom) # Helps with model outputs etc
library(here) # Helps with specifying path names
library(janitor) 
library(tidymodels) 
library(tidyverse) 
library(visdat) 
```

We can load the February 2021 data into R by:

``` r
feb_21_airbnb <- read_csv("Airbnb February 2021.csv", guess_max = 20000)
```

``` r
names(feb_21_airbnb) %>% length()
```

    ## [1] 74

Cleaning the price column by removing all dollar signs and commas.

``` r
feb_21_airbnb <- feb_21_airbnb %>% 
  mutate(price = str_remove(price, "\\$"),
         price = str_remove(price, ","),
         price = as.integer(price)
         )
glimpse(feb_21_airbnb$price)
```

    ##  int [1:15832] 469 96 72 45 128 100 70 113 93 101 ...

``` r
feb_21_airbnb %>%
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 20) +
  theme_classic() +
  labs(x = "February 2021 Price per night",
       y = "February 2021 number of properties")
```

![](Week-2_files/figure-markdown_github/unnamed-chunk-5-1.png)

There are definitely some outliers existing. We can split the data and
look at prices per night greater than $500 and less than $500.

``` r
feb_21_airbnb %>%
  filter(price > 1000) %>% 
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 10) +
  theme_classic() +
  labs(x = "Price per night",
       y = "Number of properties")
```

![](Week-2_files/figure-markdown_github/unnamed-chunk-6-1.png)
