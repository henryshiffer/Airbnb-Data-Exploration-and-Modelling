# Data Exploration:

This data is from Inside Airbnb (www.insideairbnb.com) (Cox, 2021). In
this section we will preform some explanatory statistics of Toronto’s
Airbnb data from the month February 2021.

``` r
library(tidymodels) 
library(tidyverse) 
library(visdat) 
```

We can load Toronto’s February 2021 Airbnb data into R by:

``` r
feb_21_airbnb <- read_csv("Airbnb February 2021.csv", guess_max = 20000)
```

To get a high level view of the data and it’s attributes, we can use the
**glimpse()** function from the package **tidyverse**.

``` r
glimpse(feb_21_airbnb)
```

    ## Rows: 15,832
    ## Columns: 74
    ## $ id                                           <dbl> 1419, 8077, 23691, 27423…
    ## $ listing_url                                  <chr> "https://www.airbnb.com/…
    ## $ scrape_id                                    <dbl> 2.021021e+13, 2.021021e+…
    ## $ last_scraped                                 <date> 2021-02-09, 2021-02-09,…
    ## $ name                                         <chr> "Beautiful home in amazi…
    ## $ description                                  <chr> "This large, family home…
    ## $ neighborhood_overview                        <chr> "The apartment is locate…
    ## $ picture_url                                  <chr> "https://a0.muscache.com…
    ## $ host_id                                      <dbl> 1565, 22795, 93825, 1181…
    ## $ host_url                                     <chr> "https://www.airbnb.com/…
    ## $ host_name                                    <chr> "Alexandra", "Kathie & L…
    ## $ host_since                                   <date> 2008-08-08, 2009-06-22,…
    ## $ host_location                                <chr> "Vancouver, British Colu…
    ## $ host_about                                   <chr> "I live in Vancouver, Ca…
    ## $ host_response_time                           <chr> "N/A", "N/A", "N/A", "N/…
    ## $ host_response_rate                           <chr> "N/A", "N/A", "N/A", "N/…
    ## $ host_acceptance_rate                         <chr> "N/A", "N/A", "N/A", "N/…
    ## $ host_is_superhost                            <lgl> FALSE, FALSE, FALSE, FAL…
    ## $ host_thumbnail_url                           <chr> "https://a0.muscache.com…
    ## $ host_picture_url                             <chr> "https://a0.muscache.com…
    ## $ host_neighbourhood                           <chr> "Commercial Drive", "Har…
    ## $ host_listings_count                          <dbl> 1, 2, 2, 1, 2, 3, 2, 13,…
    ## $ host_total_listings_count                    <dbl> 1, 2, 2, 1, 2, 3, 2, 13,…
    ## $ host_verifications                           <chr> "['email', 'phone', 'rev…
    ## $ host_has_profile_pic                         <lgl> TRUE, TRUE, TRUE, TRUE, …
    ## $ host_identity_verified                       <lgl> TRUE, FALSE, TRUE, TRUE,…
    ## $ neighbourhood                                <chr> "Toronto, Ontario, Canad…
    ## $ neighbourhood_cleansed                       <chr> "Little Portugal", "Wate…
    ## $ neighbourhood_group_cleansed                 <lgl> NA, NA, NA, NA, NA, NA, …
    ## $ latitude                                     <dbl> 43.64617, 43.64105, 43.6…
    ## $ longitude                                    <dbl> -79.42451, -79.37628, -7…
    ## $ property_type                                <chr> "Entire house", "Private…
    ## $ room_type                                    <chr> "Entire home/apt", "Priv…
    ## $ accommodates                                 <dbl> 10, 2, 3, 1, 2, 5, 2, 4,…
    ## $ bathrooms                                    <lgl> NA, NA, NA, NA, NA, NA, …
    ## $ bathrooms_text                               <chr> "3 baths", "1.5 baths", …
    ## $ bedrooms                                     <dbl> 5, 1, 1, NA, 1, 2, NA, 3…
    ## $ beds                                         <dbl> 7, 1, 1, 1, 2, 2, 1, 3, …
    ## $ amenities                                    <chr> "[\"TV\", \"Washer\", \"…
    ## $ price                                        <chr> "$469.00", "$96.00", "$7…
    ## $ minimum_nights                               <dbl> 28, 180, 28, 365, 180, 3…
    ## $ maximum_nights                               <dbl> 730, 365, 28, 365, 365, …
    ## $ minimum_minimum_nights                       <dbl> 28, 180, 28, 365, 180, 3…
    ## $ maximum_minimum_nights                       <dbl> 28, 180, 28, 365, 180, 3…
    ## $ minimum_maximum_nights                       <dbl> 730, 365, 28, 365, 365, …
    ## $ maximum_maximum_nights                       <dbl> 730, 365, 28, 365, 365, …
    ## $ minimum_nights_avg_ntm                       <dbl> 28, 180, 28, 365, 180, 3…
    ## $ maximum_nights_avg_ntm                       <dbl> 730, 365, 28, 365, 365, …
    ## $ calendar_updated                             <lgl> NA, NA, NA, NA, NA, NA, …
    ## $ has_availability                             <lgl> TRUE, TRUE, TRUE, TRUE, …
    ## $ availability_30                              <dbl> 0, 30, 28, 11, 30, 11, 2…
    ## $ availability_60                              <dbl> 0, 60, 58, 41, 60, 41, 5…
    ## $ availability_90                              <dbl> 0, 90, 88, 71, 90, 71, 8…
    ## $ availability_365                             <dbl> 0, 365, 362, 346, 365, 3…
    ## $ calendar_last_scraped                        <date> 2021-02-09, 2021-02-09,…
    ## $ number_of_reviews                            <dbl> 7, 169, 217, 26, 1, 111,…
    ## $ number_of_reviews_ltm                        <dbl> 0, 0, 0, 0, 0, 2, 2, 0, …
    ## $ number_of_reviews_l30d                       <dbl> 0, 0, 0, 0, 0, 0, 1, 0, …
    ## $ first_review                                 <date> 2015-07-19, 2009-08-20,…
    ## $ last_review                                  <date> 2017-12-04, 2013-08-27,…
    ## $ review_scores_rating                         <dbl> 100, 97, 95, 98, 100, 92…
    ## $ review_scores_accuracy                       <dbl> 10, 10, 10, 10, NA, 9, 1…
    ## $ review_scores_cleanliness                    <dbl> 10, 10, 10, 10, NA, 9, 9…
    ## $ review_scores_checkin                        <dbl> 10, 10, 10, 10, NA, 10, …
    ## $ review_scores_communication                  <dbl> 10, 10, 10, 10, NA, 10, …
    ## $ review_scores_location                       <dbl> 10, 10, 9, 10, NA, 9, 9,…
    ## $ review_scores_value                          <dbl> 10, 10, 10, 10, NA, 9, 1…
    ## $ license                                      <chr> NA, NA, NA, NA, NA, NA, …
    ## $ instant_bookable                             <lgl> FALSE, TRUE, TRUE, FALSE…
    ## $ calculated_host_listings_count               <dbl> 1, 2, 2, 1, 2, 4, 2, 13,…
    ## $ calculated_host_listings_count_entire_homes  <dbl> 1, 1, 0, 1, 1, 4, 2, 13,…
    ## $ calculated_host_listings_count_private_rooms <dbl> 0, 1, 2, 0, 1, 0, 0, 0, …
    ## $ calculated_host_listings_count_shared_rooms  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ reviews_per_month                            <dbl> 0.10, 1.21, 1.66, 0.20, …

## Exploring Prices

We can see that the price column contains both dollar signs and commas
when the data was scraped. We are going to want to remove the dollar
signs and commas and convert these values to integers to be able to
graph these values.

``` r
glimpse(feb_21_airbnb$price)
```

    ##  chr [1:15832] "$469.00" "$96.00" "$72.00" "$45.00" "$128.00" "$100.00" ...

Now for cleaning the price column by removing all dollar signs and
commas and converting these values to integers.

``` r
feb_21_airbnb <- feb_21_airbnb %>% 
  mutate(price = str_remove(price, "\\$"),
         price = str_remove(price, ","),
         price = as.integer(price)
         )
glimpse(feb_21_airbnb$price)
```

    ##  int [1:15832] 469 96 72 45 128 100 70 113 93 101 ...

We can plot the Airbnb price per night data on the X-axis with number of
properties having that price on the Y-axis.

``` r
feb_21_airbnb %>%
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 20) +
  theme_classic() +
  labs(x = "February 2021 Price per night",
       y = "February 2021 number of properties")
```

![](Week-2_files/figure-markdown_github/unnamed-chunk-6-1.png)

There are definitely some outliers existing in price per night. We can
split the data and look at prices per night greater than $500 and less
than $500.

``` r
feb_21_airbnb %>%
  filter(price > 500) %>% 
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 10) +
  theme_classic() +
  labs(x = "Price per night greater than 500",
       y = "Number of properties")
```

![](Week-2_files/figure-markdown_github/unnamed-chunk-7-1.png)

We can see that there exists properties that are greater than $10,000 a
night.

We can can get a better view of these higher end prices by examining the
amount of properties greater than 10,000 a night.

``` r
feb_21_airbnb %>%
  filter(price > 10000) %>% 
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 10) +
  theme_classic() +
  labs(x = "Price per night greater than 10,000",
       y = "Number of properties")
```

![](Week-2_files/figure-markdown_github/unnamed-chunk-8-1.png)

It appears that the highest price in February 2021 is $13,000 a night,
and there are 3 properties that have this price.

Now we can look at properties less than $500.

``` r
feb_21_airbnb %>%
  filter(price < 500) %>% 
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 1) +
  theme_classic() +
  labs(x = "Price per night less than 500",
       y = "Number of properties")
```

![](Week-2_files/figure-markdown_github/unnamed-chunk-9-1.png)

We can see a majority of prices are less than $100.

We can take a further look at these values.

``` r
feb_21_airbnb %>%
  filter(price < 100) %>% 
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 1) +
  theme_classic() +
  labs(x = "Price per night less than 100",
       y = "Number of properties")
```

![](Week-2_files/figure-markdown_github/unnamed-chunk-10-1.png)

We can take a closer look at the lowest price Airbnbs, the properties
that are less than $25 per night.

``` r
feb_21_airbnb %>%
  filter(price < 25) %>% 
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 1) +
  theme_classic() +
  labs(x = "Price per night less than 25",
       y = "Number of properties")
```

![](Week-2_files/figure-markdown_github/unnamed-chunk-11-1.png)

## Property Type

We can get a better view of property types and the amount of properties
of each type.

``` r
ggplot(feb_21_airbnb, aes(property_type)) +
  geom_bar(binwidth = 100, stat = "count") + theme(text = element_text(size=10), axis.text.x = element_text(angle=90, hjust=1)) 
```

    ## Warning: Ignoring unknown parameters: binwidth

![](Week-2_files/figure-markdown_github/unnamed-chunk-12-1.png)

We can see from this visual that entire apartment and entire condominium
are the most represented Toronto Airbnb property types for February
2021.

## Nights able to stay:

We can also gain a view of the minimum amount of nights available to
book a stay at the Airbnb.

``` r
feb_21_airbnb %>%
  ggplot(aes(x = minimum_nights)) +
  geom_histogram(binwidth = 5) +
  theme_classic() +
  labs(x = "Minimum night booking availability",
       y = "Number of properties")
```

![](Week-2_files/figure-markdown_github/unnamed-chunk-13-1.png)

We can zoom in on properties that have a 100 night minimum booking or
less.

``` r
feb_21_airbnb %>%
  filter(minimum_nights < 100) %>% 
  ggplot(aes(x = minimum_nights)) +
  geom_histogram(binwidth = 1) +
  theme_classic() +
  labs(x = "Minimum night booking availability",
       y = "Number of properties")
```

![](Week-2_files/figure-markdown_github/unnamed-chunk-14-1.png)

We can see that the majority of properties have a minimum amount of
night booking of around 28. This makes sense as in April 2020, Ontario
had banned all short term rentals (a rental period of less than 28 days)
unless the unit is being rented to someone in need of housing during the
“Emergency Order”.

References:

(Rohan Alexander, 2021) Telling Stories With Data
<https://www.tellingstorieswithdata.com/exploratory-data-analysis.html#case-study---airbnb-listing-in-toronto>
