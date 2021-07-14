Linear Mixed Effects Model
================
Henry Shiffer

``` r
library(tidyverse) 
library(tidymodels) 
library(ggmap)
library(varhandle)
library(caret)
library(readr)
library(yardstick)
library(dplyr)
library(purrr)
library(stringr)
library(olsrr)
library(lme4)
```

We can first load the full Airbnb dataset that contains the attributes
of interest every month from February 2020 to April 2021.

``` r
#loading data
airbnb_data_full <- read_csv("full_airbnb_data_final.csv", guess_max = 20000)
airbnb_data_full <- subset(airbnb_data_full, select = c(-X1))
glimpse(airbnb_data_full)
```

    ## Rows: 7,824
    ## Columns: 73
    ## $ id                                           <dbl> 50110, 84151, 224775, 273…
    ## $ listing_url                                  <chr> "https://www.airbnb.com/r…
    ## $ scrape_id                                    <dbl> 2.020021e+13, 2.020021e+1…
    ## $ last_scraped                                 <dbl> 43875, 43875, 43875, 4387…
    ## $ name                                         <chr> "Yorkville one bedroom Co…
    ## $ description                                  <chr> "Fully Furnished 1-bedroo…
    ## $ neighborhood_overview                        <chr> "The neighbourhood is one…
    ## $ picture_url                                  <chr> "https://a0.muscache.com/…
    ## $ host_id                                      <dbl> 195095, 195095, 1170217, …
    ## $ host_url                                     <chr> "https://www.airbnb.com/u…
    ## $ host_name                                    <chr> "Urbano", "Urbano", "Fran…
    ## $ host_since                                   <dbl> 40398, 40398, 40802, 4086…
    ## $ host_location                                <chr> "Toronto, Ontario, Canada…
    ## $ host_about                                   <chr> "Boutique property manage…
    ## $ host_response_time                           <chr> "within an hour", "within…
    ## $ host_response_rate                           <dbl> 1, 1, 1, NA, NA, 1, 1, NA…
    ## $ host_acceptance_rate                         <chr> "0.92", "0.92", "0.91", "…
    ## $ host_is_superhost                            <lgl> FALSE, FALSE, FALSE, FALS…
    ## $ host_thumbnail_url                           <chr> "https://a0.muscache.com/…
    ## $ host_picture_url                             <chr> "https://a0.muscache.com/…
    ## $ host_neighbourhood                           <chr> "Rosedale", "Rosedale", "…
    ## $ host_listings_count                          <dbl> 13, 13, 2, 1, 1, 3, 1, 1,…
    ## $ host_total_listings_count                    <dbl> 13, 13, 2, 1, 1, 3, 1, 1,…
    ## $ host_verifications                           <chr> "['email', 'phone', 'revi…
    ## $ host_has_profile_pic                         <lgl> TRUE, TRUE, TRUE, TRUE, T…
    ## $ host_identity_verified                       <lgl> TRUE, TRUE, FALSE, TRUE, …
    ## $ neighbourhood                                <chr> "Downtown Toronto", "Down…
    ## $ neighbourhood_cleansed                       <chr> "Church-Yonge Corridor", …
    ## $ neighbourhood_group_cleansed                 <lgl> NA, NA, NA, NA, NA, NA, N…
    ## $ latitude                                     <dbl> 43.66899, 43.67033, 43.66…
    ## $ longitude                                    <dbl> -79.38548, -79.38612, -79…
    ## $ property_type                                <chr> "Apartment", "Apartment",…
    ## $ room_type                                    <chr> "Entire home/apt", "Entir…
    ## $ accommodates                                 <dbl> 3, 3, 2, 1, 1, 2, 4, 8, 4…
    ## $ bathrooms                                    <dbl> 1.0, 1.0, 1.0, 1.0, 1.0, …
    ## $ bedrooms                                     <dbl> 1, 1, 1, 1, 2, 1, 2, 4, 1…
    ## $ beds                                         <dbl> 1, 1, 1, 1, 2, 1, 2, 5, 2…
    ## $ amenities                                    <chr> "{TV,Internet,Wifi,\"Air …
    ## $ price                                        <dbl> 121, 144, 85, 176, 99, 90…
    ## $ minimum_nights                               <dbl> 1, 1, 7, 2, 3, 2, 3, 1, 1…
    ## $ maximum_nights                               <dbl> 365, 365, 162, 365, 1125,…
    ## $ minimum_minimum_nights                       <dbl> 1, 1, 7, 2, 3, 1, 3, 1, 1…
    ## $ maximum_minimum_nights                       <dbl> 1, 1, 7, 2, 3, 2, 4, 1, 1…
    ## $ minimum_maximum_nights                       <dbl> 365, 365, 162, 365, 1125,…
    ## $ maximum_maximum_nights                       <dbl> 365, 365, 162, 365, 1125,…
    ## $ minimum_nights_avg_ntm                       <dbl> 1.0, 1.0, 7.0, 2.0, 3.0, …
    ## $ maximum_nights_avg_ntm                       <dbl> 365, 365, 162, 365, 1125,…
    ## $ calendar_updated                             <chr> "a week ago", "today", "2…
    ## $ has_availability                             <lgl> TRUE, TRUE, TRUE, TRUE, T…
    ## $ availability_30                              <dbl> 0, 28, 14, 0, 29, 14, 21,…
    ## $ availability_60                              <dbl> 5, 58, 44, 0, 59, 33, 46,…
    ## $ availability_90                              <dbl> 30, 88, 74, 0, 89, 61, 69…
    ## $ availability_365                             <dbl> 292, 350, 349, 0, 364, 11…
    ## $ calendar_last_scraped                        <dbl> 43875, 43875, 43875, 4387…
    ## $ number_of_reviews                            <dbl> 55, 24, 26, 0, 28, 347, 2…
    ## $ number_of_reviews_ltm                        <dbl> 15, 0, 5, 0, 1, 81, 55, 0…
    ## $ first_review                                 <dbl> 40502, 40790, 42184, NA, …
    ## $ last_review                                  <dbl> 43761, 42918, 43757, NA, …
    ## $ review_scores_rating                         <dbl> 93, 89, 92, NA, 91, 100, …
    ## $ review_scores_accuracy                       <dbl> 10, 9, 10, NA, 9, 10, 10,…
    ## $ review_scores_cleanliness                    <dbl> 9, 8, 9, NA, 10, 10, 10, …
    ## $ review_scores_checkin                        <dbl> 10, 10, 10, NA, 10, 10, 1…
    ## $ review_scores_communication                  <dbl> 10, 10, 10, NA, 9, 10, 10…
    ## $ review_scores_location                       <dbl> 10, 10, 10, NA, 10, 10, 1…
    ## $ review_scores_value                          <dbl> 9, 9, 9, NA, 8, 10, 10, 1…
    ## $ license                                      <chr> NA, NA, NA, NA, NA, NA, N…
    ## $ instant_bookable                             <lgl> FALSE, FALSE, FALSE, FALS…
    ## $ calculated_host_listings_count               <dbl> 13, 13, 2, 1, 1, 3, 1, 2,…
    ## $ calculated_host_listings_count_entire_homes  <dbl> 10, 10, 0, 0, 0, 2, 1, 0,…
    ## $ calculated_host_listings_count_private_rooms <dbl> 3, 3, 2, 1, 1, 1, 0, 2, 1…
    ## $ calculated_host_listings_count_shared_rooms  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ reviews_per_month                            <dbl> 0.49, 0.23, 0.46, NA, 0.2…
    ## $ month_year                                   <chr> "February_2020", "Februar…

We can get rid of the variables in the data set that will not be passed
through the model.

``` r
#subsetting the data frame for the columns of intrest
airbnb_data_full <- subset.data.frame(airbnb_data_full, select = c("price", "bedrooms", "bathrooms", "reviews_per_month", "host_is_superhost", "host_response_rate", "review_scores_cleanliness", "host_id", "neighbourhood_cleansed", "month_year"))

#ommiting na cases from the data
airbnb_data_full_na <- na.omit(airbnb_data_full)
```

We will explore fitting a linear mixed effects model to the data. We
will pick a ;linear mixed effects model because we know that
observations are correlated in different cluster levels within the data
and ignoring these correlations may present issues.

The first cluster would be the month and year the listings exist in. All
listings within that certain month and year category will be similar in
the sense that the were listed on the Airbnb website in that specific
month and year. Considering COVID-19 developed over time, along with the
measures taken to combat the virus, this listing information is
time-sensitive and can vary month to month. Due to fluctuating Ontario
lockdown and rental housing measures implemented, Listings within a
specific month should be more correlated to other listings in that month
than they are to listings in different months.

The second cluster would be neighbourhoods. The neighbourhoods of
interest for this study are the neighbourhoods surrounding the
University of Toronto. These neighbourhoods being Kensington-Chinatown,
Annex, University, Church-Yonge Corridor and Bay Street Corridor.
Listings within each neighbourhood are going to be more similar to
each-other than they are to lisitings in other neighbourhoods. Part of
this is due to Toronto real-estate development and city planning. For
example, if an area consists of primarily office buildings, the listings
in that area will be more similar to eachother than they would be to an
area that consists of primarily single family housing and schools.

The third cluster would be hosts. There is great variation between
different hosts that have their listings on Airbnb. Some hosts have
multiple listings throughout Toronto, other hosts have only one listing.
As there are sometimes multiple observations listed for a single host,
the model must account for this correlation within the listings a host
has posted and the variation between different hosts on Airbnb.

Taking the structure of the data under consideration, we can define a
mixed effects model of Airbnb quality predicting price in R with the
variables:

> bedrooms

> bathrooms

> neighbourhood\_cleansed

> month\_year

> reviews\_per\_month

> host\_is\_superhost

> host\_response\_rate

> review\_scores\_cleanliness

> host\_id

``` r
pricelmm <- lmer(price ~ bedrooms +
                   bathrooms +
                   reviews_per_month +
                   host_is_superhost +
                   host_response_rate +
                   review_scores_cleanliness +
                   (1|host_id) +
                   (1|neighbourhood_cleansed) +
                   (1|month_year), data = airbnb_data_full_na)
summary(pricelmm)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: 
    ## price ~ bedrooms + bathrooms + reviews_per_month + host_is_superhost +  
    ##     host_response_rate + review_scores_cleanliness + (1 | host_id) +  
    ##     (1 | neighbourhood_cleansed) + (1 | month_year)
    ##    Data: airbnb_data_full_na
    ## 
    ## REML criterion at convergence: 45842.7
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.9665 -0.1363 -0.0444  0.0444 25.1739 
    ## 
    ## Random effects:
    ##  Groups                 Name        Variance  Std.Dev.
    ##  host_id                (Intercept)  16369.35 127.943 
    ##  month_year             (Intercept)  23782.31 154.215 
    ##  neighbourhood_cleansed (Intercept)     90.47   9.512 
    ##  Residual                           114665.47 338.623 
    ## Number of obs: 3141, groups:  
    ## host_id, 1054; month_year, 15; neighbourhood_cleansed, 5
    ## 
    ## Fixed effects:
    ##                           Estimate Std. Error t value
    ## (Intercept)                54.7506    84.9993   0.644
    ## bedrooms                   52.4184    11.1869   4.686
    ## bathrooms                  57.9864    16.6438   3.484
    ## reviews_per_month          -7.5541     3.9259  -1.924
    ## host_is_superhostTRUE      -6.5621    16.0278  -0.409
    ## host_response_rate         -0.5473     0.3888  -1.408
    ## review_scores_cleanliness   5.5903     7.4901   0.746
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) bedrms bthrms rvws__ h__TRU hst_r_
    ## bedrooms    -0.026                                   
    ## bathrooms   -0.137 -0.537                            
    ## rvws_pr_mnt -0.017  0.006  0.009                     
    ## hst_s_sTRUE  0.175 -0.048  0.047 -0.147              
    ## hst_rspns_r -0.285 -0.058  0.047 -0.060 -0.112       
    ## rvw_scrs_cl -0.769  0.014 -0.041 -0.048 -0.248 -0.075

``` r
confint(pricelmm)
```

    ## Computing profile confidence intervals ...

    ## Warning in FUN(X[[i]], ...): non-monotonic profile for .sig02

    ## Warning in confint.thpr(pp, level = level, zeta = zeta): bad spline fit
    ## for .sig02: falling back to linear interpolation

    ##                                 2.5 %      97.5 %
    ## .sig01                     112.398217 143.4237339
    ## .sig02                       0.000000 247.9550450
    ## .sig03                       0.000000  45.1659901
    ## .sigma                     329.493385 347.8423101
    ## (Intercept)               -111.885126 219.8489049
    ## bedrooms                    30.321704  74.4420410
    ## bathrooms                   25.292172  90.6731218
    ## reviews_per_month          -15.326493   0.1226505
    ## host_is_superhostTRUE      -38.030137  24.8608454
    ## host_response_rate          -1.300666   0.2220016
    ## review_scores_cleanliness   -9.116429  20.3902435

We can see that bedroom and bathroom amounts are significant in
predicting price as their confidence intervals do not contain 0. This
suggests that when taking into account the variation between months,
neighbourhoods and hosts, bedroom and bathroom amount are significant in
predicting price.
