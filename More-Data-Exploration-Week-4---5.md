More Data Exploration
================
Henry Shiffer

# Continued Data Exploration and Initial Modelling:

``` r
library(tidyverse) 
library(tidymodels) 
library(ggmap)
library(varhandle)
library(caret)
library(readr)
library(yardstick)
```

In this section will continue exploring Toronto’s February 2021 Airbnb
data and build some initial models.

Loading Toronto’s February 2021 Airbnb data into R by:

``` r
feb_21_airbnb <- read_csv("AirbnbFebruary2021.csv", guess_max = 20000)
glimpse(feb_21_airbnb)
```

    ## Rows: 15,832
    ## Columns: 74
    ## $ id                                           <dbl> 1419, 8077, 23691, 27423,…
    ## $ listing_url                                  <chr> "https://www.airbnb.com/r…
    ## $ scrape_id                                    <dbl> 2.021021e+13, 2.021021e+1…
    ## $ last_scraped                                 <date> 2021-02-09, 2021-02-09, …
    ## $ name                                         <chr> "Beautiful home in amazin…
    ## $ description                                  <chr> "This large, family home …
    ## $ neighborhood_overview                        <chr> "The apartment is located…
    ## $ picture_url                                  <chr> "https://a0.muscache.com/…
    ## $ host_id                                      <dbl> 1565, 22795, 93825, 11812…
    ## $ host_url                                     <chr> "https://www.airbnb.com/u…
    ## $ host_name                                    <chr> "Alexandra", "Kathie & La…
    ## $ host_since                                   <date> 2008-08-08, 2009-06-22, …
    ## $ host_location                                <chr> "Vancouver, British Colum…
    ## $ host_about                                   <chr> "I live in Vancouver, Can…
    ## $ host_response_time                           <chr> "N/A", "N/A", "N/A", "N/A…
    ## $ host_response_rate                           <chr> "N/A", "N/A", "N/A", "N/A…
    ## $ host_acceptance_rate                         <chr> "N/A", "N/A", "N/A", "N/A…
    ## $ host_is_superhost                            <lgl> FALSE, FALSE, FALSE, FALS…
    ## $ host_thumbnail_url                           <chr> "https://a0.muscache.com/…
    ## $ host_picture_url                             <chr> "https://a0.muscache.com/…
    ## $ host_neighbourhood                           <chr> "Commercial Drive", "Harb…
    ## $ host_listings_count                          <dbl> 1, 2, 2, 1, 2, 3, 2, 13, …
    ## $ host_total_listings_count                    <dbl> 1, 2, 2, 1, 2, 3, 2, 13, …
    ## $ host_verifications                           <chr> "['email', 'phone', 'revi…
    ## $ host_has_profile_pic                         <lgl> TRUE, TRUE, TRUE, TRUE, T…
    ## $ host_identity_verified                       <lgl> TRUE, FALSE, TRUE, TRUE, …
    ## $ neighbourhood                                <chr> "Toronto, Ontario, Canada…
    ## $ neighbourhood_cleansed                       <chr> "Little Portugal", "Water…
    ## $ neighbourhood_group_cleansed                 <lgl> NA, NA, NA, NA, NA, NA, N…
    ## $ latitude                                     <dbl> 43.64617, 43.64105, 43.69…
    ## $ longitude                                    <dbl> -79.42451, -79.37628, -79…
    ## $ property_type                                <chr> "Entire house", "Private …
    ## $ room_type                                    <chr> "Entire home/apt", "Priva…
    ## $ accommodates                                 <dbl> 10, 2, 3, 1, 2, 5, 2, 4, …
    ## $ bathrooms                                    <lgl> NA, NA, NA, NA, NA, NA, N…
    ## $ bathrooms_text                               <chr> "3 baths", "1.5 baths", "…
    ## $ bedrooms                                     <dbl> 5, 1, 1, NA, 1, 2, NA, 3,…
    ## $ beds                                         <dbl> 7, 1, 1, 1, 2, 2, 1, 3, 2…
    ## $ amenities                                    <chr> "[\"TV\", \"Washer\", \"H…
    ## $ price                                        <chr> "$469.00", "$96.00", "$72…
    ## $ minimum_nights                               <dbl> 28, 180, 28, 365, 180, 30…
    ## $ maximum_nights                               <dbl> 730, 365, 28, 365, 365, 3…
    ## $ minimum_minimum_nights                       <dbl> 28, 180, 28, 365, 180, 30…
    ## $ maximum_minimum_nights                       <dbl> 28, 180, 28, 365, 180, 30…
    ## $ minimum_maximum_nights                       <dbl> 730, 365, 28, 365, 365, 1…
    ## $ maximum_maximum_nights                       <dbl> 730, 365, 28, 365, 365, 1…
    ## $ minimum_nights_avg_ntm                       <dbl> 28, 180, 28, 365, 180, 30…
    ## $ maximum_nights_avg_ntm                       <dbl> 730, 365, 28, 365, 365, 1…
    ## $ calendar_updated                             <lgl> NA, NA, NA, NA, NA, NA, N…
    ## $ has_availability                             <lgl> TRUE, TRUE, TRUE, TRUE, T…
    ## $ availability_30                              <dbl> 0, 30, 28, 11, 30, 11, 23…
    ## $ availability_60                              <dbl> 0, 60, 58, 41, 60, 41, 53…
    ## $ availability_90                              <dbl> 0, 90, 88, 71, 90, 71, 83…
    ## $ availability_365                             <dbl> 0, 365, 362, 346, 365, 34…
    ## $ calendar_last_scraped                        <date> 2021-02-09, 2021-02-09, …
    ## $ number_of_reviews                            <dbl> 7, 169, 217, 26, 1, 111, …
    ## $ number_of_reviews_ltm                        <dbl> 0, 0, 0, 0, 0, 2, 2, 0, 1…
    ## $ number_of_reviews_l30d                       <dbl> 0, 0, 0, 0, 0, 0, 1, 0, 0…
    ## $ first_review                                 <date> 2015-07-19, 2009-08-20, …
    ## $ last_review                                  <date> 2017-12-04, 2013-08-27, …
    ## $ review_scores_rating                         <dbl> 100, 97, 95, 98, 100, 92,…
    ## $ review_scores_accuracy                       <dbl> 10, 10, 10, 10, NA, 9, 10…
    ## $ review_scores_cleanliness                    <dbl> 10, 10, 10, 10, NA, 9, 9,…
    ## $ review_scores_checkin                        <dbl> 10, 10, 10, 10, NA, 10, 1…
    ## $ review_scores_communication                  <dbl> 10, 10, 10, 10, NA, 10, 1…
    ## $ review_scores_location                       <dbl> 10, 10, 9, 10, NA, 9, 9, …
    ## $ review_scores_value                          <dbl> 10, 10, 10, 10, NA, 9, 10…
    ## $ license                                      <chr> NA, NA, NA, NA, NA, NA, N…
    ## $ instant_bookable                             <lgl> FALSE, TRUE, TRUE, FALSE,…
    ## $ calculated_host_listings_count               <dbl> 1, 2, 2, 1, 2, 4, 2, 13, …
    ## $ calculated_host_listings_count_entire_homes  <dbl> 1, 1, 0, 1, 1, 4, 2, 13, …
    ## $ calculated_host_listings_count_private_rooms <dbl> 0, 1, 2, 0, 1, 0, 0, 0, 1…
    ## $ calculated_host_listings_count_shared_rooms  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ reviews_per_month                            <dbl> 0.10, 1.21, 1.66, 0.20, 0…

``` r
#getting rid of the $ and , in price
feb_21_airbnb <- feb_21_airbnb %>% 
  mutate(price = str_remove(price, "\\$"),
         price = str_remove(price, ","),
         price = as.integer(price)
         )
```

A note: The Airbnb data is being called locally and is not on Github.

## Review Scores Map:

We can gain a view of where in Toronto has the highest and lowest Airbnb
review score ratings. To do this the NA values must be filtered out
before these values can be mapped.

``` r
#getting rid of NA values for review_scores_rating in the original data-set
rsr_filtered_airbnb <- feb_21_airbnb %>%
  filter(!is.na(review_scores_rating))

#scale_colour_distiller(palette = "RdYlBu")
#mapping the NA value filtered review_scores_ratings values
rsr_filtered_borders <- c(bottom  = min(rsr_filtered_airbnb$latitude), 
                top = max(rsr_filtered_airbnb$latitude),
                left = min(rsr_filtered_airbnb$longitude),
                right = max(rsr_filtered_airbnb$longitude))
map1 <- get_stamenmap(rsr_filtered_borders, maptype = "toner-lite")
ggmap(map1) + geom_point(data = rsr_filtered_airbnb, mapping = aes(x = longitude, y = latitude, 
                                               col = review_scores_rating)) +
  scale_colour_distiller(palette = "RdYlBu") +
  geom_jitter(alpha = 0.4) + ggtitle("Feb 2021 Review Scores")
```

    ## Warning: Removed 3 rows containing missing values (geom_point).

![](More-Data-Exploration-Week-4---5_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## Superhosts Map:

A map of superhosts would also be interesting. From the Airbnb website
(<https://www.airbnb.ca/help/article/829/how-do-i-become-a-superhost>),
a superhost is defined as a host who has met all of the following
requirements:

> “Completed at least 10 trips OR completed 3 reservations that total at
> least 100 nights”

> “Maintained a 90% response rate or higher”

> “Maintained a 1% percent cancellation rate (1 cancellation per 100
> reservations)”

> “Maintained a 4.8 overall rating (this rating looks at the past 365
> days of reviews, based on the date the guest left a review, not the
> date the guest checked out)”

If these listings have hosts that are superhosts, the listings have a
better chance of being better maintained and of higher quality to stay
in.

``` r
#getting rid of NA values for host_is_superhost in the original data-set
super_filtered_airbnb <- feb_21_airbnb %>%
  filter(!is.na(host_is_superhost))

#mapping the NA value filtered host_is_superhost values
super_filtered_borders <- c(bottom  = min(super_filtered_airbnb$latitude), 
                top = max(super_filtered_airbnb$latitude),
                left = min(super_filtered_airbnb$longitude),
                right = max(super_filtered_airbnb$longitude))
map2 <- get_stamenmap(super_filtered_borders, maptype = "toner-lite")
ggmap(map2) + geom_point(data = super_filtered_airbnb, mapping = aes(x = longitude, y = latitude, 
                                               col = host_is_superhost)) + 
  geom_jitter(alpha = 0.4) + ggtitle("Feb 2021 Superhosts")
```

    ## Warning: Removed 4 rows containing missing values (geom_point).

![](More-Data-Exploration-Week-4---5_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

We can see that there does exists a good amount of superhost listings,
but ultimately it looks like there is more hosts that do not have
superhost status.

## Bedrooms Map:

To also get a sense of listing quality, we can plot bedroom amount on
the map.

``` r
#getting rid of NA values for host_is_superhost in the original data-set
bedrooms_filtered_airbnb <- feb_21_airbnb %>%
  filter(!is.na(bedrooms))

#mapping the NA value filtered host_is_superhost values
bedrooms_filtered_borders <- c(bottom  = min(bedrooms_filtered_airbnb$latitude), 
                top = max(bedrooms_filtered_airbnb$latitude),
                left = min(bedrooms_filtered_airbnb$longitude),
                right = max(bedrooms_filtered_airbnb$longitude))
map3 <- get_stamenmap(bedrooms_filtered_borders, maptype = "toner-lite")
ggmap(map3) + geom_point(data = bedrooms_filtered_airbnb, mapping = aes(x = longitude, y = latitude, 
                                               col = bedrooms)) +
  scale_colour_distiller(palette = "RdYlBu") +
  geom_jitter(alpha = 0.4) +
  ggtitle("Feb 2021 Bedroom Amount")
```

    ## Warning: Removed 3 rows containing missing values (geom_point).

![](More-Data-Exploration-Week-4---5_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

We can see an overwhelming amount of listings have less than \~5
bedrooms. This makes sense. Bedroom space is very hard to come by in the
city, and these listings reflect that.

## Cleanliness Map:

Regardless of the pandemic, cleanliness is a good indicator of Airbnb
listing quality. If a listing has received low review scores for
cleanliness, it is unlikely anyone will feel comfortable living in that
property.

We can gain a view of Toronto Airbnb listing cleanliness on a map of the
city.

``` r
#getting rid of NA values for review_scores_cleanliness in the original data-set
clean_filtered_airbnb <- feb_21_airbnb %>%
  filter(!is.na(review_scores_cleanliness))

#mapping the NA value filtered out review_scores_cleanliness values
clean_filtered_borders <- c(bottom  = min(clean_filtered_airbnb$latitude), 
                top = max(clean_filtered_airbnb$latitude),
                left = min(clean_filtered_airbnb$longitude),
                right = max(clean_filtered_airbnb$longitude))
map4 <- get_stamenmap(clean_filtered_borders, maptype = "toner-lite")
ggmap(map4) + geom_point(data = clean_filtered_airbnb, mapping = aes(x = longitude, y = latitude, 
                                               col = review_scores_cleanliness)) +
  scale_colour_distiller(palette = "RdYlBu") +
  geom_jitter(alpha = 0.4) +
  ggtitle("Feb 2021 Cleanliness Ratings")
```

    ## Warning: Removed 4 rows containing missing values (geom_point).

![](More-Data-Exploration-Week-4---5_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

We can see there is definitely many listings that have review scores for
cleanliness that are 6 and below. These listings are likely not very
well maintained and are possibly used just as a short-term rental
property without a host living there.

I would not recommend any listings that are below a cleanliness rating
of 6 to anyone, especially during a pandemic.

## Initial Linear Modelling of Feburary 2021 Airbnb data:

We can start fitting simple linear models with the variables we are
interested in.

### Cleaning variables before modelling:

We can see that bathroom amount is categorized as a string. For analysis
of bathroom amount, it would make sense to extract the numeric value in
the string.

``` r
glimpse(feb_21_airbnb$bathrooms_text)
```

    ##  chr [1:15832] "3 baths" "1.5 baths" "1 shared bath" "1 bath" "1.5 baths" ...

``` r
#parse_number() drops any non-numeric characters
feb_21_airbnb$bathrooms_numeric <- parse_number(feb_21_airbnb$bathrooms_text)
```

We must also make sure that price is an integer and not a character.

``` r
#stripping away $ and , from price
feb_21_airbnb <- feb_21_airbnb %>% 
  mutate(price = str_remove(price, "\\$"),
         price = str_remove(price, ","),
         price = as.integer(price)
         )

#removing NAs from price
feb_21_airbnb <- feb_21_airbnb %>%
  filter(!is.na(price))
```

We can see that host response rates are coded as characters. We can code
them as integers instead.

``` r
glimpse(feb_21_airbnb$host_response_rate)
```

    ##  chr [1:15832] "N/A" "N/A" "N/A" "N/A" "N/A" "100%" "100%" "100%" "N/A" ...

``` r
#stripping away % and from host_response_rate and converting to integer
feb_21_airbnb <- feb_21_airbnb %>% 
  mutate(host_response_rate = str_remove(host_response_rate, "\\%"),
         host_response_rate = as.integer(host_response_rate)
         )
```

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

### Test set and train set:

We can now split the data into a test set and training set to test the
accuracy of our model in prediction later on.

``` r
set.seed(496)

#splitting the data
feb_21_airbnb_split <- 
  feb_21_airbnb %>%
  initial_split(prop = 3/4)

#creating the test and train set
feb_21_airbnb_train <- training(feb_21_airbnb_split)
feb_21_airbnb_test <- testing(feb_21_airbnb_split)
```

### Modelling price with bedrooms:

``` r
#removing NAs from bedrooms
feb_21_airbnb_train <- feb_21_airbnb_train %>%
  filter(!is.na(bedrooms))

#price ~ bedrooms model
price_and_bedrooms <- lm(price ~ bedrooms, data = feb_21_airbnb_train)
summary(price_and_bedrooms)
```

    ## 
    ## Call:
    ## lm(formula = price ~ bedrooms, data = feb_21_airbnb_train)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ##  -883.6   -63.3   -31.8    11.2 12896.7 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   18.841      6.077    3.10  0.00194 ** 
    ## bedrooms      84.477      3.684   22.93  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 315.1 on 10988 degrees of freedom
    ## Multiple R-squared:  0.04566,    Adjusted R-squared:  0.04557 
    ## F-statistic: 525.7 on 1 and 10988 DF,  p-value: < 2.2e-16

We can see that the p-value for the bedrooms covariate is &lt; 2e-16
when that variable is the only predictor.

### Modelling price with bathrooms:

``` r
#removing any possible NAs still in bathrooms_numeric 
feb_21_airbnb_train <- feb_21_airbnb_train %>%
  filter(!is.na(bathrooms_numeric))

#price ~ bathrooms model
price_and_bathrooms <- lm(price ~ bathrooms_numeric, data = feb_21_airbnb_train)
summary(price_and_bathrooms)
```

    ## 
    ## Call:
    ## lm(formula = price ~ bathrooms_numeric, data = feb_21_airbnb_train)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ##  -1084    -64    -31     14  12889 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          8.943      7.324   1.221    0.222    
    ## bathrooms_numeric  102.029      5.202  19.613   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 316.6 on 10958 degrees of freedom
    ## Multiple R-squared:  0.03391,    Adjusted R-squared:  0.03383 
    ## F-statistic: 384.7 on 1 and 10958 DF,  p-value: < 2.2e-16

We can see that the p-value for the bathrooms covariate is &lt; 2e-16
when that variable is the only predictor.

### Modelling price with neighbourhood:

``` r
#removing any possible NAs in neighbourhood_cleansed
feb_21_airbnb_train <- feb_21_airbnb_train %>%
  filter(!is.na(neighbourhood_cleansed))

#price ~ neighbourhood model
price_and_neighbourhood <- lm(price ~ neighbourhood_cleansed, data = feb_21_airbnb_train)
summary(price_and_neighbourhood)
```

    ## 
    ## Call:
    ## lm(formula = price ~ neighbourhood_cleansed, data = feb_21_airbnb_train)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ##  -661.0   -71.3   -34.3     8.4 12804.0 
    ## 
    ## Coefficients:
    ##                                                             Estimate Std. Error
    ## (Intercept)                                                91.235294  77.663554
    ## neighbourhood_cleansedAgincourt South-Malvern West        -33.602941  86.830493
    ## neighbourhood_cleansedAlderwood                            -5.335294 105.633854
    ## neighbourhood_cleansedAnnex                               117.470227  79.662791
    ## neighbourhood_cleansedBanbury-Don Mills                    26.223039 101.508770
    ## neighbourhood_cleansedBathurst Manor                      134.764706  97.208795
    ## neighbourhood_cleansedBay Street Corridor                  57.412732  79.805530
    ## neighbourhood_cleansedBayview Village                      26.598039  87.980616
    ## neighbourhood_cleansedBayview Woods-Steeles               102.314706  92.709597
    ## neighbourhood_cleansedBedford Park-Nortown                 14.064706  89.902063
    ## neighbourhood_cleansedBeechborough-Greenbrook              34.764706 127.614241
    ## neighbourhood_cleansedBendale                              12.359300  93.823856
    ## neighbourhood_cleansedBirchcliffe-Cliffside                -3.701961  91.160531
    ## neighbourhood_cleansedBlack Creek                         -41.797794 111.535787
    ## neighbourhood_cleansedBlake-Jones                          14.396285  93.434401
    ## neighbourhood_cleansedBriar Hill-Belgravia                -31.127186  93.823856
    ## neighbourhood_cleansedBridle Path-Sunnybrook-York Mills   108.647059 109.832851
    ## neighbourhood_cleansedBroadview North                      81.069054 102.419662
    ## neighbourhood_cleansedBrookhaven-Amesbury                 -26.335294 127.614241
    ## neighbourhood_cleansedCabbagetown-South St.James Town      75.747315  83.206126
    ## neighbourhood_cleansedCaledonia-Fairbank                  -41.083779  95.597226
    ## neighbourhood_cleansedCasa Loma                           103.440382  93.823856
    ## neighbourhood_cleansedCentennial Scarborough               21.064706 127.614241
    ## neighbourhood_cleansedChurch-Yonge Corridor                41.498259  79.805530
    ## neighbourhood_cleansedClairlea-Birchmount                   0.787433  91.444131
    ## neighbourhood_cleansedClanton Park                        -26.952685  90.888433
    ## neighbourhood_cleansedCliffcrest                           13.098039 104.471880
    ## neighbourhood_cleansedCorso Italia-Davenport              -20.220588  86.830493
    ## neighbourhood_cleansedDanforth                             26.253078  91.739983
    ## neighbourhood_cleansedDanforth East York                   52.521463  93.823856
    ## neighbourhood_cleansedDon Valley Village                   13.025575  86.704560
    ## neighbourhood_cleansedDorset Park                         -17.630031  93.434401
    ## neighbourhood_cleansedDovercourt-Wallace Emerson-Junction   9.195536  80.230386
    ## neighbourhood_cleansedDownsview-Roding-CFB                -13.647059  95.118039
    ## neighbourhood_cleansedDufferin Grove                       22.960784  83.886226
    ## neighbourhood_cleansedEast End-Danforth                    16.297173  85.809589
    ## neighbourhood_cleansedEdenbridge-Humber Valley             97.514706 120.732901
    ## neighbourhood_cleansedEglinton East                       -32.409207 102.419662
    ## neighbourhood_cleansedElms-Old Rexdale                     -4.568627 132.002659
    ## neighbourhood_cleansedEnglemount-Lawrence                  14.550420  98.456524
    ## neighbourhood_cleansedEringate-Centennial-West Deane       14.635674  96.640024
    ## neighbourhood_cleansedEtobicoke West Mall                  55.448916 106.903447
    ## neighbourhood_cleansedFlemingdon Park                       0.264706 111.535787
    ## neighbourhood_cleansedForest Hill North                   -10.568627 113.434881
    ## neighbourhood_cleansedForest Hill South                   146.882353 109.832851
    ## neighbourhood_cleansedGlenfield-Jane Heights              -38.672794 111.535787
    ## neighbourhood_cleansedGreenwood-Coxwell                    31.349121  85.809589
    ## neighbourhood_cleansedGuildwood                           -17.860294 137.291064
    ## neighbourhood_cleansedHenry Farm                           88.764706 100.663471
    ## neighbourhood_cleansedHigh Park North                      27.435160  84.834123
    ## neighbourhood_cleansedHigh Park-Swansea                    48.246849  83.349520
    ## neighbourhood_cleansedHighland Creek                       -5.485294  96.103745
    ## neighbourhood_cleansedHillcrest Village                   -17.235294  91.160531
    ## neighbourhood_cleansedHumber Heights-Westmount            -12.457516 132.002659
    ## neighbourhood_cleansedHumber Summit                       -21.508021 123.908167
    ## neighbourhood_cleansedHumbermede                          -20.326203 123.908167
    ## neighbourhood_cleansedHumewood-Cedarvale                   39.653595  89.053205
    ## neighbourhood_cleansedIonview                             -22.041746  96.640024
    ## neighbourhood_cleansedIslington-City Centre West           -0.605664  83.552681
    ## neighbourhood_cleansedJunction Area                         9.853313  85.612924
    ## neighbourhood_cleansedKeelesdale-Eglinton West            -25.140056 104.471880
    ## neighbourhood_cleansedKennedy Park                         -4.161220  99.142922
    ## neighbourhood_cleansedKensington-Chinatown                 27.926611  79.731700
    ## neighbourhood_cleansedKingsview Village-The Westway         9.117647 109.832851
    ## neighbourhood_cleansedKingsway South                      121.098039 152.056697
    ## neighbourhood_cleansedL'Amoreaux                          -22.087753  87.821256
    ## neighbourhood_cleansedLambton Baby Point                   46.336134 115.567066
    ## neighbourhood_cleansedLansing-Westgate                      6.073348  85.425551
    ## neighbourhood_cleansedLawrence Park North                  85.450420  94.664012
    ## neighbourhood_cleansedLawrence Park South                  76.420956  96.103745
    ## neighbourhood_cleansedLeaside-Bennington                  102.170956  96.103745
    ## neighbourhood_cleansedLittle Portugal                      51.931373  80.745911
    ## neighbourhood_cleansedLong Branch                          30.952206  96.103745
    ## neighbourhood_cleansedMalvern                             -42.016116  86.233713
    ## neighbourhood_cleansedMaple Leaf                           21.514706 120.732901
    ## neighbourhood_cleansedMarkland Wood                        88.431373 132.002659
    ## neighbourhood_cleansedMilliken                             -3.651961 120.732901
    ## neighbourhood_cleansedMimico (includes Humber Bay Shores)  39.379521  82.408530
    ## neighbourhood_cleansedMorningside                          -2.901961 108.296652
    ## neighbourhood_cleansedMoss Park                            60.507897  80.191065
    ## neighbourhood_cleansedMount Dennis                         -6.705882 109.832851
    ## neighbourhood_cleansedMount Olive-Silverstone-Jamestown   -23.306723 115.567066
    ## neighbourhood_cleansedMount Pleasant East                  47.615770  90.627149
    ## neighbourhood_cleansedMount Pleasant West                  18.642568  82.549124
    ## neighbourhood_cleansedNew Toronto                          35.564706  94.664012
    ## neighbourhood_cleansedNewtonbrook East                      0.003342  84.834123
    ## neighbourhood_cleansedNewtonbrook West                    104.789499  82.940016
    ## neighbourhood_cleansedNiagara                              89.865855  79.166573
    ## neighbourhood_cleansedNorth Riverdale                      54.080495  88.490305
    ## neighbourhood_cleansedNorth St.James Town                  17.339174  84.394632
    ## neighbourhood_cleansedO'Connor-Parkview                   -19.337858  93.063412
    ## neighbourhood_cleansedOakridge                             14.419878  97.813134
    ## neighbourhood_cleansedOakwood Village                      -1.327399  85.911626
    ## neighbourhood_cleansedOld East York                        -9.701961  97.208795
    ## neighbourhood_cleansedPalmerston-Little Italy              41.989305  81.116936
    ## neighbourhood_cleansedParkwoods-Donalda                    -8.147575  88.490305
    ## neighbourhood_cleansedPelmo Park-Humberlea                 25.173797 103.404210
    ## neighbourhood_cleansedPlayter Estates-Danforth             30.119545  96.640024
    ## neighbourhood_cleansedPleasant View                       -32.511156  88.314882
    ## neighbourhood_cleansedPrincess-Rosethorn                    8.264706 108.296652
    ## neighbourhood_cleansedRegent Park                          63.389706  88.671627
    ## neighbourhood_cleansedRexdale-Kipling                     -17.385294 105.633854
    ## neighbourhood_cleansedRockcliffe-Smythe                    19.693277  98.456524
    ## neighbourhood_cleansedRoncesvalles                         49.255934  83.253113
    ## neighbourhood_cleansedRosedale-Moore Park                 121.525900  86.959994
    ## neighbourhood_cleansedRouge                                 1.310160 103.404210
    ## neighbourhood_cleansedRunnymede-Bloor West Village         27.414706 105.633854
    ## neighbourhood_cleansedRustic                              599.764706 200.526433
    ## neighbourhood_cleansedScarborough Village                   9.407563 115.567066
    ## neighbourhood_cleansedSouth Parkdale                       12.568503  81.734938
    ## neighbourhood_cleansedSouth Riverdale                      61.440896  80.745911
    ## neighbourhood_cleansedSt.Andrew-Windfields                 91.598039  97.208795
    ## neighbourhood_cleansedSteeles                             -38.910970  93.823856
    ## neighbourhood_cleansedStonegate-Queensway                  19.320261  87.516911
    ## neighbourhood_cleansedTam O'Shanter-Sullivan              -17.485294  90.376042
    ## neighbourhood_cleansedTaylor-Massey                         3.264706  98.456524
    ## neighbourhood_cleansedThe Beaches                          60.886928  84.681373
    ## neighbourhood_cleansedThistletown-Beaumond Heights         18.264706 152.056697
    ## neighbourhood_cleansedThorncliffe Park                    -18.445820 106.903447
    ## neighbourhood_cleansedTrinity-Bellwoods                    40.464340  80.045134
    ## neighbourhood_cleansedUniversity                           44.219930  82.442914
    ## neighbourhood_cleansedVictoria Village                     -2.187675 104.471880
    ## neighbourhood_cleansedWaterfront Communities-The Island   110.094354  77.994577
    ## neighbourhood_cleansedWest Hill                           -27.332068  96.640024
    ## neighbourhood_cleansedWest Humber-Clairville              -26.735294  87.371498
    ## neighbourhood_cleansedWestminster-Branson                 -21.945820  93.434401
    ## neighbourhood_cleansedWeston                               34.176471  95.118039
    ## neighbourhood_cleansedWeston-Pellam Park                   -7.025992  91.739983
    ## neighbourhood_cleansedWexford/Maryvale                     -2.703036  87.666761
    ## neighbourhood_cleansedWillowdale East                      17.113240  79.784876
    ## neighbourhood_cleansedWillowdale West                      15.089706  85.518118
    ## neighbourhood_cleansedWillowridge-Martingrove-Richview     -5.998452  93.434401
    ## neighbourhood_cleansedWoburn                              326.039706  92.709597
    ## neighbourhood_cleansedWoodbine Corridor                    47.984706  89.902063
    ## neighbourhood_cleansedWoodbine-Lumsden                     23.673797 103.404210
    ## neighbourhood_cleansedWychwood                             21.538899  87.666761
    ## neighbourhood_cleansedYonge-Eglinton                       42.744298  90.134528
    ## neighbourhood_cleansedYonge-St.Clair                       88.250420  94.664012
    ## neighbourhood_cleansedYork University Heights             -35.483157  83.114487
    ## neighbourhood_cleansedYorkdale-Glen Park                   13.696213  86.233713
    ##                                                           t value Pr(>|t|)    
    ## (Intercept)                                                 1.175 0.240120    
    ## neighbourhood_cleansedAgincourt South-Malvern West         -0.387 0.698768    
    ## neighbourhood_cleansedAlderwood                            -0.051 0.959719    
    ## neighbourhood_cleansedAnnex                                 1.475 0.140351    
    ## neighbourhood_cleansedBanbury-Don Mills                     0.258 0.796155    
    ## neighbourhood_cleansedBathurst Manor                        1.386 0.165671    
    ## neighbourhood_cleansedBay Street Corridor                   0.719 0.471905    
    ## neighbourhood_cleansedBayview Village                       0.302 0.762416    
    ## neighbourhood_cleansedBayview Woods-Steeles                 1.104 0.269789    
    ## neighbourhood_cleansedBedford Park-Nortown                  0.156 0.875685    
    ## neighbourhood_cleansedBeechborough-Greenbrook               0.272 0.785304    
    ## neighbourhood_cleansedBendale                               0.132 0.895201    
    ## neighbourhood_cleansedBirchcliffe-Cliffside                -0.041 0.967608    
    ## neighbourhood_cleansedBlack Creek                          -0.375 0.707855    
    ## neighbourhood_cleansedBlake-Jones                           0.154 0.877550    
    ## neighbourhood_cleansedBriar Hill-Belgravia                 -0.332 0.740075    
    ## neighbourhood_cleansedBridle Path-Sunnybrook-York Mills     0.989 0.322586    
    ## neighbourhood_cleansedBroadview North                       0.792 0.428647    
    ## neighbourhood_cleansedBrookhaven-Amesbury                  -0.206 0.836509    
    ## neighbourhood_cleansedCabbagetown-South St.James Town       0.910 0.362654    
    ## neighbourhood_cleansedCaledonia-Fairbank                   -0.430 0.667379    
    ## neighbourhood_cleansedCasa Loma                             1.102 0.270271    
    ## neighbourhood_cleansedCentennial Scarborough                0.165 0.868896    
    ## neighbourhood_cleansedChurch-Yonge Corridor                 0.520 0.603080    
    ## neighbourhood_cleansedClairlea-Birchmount                   0.009 0.993130    
    ## neighbourhood_cleansedClanton Park                         -0.297 0.766818    
    ## neighbourhood_cleansedCliffcrest                            0.125 0.900230    
    ## neighbourhood_cleansedCorso Italia-Davenport               -0.233 0.815863    
    ## neighbourhood_cleansedDanforth                              0.286 0.774755    
    ## neighbourhood_cleansedDanforth East York                    0.560 0.575636    
    ## neighbourhood_cleansedDon Valley Village                    0.150 0.880586    
    ## neighbourhood_cleansedDorset Park                          -0.189 0.850340    
    ## neighbourhood_cleansedDovercourt-Wallace Emerson-Junction   0.115 0.908753    
    ## neighbourhood_cleansedDownsview-Roding-CFB                 -0.143 0.885918    
    ## neighbourhood_cleansedDufferin Grove                        0.274 0.784310    
    ## neighbourhood_cleansedEast End-Danforth                     0.190 0.849373    
    ## neighbourhood_cleansedEdenbridge-Humber Valley              0.808 0.419287    
    ## neighbourhood_cleansedEglinton East                        -0.316 0.751678    
    ## neighbourhood_cleansedElms-Old Rexdale                     -0.035 0.972391    
    ## neighbourhood_cleansedEnglemount-Lawrence                   0.148 0.882515    
    ## neighbourhood_cleansedEringate-Centennial-West Deane        0.151 0.879627    
    ## neighbourhood_cleansedEtobicoke West Mall                   0.519 0.603993    
    ## neighbourhood_cleansedFlemingdon Park                       0.002 0.998106    
    ## neighbourhood_cleansedForest Hill North                    -0.093 0.925771    
    ## neighbourhood_cleansedForest Hill South                     1.337 0.181144    
    ## neighbourhood_cleansedGlenfield-Jane Heights               -0.347 0.728801    
    ## neighbourhood_cleansedGreenwood-Coxwell                     0.365 0.714870    
    ## neighbourhood_cleansedGuildwood                            -0.130 0.896497    
    ## neighbourhood_cleansedHenry Farm                            0.882 0.377906    
    ## neighbourhood_cleansedHigh Park North                       0.323 0.746400    
    ## neighbourhood_cleansedHigh Park-Swansea                     0.579 0.562703    
    ## neighbourhood_cleansedHighland Creek                       -0.057 0.954485    
    ## neighbourhood_cleansedHillcrest Village                    -0.189 0.850045    
    ## neighbourhood_cleansedHumber Heights-Westmount             -0.094 0.924814    
    ## neighbourhood_cleansedHumber Summit                        -0.174 0.862199    
    ## neighbourhood_cleansedHumbermede                           -0.164 0.869701    
    ## neighbourhood_cleansedHumewood-Cedarvale                    0.445 0.656126    
    ## neighbourhood_cleansedIonview                              -0.228 0.819588    
    ## neighbourhood_cleansedIslington-City Centre West           -0.007 0.994216    
    ## neighbourhood_cleansedJunction Area                         0.115 0.908375    
    ## neighbourhood_cleansedKeelesdale-Eglinton West             -0.241 0.809839    
    ## neighbourhood_cleansedKennedy Park                         -0.042 0.966522    
    ## neighbourhood_cleansedKensington-Chinatown                  0.350 0.726152    
    ## neighbourhood_cleansedKingsview Village-The Westway         0.083 0.933842    
    ## neighbourhood_cleansedKingsway South                        0.796 0.425817    
    ## neighbourhood_cleansedL'Amoreaux                           -0.252 0.801426    
    ## neighbourhood_cleansedLambton Baby Point                    0.401 0.688468    
    ## neighbourhood_cleansedLansing-Westgate                      0.071 0.943323    
    ## neighbourhood_cleansedLawrence Park North                   0.903 0.366721    
    ## neighbourhood_cleansedLawrence Park South                   0.795 0.426519    
    ## neighbourhood_cleansedLeaside-Bennington                    1.063 0.287746    
    ## neighbourhood_cleansedLittle Portugal                       0.643 0.520143    
    ## neighbourhood_cleansedLong Branch                           0.322 0.747405    
    ## neighbourhood_cleansedMalvern                              -0.487 0.626101    
    ## neighbourhood_cleansedMaple Leaf                            0.178 0.858569    
    ## neighbourhood_cleansedMarkland Wood                         0.670 0.502922    
    ## neighbourhood_cleansedMilliken                             -0.030 0.975870    
    ## neighbourhood_cleansedMimico (includes Humber Bay Shores)   0.478 0.632761    
    ## neighbourhood_cleansedMorningside                          -0.027 0.978623    
    ## neighbourhood_cleansedMoss Park                             0.755 0.450537    
    ## neighbourhood_cleansedMount Dennis                         -0.061 0.951316    
    ## neighbourhood_cleansedMount Olive-Silverstone-Jamestown    -0.202 0.840176    
    ## neighbourhood_cleansedMount Pleasant East                   0.525 0.599314    
    ## neighbourhood_cleansedMount Pleasant West                   0.226 0.821333    
    ## neighbourhood_cleansedNew Toronto                           0.376 0.707152    
    ## neighbourhood_cleansedNewtonbrook East                      0.000 0.999969    
    ## neighbourhood_cleansedNewtonbrook West                      1.263 0.206459    
    ## neighbourhood_cleansedNiagara                               1.135 0.256338    
    ## neighbourhood_cleansedNorth Riverdale                       0.611 0.541116    
    ## neighbourhood_cleansedNorth St.James Town                   0.205 0.837222    
    ## neighbourhood_cleansedO'Connor-Parkview                    -0.208 0.835395    
    ## neighbourhood_cleansedOakridge                              0.147 0.882801    
    ## neighbourhood_cleansedOakwood Village                      -0.015 0.987673    
    ## neighbourhood_cleansedOld East York                        -0.100 0.920501    
    ## neighbourhood_cleansedPalmerston-Little Italy               0.518 0.604721    
    ## neighbourhood_cleansedParkwoods-Donalda                    -0.092 0.926642    
    ## neighbourhood_cleansedPelmo Park-Humberlea                  0.243 0.807661    
    ## neighbourhood_cleansedPlayter Estates-Danforth              0.312 0.755299    
    ## neighbourhood_cleansedPleasant View                        -0.368 0.712785    
    ## neighbourhood_cleansedPrincess-Rosethorn                    0.076 0.939170    
    ## neighbourhood_cleansedRegent Park                           0.715 0.474698    
    ## neighbourhood_cleansedRexdale-Kipling                      -0.165 0.869277    
    ## neighbourhood_cleansedRockcliffe-Smythe                     0.200 0.841469    
    ## neighbourhood_cleansedRoncesvalles                          0.592 0.554104    
    ## neighbourhood_cleansedRosedale-Moore Park                   1.397 0.162294    
    ## neighbourhood_cleansedRouge                                 0.013 0.989891    
    ## neighbourhood_cleansedRunnymede-Bloor West Village          0.260 0.795235    
    ## neighbourhood_cleansedRustic                                2.991 0.002787 ** 
    ## neighbourhood_cleansedScarborough Village                   0.081 0.935123    
    ## neighbourhood_cleansedSouth Parkdale                        0.154 0.877793    
    ## neighbourhood_cleansedSouth Riverdale                       0.761 0.446724    
    ## neighbourhood_cleansedSt.Andrew-Windfields                  0.942 0.346070    
    ## neighbourhood_cleansedSteeles                              -0.415 0.678352    
    ## neighbourhood_cleansedStonegate-Queensway                   0.221 0.825283    
    ## neighbourhood_cleansedTam O'Shanter-Sullivan               -0.193 0.846592    
    ## neighbourhood_cleansedTaylor-Massey                         0.033 0.973549    
    ## neighbourhood_cleansedThe Beaches                           0.719 0.472149    
    ## neighbourhood_cleansedThistletown-Beaumond Heights          0.120 0.904392    
    ## neighbourhood_cleansedThorncliffe Park                     -0.173 0.863011    
    ## neighbourhood_cleansedTrinity-Bellwoods                     0.506 0.613205    
    ## neighbourhood_cleansedUniversity                            0.536 0.591714    
    ## neighbourhood_cleansedVictoria Village                     -0.021 0.983294    
    ## neighbourhood_cleansedWaterfront Communities-The Island     1.412 0.158107    
    ## neighbourhood_cleansedWest Hill                            -0.283 0.777318    
    ## neighbourhood_cleansedWest Humber-Clairville               -0.306 0.759614    
    ## neighbourhood_cleansedWestminster-Branson                  -0.235 0.814307    
    ## neighbourhood_cleansedWeston                                0.359 0.719373    
    ## neighbourhood_cleansedWeston-Pellam Park                   -0.077 0.938954    
    ## neighbourhood_cleansedWexford/Maryvale                     -0.031 0.975403    
    ## neighbourhood_cleansedWillowdale East                       0.214 0.830167    
    ## neighbourhood_cleansedWillowdale West                       0.176 0.859943    
    ## neighbourhood_cleansedWillowridge-Martingrove-Richview     -0.064 0.948812    
    ## neighbourhood_cleansedWoburn                                3.517 0.000439 ***
    ## neighbourhood_cleansedWoodbine Corridor                     0.534 0.593529    
    ## neighbourhood_cleansedWoodbine-Lumsden                      0.229 0.818917    
    ## neighbourhood_cleansedWychwood                              0.246 0.805926    
    ## neighbourhood_cleansedYonge-Eglinton                        0.474 0.635347    
    ## neighbourhood_cleansedYonge-St.Clair                        0.932 0.351229    
    ## neighbourhood_cleansedYork University Heights              -0.427 0.669447    
    ## neighbourhood_cleansedYorkdale-Glen Park                    0.159 0.873808    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 320.2 on 10820 degrees of freedom
    ## Multiple R-squared:  0.02417,    Adjusted R-squared:  0.01164 
    ## F-statistic: 1.928 on 139 and 10820 DF,  p-value: 4.689e-10

We can see that all the neighbourhood levels within the
neighbourhood\_cleansed covariate have p-values that are greater than
0.05, which indicates none of these levels are significant in predicting
price when neighbourhood\_cleansed is the only predictor included in the
linear model.

### Modelling Price with cleanliness score:

``` r
#removing any possible NAs still in review_scores_cleanliness 
feb_21_airbnb_train <- feb_21_airbnb_train %>%
  filter(!is.na(review_scores_cleanliness))

#price ~ cleanliness model
price_and_cleanliness <- lm(price ~ review_scores_cleanliness, data = feb_21_airbnb_train)
summary(price_and_cleanliness)
```

    ## 
    ## Call:
    ## lm(formula = price ~ review_scores_cleanliness, data = feb_21_airbnb_train)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ##  -118.4   -68.4   -36.4    16.5 12877.3 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                102.257     24.124   4.239 2.27e-05 ***
    ## review_scores_cleanliness    2.913      2.550   1.143    0.253    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 251.7 on 8306 degrees of freedom
    ## Multiple R-squared:  0.0001572,  Adjusted R-squared:  3.681e-05 
    ## F-statistic: 1.306 on 1 and 8306 DF,  p-value: 0.2532

We can see that the p-value for the cleanliness score covariate is
0.07707 when that variable is the only predictor.

### Modelling Price with overall review score

``` r
#removing any possible NAs in review_scores_rating 
feb_21_airbnb_train <- feb_21_airbnb_train %>%
  filter(!is.na(review_scores_rating))

#price ~ overall review score  model
price_and_reviewscore <- lm(price ~ review_scores_rating, data = feb_21_airbnb_train)
summary(price_and_reviewscore)
```

    ## 
    ## Call:
    ## lm(formula = price ~ review_scores_rating, data = feb_21_airbnb_train)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ##  -117.6   -68.9   -35.6    16.3 12876.2 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          113.6346    27.9879   4.060 4.95e-05 ***
    ## review_scores_rating   0.1698     0.2955   0.575    0.565    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 251.7 on 8306 degrees of freedom
    ## Multiple R-squared:  3.977e-05,  Adjusted R-squared:  -8.062e-05 
    ## F-statistic: 0.3304 on 1 and 8306 DF,  p-value: 0.5655

We can see that the p-value for the overall review score covariate is
0.29881 when that variable is the only predictor.

### Modelling price with reviews per month:

``` r
#removing any possible NAs in reviews_per_month 
feb_21_airbnb_train <- feb_21_airbnb_train %>%
  filter(!is.na(reviews_per_month))

#price ~ reviews per month model
price_and_reviewspmonth <- lm(price ~ reviews_per_month, data = feb_21_airbnb_train)
summary(price_and_reviewspmonth)
```

    ## 
    ## Call:
    ## lm(formula = price ~ reviews_per_month, data = feb_21_airbnb_train)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ##  -119.0   -68.4   -34.1    16.9 12867.1 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        133.234      3.454   38.57   <2e-16 ***
    ## reviews_per_month   -3.324      1.921   -1.73   0.0836 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 251.7 on 8306 degrees of freedom
    ## Multiple R-squared:  0.0003603,  Adjusted R-squared:  0.00024 
    ## F-statistic: 2.994 on 1 and 8306 DF,  p-value: 0.08362

We can see that the p-value for the reviews\_per\_month covariate is
0.219 when that variable is the only predictor.

### Modelling price with host response rate:

``` r
#removing any possible NAs in host_response_rate 
feb_21_airbnb_train <- feb_21_airbnb_train %>%
  filter(!is.na(host_response_rate))

#price ~ host response rate model
price_and_hostresp<- lm(price ~ host_response_rate, data = feb_21_airbnb_train)
summary(price_and_hostresp)
```

    ## 
    ## Call:
    ## lm(formula = price ~ host_response_rate, data = feb_21_airbnb_train)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -119.9  -65.5  -31.5   16.5 3437.1 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        132.88609    7.78545  17.069   <2e-16 ***
    ## host_response_rate  -0.12389    0.08339  -1.486    0.137    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 146.1 on 5085 degrees of freedom
    ## Multiple R-squared:  0.0004339,  Adjusted R-squared:  0.0002374 
    ## F-statistic: 2.208 on 1 and 5085 DF,  p-value: 0.1374

We can see that the p-value for the host\_response\_rate covariate is
0.137 when that variable is the only predictor.

### Modelling price with minimum nights available to book:

``` r
#removing any possible NAs in minimum_nights
feb_21_airbnb_train <- feb_21_airbnb_train %>%
  filter(!is.na(minimum_nights))

#price ~ minimum nights model
price_and_minnights <- lm(price ~ minimum_nights, data = feb_21_airbnb_train)
summary(price_and_minnights)
```

    ## 
    ## Call:
    ## lm(formula = price ~ minimum_nights, data = feb_21_airbnb_train)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -164.0  -64.0  -32.5   16.5 3447.5 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    118.83963    2.77494  42.826   <2e-16 ***
    ## minimum_nights   0.12927    0.08385   1.542    0.123    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 146.1 on 5085 degrees of freedom
    ## Multiple R-squared:  0.0004672,  Adjusted R-squared:  0.0002706 
    ## F-statistic: 2.377 on 1 and 5085 DF,  p-value: 0.1232

We can see that the p-value for the minimum\_nights covariate is
1.65e-06 when that variable is the only predictor.

### Modelling price with maximum nights available to book:

``` r
#removing any possible NAs in maximum_nights
feb_21_airbnb_train <- feb_21_airbnb_train %>%
  filter(!is.na(maximum_nights))

#price ~ maximum nights model
price_and_maxnights <- lm(price ~ maximum_nights, data = feb_21_airbnb_train)
summary(price_and_maxnights)
```

    ## 
    ## Call:
    ## lm(formula = price ~ maximum_nights, data = feb_21_airbnb_train)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -108.7  -63.7  -31.7   17.3 3448.3 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     1.217e+02  2.049e+00  59.415   <2e-16 ***
    ## maximum_nights -2.718e-09  1.461e-07  -0.019    0.985    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 146.1 on 5085 degrees of freedom
    ## Multiple R-squared:  6.805e-08,  Adjusted R-squared:  -0.0001966 
    ## F-statistic: 0.000346 on 1 and 5085 DF,  p-value: 0.9852

We can see that the p-value for the maximum\_nights covariate is 0.645
when that variable is the only predictor.

### Modelling price with total amount of host listings for a given host:

``` r
#removing any possible NAs in host_total_listings_count
feb_21_airbnb_train <- feb_21_airbnb_train %>%
  filter(!is.na(host_total_listings_count))

#price ~ amount of host listings model
price_and_totallists <- lm(price ~ host_total_listings_count, data = feb_21_airbnb_train)
summary(price_and_totallists)
```

    ## 
    ## Call:
    ## lm(formula = price ~ host_total_listings_count, data = feb_21_airbnb_train)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -110.6  -63.6  -31.5   17.5 3448.7 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               121.34071    2.22325  54.578   <2e-16 ***
    ## host_total_listings_count   0.04516    0.10122   0.446    0.656    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 146.1 on 5085 degrees of freedom
    ## Multiple R-squared:  3.914e-05,  Adjusted R-squared:  -0.0001575 
    ## F-statistic: 0.1991 on 1 and 5085 DF,  p-value: 0.6555

We can see that the p-value for the host\_total\_listings\_count
covariate is 0.402 when that variable is the only predictor.

### Modelling price with if a host is a Superhost or not:

We can see that the variable host\_is\_superhost is a logical variable.
This indicates it has levels TRUE and FALSE.

``` r
#viewing type of variable for the host_is_superhost variable
typeof(feb_21_airbnb_train$host_is_superhost)
```

    ## [1] "logical"

``` r
#removing any possible NAs in host_is_superhost
feb_21_airbnb_train <- feb_21_airbnb_train %>%
  filter(!is.na(host_is_superhost))

#price ~ is a superhost model without an intercept
price_and_superhost <- lm(price ~ 0 + host_is_superhost, data = feb_21_airbnb_train)
summary(price_and_superhost)
```

    ## 
    ## Call:
    ## lm(formula = price ~ 0 + host_is_superhost, data = feb_21_airbnb_train)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -107.7  -64.1  -31.7   17.3 3449.3 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## host_is_superhostFALSE  120.749      2.664   45.33   <2e-16 ***
    ## host_is_superhostTRUE   123.140      3.204   38.43   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 146.1 on 5085 degrees of freedom
    ## Multiple R-squared:  0.4098, Adjusted R-squared:  0.4096 
    ## F-statistic:  1766 on 2 and 5085 DF,  p-value: < 2.2e-16

``` r
#price ~ is a superhost model with an intercept
price_and_superhost <- lm(price ~ host_is_superhost, data = feb_21_airbnb_train)
summary(price_and_superhost)
```

    ## 
    ## Call:
    ## lm(formula = price ~ host_is_superhost, data = feb_21_airbnb_train)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -107.7  -64.1  -31.7   17.3 3449.3 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            120.749      2.664  45.327   <2e-16 ***
    ## host_is_superhostTRUE    2.391      4.167   0.574    0.566    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 146.1 on 5085 degrees of freedom
    ## Multiple R-squared:  6.474e-05,  Adjusted R-squared:  -0.0001319 
    ## F-statistic: 0.3292 on 1 and 5085 DF,  p-value: 0.5661

We can see that when no intercept is included in the model, that both
levels of host\_is\_superhost are significant at the 0.05 significance
level. However, when an intercept is included in the model,
host\_is\_superhostTRUE is not significant.

### Modelling price with the selected possibly relevant variables:

Using AIC:

``` r
#fitting an initial model with all the possibly relevant variables
initial_full_model <- lm(price ~ bedrooms + 
                        bathrooms_numeric + 
                        neighbourhood_cleansed +
                        review_scores_cleanliness +
                        review_scores_rating +
                        reviews_per_month +
                        host_response_rate +
                        minimum_nights +
                        maximum_nights +
                        host_is_superhost,
                        data = feb_21_airbnb_train)
summary(initial_full_model)
```

    ## 
    ## Call:
    ## lm(formula = price ~ bedrooms + bathrooms_numeric + neighbourhood_cleansed + 
    ##     review_scores_cleanliness + review_scores_rating + reviews_per_month + 
    ##     host_response_rate + minimum_nights + maximum_nights + host_is_superhost, 
    ##     data = feb_21_airbnb_train)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -538.8  -41.8  -11.8   20.7 3373.8 
    ## 
    ## Coefficients:
    ##                                                             Estimate Std. Error
    ## (Intercept)                                               -1.229e+02  5.535e+01
    ## bedrooms                                                   6.193e+01  2.759e+00
    ## bathrooms_numeric                                          3.407e+01  4.036e+00
    ## neighbourhood_cleansedAgincourt South-Malvern West         2.830e+01  5.593e+01
    ## neighbourhood_cleansedAlderwood                            5.938e+01  6.851e+01
    ## neighbourhood_cleansedAnnex                                1.025e+02  5.285e+01
    ## neighbourhood_cleansedBanbury-Don Mills                    5.567e+01  6.438e+01
    ## neighbourhood_cleansedBathurst Manor                       1.854e+02  6.338e+01
    ## neighbourhood_cleansedBay Street Corridor                  1.052e+02  5.287e+01
    ## neighbourhood_cleansedBayview Village                      6.568e+01  5.739e+01
    ## neighbourhood_cleansedBayview Woods-Steeles                4.808e+01  5.659e+01
    ## neighbourhood_cleansedBedford Park-Nortown                 4.915e+00  5.744e+01
    ## neighbourhood_cleansedBeechborough-Greenbrook              3.688e+01  7.057e+01
    ## neighbourhood_cleansedBendale                              5.768e+01  5.980e+01
    ## neighbourhood_cleansedBirchcliffe-Cliffside                2.151e+01  5.688e+01
    ## neighbourhood_cleansedBlack Creek                         -1.062e+02  7.062e+01
    ## neighbourhood_cleansedBlake-Jones                          6.577e+01  5.943e+01
    ## neighbourhood_cleansedBriar Hill-Belgravia                 3.054e+01  6.256e+01
    ## neighbourhood_cleansedBridle Path-Sunnybrook-York Mills    5.625e+01  6.683e+01
    ## neighbourhood_cleansedBroadview North                      1.690e+02  6.683e+01
    ## neighbourhood_cleansedBrookhaven-Amesbury                  2.479e+01  7.052e+01
    ## neighbourhood_cleansedCabbagetown-South St.James Town      1.142e+02  5.475e+01
    ## neighbourhood_cleansedCaledonia-Fairbank                   4.246e+01  6.022e+01
    ## neighbourhood_cleansedCasa Loma                            1.260e+02  6.682e+01
    ## neighbourhood_cleansedCentennial Scarborough               7.986e+01  8.181e+01
    ## neighbourhood_cleansedChurch-Yonge Corridor                1.081e+02  5.300e+01
    ## neighbourhood_cleansedClairlea-Birchmount                  8.362e+01  5.906e+01
    ## neighbourhood_cleansedClanton Park                         2.732e+01  5.838e+01
    ## neighbourhood_cleansedCliffcrest                           5.341e+01  6.186e+01
    ## neighbourhood_cleansedCorso Italia-Davenport               4.356e+01  5.567e+01
    ## neighbourhood_cleansedDanforth                             7.040e+01  5.940e+01
    ## neighbourhood_cleansedDanforth East York                   6.861e+01  6.071e+01
    ## neighbourhood_cleansedDon Valley Village                   4.736e+01  5.744e+01
    ## neighbourhood_cleansedDorset Park                          3.924e+01  5.905e+01
    ## neighbourhood_cleansedDovercourt-Wallace Emerson-Junction  5.538e+01  5.329e+01
    ## neighbourhood_cleansedDownsview-Roding-CFB                 2.589e+01  5.869e+01
    ## neighbourhood_cleansedDufferin Grove                       6.817e+01  5.500e+01
    ## neighbourhood_cleansedEast End-Danforth                    5.530e+01  5.506e+01
    ## neighbourhood_cleansedEdenbridge-Humber Valley             6.390e+01  8.176e+01
    ## neighbourhood_cleansedEglinton East                        3.189e+01  6.680e+01
    ## neighbourhood_cleansedElms-Old Rexdale                     7.698e+01  7.318e+01
    ## neighbourhood_cleansedEnglemount-Lawrence                  4.444e+01  6.068e+01
    ## neighbourhood_cleansedEringate-Centennial-West Deane       5.435e+01  5.938e+01
    ## neighbourhood_cleansedEtobicoke West Mall                  8.281e+01  7.049e+01
    ## neighbourhood_cleansedFlemingdon Park                      4.658e+01  8.961e+01
    ## neighbourhood_cleansedForest Hill North                    7.576e+01  7.676e+01
    ## neighbourhood_cleansedForest Hill South                    4.803e+01  8.960e+01
    ## neighbourhood_cleansedGlenfield-Jane Heights               3.799e+01  6.847e+01
    ## neighbourhood_cleansedGreenwood-Coxwell                    7.314e+01  5.562e+01
    ## neighbourhood_cleansedGuildwood                            2.482e+01  7.676e+01
    ## neighbourhood_cleansedHenry Farm                           6.944e+01  6.183e+01
    ## neighbourhood_cleansedHigh Park North                      4.804e+01  5.606e+01
    ## neighbourhood_cleansedHigh Park-Swansea                    8.561e+01  5.438e+01
    ## neighbourhood_cleansedHighland Creek                       1.905e+01  6.189e+01
    ## neighbourhood_cleansedHillcrest Village                    8.948e+00  6.122e+01
    ## neighbourhood_cleansedHumber Heights-Westmount             7.268e+01  7.326e+01
    ## neighbourhood_cleansedHumber Summit                        5.454e+01  1.035e+02
    ## neighbourhood_cleansedHumbermede                           7.127e+01  7.053e+01
    ## neighbourhood_cleansedHumewood-Cedarvale                   5.705e+01  5.933e+01
    ## neighbourhood_cleansedIonview                              1.607e+01  5.905e+01
    ## neighbourhood_cleansedIslington-City Centre West           4.609e+01  5.511e+01
    ## neighbourhood_cleansedJunction Area                        5.940e+01  5.540e+01
    ## neighbourhood_cleansedKeelesdale-Eglinton West             4.000e+01  6.432e+01
    ## neighbourhood_cleansedKennedy Park                         2.486e+01  5.898e+01
    ## neighbourhood_cleansedKensington-Chinatown                 7.979e+01  5.302e+01
    ## neighbourhood_cleansedKingsview Village-The Westway        6.143e+01  6.690e+01
    ## neighbourhood_cleansedKingsway South                       6.937e+01  1.035e+02
    ## neighbourhood_cleansedL'Amoreaux                          -5.458e+00  5.867e+01
    ## neighbourhood_cleansedLambton Baby Point                   8.167e+01  7.676e+01
    ## neighbourhood_cleansedLansing-Westgate                     4.597e+01  5.525e+01
    ## neighbourhood_cleansedLawrence Park North                  5.988e+01  5.940e+01
    ## neighbourhood_cleansedLawrence Park South                  6.705e+01  6.072e+01
    ## neighbourhood_cleansedLeaside-Bennington                   1.127e+02  6.185e+01
    ## neighbourhood_cleansedLittle Portugal                      1.057e+02  5.356e+01
    ## neighbourhood_cleansedLong Branch                          4.364e+01  6.019e+01
    ## neighbourhood_cleansedMalvern                              1.867e+01  5.551e+01
    ## neighbourhood_cleansedMaple Leaf                           7.031e+01  7.319e+01
    ## neighbourhood_cleansedMarkland Wood                        8.148e+01  7.317e+01
    ## neighbourhood_cleansedMilliken                             4.345e+01  7.049e+01
    ## neighbourhood_cleansedMimico (includes Humber Bay Shores)  1.083e+02  5.462e+01
    ## neighbourhood_cleansedMorningside                          2.547e+01  6.431e+01
    ## neighbourhood_cleansedMoss Park                            1.097e+02  5.297e+01
    ## neighbourhood_cleansedMount Dennis                        -2.198e+00  6.432e+01
    ## neighbourhood_cleansedMount Olive-Silverstone-Jamestown    4.197e+01  7.055e+01
    ## neighbourhood_cleansedMount Pleasant East                  9.051e+01  6.019e+01
    ## neighbourhood_cleansedMount Pleasant West                  7.815e+01  5.425e+01
    ## neighbourhood_cleansedNew Toronto                          1.539e+02  6.017e+01
    ## neighbourhood_cleansedNewtonbrook East                     2.440e+01  5.471e+01
    ## neighbourhood_cleansedNewtonbrook West                     4.164e+01  5.458e+01
    ## neighbourhood_cleansedNiagara                              1.283e+02  5.279e+01
    ## neighbourhood_cleansedNorth Riverdale                      7.479e+01  5.674e+01
    ## neighbourhood_cleansedNorth St.James Town                  7.736e+01  5.687e+01
    ## neighbourhood_cleansedO'Connor-Parkview                    4.873e+01  5.873e+01
    ## neighbourhood_cleansedOakridge                             1.076e+02  6.185e+01
    ## neighbourhood_cleansedOakwood Village                      4.295e+01  5.599e+01
    ## neighbourhood_cleansedOld East York                        5.463e+01  5.907e+01
    ## neighbourhood_cleansedPalmerston-Little Italy              9.768e+01  5.354e+01
    ## neighbourhood_cleansedParkwoods-Donalda                   -1.585e+00  6.187e+01
    ## neighbourhood_cleansedPelmo Park-Humberlea                 4.469e+01  6.551e+01
    ## neighbourhood_cleansedPlayter Estates-Danforth             6.809e+01  6.072e+01
    ## neighbourhood_cleansedPleasant View                        2.694e+01  5.723e+01
    ## neighbourhood_cleansedPrincess-Rosethorn                   5.658e+01  6.341e+01
    ## neighbourhood_cleansedRegent Park                          1.169e+02  5.841e+01
    ## neighbourhood_cleansedRexdale-Kipling                      3.047e+01  6.551e+01
    ## neighbourhood_cleansedRockcliffe-Smythe                    5.674e+01  7.322e+01
    ## neighbourhood_cleansedRoncesvalles                         9.167e+01  5.485e+01
    ## neighbourhood_cleansedRosedale-Moore Park                  9.500e+01  5.839e+01
    ## neighbourhood_cleansedRouge                                5.181e+01  6.253e+01
    ## neighbourhood_cleansedRunnymede-Bloor West Village         4.774e+01  6.684e+01
    ## neighbourhood_cleansedRustic                               8.585e-01  1.369e+02
    ## neighbourhood_cleansedScarborough Village                  4.668e+01  8.962e+01
    ## neighbourhood_cleansedSouth Parkdale                       5.240e+01  5.381e+01
    ## neighbourhood_cleansedSouth Riverdale                      9.794e+01  5.346e+01
    ## neighbourhood_cleansedSt.Andrew-Windfields                 1.263e+02  5.977e+01
    ## neighbourhood_cleansedSteeles                              3.446e+01  6.018e+01
    ## neighbourhood_cleansedStonegate-Queensway                  5.484e+01  5.550e+01
    ## neighbourhood_cleansedTam O'Shanter-Sullivan               1.597e+01  5.763e+01
    ## neighbourhood_cleansedTaylor-Massey                        4.227e+01  6.680e+01
    ## neighbourhood_cleansedThe Beaches                          8.361e+01  5.484e+01
    ## neighbourhood_cleansedThistletown-Beaumond Heights         6.979e+01  8.962e+01
    ## neighbourhood_cleansedThorncliffe Park                     5.473e+01  6.846e+01
    ## neighbourhood_cleansedTrinity-Bellwoods                    9.578e+01  5.292e+01
    ## neighbourhood_cleansedUniversity                           5.751e+01  5.552e+01
    ## neighbourhood_cleansedVictoria Village                     1.877e+01  6.339e+01
    ## neighbourhood_cleansedWaterfront Communities-The Island    1.242e+02  5.193e+01
    ## neighbourhood_cleansedWest Hill                            4.127e+01  5.872e+01
    ## neighbourhood_cleansedWest Humber-Clairville               5.074e+01  5.722e+01
    ## neighbourhood_cleansedWestminster-Branson                  3.385e+01  5.940e+01
    ## neighbourhood_cleansedWeston                               4.554e+01  6.020e+01
    ## neighbourhood_cleansedWeston-Pellam Park                   7.814e+01  5.786e+01
    ## neighbourhood_cleansedWexford/Maryvale                     3.413e+01  5.602e+01
    ## neighbourhood_cleansedWillowdale East                      6.398e+01  5.283e+01
    ## neighbourhood_cleansedWillowdale West                      4.756e+01  5.591e+01
    ## neighbourhood_cleansedWillowridge-Martingrove-Richview     2.738e+01  5.904e+01
    ## neighbourhood_cleansedWoburn                               4.178e+01  5.814e+01
    ## neighbourhood_cleansedWoodbine Corridor                    1.038e+02  5.902e+01
    ## neighbourhood_cleansedWoodbine-Lumsden                     5.586e+01  6.338e+01
    ## neighbourhood_cleansedWychwood                             7.622e+01  5.673e+01
    ## neighbourhood_cleansedYonge-Eglinton                       7.044e+01  5.811e+01
    ## neighbourhood_cleansedYonge-St.Clair                       8.937e+01  6.185e+01
    ## neighbourhood_cleansedYork University Heights              3.398e+01  5.372e+01
    ## neighbourhood_cleansedYorkdale-Glen Park                   5.812e+01  5.458e+01
    ## review_scores_cleanliness                                  5.410e+00  2.873e+00
    ## review_scores_rating                                      -8.281e-03  3.241e-01
    ## reviews_per_month                                         -4.052e+00  1.217e+00
    ## host_response_rate                                        -1.787e-01  7.747e-02
    ## minimum_nights                                             4.815e-02  7.602e-02
    ## maximum_nights                                             1.112e-08  1.272e-07
    ## host_is_superhostTRUE                                      1.498e-01  3.999e+00
    ##                                                           t value Pr(>|t|)    
    ## (Intercept)                                                -2.221 0.026363 *  
    ## bedrooms                                                   22.450  < 2e-16 ***
    ## bathrooms_numeric                                           8.440  < 2e-16 ***
    ## neighbourhood_cleansedAgincourt South-Malvern West          0.506 0.612810    
    ## neighbourhood_cleansedAlderwood                             0.867 0.386170    
    ## neighbourhood_cleansedAnnex                                 1.940 0.052435 .  
    ## neighbourhood_cleansedBanbury-Don Mills                     0.865 0.387266    
    ## neighbourhood_cleansedBathurst Manor                        2.925 0.003456 ** 
    ## neighbourhood_cleansedBay Street Corridor                   1.989 0.046718 *  
    ## neighbourhood_cleansedBayview Village                       1.145 0.252443    
    ## neighbourhood_cleansedBayview Woods-Steeles                 0.850 0.395607    
    ## neighbourhood_cleansedBedford Park-Nortown                  0.086 0.931821    
    ## neighbourhood_cleansedBeechborough-Greenbrook               0.523 0.601269    
    ## neighbourhood_cleansedBendale                               0.965 0.334789    
    ## neighbourhood_cleansedBirchcliffe-Cliffside                 0.378 0.705329    
    ## neighbourhood_cleansedBlack Creek                          -1.504 0.132645    
    ## neighbourhood_cleansedBlake-Jones                           1.107 0.268506    
    ## neighbourhood_cleansedBriar Hill-Belgravia                  0.488 0.625383    
    ## neighbourhood_cleansedBridle Path-Sunnybrook-York Mills     0.842 0.400021    
    ## neighbourhood_cleansedBroadview North                       2.529 0.011464 *  
    ## neighbourhood_cleansedBrookhaven-Amesbury                   0.352 0.725168    
    ## neighbourhood_cleansedCabbagetown-South St.James Town       2.086 0.036999 *  
    ## neighbourhood_cleansedCaledonia-Fairbank                    0.705 0.480782    
    ## neighbourhood_cleansedCasa Loma                             1.886 0.059401 .  
    ## neighbourhood_cleansedCentennial Scarborough                0.976 0.329017    
    ## neighbourhood_cleansedChurch-Yonge Corridor                 2.039 0.041476 *  
    ## neighbourhood_cleansedClairlea-Birchmount                   1.416 0.156877    
    ## neighbourhood_cleansedClanton Park                          0.468 0.639782    
    ## neighbourhood_cleansedCliffcrest                            0.863 0.387975    
    ## neighbourhood_cleansedCorso Italia-Davenport                0.782 0.433985    
    ## neighbourhood_cleansedDanforth                              1.185 0.235946    
    ## neighbourhood_cleansedDanforth East York                    1.130 0.258477    
    ## neighbourhood_cleansedDon Valley Village                    0.824 0.409732    
    ## neighbourhood_cleansedDorset Park                           0.665 0.506359    
    ## neighbourhood_cleansedDovercourt-Wallace Emerson-Junction   1.039 0.298754    
    ## neighbourhood_cleansedDownsview-Roding-CFB                  0.441 0.659124    
    ## neighbourhood_cleansedDufferin Grove                        1.239 0.215250    
    ## neighbourhood_cleansedEast End-Danforth                     1.004 0.315204    
    ## neighbourhood_cleansedEdenbridge-Humber Valley              0.782 0.434524    
    ## neighbourhood_cleansedEglinton East                         0.477 0.633139    
    ## neighbourhood_cleansedElms-Old Rexdale                      1.052 0.292876    
    ## neighbourhood_cleansedEnglemount-Lawrence                   0.732 0.463984    
    ## neighbourhood_cleansedEringate-Centennial-West Deane        0.915 0.360139    
    ## neighbourhood_cleansedEtobicoke West Mall                   1.175 0.240172    
    ## neighbourhood_cleansedFlemingdon Park                       0.520 0.603194    
    ## neighbourhood_cleansedForest Hill North                     0.987 0.323703    
    ## neighbourhood_cleansedForest Hill South                     0.536 0.591972    
    ## neighbourhood_cleansedGlenfield-Jane Heights                0.555 0.579071    
    ## neighbourhood_cleansedGreenwood-Coxwell                     1.315 0.188519    
    ## neighbourhood_cleansedGuildwood                             0.323 0.746391    
    ## neighbourhood_cleansedHenry Farm                            1.123 0.261490    
    ## neighbourhood_cleansedHigh Park North                       0.857 0.391575    
    ## neighbourhood_cleansedHigh Park-Swansea                     1.574 0.115500    
    ## neighbourhood_cleansedHighland Creek                        0.308 0.758256    
    ## neighbourhood_cleansedHillcrest Village                     0.146 0.883789    
    ## neighbourhood_cleansedHumber Heights-Westmount              0.992 0.321191    
    ## neighbourhood_cleansedHumber Summit                         0.527 0.598110    
    ## neighbourhood_cleansedHumbermede                            1.010 0.312315    
    ## neighbourhood_cleansedHumewood-Cedarvale                    0.962 0.336327    
    ## neighbourhood_cleansedIonview                               0.272 0.785462    
    ## neighbourhood_cleansedIslington-City Centre West            0.836 0.402992    
    ## neighbourhood_cleansedJunction Area                         1.072 0.283638    
    ## neighbourhood_cleansedKeelesdale-Eglinton West              0.622 0.534108    
    ## neighbourhood_cleansedKennedy Park                          0.422 0.673366    
    ## neighbourhood_cleansedKensington-Chinatown                  1.505 0.132435    
    ## neighbourhood_cleansedKingsview Village-The Westway         0.918 0.358515    
    ## neighbourhood_cleansedKingsway South                        0.670 0.502684    
    ## neighbourhood_cleansedL'Amoreaux                           -0.093 0.925879    
    ## neighbourhood_cleansedLambton Baby Point                    1.064 0.287404    
    ## neighbourhood_cleansedLansing-Westgate                      0.832 0.405495    
    ## neighbourhood_cleansedLawrence Park North                   1.008 0.313526    
    ## neighbourhood_cleansedLawrence Park South                   1.104 0.269541    
    ## neighbourhood_cleansedLeaside-Bennington                    1.822 0.068465 .  
    ## neighbourhood_cleansedLittle Portugal                       1.973 0.048526 *  
    ## neighbourhood_cleansedLong Branch                           0.725 0.468454    
    ## neighbourhood_cleansedMalvern                               0.336 0.736665    
    ## neighbourhood_cleansedMaple Leaf                            0.961 0.336764    
    ## neighbourhood_cleansedMarkland Wood                         1.113 0.265552    
    ## neighbourhood_cleansedMilliken                              0.616 0.537634    
    ## neighbourhood_cleansedMimico (includes Humber Bay Shores)   1.983 0.047469 *  
    ## neighbourhood_cleansedMorningside                           0.396 0.692138    
    ## neighbourhood_cleansedMoss Park                             2.071 0.038413 *  
    ## neighbourhood_cleansedMount Dennis                         -0.034 0.972742    
    ## neighbourhood_cleansedMount Olive-Silverstone-Jamestown     0.595 0.551984    
    ## neighbourhood_cleansedMount Pleasant East                   1.504 0.132697    
    ## neighbourhood_cleansedMount Pleasant West                   1.441 0.149749    
    ## neighbourhood_cleansedNew Toronto                           2.558 0.010554 *  
    ## neighbourhood_cleansedNewtonbrook East                      0.446 0.655577    
    ## neighbourhood_cleansedNewtonbrook West                      0.763 0.445499    
    ## neighbourhood_cleansedNiagara                               2.430 0.015135 *  
    ## neighbourhood_cleansedNorth Riverdale                       1.318 0.187558    
    ## neighbourhood_cleansedNorth St.James Town                   1.360 0.173776    
    ## neighbourhood_cleansedO'Connor-Parkview                     0.830 0.406780    
    ## neighbourhood_cleansedOakridge                              1.740 0.081915 .  
    ## neighbourhood_cleansedOakwood Village                       0.767 0.443092    
    ## neighbourhood_cleansedOld East York                         0.925 0.355088    
    ## neighbourhood_cleansedPalmerston-Little Italy               1.824 0.068165 .  
    ## neighbourhood_cleansedParkwoods-Donalda                    -0.026 0.979557    
    ## neighbourhood_cleansedPelmo Park-Humberlea                  0.682 0.495174    
    ## neighbourhood_cleansedPlayter Estates-Danforth              1.121 0.262169    
    ## neighbourhood_cleansedPleasant View                         0.471 0.637804    
    ## neighbourhood_cleansedPrincess-Rosethorn                    0.892 0.372284    
    ## neighbourhood_cleansedRegent Park                           2.001 0.045444 *  
    ## neighbourhood_cleansedRexdale-Kipling                       0.465 0.641849    
    ## neighbourhood_cleansedRockcliffe-Smythe                     0.775 0.438396    
    ## neighbourhood_cleansedRoncesvalles                          1.671 0.094764 .  
    ## neighbourhood_cleansedRosedale-Moore Park                   1.627 0.103767    
    ## neighbourhood_cleansedRouge                                 0.829 0.407421    
    ## neighbourhood_cleansedRunnymede-Bloor West Village          0.714 0.475101    
    ## neighbourhood_cleansedRustic                                0.006 0.994997    
    ## neighbourhood_cleansedScarborough Village                   0.521 0.602494    
    ## neighbourhood_cleansedSouth Parkdale                        0.974 0.330151    
    ## neighbourhood_cleansedSouth Riverdale                       1.832 0.066988 .  
    ## neighbourhood_cleansedSt.Andrew-Windfields                  2.113 0.034660 *  
    ## neighbourhood_cleansedSteeles                               0.573 0.566886    
    ## neighbourhood_cleansedStonegate-Queensway                   0.988 0.323152    
    ## neighbourhood_cleansedTam O'Shanter-Sullivan                0.277 0.781714    
    ## neighbourhood_cleansedTaylor-Massey                         0.633 0.526928    
    ## neighbourhood_cleansedThe Beaches                           1.525 0.127412    
    ## neighbourhood_cleansedThistletown-Beaumond Heights          0.779 0.436166    
    ## neighbourhood_cleansedThorncliffe Park                      0.800 0.424009    
    ## neighbourhood_cleansedTrinity-Bellwoods                     1.810 0.070411 .  
    ## neighbourhood_cleansedUniversity                            1.036 0.300314    
    ## neighbourhood_cleansedVictoria Village                      0.296 0.767168    
    ## neighbourhood_cleansedWaterfront Communities-The Island     2.392 0.016795 *  
    ## neighbourhood_cleansedWest Hill                             0.703 0.482231    
    ## neighbourhood_cleansedWest Humber-Clairville                0.887 0.375303    
    ## neighbourhood_cleansedWestminster-Branson                   0.570 0.568743    
    ## neighbourhood_cleansedWeston                                0.756 0.449387    
    ## neighbourhood_cleansedWeston-Pellam Park                    1.351 0.176865    
    ## neighbourhood_cleansedWexford/Maryvale                      0.609 0.542452    
    ## neighbourhood_cleansedWillowdale East                       1.211 0.225963    
    ## neighbourhood_cleansedWillowdale West                       0.851 0.394985    
    ## neighbourhood_cleansedWillowridge-Martingrove-Richview      0.464 0.642816    
    ## neighbourhood_cleansedWoburn                                0.719 0.472396    
    ## neighbourhood_cleansedWoodbine Corridor                     1.760 0.078530 .  
    ## neighbourhood_cleansedWoodbine-Lumsden                      0.881 0.378212    
    ## neighbourhood_cleansedWychwood                              1.344 0.179138    
    ## neighbourhood_cleansedYonge-Eglinton                        1.212 0.225514    
    ## neighbourhood_cleansedYonge-St.Clair                        1.445 0.148562    
    ## neighbourhood_cleansedYork University Heights               0.633 0.526992    
    ## neighbourhood_cleansedYorkdale-Glen Park                    1.065 0.287009    
    ## review_scores_cleanliness                                   1.883 0.059756 .  
    ## review_scores_rating                                       -0.026 0.979614    
    ## reviews_per_month                                          -3.329 0.000877 ***
    ## host_response_rate                                         -2.307 0.021080 *  
    ## minimum_nights                                              0.633 0.526556    
    ## maximum_nights                                              0.087 0.930289    
    ## host_is_superhostTRUE                                       0.037 0.970121    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 126.6 on 4938 degrees of freedom
    ## Multiple R-squared:  0.2705, Adjusted R-squared:  0.2487 
    ## F-statistic: 12.37 on 148 and 4938 DF,  p-value: < 2.2e-16

``` r
stats::step(initial_full_model, direction = "backward")
```

    ## Start:  AIC=49402.17
    ## price ~ bedrooms + bathrooms_numeric + neighbourhood_cleansed + 
    ##     review_scores_cleanliness + review_scores_rating + reviews_per_month + 
    ##     host_response_rate + minimum_nights + maximum_nights + host_is_superhost
    ## 
    ##                              Df Sum of Sq      RSS   AIC
    ## - review_scores_rating        1        10 79186598 49400
    ## - host_is_superhost           1        23 79186611 49400
    ## - maximum_nights              1       123 79186711 49400
    ## - minimum_nights              1      6432 79193020 49401
    ## <none>                                    79186588 49402
    ## - review_scores_cleanliness   1     56861 79243449 49404
    ## - host_response_rate          1     85370 79271958 49406
    ## - reviews_per_month           1    177755 79364343 49412
    ## - bathrooms_numeric           1   1142298 80328886 49473
    ## - neighbourhood_cleansed    139   6529032 85715620 49527
    ## - bedrooms                    1   8082434 87269022 49895
    ## 
    ## Step:  AIC=49400.17
    ## price ~ bedrooms + bathrooms_numeric + neighbourhood_cleansed + 
    ##     review_scores_cleanliness + reviews_per_month + host_response_rate + 
    ##     minimum_nights + maximum_nights + host_is_superhost
    ## 
    ##                              Df Sum of Sq      RSS   AIC
    ## - host_is_superhost           1        21 79186619 49398
    ## - maximum_nights              1       123 79186722 49398
    ## - minimum_nights              1      6423 79193022 49399
    ## <none>                                    79186598 49400
    ## - host_response_rate          1     85439 79272038 49404
    ## - review_scores_cleanliness   1    139273 79325871 49407
    ## - reviews_per_month           1    177814 79364412 49410
    ## - bathrooms_numeric           1   1142289 80328888 49471
    ## - neighbourhood_cleansed    139   6529863 85716461 49525
    ## - bedrooms                    1   8085064 87271662 49893
    ## 
    ## Step:  AIC=49398.17
    ## price ~ bedrooms + bathrooms_numeric + neighbourhood_cleansed + 
    ##     review_scores_cleanliness + reviews_per_month + host_response_rate + 
    ##     minimum_nights + maximum_nights
    ## 
    ##                              Df Sum of Sq      RSS   AIC
    ## - maximum_nights              1       122 79186741 49396
    ## - minimum_nights              1      6426 79193045 49397
    ## <none>                                    79186619 49398
    ## - host_response_rate          1     88442 79275061 49402
    ## - review_scores_cleanliness   1    147765 79334384 49406
    ## - reviews_per_month           1    181451 79368070 49408
    ## - bathrooms_numeric           1   1142276 80328895 49469
    ## - neighbourhood_cleansed    139   6530548 85717167 49523
    ## - bedrooms                    1   8085115 87271734 49891
    ## 
    ## Step:  AIC=49396.18
    ## price ~ bedrooms + bathrooms_numeric + neighbourhood_cleansed + 
    ##     review_scores_cleanliness + reviews_per_month + host_response_rate + 
    ##     minimum_nights
    ## 
    ##                              Df Sum of Sq      RSS   AIC
    ## - minimum_nights              1      6396 79193137 49395
    ## <none>                                    79186741 49396
    ## - host_response_rate          1     88419 79275160 49400
    ## - review_scores_cleanliness   1    147719 79334460 49404
    ## - reviews_per_month           1    181535 79368276 49406
    ## - bathrooms_numeric           1   1142207 80328948 49467
    ## - neighbourhood_cleansed    139   6532367 85719108 49521
    ## - bedrooms                    1   8085005 87271746 49889
    ## 
    ## Step:  AIC=49394.59
    ## price ~ bedrooms + bathrooms_numeric + neighbourhood_cleansed + 
    ##     review_scores_cleanliness + reviews_per_month + host_response_rate
    ## 
    ##                              Df Sum of Sq      RSS   AIC
    ## <none>                                    79193137 49395
    ## - host_response_rate          1     92157 79285295 49399
    ## - review_scores_cleanliness   1    146838 79339975 49402
    ## - reviews_per_month           1    200424 79393562 49405
    ## - bathrooms_numeric           1   1138292 80331429 49465
    ## - neighbourhood_cleansed    139   6623010 85816147 49525
    ## - bedrooms                    1   8085793 87278931 49887

    ## 
    ## Call:
    ## lm(formula = price ~ bedrooms + bathrooms_numeric + neighbourhood_cleansed + 
    ##     review_scores_cleanliness + reviews_per_month + host_response_rate, 
    ##     data = feb_21_airbnb_train)
    ## 
    ## Coefficients:
    ##                                               (Intercept)  
    ##                                                 -121.7356  
    ##                                                  bedrooms  
    ##                                                   61.9303  
    ##                                         bathrooms_numeric  
    ##                                                   33.9926  
    ##        neighbourhood_cleansedAgincourt South-Malvern West  
    ##                                                   28.2150  
    ##                           neighbourhood_cleansedAlderwood  
    ##                                                   59.2782  
    ##                               neighbourhood_cleansedAnnex  
    ##                                                  102.9340  
    ##                   neighbourhood_cleansedBanbury-Don Mills  
    ##                                                   57.5445  
    ##                      neighbourhood_cleansedBathurst Manor  
    ##                                                  185.2096  
    ##                 neighbourhood_cleansedBay Street Corridor  
    ##                                                  105.5584  
    ##                     neighbourhood_cleansedBayview Village  
    ##                                                   66.1007  
    ##               neighbourhood_cleansedBayview Woods-Steeles  
    ##                                                   48.3429  
    ##                neighbourhood_cleansedBedford Park-Nortown  
    ##                                                    4.8631  
    ##             neighbourhood_cleansedBeechborough-Greenbrook  
    ##                                                   36.2907  
    ##                             neighbourhood_cleansedBendale  
    ##                                                   57.9337  
    ##               neighbourhood_cleansedBirchcliffe-Cliffside  
    ##                                                   21.8315  
    ##                         neighbourhood_cleansedBlack Creek  
    ##                                                 -106.1108  
    ##                         neighbourhood_cleansedBlake-Jones  
    ##                                                   65.6957  
    ##                neighbourhood_cleansedBriar Hill-Belgravia  
    ##                                                   30.6090  
    ##   neighbourhood_cleansedBridle Path-Sunnybrook-York Mills  
    ##                                                   56.2733  
    ##                     neighbourhood_cleansedBroadview North  
    ##                                                  168.9248  
    ##                 neighbourhood_cleansedBrookhaven-Amesbury  
    ##                                                   25.1542  
    ##     neighbourhood_cleansedCabbagetown-South St.James Town  
    ##                                                  114.3096  
    ##                  neighbourhood_cleansedCaledonia-Fairbank  
    ##                                                   42.5846  
    ##                           neighbourhood_cleansedCasa Loma  
    ##                                                  126.0772  
    ##              neighbourhood_cleansedCentennial Scarborough  
    ##                                                   79.5903  
    ##               neighbourhood_cleansedChurch-Yonge Corridor  
    ##                                                  108.6254  
    ##                 neighbourhood_cleansedClairlea-Birchmount  
    ##                                                   83.5965  
    ##                        neighbourhood_cleansedClanton Park  
    ##                                                   27.3989  
    ##                          neighbourhood_cleansedCliffcrest  
    ##                                                   53.4681  
    ##              neighbourhood_cleansedCorso Italia-Davenport  
    ##                                                   43.4659  
    ##                            neighbourhood_cleansedDanforth  
    ##                                                   70.3989  
    ##                  neighbourhood_cleansedDanforth East York  
    ##                                                   68.7052  
    ##                  neighbourhood_cleansedDon Valley Village  
    ##                                                   47.3967  
    ##                         neighbourhood_cleansedDorset Park  
    ##                                                   39.0113  
    ## neighbourhood_cleansedDovercourt-Wallace Emerson-Junction  
    ##                                                   55.6025  
    ##                neighbourhood_cleansedDownsview-Roding-CFB  
    ##                                                   25.9334  
    ##                      neighbourhood_cleansedDufferin Grove  
    ##                                                   68.4278  
    ##                   neighbourhood_cleansedEast End-Danforth  
    ##                                                   55.3660  
    ##            neighbourhood_cleansedEdenbridge-Humber Valley  
    ##                                                   63.9790  
    ##                       neighbourhood_cleansedEglinton East  
    ##                                                   32.1102  
    ##                    neighbourhood_cleansedElms-Old Rexdale  
    ##                                                   77.2217  
    ##                 neighbourhood_cleansedEnglemount-Lawrence  
    ##                                                   44.6150  
    ##      neighbourhood_cleansedEringate-Centennial-West Deane  
    ##                                                   54.2876  
    ##                 neighbourhood_cleansedEtobicoke West Mall  
    ##                                                   82.7682  
    ##                     neighbourhood_cleansedFlemingdon Park  
    ##                                                   47.0186  
    ##                   neighbourhood_cleansedForest Hill North  
    ##                                                   75.9347  
    ##                   neighbourhood_cleansedForest Hill South  
    ##                                                   48.3891  
    ##              neighbourhood_cleansedGlenfield-Jane Heights  
    ##                                                   37.8042  
    ##                   neighbourhood_cleansedGreenwood-Coxwell  
    ##                                                   73.1405  
    ##                           neighbourhood_cleansedGuildwood  
    ##                                                   24.3660  
    ##                          neighbourhood_cleansedHenry Farm  
    ##                                                   69.4996  
    ##                     neighbourhood_cleansedHigh Park North  
    ##                                                   48.1410  
    ##                   neighbourhood_cleansedHigh Park-Swansea  
    ##                                                   85.6317  
    ##                      neighbourhood_cleansedHighland Creek  
    ##                                                   18.9339  
    ##                   neighbourhood_cleansedHillcrest Village  
    ##                                                    9.0871  
    ##            neighbourhood_cleansedHumber Heights-Westmount  
    ##                                                   72.7037  
    ##                       neighbourhood_cleansedHumber Summit  
    ##                                                   53.7664  
    ##                          neighbourhood_cleansedHumbermede  
    ##                                                   71.5335  
    ##                  neighbourhood_cleansedHumewood-Cedarvale  
    ##                                                   57.5748  
    ##                             neighbourhood_cleansedIonview  
    ##                                                   16.1967  
    ##          neighbourhood_cleansedIslington-City Centre West  
    ##                                                   46.2777  
    ##                       neighbourhood_cleansedJunction Area  
    ##                                                   59.4756  
    ##            neighbourhood_cleansedKeelesdale-Eglinton West  
    ##                                                   40.3071  
    ##                        neighbourhood_cleansedKennedy Park  
    ##                                                   25.0838  
    ##                neighbourhood_cleansedKensington-Chinatown  
    ##                                                   80.0350  
    ##       neighbourhood_cleansedKingsview Village-The Westway  
    ##                                                   61.7142  
    ##                      neighbourhood_cleansedKingsway South  
    ##                                                   69.5278  
    ##                          neighbourhood_cleansedL'Amoreaux  
    ##                                                   -5.6422  
    ##                  neighbourhood_cleansedLambton Baby Point  
    ##                                                   81.9009  
    ##                    neighbourhood_cleansedLansing-Westgate  
    ##                                                   45.9043  
    ##                 neighbourhood_cleansedLawrence Park North  
    ##                                                   60.2457  
    ##                 neighbourhood_cleansedLawrence Park South  
    ##                                                   66.6789  
    ##                  neighbourhood_cleansedLeaside-Bennington  
    ##                                                  113.0356  
    ##                     neighbourhood_cleansedLittle Portugal  
    ##                                                  105.8833  
    ##                         neighbourhood_cleansedLong Branch  
    ##                                                   43.9776  
    ##                             neighbourhood_cleansedMalvern  
    ##                                                   18.7724  
    ##                          neighbourhood_cleansedMaple Leaf  
    ##                                                   69.7713  
    ##                       neighbourhood_cleansedMarkland Wood  
    ##                                                   81.1930  
    ##                            neighbourhood_cleansedMilliken  
    ##                                                   43.4465  
    ## neighbourhood_cleansedMimico (includes Humber Bay Shores)  
    ##                                                  108.8512  
    ##                         neighbourhood_cleansedMorningside  
    ##                                                   25.4431  
    ##                           neighbourhood_cleansedMoss Park  
    ##                                                  110.4414  
    ##                        neighbourhood_cleansedMount Dennis  
    ##                                                   -2.2560  
    ##   neighbourhood_cleansedMount Olive-Silverstone-Jamestown  
    ##                                                   41.9692  
    ##                 neighbourhood_cleansedMount Pleasant East  
    ##                                                   90.4592  
    ##                 neighbourhood_cleansedMount Pleasant West  
    ##                                                   78.2688  
    ##                         neighbourhood_cleansedNew Toronto  
    ##                                                  154.1641  
    ##                    neighbourhood_cleansedNewtonbrook East  
    ##                                                   24.3105  
    ##                    neighbourhood_cleansedNewtonbrook West  
    ##                                                   41.7989  
    ##                             neighbourhood_cleansedNiagara  
    ##                                                  128.7570  
    ##                     neighbourhood_cleansedNorth Riverdale  
    ##                                                   75.0668  
    ##                 neighbourhood_cleansedNorth St.James Town  
    ##                                                   77.5311  
    ##                   neighbourhood_cleansedO'Connor-Parkview  
    ##                                                   48.4269  
    ##                            neighbourhood_cleansedOakridge  
    ##                                                  107.8198  
    ##                     neighbourhood_cleansedOakwood Village  
    ##                                                   44.5250  
    ##                       neighbourhood_cleansedOld East York  
    ##                                                   55.2323  
    ##             neighbourhood_cleansedPalmerston-Little Italy  
    ##                                                   97.8335  
    ##                   neighbourhood_cleansedParkwoods-Donalda  
    ##                                                   -1.7111  
    ##                neighbourhood_cleansedPelmo Park-Humberlea  
    ##                                                   45.6759  
    ##            neighbourhood_cleansedPlayter Estates-Danforth  
    ##                                                   68.1103  
    ##                       neighbourhood_cleansedPleasant View  
    ##                                                   26.9079  
    ##                  neighbourhood_cleansedPrincess-Rosethorn  
    ##                                                   56.6602  
    ##                         neighbourhood_cleansedRegent Park  
    ##                                                  116.7789  
    ##                     neighbourhood_cleansedRexdale-Kipling  
    ##                                                   29.9660  
    ##                   neighbourhood_cleansedRockcliffe-Smythe  
    ##                                                   57.0034  
    ##                        neighbourhood_cleansedRoncesvalles  
    ##                                                   91.5549  
    ##                 neighbourhood_cleansedRosedale-Moore Park  
    ##                                                   95.4218  
    ##                               neighbourhood_cleansedRouge  
    ##                                                   51.9739  
    ##        neighbourhood_cleansedRunnymede-Bloor West Village  
    ##                                                   47.5092  
    ##                              neighbourhood_cleansedRustic  
    ##                                                    1.2657  
    ##                 neighbourhood_cleansedScarborough Village  
    ##                                                   46.3965  
    ##                      neighbourhood_cleansedSouth Parkdale  
    ##                                                   52.8077  
    ##                     neighbourhood_cleansedSouth Riverdale  
    ##                                                   98.0296  
    ##                neighbourhood_cleansedSt.Andrew-Windfields  
    ##                                                  126.3147  
    ##                             neighbourhood_cleansedSteeles  
    ##                                                   34.1486  
    ##                 neighbourhood_cleansedStonegate-Queensway  
    ##                                                   54.7911  
    ##              neighbourhood_cleansedTam O'Shanter-Sullivan  
    ##                                                   16.0197  
    ##                       neighbourhood_cleansedTaylor-Massey  
    ##                                                   42.7337  
    ##                         neighbourhood_cleansedThe Beaches  
    ##                                                   83.8189  
    ##        neighbourhood_cleansedThistletown-Beaumond Heights  
    ##                                                   70.0515  
    ##                    neighbourhood_cleansedThorncliffe Park  
    ##                                                   54.6306  
    ##                   neighbourhood_cleansedTrinity-Bellwoods  
    ##                                                   95.9096  
    ##                          neighbourhood_cleansedUniversity  
    ##                                                   57.6974  
    ##                    neighbourhood_cleansedVictoria Village  
    ##                                                   18.7403  
    ##   neighbourhood_cleansedWaterfront Communities-The Island  
    ##                                                  124.5834  
    ##                           neighbourhood_cleansedWest Hill  
    ##                                                   40.9581  
    ##              neighbourhood_cleansedWest Humber-Clairville  
    ##                                                   50.9106  
    ##                 neighbourhood_cleansedWestminster-Branson  
    ##                                                   33.8150  
    ##                              neighbourhood_cleansedWeston  
    ##                                                   45.7239  
    ##                  neighbourhood_cleansedWeston-Pellam Park  
    ##                                                   77.8994  
    ##                    neighbourhood_cleansedWexford/Maryvale  
    ##                                                   34.0869  
    ##                     neighbourhood_cleansedWillowdale East  
    ##                                                   64.1892  
    ##                     neighbourhood_cleansedWillowdale West  
    ##                                                   47.7324  
    ##    neighbourhood_cleansedWillowridge-Martingrove-Richview  
    ##                                                   27.5708  
    ##                              neighbourhood_cleansedWoburn  
    ##                                                   41.8831  
    ##                   neighbourhood_cleansedWoodbine Corridor  
    ##                                                  103.7267  
    ##                    neighbourhood_cleansedWoodbine-Lumsden  
    ##                                                   55.7153  
    ##                            neighbourhood_cleansedWychwood  
    ##                                                   76.1031  
    ##                      neighbourhood_cleansedYonge-Eglinton  
    ##                                                   70.3476  
    ##                      neighbourhood_cleansedYonge-St.Clair  
    ##                                                   89.7613  
    ##             neighbourhood_cleansedYork University Heights  
    ##                                                   34.1811  
    ##                  neighbourhood_cleansedYorkdale-Glen Park  
    ##                                                   58.0803  
    ##                                 review_scores_cleanliness  
    ##                                                    5.3503  
    ##                                         reviews_per_month  
    ##                                                   -4.1829  
    ##                                        host_response_rate  
    ##                                                   -0.1815

We can see from the AIC backwards selection that the resulting linear
model has variables:

> bedrooms

> bathrooms\_numeric

> minimum\_nights

> neighbourhood\_cleansed

> review\_scores\_cleanliness

> reviews\_per\_month

> host\_response\_rate

### Prediction of Price:

We can now see how the resulting model preforms in prediction.

``` r
#initial price model
price_inital_model <- lm(formula = price ~ bedrooms + bathrooms_numeric + neighbourhood_cleansed + 
    review_scores_cleanliness + reviews_per_month + host_response_rate, 
    data = feb_21_airbnb_train)
  

#summary of the initial price model
summary(price_inital_model)
```

    ## 
    ## Call:
    ## lm(formula = price ~ bedrooms + bathrooms_numeric + neighbourhood_cleansed + 
    ##     review_scores_cleanliness + reviews_per_month + host_response_rate, 
    ##     data = feb_21_airbnb_train)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -539.0  -41.9  -11.6   20.6 3373.8 
    ## 
    ## Coefficients:
    ##                                                             Estimate Std. Error
    ## (Intercept)                                               -121.73555   54.33105
    ## bedrooms                                                    61.93025    2.75698
    ## bathrooms_numeric                                           33.99262    4.03320
    ## neighbourhood_cleansedAgincourt South-Malvern West          28.21504   55.88989
    ## neighbourhood_cleansedAlderwood                             59.27816   68.48316
    ## neighbourhood_cleansedAnnex                                102.93397   52.81809
    ## neighbourhood_cleansedBanbury-Don Mills                     57.54450   64.28410
    ## neighbourhood_cleansedBathurst Manor                       185.20958   63.35810
    ## neighbourhood_cleansedBay Street Corridor                  105.55837   52.84278
    ## neighbourhood_cleansedBayview Village                       66.10069   57.35465
    ## neighbourhood_cleansedBayview Woods-Steeles                 48.34290   56.56695
    ## neighbourhood_cleansedBedford Park-Nortown                   4.86307   57.41636
    ## neighbourhood_cleansedBeechborough-Greenbrook               36.29071   70.53859
    ## neighbourhood_cleansedBendale                               57.93370   59.77398
    ## neighbourhood_cleansedBirchcliffe-Cliffside                 21.83151   56.85558
    ## neighbourhood_cleansedBlack Creek                         -106.11085   70.59377
    ## neighbourhood_cleansedBlake-Jones                           65.69570   59.38997
    ## neighbourhood_cleansedBriar Hill-Belgravia                  30.60899   62.52848
    ## neighbourhood_cleansedBridle Path-Sunnybrook-York Mills     56.27332   66.79433
    ## neighbourhood_cleansedBroadview North                      168.92484   66.80095
    ## neighbourhood_cleansedBrookhaven-Amesbury                   25.15417   70.48586
    ## neighbourhood_cleansedCabbagetown-South St.James Town      114.30956   54.72721
    ## neighbourhood_cleansedCaledonia-Fairbank                    42.58461   60.19972
    ## neighbourhood_cleansedCasa Loma                            126.07717   66.78579
    ## neighbourhood_cleansedCentennial Scarborough                79.59029   81.77672
    ## neighbourhood_cleansedChurch-Yonge Corridor                108.62539   52.96568
    ## neighbourhood_cleansedClairlea-Birchmount                   83.59650   59.02394
    ## neighbourhood_cleansedClanton Park                          27.39891   58.35248
    ## neighbourhood_cleansedCliffcrest                            53.46806   61.83344
    ## neighbourhood_cleansedCorso Italia-Davenport                43.46585   55.65210
    ## neighbourhood_cleansedDanforth                              70.39895   59.36977
    ## neighbourhood_cleansedDanforth East York                    68.70522   60.68530
    ## neighbourhood_cleansedDon Valley Village                    47.39669   57.41356
    ## neighbourhood_cleansedDorset Park                           39.01126   58.99556
    ## neighbourhood_cleansedDovercourt-Wallace Emerson-Junction   55.60253   53.26090
    ## neighbourhood_cleansedDownsview-Roding-CFB                  25.93339   58.67069
    ## neighbourhood_cleansedDufferin Grove                        68.42775   54.96315
    ## neighbourhood_cleansedEast End-Danforth                     55.36604   55.03661
    ## neighbourhood_cleansedEdenbridge-Humber Valley              63.97905   81.73086
    ## neighbourhood_cleansedEglinton East                         32.11023   66.76894
    ## neighbourhood_cleansedElms-Old Rexdale                      77.22166   73.13486
    ## neighbourhood_cleansedEnglemount-Lawrence                   44.61502   60.65020
    ## neighbourhood_cleansedEringate-Centennial-West Deane        54.28762   59.35495
    ## neighbourhood_cleansedEtobicoke West Mall                   82.76816   70.46154
    ## neighbourhood_cleansedFlemingdon Park                       47.01862   89.57147
    ## neighbourhood_cleansedForest Hill North                     75.93472   76.73066
    ## neighbourhood_cleansedForest Hill South                     48.38913   89.56093
    ## neighbourhood_cleansedGlenfield-Jane Heights                37.80424   68.44033
    ## neighbourhood_cleansedGreenwood-Coxwell                     73.14046   55.59482
    ## neighbourhood_cleansedGuildwood                             24.36596   76.71794
    ## neighbourhood_cleansedHenry Farm                            69.49965   61.80929
    ## neighbourhood_cleansedHigh Park North                       48.14101   56.03278
    ## neighbourhood_cleansedHigh Park-Swansea                     85.63168   54.36163
    ## neighbourhood_cleansedHighland Creek                        18.93394   61.85518
    ## neighbourhood_cleansedHillcrest Village                      9.08714   61.18600
    ## neighbourhood_cleansedHumber Heights-Westmount              72.70366   73.22619
    ## neighbourhood_cleansedHumber Summit                         53.76641  103.39917
    ## neighbourhood_cleansedHumbermede                            71.53351   70.49171
    ## neighbourhood_cleansedHumewood-Cedarvale                    57.57482   59.30140
    ## neighbourhood_cleansedIonview                               16.19674   59.01228
    ## neighbourhood_cleansedIslington-City Centre West            46.27765   55.08429
    ## neighbourhood_cleansedJunction Area                         59.47560   55.37449
    ## neighbourhood_cleansedKeelesdale-Eglinton West              40.30712   64.28998
    ## neighbourhood_cleansedKennedy Park                          25.08384   58.94586
    ## neighbourhood_cleansedKensington-Chinatown                  80.03500   52.99185
    ## neighbourhood_cleansedKingsview Village-The Westway         61.71418   66.86352
    ## neighbourhood_cleansedKingsway South                        69.52776  103.45350
    ## neighbourhood_cleansedL'Amoreaux                            -5.64220   58.64732
    ## neighbourhood_cleansedLambton Baby Point                    81.90088   76.69245
    ## neighbourhood_cleansedLansing-Westgate                      45.90434   55.23227
    ## neighbourhood_cleansedLawrence Park North                   60.24570   59.37516
    ## neighbourhood_cleansedLawrence Park South                   66.67885   60.69304
    ## neighbourhood_cleansedLeaside-Bennington                   113.03558   61.82030
    ## neighbourhood_cleansedLittle Portugal                      105.88329   53.53351
    ## neighbourhood_cleansedLong Branch                           43.97762   60.16697
    ## neighbourhood_cleansedMalvern                               18.77242   55.47686
    ## neighbourhood_cleansedMaple Leaf                            69.77125   73.15677
    ## neighbourhood_cleansedMarkland Wood                         81.19300   73.13911
    ## neighbourhood_cleansedMilliken                              43.44646   70.45934
    ## neighbourhood_cleansedMimico (includes Humber Bay Shores)  108.85120   54.59676
    ## neighbourhood_cleansedMorningside                           25.44315   64.27982
    ## neighbourhood_cleansedMoss Park                            110.44136   52.92790
    ## neighbourhood_cleansedMount Dennis                          -2.25602   64.28681
    ## neighbourhood_cleansedMount Olive-Silverstone-Jamestown     41.96917   70.51696
    ## neighbourhood_cleansedMount Pleasant East                   90.45922   60.16782
    ## neighbourhood_cleansedMount Pleasant West                   78.26881   54.22280
    ## neighbourhood_cleansedNew Toronto                          154.16413   60.13425
    ## neighbourhood_cleansedNewtonbrook East                      24.31046   54.68252
    ## neighbourhood_cleansedNewtonbrook West                      41.79886   54.55382
    ## neighbourhood_cleansedNiagara                              128.75699   52.76414
    ## neighbourhood_cleansedNorth Riverdale                       75.06681   56.71663
    ## neighbourhood_cleansedNorth St.James Town                   77.53105   56.83802
    ## neighbourhood_cleansedO'Connor-Parkview                     48.42686   58.69231
    ## neighbourhood_cleansedOakridge                             107.81982   61.81993
    ## neighbourhood_cleansedOakwood Village                       44.52500   55.91769
    ## neighbourhood_cleansedOld East York                         55.23230   59.03546
    ## neighbourhood_cleansedPalmerston-Little Italy               97.83354   53.52078
    ## neighbourhood_cleansedParkwoods-Donalda                     -1.71112   61.84688
    ## neighbourhood_cleansedPelmo Park-Humberlea                  45.67588   65.45488
    ## neighbourhood_cleansedPlayter Estates-Danforth              68.11035   60.69467
    ## neighbourhood_cleansedPleasant View                         26.90795   57.20589
    ## neighbourhood_cleansedPrincess-Rosethorn                    56.66022   63.36488
    ## neighbourhood_cleansedRegent Park                          116.77893   58.39128
    ## neighbourhood_cleansedRexdale-Kipling                       29.96597   65.47545
    ## neighbourhood_cleansedRockcliffe-Smythe                     57.00338   73.18242
    ## neighbourhood_cleansedRoncesvalles                          91.55488   54.83240
    ## neighbourhood_cleansedRosedale-Moore Park                   95.42178   58.36142
    ## neighbourhood_cleansedRouge                                 51.97388   62.50737
    ## neighbourhood_cleansedRunnymede-Bloor West Village          47.50919   66.80825
    ## neighbourhood_cleansedRustic                                 1.26572  136.83056
    ## neighbourhood_cleansedScarborough Village                   46.39654   89.58206
    ## neighbourhood_cleansedSouth Parkdale                        52.80769   53.77613
    ## neighbourhood_cleansedSouth Riverdale                       98.02956   53.43673
    ## neighbourhood_cleansedSt.Andrew-Windfields                 126.31466   59.74624
    ## neighbourhood_cleansedSteeles                               34.14864   60.15610
    ## neighbourhood_cleansedStonegate-Queensway                   54.79114   55.47941
    ## neighbourhood_cleansedTam O'Shanter-Sullivan                16.01971   57.60476
    ## neighbourhood_cleansedTaylor-Massey                         42.73372   66.76877
    ## neighbourhood_cleansedThe Beaches                           83.81887   54.81486
    ## neighbourhood_cleansedThistletown-Beaumond Heights          70.05145   89.54699
    ## neighbourhood_cleansedThorncliffe Park                      54.63062   68.42841
    ## neighbourhood_cleansedTrinity-Bellwoods                     95.90956   52.90327
    ## neighbourhood_cleansedUniversity                            57.69736   55.49870
    ## neighbourhood_cleansedVictoria Village                      18.74031   63.36410
    ## neighbourhood_cleansedWaterfront Communities-The Island    124.58335   51.90122
    ## neighbourhood_cleansedWest Hill                             40.95810   58.69203
    ## neighbourhood_cleansedWest Humber-Clairville                50.91058   57.18502
    ## neighbourhood_cleansedWestminster-Branson                   33.81502   59.36025
    ## neighbourhood_cleansedWeston                                45.72387   60.17892
    ## neighbourhood_cleansedWeston-Pellam Park                    77.89941   57.83086
    ## neighbourhood_cleansedWexford/Maryvale                      34.08688   56.00245
    ## neighbourhood_cleansedWillowdale East                       64.18921   52.79958
    ## neighbourhood_cleansedWillowdale West                       47.73237   55.88899
    ## neighbourhood_cleansedWillowridge-Martingrove-Richview      27.57079   58.99314
    ## neighbourhood_cleansedWoburn                                41.88305   58.09477
    ## neighbourhood_cleansedWoodbine Corridor                    103.72668   58.99319
    ## neighbourhood_cleansedWoodbine-Lumsden                      55.71530   63.35895
    ## neighbourhood_cleansedWychwood                              76.10310   56.70259
    ## neighbourhood_cleansedYonge-Eglinton                        70.34756   58.07453
    ## neighbourhood_cleansedYonge-St.Clair                        89.76135   61.82675
    ## neighbourhood_cleansedYork University Heights               34.18108   53.69382
    ## neighbourhood_cleansedYorkdale-Glen Park                    58.08027   54.54544
    ## review_scores_cleanliness                                    5.35030    1.76747
    ## reviews_per_month                                           -4.18285    1.18274
    ## host_response_rate                                          -0.18150    0.07568
    ##                                                           t value Pr(>|t|)    
    ## (Intercept)                                                -2.241 0.025095 *  
    ## bedrooms                                                   22.463  < 2e-16 ***
    ## bathrooms_numeric                                           8.428  < 2e-16 ***
    ## neighbourhood_cleansedAgincourt South-Malvern West          0.505 0.613699    
    ## neighbourhood_cleansedAlderwood                             0.866 0.386758    
    ## neighbourhood_cleansedAnnex                                 1.949 0.051371 .  
    ## neighbourhood_cleansedBanbury-Don Mills                     0.895 0.370746    
    ## neighbourhood_cleansedBathurst Manor                        2.923 0.003480 ** 
    ## neighbourhood_cleansedBay Street Corridor                   1.998 0.045816 *  
    ## neighbourhood_cleansedBayview Village                       1.152 0.249175    
    ## neighbourhood_cleansedBayview Woods-Steeles                 0.855 0.392806    
    ## neighbourhood_cleansedBedford Park-Nortown                  0.085 0.932505    
    ## neighbourhood_cleansedBeechborough-Greenbrook               0.514 0.606939    
    ## neighbourhood_cleansedBendale                               0.969 0.332487    
    ## neighbourhood_cleansedBirchcliffe-Cliffside                 0.384 0.701008    
    ## neighbourhood_cleansedBlack Creek                          -1.503 0.132872    
    ## neighbourhood_cleansedBlake-Jones                           1.106 0.268705    
    ## neighbourhood_cleansedBriar Hill-Belgravia                  0.490 0.624495    
    ## neighbourhood_cleansedBridle Path-Sunnybrook-York Mills     0.842 0.399556    
    ## neighbourhood_cleansedBroadview North                       2.529 0.011477 *  
    ## neighbourhood_cleansedBrookhaven-Amesbury                   0.357 0.721206    
    ## neighbourhood_cleansedCabbagetown-South St.James Town       2.089 0.036784 *  
    ## neighbourhood_cleansedCaledonia-Fairbank                    0.707 0.479358    
    ## neighbourhood_cleansedCasa Loma                             1.888 0.059113 .  
    ## neighbourhood_cleansedCentennial Scarborough                0.973 0.330470    
    ## neighbourhood_cleansedChurch-Yonge Corridor                 2.051 0.040333 *  
    ## neighbourhood_cleansedClairlea-Birchmount                   1.416 0.156746    
    ## neighbourhood_cleansedClanton Park                          0.470 0.638703    
    ## neighbourhood_cleansedCliffcrest                            0.865 0.387239    
    ## neighbourhood_cleansedCorso Italia-Davenport                0.781 0.434823    
    ## neighbourhood_cleansedDanforth                              1.186 0.235770    
    ## neighbourhood_cleansedDanforth East York                    1.132 0.257624    
    ## neighbourhood_cleansedDon Valley Village                    0.826 0.409110    
    ## neighbourhood_cleansedDorset Park                           0.661 0.508478    
    ## neighbourhood_cleansedDovercourt-Wallace Emerson-Junction   1.044 0.296553    
    ## neighbourhood_cleansedDownsview-Roding-CFB                  0.442 0.658497    
    ## neighbourhood_cleansedDufferin Grove                        1.245 0.213200    
    ## neighbourhood_cleansedEast End-Danforth                     1.006 0.314472    
    ## neighbourhood_cleansedEdenbridge-Humber Valley              0.783 0.433781    
    ## neighbourhood_cleansedEglinton East                         0.481 0.630598    
    ## neighbourhood_cleansedElms-Old Rexdale                      1.056 0.291075    
    ## neighbourhood_cleansedEnglemount-Lawrence                   0.736 0.462002    
    ## neighbourhood_cleansedEringate-Centennial-West Deane        0.915 0.360432    
    ## neighbourhood_cleansedEtobicoke West Mall                   1.175 0.240188    
    ## neighbourhood_cleansedFlemingdon Park                       0.525 0.599656    
    ## neighbourhood_cleansedForest Hill North                     0.990 0.322405    
    ## neighbourhood_cleansedForest Hill South                     0.540 0.589019    
    ## neighbourhood_cleansedGlenfield-Jane Heights                0.552 0.580721    
    ## neighbourhood_cleansedGreenwood-Coxwell                     1.316 0.188370    
    ## neighbourhood_cleansedGuildwood                             0.318 0.750798    
    ## neighbourhood_cleansedHenry Farm                            1.124 0.260889    
    ## neighbourhood_cleansedHigh Park North                       0.859 0.390295    
    ## neighbourhood_cleansedHigh Park-Swansea                     1.575 0.115269    
    ## neighbourhood_cleansedHighland Creek                        0.306 0.759541    
    ## neighbourhood_cleansedHillcrest Village                     0.149 0.881941    
    ## neighbourhood_cleansedHumber Heights-Westmount              0.993 0.320825    
    ## neighbourhood_cleansedHumber Summit                         0.520 0.603095    
    ## neighbourhood_cleansedHumbermede                            1.015 0.310261    
    ## neighbourhood_cleansedHumewood-Cedarvale                    0.971 0.331653    
    ## neighbourhood_cleansedIonview                               0.274 0.783740    
    ## neighbourhood_cleansedIslington-City Centre West            0.840 0.400879    
    ## neighbourhood_cleansedJunction Area                         1.074 0.282848    
    ## neighbourhood_cleansedKeelesdale-Eglinton West              0.627 0.530716    
    ## neighbourhood_cleansedKennedy Park                          0.426 0.670461    
    ## neighbourhood_cleansedKensington-Chinatown                  1.510 0.131024    
    ## neighbourhood_cleansedKingsview Village-The Westway         0.923 0.356059    
    ## neighbourhood_cleansedKingsway South                        0.672 0.501572    
    ## neighbourhood_cleansedL'Amoreaux                           -0.096 0.923361    
    ## neighbourhood_cleansedLambton Baby Point                    1.068 0.285612    
    ## neighbourhood_cleansedLansing-Westgate                      0.831 0.405949    
    ## neighbourhood_cleansedLawrence Park North                   1.015 0.310317    
    ## neighbourhood_cleansedLawrence Park South                   1.099 0.271985    
    ## neighbourhood_cleansedLeaside-Bennington                    1.828 0.067542 .  
    ## neighbourhood_cleansedLittle Portugal                       1.978 0.047997 *  
    ## neighbourhood_cleansedLong Branch                           0.731 0.464859    
    ## neighbourhood_cleansedMalvern                               0.338 0.735089    
    ## neighbourhood_cleansedMaple Leaf                            0.954 0.340271    
    ## neighbourhood_cleansedMarkland Wood                         1.110 0.267002    
    ## neighbourhood_cleansedMilliken                              0.617 0.537515    
    ## neighbourhood_cleansedMimico (includes Humber Bay Shores)   1.994 0.046236 *  
    ## neighbourhood_cleansedMorningside                           0.396 0.692256    
    ## neighbourhood_cleansedMoss Park                             2.087 0.036972 *  
    ## neighbourhood_cleansedMount Dennis                         -0.035 0.972007    
    ## neighbourhood_cleansedMount Olive-Silverstone-Jamestown     0.595 0.551761    
    ## neighbourhood_cleansedMount Pleasant East                   1.503 0.132787    
    ## neighbourhood_cleansedMount Pleasant West                   1.443 0.148952    
    ## neighbourhood_cleansedNew Toronto                           2.564 0.010387 *  
    ## neighbourhood_cleansedNewtonbrook East                      0.445 0.656647    
    ## neighbourhood_cleansedNewtonbrook West                      0.766 0.443597    
    ## neighbourhood_cleansedNiagara                               2.440 0.014713 *  
    ## neighbourhood_cleansedNorth Riverdale                       1.324 0.185717    
    ## neighbourhood_cleansedNorth St.James Town                   1.364 0.172608    
    ## neighbourhood_cleansedO'Connor-Parkview                     0.825 0.409356    
    ## neighbourhood_cleansedOakridge                              1.744 0.081205 .  
    ## neighbourhood_cleansedOakwood Village                       0.796 0.425919    
    ## neighbourhood_cleansedOld East York                         0.936 0.349536    
    ## neighbourhood_cleansedPalmerston-Little Italy               1.828 0.067617 .  
    ## neighbourhood_cleansedParkwoods-Donalda                    -0.028 0.977929    
    ## neighbourhood_cleansedPelmo Park-Humberlea                  0.698 0.485321    
    ## neighbourhood_cleansedPlayter Estates-Danforth              1.122 0.261840    
    ## neighbourhood_cleansedPleasant View                         0.470 0.638111    
    ## neighbourhood_cleansedPrincess-Rosethorn                    0.894 0.371264    
    ## neighbourhood_cleansedRegent Park                           2.000 0.045562 *  
    ## neighbourhood_cleansedRexdale-Kipling                       0.458 0.647212    
    ## neighbourhood_cleansedRockcliffe-Smythe                     0.779 0.436063    
    ## neighbourhood_cleansedRoncesvalles                          1.670 0.095038 .  
    ## neighbourhood_cleansedRosedale-Moore Park                   1.635 0.102110    
    ## neighbourhood_cleansedRouge                                 0.831 0.405740    
    ## neighbourhood_cleansedRunnymede-Bloor West Village          0.711 0.477039    
    ## neighbourhood_cleansedRustic                                0.009 0.992620    
    ## neighbourhood_cleansedScarborough Village                   0.518 0.604536    
    ## neighbourhood_cleansedSouth Parkdale                        0.982 0.326152    
    ## neighbourhood_cleansedSouth Riverdale                       1.834 0.066640 .  
    ## neighbourhood_cleansedSt.Andrew-Windfields                  2.114 0.034549 *  
    ## neighbourhood_cleansedSteeles                               0.568 0.570287    
    ## neighbourhood_cleansedStonegate-Queensway                   0.988 0.323400    
    ## neighbourhood_cleansedTam O'Shanter-Sullivan                0.278 0.780950    
    ## neighbourhood_cleansedTaylor-Massey                         0.640 0.522186    
    ## neighbourhood_cleansedThe Beaches                           1.529 0.126297    
    ## neighbourhood_cleansedThistletown-Beaumond Heights          0.782 0.434083    
    ## neighbourhood_cleansedThorncliffe Park                      0.798 0.424699    
    ## neighbourhood_cleansedTrinity-Bellwoods                     1.813 0.069904 .  
    ## neighbourhood_cleansedUniversity                            1.040 0.298569    
    ## neighbourhood_cleansedVictoria Village                      0.296 0.767429    
    ## neighbourhood_cleansedWaterfront Communities-The Island     2.400 0.016414 *  
    ## neighbourhood_cleansedWest Hill                             0.698 0.485305    
    ## neighbourhood_cleansedWest Humber-Clairville                0.890 0.373360    
    ## neighbourhood_cleansedWestminster-Branson                   0.570 0.568936    
    ## neighbourhood_cleansedWeston                                0.760 0.447411    
    ## neighbourhood_cleansedWeston-Pellam Park                    1.347 0.178035    
    ## neighbourhood_cleansedWexford/Maryvale                      0.609 0.542773    
    ## neighbourhood_cleansedWillowdale East                       1.216 0.224152    
    ## neighbourhood_cleansedWillowdale West                       0.854 0.393115    
    ## neighbourhood_cleansedWillowridge-Martingrove-Richview      0.467 0.640266    
    ## neighbourhood_cleansedWoburn                                0.721 0.470978    
    ## neighbourhood_cleansedWoodbine Corridor                     1.758 0.078761 .  
    ## neighbourhood_cleansedWoodbine-Lumsden                      0.879 0.379249    
    ## neighbourhood_cleansedWychwood                              1.342 0.179611    
    ## neighbourhood_cleansedYonge-Eglinton                        1.211 0.225826    
    ## neighbourhood_cleansedYonge-St.Clair                        1.452 0.146615    
    ## neighbourhood_cleansedYork University Heights               0.637 0.524420    
    ## neighbourhood_cleansedYorkdale-Glen Park                    1.065 0.287016    
    ## review_scores_cleanliness                                   3.027 0.002482 ** 
    ## reviews_per_month                                          -3.537 0.000409 ***
    ## host_response_rate                                         -2.398 0.016516 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 126.6 on 4942 degrees of freedom
    ## Multiple R-squared:  0.2705, Adjusted R-squared:  0.2492 
    ## F-statistic: 12.72 on 144 and 4942 DF,  p-value: < 2.2e-16

``` r
#making price predictions
price_predictions <- c(predict(price_inital_model, feb_21_airbnb_test))
#true values of price
price_feb <- c(feb_21_airbnb_test$price)
#computing RMSE with the yardstick package
yardstick::rmse_vec(price_feb, price_predictions)
```

    ## [1] 333.8244

We can see that both the RMSE is high (333.8244), and the R-squared for
the model is low (0.2492). This indicates that this linear regression
model preformed poorly in predicting price in February 2021.
