# Week 1 Reflection:

> This week I look to set the stage for the project. This project will
> look towards assessing the impact of COVID-19 on Toronto Airbnb prices
> and listings.

> I start out by providing background on the effect of COVID-19 on
> Airbnb, Airbnb in Toronto, and background on COVID-19 in Toronto.

> As this project will likely involve spatial modelling, I also provide
> a some review of Paula Moraga’s “Small Area Disease Risk Estimation
> and Visualization Using R”

## Setting the stage for the Project

**COVID-19 effect on Airbnb:**

> With the fluctuating COVID-19 restrictions on travel, and with the
> risks that travelling during COVID-19 impose, it is no question that
> Airbnb whose market relies heavily on tourism has taken a significant
> hit during the COVID-19 pandemic. In May 2020, Airbnb had laid off a
> quarter of its workforce (1) and with no end to the pandemic in sight,
> there is no way to predict if the organization will ever recover it’s
> pre-pandemic success.

> This loss, while significant to the Airbnb organization, can be felt
> much further down by Airbnb hosts. Many hosts, whose primary income
> comes from rental of the units they provide, are stuck with bills and
> mortgages to pay, sometimes for many units.

**Airbnb in Toronto:**

> In April 2020, Ontario had banned all short term rentals (a rental
> period of less than 28 days) unless the unit is being rented to
> someone in need of housing during the “Emergency Order”. In September
> 2020, Toronto laid out a series of short term rental reforms. These
> reforms require short-term rental operators to need to register with
> the city in order to allow short-term tenants. The reforms also state
> that these landlords m ay only be allowed to rent out their primary
> residence for up to 180 nights per year.

**COVID-19 in Toronto:**

> In March 2020, Ontario Premier Doug Ford declared a provincial state
> of emergency which prompted the closing of many non-essential
> businesses and facilities across Ontario. In late July 2020, Toronto
> and Peel region were allowed to enter stage 3 of the Ontario reopening
> plan. In October 2020, Peel region, Toronto and Ottawa were rolled
> back to modified stage 2 for 28 days. In November 2020, Doug Ford
> announced that the province would move towards a 5-tier system to
> determine restrictions. Mid-November Toronto was announced to move
> into the Red Tier “Control” to provide more restrictions, and finally
> on November 20th, Ford announced Toronto and Peel region would be
> moved into Lockdown effective November 23rd. This prompted all
> non-essential businesses to be closed along with all dine-in
> resturants, gyms, and personal care services. In Janurary 2021, Doug
> Ford declared a second declaration of emergency, and stay-at-home
> orders were declared for the province. In March 2021, Toronto and Peel
> region were able to exit stay-at-home orders and Toronto and Peel
> region entered Grey Tier “Lockdown”. In April 2021, Ford announced the
> province would move into White Tier “Shutdown”, which would impose the
> same measures put in place during the original province wide shutdown
> in March 2020.

Sources:

1.  Source
    <https://www.theguardian.com/technology/2020/may/06/airbnb-to-make-quarter-of-its-global-workforce-redundant>

2.  <https://www.weirfoulds.com/the-long-term-impact-of-covid-19-on-short-term-rentals>

3.  <https://shorttermrentalz.com/news/toronto-rental-reforms-september/>

## Literature:

Paula Moraga’s “Small Area Disease Risk Estimation and Visualization
Using R”

<https://journal.r-project.org/archive/2018/RJ-2018-036/RJ-2018-036.pdf>

This article explores how R can be used to obtain disease risk estimates
and quantify risk factors using areal data. This article uses an example
of Lung Cancer risk in Pennsylvania. Visualization of this data is shown
using the **leaflet** package, and Bayesian inference is conducted using
the **INLA** package in R.

According to Moraga, Linear mixed models are used in this article as
they enable improvement of local estimates by accommodating spatial
correlation and the effects of Explanatory Variables. The Bayesian
inference in these models is conducted using the Integrated Nested
Laplace Approximation (INLA) approach.

According to Moraga, limitations to the disease models in this article
is that they are subject to ecological bias. Moraga writes that this
ecological bias occurs when associations obtained from analyses that use
variables at an aggregated level lead to conclusions different from
analyses that use these variables at an individual level.
