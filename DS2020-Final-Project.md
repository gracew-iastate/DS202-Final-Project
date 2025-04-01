DS2020 Final Project
================
Grace Wu, Naomi Mauss

## Health Data in 500 Cities

### Data

#### Description of Dataset:

- This dataset includes information regarding chronic diseases related
  to three categories (unhalthy behaviors, health outcomes, and
  prevention). The data was provided by the CDC and consists of 500 of
  the largest cities.

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.4     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
Health <- read.csv("C:\\Users\\Grace\\Downloads\\500_Cities__Local_Data_for_Better_Health__2019_release_20250331.csv")
```

#### Data Cleaning

``` r
Health <- Health[complete.cases(Health), ]
```

#### Marginal Summaries

``` r
head(Health)
```

    ##    Year StateAbbr  StateDesc   CityName GeographicLevel DataSource
    ## 1  2017        CA California  Hawthorne    Census Tract      BRFSS
    ## 6  2017        CA California      Indio    Census Tract      BRFSS
    ## 9  2017        CA California  Inglewood    Census Tract      BRFSS
    ## 16 2016        AL    Alabama     Hoover    Census Tract      BRFSS
    ## 19 2017        AL    Alabama Huntsville    Census Tract      BRFSS
    ## 20 2017        AL    Alabama Huntsville    Census Tract      BRFSS
    ##               Category            UniqueID
    ## 1      Health Outcomes 0632548-06037602504
    ## 6      Health Outcomes 0636448-06065045213
    ## 9      Health Outcomes 0636546-06037601801
    ## 16          Prevention 0135896-01073014302
    ## 19     Health Outcomes 0137000-01089002922
    ## 20 Unhealthy Behaviors 0137000-01089010612
    ##                                            Measure Data_Value_Unit
    ## 1           Arthritis among adults aged >=18 Years               %
    ## 6           Arthritis among adults aged >=18 Years               %
    ## 9  Diagnosed diabetes among adults aged >=18 Years               %
    ## 16    Mammography use among women aged 50–74 Years               %
    ## 19 Diagnosed diabetes among adults aged >=18 Years               %
    ## 20            Obesity among adults aged >=18 Years               %
    ##    DataValueTypeID  Data_Value_Type Data_Value Low_Confidence_Limit
    ## 1           CrdPrv Crude prevalence       14.6                 13.9
    ## 6           CrdPrv Crude prevalence       22.0                 21.1
    ## 9           CrdPrv Crude prevalence       12.7                 12.0
    ## 16          CrdPrv Crude prevalence       81.9                 78.7
    ## 19          CrdPrv Crude prevalence        9.3                  8.5
    ## 20          CrdPrv Crude prevalence       30.3                 29.2
    ##    High_Confidence_Limit Data_Value_Footnote_Symbol Data_Value_Footnote
    ## 1                   15.2                                               
    ## 6                   22.8                                               
    ## 9                   13.5                                               
    ## 16                  84.5                                               
    ## 19                  10.3                                               
    ## 20                  31.5                                               
    ##    PopulationCount                     GeoLocation CategoryID MeasureId
    ## 1             4407  (33.905547923, -118.337332298)    HLTHOUT ARTHRITIS
    ## 6             5006 (33.7144617083, -116.258246324)    HLTHOUT ARTHRITIS
    ## 9             2472 (33.9439711273, -118.349937728)    HLTHOUT  DIABETES
    ## 16            1636 (33.3923792867, -86.8833755105)    PREVENT  MAMMOUSE
    ## 19            4387  (34.612755588, -86.5329568642)    HLTHOUT  DIABETES
    ## 20            2654 (34.7636374497, -86.7500225775)     UNHBEH   OBESITY
    ##    CityFIPS  TractFIPS Short_Question_Text
    ## 1    632548 6037602504           Arthritis
    ## 6    636448 6065045213           Arthritis
    ## 9    636546 6037601801            Diabetes
    ## 16   135896 1073014302         Mammography
    ## 19   137000 1089002922            Diabetes
    ## 20   137000 1089010612             Obesity

``` r
dim(Health)
```

    ## [1] 759349     24

``` r
str(Health)
```

    ## 'data.frame':    759349 obs. of  24 variables:
    ##  $ Year                      : int  2017 2017 2017 2016 2017 2017 2017 2016 2017 2016 ...
    ##  $ StateAbbr                 : chr  "CA" "CA" "CA" "AL" ...
    ##  $ StateDesc                 : chr  "California" "California" "California" "Alabama" ...
    ##  $ CityName                  : chr  "Hawthorne" "Indio" "Inglewood" "Hoover" ...
    ##  $ GeographicLevel           : chr  "Census Tract" "Census Tract" "Census Tract" "Census Tract" ...
    ##  $ DataSource                : chr  "BRFSS" "BRFSS" "BRFSS" "BRFSS" ...
    ##  $ Category                  : chr  "Health Outcomes" "Health Outcomes" "Health Outcomes" "Prevention" ...
    ##  $ UniqueID                  : chr  "0632548-06037602504" "0636448-06065045213" "0636546-06037601801" "0135896-01073014302" ...
    ##  $ Measure                   : chr  "Arthritis among adults aged >=18 Years" "Arthritis among adults aged >=18 Years" "Diagnosed diabetes among adults aged >=18 Years" "Mammography use among women aged 50–74 Years" ...
    ##  $ Data_Value_Unit           : chr  "%" "%" "%" "%" ...
    ##  $ DataValueTypeID           : chr  "CrdPrv" "CrdPrv" "CrdPrv" "CrdPrv" ...
    ##  $ Data_Value_Type           : chr  "Crude prevalence" "Crude prevalence" "Crude prevalence" "Crude prevalence" ...
    ##  $ Data_Value                : num  14.6 22 12.7 81.9 9.3 30.3 7.4 74 30.6 81.2 ...
    ##  $ Low_Confidence_Limit      : num  13.9 21.1 12 78.7 8.5 29.2 6.9 69.4 29.6 77.7 ...
    ##  $ High_Confidence_Limit     : num  15.2 22.8 13.5 84.5 10.3 31.5 7.8 77.9 31.5 83.5 ...
    ##  $ Data_Value_Footnote_Symbol: chr  "" "" "" "" ...
    ##  $ Data_Value_Footnote       : chr  "" "" "" "" ...
    ##  $ PopulationCount           : int  4407 5006 2472 1636 4387 2654 4993 5217 3978 4362 ...
    ##  $ GeoLocation               : chr  "(33.905547923, -118.337332298)" "(33.7144617083, -116.258246324)" "(33.9439711273, -118.349937728)" "(33.3923792867, -86.8833755105)" ...
    ##  $ CategoryID                : chr  "HLTHOUT" "HLTHOUT" "HLTHOUT" "PREVENT" ...
    ##  $ MeasureId                 : chr  "ARTHRITIS" "ARTHRITIS" "DIABETES" "MAMMOUSE" ...
    ##  $ CityFIPS                  : int  632548 636448 636546 135896 137000 137000 203000 203000 404720 427820 ...
    ##  $ TractFIPS                 : num  6.04e+09 6.07e+09 6.04e+09 1.07e+09 1.09e+09 ...
    ##  $ Short_Question_Text       : chr  "Arthritis" "Arthritis" "Diabetes" "Mammography" ...

``` r
summary(Health)
```

    ##       Year       StateAbbr          StateDesc           CityName        
    ##  Min.   :2016   Length:759349      Length:759349      Length:759349     
    ##  1st Qu.:2016   Class :character   Class :character   Class :character  
    ##  Median :2017   Mode  :character   Mode  :character   Mode  :character  
    ##  Mean   :2017                                                           
    ##  3rd Qu.:2017                                                           
    ##  Max.   :2017                                                           
    ##  GeographicLevel     DataSource          Category           UniqueID        
    ##  Length:759349      Length:759349      Length:759349      Length:759349     
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##    Measure          Data_Value_Unit    DataValueTypeID    Data_Value_Type   
    ##  Length:759349      Length:759349      Length:759349      Length:759349     
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##    Data_Value    Low_Confidence_Limit High_Confidence_Limit
    ##  Min.   : 0.30   Min.   : 0.20        Min.   : 0.30        
    ##  1st Qu.:10.00   1st Qu.: 8.80        1st Qu.:11.20        
    ##  Median :22.90   Median :20.70        Median :25.30        
    ##  Mean   :31.42   Mean   :29.67        Mean   :33.17        
    ##  3rd Qu.:46.00   3rd Qu.:43.20        3rd Qu.:49.20        
    ##  Max.   :95.70   Max.   :94.60        Max.   :96.50        
    ##  Data_Value_Footnote_Symbol Data_Value_Footnote PopulationCount
    ##  Length:759349              Length:759349       Min.   :   50  
    ##  Class :character           Class :character    1st Qu.: 2458  
    ##  Mode  :character           Mode  :character    Median : 3611  
    ##                                                 Mean   : 3786  
    ##                                                 3rd Qu.: 4900  
    ##                                                 Max.   :28960  
    ##  GeoLocation         CategoryID         MeasureId            CityFIPS      
    ##  Length:759349      Length:759349      Length:759349      Min.   :  15003  
    ##  Class :character   Class :character   Class :character   1st Qu.: 681666  
    ##  Mode  :character   Mode  :character   Mode  :character   Median :2622000  
    ##                                                           Mean   :2607002  
    ##                                                           3rd Qu.:4052500  
    ##                                                           Max.   :5613900  
    ##    TractFIPS         Short_Question_Text
    ##  Min.   :1.073e+09   Length:759349      
    ##  1st Qu.:8.005e+09   Class :character   
    ##  Median :2.608e+10   Mode  :character   
    ##  Mean   :2.588e+10                      
    ##  3rd Qu.:4.011e+10                      
    ##  Max.   :5.602e+10

#### Questions to be addressed

Consider the variables: - mental illness - binge drinking - asthma -
arthritis - cancer - kidney disease - high blood pressure - diabetes -
COPD

When grouped by state, what states have the highest rates of each
variable? When grouped by state, what states have the lowest rates of
each variable?

Which places have the highest rate of each variable? Which places have
the lowest rate of each variable?

Are there any trends regarding the least healthy places to live in the
United States? Are there any trends regarding the healthiest places to
live in the United States?

The goal of this project is to determine where in the United States is
the healthiest, and where is the least healthy. This information is
important for people who might want to know how the places they move
might affect their health.
