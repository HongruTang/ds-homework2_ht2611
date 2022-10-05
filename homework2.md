homework2
================
ht2611
2022-10-05

### Problem 0

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.0      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(readxl)
```

### Problem 1

update `entry` from `yes` / `no` to a logical variable. `Route` columns
8-11 should be character for consistency with 1-7.

``` r
trans_ent = 
  read_csv(
    "data/NYC_Transit_Subway_Entrance_And_Exit_Data.csv",
    col_types = cols(Route8 = "c", Route9 = "c", Route10 = "c", Route11 = "c")) %>% 
  janitor::clean_names() %>% 
  select(
    line, station_name, station_latitude, station_longitude, 
    starts_with("route"), entry, exit_only, vending, entrance_type, 
    ada) %>% 
  mutate(entry = ifelse(entry == "YES", TRUE, FALSE))
```

these data are not “tidy”: route number should be a variable, as should
route. That is, to obtain a tidy dataset we would need to convert
`route` variables from wide to long format.

Tuses `distinct()` to obtain all unique combinations.

``` r
trans_ent %>% 
  select(station_name, line) %>% 
  distinct
```

    ## # A tibble: 465 × 2
    ##    station_name             line    
    ##    <chr>                    <chr>   
    ##  1 25th St                  4 Avenue
    ##  2 36th St                  4 Avenue
    ##  3 45th St                  4 Avenue
    ##  4 53rd St                  4 Avenue
    ##  5 59th St                  4 Avenue
    ##  6 77th St                  4 Avenue
    ##  7 86th St                  4 Avenue
    ##  8 95th St                  4 Avenue
    ##  9 9th St                   4 Avenue
    ## 10 Atlantic Av-Barclays Ctr 4 Avenue
    ## # … with 455 more rows

filters according to ADA compliance as an initial step. T

``` r
trans_ent %>% 
  filter(ada == TRUE) %>% 
  select(station_name, line) %>% 
  distinct
```

    ## # A tibble: 84 × 2
    ##    station_name                   line           
    ##    <chr>                          <chr>          
    ##  1 Atlantic Av-Barclays Ctr       4 Avenue       
    ##  2 DeKalb Av                      4 Avenue       
    ##  3 Pacific St                     4 Avenue       
    ##  4 Grand Central                  42nd St Shuttle
    ##  5 34th St                        6 Avenue       
    ##  6 47-50th Sts Rockefeller Center 6 Avenue       
    ##  7 Church Av                      6 Avenue       
    ##  8 21st St                        63rd Street    
    ##  9 Lexington Av                   63rd Street    
    ## 10 Roosevelt Island               63rd Street    
    ## # … with 74 more rows

exclude station entrances that do not allow vending. Then, we focus on
the `entry` variable – this logical, so taking the mean will produce the
desired proportion

``` r
trans_ent %>% 
  filter(vending == "NO") %>% 
  pull(entry) %>% 
  mean
```

    ## [1] 0.3770492

convert `route` from wide to long format. filtering to focus on the A
train, and on ADA compliance; selecting and using `distinct`

``` r
trans_ent %>% 
  pivot_longer(
    route1:route11,
    names_to = "route_num",
    values_to = "route") %>% 
  filter(route == "A") %>% 
  select(station_name, line) %>% 
  distinct
```

    ## # A tibble: 60 × 2
    ##    station_name                  line           
    ##    <chr>                         <chr>          
    ##  1 Times Square                  42nd St Shuttle
    ##  2 125th St                      8 Avenue       
    ##  3 145th St                      8 Avenue       
    ##  4 14th St                       8 Avenue       
    ##  5 168th St - Washington Heights 8 Avenue       
    ##  6 175th St                      8 Avenue       
    ##  7 181st St                      8 Avenue       
    ##  8 190th St                      8 Avenue       
    ##  9 34th St                       8 Avenue       
    ## 10 42nd St                       8 Avenue       
    ## # … with 50 more rows

``` r
trans_ent %>% 
  pivot_longer(
    route1:route11,
    names_to = "route_num",
    values_to = "route") %>% 
  filter(route == "A", ada == TRUE) %>% 
  select(station_name, line) %>% 
  distinct
```

    ## # A tibble: 17 × 2
    ##    station_name                  line            
    ##    <chr>                         <chr>           
    ##  1 14th St                       8 Avenue        
    ##  2 168th St - Washington Heights 8 Avenue        
    ##  3 175th St                      8 Avenue        
    ##  4 34th St                       8 Avenue        
    ##  5 42nd St                       8 Avenue        
    ##  6 59th St                       8 Avenue        
    ##  7 Inwood - 207th St             8 Avenue        
    ##  8 West 4th St                   8 Avenue        
    ##  9 World Trade Center            8 Avenue        
    ## 10 Times Square-42nd St          Broadway        
    ## 11 59th St-Columbus Circle       Broadway-7th Ave
    ## 12 Times Square                  Broadway-7th Ave
    ## 13 8th Av                        Canarsie        
    ## 14 Franklin Av                   Franklin        
    ## 15 Euclid Av                     Fulton          
    ## 16 Franklin Av                   Fulton          
    ## 17 Howard Beach                  Rockaway

### Problem 2

``` r
data1=read_excel("./data/Trash-Wheel-Collection-Totals-7-2020-2.xlsx",sheet="Mr. Trash Wheel",skip=1)%>%
  janitor::clean_names() %>%
  select(1:14)%>%
  na.omit()%>%
  mutate(sports_balls=as.integer(round(sports_balls)),type="Mr.")
```

    ## New names:
    ## • `` -> `...15`
    ## • `` -> `...16`
    ## • `` -> `...17`

``` r
data2=read_excel("./data/Trash-Wheel-Collection-Totals-7-2020-2.xlsx",sheet="Professor Trash Wheel",skip=1)%>%
  janitor::clean_names() %>%
  select(1:14)%>%
  na.omit()%>%
  mutate(dumpster=as.character(dumpster),sports_balls=as.integer(round(sports_balls)),type="Professor")
final_data=full_join(data1,data2)
```

    ## Joining, by = c("dumpster", "month", "year", "date", "weight_tons",
    ## "volume_cubic_yards", "plastic_bottles", "polystyrene", "cigarette_butts",
    ## "glass_bottles", "grocery_bags", "chip_bags", "sports_balls", "homes_powered",
    ## "type")

All the data have 15 variables. The data1 has 453 observations. Data2
has 71 observations. Final data has 524 observations. The variables such
as “weight” and “volume”. The total weight is 135.5. The total number is
856. \### Problem 3

``` r
data3=read_csv("./data/pols-month.csv")%>%
  janitor::clean_names() %>%
  separate(mon,into=c("year","month","day"),sep="-")%>%
  mutate(
    month=recode(month,"01"="Jan","02"="Feb","03"="Mar","04"="Apr",
                 "05"="May","06"="Jun","07"="Jul","08"="Agu","09"="Sep","10"="Oct","11"="Nov","12"="Dec"
               ),
    president=ifelse(prez_gop==0,'dem','gop'))%>%
  select(-prez_gop,-prez_dem,-day)
```

    ## Rows: 822 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (8): prez_gop, gov_gop, sen_gop, rep_gop, prez_dem, gov_dem, sen_dem, r...
    ## date (1): mon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
data3$year=as.numeric(data3$year)
```

``` r
data4=read_csv("./data/snp.csv")%>%
  janitor::clean_names() %>%
  separate(date,into=c("month","day","year"),sep="/")
```

    ## Rows: 787 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): date
    ## dbl (1): close
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
data4$year=as.numeric(data4$year)
data4$month=as.numeric(data4$month)
data5=data4%>%
  mutate(data4,year=ifelse(year<=20,year+2000,year+1900),
         month=recode(month,"01"="Jan","02"="Feb","03"="Mar","04"="Apr",
                 "05"="May","06"="Jun","07"="Jul","08"="Agu","09"="Sep","10"="Oct","11"="Nov","12"="Dec"
               ))%>%
  select(year,month,close)
```

``` r
data6=read_csv("./data/unemployment.csv")
```

    ## Rows: 68 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (13): Year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
data6=pivot_longer(data6,Jan:Dec,names_to="month",values_to = "close")%>%
  janitor::clean_names()
final_data2=full_join(data3, data5)
```

    ## Joining, by = c("year", "month")

``` r
final_data3=full_join(final_data2,data6)
```

    ## Joining, by = c("year", "month", "close")

``` r
summary(final_data3)
```

    ##       year         month              gov_gop         sen_gop    
    ##  Min.   :1947   Length:1639        Min.   :12.00   Min.   :32.0  
    ##  1st Qu.:1964   Class :character   1st Qu.:18.00   1st Qu.:42.0  
    ##  Median :1981   Mode  :character   Median :22.00   Median :46.0  
    ##  Mean   :1981                      Mean   :22.48   Mean   :46.1  
    ##  3rd Qu.:1998                      3rd Qu.:28.00   3rd Qu.:51.0  
    ##  Max.   :2015                      Max.   :34.00   Max.   :56.0  
    ##                                    NA's   :817     NA's   :817   
    ##     rep_gop         gov_dem        sen_dem         rep_dem   
    ##  Min.   :141.0   Min.   :17.0   Min.   :44.00   Min.   :188  
    ##  1st Qu.:176.0   1st Qu.:22.0   1st Qu.:48.00   1st Qu.:211  
    ##  Median :195.0   Median :28.0   Median :53.00   Median :250  
    ##  Mean   :194.9   Mean   :27.2   Mean   :54.41   Mean   :245  
    ##  3rd Qu.:222.0   3rd Qu.:32.0   3rd Qu.:58.00   3rd Qu.:268  
    ##  Max.   :253.0   Max.   :41.0   Max.   :71.00   Max.   :301  
    ##  NA's   :817     NA's   :817    NA's   :817     NA's   :817  
    ##   president             close       
    ##  Length:1639        Min.   :   2.5  
    ##  Class :character   1st Qu.:   5.6  
    ##  Mode  :character   Median :   9.9  
    ##                     Mean   : 237.0  
    ##                     3rd Qu.: 131.3  
    ##                     Max.   :2107.4  
    ##                     NA's   :42

Data3 has 822 observations of 9 variables, it has variables such as
gov_gop and president, Data5 has 787 observations of 3 variables. Data6
has 816 observations of 3 variables. Data5 and Data6 have the same
variables. The final_data3 has 1639 observations of 10 variables. The
range of year is 68. The key variables are year, month.
