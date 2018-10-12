---
title: "Assignment_5"
output:   
 html_document:
    keep_md: true
    toc: true
    theme: readable

---


## Part 1 Factor Management

```r
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(gapminder))
suppressPackageStartupMessages(library(forcats))
suppressPackageStartupMessages(library(plotly))  
```


# Elaboration for the gapminder data set

__1. Drop Oceania__
Before dropping the unused data the country and continent have 142 and 5 levels repectively. In addition, the row number before dropping is 1704.

```r
#before dropping Oceania, the number of levels of continent is 5.
nrow(gapminder)
```

```
## [1] 1704
```

```r
Ocean_drop <- gapminder %>% 
  filter(continent != "Oceania")
str(Ocean_drop)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	1680 obs. of  6 variables:
##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
##  $ gdpPercap: num  779 821 853 836 740 ...
```

```r
nrow(Ocean_drop)  # Number of rows after dropping levels
```

```
## [1] 1680
```

After filting out the Oceania, the row number drop to 1680. By appling fct_drop on continent, only the level of continent drop to 4 by removing Oceania. And while droplevels() was applied, it applied to all the factors in the structure, therefore, the level of country drop to 140 which mean the country in Oceania were removed. And the level of continent is also reduced to 4. 

```r
# dropping unused Oceania data
drop1 <- Ocean_drop %>% 
  mutate(continent=fct_drop(continent))
  str(drop1)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	1680 obs. of  6 variables:
##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ continent: Factor w/ 4 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
##  $ gdpPercap: num  779 821 853 836 740 ...
```

```r
  nrow(drop1)  # the row number remaining at 1680
```

```
## [1] 1680
```

```r
drop2 <- Ocean_drop %>% 
  droplevels() 
  str(drop2) 
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	1680 obs. of  6 variables:
##  $ country  : Factor w/ 140 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ continent: Factor w/ 4 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
##  $ gdpPercap: num  779 821 853 836 740 ...
```

```r
  nrow(drop2)# Number of rows after dropping levels
```

```
## [1] 1680
```



__2. Reorder the levels of `country` or `continent`__
The continent is originally ordered alphabetically, here we used different method to reorder the levels of the continient. 
First, we will check the original looking of the gapminder dataset before reordering. It is obviously that the continent is ordered by alphabetically. 


```r
levels(gapminder$continent)
```

```
## [1] "Africa"   "Americas" "Asia"     "Europe"   "Oceania"
```


Now we try to reordered the levels of the continent in the following different ways:

1. Reordered by the maximum GDP per capital of each continent

```r
gapminder %>% 
  mutate(continent=fct_reorder(continent,gdpPercap,max)) %>% 
ggplot(aes(continent,gdpPercap)) + geom_boxplot(aes(fill=continent))
```

![](assignment_5_files/figure-html/reordered by max-1.png)<!-- -->

```r
# to verify that this is reordered using maximum GDP per capital of each continent, we do the following:

gapminder %>% 
  group_by(continent) %>% 
  # calculate the maximum gdpPercap in each continent
  summarize(
    max_gdp = max(gdpPercap)
  ) %>% 
  arrange(max_gdp) %>%  # which is consistent with the order in the box plot
 knitr::kable()
```



continent      max_gdp
----------  ----------
Africa        21951.21
Oceania       34435.37
Americas      42951.65
Europe        49357.19
Asia         113523.13

2. Reordered by the minimum GDP per capital of each continent

```r
gapminder %>% 
  mutate(continent=fct_reorder(continent,gdpPercap,min)) %>% 
ggplot(aes(continent,gdpPercap)) + geom_boxplot(aes(fill=continent))
```

![](assignment_5_files/figure-html/reordered by min-1.png)<!-- -->

```r
# to verify that this is reordered using maximum GDP per capital of each continent, we do the following:

gapminder %>% 
  group_by(continent) %>% 
  # calculate the maximum gdpPercap in each continent
  summarize(
    min_gdp = min(gdpPercap)
  ) %>% 
  arrange(min_gdp) %>%  # which is consistent with the order in the box plot
 knitr::kable()
```



continent       min_gdp
----------  -----------
Africa         241.1659
Asia           331.0000
Europe         973.5332
Americas      1201.6372
Oceania      10039.5956


3. Reordered by the mean GDP per capital of each continent


```r
gapminder %>% 
  mutate(continent=fct_reorder(continent,gdpPercap,mean)) %>% 
ggplot(aes(continent,gdpPercap)) + geom_boxplot(aes(fill=continent))
```

![](assignment_5_files/figure-html/reordered by mean-1.png)<!-- -->

```r
gapminder %>% 
  group_by(continent) %>% 
  # calculate the maximum gdpPercap in each continent
  summarize(
    mean_gdp = mean(gdpPercap)
  ) %>% 
  arrange(mean_gdp) %>%  # which is consistent with the order in the box plot
 knitr::kable()
```



continent     mean_gdp
----------  ----------
Africa        2193.755
Americas      7136.110
Asia          7902.150
Europe       14469.476
Oceania      18621.609



## Part 2: File I/O

In order to explore whether this survives the round trip of writing to file then reading back in, we filter the gapminder dataframe and get only the data from Asian country in 2007.


```r
filterdata <- gapminder %>% 
  filter(continent == "Asia" & year == 2007) 

# drop the unused level
asiadata <- filterdata %>% 
 droplevels() 
# check the level of continent and country to make sure the unused data is successfully dropped.
str(asiadata)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	33 obs. of  6 variables:
##  $ country  : Factor w/ 33 levels "Afghanistan",..: 1 2 3 4 5 6 7 8 9 10 ...
##  $ continent: Factor w/ 1 level "Asia": 1 1 1 1 1 1 1 1 1 1 ...
##  $ year     : int  2007 2007 2007 2007 2007 2007 2007 2007 2007 2007 ...
##  $ lifeExp  : num  43.8 75.6 64.1 59.7 73 ...
##  $ pop      : int  31889923 708573 150448339 14131858 1318683096 6980412 1110396331 223547000 69453570 27499638 ...
##  $ gdpPercap: num  975 29796 1391 1714 4959 ...
```


# `write_csv()`/`read_csv()`

so now we will try to write the __asiadata__ into the csv fie and again read it out to see whether the write and read has effected the data.


```r
#first check the original data frame asiadata
head(asiadata)
```

```
## # A tibble: 6 x 6
##   country          continent  year lifeExp        pop gdpPercap
##   <fct>            <fct>     <int>   <dbl>      <int>     <dbl>
## 1 Afghanistan      Asia       2007    43.8   31889923      975.
## 2 Bahrain          Asia       2007    75.6     708573    29796.
## 3 Bangladesh       Asia       2007    64.1  150448339     1391.
## 4 Cambodia         Asia       2007    59.7   14131858     1714.
## 5 China            Asia       2007    73.0 1318683096     4959.
## 6 Hong Kong, China Asia       2007    82.2    6980412    39725.
```

```r
# wirte the csv file
write_csv(asiadata,"asiadata.csv")
```

Now we found out that the variable __country__ and __continent__ which used to be factor transfer into a characters after reading back from the csv file. Apart from that, the data remains unchanged.


```r
# read back the csv file
 read_data <- read_csv("asiadata.csv")
```

```
## Parsed with column specification:
## cols(
##   country = col_character(),
##   continent = col_character(),
##   year = col_integer(),
##   lifeExp = col_double(),
##   pop = col_integer(),
##   gdpPercap = col_double()
## )
```

```r
 is.factor(read_data$country)  # which is false
```

```
## [1] FALSE
```

```r
 is.factor(read_data$continent)
```

```
## [1] FALSE
```

```r
 is.character(read_data$country) 
```

```
## [1] TRUE
```

```r
# now check the new read_in data
 head(read_data)
```

```
## # A tibble: 6 x 6
##   country          continent  year lifeExp        pop gdpPercap
##   <chr>            <chr>     <int>   <dbl>      <int>     <dbl>
## 1 Afghanistan      Asia       2007    43.8   31889923      975.
## 2 Bahrain          Asia       2007    75.6     708573    29796.
## 3 Bangladesh       Asia       2007    64.1  150448339     1391.
## 4 Cambodia         Asia       2007    59.7   14131858     1714.
## 5 China            Asia       2007    73.0 1318683096     4959.
## 6 Hong Kong, China Asia       2007    82.2    6980412    39725.
```


# `saveRDS()/readRDS()`

while saving and reading RDS file, the class of the data is preserved, therefore the variable continent and country returned are with class factor. 


```r
# save to RDS file
saveRDS(asiadata, "asiadata.rds")

# read from RDS file
read_rdsdata <- readRDS("asiadata.rds")

# check the readin data
 head(read_rdsdata) 
```

```
## # A tibble: 6 x 6
##   country          continent  year lifeExp        pop gdpPercap
##   <fct>            <fct>     <int>   <dbl>      <int>     <dbl>
## 1 Afghanistan      Asia       2007    43.8   31889923      975.
## 2 Bahrain          Asia       2007    75.6     708573    29796.
## 3 Bangladesh       Asia       2007    64.1  150448339     1391.
## 4 Cambodia         Asia       2007    59.7   14131858     1714.
## 5 China            Asia       2007    73.0 1318683096     4959.
## 6 Hong Kong, China Asia       2007    82.2    6980412    39725.
```

```r
  is.factor(read_rdsdata$country)  # which is true
```

```
## [1] TRUE
```

```r
  is.factor(read_rdsdata$continent)
```

```
## [1] TRUE
```


# `dput()/dget()`


```r
# put data into file
dput(asiadata, "asiadata.txt")

# get data from text file
data_txt <- dget("asiadata.txt")
 
head(data_txt) 
```

```
## # A tibble: 6 x 6
##   country          continent  year lifeExp        pop gdpPercap
##   <fct>            <fct>     <int>   <dbl>      <int>     <dbl>
## 1 Afghanistan      Asia       2007    43.8   31889923      975.
## 2 Bahrain          Asia       2007    75.6     708573    29796.
## 3 Bangladesh       Asia       2007    64.1  150448339     1391.
## 4 Cambodia         Asia       2007    59.7   14131858     1714.
## 5 China            Asia       2007    73.0 1318683096     4959.
## 6 Hong Kong, China Asia       2007    82.2    6980412    39725.
```

```r
  is.factor(data_txt$country)  # which is true
```

```
## [1] TRUE
```

```r
  is.factor(data_txt$continent)
```

```
## [1] TRUE
```

This is similar to `saveRDS()/readRDS()` which preserve the class of the data and the data remains unchanged while reading in from the txt file.

## Part 3: Visualization design

1. In this part, I am going to remake the graph I have plotted for the previous assignment. 
this is a graph plotted in assignment 2. In this graph, I tried to showed the gdpPerCap of each country grouped by the countinent. But this graph did not show that much useful information and it did not show any information how the gdpPercap varies with time. Therefore, we are going to replot this graph.


```r
ggplot(gapminder,aes(gdpPercap,continent))+
  geom_point(aes(colour=continent,size=gdpPercap),alpha=0.4)
```

![](assignment_5_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

Here is the modified graph:


```r
# get the max, min, median and mean for each contient in different year
  new_table <-  gapminder %>% 
  group_by(continent,year) %>% 
summarize(
  min_gdp = min(min(gdpPercap)),
  max_gdp = max(max(gdpPercap)),
  mean_gdp = mean(mean(gdpPercap)),
  median_gdp = median(median(gdpPercap))
)

# then we need to gather the gdp together to tidy up the data

tidy_table <- gather(new_table,key = "Stat_GDP", value="GDP_value", min_gdp, max_gdp,mean_gdp,median_gdp)
# then check the new gathered table
knitr::kable(head(tidy_table))
```



continent    year  Stat_GDP    GDP_value
----------  -----  ---------  ----------
Africa       1952  min_gdp      298.8462
Africa       1957  min_gdp      335.9971
Africa       1962  min_gdp      355.2032
Africa       1967  min_gdp      412.9775
Africa       1972  min_gdp      464.0995
Africa       1977  min_gdp      502.3197

```r
# now the data is ready for plotting

new_graph <-  tidy_table %>% 
  ggplot(aes(x = year, y = GDP_value, color = Stat_GDP) ) +
  facet_wrap(~continent) +
  #use log10 in y-axis
  scale_y_log10()+
  geom_point()+
  #use line to show the trend
  geom_line()+
  ylab("GDP distribution")+
  ggtitle("Summarized GDP per Capital vs Year")
  
  new_graph
```

![](assignment_5_files/figure-html/modified graph for GDP per capital-1.png)<!-- -->

The differences between these two version are:

* The graph is faceted which can easier to access the data for each continent
* Various statistic such as maximum, minimum, mean and median were computed and the trend of each was shown instead of raw data. 
* The trend against the year is shown.

2. Next we are going to convert this graph into a plotly graph and to see the differences with `ggplot2`.


```r
ggplotly(new_graph)
```

<!--html_preserve--><div id="htmlwidget-80953cf63c9524c542a6" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-80953cf63c9524c542a6">{"x":{"data":[{"x":[1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007],"y":[3.6744289754842,3.73934320902181,3.82975589956553,4.27352793571109,4.32245699990115,4.34145849934605,4.23965666469638,4.07424608914852,4.13104599073842,4.16799164788718,4.0976637773612,4.12078722664154],"text":["year: 1952<br />GDP_value:   4725.2955<br />Stat_GDP: max_gdp","year: 1957<br />GDP_value:   5487.1042<br />Stat_GDP: max_gdp","year: 1962<br />GDP_value:   6757.0308<br />Stat_GDP: max_gdp","year: 1967<br />GDP_value:  18772.7517<br />Stat_GDP: max_gdp","year: 1972<br />GDP_value:  21011.4972<br />Stat_GDP: max_gdp","year: 1977<br />GDP_value:  21951.2118<br />Stat_GDP: max_gdp","year: 1982<br />GDP_value:  17364.2754<br />Stat_GDP: max_gdp","year: 1987<br />GDP_value:  11864.4084<br />Stat_GDP: max_gdp","year: 1992<br />GDP_value:  13522.1575<br />Stat_GDP: max_gdp","year: 1997<br />GDP_value:  14722.8419<br />Stat_GDP: max_gdp","year: 2002<br />GDP_value:  12521.7139<br />Stat_GDP: max_gdp","year: 2007<br />GDP_value:  13206.4845<br />Stat_GDP: max_gdp"],"type":"scatter","mode":"markers+lines","marker":{"autocolorscale":false,"color":"rgba(248,118,109,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(248,118,109,1)"}},"hoveron":"points","name":"max_gdp","legendgroup":"max_gdp","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","line":{"width":1.88976377952756,"color":"rgba(248,118,109,1)","dash":"solid"},"frame":null},{"x":[1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007],"y":[4.14583267954377,4.17164242694385,4.20879450331745,4.29071037250177,4.33857672354045,4.38152357914239,4.3981060362026,4.47544382010215,4.5052033422329,4.55348777266477,4.59214454010295,4.632979883279],"text":["year: 1952<br />GDP_value:  13990.4821<br />Stat_GDP: max_gdp","year: 1957<br />GDP_value:  14847.1271<br />Stat_GDP: max_gdp","year: 1962<br />GDP_value:  16173.1459<br />Stat_GDP: max_gdp","year: 1967<br />GDP_value:  19530.3656<br />Stat_GDP: max_gdp","year: 1972<br />GDP_value:  21806.0359<br />Stat_GDP: max_gdp","year: 1977<br />GDP_value:  24072.6321<br />Stat_GDP: max_gdp","year: 1982<br />GDP_value:  25009.5591<br />Stat_GDP: max_gdp","year: 1987<br />GDP_value:  29884.3504<br />Stat_GDP: max_gdp","year: 1992<br />GDP_value:  32003.9322<br />Stat_GDP: max_gdp","year: 1997<br />GDP_value:  35767.4330<br />Stat_GDP: max_gdp","year: 2002<br />GDP_value:  39097.0995<br />Stat_GDP: max_gdp","year: 2007<br />GDP_value:  42951.6531<br />Stat_GDP: max_gdp"],"type":"scatter","mode":"markers+lines","marker":{"autocolorscale":false,"color":"rgba(248,118,109,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(248,118,109,1)"}},"hoveron":"points","name":"max_gdp","legendgroup":"max_gdp","showlegend":false,"xaxis":"x2","yaxis":"y","hoverinfo":"text","line":{"width":1.88976377952756,"color":"rgba(248,118,109,1)","dash":"solid"},"frame":null},{"x":[1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007],"y":[5.03495857498782,5.05508436784766,4.97981283941297,4.90792105261074,5.0388103158678,4.7728017852095,4.52754194088831,4.4489910677682,4.54323488420159,4.60531172711223,4.5565811487931,4.67492531406996],"text":["year: 1952<br />GDP_value: 108382.3529<br />Stat_GDP: max_gdp","year: 1957<br />GDP_value: 113523.1329<br />Stat_GDP: max_gdp","year: 1962<br />GDP_value:  95458.1118<br />Stat_GDP: max_gdp","year: 1967<br />GDP_value:  80894.8833<br />Stat_GDP: max_gdp","year: 1972<br />GDP_value: 109347.8670<br />Stat_GDP: max_gdp","year: 1977<br />GDP_value:  59265.4771<br />Stat_GDP: max_gdp","year: 1982<br />GDP_value:  33693.1753<br />Stat_GDP: max_gdp","year: 1987<br />GDP_value:  28118.4300<br />Stat_GDP: max_gdp","year: 1992<br />GDP_value:  34932.9196<br />Stat_GDP: max_gdp","year: 1997<br />GDP_value:  40300.6200<br />Stat_GDP: max_gdp","year: 2002<br />GDP_value:  36023.1054<br />Stat_GDP: max_gdp","year: 2007<br />GDP_value:  47306.9898<br />Stat_GDP: max_gdp"],"type":"scatter","mode":"markers+lines","marker":{"autocolorscale":false,"color":"rgba(248,118,109,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(248,118,109,1)"}},"hoveron":"points","name":"max_gdp","legendgroup":"max_gdp","showlegend":false,"xaxis":"x3","yaxis":"y","hoverinfo":"text","line":{"width":1.88976377952756,"color":"rgba(248,118,109,1)","dash":"solid"},"frame":null},{"x":[1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007],"y":[4.16832752592624,4.25308321228107,4.31029159428248,4.36108808961809,4.43449086835605,4.43107881405245,4.45328339812114,4.49887511143166,4.53104007265859,4.61577297830881,4.6501518025891,4.69335042800253],"text":["year: 1952<br />GDP_value:  14734.2327<br />Stat_GDP: max_gdp","year: 1957<br />GDP_value:  17909.4897<br />Stat_GDP: max_gdp","year: 1962<br />GDP_value:  20431.0927<br />Stat_GDP: max_gdp","year: 1967<br />GDP_value:  22966.1443<br />Stat_GDP: max_gdp","year: 1972<br />GDP_value:  27195.1130<br />Stat_GDP: max_gdp","year: 1977<br />GDP_value:  26982.2905<br />Stat_GDP: max_gdp","year: 1982<br />GDP_value:  28397.7151<br />Stat_GDP: max_gdp","year: 1987<br />GDP_value:  31540.9748<br />Stat_GDP: max_gdp","year: 1992<br />GDP_value:  33965.6611<br />Stat_GDP: max_gdp","year: 1997<br />GDP_value:  41283.1643<br />Stat_GDP: max_gdp","year: 2002<br />GDP_value:  44683.9753<br />Stat_GDP: max_gdp","year: 2007<br />GDP_value:  49357.1902<br />Stat_GDP: max_gdp"],"type":"scatter","mode":"markers+lines","marker":{"autocolorscale":false,"color":"rgba(248,118,109,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(248,118,109,1)"}},"hoveron":"points","name":"max_gdp","legendgroup":"max_gdp","showlegend":false,"xaxis":"x","yaxis":"y2","hoverinfo":"text","line":{"width":1.88976377952756,"color":"rgba(248,118,109,1)","dash":"solid"},"frame":null},{"x":[1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007],"y":[4.02352306468033,4.08804373617567,4.11977297258156,4.16214976656191,4.22501524446638,4.26326190559163,4.28952227118181,4.34022371952203,4.36967527661645,4.43133057265871,4.4869651144324,4.53700472145269],"text":["year: 1952<br />GDP_value:  10556.5757<br />Stat_GDP: max_gdp","year: 1957<br />GDP_value:  12247.3953<br />Stat_GDP: max_gdp","year: 1962<br />GDP_value:  13175.6780<br />Stat_GDP: max_gdp","year: 1967<br />GDP_value:  14526.1246<br />Stat_GDP: max_gdp","year: 1972<br />GDP_value:  16788.6295<br />Stat_GDP: max_gdp","year: 1977<br />GDP_value:  18334.1975<br />Stat_GDP: max_gdp","year: 1982<br />GDP_value:  19477.0093<br />Stat_GDP: max_gdp","year: 1987<br />GDP_value:  21888.8890<br />Stat_GDP: max_gdp","year: 1992<br />GDP_value:  23424.7668<br />Stat_GDP: max_gdp","year: 1997<br />GDP_value:  26997.9366<br />Stat_GDP: max_gdp","year: 2002<br />GDP_value:  30687.7547<br />Stat_GDP: max_gdp","year: 2007<br />GDP_value:  34435.3674<br />Stat_GDP: max_gdp"],"type":"scatter","mode":"markers+lines","marker":{"autocolorscale":false,"color":"rgba(248,118,109,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(248,118,109,1)"}},"hoveron":"points","name":"max_gdp","legendgroup":"max_gdp","showlegend":false,"xaxis":"x2","yaxis":"y2","hoverinfo":"text","line":{"width":1.88976377952756,"color":"rgba(248,118,109,1)","dash":"solid"},"frame":null},{"x":[1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007],"y":[3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353],"text":["year: 1952<br />GDP_value:   1252.5725<br />Stat_GDP: mean_gdp","year: 1957<br />GDP_value:   1385.2361<br />Stat_GDP: mean_gdp","year: 1962<br />GDP_value:   1598.0788<br />Stat_GDP: mean_gdp","year: 1967<br />GDP_value:   2050.3638<br />Stat_GDP: mean_gdp","year: 1972<br />GDP_value:   2339.6157<br />Stat_GDP: mean_gdp","year: 1977<br />GDP_value:   2585.9385<br />Stat_GDP: mean_gdp","year: 1982<br />GDP_value:   2481.5930<br />Stat_GDP: mean_gdp","year: 1987<br />GDP_value:   2282.6690<br />Stat_GDP: mean_gdp","year: 1992<br />GDP_value:   2281.8103<br />Stat_GDP: mean_gdp","year: 1997<br />GDP_value:   2378.7596<br />Stat_GDP: mean_gdp","year: 2002<br />GDP_value:   2599.3852<br />Stat_GDP: mean_gdp","year: 2007<br />GDP_value:   3089.0326<br />Stat_GDP: mean_gdp"],"type":"scatter","mode":"markers+lines","marker":{"autocolorscale":false,"color":"rgba(124,174,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(124,174,0,1)"}},"hoveron":"points","name":"mean_gdp","legendgroup":"mean_gdp","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","line":{"width":1.88976377952756,"color":"rgba(124,174,0,1)","dash":"solid"},"frame":null},{"x":[1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007],"y":[3.61056036525017,3.66426991474475,3.69033271685992,3.75344926455876,3.81233396485541,3.86640591933869,3.87545120563652,3.89172698173537,3.90552250742363,3.94886760534905,3.96790710843737,4.04151236122794],"text":["year: 1952<br />GDP_value:   4079.0626<br />Stat_GDP: mean_gdp","year: 1957<br />GDP_value:   4616.0437<br />Stat_GDP: mean_gdp","year: 1962<br />GDP_value:   4901.5419<br />Stat_GDP: mean_gdp","year: 1967<br />GDP_value:   5668.2535<br />Stat_GDP: mean_gdp","year: 1972<br />GDP_value:   6491.3341<br />Stat_GDP: mean_gdp","year: 1977<br />GDP_value:   7352.0071<br />Stat_GDP: mean_gdp","year: 1982<br />GDP_value:   7506.7371<br />Stat_GDP: mean_gdp","year: 1987<br />GDP_value:   7793.4003<br />Stat_GDP: mean_gdp","year: 1992<br />GDP_value:   8044.9344<br />Stat_GDP: mean_gdp","year: 1997<br />GDP_value:   8889.3009<br />Stat_GDP: mean_gdp","year: 2002<br />GDP_value:   9287.6771<br />Stat_GDP: mean_gdp","year: 2007<br />GDP_value:  11003.0316<br />Stat_GDP: mean_gdp"],"type":"scatter","mode":"markers+lines","marker":{"autocolorscale":false,"color":"rgba(124,174,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(124,174,0,1)"}},"hoveron":"points","name":"mean_gdp","legendgroup":"mean_gdp","showlegend":false,"xaxis":"x2","yaxis":"y","hoverinfo":"text","line":{"width":1.88976377952756,"color":"rgba(124,174,0,1)","dash":"solid"},"frame":null},{"x":[1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007],"y":[3.7156260120457,3.76250848350692,3.7581068412446,3.77605968114783,3.9131496527027,3.89161070844018,3.87123045259187,3.88128343368659,3.93649817232693,3.99273432409034,4.0074955920098,4.09597185792762],"text":["year: 1952<br />GDP_value:   5195.4840<br />Stat_GDP: mean_gdp","year: 1957<br />GDP_value:   5787.7329<br />Stat_GDP: mean_gdp","year: 1962<br />GDP_value:   5729.3696<br />Stat_GDP: mean_gdp","year: 1967<br />GDP_value:   5971.1734<br />Stat_GDP: mean_gdp","year: 1972<br />GDP_value:   8187.4687<br />Stat_GDP: mean_gdp","year: 1977<br />GDP_value:   7791.3140<br />Stat_GDP: mean_gdp","year: 1982<br />GDP_value:   7434.1352<br />Stat_GDP: mean_gdp","year: 1987<br />GDP_value:   7608.2265<br />Stat_GDP: mean_gdp","year: 1992<br />GDP_value:   8639.6902<br />Stat_GDP: mean_gdp","year: 1997<br />GDP_value:   9834.0933<br />Stat_GDP: mean_gdp","year: 2002<br />GDP_value:  10174.0904<br />Stat_GDP: mean_gdp","year: 2007<br />GDP_value:  12473.0269<br />Stat_GDP: mean_gdp"],"type":"scatter","mode":"markers+lines","marker":{"autocolorscale":false,"color":"rgba(124,174,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(124,174,0,1)"}},"hoveron":"points","name":"mean_gdp","legendgroup":"mean_gdp","showlegend":false,"xaxis":"x3","yaxis":"y","hoverinfo":"text","line":{"width":1.88976377952756,"color":"rgba(124,174,0,1)","dash":"solid"},"frame":null},{"x":[1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007],"y":[3.75289756108011,3.84279719452701,3.9224912190118,4.00620169497082,4.09619980399585,4.15484920636308,4.193622541852,4.23588963792581,4.23201894353838,4.28050511231307,4.33669447805992,4.3988854218467],"text":["year: 1952<br />GDP_value:   5661.0574<br />Stat_GDP: mean_gdp","year: 1957<br />GDP_value:   6963.0128<br />Stat_GDP: mean_gdp","year: 1962<br />GDP_value:   8365.4868<br />Stat_GDP: mean_gdp","year: 1967<br />GDP_value:  10143.8238<br />Stat_GDP: mean_gdp","year: 1972<br />GDP_value:  12479.5752<br />Stat_GDP: mean_gdp","year: 1977<br />GDP_value:  14283.9791<br />Stat_GDP: mean_gdp","year: 1982<br />GDP_value:  15617.8966<br />Stat_GDP: mean_gdp","year: 1987<br />GDP_value:  17214.3107<br />Stat_GDP: mean_gdp","year: 1992<br />GDP_value:  17061.5681<br />Stat_GDP: mean_gdp","year: 1997<br />GDP_value:  19076.7818<br />Stat_GDP: mean_gdp","year: 2002<br />GDP_value:  21711.7324<br />Stat_GDP: mean_gdp","year: 2007<br />GDP_value:  25054.4816<br />Stat_GDP: mean_gdp"],"type":"scatter","mode":"markers+lines","marker":{"autocolorscale":false,"color":"rgba(124,174,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(124,174,0,1)"}},"hoveron":"points","name":"mean_gdp","legendgroup":"mean_gdp","showlegend":false,"xaxis":"x","yaxis":"y2","hoverinfo":"text","line":{"width":1.88976377952756,"color":"rgba(124,174,0,1)","dash":"solid"},"frame":null},{"x":[1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007],"y":[4.01275649956809,4.06440266763112,4.10368238982993,4.16121887255543,4.21530261731451,4.23764319238016,4.26845416720692,4.31065168943455,4.32002254411614,4.38064848581959,4.43037789196153,4.47436471895566],"text":["year: 1952<br />GDP_value:  10298.0857<br />Stat_GDP: mean_gdp","year: 1957<br />GDP_value:  11598.5225<br />Stat_GDP: mean_gdp","year: 1962<br />GDP_value:  12696.4524<br />Stat_GDP: mean_gdp","year: 1967<br />GDP_value:  14495.0218<br />Stat_GDP: mean_gdp","year: 1972<br />GDP_value:  16417.3334<br />Stat_GDP: mean_gdp","year: 1977<br />GDP_value:  17283.9576<br />Stat_GDP: mean_gdp","year: 1982<br />GDP_value:  18554.7098<br />Stat_GDP: mean_gdp","year: 1987<br />GDP_value:  20448.0402<br />Stat_GDP: mean_gdp","year: 1992<br />GDP_value:  20894.0459<br />Stat_GDP: mean_gdp","year: 1997<br />GDP_value:  24024.1752<br />Stat_GDP: mean_gdp","year: 2002<br />GDP_value:  26938.7780<br />Stat_GDP: mean_gdp","year: 2007<br />GDP_value:  29810.1883<br />Stat_GDP: mean_gdp"],"type":"scatter","mode":"markers+lines","marker":{"autocolorscale":false,"color":"rgba(124,174,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(124,174,0,1)"}},"hoveron":"points","name":"mean_gdp","legendgroup":"mean_gdp","showlegend":false,"xaxis":"x2","yaxis":"y2","hoverinfo":"text","line":{"width":1.88976377952756,"color":"rgba(124,174,0,1)","dash":"solid"},"frame":null},{"x":[1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007],"y":[2.99432840306107,3.0103097056781,3.05453020038585,3.08292043983923,3.15937842900831,3.14601598441569,3.1217988555489,3.08621227470733,3.0650683270177,3.0718389859146,3.08482042130593,3.16204649213426],"text":["year: 1952<br />GDP_value:    987.0256<br />Stat_GDP: median_gdp","year: 1957<br />GDP_value:   1024.0230<br />Stat_GDP: median_gdp","year: 1962<br />GDP_value:   1133.7837<br />Stat_GDP: median_gdp","year: 1967<br />GDP_value:   1210.3764<br />Stat_GDP: median_gdp","year: 1972<br />GDP_value:   1443.3725<br />Stat_GDP: median_gdp","year: 1977<br />GDP_value:   1399.6388<br />Stat_GDP: median_gdp","year: 1982<br />GDP_value:   1323.7283<br />Stat_GDP: median_gdp","year: 1987<br />GDP_value:   1219.5856<br />Stat_GDP: median_gdp","year: 1992<br />GDP_value:   1161.6314<br />Stat_GDP: median_gdp","year: 1997<br />GDP_value:   1179.8831<br />Stat_GDP: median_gdp","year: 2002<br />GDP_value:   1215.6832<br />Stat_GDP: median_gdp","year: 2007<br />GDP_value:   1452.2671<br />Stat_GDP: median_gdp"],"type":"scatter","mode":"markers+lines","marker":{"autocolorscale":false,"color":"rgba(0,191,196,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,191,196,1)"}},"hoveron":"points","name":"median_gdp","legendgroup":"median_gdp","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","line":{"width":1.88976377952756,"color":"rgba(0,191,196,1)","dash":"solid"},"frame":null},{"x":[1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007],"y":[3.48405811924978,3.57755460151657,3.61131048729748,3.66683549228173,3.72472183764533,3.7980489038637,3.80851492650228,3.8035215342233,3.82077552142161,3.85209507308631,3.84477374052922,3.95173097080165],"text":["year: 1952<br />GDP_value:   3048.3029<br />Stat_GDP: median_gdp","year: 1957<br />GDP_value:   3780.5467<br />Stat_GDP: median_gdp","year: 1962<br />GDP_value:   4086.1141<br />Stat_GDP: median_gdp","year: 1967<br />GDP_value:   4643.3935<br />Stat_GDP: median_gdp","year: 1972<br />GDP_value:   5305.4453<br />Stat_GDP: median_gdp","year: 1977<br />GDP_value:   6281.2909<br />Stat_GDP: median_gdp","year: 1982<br />GDP_value:   6434.5018<br />Stat_GDP: median_gdp","year: 1987<br />GDP_value:   6360.9434<br />Stat_GDP: median_gdp","year: 1992<br />GDP_value:   6618.7431<br />Stat_GDP: median_gdp","year: 1997<br />GDP_value:   7113.6923<br />Stat_GDP: median_gdp","year: 2002<br />GDP_value:   6994.7749<br />Stat_GDP: median_gdp","year: 2007<br />GDP_value:   8948.1029<br />Stat_GDP: median_gdp"],"type":"scatter","mode":"markers+lines","marker":{"autocolorscale":false,"color":"rgba(0,191,196,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,191,196,1)"}},"hoveron":"points","name":"median_gdp","legendgroup":"median_gdp","showlegend":false,"xaxis":"x2","yaxis":"y","hoverinfo":"text","line":{"width":1.88976377952756,"color":"rgba(0,191,196,1)","dash":"solid"},"frame":null},{"x":[1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007],"y":[3.08168852810498,3.18975548194558,3.21736605095263,3.30733087662514,3.41017352648115,3.50453672644772,3.6134745020876,3.61347101441374,3.57125025273538,3.56174275554633,3.61182155267593,3.65041068311967],"text":["year: 1952<br />GDP_value:   1206.9479<br />Stat_GDP: median_gdp","year: 1957<br />GDP_value:   1547.9448<br />Stat_GDP: median_gdp","year: 1962<br />GDP_value:   1649.5522<br />Stat_GDP: median_gdp","year: 1967<br />GDP_value:   2029.2281<br />Stat_GDP: median_gdp","year: 1972<br />GDP_value:   2571.4230<br />Stat_GDP: median_gdp","year: 1977<br />GDP_value:   3195.4846<br />Stat_GDP: median_gdp","year: 1982<br />GDP_value:   4106.5253<br />Stat_GDP: median_gdp","year: 1987<br />GDP_value:   4106.4923<br />Stat_GDP: median_gdp","year: 1992<br />GDP_value:   3726.0635<br />Stat_GDP: median_gdp","year: 1997<br />GDP_value:   3645.3796<br />Stat_GDP: median_gdp","year: 2002<br />GDP_value:   4090.9253<br />Stat_GDP: median_gdp","year: 2007<br />GDP_value:   4471.0619<br />Stat_GDP: median_gdp"],"type":"scatter","mode":"markers+lines","marker":{"autocolorscale":false,"color":"rgba(0,191,196,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,191,196,1)"}},"hoveron":"points","name":"median_gdp","legendgroup":"median_gdp","showlegend":false,"xaxis":"x3","yaxis":"y","hoverinfo":"text","line":{"width":1.88976377952756,"color":"rgba(0,191,196,1)","dash":"solid"},"frame":null},{"x":[1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007],"y":[3.71117174281444,3.78295405827951,3.87597138580012,3.97155726172868,4.09083555176205,4.15307531007667,4.18533883365344,4.20992996678362,4.24428097981906,4.29217847971059,4.37428747875756,4.44799581112526],"text":["year: 1952<br />GDP_value:   5142.4697<br />Stat_GDP: median_gdp","year: 1957<br />GDP_value:   6066.7215<br />Stat_GDP: median_gdp","year: 1962<br />GDP_value:   7515.7337<br />Stat_GDP: median_gdp","year: 1967<br />GDP_value:   9366.0670<br />Stat_GDP: median_gdp","year: 1972<br />GDP_value:  12326.3800<br />Stat_GDP: median_gdp","year: 1977<br />GDP_value:  14225.7545<br />Stat_GDP: median_gdp","year: 1982<br />GDP_value:  15322.8247<br />Stat_GDP: median_gdp","year: 1987<br />GDP_value:  16215.4859<br />Stat_GDP: median_gdp","year: 1992<br />GDP_value:  17550.1559<br />Stat_GDP: median_gdp","year: 1997<br />GDP_value:  19596.4986<br />Stat_GDP: median_gdp","year: 2002<br />GDP_value:  23674.8632<br />Stat_GDP: median_gdp","year: 2007<br />GDP_value:  28054.0658<br />Stat_GDP: median_gdp"],"type":"scatter","mode":"markers+lines","marker":{"autocolorscale":false,"color":"rgba(0,191,196,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,191,196,1)"}},"hoveron":"points","name":"median_gdp","legendgroup":"median_gdp","showlegend":false,"xaxis":"x","yaxis":"y2","hoverinfo":"text","line":{"width":1.88976377952756,"color":"rgba(0,191,196,1)","dash":"solid"},"frame":null},{"x":[1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007],"y":[4.01275649956809,4.06440266763112,4.10368238982993,4.16121887255543,4.21530261731451,4.23764319238016,4.26845416720692,4.31065168943455,4.32002254411614,4.38064848581959,4.43037789196153,4.47436471895566],"text":["year: 1952<br />GDP_value:  10298.0857<br />Stat_GDP: median_gdp","year: 1957<br />GDP_value:  11598.5225<br />Stat_GDP: median_gdp","year: 1962<br />GDP_value:  12696.4524<br />Stat_GDP: median_gdp","year: 1967<br />GDP_value:  14495.0218<br />Stat_GDP: median_gdp","year: 1972<br />GDP_value:  16417.3334<br />Stat_GDP: median_gdp","year: 1977<br />GDP_value:  17283.9576<br />Stat_GDP: median_gdp","year: 1982<br />GDP_value:  18554.7098<br />Stat_GDP: median_gdp","year: 1987<br />GDP_value:  20448.0402<br />Stat_GDP: median_gdp","year: 1992<br />GDP_value:  20894.0459<br />Stat_GDP: median_gdp","year: 1997<br />GDP_value:  24024.1752<br />Stat_GDP: median_gdp","year: 2002<br />GDP_value:  26938.7780<br />Stat_GDP: median_gdp","year: 2007<br />GDP_value:  29810.1883<br />Stat_GDP: median_gdp"],"type":"scatter","mode":"markers+lines","marker":{"autocolorscale":false,"color":"rgba(0,191,196,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,191,196,1)"}},"hoveron":"points","name":"median_gdp","legendgroup":"median_gdp","showlegend":false,"xaxis":"x2","yaxis":"y2","hoverinfo":"text","line":{"width":1.88976377952756,"color":"rgba(0,191,196,1)","dash":"solid"},"frame":null},{"x":[1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007],"y":[2.47544775548677,2.52633554851624,2.55047690303059,2.61592640520288,2.66661110417941,2.70098023955392,2.66484066673129,2.59092670732778,2.61373278431849,2.49441679392038,2.38231585779586,2.44334414004637],"text":["year: 1952<br />GDP_value:    298.8462<br />Stat_GDP: min_gdp","year: 1957<br />GDP_value:    335.9971<br />Stat_GDP: min_gdp","year: 1962<br />GDP_value:    355.2032<br />Stat_GDP: min_gdp","year: 1967<br />GDP_value:    412.9775<br />Stat_GDP: min_gdp","year: 1972<br />GDP_value:    464.0995<br />Stat_GDP: min_gdp","year: 1977<br />GDP_value:    502.3197<br />Stat_GDP: min_gdp","year: 1982<br />GDP_value:    462.2114<br />Stat_GDP: min_gdp","year: 1987<br />GDP_value:    389.8762<br />Stat_GDP: min_gdp","year: 1992<br />GDP_value:    410.8968<br />Stat_GDP: min_gdp","year: 1997<br />GDP_value:    312.1884<br />Stat_GDP: min_gdp","year: 2002<br />GDP_value:    241.1659<br />Stat_GDP: min_gdp","year: 2007<br />GDP_value:    277.5519<br />Stat_GDP: min_gdp"],"type":"scatter","mode":"markers+lines","marker":{"autocolorscale":false,"color":"rgba(199,124,255,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(199,124,255,1)"}},"hoveron":"points","name":"min_gdp","legendgroup":"min_gdp","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","line":{"width":1.88976377952756,"color":"rgba(199,124,255,1)","dash":"solid"},"frame":null},{"x":[1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007],"y":[3.14541928995783,3.18876063516304,3.22066691101895,3.16198386397309,3.21865546998006,3.27283885748582,3.30344652534648,3.26079047913757,3.1632536876415,3.12766413697526,3.10392849669425,3.0797733480287],"text":["year: 1952<br />GDP_value:   1397.7171<br />Stat_GDP: min_gdp","year: 1957<br />GDP_value:   1544.4030<br />Stat_GDP: min_gdp","year: 1962<br />GDP_value:   1662.1374<br />Stat_GDP: min_gdp","year: 1967<br />GDP_value:   1452.0577<br />Stat_GDP: min_gdp","year: 1972<br />GDP_value:   1654.4569<br />Stat_GDP: min_gdp","year: 1977<br />GDP_value:   1874.2989<br />Stat_GDP: min_gdp","year: 1982<br />GDP_value:   2011.1595<br />Stat_GDP: min_gdp","year: 1987<br />GDP_value:   1823.0160<br />Stat_GDP: min_gdp","year: 1992<br />GDP_value:   1456.3095<br />Stat_GDP: min_gdp","year: 1997<br />GDP_value:   1341.7269<br />Stat_GDP: min_gdp","year: 2002<br />GDP_value:   1270.3649<br />Stat_GDP: min_gdp","year: 2007<br />GDP_value:   1201.6372<br />Stat_GDP: min_gdp"],"type":"scatter","mode":"markers+lines","marker":{"autocolorscale":false,"color":"rgba(199,124,255,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(199,124,255,1)"}},"hoveron":"points","name":"min_gdp","legendgroup":"min_gdp","showlegend":false,"xaxis":"x2","yaxis":"y","hoverinfo":"text","line":{"width":1.88976377952756,"color":"rgba(199,124,255,1)","dash":"solid"},"frame":null},{"x":[1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007],"y":[2.51982799377572,2.54406804435028,2.58883172559421,2.54282542695918,2.55266821611219,2.56937390961505,2.62736585659273,2.5854607295085,2.54032947479087,2.61804809671209,2.78604121024255,2.97497199429807],"text":["year: 1952<br />GDP_value:    331.0000<br />Stat_GDP: min_gdp","year: 1957<br />GDP_value:    350.0000<br />Stat_GDP: min_gdp","year: 1962<br />GDP_value:    388.0000<br />Stat_GDP: min_gdp","year: 1967<br />GDP_value:    349.0000<br />Stat_GDP: min_gdp","year: 1972<br />GDP_value:    357.0000<br />Stat_GDP: min_gdp","year: 1977<br />GDP_value:    371.0000<br />Stat_GDP: min_gdp","year: 1982<br />GDP_value:    424.0000<br />Stat_GDP: min_gdp","year: 1987<br />GDP_value:    385.0000<br />Stat_GDP: min_gdp","year: 1992<br />GDP_value:    347.0000<br />Stat_GDP: min_gdp","year: 1997<br />GDP_value:    415.0000<br />Stat_GDP: min_gdp","year: 2002<br />GDP_value:    611.0000<br />Stat_GDP: min_gdp","year: 2007<br />GDP_value:    944.0000<br />Stat_GDP: min_gdp"],"type":"scatter","mode":"markers+lines","marker":{"autocolorscale":false,"color":"rgba(199,124,255,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(199,124,255,1)"}},"hoveron":"points","name":"min_gdp","legendgroup":"min_gdp","showlegend":false,"xaxis":"x3","yaxis":"y","hoverinfo":"text","line":{"width":1.88976377952756,"color":"rgba(199,124,255,1)","dash":"solid"},"frame":null},{"x":[1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007],"y":[2.98835076435393,3.13161519254539,3.23291576584681,3.33693028267941,3.45639180910827,3.54758782074795,3.56001198214669,3.57274765208091,3.39749469826406,3.50420634541484,3.66315528761115,3.77356920876217],"text":["year: 1952<br />GDP_value:    973.5332<br />Stat_GDP: min_gdp","year: 1957<br />GDP_value:   1353.9892<br />Stat_GDP: min_gdp","year: 1962<br />GDP_value:   1709.6837<br />Stat_GDP: min_gdp","year: 1967<br />GDP_value:   2172.3524<br />Stat_GDP: min_gdp","year: 1972<br />GDP_value:   2860.1698<br />Stat_GDP: min_gdp","year: 1977<br />GDP_value:   3528.4813<br />Stat_GDP: min_gdp","year: 1982<br />GDP_value:   3630.8807<br />Stat_GDP: min_gdp","year: 1987<br />GDP_value:   3738.9327<br />Stat_GDP: min_gdp","year: 1992<br />GDP_value:   2497.4379<br />Stat_GDP: min_gdp","year: 1997<br />GDP_value:   3193.0546<br />Stat_GDP: min_gdp","year: 2002<br />GDP_value:   4604.2117<br />Stat_GDP: min_gdp","year: 2007<br />GDP_value:   5937.0295<br />Stat_GDP: min_gdp"],"type":"scatter","mode":"markers+lines","marker":{"autocolorscale":false,"color":"rgba(199,124,255,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(199,124,255,1)"}},"hoveron":"points","name":"min_gdp","legendgroup":"min_gdp","showlegend":false,"xaxis":"x","yaxis":"y2","hoverinfo":"text","line":{"width":1.88976377952756,"color":"rgba(199,124,255,1)","dash":"solid"},"frame":null},{"x":[1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007],"y":[4.00171622128976,4.03940022113372,4.08697263830624,4.16028597892551,4.20536779687056,4.21041798943211,4.24631168563251,4.27891794551479,4.26395131915053,4.3232606368112,4.365297028372,4.40114211228811],"text":["year: 1952<br />GDP_value:  10039.5956<br />Stat_GDP: min_gdp","year: 1957<br />GDP_value:  10949.6496<br />Stat_GDP: min_gdp","year: 1962<br />GDP_value:  12217.2269<br />Stat_GDP: min_gdp","year: 1967<br />GDP_value:  14463.9189<br />Stat_GDP: min_gdp","year: 1972<br />GDP_value:  16046.0373<br />Stat_GDP: min_gdp","year: 1977<br />GDP_value:  16233.7177<br />Stat_GDP: min_gdp","year: 1982<br />GDP_value:  17632.4104<br />Stat_GDP: min_gdp","year: 1987<br />GDP_value:  19007.1913<br />Stat_GDP: min_gdp","year: 1992<br />GDP_value:  18363.3249<br />Stat_GDP: min_gdp","year: 1997<br />GDP_value:  21050.4138<br />Stat_GDP: min_gdp","year: 2002<br />GDP_value:  23189.8014<br />Stat_GDP: min_gdp","year: 2007<br />GDP_value:  25185.0091<br />Stat_GDP: min_gdp"],"type":"scatter","mode":"markers+lines","marker":{"autocolorscale":false,"color":"rgba(199,124,255,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(199,124,255,1)"}},"hoveron":"points","name":"min_gdp","legendgroup":"min_gdp","showlegend":false,"xaxis":"x2","yaxis":"y2","hoverinfo":"text","line":{"width":1.88976377952756,"color":"rgba(199,124,255,1)","dash":"solid"},"frame":null}],"layout":{"margin":{"t":55.4520547945205,"r":7.30593607305936,"b":40.1826484018265,"l":54.7945205479452},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":"Summarized GDP per Capital vs Year","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"xaxis":{"domain":[0,0.325179386823222],"automargin":true,"type":"linear","autorange":false,"range":[1949.25,2009.75],"tickmode":"array","ticktext":["1950","1960","1970","1980","1990","2000"],"tickvals":[1950,1960,1970,1980,1990,2000],"categoryorder":"array","categoryarray":["1950","1960","1970","1980","1990","2000"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y2","title":"","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"annotations":[{"text":"year","x":0.5,"y":-0.0353881278538813,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"top","annotationType":"axis"},{"text":"GDP distribution","x":-0.0448467058056099,"y":0.5,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-90,"xanchor":"right","yanchor":"center","annotationType":"axis"},{"text":"Africa","x":0.162589693411611,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.689497716895},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"Americas","x":0.5,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.689497716895},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"Asia","x":0.837410306588389,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.689497716895},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"Europe","x":0.162589693411611,"y":0.470319634703196,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.689497716895},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"Oceania","x":0.5,"y":0.470319634703196,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.689497716895},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"Stat_GDP","x":1.02,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"left","yanchor":"bottom","legendTitle":true}],"yaxis":{"domain":[0.529680365296804,1],"automargin":true,"type":"linear","autorange":false,"range":[2.24867743229327,5.18872279335025],"tickmode":"array","ticktext":["1e+03","1e+04","1e+05"],"tickvals":[3,4,5],"categoryorder":"array","categoryarray":["1e+03","1e+04","1e+05"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":"","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":0.325179386823222,"y0":0.529680365296804,"y1":1},{"type":"rect","fillcolor":"rgba(217,217,217,1)","line":{"color":"transparent","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0,"x1":0.325179386823222,"y0":0,"y1":23.37899543379,"yanchor":1,"ysizemode":"pixel"},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0.341487279843444,"x1":0.658512720156556,"y0":0.529680365296804,"y1":1},{"type":"rect","fillcolor":"rgba(217,217,217,1)","line":{"color":"transparent","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0.341487279843444,"x1":0.658512720156556,"y0":0,"y1":23.37899543379,"yanchor":1,"ysizemode":"pixel"},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0.674820613176778,"x1":1,"y0":0.529680365296804,"y1":1},{"type":"rect","fillcolor":"rgba(217,217,217,1)","line":{"color":"transparent","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0.674820613176778,"x1":1,"y0":0,"y1":23.37899543379,"yanchor":1,"ysizemode":"pixel"},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":0.325179386823222,"y0":0,"y1":0.470319634703196},{"type":"rect","fillcolor":"rgba(217,217,217,1)","line":{"color":"transparent","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0,"x1":0.325179386823222,"y0":0,"y1":23.37899543379,"yanchor":0.470319634703196,"ysizemode":"pixel"},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0.341487279843444,"x1":0.658512720156556,"y0":0,"y1":0.470319634703196},{"type":"rect","fillcolor":"rgba(217,217,217,1)","line":{"color":"transparent","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0.341487279843444,"x1":0.658512720156556,"y0":0,"y1":23.37899543379,"yanchor":0.470319634703196,"ysizemode":"pixel"}],"xaxis2":{"type":"linear","autorange":false,"range":[1949.25,2009.75],"tickmode":"array","ticktext":["1950","1960","1970","1980","1990","2000"],"tickvals":[1950,1960,1970,1980,1990,2000],"categoryorder":"array","categoryarray":["1950","1960","1970","1980","1990","2000"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0.341487279843444,0.658512720156556],"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y2","title":"","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"xaxis3":{"type":"linear","autorange":false,"range":[1949.25,2009.75],"tickmode":"array","ticktext":["1950","1960","1970","1980","1990","2000"],"tickvals":[1950,1960,1970,1980,1990,2000],"categoryorder":"array","categoryarray":["1950","1960","1970","1980","1990","2000"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0.674820613176778,1],"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y2","title":"","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"yaxis2":{"type":"linear","autorange":false,"range":[2.24867743229327,5.18872279335025],"tickmode":"array","ticktext":["1e+03","1e+04","1e+05"],"tickvals":[3,4,5],"categoryorder":"array","categoryarray":["1e+03","1e+04","1e+05"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0,0.470319634703196],"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":"","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"y":0.93503937007874},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"cloud":false},"source":"A","attrs":{"147fcdfb6348":{"x":{},"y":{},"colour":{},"type":"scatter"},"147fc4abe610f":{"x":{},"y":{},"colour":{}}},"cur_data":"147fcdfb6348","visdat":{"147fcdfb6348":["function (y) ","x"],"147fc4abe610f":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":[]}</script><!--/html_preserve-->



