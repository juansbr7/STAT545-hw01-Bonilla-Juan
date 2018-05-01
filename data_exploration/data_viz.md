Data viz
================
juan
2017-10-18

##### 1. GAPMINDER DATA

-   Drop Oceania. Filter the Gapminder data to remove observations associated with the continent of Oceania. Additionally, remove unused factor levels. Provide concrete information on the data before and after removing these rows and Oceania; address the number of rows and the levels of the affected factors.

``` r
library(gapminder)
suppressMessages(library(tidyverse))
library(forcats)
library(ggplot2)
suppressMessages(library(gridExtra) )
suppressMessages(library(mgcv))
library(png)
library(grid)
library(RColorBrewer)
```

-   Let's start by exploring the factors in the gapminder dataframe. There are 5 levels in the continent factor and 142 levels in the countries, the number of rows (obs) 1704. So if we drop Ocenia and check the levels we should expect to see 4 levels. Oceania only has two countries in this dataframe so we should see 140 levels in the country factor and less than 1704 rows.

``` r
str(gapminder)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1704 obs. of  6 variables:
    ##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

``` r
gapminder %>% filter(continent== "Oceania") %>% count(country)
```

    ## # A tibble: 2 x 2
    ##   country         n
    ##   <fct>       <int>
    ## 1 Australia      12
    ## 2 New Zealand    12

``` r
conts <- c("Americas", "Europe", "Asia", "Africa")
no_oceania <- gapminder %>%
  filter(continent %in% conts)
str(no_oceania)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1680 obs. of  6 variables:
    ##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

-   Ooops The number of rows decreased to 1680 but there are still 5 levels and 142 levels which means that the filter did not dropped the factot levels belonging to country and continent. Let's use droplevels().

``` r
no_oceania <- gapminder %>%
  filter(continent %in% conts) %>% 
  droplevels() 

str(no_oceania)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1680 obs. of  6 variables:
    ##  $ country  : Factor w/ 140 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 4 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

``` r
nrow(no_oceania)
```

    ## [1] 1680

Ok, dropped. Now we really have 4 continents. Now, I'll order the countries based on their population, from the largest to the lowest. It is a pretty long list so I'll disploy only the first 5.

``` r
fct_reorder(no_oceania$country, no_oceania$pop, max, .desc = TRUE) %>% levels() %>% head()
```

    ## [1] "China"         "India"         "United States" "Indonesia"    
    ## [5] "Brazil"        "Pakistan"

-   Does merely arranging the data have any effect on, say, a figure? what effect does this have on a figure? Let's see figures with ordered and unordered levels. I'll save my graphs in ***.png*** format using ggsave and I'll use ***saveRDS()*** to save my data.

``` r
americas_data <- gapminder %>% filter( continent == "Americas") %>% mutate(pop= pop/10^6) 
plot_a<- ggplot(americas_data, aes(x = pop, y = country)) + geom_point(aes(colour= year))+ labs(x="", y="")+ guides(colour= FALSE)
plot_b<- ggplot(americas_data, aes(x = pop, y = fct_reorder(country, pop))) +
  geom_point(aes(colour= year)) + labs(x="", y="")+ 
  theme( legend.position = c(.95, .10),
  legend.justification = c("right", "bottom"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6))
  
grid.arrange(plot_a, plot_b, ncol= 2, top= "Levels= unordered vs ordered", left= "countries", bottom= "Population in Millions")
```

![](data_viz_files/figure-markdown_github/figure%201-1.png)

``` r
saveRDS(americas_data, "americas_data.rds")
ggsave("plot_a.png", plot_a)
```

    ## Saving 7 x 5 in image

``` r
ggsave("plot_b.png", plot_b)
```

    ## Saving 7 x 5 in image

##### Visualization design:

-   Here I will work with the mean of the gdpPercapita and ordered values according to bothe the max and min gdpPercapita in the Americas. In this case I'll save my graphs independently in .png and the grids as pdfs. I'll also toy a bit more with the background, the shapes and colors in the graphs.

``` r
img<-readPNG("dollar.png")
```

    ## Warning in readPNG("dollar.png"): libpng warning: iCCP: known incorrect
    ## sRGB profile

``` r
g <- rasterGrob(img, interpolate=TRUE)
ordered_americas_data <- readRDS("americas_data.rds") %>% droplevels() %>% group_by(country)  %>%   summarise(meangdp= mean(gdpPercap), maxgdp = max(gdpPercap), mingdp = min(gdpPercap)) %>% mutate(country = fct_reorder(country, mingdp, min))
ordered_americas_data %>% head(10) %>% knitr::kable(format = "markdown", padding=2, caption= "default alphabetical order")
```

| country            |    meangdp|     maxgdp|     mingdp|
|:-------------------|----------:|----------:|----------:|
| Argentina          |   8955.554|  12779.380|   5911.315|
| Bolivia            |   2961.229|   3822.137|   2127.686|
| Brazil             |   5829.317|   9065.801|   2108.944|
| Canada             |  22410.746|  36319.235|  11367.161|
| Chile              |   6703.289|  13171.639|   3939.979|
| Colombia           |   4195.343|   7006.580|   2144.115|
| Costa Rica         |   5448.611|   9645.061|   2627.009|
| Cuba               |   6283.259|   8948.103|   5180.756|
| Dominican Republic |   2844.856|   6025.375|   1397.717|
| Ecuador            |   5733.625|   7429.456|   3522.111|

``` r
levels(ordered_americas_data$country) %>% head(10) %>%   knitr::kable(format = "markdown", padding=2, col.names =  "reordered based on mingdp")
```

| reordered based on mingdp |
|:--------------------------|
| Haiti                     |
| Dominican Republic        |
| Paraguay                  |
| Brazil                    |
| Bolivia                   |
| Colombia                  |
| Nicaragua                 |
| Honduras                  |
| Guatemala                 |
| Panama                    |

``` r
plot_c<- ggplot(ordered_americas_data, aes(x = meangdp, y = country)) + geom_point(aes(mingdp), colour= "orangered4", size= 5, shape= "-")+ geom_point(aes(maxgdp), colour= "darkgreen", size= 5, shape= "+")+ labs(y= "", x= "")+ ggtitle(" Ordered by Mingdp(-)")+ annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

plot_d<- ggplot(ordered_americas_data, aes(x = meangdp, y = fct_reorder(country, maxgdp))) + geom_point(aes(mingdp), colour= "orangered4", size= 5, shape= "-")+ geom_point(aes(maxgdp), colour= "darkgreen", size= 5, shape= "+")+ labs(y= "", x= "")+ ggtitle("Ordered by Maxgdp(+)") + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
ggsave("plot_b.png", plot_b)
```

    ## Saving 7 x 5 in image

``` r
grid_a_b<- grid.arrange(plot_c, plot_d, ncol= 2, top= "GdpPercapita in America ordered by the Maximun and the Minimun", left= "countries", bottom= "gdpPercapita")
```

![](data_viz_files/figure-markdown_github/figure%202-1.png)

``` r
grid_a_b
```

    ## TableGrob (3 x 3) "arrange": 5 grobs
    ##   z     cells    name                grob
    ## 1 1 (2-2,2-2) arrange      gtable[layout]
    ## 2 2 (2-2,3-3) arrange      gtable[layout]
    ## 3 3 (1-1,2-3) arrange text[GRID.text.327]
    ## 4 4 (3-3,2-3) arrange text[GRID.text.328]
    ## 5 5 (1-3,1-1) arrange text[GRID.text.329]

``` r
pdf("grid_a_b.pdf", width=12, height=6)
```

-   Here I took two of the graphs from previous assignements and tweaked them based on recommendations of the peer reviewers and TAs. I also gave color brewer a try and used some of their palettes (Set3).

``` r
 south_america<-  c("Colombia", "Chile", "Argentina", "Uruguay", "Ecuador", "Bolivia", "Paraguay", "Peru", "Brazil", "Venezuela")

s_america<- gapminder %>% filter(country %in% south_america) %>% droplevels() %>%  group_by(country)
write.csv(s_america, "s_america.csv") 
 
plot_c<-ggplot(s_america,aes(year, gdpPercap)) +
  geom_point(aes(color= country, size= pop/10^6 ))+ geom_line(aes(colour= country))+ scale_size_continuous("Population in Millions") + scale_colour_manual(values=brewer.pal(n=10, "Set3"))
plot_c
```

![](data_viz_files/figure-markdown_github/figure%203-1.png)

``` r
ggsave("plot_c.png", plot_c)
```

    ## Saving 7 x 5 in image

-   Now let's look at some min, max life expectancies in all continents.

``` r
lifeEx_world<- gapminder %>%
  group_by(year, continent) %>% 
  select(lifeExp, continent) %>% 
summarise(mean_le= mean(lifeExp, trim= 0.2), max_le= max(lifeExp), min_le= min(lifeExp), md_le= median(lifeExp)) 
```

    ## Adding missing grouping variables: `year`

``` r
maxlifeEx_world<-lifeEx_world %>% 
  select(year, max_le, continent) %>% 
    spread(key = year, value = max_le) %>% 
  knitr::kable(format = "markdown", padding=2, caption= "Max Life Expectancy per year in all Countries")
maxlifeEx_world
```

<table style="width:100%;">
<colgroup>
<col width="10%" />
<col width="7%" />
<col width="7%" />
<col width="7%" />
<col width="7%" />
<col width="7%" />
<col width="7%" />
<col width="7%" />
<col width="7%" />
<col width="7%" />
<col width="7%" />
<col width="7%" />
<col width="7%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">continent</th>
<th align="right">1952</th>
<th align="right">1957</th>
<th align="right">1962</th>
<th align="right">1967</th>
<th align="right">1972</th>
<th align="right">1977</th>
<th align="right">1982</th>
<th align="right">1987</th>
<th align="right">1992</th>
<th align="right">1997</th>
<th align="right">2002</th>
<th align="right">2007</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Africa</td>
<td align="right">52.724</td>
<td align="right">58.089</td>
<td align="right">60.246</td>
<td align="right">61.557</td>
<td align="right">64.274</td>
<td align="right">67.064</td>
<td align="right">69.885</td>
<td align="right">71.913</td>
<td align="right">73.615</td>
<td align="right">74.772</td>
<td align="right">75.744</td>
<td align="right">76.442</td>
</tr>
<tr class="even">
<td align="left">Americas</td>
<td align="right">68.750</td>
<td align="right">69.960</td>
<td align="right">71.300</td>
<td align="right">72.130</td>
<td align="right">72.880</td>
<td align="right">74.210</td>
<td align="right">75.760</td>
<td align="right">76.860</td>
<td align="right">77.950</td>
<td align="right">78.610</td>
<td align="right">79.770</td>
<td align="right">80.653</td>
</tr>
<tr class="odd">
<td align="left">Asia</td>
<td align="right">65.390</td>
<td align="right">67.840</td>
<td align="right">69.390</td>
<td align="right">71.430</td>
<td align="right">73.420</td>
<td align="right">75.380</td>
<td align="right">77.110</td>
<td align="right">78.670</td>
<td align="right">79.360</td>
<td align="right">80.690</td>
<td align="right">82.000</td>
<td align="right">82.603</td>
</tr>
<tr class="even">
<td align="left">Europe</td>
<td align="right">72.670</td>
<td align="right">73.470</td>
<td align="right">73.680</td>
<td align="right">74.160</td>
<td align="right">74.720</td>
<td align="right">76.110</td>
<td align="right">76.990</td>
<td align="right">77.410</td>
<td align="right">78.770</td>
<td align="right">79.390</td>
<td align="right">80.620</td>
<td align="right">81.757</td>
</tr>
<tr class="odd">
<td align="left">Oceania</td>
<td align="right">69.390</td>
<td align="right">70.330</td>
<td align="right">71.240</td>
<td align="right">71.520</td>
<td align="right">71.930</td>
<td align="right">73.490</td>
<td align="right">74.740</td>
<td align="right">76.320</td>
<td align="right">77.560</td>
<td align="right">78.830</td>
<td align="right">80.370</td>
<td align="right">81.235</td>
</tr>
</tbody>
</table>

``` r
minlifeEx_world<-lifeEx_world %>% 
  select(year, min_le, continent) %>% 
    spread(key = year, value = min_le) %>% 
  knitr::kable(format = "markdown", padding=2, caption= "Min Life Expectancy per year in all Countries")
minlifeEx_world
```

<table style="width:100%;">
<colgroup>
<col width="10%" />
<col width="7%" />
<col width="7%" />
<col width="7%" />
<col width="7%" />
<col width="7%" />
<col width="7%" />
<col width="7%" />
<col width="7%" />
<col width="7%" />
<col width="7%" />
<col width="7%" />
<col width="7%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">continent</th>
<th align="right">1952</th>
<th align="right">1957</th>
<th align="right">1962</th>
<th align="right">1967</th>
<th align="right">1972</th>
<th align="right">1977</th>
<th align="right">1982</th>
<th align="right">1987</th>
<th align="right">1992</th>
<th align="right">1997</th>
<th align="right">2002</th>
<th align="right">2007</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Africa</td>
<td align="right">30.000</td>
<td align="right">31.570</td>
<td align="right">32.767</td>
<td align="right">34.113</td>
<td align="right">35.400</td>
<td align="right">36.788</td>
<td align="right">38.445</td>
<td align="right">39.906</td>
<td align="right">23.599</td>
<td align="right">36.087</td>
<td align="right">39.193</td>
<td align="right">39.613</td>
</tr>
<tr class="even">
<td align="left">Americas</td>
<td align="right">37.579</td>
<td align="right">40.696</td>
<td align="right">43.428</td>
<td align="right">45.032</td>
<td align="right">46.714</td>
<td align="right">49.923</td>
<td align="right">51.461</td>
<td align="right">53.636</td>
<td align="right">55.089</td>
<td align="right">56.671</td>
<td align="right">58.137</td>
<td align="right">60.916</td>
</tr>
<tr class="odd">
<td align="left">Asia</td>
<td align="right">28.801</td>
<td align="right">30.332</td>
<td align="right">31.997</td>
<td align="right">34.020</td>
<td align="right">36.088</td>
<td align="right">31.220</td>
<td align="right">39.854</td>
<td align="right">40.822</td>
<td align="right">41.674</td>
<td align="right">41.763</td>
<td align="right">42.129</td>
<td align="right">43.828</td>
</tr>
<tr class="even">
<td align="left">Europe</td>
<td align="right">43.585</td>
<td align="right">48.079</td>
<td align="right">52.098</td>
<td align="right">54.336</td>
<td align="right">57.005</td>
<td align="right">59.507</td>
<td align="right">61.036</td>
<td align="right">63.108</td>
<td align="right">66.146</td>
<td align="right">68.835</td>
<td align="right">70.845</td>
<td align="right">71.777</td>
</tr>
<tr class="odd">
<td align="left">Oceania</td>
<td align="right">69.120</td>
<td align="right">70.260</td>
<td align="right">70.930</td>
<td align="right">71.100</td>
<td align="right">71.890</td>
<td align="right">72.220</td>
<td align="right">73.840</td>
<td align="right">74.320</td>
<td align="right">76.330</td>
<td align="right">77.550</td>
<td align="right">79.110</td>
<td align="right">80.204</td>
</tr>
</tbody>
</table>

``` r
plot_d<- lifeEx_world %>% 
  ggplot(aes(x= year, y= mean_le )) +
  facet_grid(~ continent)+
    geom_ribbon(aes(ymin= min_le, ymax= max_le), fill= "darkblue")+
  geom_line(aes(y= mean_le, colour= "mean_le"))+
      theme(axis.text.x = element_text(angle=90),
          plot.title = element_text(hjust=0.5))+
    geom_line(aes(y=md_le , colour= "md_le"))+
      ggtitle("Max, Min, Mean and Median Life Expectancy in all Continents")+
      ylab("Life Expectancy")+
      xlab("years")
plot_d
```

![](data_viz_files/figure-markdown_github/figure%204-1.png)

``` r
ggsave("plot_d.png", plot_d)
```

    ## Saving 7 x 5 in image

-   Extra bit: I guess gapminder forgot to add the important detail of weird\_food in some countries. We all know that Japan is known for its sushi, but also, for its tuna eyeballs. Let's see others:

``` r
c<-  c("France", "Thailand", "United States", "China", "Japan", "Ecuador", "Cambodia", "Philippines", "Mexico", "Australia")
food<- c("frog legs", "fried insects", "fried rattlesnake", "rotten eggs", "tuna eyeballs", "guinea pig", "fried spider", "fertilized egg", "insect eggs", "grubs")
country<- factor(c)
weird_food<-factor(food)
newdf<- data_frame(country, weird_food)
newdf
```

    ## # A tibble: 10 x 2
    ##    country       weird_food       
    ##    <fct>         <fct>            
    ##  1 France        frog legs        
    ##  2 Thailand      fried insects    
    ##  3 United States fried rattlesnake
    ##  4 China         rotten eggs      
    ##  5 Japan         tuna eyeballs    
    ##  6 Ecuador       guinea pig       
    ##  7 Cambodia      fried spider     
    ##  8 Philippines   fertilized egg   
    ##  9 Mexico        insect eggs      
    ## 10 Australia     grubs

``` r
dataset1<- gapminder %>%
  filter(country %in% c, year== 2007) %>%
           droplevels() 
both<-left_join(dataset1, newdf, by= "country")
both%>% knitr::kable()
```

| country       | continent |  year|  lifeExp|         pop|  gdpPercap| weird\_food       |
|:--------------|:----------|-----:|--------:|-----------:|----------:|:------------------|
| Australia     | Oceania   |  2007|   81.235|    20434176|  34435.367| grubs             |
| Cambodia      | Asia      |  2007|   59.723|    14131858|   1713.779| fried spider      |
| China         | Asia      |  2007|   72.961|  1318683096|   4959.115| rotten eggs       |
| Ecuador       | Americas  |  2007|   74.994|    13755680|   6873.262| guinea pig        |
| France        | Europe    |  2007|   80.657|    61083916|  30470.017| frog legs         |
| Japan         | Asia      |  2007|   82.603|   127467972|  31656.068| tuna eyeballs     |
| Mexico        | Americas  |  2007|   76.195|   108700891|  11977.575| insect eggs       |
| Philippines   | Asia      |  2007|   71.688|    91077287|   3190.481| fertilized egg    |
| Thailand      | Asia      |  2007|   70.616|    65068149|   7458.396| fried insects     |
| United States | Americas  |  2007|   78.242|   301139947|  42951.653| fried rattlesnake |

``` r
ggplot(both, aes(country, weird_food, fill= weird_food))+ geom_bar(stat= "identity", colour= "black")+ scale_fill_manual(values=brewer.pal(n=10, "Paired"))+ labs(x="", y= "") +
    theme(axis.text.x = element_text(angle=90))+ ggtitle("Weird, Gross and Tasty")
```

![](data_viz_files/figure-markdown_github/figure%205-1.png)
