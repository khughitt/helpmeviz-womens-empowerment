---
author: "Keith Hughitt"
date: "06/28/2014"
output:
  knitrBootstrap::bootstrap_document:
    title: "Bread for the World: Women's Empowerment and Nutrition"
    theme: flatly
---



Bread for the World Hackathon: Women's Empowerment & Nutrition
==============================================================

Overview
--------

This analysis was put together for the [HelpMeViz](http://helpmeviz.com/)
[hackathon on Women's Empowerment &
Nutrition](http://helpmeviz.com/2014/06/28/hackathon-womens-empowerment-nutrition/)
which took place at the [Bread for the World
Institute](http://www.bread.org/institute/) on Saturday, June 28, 2014. The
purpose of the hackathon was to explore the relationship between the status of
Women in various countries and child nutrition and stunting. This is one part
of two separate visualization challenges addressed in the hackathon.

From the HelpMeViz description:

> As we begin to explore years of data on women’s empowerment from the World
> Bank and United Nations, we want to ask the question: Do countries that
> significantly improve the status of women also see lower rates of stunting?
> Women are the primary caregivers in the family. Research from countries
> around the world has shown that when women are empowered to earn more and
> have a greater say in home finances, they are more likely than men to invest
> the additional money in promoting the welfare of their children — through
> nutritious food, for example. In this project, Bread for the World Institute
> is interested in exploring whether and where women’s empowerment is
> associated with improvements in stunting and, if so, over what period of
> time. The answer to the question of whether the two indicators coincide is
> most likely to be "sometimes yes, sometimes no."

For more information on the goals of the visualization challenge, see:

* [HelpMeViz: Women's Empowerment &
  Nutrition](http://helpmeviz.com/2014/06/28/hackathon-womens-empowerment-nutrition/)
* [Bread for the World](http://www.bread.org/institute/)

Data Sources
------------

The data used for this analysis comes from several sources:

1. [The association of maternal age with infant mortality, child anthropometric
   failure, diarrhoea and anaemia for first births: evidence from 55 low- and
   middle-income countries (Finlay et al. 2011)](http://bmjopen.bmj.com/content/1/2/e000226.full)
2. [Women’s empowerment and nutrition: An evidence review
   (2013)](http://www.ifpri.org/publication/women-s-empowerment-and-nutrition)
3. [USAID: Women's Lives and Challenges: Equality and Empowerment since 2000
   (2014)](http://www.usaid.gov/news-information/press-releases/mar-7-2014-usaid-release-womens-lives-and-challenges-report)
4. [Hunger and Nutrition Commitment Index](http://www.hancindex.org/)
5. [The State of the World's
   Children (2014)](http://www.unicef.org/sowc2014/numbers/#statistics)
6. [GII: Gender Inequality Index over
   time](https://data.undp.org/dataset/GII-Gender-Inequality-Index-value/bh77-rzbn)

Some relevant tables from each of the above reports have been compiled into CSV
and excel spreadsheets and made available for the hackathon. The CSV versions
of those tables are included in the `input` folder.

A table containing country/region mappings was downloaded from:

* [https://github.com/lukes/](https://github.com/lukes/ISO-3166-Countries-with-Regional-Codes/blob/master/all/all.csv)

Finally, a mapping from region codes to region names was retrieved from:

* [https://github.com/nexgenta/geo/](https://github.com/nexgenta/geo/blob/master/data/00-georegion.csv)

Data preparation
----------------

### Clean up stunting data


```r
library(dplyr)
options(StringsAsFactors=FALSE)

# Load stunting data
stunting = tbl_df(read.csv('input/World Bank Stunting and Wasting Data, by gender.csv'))
                           
# Grab data for the last 15 years
stunting = stunting %>% select(Country.Name, Country.Code, Indicator.Code, 
                               X2000, X2001, X2002, X2003, X2004, X2005, X2006,
                               X2007, X2008, X2009, X2010, X2011, X2012, X2013)

# Mean stunting for last 15 years
values = stunting  %>% select(-Country.Name, -Country.Code, -Indicator.Code)
stunting$Mean.Stunting = apply(values, 1, function(x) { mean(x, na.rm=T) })

# Most recent data point for each Country for which data is available during
# this time period
stunting$Most.Recent.Stunting = as.numeric(apply(values, 1, function(x) {
                                                 tail(x[!is.na(x)], 1) }))

# Drop the individual year columns
stunting = stunting %>% select(Country.Name, Country.Code, Indicator.Code,
                               Mean.Stunting, Most.Recent.Stunting)

# Drop Countries with no recent data
stunting = stunting %>% filter(!is.nan(Mean.Stunting))

# Male vs. Female Stunting
male_stunting = stunting %>% filter(Indicator.Code == 'SH.STA.STNT.MA.ZS')
female_stunting = stunting %>% filter(Indicator.Code == 'SH.STA.STNT.FE.ZS')

# Save output
write.csv(male_stunting, file='output/male_stunting.csv', row.names=FALSE)
write.csv(female_stunting, file='output/female_stunting.csv', row.names=FALSE)
```

### Combined dataset

The mean stunting rates for male and female from above were combined with
predictor variables from other datasets into a single file to make it easier to
visualize. Let's now load that complete dataset and see what we can find.

#### Data fields

The fields contained in the dataset `Stunting4.csv` include:

* Country name
* 2012 Seats in National Parliament (% female)
* 2010 Maternal Mortality Ratio
* Adolescent Fertility Rate
* 2006-2010 Population with at least secondary education (Female)
* 2006-2010 Population with at least secondary education (Male)
* 2011 Labour force participation rate (Female)
* 2011 Labour force participation rate (Male)
* 2010 Global Inequality Index (GII)
* 2012 Global Inequality Index (GII)
* 2013 Global Hunger Index (IFPRI)
* 2011 Under-five Mortality rate (IFPRI)
* 2010-2012 Prevalence of undernourishment in the population (IFPRI)
* 2008-2012 Prevalence of underweight in children under five years (IFPRI)
* Percent children with stunting (Female)
* Percent children with stunting (Male)


```r
# load cleaned data
df = tbl_df(read.csv('input/Stunting4.csv'))

# let's make names easy to type..
colnames(df) = c("country", "seats_parlim_female", "maternal_mortality_ratio",
                 "adolescent_fertility_rate", "secondary_education_female",
                 "secondary_education_male", "labor_participation_female",
                 "labor_participation_male", "GII_2010", "GII_2012",
                 "ifpri_hunger_index", "ifpri_under5_mortality",
                 "ifpri_undernourishment", "ifpri_under5_underweight",
                 "mean_stunting_female", "mean_stunting_male")

# more sensible row names
rownames(df) = df$country

# load region info
region_info = read.delim('input/ISO-3166-Countries-with-Regional-Codes.csv')
region_names = read.csv('input/georegion.csv')

# add region names main dataframe
region_info$region = region_names$Region[match(region_info$region.code,
                                             region_names$M49.numeric.code)]
regions = region_info$region[match(df$country, region_info$name)]

# store country names separately to make it easy to work with the remaining
# numeric predictor and outcome variables
country_names = as.character(df$country)
df = df %>% select(-country)

# what are we missing?
print("Number of missing datapoints for each variable:")
```

```
## [1] "Number of missing datapoints for each variable:"
```

```r
apply(df, 2, function (x) {sum(is.na(x))})
```

```
##        seats_parlim_female   maternal_mortality_ratio 
##                          1                          0 
##  adolescent_fertility_rate secondary_education_female 
##                          0                          8 
##   secondary_education_male labor_participation_female 
##                          8                          1 
##   labor_participation_male                   GII_2010 
##                          1                         17 
##                   GII_2012         ifpri_hunger_index 
##                         10                          4 
##     ifpri_under5_mortality     ifpri_undernourishment 
##                          0                          4 
##   ifpri_under5_underweight       mean_stunting_female 
##                          0                          0 
##         mean_stunting_male 
##                          0
```

```r
print("Countries with missing data:")
```

```
## [1] "Countries with missing data:"
```

```r
country_names[!complete.cases(df)]
```

```
##  [1] "Azerbaijan"   "Belarus"      "Bhutan"       "Burkina Faso"
##  [5] "Chad"         "Comoros"      "Djibouti"     "Eritrea"     
##  [9] "Ethiopia"     "Fiji"         "Iraq"         "Lebanon"     
## [13] "Myanmar"      "Oman"         "Serbia"       "Suriname"    
## [17] "Timor-Leste"  "Tunisia"      "Uzbekistan"
```

```r
# create a version which includes only those countries that have all data
# fields populated
df_complete = df[complete.cases(df),]
country_names_complete = country_names[complete.cases(df)]
regions_complete = regions[complete.cases(df)]
```

Let's see what the dataset looks like now at this point:


```r
kable(df_complete[1:5,2:5])
```



|         | maternal_mortality_ratio| adolescent_fertility_rate| secondary_education_female| secondary_education_male|
|:--------|------------------------:|-------------------------:|--------------------------:|------------------------:|
|Albania  |                       27|                      14.9|                       78.8|                     85.0|
|Armenia  |                       30|                      33.2|                       94.1|                     94.8|
|Benin    |                      350|                      97.0|                       11.2|                     25.6|
|Botswana |                      160|                      43.8|                       73.6|                     77.5|
|Brazil   |                       56|                      76.0|                       50.5|                     48.5|

```r
kable(df_complete[1:5,6:10])
```



|         | labor_participation_female| labor_participation_male| GII_2010| GII_2012| ifpri_hunger_index|
|:--------|--------------------------:|------------------------:|--------:|--------:|------------------:|
|Albania  |                       49.6|                     71.3|    0.261|    0.251|                5.2|
|Armenia  |                       49.4|                     70.2|    0.355|    0.340|                5.0|
|Benin    |                       67.4|                     78.2|    0.614|    0.618|               13.3|
|Botswana |                       71.7|                     81.6|    0.497|    0.485|               13.9|
|Brazil   |                       59.6|                     80.9|    0.446|    0.447|                5.0|

```r
kable(df_complete[1:5,11:15])
```



|         | ifpri_under5_mortality| ifpri_undernourishment| ifpri_under5_underweight| mean_stunting_female| mean_stunting_male|
|:--------|----------------------:|----------------------:|------------------------:|--------------------:|------------------:|
|Albania  |                    1.4|                    7.8|                      6.3|                28.43|              30.93|
|Armenia  |                    1.8|                    3.0|                      5.3|                18.10|              19.57|
|Benin    |                   10.6|                    8.1|                     21.2|                39.50|              44.40|
|Botswana |                    2.6|                   27.9|                     11.2|                27.65|              32.80|
|Brazil   |                    1.6|                    6.9|                      3.0|                 5.80|               8.30|

Visualization
-------------

### How do the predictors relate to one another? (In progress...)

Let's start by exploring the relationship between the various predictors and
our outcome of interest; rate of stunting. Below, a data-dimension reduction
tool called a [biplot](http://en.wikipedia.org/wiki/Biplot) is used for this.

[SVG version](figure/biplot.svg).


```r
library(bpca)
library(RColorBrewer)

# region colors
region_colors = brewer.pal(length(unique(regions_complete)), "Set1")[
                    as.numeric(as.factor((as.character(regions_complete))))]
names(region_colors) = regions_complete

# biplot of uncleaned data
plot(bpca(df_complete),
     var.factor=.5,
     obj.names=TRUE,
     obj.col=region_colors,
     obj.labels=country_names_complete)
title("biplot (original)")
```

![plot of chunk biplot_orig](figure/biplot_orig.png) 

Another way to visualize these relationships is using a heatmap:


```r
library(gplots)
heatmap.2(log1p(as.matrix(df_complete)), 
          RowSideColors=region_colors,
          trace="none", margins=c(12,8), 
          xlab="Measure", ylab="Country", main="Heatmap (original)")
```

![plot of chunk heatmap_orig](figure/heatmap_orig.png) 

A few interesting things to note here:

- GII 2010 & 2012 are *very* similar
- Maternal mortality looks like an outlier and does not seem to corerlate well
  with any of the other variables.
- Something looks amiss with labor_participation_male
- Not surprising, but there are some clear regional trends in the data (e.g.
  the bottom-most cluster of North and Central African countries).

Finally, let's look at the the correlations between the variables directly:


```r
heatmap.2(cor(df_complete), trace="none", margins=c(13,13), main="Variable
          correlations (original)")
```

![plot of chunk variable_correlations](figure/variable_correlations.png) 

Observations:

- Male and female education are highly correlated with one another, but do not
  correlate well with any of the other variables of interest.
- Nutrition and hunger related variables group together, as expected
- GII is moderately correlated with these variables, including stunting.

### Some more data cleaning

In order to continue simplifying our dataset, let's combine some of the fields
above that turn out to be very similar, namely:

- GII 2010 & 2012
- Stunting male & female
- Secondary education male & female

For now, let's also drop the labor participation male since it doesn't seem to
be affected by any of the other variables.


```r
# combine above variables and drop original columns
df_complete = df_complete %>% 
    mutate(
        mean_stunting = (mean_stunting_female + mean_stunting_male) / 2,
        GII = (GII_2010 + GII_2012) / 2,
        secondary_education = (secondary_education_male +
                               secondary_education_female) / 2) %>% 
    select(-mean_stunting_female, -mean_stunting_male, -GII_2010, -GII_2012,
           -secondary_education_male, -secondary_education_female,
           -labor_participation_male)
row.names(df_complete) = country_names_complete

# Save this dataset
write.csv(df_complete, file='output/Stunting_clean.csv', row.names=FALSE)
```

Let's redo some of the above plots to see how things look now...


```r
# biplot
plot(bpca(df_complete),
     var.factor=.5,
     obj.col=region_colors,
     obj.names=TRUE,
     obj.labels=country_names_complete)
title("Biplot (clean)")
```

![plot of chunk cleaned_data_plots](figure/cleaned_data_plots1.png) 

```r
# country and variable biclustering and heatmap
heatmap.2(log1p(as.matrix(df_complete)), 
          RowSideColors=region_colors,
          trace="none", margins=c(12,8), 
          xlab="Measure", ylab="Country", main="Heatmap (clean)")
```

![plot of chunk cleaned_data_plots](figure/cleaned_data_plots2.png) 

```r
# variable correlations
heatmap.2(cor(df_complete), trace="none", margins=c(13,13), main="Variable
          correlations (clean)")
```

![plot of chunk cleaned_data_plots](figure/cleaned_data_plots3.png) 

### Stunting Rate vs. Global Inequality Index

[SVG version](figure/stunting_vs_gii.svg)


```r
library(ggplot2)

rho = cor(df_complete$GII, df_complete$mean_stunting)

ggplot(df_complete, aes(GII, mean_stunting, label=country_names_complete)) + 
    geom_point(aes(color=regions_complete)) + 
    geom_text(aes(label=country_names_complete, hjust=-0.2, vjust=-0.2, size=0.4)) + 
    geom_smooth(method="lm") +
    xlab('Global Inequality Index') +
    ylab('Mean Stunting Rate') + 
    ggtitle(sprintf('Stunting rate vs. Gender Inequality (cor=%0.2f)', rho))
```

![plot of chunk stunting_vs_gii](figure/stunting_vs_gii.png) 

### Stunting vs. Secondary Education


```r
rho = cor(df_complete$secondary_education, df_complete$mean_stunting)

ggplot(df_complete, aes(secondary_education, mean_stunting, label=country_names_complete)) + 
    geom_point(aes(color=regions_complete)) + 
    geom_text(aes(label=country_names_complete, hjust=-0.2, vjust=-0.2, size=0.4)) + 
    geom_smooth(method="lm") +
    xlab('Rate of Population with Secondary Education or Higher') +
    ylab('Mean Stunting Rate') + 
    ggtitle(sprintf('Stunting rate vs. Rate of Population with Secondary Education or 
             Higher (cor=%0.2f)', rho))
```

![plot of chunk stunting_vs_education](figure/stunting_vs_education.png) 

System Information
------------------


```r
sessionInfo()
```

```
## R version 3.1.0 (2014-04-10)
## Platform: x86_64-unknown-linux-gnu (64-bit)
## 
## locale:
##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
##  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
##  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] ggplot2_1.0.0        gplots_2.13.0        RColorBrewer_1.0-5  
##  [4] bpca_1.2-2           rgl_0.93.996         scatterplot3d_0.3-35
##  [7] dplyr_0.2            knitr_1.6.5          rmarkdown_0.2.49    
## [10] knitrBootstrap_1.0.0 vimcom.plus_1.0-0    setwidth_1.0-3      
## [13] colorout_1.0-3      
## 
## loaded via a namespace (and not attached):
##  [1] assertthat_0.1     bitops_1.0-6       caTools_1.17      
##  [4] colorspace_1.2-4   digest_0.6.4       evaluate_0.5.5    
##  [7] formatR_0.10       gdata_2.13.3       grid_3.1.0        
## [10] gtable_0.1.2       gtools_3.4.0       htmltools_0.2.4   
## [13] KernSmooth_2.23-12 labeling_0.2       magrittr_1.0.1    
## [16] markdown_0.7       MASS_7.3-31        mime_0.1.1        
## [19] munsell_0.4.2      parallel_3.1.0     plyr_1.8.1        
## [22] proto_0.3-10       Rcpp_0.11.1        reshape2_1.4      
## [25] scales_0.2.4       stringr_0.6.2      tools_3.1.0       
## [28] yaml_2.1.11
```
