---
output:
  knitrBootstrap::bootstrap_document:
    title: "Bread for the World: Relationship between Women's Empowerment and Childhood Stunting"
    theme: flatly
---



Bread for the World Hackathon
=============================

Data preparation
----------------


```r
library(dplyr)
options(StringsAsFactors=FALSE)

# Load stunting data
stunting = tbl_df(read.csv('input/World Bank Stunting and Wasting Data, by gender.csv'))
                           
# Grab data for the last 15 years
stunting = stunting %>% select(Country.Name, Country.Code, Indicator.Code, X2000, X2001, 
                               X2002, X2003, X2004, X2005, X2006, X2007, X2008, X2009,
                               X2010, X2011, X2012, X2013)

# Mean stunting for last 15 years
values = stunting  %>% select(-Country.Name, -Country.Code, -Indicator.Code)
stunting$Most.Recent.Stunting = as.numeric(apply(values, 1, function(x) {
                                                 tail(x[!is.na(x)], 1) }))

# Most recent data point for each Country for which data is available during
# this time period
stunting$Mean.Stunting = apply(values, 1, function(x) { mean(x, na.rm=T) })

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
