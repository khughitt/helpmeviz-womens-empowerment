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

# Count number of non-zero values
values = stunting  %>% select(-Country.Name, -Country.Code, -Indicator.Code)
stunting$Mean.Stunting = apply(values, 1, function(x) { mean(x, na.rm=T) })

# Drop the individual year columns
stunting = stunting %>% select(Country.Name, Country.Code, Indicator.Code,
                               Mean.Stunting)

# Drop Countries with no recent data
stunting = stunting %>% filter(!is.nan(Mean.Stunting))

# Male vs. Female Stunting
male_stunting = stunting %>% filter(Indicator.Code == 'SH.STA.STNT.MA.ZS')
female_stunting = stunting %>% filter(Indicator.Code == 'SH.STA.STNT.FE.ZS')

# Save output
write.csv(male_stunting, file='output/male_stunting.csv', row.names=FALSE)
write.csv(female_stunting, file='output/female_stunting.csv', row.names=FALSE)
```
