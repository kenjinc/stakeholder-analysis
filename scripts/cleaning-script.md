Cleaning Script
================
Last Updated: February 19, 2025

## Package Loading

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(ggpubr)
library(ggsignif)
library(maps)
```

    ## 
    ## Attaching package: 'maps'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     map

``` r
library(sf)
```

    ## Linking to GEOS 3.11.0, GDAL 3.5.3, PROJ 9.1.0; sf_use_s2() is TRUE

``` r
library(rnaturalearth)
library(rnaturalearthhires)
```

## Data Loading

``` r
survey_data <- read.csv("/Users/kenjinchang/github/stakeholder-analysis/data/parent-files/survey-data.csv")
extraction_data <- read.csv("/Users/kenjinchang/github/stakeholder-analysis/data/parent-files/extraction-data.csv")
```

## Data Cleaning: Scoping Review

- `study_id`: the unique numeral identifier assigned to each
  independently eligible study included within our scoping review
  (1-116).
- `study`: the parenthetical, in-text citation for each independently
  eligible study’s corresponding record of reference.
- `study_no`: the assigned number specifying the order in which the
  independently eligible study appears within the corresponding record
  of reference.
- `study_count`: the total number of independently eligible studies
  appearing within the corresponding record of reference.
- `doi`: the handle, or digital object identifier (DOI), assigned to
  each independently eligible study’s corresponding record of reference.
- `pub_year`: the calendar year when each independently eligible study’s
  corresponding record of reference was published (2001-2024).
- `pub_title`: the given title for each independently eligible study’s
  corresponding record of reference.
- `pub_journal`: the publishing journal of each independently eligible
  study’s record of reference.
- `university_list`: the complete list of treatment-receiving
  universities participating in the independently eligible study.
- `university_count`: the total number of treatment-receiving
  universities participating in the independently eligible study.
- `investigation_cat`: the scale of the independently eligible study’s
  investigation (single-site for study’s localized to one
  treatment-receiving university or multi-site for study’s implemented
  across multiple treatment-receiving universities).
- `nation_list`: a duplicate-allowing list pairing the participating
  treatment-receiving universities mentioned in each study with the ISO
  (International Organization for Standardization) countries in which
  they are located.
- `nation_list_count`: the total number of unique and duplicate ISO
  countries associated with the participating treatment-receiving
  universities mentioned in each study.
- `nation_var`: a non-duplicate-allowing list pairing the participating
  treatment-receiving universities mentioned in each study with the ISO
  countries in which they are located.
- `nation_var_count`: the total number of unique ISO countries
  associated with the participating treatment-receiving universities
  mentioned in each study.
- `us_state_list`: a conditional, duplicate-allowing list pairing the
  participating treatment-receiving universities mentioned in each study
  with the U.S. state in which they are located.
- `us_state_list_count`: the total number of unique and duplicate U.S.
  states associated with the participating treatment-receiving
  universities mentioned in each study.
- `us_state_var`: a conditional, non-duplicate-allowing list pairing the
  participating treatment-receiving universities mentioned in each study
  with the U.S. states in which they are located.
- `us_state_var_count`: the total number of unique U.S. states
  associated with the participating treatment-receiving universities
  mentioned in each study.
- `uk_country_list`: a conditional, duplicate-allowing list pairing the
  participating treatment-receiving universities mentioned in each study
  with the U.K. country in which they are located.
- `uk_country_list_count`: the total number of unique and duplicate U.K.
  countries associated with the participating treatment-receiving
  universities mentioned each study.
- `uk_country_var`: a conditional, non-duplicate allowing list pairing
  the participating treatment-receiving universities mentioned in each
  study with the U.K. countries in which they are located.
- `uk_country_var_count`: the total number of unique U.K. countries
  associated with the participating treatment-receiving universities
  mentioned in each study.
- `strategy_list`: a duplicate-allowing list pairing the reported
  intervention components mentioned in each study with the specific
  behavioral strategies being leveraged.
- `approach_cat`: the diagnosed behavioral approach associated with each
  study, based on the strategies mapping on to each study’s reported
  intervention components.
- `target_list`: a duplicate-allowing list pairing the reported
  intervention components mentioned in each study with the specific
  socio-ecological tiers being targeted.
- `breadth_cat`: the diagnosed breadth of change associated each study,
  based on the socio-ecological tiers mapping on to each study’s
  reported intervention components.
- `design`: the research design of the included study, as determined by
  the nature of comparison being made (between, within, or between and
  within).
- `principal_indicator_list`: a duplicate-allowing list outlining the
  primary outcome measures being used to document changes to food
  selection or food service in response to the study’s reported
  intervention.
- `principal_indicator_list_count`: the total number of unique and
  duplicate primary outcome measures being
- `principal_indicator_var`:
- `principal_indicator_var_count`:
- `accessory_indicator_list`:
- `accessory_indicator_list_count`:
- `accessory_indicator_var`:
- `accessory_indicator_var_count`:
- `qualifying_indicator_list`:
- `qualifying_indicator_list_count`:
- `qualifying_indicator_var`:
- `qualifying_indicator_var_count`:
- `spillover_monitoring_list`:
- `spillover_monitoring_list_count`:
- `spillover_monitoring_var`:
- `spillover_monitoring_var_count`:
- `intention_behavior_monitoring`:

### Time

``` r
temporal_frequencies <- extraction_data %>%
  select(pub_year,university_count) %>%
  drop_na() %>%
  mutate(count=case_when(university_count>=1~1)) %>%
  group_by(pub_year) %>%
  summarise(frequency=sum(count)) %>%
  mutate(cumulative_frequency=cumsum(frequency)) %>%
  add_row(pub_year=2000,frequency=0) %>%
  add_row(pub_year=2002,frequency=0) %>%
  add_row(pub_year=2003,frequency=0) %>%
  add_row(pub_year=2004,frequency=0) %>%
  add_row(pub_year=2007,frequency=0) %>%
  add_row(pub_year=2008,frequency=0) %>%
  add_row(pub_year=2010,frequency=0) %>%
  arrange(pub_year) %>%
  mutate(cumulative_frequency=cumsum(frequency)) 
temporal_frequencies
```

    ## # A tibble: 25 × 3
    ##    pub_year frequency cumulative_frequency
    ##       <dbl>     <dbl>                <dbl>
    ##  1     2000         0                    0
    ##  2     2001         2                    2
    ##  3     2002         0                    2
    ##  4     2003         0                    2
    ##  5     2004         0                    2
    ##  6     2005         1                    3
    ##  7     2006         1                    4
    ##  8     2007         0                    4
    ##  9     2008         0                    4
    ## 10     2009         3                    7
    ## # ℹ 15 more rows

``` r
exponential_fit <- temporal_frequencies %>%
  slice(-1)
exponential_model <- lm(log(cumulative_frequency)~pub_year,data=exponential_fit)
summary(exponential_model)
```

    ## 
    ## Call:
    ## lm(formula = log(cumulative_frequency) ~ pub_year, data = exponential_fit)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.37237 -0.09186  0.03052  0.09792  0.36681 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -409.1142    10.4655  -39.09   <2e-16 ***
    ## pub_year       0.2046     0.0052   39.35   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1763 on 22 degrees of freedom
    ## Multiple R-squared:  0.986,  Adjusted R-squared:  0.9854 
    ## F-statistic:  1548 on 1 and 22 DF,  p-value: < 2.2e-16

``` r
temporal_frequencies_plot <- temporal_frequencies %>%
  ggplot(aes(x=pub_year,y=frequency,color=frequency,fill=frequency)) + 
  geom_col() +
  scale_fill_gradient(name="Count",low="lavender",high="lightslateblue",limits=c(1,116),na.value="lavender",breaks=c(1,29,58,87,116)) +
  scale_color_gradient(name="Count",low="lavender",high="lightslateblue",limits=c(1,116),na.value="lavender",breaks=c(1,29,58,87,116)) +
  xlab("Publication Year") + 
  ylab("Frequency") + 
  scale_y_continuous(limits=c(0,120)) +
  scale_x_continuous(breaks=c(2000,2004,2008,2012,2016,2020,2024),limits=c(2000,2025)) +
  labs(caption="   ") + 
  theme(legend.position="none",legend.justification="center",legend.box.spacing=unit(0,"pt"),legend.key.size=unit(10,"pt"),panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10))
temporal_cumulative_frequencies_plot <- temporal_frequencies %>%
  ggplot(aes(x=pub_year,y=cumulative_frequency,color=cumulative_frequency,fill=cumulative_frequency)) +
  scale_fill_gradient(name="Count",low="lavender",high="lightslateblue",limits=c(1,116),na.value="lavender",breaks=c(1,29,58,87,116)) +
  scale_color_gradient(name="Count",low="lavender",high="lightslateblue",limits=c(1,116),na.value="lavender",breaks=c(1,29,58,87,116)) +
  geom_col() + 
  xlab("Publication Year") + 
  ylab("Cumulative Frequency") + 
  scale_y_continuous(limits=c(0,120)) +
  scale_x_continuous(breaks=c(2000,2004,2008,2012,2016,2020,2024),limits=c(2000,2025)) +
  labs(caption="Adjusted R-Squared: 0.985 (3sf); p-value < 0.001") + 
  theme(legend.title.position="none",legend.position="none",legend.justification="center",legend.box.spacing=unit(0,"pt"),legend.key.width=unit(50,"pt"),legend.key.height=unit(7.5,"pt"),panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10))
```

``` r
temporal_plots <- ggarrange(temporal_frequencies_plot,temporal_cumulative_frequencies_plot,
          nrow=2,
          labels=c("A","B"))
```

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_col()`).
    ## Removed 1 row containing missing values or values outside the scale range
    ## (`geom_col()`).

``` r
ggsave(filename="publication-rate.png",plot=temporal_plots,path="/Users/kenjinchang/github/stakeholder-analysis/figures",width=40,height=30,units="cm",dpi=150,limitsize=TRUE)
temporal_plots
```

![](cleaning-script_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### Place

``` r
global_shapefile <- map_data("world")
global_shapefile <- global_shapefile %>% 
  rename(country=region) %>%
  mutate(country=case_when(country=="Macedonia"~"North Macedonia",
                           country=="Ivory Coast"~"Cote d'Ivoire",
                           country=="Democratic Republic of the Congo"~"Congo, Dem. Rep.",
                           country=="Republic of Congo"~"Congo, Rep.",
                           country=="UK"~"United Kingdom",
                           country=="USA"~"United States",
                           country=="Laos"~"Lao",
                           country=="Slovakia"~"Slovak Republic",
                           country=="Saint Lucia"~"St. Lucia",
                           country=="Kyrgyzstan"~"Krygyz Republic",
                           country=="Micronesia"~"Micronesia, Fed. Sts.",
                           country=="Swaziland"~"Eswatini",
                           country=="Virgin Islands"~"Virgin Islands (U.S.)",
                        TRUE~country))
island_nations <- c("Antigua","Barbuda","Nevis", 
                 "Saint Kitts","Trinidad",
                 "Tobago","Grenadines","Saint Vincent")
island_nations_match <- global_shapefile %>% 
  filter(country %in% island_nations)
ant_bar <- c(137,138 )
kit_nev <- c(930,931)
tri_tog <- c(1425,1426)
vin_gre <- c(1575,1576,1577)
island_nation_names <- c("Antigua and Barbuda","St. Kitts and Nevis","Trinidad and Tobago","St. Vincent and the Grenadines")
island_nations_match <- island_nations_match %>% 
  mutate(country=case_when(group %in% ant_bar~"Antigua and Barbuda",
                           group %in% kit_nev~"St. Kitts and Nevis",
                           group %in% tri_tog~"Trinidad and Tobago",
                           group %in% vin_gre~"St. Vincent and the Grenadines")) %>% 
  tibble()
global_shapefile <- global_shapefile %>%
  filter(!country %in% island_nation_names)
global_shapefile <- global_shapefile %>% 
  bind_rows(island_nations_match) %>%
  arrange(country) %>%
  tibble()
sra_names <- c("Hong Kong","Macao")
hk_mc <- global_shapefile %>% 
  filter(subregion %in% sra_names)
hk_mc <- hk_mc %>%
  mutate(country = case_when(subregion=="Hong Kong"~"Hong Kong, China",
                             subregion=="Macao"~"Macao, China"))
global_shapefile <- global_shapefile %>%
  filter(!subregion %in% sra_names)
global_shapefile <- global_shapefile %>% 
  bind_rows(hk_mc) %>%
  select(-subregion) %>% 
  tibble()
```

``` r
spatial_frequencies <- extraction_data %>%
  select(nation_var,nation_var_count) %>%
  rename(country=nation_var) %>%
  mutate(across(country,str_replace,"USA","United States")) %>%
  mutate(across(country,str_replace,"UK","United Kingdom")) %>%
  group_by(country) %>%
  summarise(frequency=sum(nation_var_count)) %>%
  arrange(desc(frequency))
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `across(country, str_replace, "USA", "United States")`.
    ## Caused by warning:
    ## ! The `...` argument of `across()` is deprecated as of dplyr 1.1.0.
    ## Supply arguments directly to `.fns` through an anonymous function instead.
    ## 
    ##   # Previously
    ##   across(a:b, mean, na.rm = TRUE)
    ## 
    ##   # Now
    ##   across(a:b, \(x) mean(x, na.rm = TRUE))

``` r
spatial_frequencies
```

    ## # A tibble: 17 × 2
    ##    country          frequency
    ##    <chr>                <int>
    ##  1 "United States"         55
    ##  2 "United Kingdom"        16
    ##  3 "Canada"                13
    ##  4 "Italy"                  6
    ##  5 "Germany"                4
    ##  6 "China"                  3
    ##  7 "Portugal"               3
    ##  8 "Sweden"                 3
    ##  9 "Australia"              2
    ## 10 "Belgium "               2
    ## 11 "Brazil"                 2
    ## 12 "Netherlands"            2
    ## 13 "France"                 1
    ## 14 "India"                  1
    ## 15 "Norway"                 1
    ## 16 "Switzerland"            1
    ## 17 "Thailand"               1

``` r
spatial_frequencies %>%
  summarise(sum(frequency))
```

    ## # A tibble: 1 × 1
    ##   `sum(frequency)`
    ##              <int>
    ## 1              116

scale_fill_gradient(low=‘white’, high=‘grey20’, limits=c(1, 10))

scale_fill_gradient(low=“lavender”,high=“slateblue4”,na.value=“white”,name=“Intervention-Receiving
Institutions”,guide=guide_colourbar(reverse=FALSE,title.position=“top”,title.hjust=0.5,limits=c(1,116)))
+

``` r
crs_robin <- "+proj=robin +lat_0=0 +lon_0=0 +x0=0 +y0=0"
```

``` r
spatial_frequencies_joined <- left_join(global_shapefile,spatial_frequencies,by="country")
national_frequencies <- spatial_frequencies_joined %>%
  ggplot(aes(x=long,y=lat,fill=frequency,group=group)) + 
  geom_polygon(color="black",linewidth=0.125) +
  scale_fill_gradient(low="lavender",high="lightslateblue",na.value="white",limits=c(0,55),breaks=c(0,11,22,33,44,55)) +
  coord_sf(crs=crs_robin) +
  xlab("") + 
  ylab("") +
  scale_y_continuous(limits=c(-55,85)) +
  labs(fill="Frequency") +
  theme(aspect.ratio=.48,legend.position="bottom",legend.title.position="top",legend.box.spacing=unit(-10,"pt"),legend.key.width=unit(100,"pt"),legend.key.height=unit(5,"pt"),panel.grid=element_blank(),panel.background=element_rect(fill="aliceblue"),panel.border=element_rect(fill=NA),axis.text=element_blank(),axis.ticks=element_blank(),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10))
```

``` r
usa_shapefile <- map_data("county") %>%
  rename(state=region)
```

``` r
state_frequencies <- extraction_data %>%
  select(study,us_state_var) %>%
  separate_longer_delim(c(us_state_var),delim=";") %>%
  add_column(count=1) %>%
  group_by(us_state_var) %>%
  summarise(frequency=sum(count)) %>%
  arrange(desc(frequency)) %>%
  mutate(us_state_var=tolower(us_state_var)) %>%
  rename(state=us_state_var) %>%
  drop_na()
state_frequencies 
```

    ## # A tibble: 38 × 2
    ##    state            frequency
    ##    <chr>                <dbl>
    ##  1 "california"            15
    ##  2 "illinois"               4
    ##  3 "michigan"               3
    ##  4 "new york"               3
    ##  5 "pennsylvania"           3
    ##  6 "utah"                   3
    ##  7 " massachusetts"         2
    ##  8 "arkansas"               2
    ##  9 "florida"                2
    ## 10 "indiana"                2
    ## # ℹ 28 more rows

``` r
state_frequencies %>% summarise(sum(frequency))
```

    ## # A tibble: 1 × 1
    ##   `sum(frequency)`
    ##              <dbl>
    ## 1               69

``` r
state_frequencies_joined <- left_join(usa_shapefile,state_frequencies,by="state")
```

``` r
usa_frequencies <- state_frequencies_joined %>%
  ggplot(aes(x=long,y=lat,fill=frequency,group=group)) + 
  geom_polygon(color="black",linewidth=0.125) +
  scale_fill_gradient(low="lavender",high="lightslateblue",na.value="white",limits=c(1,55)) +
  xlab("") + 
  ylab("") +
  labs(caption="") +
  coord_map("albers",lat0=45.5,lat1=29.5) +
  theme(legend.key.width=unit(3,"lines"),legend.position="none",legend.justification="center",legend.box.spacing=unit(-15,"pt"),legend.key.size=unit(10,"pt"),panel.grid=element_blank(),panel.background=element_rect(fill="aliceblue"),panel.border=element_rect(fill=NA),axis.text=element_blank(),axis.ticks=element_blank(),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10))
```

``` r
subnational_frequencies <- extraction_data %>%
  select(uk_country_var,uk_country_var_count) %>%
  rename(subregion=uk_country_var) %>%
  separate_longer_delim(c(subregion,uk_country_var_count),delim=";") %>%
  mutate(uk_country_var_count=as.numeric(uk_country_var_count)) %>%
  group_by(subregion) %>%
  summarise(frequency=sum(uk_country_var_count)) %>%
  arrange(desc(frequency)) %>%
  mutate(across("subregion",str_replace,"England","Great Britain")) %>%
  drop_na()
subnational_frequencies
```

    ## # A tibble: 3 × 2
    ##   subregion     frequency
    ##   <chr>             <dbl>
    ## 1 Great Britain        14
    ## 2 Scotland              1
    ## 3 Wales                 1

``` r
uk_shapefile <- ne_states(country="united kingdom",returnclass="sf")
```

``` r
subnational_frequencies <- subnational_frequencies %>%
  mutate(across('subregion',str_replace,"Great Britain","England")) %>%
  rename(geonunit=subregion)
subnational_frequencies
```

    ## # A tibble: 3 × 2
    ##   geonunit frequency
    ##   <chr>        <dbl>
    ## 1 England         14
    ## 2 Scotland         1
    ## 3 Wales            1

``` r
subnational_frequencies_joined <- left_join(uk_shapefile,subnational_frequencies,by="geonunit")
```

``` r
uk_frequencies <- subnational_frequencies_joined %>%
ggplot(aes(fill=frequency,group=geonunit)) + 
  geom_sf(aes(fill=frequency),color="black",linewidth=0.125) +
  scale_fill_gradient(low="lavender",high="lightslateblue",na.value="white",limits=c(1,55)) +
  theme(legend.key.width=unit(3,"lines"),legend.position="none",legend.justification="center",legend.box.spacing=unit(-15,"pt"),legend.key.size=unit(10,"pt"),panel.grid=element_blank(),panel.background=element_rect(fill="aliceblue"),panel.border=element_rect(fill=NA),axis.text=element_blank(),axis.ticks=element_blank(),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10))
```

``` r
subnational_frequency_choros <- ggarrange(usa_frequencies,uk_frequencies,
                                         ncol=2,
                                         labels=c("B","C"))
national_frequency_choros <- ggarrange(national_frequencies,
                                       labels="A")
```

``` r
spatial_plots <- ggarrange(national_frequency_choros,subnational_frequency_choros,
          nrow=2)
ggsave(filename="publication-distribution.png",plot=spatial_plots,path="/Users/kenjinchang/github/stakeholder-analysis/figures",width=40,height=40,units="cm",dpi=150,limitsize=TRUE)
spatial_plots
```

![](cleaning-script_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

### Primary Performance Indicators

``` r
ppi_frequencies <- extraction_data %>%
  select(study,principal_indicator_var) %>%
  separate_longer_delim(c(principal_indicator_var),delim=";") %>%
  add_column(count=1) %>%
  group_by(principal_indicator_var) %>%
  summarise(frequency=sum(count)) %>%
  mutate(across(principal_indicator_var,str_replace_all,"Observed Food Choice","Food Choice, Observed")) %>%
  mutate(across(principal_indicator_var,str_replace_all,"Observed Food Service","Food Service, Observed")) %>%
  mutate(across(principal_indicator_var,str_replace_all,"Self-Reported Food Choice","Food Choice, Self-Reported")) %>%
  mutate(across(principal_indicator_var,str_replace_all,"Intended Food Choice","Food Choice, Intended")) 
ppi_frequencies 
```

    ## # A tibble: 8 × 2
    ##   principal_indicator_var       frequency
    ##   <chr>                             <dbl>
    ## 1 "  Food Choice, Observed"             1
    ## 2 " Food Choice, Observed"             11
    ## 3 " Food Service, Observed"             1
    ## 4 " Food Choice, Self-Reported"        30
    ## 5 "Food Choice, Intended"              36
    ## 6 "Food Choice, Observed"              42
    ## 7 "Food Service, Observed"              1
    ## 8 "Food Choice, Self-Reported"         37

``` r
principal_indicator_var_manual <- c("Food Choice, Intended","Food Choice, Self-Reported","Food Choice, Observed","Food Service, Observed")
frequency_manual <- c(36,67,54,2)
ppi_frequencies_manual <- tibble(principal_indicator_var_manual,frequency_manual) %>%
  arrange(desc(frequency_manual))
ppi_frequencies_manual 
```

    ## # A tibble: 4 × 2
    ##   principal_indicator_var_manual frequency_manual
    ##   <chr>                                     <dbl>
    ## 1 Food Choice, Self-Reported                   67
    ## 2 Food Choice, Observed                        54
    ## 3 Food Choice, Intended                        36
    ## 4 Food Service, Observed                        2

``` r
ppi_contingency_table <- ppi_frequencies_manual %>%
  rename(Reported=frequency_manual) %>%
  mutate(Omitted=116-Reported)
ppi_contingency_table
```

    ## # A tibble: 4 × 3
    ##   principal_indicator_var_manual Reported Omitted
    ##   <chr>                             <dbl>   <dbl>
    ## 1 Food Choice, Self-Reported           67      49
    ## 2 Food Choice, Observed                54      62
    ## 3 Food Choice, Intended                36      80
    ## 4 Food Service, Observed                2     114

``` r
ppi_contigency_table <- as.table(rbind(c(67,54,36,2),c(49,62,80,114)))
dimnames(ppi_contigency_table) <- list(dichotomy=c("Reported","Omitted"),
                                       indicators=c("Food Choice, Self-Reported","Food Choice, Observed","Food Choice, Intended","Food Service, Observed"))
ppi_chisq_test <- chisq.test(ppi_contigency_table)
ppi_chisq_test
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  ppi_contigency_table
    ## X-squared = 91.269, df = 3, p-value < 2.2e-16

Significant associated between the type of principal performance
indicator and its likelihood of appearance

``` r
ppi_observed_counts <- ppi_chisq_test$observed
print(ppi_chisq_test)
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  ppi_contigency_table
    ## X-squared = 91.269, df = 3, p-value < 2.2e-16

``` r
ppi_expected_counts <- ppi_chisq_test$expected
print(round(ppi_expected_counts,2))
```

    ##           indicators
    ## dichotomy  Food Choice, Self-Reported Food Choice, Observed
    ##   Reported                      39.75                 39.75
    ##   Omitted                       76.25                 76.25
    ##           indicators
    ## dichotomy  Food Choice, Intended Food Service, Observed
    ##   Reported                 39.75                  39.75
    ##   Omitted                  76.25                  76.25

``` r
ppi_pearson_residuals <- ppi_chisq_test$residuals
print(round(ppi_pearson_residuals,2))
```

    ##           indicators
    ## dichotomy  Food Choice, Self-Reported Food Choice, Observed
    ##   Reported                       4.32                  2.26
    ##   Omitted                       -3.12                 -1.63
    ##           indicators
    ## dichotomy  Food Choice, Intended Food Service, Observed
    ##   Reported                 -0.59                  -5.99
    ##   Omitted                   0.43                   4.32

``` r
ppi_contributions <- (ppi_observed_counts-ppi_expected_counts)^2/ppi_expected_counts
ppi_total_chi_square <- ppi_chisq_test$statistic
ppi_percentage_contributions <- 100*ppi_contributions/ppi_total_chi_square
print("Percentage Contributions:")
```

    ## [1] "Percentage Contributions:"

``` r
print(round(ppi_percentage_contributions,2))
```

    ##           indicators
    ## dichotomy  Food Choice, Self-Reported Food Choice, Observed
    ##   Reported                      20.47                  5.60
    ##   Omitted                       10.67                  2.92
    ##           indicators
    ## dichotomy  Food Choice, Intended Food Service, Observed
    ##   Reported                  0.39                  39.28
    ##   Omitted                   0.20                  20.48

``` r
library(pheatmap)
pheatmap(ppi_percentage_contributions,display_numbers=TRUE,cluster_rows=FALSE,cluster_cols=FALSE,main="Percentage Contributions to Chi-Square Statistic")
```

![](cleaning-script_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

``` r
ppi_contingency_table_alt <- ppi_contingency_table %>%
  pivot_longer(!principal_indicator_var_manual,names_to="appearance",values_to="frequency_manual") %>%
  mutate(contribution=case_when(principal_indicator_var_manual=="Food Choice, Self-Reported"&appearance=="Reported"~20.47,
    principal_indicator_var_manual=="Food Choice, Self-Reported"&appearance=="Omitted"~10.67,
    principal_indicator_var_manual=="Food Choice, Observed"&appearance=="Reported"~5.60,
    principal_indicator_var_manual=="Food Choice, Observed"&appearance=="Omitted"~2.92,
    principal_indicator_var_manual=="Food Choice, Intended"&appearance=="Reported"~0.39,
    principal_indicator_var_manual=="Food Choice, Intended"&appearance=="Omitted"~0.20,
    principal_indicator_var_manual=="Food Service, Observed"&appearance=="Reported"~39.28,
    principal_indicator_var_manual=="Food Service, Observed"&appearance=="Omitted"~20.48)) %>%
  mutate(label_y=case_when(appearance=="Reported"~118,
                           appearance=="Omitted"~-2))
ppi_contingency_table_alt
```

    ## # A tibble: 8 × 5
    ##   principal_indicator_var_man…¹ appearance frequency_manual contribution label_y
    ##   <chr>                         <chr>                 <dbl>        <dbl>   <dbl>
    ## 1 Food Choice, Self-Reported    Reported                 67        20.5      118
    ## 2 Food Choice, Self-Reported    Omitted                  49        10.7       -2
    ## 3 Food Choice, Observed         Reported                 54         5.6      118
    ## 4 Food Choice, Observed         Omitted                  62         2.92      -2
    ## 5 Food Choice, Intended         Reported                 36         0.39     118
    ## 6 Food Choice, Intended         Omitted                  80         0.2       -2
    ## 7 Food Service, Observed        Reported                  2        39.3      118
    ## 8 Food Service, Observed        Omitted                 114        20.5       -2
    ## # ℹ abbreviated name: ¹​principal_indicator_var_manual

``` r
ppi_frequencies_plot <- ppi_contingency_table_alt %>%
  ggplot(aes(x=principal_indicator_var_manual,y=frequency_manual,fill=appearance,color=appearance)) + 
  geom_col() +
  geom_text(aes(y=label_y,label=paste(format(contribution,nsmall=2),"%")),color="black",size=3) +
  geom_signif(comparisons=list(c("Food Choice, Self-Reported","Food Service, Observed")),color="black",size=0.25,annotation="***",y_position=116,tip_length=0.02,vjust=0) +
  xlab("Principal Indicator") +
  ylab("Frequency") +
  scale_x_discrete(limits=c("Food Service, Observed","Food Choice, Intended","Food Choice, Observed","Food Choice, Self-Reported")) + 
  scale_y_continuous(breaks=c(0,29,58,87,116)) +
  scale_fill_manual(values=c("lavender","lightslateblue")) +
  scale_color_manual(values=c("lavender","lightslateblue")) +
  guides(fill=guide_legend(title="Mode"),color=guide_legend(title="Mode")) +
  labs(caption="X-squared: 91.3 (3sf); p-value < 0.001") + 
  theme(aspect.ratio=0.8,legend.position="right",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10))
```

### Accessory Performance Indicators

``` r
api_frequencies <- extraction_data %>%
  select(study,accessory_indicator_var) %>%
  separate_longer_delim(c(accessory_indicator_var),delim=";") %>%
  add_column(count=1) %>%
  group_by(accessory_indicator_var) %>%
  summarise(frequency=sum(count))
api_frequencies
```

    ## # A tibble: 15 × 2
    ##    accessory_indicator_var                 frequency
    ##    <chr>                                       <dbl>
    ##  1 " Campus Culture"                               2
    ##  2 " Dietary Health"                              18
    ##  3 " Food Pricing"                                 1
    ##  4 " Guest Dining Experience"                      4
    ##  5 " Institutional Sustainability"                 2
    ##  6 " Operating Costs"                             12
    ##  7 " Staff Satisfaction"                           1
    ##  8 " Sustainability of Guest Food Choices"        10
    ##  9 "Campus Culture"                               80
    ## 10 "Dietary Health"                                9
    ## 11 "Food Pricing"                                  3
    ## 12 "Guest Dining Experience"                       1
    ## 13 "Operating Costs"                               2
    ## 14 "Sustainability of Guest Food Choices"          3
    ## 15  <NA>                                          18

``` r
accessory_indicator_var_manual <- c("Campus Culture","Dietary Health","Food Pricing","Guest Dining Experience","Institutional Sustainability","Operating Costs","Staff Satisfaction","Sustainability of Guest Food Choices")
frequency_manual <- c(82,27,4,5,2,14,1,13)
api_frequencies_manual <- tibble(accessory_indicator_var_manual,frequency_manual) %>%
  arrange(desc(frequency_manual))
api_frequencies_manual 
```

    ## # A tibble: 8 × 2
    ##   accessory_indicator_var_manual       frequency_manual
    ##   <chr>                                           <dbl>
    ## 1 Campus Culture                                     82
    ## 2 Dietary Health                                     27
    ## 3 Operating Costs                                    14
    ## 4 Sustainability of Guest Food Choices               13
    ## 5 Guest Dining Experience                             5
    ## 6 Food Pricing                                        4
    ## 7 Institutional Sustainability                        2
    ## 8 Staff Satisfaction                                  1

``` r
api_contigency_table <- api_frequencies_manual %>%
  rename(Reported=frequency_manual) %>%
  mutate(Omitted=116-Reported)
api_contigency_table
```

    ## # A tibble: 8 × 3
    ##   accessory_indicator_var_manual       Reported Omitted
    ##   <chr>                                   <dbl>   <dbl>
    ## 1 Campus Culture                             82      34
    ## 2 Dietary Health                             27      89
    ## 3 Operating Costs                            14     102
    ## 4 Sustainability of Guest Food Choices       13     103
    ## 5 Guest Dining Experience                     5     111
    ## 6 Food Pricing                                4     112
    ## 7 Institutional Sustainability                2     114
    ## 8 Staff Satisfaction                          1     115

``` r
api_contigency_table_alt <- api_contigency_table %>%
  pivot_longer(!accessory_indicator_var_manual,names_to="appearance",values_to="frequency_manual") %>%
  mutate(contribution=case_when(accessory_indicator_var_manual=="Campus Culture"&appearance=="Reported"~62.97,
    accessory_indicator_var_manual=="Campus Culture"&appearance=="Omitted"~13.71,
    accessory_indicator_var_manual=="Dietary Health"&appearance=="Reported"~0.66,
    accessory_indicator_var_manual=="Dietary Health"&appearance=="Omitted"~0.14,
    accessory_indicator_var_manual=="Operating Costs"&appearance=="Reported"~0.76,
    accessory_indicator_var_manual=="Operating Costs"&appearance=="Omitted"~0.17,
    accessory_indicator_var_manual=="Sustainability of Guest Food Choices"&appearance=="Reported"~1.00,
    accessory_indicator_var_manual=="Sustainability of Guest Food Choices"&appearance=="Omitted"~0.22,
    accessory_indicator_var_manual=="Guest Dining Experience"&appearance=="Reported"~4.15,
    accessory_indicator_var_manual=="Guest Dining Experience"&appearance=="Omitted"~0.90,
    accessory_indicator_var_manual=="Food Pricing"&appearance=="Reported"~0.16,
    accessory_indicator_var_manual=="Food Pricing"&appearance=="Omitted"~0.03,
    accessory_indicator_var_manual=="Institutional Sustainability"&appearance=="Reported"~5.89,
    accessory_indicator_var_manual=="Institutional Sustainability"&appearance=="Omitted"~1.28,
    accessory_indicator_var_manual=="Staff Satisfaction"&appearance=="Reported"~6.53,
    accessory_indicator_var_manual=="Staff Satisfaction"&appearance=="Omitted"~1.42)) %>%
  mutate(label_y=case_when(appearance=="Reported"~118,
                           appearance=="Omitted"~-2))
api_contigency_table_alt
```

    ## # A tibble: 16 × 5
    ##    accessory_indicator_var_ma…¹ appearance frequency_manual contribution label_y
    ##    <chr>                        <chr>                 <dbl>        <dbl>   <dbl>
    ##  1 Campus Culture               Reported                 82        63.0      118
    ##  2 Campus Culture               Omitted                  34        13.7       -2
    ##  3 Dietary Health               Reported                 27         0.66     118
    ##  4 Dietary Health               Omitted                  89         0.14      -2
    ##  5 Operating Costs              Reported                 14         0.76     118
    ##  6 Operating Costs              Omitted                 102         0.17      -2
    ##  7 Sustainability of Guest Foo… Reported                 13         1        118
    ##  8 Sustainability of Guest Foo… Omitted                 103         0.22      -2
    ##  9 Guest Dining Experience      Reported                  5         4.15     118
    ## 10 Guest Dining Experience      Omitted                 111         0.9       -2
    ## 11 Food Pricing                 Reported                  4         0.16     118
    ## 12 Food Pricing                 Omitted                 112         0.03      -2
    ## 13 Institutional Sustainability Reported                  2         5.89     118
    ## 14 Institutional Sustainability Omitted                 114         1.28      -2
    ## 15 Staff Satisfaction           Reported                  1         6.53     118
    ## 16 Staff Satisfaction           Omitted                 115         1.42      -2
    ## # ℹ abbreviated name: ¹​accessory_indicator_var_manual

``` r
api_frequencies_plot <- api_contigency_table_alt %>%
  ggplot(aes(x=accessory_indicator_var_manual,y=frequency_manual,fill=appearance,color=appearance)) + 
  geom_col() +
  geom_text(aes(y=label_y,label=paste(format(contribution,nsmall=2),"%")),color="black",size=3) +
  geom_signif(comparisons=list(c("Campus Culture","Staff Satisfaction")),color="black",size=0.25,annotation="***",y_position=116,tip_length=0.02,vjust=0) +
  xlab("Accessory Indicator") +
  ylab("Frequency") +
  scale_x_discrete(limits=c("Staff Satisfaction","Institutional Sustainability","Food Pricing","Guest Dining Experience","Sustainability of Guest Food Choices","Operating Costs","Dietary Health","Campus Culture")) + 
  scale_y_continuous(breaks=c(0,29,58,87,116)) +
  scale_fill_manual(values=c("aliceblue","lightsteelblue")) +
  scale_color_manual(values=c("aliceblue","lightsteelblue")) +
  guides(fill=guide_legend(title="Mode"),color=guide_legend(title="Mode")) +
  labs(caption="X-squared: 287 (3sf); p-value < 0.001") + 
  theme(aspect.ratio=0.4,legend.position="right",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10))
```

``` r
api_contigency_table <- as.table(rbind(c(82,27,14,13,5,4,2,1),c(34,89,102,103,111,12,114,115)))
dimnames(api_contigency_table) <- list(dichotomy=c("Reported","Omitted"),
                                       indicators=c("Campus Culture","Dietary Health","Operating Costs","Sustainability of Guest Food Choices","Guest Dining Experience","Food Pricing","Institutional Sustainability","Staff Satisfaction"))
api_chisq_test <- chisq.test(api_contigency_table)
```

    ## Warning in chisq.test(api_contigency_table): Chi-squared approximation may be
    ## incorrect

``` r
api_chisq_test
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  api_contigency_table
    ## X-squared = 287.48, df = 7, p-value < 2.2e-16

Significant association between the type of accessory performance
indicator and its likelihood of appearance (chi-square test
interpretation (<https://www.datacamp.com/tutorial/chi-square-test-r>))

``` r
api_observed_counts <- api_chisq_test$observed
print(api_chisq_test)
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  api_contigency_table
    ## X-squared = 287.48, df = 7, p-value < 2.2e-16

``` r
api_expected_counts <- api_chisq_test$expected
print(round(api_expected_counts,2))
```

    ##           indicators
    ## dichotomy  Campus Culture Dietary Health Operating Costs
    ##   Reported          20.73          20.73           20.73
    ##   Omitted           95.27          95.27           95.27
    ##           indicators
    ## dichotomy  Sustainability of Guest Food Choices Guest Dining Experience
    ##   Reported                                20.73                   20.73
    ##   Omitted                                 95.27                   95.27
    ##           indicators
    ## dichotomy  Food Pricing Institutional Sustainability Staff Satisfaction
    ##   Reported         2.86                        20.73              20.73
    ##   Omitted         13.14                        95.27              95.27

``` r
api_pearson_residuals <- api_chisq_test$residuals
print(round(api_pearson_residuals,2))
```

    ##           indicators
    ## dichotomy  Campus Culture Dietary Health Operating Costs
    ##   Reported          13.45           1.38           -1.48
    ##   Omitted           -6.28          -0.64            0.69
    ##           indicators
    ## dichotomy  Sustainability of Guest Food Choices Guest Dining Experience
    ##   Reported                                -1.70                   -3.46
    ##   Omitted                                  0.79                    1.61
    ##           indicators
    ## dichotomy  Food Pricing Institutional Sustainability Staff Satisfaction
    ##   Reported         0.67                        -4.11              -4.33
    ##   Omitted         -0.31                         1.92               2.02

``` r
api_contributions <- (api_observed_counts-api_expected_counts)^2/api_expected_counts
api_total_chi_square <- api_chisq_test$statistic
api_percentage_contributions <- 100*api_contributions/api_total_chi_square
print("Percentage Contributions:")
```

    ## [1] "Percentage Contributions:"

``` r
print(round(api_percentage_contributions,2))
```

    ##           indicators
    ## dichotomy  Campus Culture Dietary Health Operating Costs
    ##   Reported          62.97           0.66            0.76
    ##   Omitted           13.71           0.14            0.17
    ##           indicators
    ## dichotomy  Sustainability of Guest Food Choices Guest Dining Experience
    ##   Reported                                 1.00                    4.15
    ##   Omitted                                  0.22                    0.90
    ##           indicators
    ## dichotomy  Food Pricing Institutional Sustainability Staff Satisfaction
    ##   Reported         0.16                         5.89               6.53
    ##   Omitted          0.03                         1.28               1.42

``` r
library(pheatmap)
pheatmap(api_percentage_contributions,display_numbers=TRUE,cluster_rows=FALSE,cluster_cols=FALSE,main="Percentage Contributions to Chi-Square Statistic")
```

![](cleaning-script_files/figure-gfm/unnamed-chunk-46-1.png)<!-- -->

Rerun this using list class instead of variety class

list

``` r
extraction_data %>%
  select(accessory_indicator_list) %>%
  str_count("Campus Culture")
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 398

``` r
extraction_data %>%
  select(accessory_indicator_list) %>%
  str_count("Dietary Health")
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 241

``` r
extraction_data %>%
  select(accessory_indicator_list) %>%
  str_count("Operating Costs")
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 23

``` r
extraction_data %>%
  select(accessory_indicator_list) %>%
  str_count("Sustainability of Guest Food Choices")
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 41

``` r
extraction_data %>%
  select(accessory_indicator_list) %>%
  str_count("Guest Dining Experience")
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 7

``` r
extraction_data %>%
  select(accessory_indicator_list) %>%
  str_count("Food Pricing")
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 4

``` r
extraction_data %>%
  select(accessory_indicator_list) %>%
  str_count("Institutional Sustainability")
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 8

``` r
extraction_data %>%
  select(accessory_indicator_list) %>%
  str_count("Staff Satisfaction")
```

    ## Warning in stri_count_regex(string, pattern, opts_regex = opts(pattern)):
    ## argument is not an atomic vector; coercing

    ## [1] 1

### Performance Qualifiers

``` r
qpi_frequencies <- extraction_data %>%
  select(study,qualifying_indicator_var) %>%
  separate_longer_delim(c(qualifying_indicator_var),delim=";") %>%
  add_column(count=1) %>%
  group_by(qualifying_indicator_var) %>%
  summarise(frequency=sum(count))
qpi_frequencies
```

    ## # A tibble: 9 × 2
    ##   qualifying_indicator_var frequency
    ##   <chr>                        <dbl>
    ## 1 " Lifestyle"                    37
    ## 2 " Lifestyle "                    6
    ## 3 " Program Reception"            34
    ## 4 " Situational"                  21
    ## 5 "Demographics"                  87
    ## 6 "Lifestyle"                      3
    ## 7 "Program Reception"              5
    ## 8 "Situational"                    8
    ## 9  <NA>                           13

``` r
qualifying_indicator_var_manual <- c("Demographics","Lifestyle","Program Reception","Situational")
frequency_manual <- c(87,46,39,29)
qpi_frequencies_manual <- tibble(qualifying_indicator_var_manual,frequency_manual) %>%
  arrange(desc(frequency_manual))
qpi_frequencies_manual 
```

    ## # A tibble: 4 × 2
    ##   qualifying_indicator_var_manual frequency_manual
    ##   <chr>                                      <dbl>
    ## 1 Demographics                                  87
    ## 2 Lifestyle                                     46
    ## 3 Program Reception                             39
    ## 4 Situational                                   29

``` r
qpi_contingency_table <- qpi_frequencies_manual %>%
  rename(Reported=frequency_manual) %>%
  mutate(Omitted=116-Reported)
qpi_contingency_table
```

    ## # A tibble: 4 × 3
    ##   qualifying_indicator_var_manual Reported Omitted
    ##   <chr>                              <dbl>   <dbl>
    ## 1 Demographics                          87      29
    ## 2 Lifestyle                             46      70
    ## 3 Program Reception                     39      77
    ## 4 Situational                           29      87

``` r
qpi_contingency_table_alt <- qpi_contingency_table %>%
  pivot_longer(!qualifying_indicator_var_manual,names_to="appearance",values_to="frequency_manual") %>%
  mutate(contribution=case_when(qualifying_indicator_var_manual=="Demographics"&appearance=="Reported"~39.32,
    qualifying_indicator_var_manual=="Demographics"&appearance=="Omitted"~30.05,
    qualifying_indicator_var_manual=="Lifestyle"&appearance=="Reported"~0.53,
    qualifying_indicator_var_manual=="Lifestyle"&appearance=="Omitted"~0.40,
    qualifying_indicator_var_manual=="Program Reception"&appearance=="Reported"~3.68,
    qualifying_indicator_var_manual=="Program Reception"&appearance=="Omitted"~2.82,
    qualifying_indicator_var_manual=="Situational"&appearance=="Reported"~13.15,
    qualifying_indicator_var_manual=="Situational"&appearance=="Omitted"~10.05)) %>%
  mutate(label_y=case_when(appearance=="Reported"~118,
                           appearance=="Omitted"~-2))
api_contigency_table_alt
```

    ## # A tibble: 16 × 5
    ##    accessory_indicator_var_ma…¹ appearance frequency_manual contribution label_y
    ##    <chr>                        <chr>                 <dbl>        <dbl>   <dbl>
    ##  1 Campus Culture               Reported                 82        63.0      118
    ##  2 Campus Culture               Omitted                  34        13.7       -2
    ##  3 Dietary Health               Reported                 27         0.66     118
    ##  4 Dietary Health               Omitted                  89         0.14      -2
    ##  5 Operating Costs              Reported                 14         0.76     118
    ##  6 Operating Costs              Omitted                 102         0.17      -2
    ##  7 Sustainability of Guest Foo… Reported                 13         1        118
    ##  8 Sustainability of Guest Foo… Omitted                 103         0.22      -2
    ##  9 Guest Dining Experience      Reported                  5         4.15     118
    ## 10 Guest Dining Experience      Omitted                 111         0.9       -2
    ## 11 Food Pricing                 Reported                  4         0.16     118
    ## 12 Food Pricing                 Omitted                 112         0.03      -2
    ## 13 Institutional Sustainability Reported                  2         5.89     118
    ## 14 Institutional Sustainability Omitted                 114         1.28      -2
    ## 15 Staff Satisfaction           Reported                  1         6.53     118
    ## 16 Staff Satisfaction           Omitted                 115         1.42      -2
    ## # ℹ abbreviated name: ¹​accessory_indicator_var_manual

``` r
qpi_frequencies_plot <- qpi_contingency_table_alt %>%
  ggplot(aes(x=qualifying_indicator_var_manual,y=frequency_manual,fill=appearance,color=appearance)) + 
  geom_col() +
  geom_text(aes(y=label_y,label=paste(format(contribution,nsmall=2),"%")),color="black",size=3) +
  geom_signif(comparisons=list(c("Demographics","Situational")),color="black",size=0.25,annotation="***",y_position=116,tip_length=0.02,vjust=0) +
  xlab("Qualifying Indicator") +
  ylab("Frequency") +
  scale_x_discrete(limits=c("Situational","Program Reception","Lifestyle","Demographics")) + 
  scale_y_continuous(breaks=c(0,29,58,87,116)) +
  scale_fill_manual(values=c("mistyrose","lightcoral")) +
  scale_color_manual(values=c("mistyrose","lightcoral")) +
  guides(fill=guide_legend(title="Mode"),color=guide_legend(title="Mode")) +
  labs(caption="X-squared: 68.35 (2dp); p-value < 0.001") + 
  theme(aspect.ratio=0.8,legend.position="right",panel.grid=element_blank(),panel.background=element_rect(fill="white"),panel.border=element_rect(fill=NA),legend.title=element_text(size=10),legend.text=element_text(size=10),plot.title=element_text(size=10))
```

``` r
qpi_contingency_table <- as.table(rbind(c(87,46,39,29),c(29,70,77,87)))
dimnames(qpi_contingency_table) <- list(dichomoty=c("Reported","Omitted"),
                                        indicators=c("Demographics","Lifestyle","Program Reception","Situational"))
qpi_chisq_test <- chisq.test(qpi_contingency_table)
qpi_chisq_test
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  qpi_contingency_table
    ## X-squared = 68.35, df = 3, p-value = 9.629e-15

Significant association between the type of qualifying performance
indicator and its likelihood of appearance

``` r
qpi_observed_counts <- qpi_chisq_test$observed
print(qpi_chisq_test)
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  qpi_contingency_table
    ## X-squared = 68.35, df = 3, p-value = 9.629e-15

``` r
qpi_expected_counts <- qpi_chisq_test$expected
print(round(qpi_expected_counts,2))
```

    ##           indicators
    ## dichomoty  Demographics Lifestyle Program Reception Situational
    ##   Reported        50.25     50.25             50.25       50.25
    ##   Omitted         65.75     65.75             65.75       65.75

``` r
qpi_pearson_residuals <- qpi_chisq_test$residuals
print(round(qpi_pearson_residuals,2))
```

    ##           indicators
    ## dichomoty  Demographics Lifestyle Program Reception Situational
    ##   Reported         5.18     -0.60             -1.59       -3.00
    ##   Omitted         -4.53      0.52              1.39        2.62

``` r
qpi_contributions <- (qpi_observed_counts-qpi_expected_counts)^2/qpi_expected_counts
qpi_total_chi_square <- qpi_chisq_test$statistic
qpi_percentage_contributions <- 100*qpi_contributions/qpi_total_chi_square
print("Percentage Contributions:")
```

    ## [1] "Percentage Contributions:"

``` r
print(round(qpi_percentage_contributions,2))
```

    ##           indicators
    ## dichomoty  Demographics Lifestyle Program Reception Situational
    ##   Reported        39.32      0.53              3.68       13.15
    ##   Omitted         30.05      0.40              2.82       10.05

``` r
pheatmap(qpi_percentage_contributions,display_numbers=TRUE,cluster_rows=FALSE,cluster_cols=FALSE,main="Percentage Contributions to Chi-Square Statistic")
```

![](cleaning-script_files/figure-gfm/unnamed-chunk-66-1.png)<!-- -->

``` r
principal_qualifying_indicator_plots <- ggarrange(ppi_frequencies_plot,qpi_frequencies_plot,
          ncol=2,
          labels=c("A","C"))
accessory_indicator_plots <- ggarrange(api_frequencies_plot,
          labels="B")
```

``` r
combined_indicators <- ggarrange(principal_qualifying_indicator_plots,accessory_indicator_plots,
          nrow=2)
ggsave(filename="combined-performance-indicators.png",plot=combined_indicators,path="/Users/kenjinchang/github/stakeholder-analysis/figures",width=40,height=30,units="cm",dpi=150,limitsize=TRUE)
combined_indicators
```

![](cleaning-script_files/figure-gfm/unnamed-chunk-68-1.png)<!-- -->

### Gap Monitoring

#### Spillover

#### Intention-Behavior Asymmetries

## Data Cleaning: Stakeholder Analysis

- `date`: the
- `completion`: the
- `channel`: the
- `consent`: the
- `involvement`: the
- `involvement_other`: the
- `stakeholder_type`: the
- `stakeholder_type_other`: the
- `role_title`: the
- \`role_duration\`\`: the
- `dietary_health_ranking`: the
- `dietary_sustainability_ranking`: the
- `institutional_sustainability_ranking`: the
- `food_pricing_ranking`: the
- `operating_costs_ranking`: the
- `guest_experience_ranking`: the
- `staff_satisfaction_ranking`: the
- `campus_culture_ranking`: the
- `other_ranking`: the
- `other_ranking_other`: the

``` r
survey_data %>%
  select(RecordedDate,Progress,DistributionChannel,Q1,Q2,Q2_4_TEXT,Q6,Q6_10_TEXT,Q3,Q5_1,Q2_1,Q2_2,Q2_3,Q2_4,Q2_5,Q2_6,Q2_7,Q2_8,Q2_9,Q2_9_TEXT) %>%
  rename(date=RecordedDate,completion=Progress,mode=DistributionChannel,consent=Q1,role=Q2,role_other=Q2_4_TEXT,stakeholder_type=Q6,stakeholder_type_other=Q6_10_TEXT,position_title=Q3,position_duration=Q5_1,dietary_health_ranking=Q2_1,dietary_sustainability_ranking=Q2_2,institutional_sustainability_ranking=Q2_3,food_pricing_ranking=Q2_4,operating_costs_ranking=Q2_5,guest_experience_ranking=Q2_6,staff_satisfaction_ranking=Q2_7,campus_culture_ranking=Q2_8,other_ranking=Q2_9,other_ranking_other=Q2_9_TEXT) %>%
  slice(3:n()) %>%
  mutate(dietary_health_ranking=as.numeric(dietary_health_ranking)) %>%
  mutate(dietary_sustainability_ranking=as.numeric(dietary_sustainability_ranking)) %>%
  mutate(institutional_sustainability_ranking=as.numeric(institutional_sustainability_ranking)) %>%
  mutate(food_pricing_ranking=as.numeric(food_pricing_ranking)) %>%
  mutate(operating_costs_ranking=as.numeric(operating_costs_ranking)) %>%
  mutate(guest_experience_ranking=as.numeric(guest_experience_ranking)) %>%
  mutate(staff_satisfaction_ranking=as.numeric(staff_satisfaction_ranking)) %>%
  mutate(campus_culture_ranking=as.numeric(campus_culture_ranking)) %>%
  mutate(other_ranking=as.numeric(other_ranking)) %>%
  mutate(id=row_number()) 
```

    ##                   date completion mode               consent
    ## 1  2024-10-02 12:42:10        100   qr I wish to participate
    ## 2  2024-10-02 12:42:13        100   qr I wish to participate
    ## 3  2024-10-02 12:42:20        100   qr I wish to participate
    ## 4  2024-10-02 12:42:26        100   qr I wish to participate
    ## 5  2024-10-02 12:42:29        100   qr I wish to participate
    ## 6  2024-10-02 12:42:32        100   qr I wish to participate
    ## 7  2024-10-02 12:42:37        100   qr I wish to participate
    ## 8  2024-10-02 12:42:41        100   qr I wish to participate
    ## 9  2024-10-02 12:42:51        100   qr I wish to participate
    ## 10 2024-10-02 12:42:54        100   qr I wish to participate
    ## 11 2024-10-02 12:42:57        100   qr I wish to participate
    ## 12 2024-10-02 12:42:58        100   qr I wish to participate
    ## 13 2024-10-02 12:43:05        100   qr I wish to participate
    ## 14 2024-10-02 12:43:06        100   qr I wish to participate
    ## 15 2024-10-02 12:43:09        100   qr I wish to participate
    ## 16 2024-10-02 12:43:11        100   qr I wish to participate
    ## 17 2024-10-02 12:43:16        100   qr I wish to participate
    ## 18 2024-10-02 12:43:18        100   qr I wish to participate
    ## 19 2024-10-02 12:43:20        100   qr I wish to participate
    ## 20 2024-10-02 12:43:21        100   qr I wish to participate
    ## 21 2024-10-02 12:43:30        100   qr I wish to participate
    ## 22 2024-10-02 12:43:38        100   qr I wish to participate
    ## 23 2024-10-02 12:43:52        100   qr I wish to participate
    ## 24 2024-10-02 12:44:02        100   qr I wish to participate
    ## 25 2024-10-02 12:44:04        100   qr I wish to participate
    ## 26 2024-10-02 12:44:16        100   qr I wish to participate
    ## 27 2024-10-02 12:44:22        100   qr I wish to participate
    ## 28 2024-10-02 12:44:39        100   qr I wish to participate
    ## 29 2024-10-02 12:44:43        100   qr I wish to participate
    ## 30 2024-10-02 12:44:44        100   qr I wish to participate
    ## 31 2024-10-02 14:20:00        100   qr I wish to participate
    ## 32 2024-10-04 07:14:22        100   qr I wish to participate
    ##                                     role
    ## 1            I consult on best practices
    ## 2            I consult on best practices
    ## 3                Other (please specify):
    ## 4          I am a primary decision maker
    ## 5          I am a primary decision maker
    ## 6          I am a primary decision maker
    ## 7          I am a primary decision maker
    ## 8            I consult on best practices
    ## 9            I consult on best practices
    ## 10         I am a primary decision maker
    ## 11           I consult on best practices
    ## 12         I am a primary decision maker
    ## 13           I consult on best practices
    ## 14         I am a primary decision maker
    ## 15           I consult on best practices
    ## 16           I consult on best practices
    ## 17           I consult on best practices
    ## 18         I am a primary decision maker
    ## 19           I consult on best practices
    ## 20 I offer feedback on existing services
    ## 21         I am a primary decision maker
    ## 22         I am a primary decision maker
    ## 23           I consult on best practices
    ## 24           I consult on best practices
    ## 25 I offer feedback on existing services
    ## 26           I consult on best practices
    ## 27         I am a primary decision maker
    ## 28           I consult on best practices
    ## 29           I consult on best practices
    ## 30           I consult on best practices
    ## 31         I am a primary decision maker
    ## 32 I offer feedback on existing services
    ##                                                                          role_other
    ## 1                                                                                  
    ## 2                                                                                  
    ## 3  Collaboration and influence with corporate stakeholders on food loss and waste  
    ## 4                                                                                  
    ## 5                                                                                  
    ## 6                                                                                  
    ## 7                                                                                  
    ## 8                                                                                  
    ## 9                                                                                  
    ## 10                                                                                 
    ## 11                                                                                 
    ## 12                                                                                 
    ## 13                                                                                 
    ## 14                                                                                 
    ## 15                                                                                 
    ## 16                                                                                 
    ## 17                                                                                 
    ## 18                                                                                 
    ## 19                                                                                 
    ## 20                                                                                 
    ## 21                                                                                 
    ## 22                                                                                 
    ## 23                                                                                 
    ## 24                                                                                 
    ## 25                                                                                 
    ## 26                                                                                 
    ## 27                                                                                 
    ## 28                                                                                 
    ## 29                                                                                 
    ## 30                                                                                 
    ## 31                                                                                 
    ## 32                                                                                 
    ##              stakeholder_type         stakeholder_type_other
    ## 1        Nutrition specialist                               
    ## 2  Sustainability coordinator                               
    ## 3     Other (please specify):                   NGO employee
    ## 4             Dining director                               
    ## 5             Dining director                               
    ## 6                        Chef                               
    ## 7             Dining director                               
    ## 8  Sustainability coordinator                               
    ## 9        Nutrition specialist                               
    ## 10            Dining director                               
    ## 11    Other (please specify):                       Advisor 
    ## 12                       Chef                               
    ## 13       Nutrition specialist                               
    ## 14   University administrator                               
    ## 15 Sustainability coordinator                               
    ## 16                       Chef                               
    ## 17   University administrator                               
    ## 18   University administrator                               
    ## 19    Other (please specify):             Dining management 
    ## 20                       Chef                               
    ## 21            Dining director                               
    ## 22   University administrator                               
    ## 23       Nutrition specialist                               
    ## 24 Sustainability coordinator                               
    ## 25       Nutrition specialist                               
    ## 26 Sustainability coordinator                               
    ## 27 Sustainability coordinator                               
    ## 28            Dining director                               
    ## 29    Other (please specify): Manager of residential dining 
    ## 30    Other (please specify):              Dining marketing 
    ## 31            Dining director                               
    ## 32    Other (please specify):                      marketing
    ##                                                                 position_title
    ## 1                                                                             
    ## 2                                                      Sustainability Manager 
    ## 3                                                 Associate Program Specialist
    ## 4                                                                     Director
    ## 5                                                           Director of Dining
    ## 6                                                        Campus Executive Chef
    ## 7                                                           Director of Dining
    ## 8                                                     Sustainability Director 
    ## 9                                                                    Dietitian
    ## 10                                          Senior Director of Dining Services
    ## 11                                                                            
    ## 12 Senior associate Director culinary strategies and plant forward experience 
    ## 13                                                   Nutrition Systems Manager
    ## 14                                                   Assistant Vice President 
    ## 15                                                      Sustainability Manager
    ## 16                                                              Executive chef
    ## 17                                                                         AVP
    ## 18                                                  Associate Vice Chancellor 
    ## 19                              Director of Culinary development and nutrition
    ## 20                                                                 Sous Chef/s
    ## 21                                                  Executive Director dining 
    ## 22                                                          Asst. Vice Provost
    ## 23                                                                   Dietitian
    ## 24                                                                            
    ## 25                                                        Registered Dietitian
    ## 26                                                  Director of sustainability
    ## 27                                             Regional Sustainability Manager
    ## 28                                       Associate Director, Food and beverage
    ## 29                                                       Halls cluster manger 
    ## 30                                                          Marketing manager 
    ## 31                                                            Dining Director 
    ## 32                                                                            
    ##    position_duration dietary_health_ranking dietary_sustainability_ranking
    ## 1                  2                      2                              3
    ## 2                  1                      2                              3
    ## 3                  2                      3                              4
    ## 4                  1                      2                              5
    ## 5                 11                      8                              5
    ## 6                  3                      8                              3
    ## 7                  1                      1                              2
    ## 8          5.5 years                      6                              5
    ## 9                  3                      2                              5
    ## 10                32                      4                              2
    ## 11                 1                      7                              3
    ## 12                 3                      1                              6
    ## 13                12                      6                              7
    ## 14                 2                      1                              8
    ## 15                 3                      4                              5
    ## 16                 3                      6                              4
    ## 17          20 years                      3                              6
    ## 18                 4                      4                              8
    ## 19                 2                      1                              4
    ## 20                 1                      5                              6
    ## 21                 2                      5                              2
    ## 22                16                      7                              6
    ## 23                 3                      1                              2
    ## 24                .1                      6                              2
    ## 25                 3                      2                              5
    ## 26                10                      3                              5
    ## 27                 2                      6                              5
    ## 28                 5                      5                              8
    ## 29                 2                      3                              5
    ## 30                 3                      2                              4
    ## 31                 3                      5                              6
    ## 32                                        3                              6
    ##    institutional_sustainability_ranking food_pricing_ranking
    ## 1                                     7                    5
    ## 2                                     1                    8
    ## 3                                     8                    2
    ## 4                                     6                    4
    ## 5                                     7                    1
    ## 6                                     6                    2
    ## 7                                     3                    5
    ## 8                                     4                    8
    ## 9                                     7                    8
    ## 10                                    1                    7
    ## 11                                    8                    5
    ## 12                                    8                    4
    ## 13                                    5                    3
    ## 14                                    5                    6
    ## 15                                    1                    6
    ## 16                                    8                    2
    ## 17                                    8                    1
    ## 18                                    7                    3
    ## 19                                    8                    6
    ## 20                                    4                    8
    ## 21                                    3                    7
    ## 22                                    5                    4
    ## 23                                    6                    8
    ## 24                                    1                    5
    ## 25                                    6                    3
    ## 26                                    1                    6
    ## 27                                    4                    2
    ## 28                                    6                    1
    ## 29                                    4                    7
    ## 30                                    3                    6
    ## 31                                    7                    8
    ## 32                                    8                    2
    ##    operating_costs_ranking guest_experience_ranking staff_satisfaction_ranking
    ## 1                        4                        1                          6
    ## 2                        7                        4                          5
    ## 3                        5                        6                          7
    ## 4                        3                        1                          7
    ## 5                        3                        4                          2
    ## 6                        1                        4                          7
    ## 7                        8                        4                          6
    ## 8                        3                        2                          1
    ## 9                        4                        1                          6
    ## 10                       3                        5                          6
    ## 11                       2                        1                          6
    ## 12                       7                        5                          2
    ## 13                       2                        4                          8
    ## 14                       7                        2                          3
    ## 15                       2                        3                          8
    ## 16                       3                        1                          7
    ## 17                       7                        4                          2
    ## 18                       5                        2                          6
    ## 19                       5                        2                          3
    ## 20                       7                        1                          3
    ## 21                       6                        4                          1
    ## 22                       3                        1                          8
    ## 23                       4                        3                          7
    ## 24                       3                        8                          4
    ## 25                       4                        1                          8
    ## 26                       7                        4                          8
    ## 27                       1                        3                          8
    ## 28                       3                        2                          7
    ## 29                       1                        2                          8
    ## 30                       5                        1                          7
    ## 31                       3                        4                          2
    ## 32                       1                        4                          5
    ##    campus_culture_ranking other_ranking other_ranking_other id
    ## 1                       8             9                      1
    ## 2                       6             9                      2
    ## 3                       1             9                      3
    ## 4                       8             9                      4
    ## 5                       6             9                      5
    ## 6                       5             9                      6
    ## 7                       7             9                      7
    ## 8                       7             9                      8
    ## 9                       3             9                      9
    ## 10                      8             9                     10
    ## 11                      4             9                     11
    ## 12                      3             9                     12
    ## 13                      1             9                     13
    ## 14                      4             9                     14
    ## 15                      7             9                     15
    ## 16                      5             9                     16
    ## 17                      5             9                     17
    ## 18                      1             9                     18
    ## 19                      7             9                     19
    ## 20                      2             9                     20
    ## 21                      8             9                     21
    ## 22                      9             2        Cuisine type 22
    ## 23                      5             9                     23
    ## 24                      7             9                     24
    ## 25                      7             9                     25
    ## 26                      2             9                     26
    ## 27                      7             9                     27
    ## 28                      4             9                     28
    ## 29                      6             9                     29
    ## 30                      8             9                     30
    ## 31                      1             9                     31
    ## 32                      7             9                     32
