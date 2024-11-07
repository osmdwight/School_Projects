1B ANNOVA FOR PLANTH GROWTH DATASET
================
Gonzales Dwight Jiro M.

## Load Necessary Libraries

``` r
library(tidyverse)                    
library(tidyr)                       
library(rstatix)                      
library(ggplot2)                      
library(ggpubr)                       
library(emmeans)                       
library(faraway) 
library(ggbeeswarm)
library(ggdist)
library(tidyquant)
library(ggthemes)

PlantGrowth %>%
  ggplot(aes(x = group, y = weight, fill = group)) +
  
  stat_halfeye(
    adjust = 0.5,               
    justification = -0.2,       
    .width = 0,                 
    point_colour = NA           
  ) +
  
  geom_boxplot(
    width = 0.12, 
    outlier.color = NA,         
    alpha = 0.5                 
  ) +
  
  stat_dots(
    side = "left",              
    justification = 1.1,        
    binwidth = 0.25,           
    size = 1.5                  
  ) +
  
  theme_minimal() +
  labs(title = "Raincloud Plot for Plant Growth by Group",
       x = "Group", 
       y = "Weight") +
  theme(legend.position = "none")  
```

![](FA8_GONZALES_1BANNOVA_files/figure-gfm/setup-1.png)<!-- -->

``` r
PlantGrowth %>%
  group_by(group) %>%
  summarize(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE),
    median = median(weight, na.rm = TRUE),
    min = min(weight, na.rm = TRUE),
    max = max(weight, na.rm = TRUE),
    shapiro_p_value = shapiro_test(weight)$p.value
  )
```

    ## # A tibble: 3 × 8
    ##   group count  mean    sd median   min   max shapiro_p_value
    ##   <fct> <int> <dbl> <dbl>  <dbl> <dbl> <dbl>           <dbl>
    ## 1 ctrl     10  5.03 0.583   5.15  4.17  6.11           0.747
    ## 2 trt1     10  4.66 0.794   4.55  3.59  6.03           0.452
    ## 3 trt2     10  5.53 0.443   5.44  4.92  6.31           0.564

``` r
PlantGrowth %>%
  levene_test(weight ~ group)
```

    ## # A tibble: 1 × 4
    ##     df1   df2 statistic     p
    ##   <int> <int>     <dbl> <dbl>
    ## 1     2    27      1.12 0.341

``` r
res.aov <- PlantGrowth %>% anova_test(weight ~ group)
res.aov
```

    ## ANOVA Table (type II tests)
    ## 
    ##   Effect DFn DFd     F     p p<.05   ges
    ## 1  group   2  27 4.846 0.016     * 0.264

``` r
weight_aov = aov(weight ~ group, data = PlantGrowth)
summary(weight_aov)
```

    ##             Df Sum Sq Mean Sq F value Pr(>F)  
    ## group        2  3.766  1.8832   4.846 0.0159 *
    ## Residuals   27 10.492  0.3886                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
pwc <- PlantGrowth %>% tukey_hsd(weight ~ group)
pwc
```

    ## # A tibble: 3 × 9
    ##   term  group1 group2 null.value estimate conf.low conf.high p.adj p.adj.signif
    ## * <chr> <chr>  <chr>       <dbl>    <dbl>    <dbl>     <dbl> <dbl> <chr>       
    ## 1 group ctrl   trt1            0   -0.371   -1.06      0.320 0.391 ns          
    ## 2 group ctrl   trt2            0    0.494   -0.197     1.19  0.198 ns          
    ## 3 group trt1   trt2            0    0.865    0.174     1.56  0.012 *

``` r
# Tukey's HSD post hoc test
tukey_result <- TukeyHSD(weight_aov)

print(tukey_result)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = weight ~ group, data = PlantGrowth)
    ## 
    ## $group
    ##             diff        lwr       upr     p adj
    ## trt1-ctrl -0.371 -1.0622161 0.3202161 0.3908711
    ## trt2-ctrl  0.494 -0.1972161 1.1852161 0.1979960
    ## trt2-trt1  0.865  0.1737839 1.5562161 0.0120064

``` r
plot(TukeyHSD(weight_aov, conf.level = 0.95))
```

![](FA8_GONZALES_1BANNOVA_files/figure-gfm/setup-2.png)<!-- -->
