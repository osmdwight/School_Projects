2 WAY ANNOVA
================
GONZALES_QUINTEROS

``` r
library(palmerpenguins)
library(dplyr)
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
library(car)

data <- read.csv("C:\\Users\\DWIGHT JIRO\\Downloads\\Political Interest.csv")
head(data)
```

    ##   gender education_level political_interest
    ## 1      1               1                 38
    ## 2      1               1                 39
    ## 3      1               1                 35
    ## 4      1               1                 38
    ## 5      1               1                 41
    ## 6      1               1                 40

``` r
data %>%
  group_by(gender, education_level) %>%
  summarize(
    count = n(),
    mean = mean(political_interest, na.rm = TRUE),
    sd = sd(political_interest, na.rm = TRUE),
    median = median(political_interest, na.rm = TRUE),
    min = min(political_interest, na.rm = TRUE),
    max = max(political_interest, na.rm = TRUE),
    shapiro_p_value = shapiro_test(political_interest)$p.value
  )
```

    ## # A tibble: 6 × 9
    ## # Groups:   gender [2]
    ##   gender education_level count  mean    sd median   min   max shapiro_p_value
    ##    <int>           <int> <int> <dbl> <dbl>  <dbl> <dbl> <dbl>           <dbl>
    ## 1      1               1     9  37.4  2.51   38    33    41             0.971
    ## 2      1               2     9  42.9  2.34   43.5  38.5  46.5           0.761
    ## 3      1               3    10  64.1  3.07   63.5  60    69             0.320
    ## 4      2               1    10  39.6  3.27   39.5  34    44             0.819
    ## 5      2               2    10  44.6  3.27   44.5  39    49             0.819
    ## 6      2               3    10  58    6.46   58.5  45    66             0.668

``` r
data$gender <- as.factor(data$gender)
data$education_level <- as.factor(data$education_level)

# Levene's test for homogeneity of variances
leveneTest(political_interest ~ gender * education_level, data = data)
```

    ## Levene's Test for Homogeneity of Variance (center = median)
    ##       Df F value  Pr(>F)  
    ## group  5  2.2054 0.06765 .
    ##       52                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
data$gender <- as.factor(data$gender)
data$education_level <- as.factor(data$education_level)
data$group <- interaction(data$gender, data$education_level, sep = "_")

# Raincloud plot
data %>%
  ggplot(aes(x = group, y = political_interest, fill = group)) +
  
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
    binwidth = 1,               
    size = 1.5                  
  ) +
  
  theme_minimal() +
  labs(title = "Raincloud Plot for Political Interest by Gender and Education Level",
       x = "Group (Gender_Education Level)", 
       y = "Political Interest") +
  theme(legend.position = "none")
```

![](GONZALES_QUINTEROS_FA9_files/figure-gfm/setup-1.png)<!-- -->

``` r
anova_model <- aov(political_interest ~ gender * education_level, data = data)
summary(anova_model)
```

    ##                        Df Sum Sq Mean Sq F value  Pr(>F)    
    ## gender                  1     26    25.7   1.788 0.18704    
    ## education_level         2   5410  2705.0 188.136 < 2e-16 ***
    ## gender:education_level  2    210   105.2   7.315 0.00159 ** 
    ## Residuals              52    748    14.4                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Tukey HSD test
tukey_result <- TukeyHSD(anova_model, conf.level = 0.95)

print(tukey_result)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = political_interest ~ gender * education_level, data = data)
    ## 
    ## $gender
    ##          diff       lwr      upr     p adj
    ## 2-1 -1.332143 -3.331504 0.667218 0.1870433
    ## 
    ## $education_level
    ##          diff      lwr       upr    p adj
    ## 2-1  5.236842  2.26881  8.204874 0.000253
    ## 3-1 22.435996 19.50530 25.366693 0.000000
    ## 3-2 17.199154 14.26846 20.129851 0.000000
    ## 
    ## $`gender:education_level`
    ##              diff          lwr       upr     p adj
    ## 2:1-1:1  2.155556  -2.99895749  7.310069 0.8165366
    ## 1:2-1:1  5.500000   0.21158120 10.788419 0.0370678
    ## 2:2-1:1  7.155556   2.00104251 12.310069 0.0018818
    ## 1:3-1:1 26.655556  21.50104251 31.810069 0.0000000
    ## 2:3-1:1 20.555556  15.40104251 25.710069 0.0000000
    ## 1:2-2:1  3.344444  -1.81006860  8.498957 0.4021433
    ## 2:2-2:1  5.000000  -0.01703459 10.017035 0.0512608
    ## 1:3-2:1 24.500000  19.48296541 29.517035 0.0000000
    ## 2:3-2:1 18.400000  13.38296541 23.417035 0.0000000
    ## 2:2-1:2  1.655556  -3.49895749  6.810069 0.9312003
    ## 1:3-1:2 21.155556  16.00104251 26.310069 0.0000000
    ## 2:3-1:2 15.055556   9.90104251 20.210069 0.0000000
    ## 1:3-2:2 19.500000  14.48296541 24.517035 0.0000000
    ## 2:3-2:2 13.400000   8.38296541 18.417035 0.0000000
    ## 2:3-1:3 -6.100000 -11.11703459 -1.082965 0.0088708

``` r
plot(tukey_result)
```

![](GONZALES_QUINTEROS_FA9_files/figure-gfm/setup-2.png)<!-- -->![](GONZALES_QUINTEROS_FA9_files/figure-gfm/setup-3.png)<!-- -->![](GONZALES_QUINTEROS_FA9_files/figure-gfm/setup-4.png)<!-- -->

``` r
par(mar = c(4, 4, 2, 1))  
interaction.plot(
  x.factor = data$education_level, 
  trace.factor = data$gender, 
  response = data$political_interest,
  col = c("blue", "red"),  # Colors for the lines representing different genders
  lty = 1,  # Line type for the plot (solid lines)
  type = "b",  # Both points and lines will be plotted
  xlab = "Education Level",  # Label for the x-axis
  ylab = "Political Interest",  # Label for the y-axis
  legend = TRUE,  # Show legend
  trace.label = "Gender",  # Label for the legend
  main = "Interaction Plot: Education Level and Gender on Political Interest"  # Main title of the plot
)

text(x = 2.5, y = 50, 
     col = "black", 
     cex = 0.9,  
     pos = 4)  
grid()
```

![](GONZALES_QUINTEROS_FA9_files/figure-gfm/setup-5.png)<!-- -->
