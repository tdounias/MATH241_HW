hw\_2\_review
================
Theodore Dounias
February 24, 2017

**Exercise 3.1**

We create the desired graph

``` r
ggplot(Galton, aes(father, height)) +
  geom_point(aes(col = sex)) +
  geom_smooth(method = "lm") +
  facet_wrap(~sex)
```

![](hw_2_revision_files/figure-markdown_github/unnamed-chunk-2-1.png)

**Exercise 3.2**

``` r
#Transform RailTrail weekday into more understandable form
RailTrail1 <- RailTrail
bool <- c("no", "yes")
RailTrail1$weekday <- bool[RailTrail$weekday]
#Plot graph
ggplot(RailTrail1, aes(hightemp, volume, col = weekday)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~weekday)
```

![](hw_2_revision_files/figure-markdown_github/unnamed-chunk-3-1.png)
