Exploring Spotify Music Data
================
By Mwangi George
Last edited Nov, 14 2022

-   <a href="#amount-of-spoken-words" id="toc-amount-of-spoken-words">Amount
    of Spoken Words</a>

# Amount of Spoken Words

This section aims to explore the amount of spoken words in the spotify
dataset. This variable is represented by the speechiness column, where
more values mean more spoken words. Lets start by understanding the
general statistics about the variable by calculating the overall mean,
median and other statistics as follows.

``` r
summary(spotify$speechiness)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.000   4.000   5.000   8.358   9.000  48.000

From the above output, we can see that, all the songs from 2010 to 2019
have an average speechiness score of 8.36 and a median score of 5. The
minimum and maximum speechiness scores are 0 and 48 respectively. There
is quite a big difference between the mean and the median, which means
there are outliers in the variable. Let’s utilize a density plot to
explore visualize the distribution of the overall speechiness of the
songs.

``` r
spotify %>% 
  ggplot(aes(speechiness, group =1))+
  geom_density(fill = "blue", alpha = .4)+
  theme_clean() +
  labs(
    title = "Overall Mean Speechiness",
  )
```

![](spotify_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

The distribution of speechiness is heavily skewed to the right. This
explains why the mean is greater than than the median. This skewness
renders the mean a poor measure of centrality. We should therefore use
the median as a safer alternative to describe centrality of the
speechiness variable. Let’s therefore explore how the median speechiness
of the songs varied over time.

``` r
spotify %>%
  group_by(year) %>%
  summarise(median_speechiness = median(speechiness)) %>%
  ggplot(aes(factor(year), median_speechiness, group = 1)) +
  geom_line() +
  geom_point() +
  expand_limits(y = c(4, 10)) +
  theme_clean() +
  labs(
    title = "Median Speechiness over time",
    x = "Year",
    y = "Median Speechiness"
  )
```

![](spotify_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

According to the above graph, from 2010 to 2015, the median level of
speechiness remained at 5. In 2016, this value increased to 6, and it
stayed that way until 2018, when it dropped back to 5.

Let’s explore the association between speechiness and popularity of the
songs.

``` r
# correlation coefficient
cor(spotify$speechiness, spotify$popularity)
```

    ## [1] -0.04149019

The correlation coefficient between the variables speechiness and
popularity is -0.0415. This value means that there exists a weak
negative association between speechiness and popularity. High
speechiness in a song mean lower popularity but this association is very
weak. We can visualize this relationship using a scatter plot.

``` r
spotify %>%
  ggplot(aes(speechiness, popularity)) +
  geom_jitter(color = "blue", alpha = .4) +
  theme_clean() +
  labs(
    title = "Popularity Vs Speechiness",
    x = "Speechiness",
    y = "Popularity"
  )
```

![](spotify_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
