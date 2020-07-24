Course 5 Project 1
------------------

We'll begin by pulling in the necessary libraries

    library(tidyverse)

    ## ── Attaching packages ──────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.2.1     ✓ purrr   0.3.3
    ## ✓ tibble  2.1.3     ✓ dplyr   0.8.3
    ## ✓ tidyr   1.0.0     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.4.0

    ## ── Conflicts ─────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

    library(lubridate)

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

### Step 1

First, we pull in the data (and do a little date transformation for good
measure)

    data <- read.csv("Course5Project1/activity.csv")

    data$date <- ymd(data$date)

### Step 2

Here, the goal is to summarize by total steps per day. We can use
DPLYR's group\_by function to group by date, allowing us to take a sum
for each day

    data2 <- data %>%
          group_by(date) %>%
          summarize(steps = sum(steps, na.rm = TRUE))

Next we visualize

    ggplot(data2, aes(x = data2$steps)) +
          geom_histogram(bins = 25) +
          labs(x = "steps", y = "Count", title = "Steps per Day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

### Step 3

And now it's time to summarize by the mean and median steps per day.
Once again, group\_by and summarize are our friends.

    data3 <- data %>%
          group_by(date) %>%
          summarize(mean_steps =mean(steps, na.rm = TRUE) ,
                    median_steps = median(steps, na.rm = TRUE))

And what answer do we get?

    print(data3)

    ## # A tibble: 61 x 3
    ##    date       mean_steps median_steps
    ##    <date>          <dbl>        <dbl>
    ##  1 2012-10-01    NaN               NA
    ##  2 2012-10-02      0.438            0
    ##  3 2012-10-03     39.4              0
    ##  4 2012-10-04     42.1              0
    ##  5 2012-10-05     46.2              0
    ##  6 2012-10-06     53.5              0
    ##  7 2012-10-07     38.2              0
    ##  8 2012-10-08    NaN               NA
    ##  9 2012-10-09     44.5              0
    ## 10 2012-10-10     34.4              0
    ## # … with 51 more rows

### Step 4

Now we've gotta do a time series plot, so, once again, we use group\_by
to do the heavy lifting.

    data4 <- data %>%
          group_by(interval) %>%
          summarize(mean_steps =mean(steps, na.rm = TRUE) ,
                    median_steps = median(steps, na.rm = TRUE))

And then we visualize

    ggplot(data4, aes(x = interval, y = mean_steps)) +
          geom_line() +
          labs(x = "Time Interval", y = "Mean Steps", title = "Mean Steps per Day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-8-1.png)

### Step 5

Now we have more of a quantitative question. Which time interval
contains the max number of steps, on average? Yet again, group\_by comes
to the rescue

    data5 <- data %>%
          group_by(interval) %>%
          summarize(mean = mean(steps, na.rm = TRUE))

And what are our results?

    print(data5 %>%
          filter(mean == max(data4$mean)))

    ## Warning: Unknown or uninitialised column: 'mean'.

    ## Warning in max(data4$mean): no non-missing arguments to max; returning -Inf

    ## # A tibble: 0 x 2
    ## # … with 2 variables: interval <int>, mean <dbl>

### Step 6

Ooh imputation. Intimidating. But, with some quick tricks and a forloop,
it shouldn't be too much trouble. Our strategy is to take average for
each interval and impute that into the period in question

    colSums(is.na(data))

    ##    steps     date interval 
    ##     2304        0        0

    data.imputed <- data

    for (i in 1:length(data.imputed$steps)) {
          if (is.na(data.imputed$steps[i]) == TRUE) {
                data.imputed$steps[i] <- mean(
                      (data.imputed %>%
                             filter(interval == data.imputed[i,3]) %>%
                             select(steps))$steps,
                      na.rm = TRUE
                )
          }
    }

Nice!

### Step 7

And next, of course, we visualize

    data2.imputed <- data.imputed %>%
          group_by(date) %>%
          summarize(steps = sum(steps, na.rm = TRUE))

    ggplot(data2.imputed, aes(x = data2.imputed$steps)) +
          geom_histogram(bins = 25) +
          labs(x = "steps", y = "Count", title = "Steps per Day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-12-1.png)

### Step 8

Now, a simple panel plot comparing the average number of steps taken per
5-minute interval across weekdays and weekends. This is going to take a
little extra wrangling, but not a huge deal.

    data_final <- data.imputed %>%
          mutate(weekday = weekdays(data.imputed$date)) %>%
          mutate(weekend = ifelse(weekdays(data.imputed$date) %in% c("Sunday", "Saturday"), "Weekend", "Weekday")) %>%
          group_by(interval, weekend) %>%
          summarize(mean(steps))

    names(data_final)[3] <- "steps"

But now we visualize

    ggplot(data_final, aes(x=interval, y=steps)) +
          geom_point() +
          facet_grid(cols = vars(weekend)) +
          labs(y ="Average Steps", x= "5 Minute Interval", title = "Steps per 5 Minute Interval, Weekday vs Weekend")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-14-1.png)

And voila, we're done!
