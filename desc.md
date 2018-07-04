        qqnorm(data$responsetime)

        qqline(data$responsetime)

Right skew typically exhibits a convex appearance, left skew typically concave. With excess kurtosis <0, typically the tails are closer to the horizontal mid-line than the qqline predicts; with excess kurtosis >0, typically one or both of the tails is more extreme (farther away from the horizontal mid-line) than the qqline predicts.

You should see a concave appearance in the qq-plot of your data, with the right tail much above the qqline. This indicates that your distribution produces outliers greatly in excess of what is predicted by the normal distribution in the right tail.

Kurtosis measures outliers, not the peak of the distribution. That might be a source of confusion for some people when it comes to relating the kurtosis statistic to the histogram.

The logic to understand why kurtosis measures outliers (not peak) is simple: Large |Z|-values indicate outliers. Kurtosis is the average of the Z^4 values. So |Z|-values close to zero (where the peak is) contribute virtually nothing to the kurtosis statistic, and thus the kurtosis statistic is non-informative about the peak. You can have a high kurtosis when the peak is pointy and you can have a high kurtosis when the peak is flat. It all depends on the disposition of the outliers.
