library(readr)
library(ggplot2)

twoa <- read_rds("https://github.com/bmaitner/biometry_course/raw/refs/heads/main/midterm/b_2a.RDS")
twob <- read_rds("https://github.com/bmaitner/biometry_course/raw/refs/heads/main/midterm/b_2b.RDS")
twoc <- read_rds("https://github.com/bmaitner/biometry_course/raw/refs/heads/main/midterm/b_2c.RDS")

# 2a: bird body mass vs egg mass
# A. Function: power law (slope about 0.80)
# B. Distribution: lognormal
# Evidence: on log-log axes the points fall on a straight line (R^2 = 0.94).
# Raw egg mass is strongly skewed (mean 22.7 g vs median 3.9 g).
# The lognormal fits better than gamma (by 18 AIC units) and far better than normal.
ggplot(twoa, aes(x = Adult.Body.Mass.g, y = Egg.Mass.g)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10()

# 2b: fish abundance vs species richness
# A. Function: Michaelis-Menten (saturating)
# B. Distribution: Poisson within each of two groups
# Evidence: the data are two groups mixed together. Richness has two peaks,
# and the points form two bands (one levels off near 19 species, the other near 42).
# Pooled, variance is 3-5x the mean, but a two-group Poisson model beats the
# pooled negative binomial by about 22 AIC units, and within each group the
# variance is about equal to the mean.
ggplot(twob, aes(x = Total.Fish.Abundance, y = Species.Richness)) +
  geom_point() +
  geom_smooth(method = "nls", formula = y ~ a * x / (b + x),
              method.args = list(start = list(a = 35, b = 100)), se = FALSE)

# 2c: rugosity vs % hard bottom cover
# A. Function: logistic, levelling off near 100%
# B. Distribution: beta, with spread shrinking as rugosity rises
# Evidence: the beta beats a binomial and a normal. The binomial is ruled out
# because the observed variance is 10-50 times what it allows.
ggplot(twoc, aes(x = Rugosity, y = Hard.Bottom.Cover.Pct)) +
  geom_point() +
  geom_smooth(method = "nls", formula = y ~ 100 / (1 + exp(-(a + b * x))),
              method.args = list(start = list(a = 1, b = 1)), se = FALSE)

# Part 3: Power analysis ---------------------------------------------------
#the lovely people of reddit saved me with this one :o

a <- 2
b <- 1

nsim <- 400
sdvec <- seq(1, 20, by = 1)
sample_sizes <- c(10, 20, 40)

results <- data.frame()

for(k in 1:length(sample_sizes)){
  
  sample_size <- sample_sizes[k]
  power.sd <- numeric(length(sdvec))
  pval <- numeric(nsim)
  
  for(j in 1:length(sdvec)){
    for(i in 1:nsim){
      
      x <- sample(x = 1:20,
                  size = sample_size,
                  replace = TRUE)
      
      sd <- sdvec[j]
      y_det <- a + b*x
      y <- rnorm(n = length(y_det),
                 mean = y_det,
                 sd = sd)
      
      m <- lm(y ~ x)
      
      #get p-value
      
      pval[i] <- coef(summary(m))["x","Pr(>|t|)"]
      
    }#end i loop
    power.sd[j] <- sum(pval < 0.05)/nsim
    
  }#end j loop
  
  results <- rbind(results,
                   data.frame(sample_size = sample_size,
                              sd = sdvec,
                              power = power.sd))
  
}#end k loop

# Figure

ggplot(results, aes(x = sd, y = power, color = factor(sample_size))) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  ylim(0, 1) +
  labs(x = "Standard deviation of y (noise)",
       y = "Power (proportion of p < 0.05)",
       color = "Sample size",
       caption = "Figure 1. Power to detect a slope of 1 (intercept = 2) across noise levels,\nfor three sample sizes (400 simulations each). Dashed line marks 0.8 power.")

# Double sample size vs halve noise (starting at n = 20, sd = 10)

subset(results, (sample_size == 20 & sd %in% c(5, 10)) |
         (sample_size == 40 & sd == 10))


## Written answer -----------------------------------------------------------

# The noisier the data got, the harder it was to pick up the slope, no matter
# the sample size. Once the scatter gets big enough, a real relationship starts
# to look like random noise. More samples helped. With 40 samples, power stayed
# high at noise levels where 10 samples almost never found the slope.
#
# If I had to choose, I'd cut measurement error in half rather than double my
# sample size. In the simulations, halving the noise raised power more than
# doubling the samples did. That's because the uncertainty in the slope shrinks
# with the square root of the sample size, so it takes about four times as many
# samples to match what halving the noise does. In practice, careful
# measurement can save a lot of extra fieldwork.