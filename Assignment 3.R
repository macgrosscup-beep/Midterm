# Assignment 3, Part 1: model modification and selection
# Mac Grosscup

library(bbmle)

# install.packages("remotes")
# remotes::install_github("bmaitner/biometry_course", subdir = "biometrydata")
library(biometrydata)


# 1a: Competition between bean beetles ----------------------------------------

beetles <- beetle_competition_interspecific
# or: beetles <- read.csv("https://github.com/bmaitner/biometry_course/raw/refs/heads/main/data/Beetle_competition/competition_interspecific.csv")

beetles <- beetles[beetles$pair == "Cmac_Csub" & !is.na(beetles$progeny_1), ]

plot(beetles$start_females_1 + beetles$start_females_2,
     beetles$progeny_1 / beetles$start_females_1,
     xlab = "females in the dish, both species",
     ylab = "C. maculatus offspring per female")

# Model 1: starting model (Beverton-Holt family with d)
beetle.fit <- mle2(progeny_1 ~ dnbinom(mu = R * start_females_1 /
                                         (1 + (a11 * start_females_1 +
                                                 a12 * start_females_2)^d),
                                       size = k),
                   start = list(R = 20, a11 = 0.1, a12 = 0.1, d = 1, k = 2),
                   lower = c(R = 0.01, a11 = 0, a12 = 0, d = 0.1, k = 0.01),
                   upper = c(R = 1000, a11 = 10, a12 = 10, d = 10, k = 100),
                   method = "L-BFGS-B",
                   data = beetles)

# Model 2: classic Beverton-Holt (d = 1)
beetle.bh <- mle2(progeny_1 ~ dnbinom(mu = R * start_females_1 /
                                        (1 + a11 * start_females_1 +
                                           a12 * start_females_2),
                                      size = k),
                  start = list(R = 20, a11 = 0.1, a12 = 0.1, k = 2),
                  lower = c(R = 0.01, a11 = 0, a12 = 0, k = 0.01),
                  upper = c(R = 1000, a11 = 10, a12 = 10, k = 100),
                  method = "L-BFGS-B",
                  data = beetles)

# Model 3: Beverton-Holt, no effect of C. subinnotatus (a12 = 0)
beetle.no_a12 <- mle2(progeny_1 ~ dnbinom(mu = R * start_females_1 /
                                            (1 + a11 * start_females_1),
                                          size = k),
                      start = list(R = 20, a11 = 0.1, k = 2),
                      lower = c(R = 0.01, a11 = 0, k = 0.01),
                      upper = c(R = 1000, a11 = 10, k = 100),
                      method = "L-BFGS-B",
                      data = beetles)

# Model 4: Beverton-Holt, both species equally harmful (one parameter, a)
beetle.equal <- mle2(progeny_1 ~ dnbinom(mu = R * start_females_1 /
                                           (1 + a * (start_females_1 +
                                                       start_females_2)),
                                         size = k),
                     start = list(R = 20, a = 0.1, k = 2),
                     lower = c(R = 0.01, a = 0, k = 0.01),
                     upper = c(R = 1000, a = 10, k = 100),
                     method = "L-BFGS-B",
                     data = beetles)

# Model 5: Ricker shape
beetle.ricker <- mle2(progeny_1 ~ dnbinom(mu = R * start_females_1 *
                                            exp(-a11 * start_females_1 -
                                                  a12 * start_females_2),
                                          size = k),
                      start = list(R = 20, a11 = 0.01, a12 = 0.01, k = 2),
                      lower = c(R = 0.01, a11 = 0, a12 = 0, k = 0.01),
                      upper = c(R = 1000, a11 = 10, a12 = 10, k = 100),
                      method = "L-BFGS-B",
                      data = beetles)

AICtab(beetle.fit, beetle.bh, beetle.no_a12, beetle.equal, beetle.ricker,
       base = TRUE, weights = TRUE)

coef(beetle.bh)
coef(beetle.bh)["a12"] / coef(beetle.bh)["a11"]

# Conclusion:
#   - d isn't needed. The classic Beverton-Holt (d = 1) has a lower AIC than
#     the starting model (~1 unit), so the extra parameter isn't supported.
#   - C. subinnotatus does affect C. maculatus. Dropping a12 makes the model
#     ~3.3 AIC units worse.
#   - But it isn't as harmful as C. maculatus itself. Forcing one shared
#     competition parameter is ~3.6 units worse, and a12 is only about a
#     third of a11. Competition within species is stronger than competition
#     between species, which is what you'd expect if the two can coexist.
#   - The Ricker isn't nested in the starting model (no value of d turns one
#     into the other), but AIC can still compare them since both use the
#     same data and response. It's ~1 unit better than Beverton-Holt, so the
#     data can't really tell the two shapes apart.
#   - Best model: Beverton-Holt or Ricker, with separate a11 and a12.


# 1b: Wing length in hawks and eagles -----------------------------------------

hawks <- avonet
# or: hawks <- readRDS(url("https://github.com/bmaitner/biometry_course/raw/refs/heads/main/data/Avonet/AVONET1_BirdLife.rds"))

hawks <- hawks[hawks$Family1 == "Accipitridae", c("Species1", "Wing.Length", "Mass", "Migration")]
hawks <- na.omit(hawks)
hawks$migration <- factor(hawks$Migration, levels = 1:3,
                          labels = c("resident", "partial", "migrant"))

# new column: resident vs. migratory (partial and full migrants together)
hawks$migratory <- factor(hawks$migration == "resident",
                          levels = c(TRUE, FALSE),
                          labels = c("resident", "migratory"))

plot(log(hawks$Mass), log(hawks$Wing.Length), col = hawks$migration,
     xlab = "log body mass", ylab = "log wing length")
legend("topleft", levels(hawks$migration), col = 1:3, pch = 1)

# Model 1: starting model (intercept, slope and scatter all differ)
hawk.fit <- mle2(Wing.Length ~ dlnorm(meanlog = int + b * log(Mass),
                                      sdlog = exp(log_s)),
                 start = list(int = 3, b = 0.3, log_s = -2),
                 parameters = list(int ~ migration,
                                   b ~ migration,
                                   log_s ~ migration),
                 data = hawks)

# Model 2: only the intercept differs among the 3 classes
hawk.int <- mle2(Wing.Length ~ dlnorm(meanlog = int + b * log(Mass),
                                      sdlog = exp(log_s)),
                 start = list(int = 3, b = 0.3, log_s = -2),
                 parameters = list(int ~ migration),
                 data = hawks)

# Model 3: only the intercept differs, resident vs. migratory
hawk.int2 <- mle2(Wing.Length ~ dlnorm(meanlog = int + b * log(Mass),
                                       sdlog = exp(log_s)),
                  start = list(int = 3, b = 0.3, log_s = -2),
                  parameters = list(int ~ migratory),
                  data = hawks)

# Model 4: no migration effect
hawk.null <- mle2(Wing.Length ~ dlnorm(meanlog = int + b * log(Mass),
                                       sdlog = exp(log_s)),
                  start = list(int = 3, b = 0.3, log_s = -2),
                  data = hawks)

AICtab(hawk.fit, hawk.int, hawk.int2, hawk.null,
       base = TRUE, weights = TRUE)

coef(hawk.int2)
exp(coef(hawk.int2)["int.migratorymigratory"])

# Conclusion:
#   - Migrants do have longer wings for their mass. That's the intercept,
#     and the model with no migration effect is ~14 AIC units worse than
#     the best model.
#   - Slope and scatter don't need to differ. The starting model, which lets
#     all three vary, is ~7.5 units worse than letting only the intercept
#     vary. Wing length scales with mass the same way in every group
#     (b ~ 0.33), and no group is more variable.
#   - Partial vs. full migrants isn't needed. The 2-group model is slightly
#     better than the 3-group model (~0.8 units), so the simpler split
#     is enough.
#   - Best model: shared slope and scatter, with migratory species having
#     ~8% longer wings than residents of the same mass. That fits the idea
#     that longer wings make sustained flight cheaper.
#   - Caveat: species aren't fully independent because related species
#     share traits, so the evidence is probably a bit weaker than AIC
#     suggests.


# 1c: Reef fish species richness ----------------------------------------------

reef <- reef_fish_surveys
# or: reef <- read.csv("https://github.com/bmaitner/biometry_course/raw/refs/heads/main/data/Reef_fish/NCRMP_reef_fish_surveys.csv")

reef$depth <- reef$DEPTH / 10
reef$visibility <- reef$UNDERWATER_VISIBILITY / 10
reef$year <- reef$YEAR - 2014
reef$region <- factor(reef$REGION_DESCRIPTION,
                      levels = c("Dry Tortugas",
                                 "Florida Keys: from Key West to Miami",
                                 "Miami to North Martin County"),
                      labels = c("Tortugas", "Keys", "North"))
reef$protected <- ifelse(reef$MPA_NAME == "Unprotected", "no", "yes")

plot(reef$DEPTH, reef$species_richness, col = rgb(0, 0, 0, 0.1),
     xlab = "depth (m)", ylab = "species recorded")

# Check the Poisson: variance / mean should be about 1
var(reef$species_richness) / mean(reef$species_richness)

# Model 1: starting model (Poisson)
reef.fit <- mle2(species_richness ~ dpois(lambda = exp(int +
                                                         d1 * depth +
                                                         d2 * depth^2 +
                                                         v * visibility +
                                                         t * year)),
                 start = list(int = 3, d1 = 0, d2 = 0, v = 0, t = 0),
                 parameters = list(int ~ region + protected),
                 data = reef)

# Model 2: same terms, negative binomial
reef.nb <- mle2(species_richness ~ dnbinom(mu = exp(int +
                                                      d1 * depth +
                                                      d2 * depth^2 +
                                                      v * visibility +
                                                      t * year),
                                           size = k),
                start = list(int = 3, d1 = 0, d2 = 0, v = 0, t = 0, k = 5),
                parameters = list(int ~ region + protected),
                data = reef)

# Model 3: negative binomial, no protection
reef.noprot <- mle2(species_richness ~ dnbinom(mu = exp(int +
                                                          d1 * depth +
                                                          d2 * depth^2 +
                                                          v * visibility +
                                                          t * year),
                                               size = k),
                    start = list(int = 3, d1 = 0, d2 = 0, v = 0, t = 0, k = 5),
                    parameters = list(int ~ region),
                    data = reef)

# Model 4: negative binomial, no visibility
reef.novis <- mle2(species_richness ~ dnbinom(mu = exp(int +
                                                         d1 * depth +
                                                         d2 * depth^2 +
                                                         t * year),
                                              size = k),
                   start = list(int = 3, d1 = 0, d2 = 0, t = 0, k = 5),
                   parameters = list(int ~ region + protected),
                   data = reef)

AICtab(reef.fit, reef.nb, reef.noprot, reef.novis,
       base = TRUE, weights = TRUE)

# Effect sizes: exp(coefficient) = multiplier on expected richness
# (ignore the k row here; k itself is coef(reef.nb)["k"], about 7.5)
round(exp(coef(reef.nb)), 3)

# Conclusion:
#   - The Poisson is wrong. Variance is ~4.4 times the mean, and the negative
#     binomial lowers AIC by about 13,500. Under the Poisson the standard
#     errors are too small, so every effect looks stronger than it is.
#   - With 8,560 surveys, AIC keeps every term, so the real question is how
#     big each effect is (exp of the coefficients):
#       region: Keys and North have ~35-38% more species than Tortugas
#       depth: hump-shaped, with richness peaking at ~17 m
#       visibility: ~14% more species per extra 10 m of visibility
#       protection: ~7% more species inside MPAs
#       year: under 1% per year
#   - Visibility describes the water on the day, not the reef. Its effect is
#     most likely detection: in clear water, divers can see and record more
#     of the species that are there. So it's worth keeping as a correction
#     for survey conditions, not as a cause of diversity.
#   - Protection still matters once region is in the model (dropping it is
#     ~36 units worse), but it's a modest effect. 
#   - Best model: the negative binomial with all terms. Region and depth
#     drive most of the differences; protection and year are small effects,
#     and visibility mostly reflects detection.