library(readr)
library(ggplot2)
#part 1
# Introduction -------------------------------------------------------------

# Salinity is among the most powerful abiotic stresses a plant can face. Salt
# lowers soil water potential, and sodium and chloride become toxic once
# inside, so plants rely on osmotic adjustment, ion exclusion, or storing ions
# in vacuoles to survive (Flowers & Colmer, 2008; Munns & Tester, 2008). These
# costs show up at the community level, where plant richness usually rises as
# salinity falls or peaks at intermediate salinities (Maccioni et al., 2021;
# Perry & Atkinson, 1997; Watson & Byrne, 2009).
#
# Trait-based ecology asks which functional strategies are filtered in or out
# along an environmental gradient (Keddy, 1992; Violle et al., 2007). Specific
# leaf area (SLA) separates fast, acquisitive leaves from slow, conservative
# ones (Wright et al., 2004), leaf thickness and succulence describe water
# storage and salt dilution (Ogburn & Edwards, 2012), and plant height reflects
# competition for light (Perez-Harguindeguy et al., 2013). If salinity acts as
# a filter, high-salinity communities should converge on thick, succulent,
# low-SLA, short plants, as seen in salt marshes and mangroves (Akhani et al.,
# 2022; Perri et al., 2023). Individuals also vary, and intraspecific variation
# makes up a substantial share of trait variation (Siefert et al., 2015).
#
# Florida's coasts are a useful place to ask these questions. Over a few tens
# of meters, a single transect can pass from mangroves and salt marsh
# succulents through beach-dune grasses into maritime hammock, and this
# ecotone is shifting as mangroves expand and sea level rises (Kirwan &
# Megonigal, 2013; Yando et al., 2016). Most previous studies have looked at
# one preserve or one vegetation type, and few have used the same trait
# protocol along continuous salinity gradients at several sites. This study
# fills that gap.

# Materials and Methods ----------------------------------------------------

# Study sites. I sampled five coastal preserves in Florida (CBP, EPS, FDS, PP,
# and RP; Table 1), choosing sites where vegetation ran uninterrupted from the
# shoreline inland so a single transect could capture the full salinity
# gradient.
#
# Plot layout. At each preserve I ran one transect perpendicular to the
# shoreline, starting at the seaward-most vegetation. Along it I laid out
# 1 m x 10 m plots, long side parallel to the shore, with 1 m gaps between
# plots (Horton et al., 2008). This alternate-plot design covers the full
# sequence of coastal habitats with less effort (Del Vecchio et al., 2019;
# Tirgan et al., 2022), and the narrow depth keeps each plot within one
# salinity band (Eisenlohr et al., 2011). I added plots inland until soil
# salinity dropped to background levels, giving 4 plots at CBP and PP, 6 at
# RP, 7 at FDS, and 10 at EPS (31 total). Plot 2 at FDS had no vegetation and
# was kept as a zero-richness plot.
#
# Soil and vegetation. In each plot I measured soil electrical conductivity
# (EC, mS cm-1), a common proxy for salinity (Munns & Tester, 2008). Several
# seaward plots read 4.00, which may be the meter's upper limit. I identified
# every vascular plant species and visually estimated its percent cover.
#
# Traits. For each species in each plot I measured nine mature individuals,
# the sample size Maitner et al. (2023) found reliable for community trait
# distributions, following Perez-Harguindeguy et al. (2013). So far I have
# measured plant height (cm), leaf thickness (mm), and leaf fresh mass (g);
# leaf area, dry mass, SLA, LDMC, succulence, and leaf salinity are still
# being processed.
#
# Analysis. Analyses at this stage are descriptive. In R (R Core Team, 2025),
# I calculated richness and Shannon diversity per plot with vegan (Oksanen et
# al., 2022) and plotted soil EC, diversity, and traits along each transect
# with ggplot2 (Wickham, 2016).

# Materials and Methods ----------------------------------------------------

# Study sites. I sampled six coastal preserves in Florida (CBP, EPS, FDS, PP,
# and RP; Table 1), choosing sites where vegetation ran uninterrupted from the
# shoreline inland so a single transect could capture the full salinity
# gradient.
#
# Plot layout. At each preserve I ran one transect perpendicular to the
# shoreline, starting at the seaward-most vegetation. Along it I laid out
# 1 m x 10 m plots, long side parallel to the shore, with 1 m gaps between
# plots (Horton et al., 2008). This alternate-plot design covers the full
# sequence of coastal habitats with less effort (Del Vecchio et al., 2019;
# Tirgan et al., 2022), and the narrow depth keeps each plot within one
# salinity band (Eisenlohr et al., 2011). I added plots inland until soil
# salinity dropped to background levels, giving 4 plots at CBP and PP, 6 at
# RP, 7 at FDS, and 10 at EPS (31 total). Plot 2 at FDS had no vegetation and
# was kept as a zero-richness plot.
#
# Soil and vegetation. In each plot I measured soil electrical conductivity
# (EC, mS cm-1), a common proxy for salinity (Munns & Tester, 2008). Several
# seaward plots read 4.00, which may be the meter's upper limit. I identified
# every vascular plant species and visually estimated its percent cover.
#
# Traits. For each species in each plot I measured nine mature individuals,
# the sample size Maitner et al. (2023) found reliable for community trait
# distributions, following Perez-Harguindeguy et al. (2013). So far I have
# measured plant height (cm), leaf thickness (mm), and leaf fresh mass (g);
# leaf area, dry mass, SLA, LDMC, succulence, and leaf salinity are still
# being processed.
#
# Analysis. Analyses at this stage are descriptive. In R (R Core Team, 2025),
# I calculated richness and Shannon diversity per plot with vegan (Oksanen et
# al., 2022) and plotted soil EC, diversity, and traits along each transect
# with ggplot2 (Wickham, 2016).


# Part 1: Figures for Results ------------------------------------------------
install.packages("readxl")
library(readxl)
library(ggplot2)

d <- read_excel("Masters_Data_Modified.xlsx")

# keep observed sites only (pp and rp are hypothetical)
d <- subset(d, site %in% c("cbp", "eps", "fds") & !is.na(species))

# fix species name typos
d$species[d$species == "Sporobulus_virginicus"] <- "Sporobolus_virginicus"
d$species[d$species == "Langularia_racemosa"]   <- "Laguncularia_racemosa"

d$site <- toupper(d$site)

# one row per plot: soil EC and species richness
plots <- unique(d[, c("site", "plot", "soil_salinity_ms")])
plots$richness <- sapply(1:nrow(plots), function(i)
  length(unique(d$species[d$site == plots$site[i] & d$plot == plots$plot[i]])))

# FDS plot 2 had no vegetation: add as zero richness
plots <- rbind(plots, data.frame(site = "FDS", plot = 2, soil_salinity_ms = NA, richness = 0))

# distance from first plot (m): 1 m plots with 1 m gaps
plots$distance_m <- (plots$plot - 1) * 2

# Figure 1: soil EC along each transect
ggplot(subset(plots, !is.na(soil_salinity_ms)),
       aes(x = distance_m, y = soil_salinity_ms, color = site)) +
  geom_point(size = 2) +
  geom_line() +
  labs(x = "Distance inland from first plot (m)",
       y = "Soil electrical conductivity (mS)",
       color = "Site",
       caption = "Figure 1. Soil electrical conductivity along shore-perpendicular transects at three Florida\ncoastal preserves. FDS plot 2 had no vegetation and is not shown.")

# Figure 2: species richness vs soil EC
ggplot(subset(plots, !is.na(soil_salinity_ms)),
       aes(x = soil_salinity_ms, y = richness, color = site)) +
  geom_point(size = 2) +
  labs(x = "Soil electrical conductivity (mS)",
       y = "Species richness (species per plot)",
       color = "Site",
       caption = "Figure 2. Vascular plant species richness per plot against soil electrical conductivity\nat three Florida coastal preserves.")

# Figure 3: individual traits vs soil EC
traits <- rbind(
  data.frame(soil_salinity_ms = d$soil_salinity_ms, site = d$site,
             trait = "Leaf thickness (mm)", value = d$leaf_thickness_mm),
  data.frame(soil_salinity_ms = d$soil_salinity_ms, site = d$site,
             trait = "Plant height (cm)", value = d$plant_height_cm))

ggplot(traits, aes(x = soil_salinity_ms, y = value, color = site)) +
  geom_jitter(width = 0.05, alpha = 0.4) +
  scale_y_log10() +
  facet_wrap(~ trait, scales = "free_y") +
  labs(x = "Soil electrical conductivity (mS)",
       y = "Trait value (log scale)",
       color = "Site",
       caption = "Figure 3. Leaf thickness and plant height of individual plants against soil electrical\nconductivity at three Florida coastal preserves (9 individuals per species per plot).")

# Numbers for the Results text

plots[order(plots$site, plots$plot), ]

# Results ------------------------------------------------------------------

# Soil salinity declined from the shoreline inland at all five preserves
# (Fig. 1). Every transect began at 4.00 mS, the highest reading recorded, and
# fell to between 0.03 and 0.06 mS at the most inland plot. How quickly
# salinity dropped differed among sites. At CBP and PP it fell below 1 mS by
# the third plot, whereas at EPS it declined gradually over ten plots.
#
# Species richness was generally highest where salinity was lowest (Fig. 2).
# Plots at or above 2 mS had a median of 3 species, compared with 8 species in
# plots below 2 mS. This pattern was strongest at EPS, where richness rose from
# 5 species in the most seaward plot to 15 in the most inland plot, and at FDS
# and RP, where richness peaked at 8 species in low-salinity plots. Richness at
# CBP and PP stayed between 3 and 4 species along the whole transect. EPS was
# also the richest site overall, with 24 species, compared with 8 to 10 at the
# other preserves.
#
# Leaf thickness and plant height both shifted along the gradient (Fig. 3). Individuals in plots at or above 2 mS had thicker leaves (median 0.80 mm)
# than those in lower-salinity plots (median 0.47 mm). Some of the thickest
# leaves belonged to succulents that grew in the most saline plots, such as
# Sesuvium portulacastrum and Suaeda sp. Plants in high-salinity plots were
# also shorter (median 13.0 cm) than those in low-salinity plots (median
# 19.0 cm), and the tallest species, including Dalbergia sp. and Sideroxylon
# foetidissimum, occurred only in the most inland plots. Trait values varied
# widely at every salinity level, however, reflecting the mix of growth forms
# within each plot.

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