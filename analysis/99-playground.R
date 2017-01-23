mesocosm <- readr::read_csv("data/Mesocosm_ExperimentalData_R.csv", na = c("NA", "na"))
library(tidyverse)
d <- mesocosm

#Epiphyte loading####
glimpse(mesocosm)

ggplot(d, aes(pH, Epiphyte_cm_shoot, colour = Nutrients)) + geom_point()

d <- mutate(d, id = as.character(pH))

library(lme4)

library(rstanarm)
options(mc.cores = parallel::detectCores())

d$pH_scaled <- arm::rescale(d$pH)
d$Nutrients_scaled <- arm::rescale(d$Nutrients)


names(d) <- tolower(names(d))

m <- stan_glm(log(epiphyte_cm_shoot) ~ poly(ph_scaled, 2) + nutrients, data = d)
plot(m)

m2 <- stan_glm(log(algal_mass_g_dw) ~ poly(ph_scaled, 2) + nutrients, data = d)
plot(m2)
bayesplot::mcmc_areas(as.array(m2))

m3 <- stan_glm(dead_shoots ~ poly(ph_scaled, 2) + nutrients, data = d, family = poisson(link = "log"))
plot(m3)
bayesplot::mcmc_areas(as.array(m3))

m4 <- stan_glm(log(rhi_mass_g_dw_mean) ~ poly(ph_scaled, 2) + nutrients, data = d)
plot(m4)
bayesplot::mcmc_areas(as.array(m4))

m5 <- stan_glm(change_idotea_mass_g_fw ~ poly(ph_scaled, 2) + nutrients, data = d)
bayesplot::mcmc_areas(as.array(m5))

m6 <- stan_glm(change_seahare_mass_g_fw ~ poly(ph_scaled, 2) + nutrients, data = d)
bayesplot::mcmc_areas(as.array(m6))


m1 <- lmer(log(Epiphyte_cm_shoot) ~ pH + Nutrients + (1 | id), data = d)

m1 <- lm(log(Epiphyte_cm_shoot) ~ pH + Nutrients + (1 | id), data = d)

m2 <- lm(log(Epiphyte_cm_shoot) ~ pH + Nutrients, data = d)
m3 <- lm(log(Epiphyte_cm_shoot) ~ pH * Nutrients, data = d)

e <- list()
e[[1]] <- glm(Epiphyte_cm_shoot ~ pH * Nutrients, mesocosm, family = Gamma(link = "log"))
e[[2]] <- update(e[[1]], . ~ . - pH:Nutrients)
e[[3]] <- update(e[[1]], . ~ . - pH:Nutrients - pH)

# MuMIn::dredge(glm(Epiphyte_cm_shoot ~ pH * Nutrients, data = mesocosm, family = Gamma(link = "log"), na.action = "na.fail"))

m <- MuMIn::dredge(glm(Epiphyte_cm_shoot ~ pH * Nutrients + I(pH^2) * Nutrients, 
  data = mesocosm, family = Gamma(link = "log"), na.action = "na.fail"),
  subset = dc(pH, I(pH^2)))
subset(m, delta < 4)

mm <- MuMIn::model.avg(m, subset = delta < 4)




epi_linear <- glm(Epiphyte_cm_shoot ~ pH * Nutrients, mesocosm, family = Gamma(link = "log"))
arm::display(epi_linear)
arm::coefplot(epi_linear)

library(mgcv)
epi_gam <- gam(Epiphyte_cm_shoot ~ s(pH, k = 4) + Nutrients, mesocosm, family = Gamma(link = "log"))
plot(epi_gam)
summary(epi_gam)
epi_mum <- dredge(Epiphyte_cm_shoot ~ s(pH, k = 4) + Nutrients, mesocosm, family = Gamma(link = "log"))