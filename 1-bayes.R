library(tidyverse)
library(rstanarm)
# options(mc.cores = parallel::detectCores())
options(mc.cores = 1L)
dir.create("figs", showWarnings = FALSE)
theme_gg <- function(base_size = 11, base_family = "") {
  theme_light() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "grey10"),
      axis.text = element_text(colour = "grey30"),
      axis.title = element_text(colour = "grey30"),
      legend.title = element_text(colour = "grey30", size = rel(0.9)),
      panel.border = element_rect(fill = NA, colour = "grey70", size = 1),
      legend.key.size = unit(0.8, "lines"),
      legend.text = element_text(size = rel(0.7), colour = "grey30"),
      legend.key = element_rect(colour = NA)
    )
}

d <- read_csv("data/Mesocosm_ExperimentalData_R.csv", na = c("NA", "na"))
d <- mutate(d, id = as.character(pH))
names(d) <- tolower(names(d))

# ggplot(d, aes(ph, epiphyte_cm_shoot, colour = nutrients)) + geom_point()

d$ph_scaled <- arm::rescale(d$ph)
d$nutrients_scaled <- arm::rescale(d$nutrients)
d$nutrients <- as.numeric(as.factor(d$nutrients)) - 1

d$ulva_grazing <- d$ulva_grazing + 0.01 # TODO fix

d_log <- select(d, 
  ph,
  ph_scaled,
  nutrients,
  epiphyte_cm_shoot,
  algal_mass_g_dw,
  rhi_mass_g_dw_mean,
  change_idotea_mass_g_fw,
  change_seahare_mass_g_fw,
  sht_g_dw_mean,
  rhi_mass_g_dw_mean,
  rhi_elong_cm,
  ido_mortality,
  sh_grazing,
  ulva_grazing,
  dead_shoots,
  sh_mortality)

dt <- tribble(
  ~response,                  ~log,  ~family,
  "ph_scaled",                NA,    "Gaussian",
  "nutrients",                NA,    "Gaussian",
  "algal_mass_g_dw",          TRUE,  "Gaussian",
  "change_idotea_mass_g_fw",  FALSE, "Gaussian",
  "change_seahare_mass_g_fw", FALSE, "Gaussian",
  "dead_shoots",              FALSE, "Poisson",
  "epiphyte_cm_shoot",        TRUE,  "Gaussian",
  "ido_mortality",            TRUE,  "Gaussian",
  "rhi_elong_cm",             TRUE,  "Gaussian",
  "rhi_mass_g_dw_mean",       TRUE,  "Gaussian",
  "rhi_mass_g_dw_mean",       TRUE,  "Gaussian",
  "sh_grazing",               TRUE,  "Gaussian",
  "sh_mortality",             FALSE, "Poisson",
  "sht_g_dw_mean",            TRUE,  "Gaussian",
  "ulva_grazing",             TRUE,  "Gaussian")

d_long <- gather(d_log, response, value, -ph_scaled, -nutrients, -ph)
d_long <- inner_join(d_long, dt) %>% na.omit() %>% as_data_frame()

ggplot(d_long, aes(ph_scaled, value, colour = as.factor(nutrients))) + 
  geom_point() +
  facet_wrap(~response, scales = "free")

fits <- plyr::dlply(d_long, "response", function(x) {
  if (x$log[1])
    f <- log(value) ~ poly(ph_scaled, 2) + nutrients
  else
    f <- value ~ poly(ph_scaled, 2) + nutrients
  
  if (x$family[1] == "Gaussian")
    fam <- gaussian(link = "identity")
  if (x$family[1] == "Poisson")
    fam <- neg_binomial_2(link = "log")
  # if (x$family[1] == "Poisson")
  #   fam <- poisson(link = "log")
    
  stan_glm(f, data = x, iter = 2000, chains = 4, family = fam,
    prior = normal(0, 1),
    prior_intercept = normal(0, 20),
    prior_aux = student_t(df = 3, 0, 2))
})

s <- plyr::ldply(fits, function(x) {
  mm <- as.matrix(x)
  as_data_frame(mm[, 2:4])
  }) %>% as_data_frame() %>%
  rename(linear = `poly(ph_scaled, 2)1`, quadratic = `poly(ph_scaled, 2)2`)

g <- gather(s, parameter, sample, -response) %>%
  inner_join(unique(select(d_long, response, log, family))) %>%
  mutate(multiplicative = ifelse(log | family == "Poisson", TRUE, FALSE)) %>%
  mutate(parameter = factor(parameter, levels = c("linear", "quadratic", "nutrients"))) %>%
  ggplot(aes(response, sample)) + 
  geom_hline(yintercept = 0, lty = 2, col = "grey60") +
  geom_violin(fill = "grey80", col = "grey70") +
  coord_flip() +
  facet_wrap(~parameter, scales = "free_x") +
  scale_y_continuous(limits = c(-3, 3)) +
  theme_gg() +
  ylab("Coefficient")
# g
ggsave("figs/gg-pars.pdf", width = 8, height = 4.5)

# TODO:
# newdata <- expand.grid(ph_scaled = seq(min(d$ph_scaled), max(d$ph_scaled), length.out = 200), 
#   nutrients = c(0, 1))
# newdata <- data.frame(newdata, linear = poly(newdata$ph_scaled, 2)[,1], 
  # quad = poly(newdata$ph_scaled, 2)[,2], ph = newdata$ph_scaled * 2 * sd(d$ph) + mean(d$ph))

newdata <- unique(select(d, ph_scaled, ph, nutrients))

newdata <- data.frame(newdata, linear = poly(newdata$ph_scaled, 2)[,1], 
  quad = poly(newdata$ph_scaled, 2)[,2])

newdata <- group_by(newdata, nutrients) %>%
  do({
    data.frame(ph_scaled = approx(.$ph_scaled, n = 200)$y,
      ph = approx(.$ph, n = 200)$y,
      linear = approx(.$linear, n = 200)$y,
      quad = spline(.$quad, n = 200)$y) # TODO
  })

p <- plyr::ldply(fits, function(x) {
  mm <- as.matrix(x)
  b <- as_data_frame(mm) %>% rename(
    b0 = `(Intercept)`,
    b1 = `poly(ph_scaled, 2)1`,
    b2 = `poly(ph_scaled, 2)2`)
  
  pp <- apply(newdata, 1, function(y) {
    b$b0 + b$b1 * y["linear"] + b$b2 * y["quad"] + b$nutrients * y["nutrients"]
  })
  
  l <- apply(pp, 2, quantile, probs = 0.1)
  u <- apply(pp, 2, quantile, probs = 0.9)
  med <- apply(pp, 2, quantile, probs = 0.5)
  data.frame(l, med, u, newdata)
}) %>% as_data_frame()

pp <- inner_join(p, unique(select(d_long, response, log, family)))
pp <- pp %>% group_by(response) %>% 
  mutate(med = ifelse(log | family == "Poisson", exp(med), med)) %>% 
  mutate(l = ifelse(log | family == "Poisson", exp(l), l)) %>% 
  mutate(u = ifelse(log | family == "Poisson", exp(u), u))

g <- ggplot(d_long, aes(ph, value, colour = as.factor(nutrients))) + 
  geom_point() +
  facet_wrap(~response, scales = "free_y") +
  geom_line(data = pp, aes(ph, y = med), lwd = 1) +
  geom_ribbon(data = pp, aes(ph, ymax = u, ymin = l, fill = as.factor(nutrients)), 
    alpha = 0.15, inherit.aes = FALSE) +
  theme_gg() +
  labs(colour = "Nutrients", fill = "Nutrients")
  # scale_color_brewer(palette = "Set2") +
  # scale_fill_brewer(palette = "Set2")
# g
ggsave("figs/gg-fits.pdf", width = 11, height = 7)
