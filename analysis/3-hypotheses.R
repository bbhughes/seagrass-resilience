# Plot of hypotheses

library(tidyverse)
library(ggsidekick) # devtools::install_github("seananderson/ggsidekick")

d <- read_csv("data/seagrass_mesocosm_hypotheses.csv", na = c("NA", "na"))
d <- na.omit(d) %>%
  dplyr::select(-Barrel, -Nutrients)# %>%

x <- d$pH
x_centered <- x - mean(x)
quad <- x_centered ^ 2 * -10
quad2 <- (x_centered) * 2 + (x_centered) ^ 2 * -10 - 0.1
linear <- x_centered * -2
linear2 <- x_centered * -3.5 + 0.75

scale01 <- function(x) {(x - min(x)) / (max(x) - min(x))}


gap <- 0.05
dat <- data.frame(pH = c(x, x), 
  hyp1_grazers = c(quad, quad + gap), 
  hyp2_grazers = c(quad, quad + gap), 
  hyp1_algae = c(linear, linear2), 
  hyp2_algae = c(linear, linear2), 
  hyp1_eelgrass = c(quad, quad2), 
  hyp2_eelgrass = c(linear, linear2), 
  nutrients = c(rep("No Nutrients", length(x_centered)), 
    rep("Nutrients added", length(x_centered))))

d <- dat %>% 
  gather(response, value, -pH, -nutrients)

d <- mutate(d, hypothesis = sub("(hyp[0-9]+)_([a-z]+)", "\\1", response),
  type = sub("(hyp[0-9]+)_([a-z]+)", "\\2", response))

d <- mutate(d, type = factor(type, levels = c("grazers", "algae", "eelgrass"),
  labels = c("Grazer\nefficiency", "Algal\nbiomass", "Seagrass\nbiomass")))

d <- mutate(d, hypothesis = factor(hypothesis,
  labels = c("Hypothesis 1", "Hypothesis 2")))

d <- d %>% group_by(type, hypothesis) %>% 
  mutate(value = scale01(value))

let <- unique(dplyr::select(d, hypothesis, type))
let <- as.data.frame(let)
let <- dplyr::mutate(let, letter = letters[seq_len(nrow(let))], x = rep(7.82, nrow(let)), 
  y = rep(0.9, nrow(let)), nutrients = "No Nutrients")

g <- ggplot(d, aes(pH, value, lty = nutrients)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE,
    colour = "grey50", lwd = 0.7) +
  facet_grid(type~hypothesis, switch = "y") +
  theme_sleek() +
  ylab("") +
  scale_linetype_manual(values = c(2, 1)) +
  xlab(expression(Acidification~(seawater~pH[T]))) +
  geom_text(data = let, aes(x = x, y = y, label = letter), color = "grey40") +
  theme(axis.ticks.y=element_blank(),
    axis.text.y = element_blank()) +
  theme(panel.spacing = unit(-0.1, "lines")) +
  theme(axis.title = element_text(size = rel(0.8))) +
  theme(legend.position = c(0.25, 0.12 )) +
  scale_x_reverse(breaks = c(7.2, 7.7)) +
  labs(linetype = "") +
  theme(legend.text = element_text(size = 7.7)) +
  theme(legend.key.width = unit(1.35, "line")) +
  theme(strip.background = element_blank(),
    strip.text.y = element_blank())
print(g)

ggsave("figs/fig-1.pdf", width = 3.2, height = 3.3, plot = g)

