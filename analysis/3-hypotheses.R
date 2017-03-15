# Plot of hypotheses 

library(tidyverse)
library(ggsidekick) # devtools::install_github("seananderson/ggsidekick")
# install("~/src/ggsidekick/")

d <- read_csv("data/seagrass_mesocosm_hypotheses.csv", na = c("NA", "na"))
d <- na.omit(d) %>% 
  select(-Barrel, -Nutrients) %>% 
  gather(response, value, -pH)

d <- mutate(d, hypothesis = sub("(hyp[0-9]+)_([a-z]+)", "\\1", response),
  type = sub("(hyp[0-9]+)_([a-z]+)", "\\2", response))

d <- mutate(d, type = factor(type, levels = c("grazers", "algae", "eelgrass"),
  labels = c("Grazer\nefficiency", "Algal\nbiomass", "Seagrass\nbiomass")))
d <- mutate(d, hypothesis = factor(hypothesis, 
  labels = c("Hypothesis 1", "Hypothesis 2", "Hypothesis 3")))

let <- unique(select(d, hypothesis, type))
let <- mutate(let, letter = LETTERS[seq_len(n())], x = 7.8, y = 4.75)

g <- ggplot(d, aes(pH, value)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, 
    colour = "grey50", lwd = 0.7) +
  facet_grid(type~hypothesis, switch = "y") +
  theme_sleek() +
  xlab(expression(pH[T])) + ylab("") +
  geom_text(data = let, aes(x = x, y = y, label = letter), color = "grey40") +
  theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
    axis.text.y = element_blank(), axis.text.x = element_blank()) +
  theme(panel.spacing = unit(-0.1, "lines")) +
  theme(axis.title = element_text(size = rel(0.8))) +
  scale_x_reverse()

print(g)

ggsave("figs/fig-1.pdf", width = 4, height = 2.8)

