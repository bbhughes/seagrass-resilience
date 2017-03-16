library(tidyverse)
library(RColorBrewer)
library(ggsidekick) # devtools::install_github("seananderson/ggsidekick")


d <- read.csv("data/properties.csv", stringsAsFactors = FALSE, strip.white = TRUE)

d <- mutate(d, type = rep(c("mean", "sd"), 14))

names(d) <- gsub("\\.", "_", names(d))

d <- as_data_frame(d)

d2 <- reshape2::melt(d, 
  id.vars = c("pH_Treatment", "Durafet_pHT", "type", "Nutrients")) %>% 
  as_data_frame()

m <- filter(d2, type == "mean")
s <- filter(d2, type == "sd")

m$type <- NULL
s$type <- NULL

m <- rename(m, mean = value)
s <- rename(s, sd = value)

d <- inner_join(m, s)

d <- mutate(d, ph = as.numeric(sub("([0-9\\.]+) \\([0-9\\.]+\\)", "\\1", 
  Durafet_pHT)))

cols <- c(brewer.pal(4, "Greys")[[3]], brewer.pal(4, "Blues")[[3]])

d <- mutate(d, nutrients_text = 
    ifelse(Nutrients == 0, "No Nutrients", "Nutrients added"))

# limits <- tribble(
  # ~variable,                  ~log_transform,  ~family,     ~link,
  # "Spec_pHT",          TRUE,            "gaussian",  "identity",
  # "NO3",  FALSE,           "gaussian",  "identity",
  # "PO4",   FALSE,           "gaussian",  "identity",
  # "TA"

# filter(d, variable != "Spec_pHT") %>%
filter(d) %>%
  ggplot(aes(ph, mean, fill = as.factor(nutrients_text))) + 
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.3) +
  geom_line(aes(color = as.factor(nutrients_text))) +
  facet_wrap(~variable, scales = "free_y", ncol = 2) +
  theme_sleek() +
  labs(colour = "Nutrients", fill = "Nutrients", y = "Value", 
    x = expression(Acidification~(mean~seawater~pH[T]))) +
  scale_color_manual(values = cols) + 
  scale_fill_manual(values = cols) +
  theme(legend.position = c(0.68, 0.11)) +
  # guides(fill = guide_legend(override.aes = list(fill = NA))) +
  scale_x_reverse() 

ggsave("figs/properties.pdf", height = 5.3, width = 5)
