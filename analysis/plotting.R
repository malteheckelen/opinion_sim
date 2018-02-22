## PLOTTING

library(ggplot2)

out$data_collect %>%
  ggplot(aes(time, opinions)) +
  geom_point(size= 0.02, alpha=0.2, colour="dodgerblue3") +
  theme_bw()

out$data_collect %>%
  .[ , no := .N, by = c("chosen_sending_action", "time")] %>%
  ggplot(aes(time, no)) +
  geom_line(aes(color=as.factor(chosen_sending_action))) +
  theme_bw() +
  theme(legend.position="bottom")

out$data_collect %>%
  .[ , no := .N, by = c("chosen_overall_action", "time")] %>%
  ggplot(aes(time, no)) +
  geom_line(aes(color=as.factor(chosen_overall_action))) +
  theme_bw() +
  theme(legend.position="bottom")

