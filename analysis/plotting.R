## PLOTTING

library(ggplot2)

out$data_collect %>%
  ggplot(aes(time, opinions)) +
  geom_point(size= 0.3, alpha=0.9, colour="dodgerblue3") +
  theme_bw()

out$data_collect %>%
  melt( id.vars = "time",
        measure.vars = c("no_Both", "no_Send", "no_Receive", "no_Nothing"),
	variable.name = "action",
	value.name = "no"
      ) %>%
  ggplot(aes(time, no)) +
  geom_line(aes(color=as.factor(action))) +
  theme_bw() +
  theme(legend.position="bottom")

out$data_collect %>%
  melt( id.vars = "time",
        measure.vars = c("no_Unoptimized", "no_Optimized"),
	variable.name = "action",
	value.name = "no"
      ) %>%
  ggplot(aes(time, no)) +
  geom_line(aes(color=as.factor(action))) +
  theme_bw() +
  theme(legend.position="bottom")


sim$data_collect %>%
  ggplot(aes(time, opinions)) +
  geom_point(size= 0.3, alpha=0.9, colour="dodgerblue3") +
  theme_bw()
