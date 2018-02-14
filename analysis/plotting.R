## PLOTTING

library(ggplot2)
out$data_collect %>%
  as_tibble() %>%
  ggplot(aes(time, opinions)) +
  geom_point(size= 0.02, alpha=0.2, colour="dodgerblue3") +
  theme_bw()

plot(density(out$data_collect[ time==0 , ]))