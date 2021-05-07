library(tidyverse)

# Datein einlesen
caro60 <- read_delim("caro60.csv", delim = ",")

# Euclicdian distance zwischen n+-3
caro60 <- caro60 %>%
  mutate(
    nMinus3 = sqrt((lag(E,3)-E)^2+(lag(N,3)-N)^2),   # distance to pos -3 minutes
    nMinus2 = sqrt((lag(E,2)-E)^2+(lag(N,2)-N)^2),   # distance to pos -2 minutes
    nMinus1 = sqrt((lag(E,1)-E)^2+(lag(N,1)-N)^2),   # distance to pos -1 minutes
    nPlus1  = sqrt((E-lead(E,1))^2+(N-lead(N,1))^2), # distance to pos +1 mintues
    nPlus2  = sqrt((E-lead(E,2))^2+(N-lead(N,2))^2),  # distance to pos +2 minutes
    nPlus3  = sqrt((E-lead(E,3))^2+(N-lead(N,3))^2)  # distance to pos +2 minutes
  )

# StepMean berechnen
caro60 <- caro60 %>%
  rowwise() %>%
  mutate(
    stepMean = mean(c(nMinus3, nMinus2, nMinus1,nPlus1,nPlus2, nPlus3))
  ) %>%
  ungroup() 

# Stepmean als Summary und Histogram:
summary(caro60$stepMean)
caro60 %>%
  ggplot(aes(stepMean)) +
  geom_histogram()

# Static or non static als logical
caro60 <- caro60 %>% 
  ungroup() %>%
  mutate(static = stepMean < mean(stepMean, na.rm = TRUE))

# Filter data: keep static = FALSE
caro60_filter <- caro60 %>%
  filter(!static)

# Visualize geom patz
caro60_filter%>%
  ggplot(aes(E, N))  +
  geom_path() +
  geom_point() +
  coord_fixed() +
  theme(legend.position = "bottom")
