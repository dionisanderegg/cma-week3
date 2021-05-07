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
HIST <- caro60 %>%
  ggplot(aes(stepMean)) +
  geom_histogram()
HIST

# Static or non static als logical if stepMean is Smaller than 5 (see HIST)
caro60 <- caro60 %>% 
  ungroup() %>%
  mutate(static = stepMean < 5)

# Filter data: keep static = FALSE
caro60_filter <- caro60 %>%
  filter(!static)

# Visualize geom patz
caro60%>%
  ggplot(aes(E, N))  +
  geom_path() +
  geom_point(aes(colour = static)) +
  coord_equal() +
  theme_bw()+
  theme(legend.position = "bottom")

# unique ID per move and static phase 
rle_id <- function(vec){
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times=x))
}

caro60 <- caro60 %>%
  mutate(segment_id = rle_id(static))

# Moving segments colored by segment ID (all)
caro60 %>%
  ggplot(aes(E, N, col = segment_id))  +
  geom_path() +
  geom_point() +
  coord_equal() +
  theme_bw()+
  theme(legend.position = "bottom")

# Moving segments colored by segment ID (segments > 5)
caro60 <- caro60%>%
  group_by(segment_id) %>%
  mutate(n = n()) 

caro60 %>%
  filter(n >=5) %>%
  ggplot(aes(E, N, col = segment_id))  +
  geom_path() +
  geom_point() +
  coord_equal() +
  theme_bw()+
  theme(legend.position = "bottom")



