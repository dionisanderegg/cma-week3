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

# Import pedestrian.csv
pedestrian <- read_delim("pedestrian.csv", delim = ",")

# Eplore data
ggplot(pedestrian, aes(N,E, col = as.factor(TrajID)))+
  geom_point() +
  geom_path() +
  facet_wrap(~TrajID) +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(pedestrian, aes(N,E, col = as.factor(TrajID)))+
  geom_point() +
  geom_path() +
  theme_bw() +
  theme(legend.position = "bottom")

# Use package SimilarityMeasures to analyse the similatity of the trajectories
library(SimilarityMeasures)

# help(package = "SimilarityMeasures")

# Create matrix for every trajectory without time
pedestrian.1.m <- as.matrix(filter(pedestrian[,1:3], TrajID == 1))
pedestrian.2.m <- as.matrix(filter(pedestrian[,1:3], TrajID == 2))
pedestrian.3.m <- as.matrix(filter(pedestrian[,1:3], TrajID == 3))
pedestrian.4.m <- as.matrix(filter(pedestrian[,1:3], TrajID == 4))
pedestrian.5.m <- as.matrix(filter(pedestrian[,1:3], TrajID == 5))
pedestrian.6.m <- as.matrix(filter(pedestrian[,1:3], TrajID == 6))

# Calculate the Measures and store the results in a DF
Sililarity <- data.frame(ID = seq(1:6), 
                         DTW = c(DTW(pedestrian.1.m, pedestrian.1.m),
                                 DTW(pedestrian.1.m, pedestrian.2.m),
                                 DTW(pedestrian.1.m, pedestrian.3.m),
                                 DTW(pedestrian.1.m, pedestrian.4.m),
                                 DTW(pedestrian.1.m, pedestrian.5.m),
                                 DTW(pedestrian.1.m, pedestrian.6.m)),
                         EditDist = c(EditDist(pedestrian.1.m, pedestrian.1.m),
                                      EditDist(pedestrian.1.m, pedestrian.2.m),
                                      EditDist(pedestrian.1.m, pedestrian.3.m),
                                      EditDist(pedestrian.1.m, pedestrian.4.m),
                                      EditDist(pedestrian.1.m, pedestrian.5.m),
                                      EditDist(pedestrian.1.m, pedestrian.6.m)),
                         Frechet = c(Frechet(pedestrian.1.m, pedestrian.1.m),
                                     Frechet(pedestrian.1.m, pedestrian.2.m),
                                     Frechet(pedestrian.1.m, pedestrian.3.m),
                                     Frechet(pedestrian.1.m, pedestrian.4.m),
                                     Frechet(pedestrian.1.m, pedestrian.5.m),
                                     Frechet(pedestrian.1.m, pedestrian.6.m)),
                         LCSS = c(LCSS(pedestrian.1.m, pedestrian.1.m, pointDistance = 20, pointSpacing = 0, errorMarg = 1),
                                  LCSS(pedestrian.1.m, pedestrian.2.m, pointDistance = 20, pointSpacing = 0, errorMarg = 1),
                                  LCSS(pedestrian.1.m, pedestrian.3.m, pointDistance = 20, pointSpacing = 0, errorMarg = 1),
                                  LCSS(pedestrian.1.m, pedestrian.4.m, pointDistance = 20, pointSpacing = 0, errorMarg = 1),
                                  LCSS(pedestrian.1.m, pedestrian.5.m, pointDistance = 20, pointSpacing = 0, errorMarg = 1),
                                  LCSS(pedestrian.1.m, pedestrian.6.m, pointDistance = 20, pointSpacing = 0, errorMarg = 1)))
# Question @NILS : See warning message when executing lines above. Warning message:
#    In Frechet(pedestrian.1.m, pedestrian.3.m) :
#     The Frechet distance was unable to be found
# What could be the reason for this error? Result is -1 

# Long-Format for easyer plotting
Sililarity_long <- gather(Sililarity, Measure, Value, DTW : LCSS)

# Plot results
ggplot(Sililarity_long, aes(as.factor(ID), Value, fill = as.factor(ID))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Measure, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Computed similarities using different measures \nbetween trajectory 1 and all the others",
       y = "Value\n", x = "\nComparison trajectory")

# ATTENTION: LCSS strongly depends on pointDistance and errorMarg!
#  Different caluclation times
#  Different RESULTS!