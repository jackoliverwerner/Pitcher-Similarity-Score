################
# Read in data #
################

library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)

#setwd("C:/Users/jack.werner1/Documents/BB")
setwd("/Users/jackwerner/Documents/My Stuff/Baseball/Data")

handedness <- "L"
infile <- paste0("/Users/jackwerner/Documents/My Stuff/Baseball/Pitcher Similarity Score/Pitcher_Similarities_",
                 handedness, "H.csv")

sims <- read.csv(infile)

# Read pitch data
pitch.pre <- read.csv(file = "pitch_data_2017.csv")

# Read player names/IDs
# https://github.com/chadwickbureau/register/blob/master/data/people.csv
pitcher_names <- read.csv("people.csv") %>%
  filter(!is.na(key_mlbam)) %>%
  mutate(name = paste0(name_first, " ", name_last), id = key_mlbam) %>%
  select(name, id)

####################
# Pre-process data #
####################

# Simple pitch classifications
simplePitches <- data.frame(pitch_type = sort(as.character(unique(pitch.pre$pitch_type))),
                            simple_pitch_type = c("UN", "UN",  "CH", "CU", "CH", "FC", "FF", 
                                                  "PO", "SI", "FT", "UN", "CU", "KN", "PO",
                                                  "UN", "SI", "SL", "UN"),
                            fastball = c("UN", "UN", "O", "O", "O", "F", "F", "O", "F",
                                         "F", "UN", "O", "O", "O", "UN", "F", "O", "UN")
)

pitch <- pitch.pre %>% filter() %>%
  left_join(pitcher_names, by = c("pitcher" = "id")) %>%
  left_join(simplePitches, by = "pitch_type") %>%
  group_by(pitcher) %>%
  filter(n() > 1100) %>% 
  ungroup()

# Summarize into pitcher-pitch type dataset
pitchers <- pitch %>% 
  filter(fastball != "UN", !(simple_pitch_type %in% c("UN", "PO"))) %>%
  group_by(pitcher, name) %>% mutate(total = n()) %>% ungroup() %>%
  group_by(pitcher, name, simple_pitch_type) %>% filter(n() > (.05*total)) %>% ungroup() %>%
  group_by(pitcher, name) %>%
  summarize(Hand = first(p_throws),
            # Percentages
            FF_PC = sum(simple_pitch_type == "FF")/n(),
            FT_PC = sum(simple_pitch_type == "FT")/n(),
            FC_PC = sum(simple_pitch_type == "FC")/n(),
            CH_PC = sum(simple_pitch_type == "CH")/n(),
            CU_PC = sum(simple_pitch_type == "CU")/n(),
            SI_PC = sum(simple_pitch_type == "SI")/n(),
            KN_PC = sum(simple_pitch_type == "KN")/n(),
            SL_PC = sum(simple_pitch_type == "SL")/n(),
            # Average speeds
            FF_SP = sum((simple_pitch_type == "FF")*start_speed)/sum(simple_pitch_type == "FF"),
            FT_SP = sum((simple_pitch_type == "FT")*start_speed)/sum(simple_pitch_type == "FT"),
            FC_SP = sum((simple_pitch_type == "FC")*start_speed)/sum(simple_pitch_type == "FC"),
            CH_SP = sum((simple_pitch_type == "CH")*start_speed)/sum(simple_pitch_type == "CH"),
            CU_SP = sum((simple_pitch_type == "CU")*start_speed)/sum(simple_pitch_type == "CU"),
            SI_SP = sum((simple_pitch_type == "SI")*start_speed)/sum(simple_pitch_type == "SI"),
            KN_SP = sum((simple_pitch_type == "KN")*start_speed)/sum(simple_pitch_type == "KN"),
            SL_SP = sum((simple_pitch_type == "SL")*start_speed)/sum(simple_pitch_type == "SL"),
            # Average horizontal break
            FF_PX = sum((simple_pitch_type == "FF")*pfx_x)/sum(simple_pitch_type == "FF"),
            FT_PX = sum((simple_pitch_type == "FT")*pfx_x)/sum(simple_pitch_type == "FT"),
            FC_PX = sum((simple_pitch_type == "FC")*pfx_x)/sum(simple_pitch_type == "FC"),
            CH_PX = sum((simple_pitch_type == "CH")*pfx_x)/sum(simple_pitch_type == "CH"),
            CU_PX = sum((simple_pitch_type == "CU")*pfx_x)/sum(simple_pitch_type == "CU"),
            SI_PX = sum((simple_pitch_type == "SI")*pfx_x)/sum(simple_pitch_type == "SI"),
            KN_PX = sum((simple_pitch_type == "KN")*pfx_x)/sum(simple_pitch_type == "KN"),
            SL_PX = sum((simple_pitch_type == "SL")*pfx_x)/sum(simple_pitch_type == "SL"),
            # Average vertical break
            FF_PZ = sum((simple_pitch_type == "FF")*pfx_z)/sum(simple_pitch_type == "FF"),
            FT_PZ = sum((simple_pitch_type == "FT")*pfx_z)/sum(simple_pitch_type == "FT"),
            FC_PZ = sum((simple_pitch_type == "FC")*pfx_z)/sum(simple_pitch_type == "FC"),
            CH_PZ = sum((simple_pitch_type == "CH")*pfx_z)/sum(simple_pitch_type == "CH"),
            CU_PZ = sum((simple_pitch_type == "CU")*pfx_z)/sum(simple_pitch_type == "CU"),
            SI_PZ = sum((simple_pitch_type == "SI")*pfx_z)/sum(simple_pitch_type == "SI"),
            KN_PZ = sum((simple_pitch_type == "KN")*pfx_z)/sum(simple_pitch_type == "KN"),
            SL_PZ = sum((simple_pitch_type == "SL")*pfx_z)/sum(simple_pitch_type == "SL"),
            # Totals
            FF_N = sum(simple_pitch_type == "FF"),
            FT_N = sum(simple_pitch_type == "FT"),
            FC_N = sum(simple_pitch_type == "FC"),
            CH_N = sum(simple_pitch_type == "CH"),
            CU_N = sum(simple_pitch_type == "CU"),
            SI_N = sum(simple_pitch_type == "SI"),
            KN_N = sum(simple_pitch_type == "KN"),
            SL_N = sum(simple_pitch_type == "SL")
  ) %>%
  ungroup()

pitchers$total <- apply(as.matrix(pitchers[,36:43]), 1, sum) # Total number of pitches

##############
# Clustering #
##############

# Create distance matrix
dist.mat <- matrix(sims$similarity, 
                   nrow = length(unique(sims$pitcher1)), 
                   ncol = length(unique(sims$pitcher1)))
colnames(dist.mat) <- unique(sims$pitcher1)
rownames(dist.mat) <- unique(sims$pitcher1)

# Hierarchical clustering
clusters <- hclust(as.dist(dist.mat), method = "complete")
plot(clusters)

# Interpret clusters #
class <- cutree(clusters, 4)

clusters.df <- data.frame(name = unique(sims$pitcher1), cluster = class)

# Averages by cluster
pc.nonzero <- function(x) {
  return(sum(x != 0 & !is.na(x))/length(x))
}

pitcher.clusters <- pitchers %>% filter(Hand == handedness) %>%
  left_join(clusters.df, by = "name") %>%
  group_by(cluster) %>%
  mutate(npitchers = n()) %>%
  summarize_each(funs(mean(., na.rm = T), pc.nonzero), 
                 npitchers, contains("_PC"), contains("_SP")) %>%
  select(-contains("SP_pc.nonzero"))

View(pitcher.clusters)


############################
# Multidimensional Scaling #
############################

mds <- cmdscale(dist.mat, 2)

cats <- cutree(clusters, 3)

mds.df <- data.frame(name = rownames(mds), xpos = mds[,1], ypos = mds[,2],
                     cluster = cats)

ggplot(data = mds.df, aes(x = xpos, y = ypos, label = name, color = factor(cluster))) + geom_text()

dist.mat.mds <- matrix(0, nrow = length(unique(sims$pitcher1)),
                       ncol = length(unique(sims$pitcher1)))

for (i in 1:length(unique(sims$pitcher1))) {
  for (j in 1:length(unique(sims$pitcher1))) {
    dist.mat.mds[i,j] <- sqrt(sum((mds[i,]-mds[j,])^2))
  }
}


compare.df <- data.frame(p1 = unique(sims$pitcher1), 
                         p2 = rep(unique(sims$pitcher1), each = length(unique(sims$pitcher1))),
                         actual = as.vector(dist.mat), mds = as.vector(dist.mat.mds))


ggplot(data = compare.df, aes(x = actual, y = mds)) + geom_point()

x <- unique(compare.df$actual)
y <- unique(compare.df$mds)

cor(x, y)^2




