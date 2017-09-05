################
# Read in data #
################

library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)

#setwd("C:/Users/jack.werner1/Documents/BB")
setwd("/Users/jackwerner/Documents/My Stuff/Baseball/Data")

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

# Transform to long
g_px <- pitchers %>%
  select(pitcher, name, Hand, contains("_PX")) %>%
  gather(key = pitch, value = px, contains("_PX")) %>%
  mutate(pitch = substr(pitch, 1, 2))

g_pz <- pitchers %>%
  select(pitcher, name, Hand, contains("_PZ")) %>%
  gather(key = pitch, value = pz, contains("_PZ")) %>%
  mutate(pitch = substr(pitch, 1, 2))

g_sp <- pitchers %>%
  select(pitcher, name, Hand, contains("_SP")) %>%
  gather(key = pitch, value = speed, contains("_SP")) %>%
  mutate(pitch = substr(pitch, 1, 2))

g_pc <- pitchers %>%
  select(pitcher, name, Hand, contains("_PC")) %>%
  gather(key = pitch, value = percent, contains("_PC")) %>%
  mutate(pitch = substr(pitch, 1, 2))

g_n <- pitchers %>%
  select(pitcher, name, Hand, contains("_N")) %>%
  gather(key = pitch, value = num, contains("_N")) %>%
  mutate(pitch = substr(pitch, 1, 2))

pitchers_long <- left_join(g_px, g_pz, by = c("pitcher", "name", "Hand", "pitch")) %>%
  left_join(g_sp, by = c("pitcher", "name", "Hand", "pitch")) %>%
  left_join(g_pc, by = c("pitcher", "name", "Hand", "pitch")) %>%
  left_join(g_n, by = c("pitcher", "name", "Hand", "pitch")) %>%
  filter(num >= 25) %>%
  mutate(px_scaled = as.vector(scale(px)), pz_scaled = as.vector(scale(pz)),
         speed_scaled = as.vector(scale(speed)))


# Plot pitch breaks
ggplot(data = pitchers_long, aes(x = px, y = pz, color = pitch)) +
  facet_wrap(~Hand) + geom_point()


######################
# Similarity scoring #
######################

##### Pitch similarity #####

handedness <- "R"

pitchers_hand <- filter(pitchers_long, Hand == handedness, 
                   !(name %in% c("R.A. Dickey", "Steven Wright")),
                   !is.na(name))

pitch.sims <- matrix(0, nrow(pitchers_hand), nrow(pitchers_hand))

# Calculate pitch similarities
for (i in 1:nrow(pitchers_hand)) {
  sqd.px <- (pitchers_hand$px_scaled[i] - pitchers_hand$px_scaled)^2
  sqd.pz <- (pitchers_hand$pz_scaled[i] - pitchers_hand$pz_scaled)^2
  sqd.speed <- (pitchers_hand$speed_scaled[i] - pitchers_hand$speed_scaled)^2
  euc.dist <- sqrt(sqd.px + sqd.pz + sqd.speed)
  
  pitch.sims[,i] <- euc.dist
  
}

# Are pitches most similar to other pitches of the same type?
sim.ff <- data.frame(diff = pitch.sims[,which(pitchers_hand$pitch == "FF")[1]], type = pitchers_hand$pitch)
sim.cu <- data.frame(diff = pitch.sims[,which(pitchers_hand$pitch == "CU")[1]], type = pitchers_hand$pitch)
sim.sl <- data.frame(diff = pitch.sims[,which(pitchers_hand$pitch == "SL")[1]], type = pitchers_hand$pitch)

ggplot(data = sim.cu, aes(x = type, y = diff)) + geom_boxplot()

pitchSims.df <- data.frame(sim = as.vector(pitch.sims), 
                      pitcher1 = pitchers_hand$name, 
                      pitch1 = pitchers_hand$pitch,
                      percent1 = pitchers_hand$percent,
                      
                      pitcher2 = rep(pitchers_hand$name, each = nrow(pitchers_hand)),
                      pitch2 = rep(pitchers_hand$pitch, each = nrow(pitchers_hand)),
                      percent2 = rep(pitchers_hand$percent, each = nrow(pitchers_hand)))


##### Pitcher similarity #####

# Function to calculate pitcher similarity
pitcherSimilarity <- function(p1, p2, df = pitchSims.df) {
  
  # Pitch ordering, keeping similar pitches close
  pitch.order <- c("CH", "SI", "FT", "FF", "FC", "SL", "CU")
  
  # Take just pitchers of interest
  df.sub <- df %>% filter(pitcher1 == p1, pitcher2 == p2) %>%
    mutate(pitch1 = factor(pitch1, levels = pitch.order),
           pitch2 = factor(pitch2, levels = pitch.order)) %>%
    arrange(pitch1, pitch2)
  
  # Get cumulative pitch frequencies for p1
  p1.df <- df.sub %>% filter(pitcher1 == p1) %>%
    select(pitcher1, pitch1, percent1) %>%
    unique() %>%
    mutate(cume.percent1 = round(cumsum(percent1), digits = 5)) %>%
    select(pitch1, cume.percent1)
  
  # Get cumulative pitch frequencies for p2
  p2.df <- df.sub %>% filter(pitcher2 == p2) %>%
    select(pitcher2, pitch2, percent2) %>%
    unique() %>%
    mutate(cume.percent2 = round(cumsum(percent2), digits = 5)) %>%
    select(pitch2, cume.percent2)
  
  # Create data frame of all bar plot break points, corresponding pitch similarities
  bound.df <- data.frame(pc.break = c(p1.df$cume.percent1, p2.df$cume.percent2)) %>%
    unique() %>% arrange(pc.break) %>%
    left_join(p1.df, by = c("pc.break" = "cume.percent1")) %>%
    left_join(p2.df, by = c("pc.break" = "cume.percent2"))
  
  for (i in (nrow(bound.df) - 1):1) {
    if (is.na(bound.df$pitch1[i])) {
      bound.df$pitch1[i] <- bound.df$pitch1[i+1]
    }
    
    if (is.na(bound.df$pitch2[i])) {
      bound.df$pitch2[i] <- bound.df$pitch2[i+1]
    }
  }
    
  sims.sub.df <- df.sub %>% select(pitch1, pitch2, sim)
  
  bound.df <- bound.df %>%
    left_join(sims.sub.df, by = c("pitch1", "pitch2")) %>%
    mutate(pc.diff = pc.break - lag(pc.break, default = 0))
  
  # Calculate similarity
  similarity <- weighted.mean(bound.df$sim, bound.df$pc.diff)
  
  return(similarity)
  
}

# Function for comparison plot of pitcher similarity
plotSimilarity <- function(p1, p2, df = pitchSims.df) {
  
  # Pitch ordering, keeping similar pitches close
  pitch.order <- c("CH", "SI", "FT", "FF", "FC", "SL", "CU")
  
  # Just pitchers of interest
  df.sub <- df %>% filter(pitcher1 == p1, pitcher2 == p2) %>%
    mutate(pitch1 = factor(pitch1, levels = pitch.order),
           pitch2 = factor(pitch2, levels = pitch.order)) %>%
    arrange(pitch1, pitch2)
  
  # Pitcher 1, pitch frequencies
  p1.df.a <- df.sub %>% filter(pitcher1 == p1) %>%
    select(pitcher = pitcher1, pitch = pitch1, percent = percent1) %>%
    unique()
  
  # Pitcher 2, pitch frequencies
  p2.df.a <- df.sub %>% filter(pitcher2 == p2) %>%
    select(pitcher = pitcher2, pitch = pitch2, percent = percent2) %>%
    unique()
  
  # Combined pitch frequency data frame
  p.df <- rbind(p1.df.a, p2.df.a)
  
  # Pitcher 1, cumulative pitch frequencies
  p1.df <- df.sub %>% filter(pitcher1 == p1) %>%
    select(pitcher1, pitch1, percent1) %>%
    unique() %>%
    mutate(cume.percent1 = round(cumsum(percent1), digits = 5)) %>%
    select(pitch1, cume.percent1)
  
  # Pitcher 2, cumulative pitch frequencies
  p2.df <- df.sub %>% filter(pitcher2 == p2) %>%
    select(pitcher2, pitch2, percent2) %>%
    unique() %>%
    mutate(cume.percent2 = round(cumsum(percent2), digits = 5)) %>%
    select(pitch2, cume.percent2)
  
  # Create data frame of all bar plot break points, corresponding pitch similarities
  bound.df <- data.frame(pc.break = c(p1.df$cume.percent1, p2.df$cume.percent2)) %>%
    unique() %>% arrange(pc.break) %>%
    left_join(p1.df, by = c("pc.break" = "cume.percent1")) %>%
    left_join(p2.df, by = c("pc.break" = "cume.percent2"))
  
  for (i in (nrow(bound.df) - 1):1) {
    if (is.na(bound.df$pitch1[i])) {
      bound.df$pitch1[i] <- bound.df$pitch1[i+1]
    }
    
    if (is.na(bound.df$pitch2[i])) {
      bound.df$pitch2[i] <- bound.df$pitch2[i+1]
    }
  }
  
  # Data frame of all pitch similarities between tow pitchers of interest
  sims.sub.df <- df.sub %>% select(pitch1, pitch2, sim)
  
  # Fold pitch similarities into dataset
  bound.df.2 <- bound.df %>%
    left_join(sims.sub.df, by = c("pitch1", "pitch2")) %>%
    mutate(pc.diff = pc.break - lag(pc.break, default = 0),
           comparison = factor(paste(pitch1, pitch2),
                               levels = paste(pitch1, pitch2)))
  
  # Data frames for annotation lines
  lines.df.1 <- data.frame(y = rep(c(0, p1.df$cume.percent1), each = 2),
                           x = c(.6, .8))
  
  lines.df.2 <- data.frame(y = rep(c(0, p2.df$cume.percent2), each = 2),
                           x = c(1.2, 1.4))
  
  lines.df <- rbind(lines.df.1, lines.df.2)
  lines.df$gp <- rep(LETTERS[1:(nrow(lines.df)/2)], each = 2)
  
  # Data frames for pitch labels
  labs.df.1 <- p1.df %>%
    mutate(ypos = (cume.percent1 + lag(cume.percent1, default = 0))/2,
           xpos = .7) %>%
    select(pitch = pitch1, ypos, xpos)
  
  labs.df.2 <- p2.df %>%
    mutate(ypos = (cume.percent2 + lag(cume.percent2, default = 0))/2,
           xpos = 1.3) %>%
    select(pitch = pitch2, ypos, xpos)
  
  labs.df <- rbind(labs.df.1, labs.df.2)
  
  # Plot
  ggplot(data = bound.df.2, aes(x = 1, y = pc.diff, fill = sim)) +
    geom_bar(stat = "identity", width = .4) +
    scale_fill_gradientn(colors = c("red", "darkred", "black"), 
                         values = c(0, 1, 4)/4,
                         limits = c(0, 4), na.value = "black") +
    geom_line(data = lines.df, aes(x = x, y = y, group = gp, fill = NaN), size = 1, color = "grey50") +
    geom_text(data = labs.df, aes(x = xpos, y = ypos, label = pitch, fill = NaN), 
              size = 10, color = "grey50", fontface = "bold") +
    annotate("text", x = .7, y = 1.05, label = p1, size = 8, fontface = "bold", color = "grey50") +
    annotate("text", x = 1.3, y = 1.05, label = p2, size = 8, fontface = "bold", color = "grey50") +
    labs(fill = "Similarity") +
    theme_void()
}

# Test functions
(p1 <- unique(pitchSims.df$pitcher1[1]))
(p2 <- unique(pitchSims.df$pitcher1[2]))
pitcherSimilarity(p1, p2)
plotSimilarity(p1, p2)


# Get list of similar pitchers for a given righty
pnames <- unique(pitchers_hand$name)
pitcherSims.df <- data.frame(pitcher1 = pnames, 
                             pitcher2 = rep(pnames, each = length(pnames))) %>%
  filter(pitcher1 == p1)

pitcherSims.df$similarity <- mapply(pitcherSimilarity, 
                                    p1 = pitcherSims.df$pitcher1,
                                    p2 = pitcherSims.df$pitcher2)

View(pitcherSims.df %>% arrange(similarity))


# Get all similarity scores
pnames <- unique(pitchers_hand$name)

scores.vec <- mapply(FUN = pitcherSimilarity, p1 = rep(pnames, length(pnames)), p2 = rep(pnames, each = length(pnames)))

scores.df <- data.frame(pitcher1 = pnames,
                        pitcher2 = rep(pnames, each = length(pnames)),
                        similarity = scores.vec)

# Write out similarity score file
outfile <- paste0("/Users/jackwerner/Documents/My Stuff/Baseball/Pitcher Similarity Score/",
                  "Pitcher_Similarities_", 
                  handedness, "H.csv")

write.csv(scores.df, file = outfile, row.names = F)


