library(dplyr, quietly = TRUE)
#library(dplyr, warn.conflicts = FALSE)
library(GGally, quietly = TRUE)
library(fmsb)
#library(ggthemes) # for FiveThirtyEight-style plot

# CLEAR WORKSPACE AND PLOT WINDOW
rm(list=ls())
graphics.off()
dirpath <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dirpath)

# IMPORT SONG DATA FROM CSV
song_data <- read.csv("./data/john_williams_star_wars_data.csv",stringsAsFactors=FALSE)

# ADD EPISODE
song_data <- song_data %>%
  mutate(episode = with(.,case_when(
    (album_release_year == 1977) ~ 4,
    (album_release_year == 1980) ~ 5,
    (album_release_year == 1983) ~ 6,
    (album_release_year == 1999) ~ 1,
    (album_release_year == 2002) ~ 2,
    (album_release_year == 2005) ~ 3,
    (album_release_year == 2015) ~ 7,
    (album_release_year == 2017) ~ 8,
    (album_release_year == 2019) ~ 9
  )))

# GET MOVIE TITLE FROM ALBUM TITLE
song_data$movie_title <- song_data$album_name %>%
  gsub(".*[:]\\s(.*)\\s\\(.*","\\1",.)

# ADD TRILOGY
ELSE <- TRUE
song_data <- song_data %>%
  mutate(trilogy = with(.,case_when(
    (episode <= 3) ~ "Prequel",
    (episode <= 6) ~ "Original",
    ELSE ~ "Sequel"
  )))

# ORGANIZE DATA
song_data <- song_data[
  order(song_data$album_release_year, song_data$track_number),
]
rownames(song_data) <- NULL

# REORDER COLUMNS (OMIT album_release_date, key, mode)
song_data <- song_data %>%
  select(
    episode:trilogy,
    track_number,
    track_name,
    artist_name,
    album_name,
    duration_ms,
    album_release_year,
    danceability:energy,
    speechiness:tempo,
    loudness,
    key_name:key_mode
  )

og_data <- song_data %>%
  filter(trilogy=="Original")

categories <- c("danceability", 
                "energy", 
                "speechiness", 
                "acousticness", 
                "instrumentalness", 
                "liveness", 
                "valence")
cat_col <- which(colnames(song_data) %in% categories)

df <- og_data[og_data$episode==4,]

ggparcoord(df,
  columns=cat_col,
  groupColumn=ifelse(length(unique("movie_title"))==1,"track_name","movie_title"),
  scale="globalminmax",
  showPoints=FALSE,
  alphaLines=1
) +
  theme_minimal() +
  scale_x_discrete(expand = c(0,0)) +
  # scale_color_manual(values=c("#2E66B1", "#E13028", "#188655", 
  #                             "#B7ADB0", "#D79E2A", "#8956A3", 
  #                             "#E9D84F", "#C60B36", "#49508D")) +
  labs(x="",y="strength") +
  theme(text = element_text(size = 16),
        legend.title = element_blank())

# radar_data <- rbind(rep(1,ncol(og_data)),rep(0,ncol(og_data)),og_data)
# radarchart(radar_data[,10:16], axistype = 4,
#            plwd = 2, plty = 1, pty = 32,
#            cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0, 1, 5), cglwd = 0.8,
#            vlcex = 0.8
#            )


# WRITE DATA TO CSV
#write.csv(song_data,"./data/song_data.csv", row.names = FALSE)
