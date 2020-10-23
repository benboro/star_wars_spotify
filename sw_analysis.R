library(dplyr, quietly = TRUE)
#library(dplyr, warn.conflicts = FALSE)
#library(GGally, quietly = TRUE)
#library(fmsb)
library(tidyr)
library(ggplot2)
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

#df <- og_data[og_data$episode==4,]

# ggparcoord(df,
#   columns=cat_col,
#   groupColumn=ifelse(length(unique("movie_title"))==1,"track_name","movie_title"),
#   scale="globalminmax",
#   showPoints=FALSE,
#   alphaLines=1
# ) +
#   theme_minimal() +
#   scale_x_discrete(expand = c(0,0)) +
#   # scale_color_manual(values=c("#2E66B1", "#E13028", "#188655", 
#   #                             "#B7ADB0", "#D79E2A", "#8956A3", 
#   #                             "#E9D84F", "#C60B36", "#49508D")) +
#   labs(x="",y="strength") +
#   theme(text = element_text(size = 16),
#         legend.title = element_blank())

# radar_data <- rbind(rep(1,ncol(og_data)),rep(0,ncol(og_data)),og_data)
# radarchart(radar_data[,10:16], axistype = 4,
#            plwd = 2, plty = 1, pty = 32,
#            cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0, 1, 5), cglwd = 0.8,
#            vlcex = 0.8
#            )


# WRITE DATA TO CSV
#write.csv(song_data,"./data/song_data.csv", row.names = FALSE)

plot_data <- song_data %>%
  pivot_longer(cols = categories, 'name', 'value') %>%
  select("movie_title", "trilogy", "track_name", "name", "value")

plot_data$track_name <- plot_data$track_name %>%
  #as.character() %>%
  {ifelse(nchar(.)>35, paste0(trimws(substr(., 1, 35-5)),"..."), .)}

plot_data[names(plot_data) != "value"] <- plot_data[names(plot_data) != "value"] %>%
  {lapply(., function(x) {factor(x, levels=unique(x))})}

filter_data <- plot_data %>%
  filter(movie_title=="The Force Awakens")
col <- if (length(unique(filter_data$movie_title))>1) {movie_colors} else {track_colors}

# colors resemble soundtrack bar color (except phantom menace because its grey)
movie_colors <- c("A New Hope"="#2E66B1", 
                  "The Empire Strikes Back"="#E13028",
                  "Return of the Jedi"="#188655", 
                  "The Phantom Menace"="#492E27", 
                  "Attack of the Clones"="#D79E2A", 
                  "Revenge of the Sith"="#8956A3", 
                  "The Force Awakens"="#E9D84F", 
                  "The Last Jedi"="#C60B36", 
                  "The Rise of Skywalker"="#49508D")
track_colors <- c("#2E66B1", "#E13028", "#188655", 
                  "#492E27", "#D79E2A", "#8956A3", 
                  "#E9D84F", "#C60B36", "#49508D")

# g_back <- ggplot() +
#   geom_line(aes(x=name, y=value, group=track_name, color=movie_title), 
#             data=plot_data, alpha=0.2, #color="grey",
#             show.legend=FALSE) +
#   scale_color_manual(values=rep(sw_colors))
# g_back
# g_dynamic <- ggplot() +
#   geom_line(aes(x=name, y=value, group=track_name, color=movie_title), 
#             data=filter_data, alpha=1.0, lwd=1, 
#             show.legend=TRUE) +
#   scale_color_manual(values=rep(sw_colors, length.out=3))
# g_dynamic

ggplot() +
  geom_line(aes(x=name, y=value, group=track_name, color=movie_title), 
            data=plot_data, alpha=1.0, color="grey",
            show.legend=FALSE) +
  geom_line(aes(x=name, y=value, group=track_name, color=track_name), 
            data=filter_data, alpha=0.8, lwd=1.5, 
            show.legend=TRUE) +
  scale_color_manual(values=rep(col, length.out=44)) +
  guides(color=guide_legend(override.aes = list(size=1, alpha=1.0))) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 1)) + 
  scale_x_discrete(expand = c(0,0)) +
  labs(x="",y="strength") +
  theme(text = element_text(size = 16),
        panel.background = element_rect(fill = "transparent", color = NA),
        # plot.margin = unit(c(0.5, 13.0, 0.5, 0.5), "cm"),
        # legend.position = c(1.4, 0.5),
        legend.title = element_blank(),
        legend.text = element_text(size=10),
        #legend.key.width = unit(1, "cm"),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 25, vjust = 0.7, hjust=0.7))

