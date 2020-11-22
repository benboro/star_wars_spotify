library(dplyr)
library(shiny)
library(shinyWidgets)
library(tidyr)
library(ggplot2)

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

song_data <- song_data %>%
  select(
    track_name,
    movie_title,
    trilogy,
    danceability:valence
  )

# CONVERT MOVIE TITLE FROM CHARACTER TO FACTOR
song_data$movie_title <- song_data$movie_title %>%
  {factor(., levels=unique(.))}

# GET METRIC NAMES
categories <- colnames(song_data)[-c(1:3)]

# AVERAGE VALUES
song_data %>%
  group_by(movie_title) %>%
  summarise(count = n(),
            across(all_of(categories), ~ mean(.x)),
            .groups = "keep")

song_data %>%
  group_by(trilogy) %>%
  summarise(count = n(),
            across(all_of(categories), ~ mean(.x)),
            .groups = "keep")

# ANOMALIES
song_data[(song_data$valence > 0.5), c("track_name", "movie_title", "trilogy", "valence")]

# EXPORT SONG DATA TO CSV
#write.csv(song_data, file = "./data/song_data.csv")

# ASSIGN COLORS
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

# GET DATA IN TIDY FORMAT
plot_data <- song_data %>%
  pivot_longer(cols = all_of(categories), "name", "value") %>%
  select("movie_title", "trilogy", "track_name", "name", "value")

# TRUNCATE LONGER TRACK NAMES
char_lim <- 35
plot_data$track_name <- plot_data$track_name %>%
  {ifelse(nchar(.) > char_lim, paste0(trimws(substr(., 1, char_lim-5)),"..."), .)}

# CONVERT CHARACTER COLUMNS TO FACTORS
plot_data[names(plot_data) != "value"] <- plot_data[names(plot_data) != "value"] %>%
  {lapply(., function(x) {factor(x, levels=unique(x))})}

# SETUP THE SHINY APP
ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      
      selectizeGroupUI(
        id = "my-filters",
        inline = FALSE,
        params = list(
          trilogy <- list(inputId = "trilogy", title = "Select Trilogy", placeholder = "select"),
          movie <- list(inputId = "movie_title", title = "Select Movie", placeholder = "select"),
          track <- list(inputId = "track_name", title = "Select Track", placeholder = "select")
        )
      )
      
    ),
    
    mainPanel(
      
      plotOutput(
        outputId = "parallelcoordinates",
        width = "100%",
        height = "400px"
      ),
      br()
      
    )
    
  ),
  
  fluidRow(
    
    div(tableOutput("table"), style = "font-size: 90%; display: flex; justify-content: center; overflow-y: scroll; height: 200px")
    
  )
  
)
server <- function(input, output) {
  
  res_tab <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    data = song_data,
    vars = c("trilogy", "movie_title", "track_name")
  )
  
  
  res_mod <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    data = plot_data,
    vars = c("trilogy", "movie_title", "track_name")
  )
  
  output$parallelcoordinates <- renderPlot({
    
    plot_cond <- (length(unique(res_mod()$movie_title)) > 1) & 
      (length(unique(res_mod()$track_name)) > 5)
    plot_linetype <- if (plot_cond) {res_mod()$movie_title} else {res_mod()$track_name}
    plot_colors <- if (plot_cond) {movie_colors} else {track_colors}
    
    ggplot() +
      geom_line(aes(x=name, y=value, group=track_name, color=movie_title), 
                data=plot_data, alpha=1.0, color="grey",
                show.legend=FALSE) +
      geom_line(aes(x=name, y=value, group=track_name,
                    color=plot_linetype), 
                data=res_mod(), alpha=0.7, lwd=1.5, 
                show.legend=TRUE) +
      scale_color_manual(values=rep(
        plot_colors,
        #if (length(unique(res_mod()$movie_title))>1) {movie_colors} else {track_colors}, 
        length.out=nrow(res_mod())
      )) +
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
    
  })
  
  output$table <- renderTable({
    
    res_tab()
    
  }, striped = TRUE, hover = TRUE)
  
}

# RUN THE APP
shinyApp(ui = ui, server = server, options = list(width="120%", height=650))
