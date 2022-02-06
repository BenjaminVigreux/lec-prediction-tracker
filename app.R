
# Load packages ---------------------------------------------

library(shiny)
library(tidyverse)
library(janitor)
library(rvest)
library(shinydashboard)
library(DT)
library(viridis)
library(scales)

# Load predictions ------------------------------------------

matchup_preds <- read_csv("matchup_preds.csv")

preds <- read_csv("preds.csv")


# Name lookup -----------------------------------------------

leaguepedia_names <- c("MAD", "RGE", "XL", "AST", "FNC", "SK", 
                       "G2", "VIT", "BDS", "MSF")

pred_names <- c("madlions", "rogue", "excel", "astralis", "fnatic", 
                "skgaming", "g2", "vitality", "bds", "misfits")

official_names <- c("MAD Lions", "Rogue", "Excel", "Astralis", 
                    "Fnatic", "SK Gaming", "G2 Esports", 
                    "Team Vitality", "Team BDS", "Misfits Gaming")

names_lookup <- tibble(official_names, pred_names, leaguepedia_names)

leaguepedia_to_official <- names_lookup %>% select(-pred_names)

preds_to_official <- names_lookup %>% select(-leaguepedia_names)


# Swicth pred names to official names -----------------------

preds <- preds %>% 
  left_join(preds_to_official, by = c("predicted_winner" = "pred_names")) %>% 
  mutate(predicted_winner = official_names) %>% 
  select(-official_names)

matchup_preds <- matchup_preds %>% 
  left_join(preds_to_official, by = c("blue_side" = "pred_names")) %>% 
  left_join(preds_to_official, by = c("red_side" = "pred_names")) %>%
  left_join(preds_to_official, by = c("predicted_winner" = "pred_names")) %>% 
  mutate(blue_side = official_names.x,
         red_side = official_names.y,
         predicted_winner = official_names) %>% 
  select(-c(official_names.x, official_names.y, official_names))

# Scrape Leaguepedia games played table ---------------------

get_leaguepedia_data <- function() {
  
  spring_split_page <- read_html("https://lol.fandom.com/wiki/LEC/2022_Season/Spring_Season")
  
  games_played_table <- spring_split_page %>% 
    html_element(".md-table") %>% 
    html_table() 
  
  names(games_played_table) <- games_played_table[2,] %>% 
    unlist(., use.names = FALSE)
  
  games_played_table <- clean_names(games_played_table) %>% 
    filter(!str_detect(team_1, "[showhide]")) %>% 
    mutate(team_1 = iconv(team_1, "UTF-8", "ASCII", sub=""),
           team_2 = iconv(team_2, "UTF-8", "ASCII", sub=""))
  
  game_results <- games_played_table  %>% 
    select(team_1, team_2, score, blue, red) %>% 
    separate(score, into = c("team_1_score", "team_2_score"), sep = " - ") %>% 
    mutate(actual_winner = case_when(
      team_1_score == "1" ~ team_1,
      team_2_score == "1" ~ team_2,
      TRUE ~ "TBD")) %>% 
    filter(blue != "TBD") %>% 
    select(blue, red, actual_winner)
  
  return(game_results)
  
}


# UI --------------------------------------------------------

ui <- dashboardPage(
  
  dashboardHeader(title = "LoL Predictions: LEC Spring Split 2022"),
  
  dashboardSidebar(
    h5("A Shiny dashboard to keep track of predictions made ", 
       a("here", href = "https://rdvark.net/2022/01/14/who-will-top-the-2022-lec-spring-split/"))
  ),
  
  dashboardBody(
    
    fluidRow(
      
      infoBoxOutput("aim_perc",
                    width = 3),
      
      infoBoxOutput("actual_perc",
                    width = 3)
      
    ),
    
    fluidRow(
      column(
        box(
          status = "primary",
          titlePanel("Predicted LEC Table"),
          solidHeader = T,
          br(),
          DT::dataTableOutput("pred_table"),
          width = 4,
          height = "550px"
        ),
        
        box(
          status = "primary",
          titlePanel("Predicted vs. Actual"),
          solidHeader = T,
          br(),
          plotOutput("plot", height = "400px"),
          width = 4,
          height = "550px"
        ),
      
        box(
          status = "primary",
          titlePanel("Actual LEC Table"),
          solidHeader = T,
          br(),
          DT::dataTableOutput("actual_table", height = "350px"),
          width = 4,
          height = "550px"
        ),
        width = 12
      )
      ),
    
    fluidRow(
      column(
        box(
          status = "primary",
          titlePanel("Match-by-Match Predictions"),
          solidHeader = T,
          br(),
          DT::dataTableOutput("matchup_table"),
          width = 12
        ),
      width = 12
      )
    )
  )
)

server <- function(input, output) {
  

# Reactive data ---------------------------------------------
  
  latest_data <- reactive({
    
    invalidateLater(86400000)
    
    get_leaguepedia_data()
    
  })
  
  matchup_table <- reactive({
    latest_data() %>% 
      left_join(leaguepedia_to_official, by = c("blue" = "leaguepedia_names")) %>% 
      left_join(leaguepedia_to_official, by = c("red" = "leaguepedia_names")) %>%
      left_join(leaguepedia_to_official, by = c("actual_winner" = "leaguepedia_names")) %>% 
      mutate(blue = official_names.x,
             red = official_names.y,
             actual_winner = official_names) %>% 
      select(-c(official_names.x, official_names.y, official_names)) %>% 
      left_join(matchup_preds, by = c("blue" = "blue_side", "red" = "red_side")) %>% 
      select(blue, blue_win_prediction, red, red_win_prediction, predicted_winner, actual_winner)
  })
  
  perc_correct <- reactive({
    nrow(matchup_table() %>% filter(predicted_winner == actual_winner)) / 
      nrow(matchup_table()) 
  })
  
  actual_table <- reactive({ 
    matchup_table() %>% 
      select(-c(blue_win_prediction, red_win_prediction, predicted_winner)) %>% 
      pivot_longer(c(blue, red), values_to = "team") %>% 
      group_by(team) %>% 
      summarise(wins = sum(team == actual_winner),
                losses = sum(team != actual_winner)) %>% 
      arrange(desc(wins), team)
  })
  
  predicted_table <- reactive({ 
    matchup_table() %>% 
      select(-c(blue_win_prediction, red_win_prediction, actual_winner)) %>% 
      pivot_longer(c(blue, red), values_to = "team") %>% 
      group_by(team) %>% 
      summarise(wins = sum(team == predicted_winner),
                losses = sum(team != predicted_winner)) %>% 
      arrange(desc(wins), team)
  })
  
  ranks <- reactive({
    predicted_table() %>% 
      left_join(actual_table(), 
                by = "team", 
                suffix = c("_pred", "_actual")) %>% 
      arrange(desc(wins_pred), team) %>% 
      mutate(rank_pred = row_number()) %>% 
      arrange(desc(wins_actual), team) %>% 
      mutate(rank_actual = row_number()) %>% 
      select(team, rank_pred, rank_actual) %>% 
      pivot_longer(cols = c(rank_pred, rank_actual)) %>% 
      mutate(chart_index = if_else(name == "rank_pred", 0, 1),
             value = value * (-1))
  })
  
# Outputs ---------------------------------------------------

  output$pred_table <- renderDataTable( datatable(
    data = predicted_table(), options = list(dom = 't', scrollX = TRUE, scrollY = '370px', paging = FALSE ))
    )
  
  output$actual_table <- renderDataTable( datatable(
    data = actual_table(), options = list(dom = 't', scrollX = TRUE, scrollY = '370px', paging = FALSE )
  ))
  
  output$matchup_table <-renderDataTable( datatable(
    data = matchup_table() %>% 
      rename(blue_side = blue, red_side = red) %>% 
      mutate(blue_win_prediction = scales::percent_format(accuracy = 0.01)(blue_win_prediction),
             red_win_prediction = scales::percent_format(accuracy = 0.01)(red_win_prediction),
             prediction_success = if_else(predicted_winner == actual_winner, "Success!", "Unsuccessful")), 
    options = list(dom = 't', scrollX = TRUE, scrollY = '300px', 
                   paging = FALSE)
  ) %>% formatStyle(
    c("blue_side", "blue_win_prediction", "red_side", "red_win_prediction", 
      "predicted_winner", "actual_winner", "prediction_success"),
    valueColumns = "prediction_success",
    backgroundColor = styleEqual(c("Success!", "Unsuccessful"), c("#d9ead3", "#f4cccc")))) 
  
  output$plot <- renderPlot({ 
    ranks() %>% 
      ggplot(aes(x = chart_index, y = value, group = team, 
                 colour = team)) +
      geom_line(size = 1.5) +
      geom_point(size = 6) +
      geom_text(data = ranks() %>% filter(chart_index == 0),
                aes(x = chart_index - 0.05, label = team),
                size = 5, hjust = 1) +
      geom_text(data = ranks() %>% filter(chart_index == 1),
                aes(x = chart_index + 0.05, label = team),
                size = 5, hjust = 0) +
      scale_x_continuous(limits = c(-0.5, 1.5)) +
      theme_void() +
      scale_color_viridis_d() +
#      scale_color_brewer(palette = "Paired") +
      theme(legend.position = "none")
  })
  
  output$aim_perc <- renderInfoBox({
    infoBox(
      "Predictions target",
      "58%",
      icon = icon("bullseye"),
      color = "purple",
      fill = TRUE)
  })

  output$actual_perc <- renderInfoBox({
    infoBox(
      "Accurate prediction rate",
      scales::percent_format()(perc_correct()),
      icon = icon("chart-bar"),
      color = if(perc_correct() >= 0.58){"green"}else{"red"},
      fill = TRUE)
  })
  
}
  
# Run the application 
shinyApp(ui = ui, server = server)
