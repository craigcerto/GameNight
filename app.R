library(shiny)
library(bslib)
library(htmltools)
library(reactable)
library(dplyr)
library(stringr)
library(tidyr)
library(glue)
library(splitstackshape)
library(shinyjs)
library(reactable.extras)

# Card block - front-end sections
cards <- list(
  
  play_main_page = card(
    border = FALSE,
    full_screen = FALSE,
    class = "play_menu",
    card(
      class = "play_card_blends",
      div(
        class = "neon",
        tags$img(
          src = "neon.png",
          width = "300px"
        )
      )
    ),
    card(
      class = "play_card_full",
      h2("Recent Games", align = "center"),
      reactableOutput("recent_games"),
      min_height = "600px"
    ) 
  ),
  
  select_game = card(
    class = "play_card",
    min_height = "215px",
    max_height = "215px",
    h2("Start New Game", align = "center"),
    layout_column_wrap(
      tags$button(
        id = "domino_button",
        class = "btn action-button play_button",
        tags$div(
          tags$div(
            tags$img(
              src = "dominoes_b.png",
              height = "40px"
            )
          ),
          tags$div("Dominos")
        )
      ),
      tags$button(
        id = "rummy_button",
        class = "btn action-button play_button",
        tags$div(
          tags$div(
            tags$img(
              src = "rummy_b.png",
              height = "40px"
            )
          ),
          tags$div("Rummy")
        )
      ),
      tags$button(
        id = "mahjong_button",
        class = "btn action-button play_button",
        tags$div(
          tags$div(
            tags$img(
              src = "mahjong_b.png",
              height = "40px"
            )
          ),
          tags$div("Mahjong")
        )
      )
    )
  ),
  
  choose_players = card(
    border = FALSE,
    full_screen = FALSE,
    class = "player_menu",
    min_height = "235px",
    max_height = "235px",
    card(
      border = FALSE,
      class = "player_card",
      div(
        class = "choose_p",
        h2("Select Players"),
        tags$style(HTML(
          '
            .choose_p {
            
              text-align:center;
              align:middle;
              vertical-align:middle;
            }
          '
        ))
      ),
      uiOutput("player_selection_ui")
    )
  ),
    
  choose_settings = card(
    border = FALSE,
    full_screen = FALSE,
    class = "player_menu",
    min_height = "240px",
    max_height = "240px",
    card(
      border = FALSE,
      class = "player_card",
      min_height = "200px",
      max_height = "200px",
      div(
        class = "choose_p",
        h2("Choose Settings"),
        tags$style(HTML(
          '
          .choose_p {
            text-align:center;
            align:middle;
            vertical-align:middle;
            padding-top:0px;
            padding-bottom:0px;
          }
        '
        ))
      ),
      div(
        class = "setting_container",
        tags$style(HTML('
          .setting_container {
              display: table;
              width: 100%;
              padding-left:12%;
          }
          .setting_container > div {
              display: table-cell;
              width: 50%;
          }
        ')),
        div(
          class = "win_condition_container",
          div(
            class = "win_condition_element",
            h4(
              "Win Condition"
            ),
            div(
              class = "switch_class",
              span("Rounds", class = "switch_span"),
              span(input_switch("rounds_score_switch", ""), class = "switch_span"),
              span("Points", class = "switch_span")
            ),
            
          )
        ),
        # Adding comment
        div(
          class = "score_container",
          div(
            class = "score_element",
            h4(
              uiOutput("score_or_rounds")
            ),
            uiOutput("score_or_rounds_input", inline = TRUE, class = "score_area")
          )
        )
      )
    )
  ),
  
  # Have a table on the left hand side. The header of each column should be the user 
  # There should be a row number 
  scoring_page = card(
    class = "scoring_page",
    uiOutput("scoring_title"),
    uiOutput("round_indicator"),
    uiOutput("max_text"),
    layout_columns(
      col_widths = c(8, 4),
      column(
        width = 12,
        tags$style(
          HTML("
            .score_entry_text {
              text-align:center;
            }
          ")
        ),
        h3("Score Entry", class = "score_entry_text"),
        reactableOutput("score_table")
      ),
      column(
        width = 12,
        h3("Current Standings", class = "score_entry_text"),
        reactableOutput("scoreboard_table"),
        uiOutput("button_space"),
        div(actionButton("pause_game_button", "Pause Game", width = "400px", class = "pause_game_button"), class = "r_b"),
        br(),
        div(actionButton("end_game_button", "End Game", width = "400px", class = "end_game_button"), class = "r_b")
      )
    )
    
  )
)

ui <- page_navbar(
  useShinyjs(),
  title = "GameNight",
  sidebar = sidebar(open = FALSE),
  id = "navbar",
  window_title = "GameNight",
  # Dark Blue - 586F7C
  # Licorice - 231B1B
  # Gold - C6A15B
  # White - F4F4F9
  # Silver - 
  tags$head(
    HTML(
      '<link href="https://fonts.googleapis.com/css2?family=Anta" rel="stylesheet">'
    ),
    uiOutput("dis"),
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      .game_button_clicked {
        background-color:#DDDDDD !important;
        border-color:#F4F4F9 !important;
        border-width:2px !important;
      }
      .bslib-sidebar-layout>.main {
        padding-top:0px;
      }
      .neon{
        display:flex;
        justify-content:center;
      }
      .r_b {
        display: flex;
        justify-content: center;  
      }
      .t_span {
        display:inline-flex;
        justify-content:center;
        gap:10px;
      }
      .pause_game_button {
        text-align:center;
        font-size:20px;
        background-color: #F2E3BC;
      }
      .end_game_button {
        text-align:center;
        font-size:20px;
        background-color: #586F7C;
        color:#F4F4F9;
      }
      .pause_game_button:hover {
        text-align:center;
        font-size:20px;
        background-color: #231B1B;
      }
      .end_game_button:hover{
        text-align:center;
        font-size:20px;
        background-color: #231B1B;
      }
      .score_entry_text_w {
        color:#586F7C;
        text-align:center;
      }
      .score_entry_text {
        color:#F4F4F9;
      }
      .scoring_page{
        background-color:#C6A15B;
        border:none;
        box-shadow: none;
        margin-top:20px;
      }
      .play_b {
        background-color:#F2E3BC;
        font-size:30px;
        width:400px;
        color:#586F7C;
        border-color:#F2E3BC;
        border-width:2px;
        vertical-align:center;
      }
      .score_area {
        display: flex;
        align-items:baseline;
        gap:10px;
        justify-content:center;
      }
      .play_menu {
        border: none;
        box-shadow: none;
        background-color: #586F7C;
      }
      .player_menu {
        border: none;
        box-shadow: none;
        background-color: #586F7C;
        text-align:center;
        align:center;
        min-height:275px;
        max-height:275px;
        height:275px;
        
        #div{
          gap:0px;
          margin:0px;
          padding:0px;
        }
        
      }
      .play_button_menu {
        padding-top:0px;
        background-color:#C6A15B;
        border:none;
        box-shadow: none;
        align-items:center;
      }
      .player_card {
        background-color: #586F7C;
        .btn:hover {
          background-color:#F4F4F9;
        }
        border:none;
        box-shadow: none;
      }
      .player_selection_card {
        box-shadow: none;
        background-color: #586F7C;
        border:none;
      }
      .play_card {
        background-color: #586F7C;
        margin-top:20px;
      }
      .play_card_full {
        background-color: #C6A15B;
      }
      .play_card_blends {
        background-color: #586F7C;
        border:none;
        box-shadow: none;
      }
      .player_button {
        width: 250px;
        background-color:#F2E3BC;
        color: #F4F4F9;
        /*
        border-color: #F4F4F9;
        border-width: 2px;
        border-radius:0.6;
        #D3D3CF
        */
      }
      .player_button_selected {
        width: 250px;
        background-color:#E9E9E9;
        color: #F4F4F9;
        /*border-color: #C6A15B;
        border-width: 2px;
        border-radius:0.7;*/
      }
      .player_button_selected2 {
        width: 250px;
        background-color:#DDDDDD;
        color: #F4F4F9;
        border-color: #F2E3BC;
        border-width: 2px;
        border-radius:0.7;
      }
      .form-control {
        background-color: #F2E3BC;
      }
      .play_button {
        background-color:#F2E3BC;
        color: #231B1B;
        border-color: #F4F4F9;
        border: none;
      }
      .play_button:hover{
        background-color:#F2E3BC;
        color: #586F7C;
      }
      h4 {
        padding-top:10px;
      }
      h1 {
        color: #F4F4F9;
        font-weight: 300;
        font-size:50px;
      }
      h2 {
        color: #F4F4F9;
        font-weight: 300;
        font-size:36px;
      }
      .craig_div {
        color:#2854C5;
        display: inline-block;
        vertical-align: middle;
        line-height: 1;
      }
      .win_condition_container {
        background-color:#586F7C;
      }
      .win_condition_element {
        margin-top:0px;
        border-radius: 25px;
        background-color: #C6A15B;
        width: 400px;
        height:100px;
        color:#F4F4F9;
      }
      .score_container {
        background-color:#586F7C;
      }
      .score_element {
        border-radius: 25px;
        background-color: #C6A15B;
        width: 400px;
        height:100px;
        color:#F4F4F9;
      }
      .form-switch .form-check-input {
        background-color: #F2E3BC
      }
      .switch_span {
        display:inline-flex;
      }
      .switch_class {
        vertical-align:center;
        gap:10px;
        display:inline-flex;
      }
      .score_round_span {
        display:inline-flex;
        
      }
      .shiny-input-container:not(.shiny-input-container-inline) {
        width: 25%;
      }
      .input_text_class {
        background-color: #F2E3BC;
        color: #586F7C;
        text-align:center;
        width:25%;
        border:none;
      }
      .input_text_class:selected {
        background-color: #F2E3BC;
        color: #586F7C;
        text-align:center;
        width:25%;
        border:2px;
        border-color:blue;
      }
      .round_indicator {
        text-align:center;
        color:#F2E3BC;
      }
      .total_score_class {
        font-size:40px;
        color:#231B1B;
        font-weight:500;
      }
      .selected_row {
        border-width:3px;
        border-color:white;
        font-size:20px;
      }
      "
    ))
  ),
  
  # Play Page
  nav_panel(
    title = "Home",
    value = "main_page",
    icon = tags$img(src = "home.png", width = "20px"),
    cards$play_main_page
  ),
  nav_panel(
    value = "choose_settings",
    title = "Play",
    icon = tags$img(src = "play.png", width = "20px"),
    cards$select_game,
    cards$choose_players,
    cards$choose_settings,
    div(actionButton("hit_play", "Play", class = "play_b"), class = "hit_play_b", tags$style(HTML(".hit_play_b{display:flex;justify-content:center;}")))
  ),
  nav_panel(
    value = "scoring_page",
    title = "Scoring Page",
    cards$scoring_page,
    reactable.extras::reactable_extras_dependency(),
    tags$script(HTML('
      document.addEventListener("keydown", function(e) {
          Shiny.setInputValue("key_pressed", e.key, {priority: "event"});
        });
  '))
  )
)

####################
# Server Functions #
#####################


# Build the colored display name with icon for all players
build_display_score <- function(player_name, color, small_icon, score) {
  
  div(
    tags$style(
      HTML("")
    ),
    tags$img(
      src = small_icon,
      height = "20px"
    ),
    glue::glue('{player_name}: {score}')
  )
  
}

##################
# UI Functions #
################

# Combine player scores into the scoreboard
create_scoreboard <- function(player_span) {
  
  do.call(span, as.list(player_span))
  
}

# Create the menu for player selection
create_menu = function(player_select_btn) {
  
  do.call(card, as.list(player_select_btn))
  
}

# Select a player in game selection menu
#   Adds a player to the players_selected reactive value
# 
select_player <- function(player_clicked, players_selected) {
  
  p <- players_selected()
  
  # Find which player was clicked
  p_selected_bool <- p %>% 
    filter(player_name == player_clicked) %>% 
    purrr::pluck("selected", 1)
  
  # If the player is selected, turn selected to FALSE
  if (p_selected_bool) {
    
    p[p['player_name'] == player_clicked, 'selected'] <- FALSE
    
  } else {
    
    p[p['player_name'] == player_clicked, 'selected'] <- TRUE
    
  }
  
  players_selected(p)
  
}

select_game <- function(game_clicked, game_selected) {
  
  
  game_selected(game_clicked)
  
  removeClass("domino_button", "game_button_clicked")
  removeClass("mahjong_button", "game_button_clicked")
  removeClass("rummy_button", "game_button_clicked")
  
  if (game_clicked == "Dominoes") {
    
    addClass("domino_button", "game_button_clicked")
    
  } else if (game_clicked == "Rummy") {
    
    addClass("rummy_button", "game_button_clicked")
    
  } else {
    
    addClass("mahjong_button", "game_button_clicked")
    
  }
  
}

# Server
server <- function(input, output) {
  
  output$round_indicator <- renderUI({
    
    x <- if (current_game_completion_type() == "Score") {
      glue("Playing to {current_game_max_score()} points")
    } else {
      glue("Playing {current_game_max_rounds()} rounds")
    }
    h3(
      glue("Playing Round {current_game_round()} | {x}"),
      class = "round_indicator"
    )
    
  })
  
  output$dis <- renderUI({
    
    c <- if (input$navbar == "choose_settings") {"C6A15B"} else if (input$navbar == "scoring_page") {"586F7C"} else {"586F7C"}
    
    tags$style(
      HTML(glue("
        body {{
          background-color:#{c};
          color: white;
          font-family: 'Anta', sans-serif;
        }}
      "))
    )
    
  })
  
  observeEvent(input$key_pressed, {
    
    if (input$key_pressed == "Enter") {
    
      # Get all of the values for this round
      for (p in current_game_players()) {
        
        r <- input[[glue("text_{p}")]]
        score <- if (is.na(as.numeric(r$value[[1]]))) 0 else as.numeric(r$value[[1]])
        player_name <- r$column[[1]]
        round <- r$row[[1]]
        log_score(player_name = player_name, round = round, score = score)
        
      }
      
    }
    
  })
  
  observeEvent(input$end_game_button, {
    
    log_game(current_game_id())
    
    showModal(
      modalDialog(
        title = "Game Over",
        size = "m",
        div(
          "Winner!"
        )
      )
    )
    
  })
  
  b_gap_height <- reactive(400 - (80*length(current_game_players())))
  
  output$button_space <- renderUI({
    height <- b_gap_height()
    card(
      class = "p_diddy",
      min_height = glue("{height}px"),
      max_height = glue("{height}px"),
      tags$style(
        HTML("
          .p_diddy{
            background-color:#C6A15B;
            border:none;
            box-shadow:none;
          }
        ")
      )
    )
    
  })
  
  last_input_round <- reactiveVal(1)
  
  # Enter the score into the game when user types it
  observe({

    for (p in current_game_players()) {

      r <- input[[glue("text_{p}")]]

      round <- r[["row"]]
      last_input_round(round)

    }
    

  })

  #paste0(id, ": ", string_list(input[[id]]))
  
  # Score table
  output$score_table <- renderReactable({
    
    print("Printing current_game_scores before render")
    
    out <- current_game_scores() %>% 
      pivot_wider(
        names_from = player,
        values_from = score
      ) %>% 
      select(-score_time) %>% 
      group_by(round) %>% 
      summarize_all(
        max, na.rm = TRUE
      )
    
    col_definitions <- list(
      round = colDef(
        width = 100,
        name = "Round",
        align = "center",
        vAlign = "center",
        style = function(value, index, name) {
          if (index != current_game_round()) {
            list(backgroundColor = "#586F7C", color = "#C6A15B", fontSize = "20px")
          } else {
            list(backgroundColor = "#586F7C", color = "#C6A15B", fontsize = "20px", borderColor = "#231B1B", borderWidth = "5px", borderStyle = "solid")
          }
        }
      )
    )
    
    
    players <- current_game_players()
    
    i <- 2
    while(i <= length(players) + 1) {
      nm <- players[[i-1]]
      col_definitions[[nm]] = colDef(
        align = "center",
        vAlign = "center",
        cell = text_extra(glue("text_{nm}"), class = "input_text_class"),
        style = function(value, index, name) {
          if (index == current_game_round()) {
            list(borderColor = "#231B1B", borderWidth = "5px", borderStyle = "solid")
          }
        }
      )
      i <- i + 1
    }
    
    reactable(
      out,
      pagination = FALSE,
      bordered = TRUE,
      defaultColDef = colDef(
        align = "center",
        vAlign = "center"
      ),
      columns = col_definitions,
      theme = reactableTheme(
        borderColor = "#C6A15B",
        backgroundColor = "#C6A15B",
        borderWidth = "5px",
        headerStyle = list(
          backgroundColor = "#586F7C",
          color = "#F4F4F9"
        ),
        cellStyle = list(
          backgroundColor = "#F2E3BC",
          color = "#586F7C"
        ),
        highlightColor = "#f0f5f9",
        cellPadding = "8px 12px"
      )
    )
  })
  
  # Render the scorecard table for the game menu
  output$scoreboard_table <- renderReactable({
    
    cg <- current_game_scoreboard()
    
     cg %>% 
      reactable(
        bordered = TRUE,
        columns = list(
          game_id = colDef(
            show = FALSE
          ),
          player = colDef(
            show = FALSE,
            html = TRUE
          ),
          display = colDef(
            name = "Player",
            align = "center",
            vAlign = "center",
            html = TRUE
          ),
          total_score = colDef(
            name = "Total Score",
            align = "center",
            vAlign = "center",
            html = TRUE,
            class = "total_score_class"
          )
        ),
        theme = reactableTheme(
          borderColor = "#C6A15B",
          backgroundColor = "#C6A15B",
          borderWidth = "5px",
          headerStyle = list(
            backgroundColor = "#586F7C",
            color = "#F4F4F9"
          ),
          cellStyle = list(
            backgroundColor = "#F2E3BC",
            color = "#586F7C"
          ),
          highlightColor = "#f0f5f9",
          cellPadding = "8px 12px",
          #style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
          searchInputStyle = list(width = "100%")
        )
      )
      
    
  })
  
  # Output the scoring title
  output$scoring_title <- renderUI(
    tagList(
      tags$style(
        HTML("
            .score_entry_text {
              text-align:center;
            }
          ")
      ),
      tags$span(
        class = "t_span",
        h2(glue::glue("{current_game_type()}  "), class = "score_entry_text_w"),
        tags$img(
          src = "dominoes.png",
          height = "40px"
        ),
        h2(glue::glue(" {as.Date(current_game_start())}"), class = "score_entry_text_w")
      )
    )
  )
  
  # Render the score or rounds section 
  output$score_or_rounds <- renderUI({
    
    rounds_score_switch <- input$rounds_score_switch
    
    if (rounds_score_switch) "Choose Winning Score" else "Choose Number of Rounds" 
    
  })
  
  # Render the score or round section of the setting selection
  output$score_or_rounds_input <- renderUI({
    
    rounds_score_switch <- input$rounds_score_switch
    
    o <- if (rounds_score_switch) "Score" else "Rounds" 
    default <- if (o == "Rounds") 12 else 200
  
   list( 
    span(o),
    numericInput("score_round_num_input", label = NULL, value = default, width = "75px")
   )
    
  })
  
  # Player data
  player_data <- reactive({
  
    data.frame(list(
      player_name = c('Gloria', 'Paul', 'Lauren', 'Craig', 'Frank'),
      nickname = c('Won Non', 'Pah-OOL', 'Gerald McBoingBoing', 'Ya Boii', 'Franconia Springfield'),
      color = c('#321D71', '#8C1A10', '#48752C', '#2854C5', '#964B00'),
      small_icon = c("gloria_small.png", "paul_small.png", "lauren_small.png", "craig_small.png", "frank_small.png"),
      big_icon = c("gloria_medium.png", "paul_medium.png", "lauren_medium.png", "craig_medium.png", "frank_medium.png")
    ))
    
  })
  
  # Augments player data with their display name
  enhanced_player <- reactive({
    
    player_data() %>% 
      rowwise() %>% 
      mutate(
        display = glue::glue('
          <span class = "player_menu_button">
            <span><img src = "{tolower(player_name)}_medium.png" alt = "" height = "40px"><span>
            <span style = "color: {color}">{player_name}</span>
            <div style = "color: {color}"><strong>\"{nickname}\"</strong></div>
          </span>
        ')
      )
  
  })
  
  # Reactive table to track which players are selected for a game
  players_selected <- reactiveVal(data.frame(
    player_name = c("Gloria", "Paul", "Lauren", "Craig", "Frank"),
    selected = c(FALSE, FALSE, FALSE, FALSE, FALSE)
  ))
  
  game_selected <- reactiveVal()
  
  observeEvent(input$mahjong_button, select_game("Mahjong", game_selected))
  observeEvent(input$domino_button, select_game("Dominoes", game_selected))
  observeEvent(input$rummy_button, select_game("Rummy", game_selected))
  
  # Triggered on a player being selected
  observeEvent(input$Craig_select_button, select_player("Craig", players_selected))
  observeEvent(input$Lauren_select_button, select_player("Lauren", players_selected))
  observeEvent(input$Frank_select_button, select_player("Frank", players_selected))
  observeEvent(input$Paul_select_button, select_player("Paul", players_selected))
  observeEvent(input$Gloria_select_button, select_player("Gloria", players_selected))
  
  # Triggered on hitting the play button 
  observeEvent(input$hit_play, {

    game_type <- game_selected()
    
    completion_type <- if (input$rounds_score_switch) {"Score"} else {"Rounds"}
    
    score_limit <- if (completion_type == "Rounds") {NULL} else input$score_round_num_input
    rounds <- if (completion_type == "Rounds") {input$score_round_num_input} else {12}
    
    initialize_game(game_type, completion_type, rounds, score_limit)
    
    # Switch to the scorekeeping tab
    nav_select(
      "navbar",
      selected = "scoring_page"
    )
        
  })
  
  # Build logic for selecting players for a game
  output$player_selection_ui <- renderUI({
    
    build_player_menu <- enhanced_player() %>% 
      left_join(players_selected(), by = join_by("player_name")) %>% 
      mutate(selected_tag = case_when(
        selected == TRUE ~ '_selected',
        TRUE ~ ''
      )) %>% 
      rowwise() %>% 
      mutate(
        player_select_btn = list(tags$button(
          id = paste0(player_name,"_select_button"),
          class = paste0("btn action-button player_button", selected_tag),
          HTML(display)
        )),
        player_select_btn = as.character(player_select_btn)
      ) %>% 
      ungroup() %>% 
      summarize(
        menu = paste0('<span class = "player_menu">',paste(player_select_btn, collapse = " "), "</span>")
      )
    menu <- build_player_menu$menu[[1]]
    
    # Card output for player selection
    card(
      class = "player_selection_card",
      min_height = "250px",
      max_height = "250px",
      HTML(menu)
    )

  }) 
  
  # Load previous games from file system
  previous_games <- reactive({
    
    p_t <- previous_games_trigger()
    
    paste0("data/", list.files("data/")) %>% 
      purrr::map(
        .f = function(x) {
          p <- readr::read_csv(x)
          if (endsWith(x, "scores.csv")) {
            p[FALSE,]
          } else p
        }
      ) %>% 
      bind_rows()

  })
  
  # Table containing previous games with the score sheet HTML as a column
  previous_games_span_table <- reactive({
    
    # Derive the winner of each game based on their individual scores
    winners <- previous_games() %>% 
      dplyr::group_by(game_id) %>% 
      dplyr::mutate(winning_score = max(player_score)) %>% 
      dplyr::rowwise() %>% 
      dplyr::filter(player_score == winning_score) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(game_id, winner = player_name)
    
    # Join winner to the previous games original data
    previous_games_w_winners <- 
      previous_games() %>% 
      dplyr::left_join(
        winners,
        dplyr::join_by(game_id)
      )
    
    # Join the previous games w/ winners with player data to build
    # the score display for each person per game
    previous_games_players <- 
      previous_games_w_winners %>% 
      dplyr::left_join(
        player_data(),
        by = dplyr::join_by(player_name)
      ) %>% 
      dplyr::mutate(
        score_display = purrr::pmap(list(player_name, color, small_icon, player_score), build_display_score)
      )
    
    # Combine individual player score displays into a game level scoreboard
    displayable_previous_games <- previous_games_players %>% 
      dplyr::group_by(game_id, game_type, rounds, winner) %>% 
      dplyr::arrange(desc(player_score)) %>% 
      dplyr::summarize(
        scoreboard = capture.output(
          purrr::map(list(score_display), create_scoreboard)
        )
      )
    
    # Join the previous games with the game_dttm to display full output
    final <- displayable_previous_games %>% 
      dplyr::left_join(
        {
          previous_games() %>% 
            dplyr::group_by(game_id) %>% 
            dplyr::summarise(
              game_dttm = max(game_dttm)
            )
        },
        by = dplyr::join_by(game_id)
      ) %>% 
      dplyr::select(
        game_id,
        game_dttm,
        game_type, 
        rounds,
        winner,
        scoreboard
      ) 
    
    reduced <- final %>% 
      group_by(game_id) %>%
      summarize(scoreboard = paste(scoreboard, collapse = "\n")) %>% 
      ungroup() %>% 
      mutate(scoreboard = str_replace_all(scoreboard, fixed("[[1]]"), replacement = "")) %>% 
      mutate(scoreboard = str_replace_all(scoreboard, fixed("\\"), replacement = "")) 
    
    f <- reduced %>% 
      left_join(final %>% distinct(game_id, game_dttm, game_type, rounds, winner), by = join_by("game_id")) %>% 
      left_join(player_data(), by = join_by("winner" == "player_name")) %>% 
      rowwise() %>% 
      mutate(
        winner = glue::glue('
          <div style = "vertical-align:middle;align:center;">
            <span><img src = "{tolower(winner)}_medium.png" alt = "" height = "40px"><span>
            <span style = "color: {color}">{winner}</span>
            <div style = "color: {color}"><strong>\"{nickname}\"</strong></div>
          </div>
        '),
        game_type = glue::glue('
          <div>
            <div>
              <img
                src="{tolower(game_type)}_blue.png"
                height = "40px"
                alt = ""
              >
            </div>
            <div>{game_type}</div>
          </div>
        ')
      ) %>% 
      select(game_id, game_dttm, game_type, rounds, winner, scoreboard)
    
    f

  })
  
  # Game configurations
  current_game_id <- reactiveVal(NULL)
  current_game_start <- reactiveVal(NULL)
  current_game_players <- reactiveVal(NULL)
  current_game_scores <- reactiveVal(NULL)
  current_game_round <- reactiveVal(NULL)
  current_game_max_rounds <- reactiveVal(NULL)
  current_game_max_score <- reactiveVal(NULL)
  current_game_type <- reactiveVal(NULL)
  current_game_completion_type <- reactiveVal(NULL)
  
  # Initialize a game
  initialize_game <- function(game_type, completion_type, rounds, score_limit) {
    
    current_game_start(Sys.time())
    current_game_players(
      players_selected() %>% 
        filter(selected == TRUE) %>% 
        purrr::pluck("player_name")
    )
    current_game_id(
      (previous_games_span_table() %>% 
        arrange(game_id) %>% 
        purrr::pluck("game_id", -1)) + 1
    )
    print(current_game_id)
    current_game_type(game_type)
    current_game_completion_type(completion_type)
    current_game_max_rounds(rounds)
    current_game_max_score(score_limit)
    current_game_round(1)
    current_game_scores(
      data.frame(
        list(
          player = current_game_players(),
          score = 0,
          score_time = current_game_start()
        )
      ) %>% 
      expandRows(rounds, drop = FALSE, count.is.col = FALSE) %>% 
      group_by(player) %>% 
      mutate(round = row_number()) %>% 
      ungroup() %>% 
      arrange(round)
    )
  }
  
  observeEvent(input$navbar, {
    
    
    #intialize_game("Dominoes", "Score", 12, 200)
  })
  
  # Calculate the current scoreboard with each player's display
  current_game_scoreboard <- reactive({
    
    scores <- current_game_scores()
    
    scores %>% 
      group_by(player) %>% 
      summarize(
        total_score = sum(score)
      ) %>% 
      left_join(
        enhanced_player(),
        by = join_by(player == player_name)
      ) %>% 
      mutate(game_id = current_game_id()) %>% 
      select(game_id, player, display, total_score) %>% 
      arrange(desc(total_score))
    
  })
  
  previous_games_trigger <- reactiveVal(1)
  
  # Log a game
  log_game <- function(game_id = current_game_id()) {
    
    game_metadata <- data.frame(list(
      game_id = game_id,
      game_dttm = current_game_start(),
      game_type = current_game_type(),
      game_completion_type = current_game_completion_type(),
      rounds = current_game_round(),
      game_end_dttm = Sys.time()
    ))
    
    summary_table <- current_game_scoreboard() %>% 
      left_join(
        game_metadata,
        by = join_by("game_id")
      ) %>% 
      select(
        game_id,
        game_dttm,
        game_type,
        rounds,
        game_completion_type,
        game_end_dttm,
        player_name = player,
        player_score = total_score
      )
    print(summary_table)
    
    # game_id, game_dttm, game_type, rounds, game_completion_type, game_end_dttm, player_name, player_score
    # 1, 2024-12-22T05:37:37Z, "Rummy", 12, "Score", 2024-12-22T05:37:37Z, "Craig", 400
    
    print(current_game_scores())
    scores_table <- current_game_scores() %>% 
      mutate(game_id = game_id) %>% 
      select(
        game_id,
        round,
        player,
        score,
        score_time
      )
    
    # Log game summary
    summary_table %>% 
      readr::write_csv(glue("data/game_{game_id}_summary.csv"))
    
    # Log game scores
    scores_table %>% 
      readr::write_csv(glue("data/game_{game_id}_scores.csv"))
    
    # Trigger reload of previous games data
    previous_games_trigger(previous_games_trigger() + 1)
      
  }
  
  # Log a player's score
  log_score <- function(
    
    game_id = current_game_id(),
    player_name,
    round = current_game_round(),
    score
    
  ) {
    
    # Add a players score to the current scores table
    scores <- current_game_scores()
    
    # Insert score and current time for each player in the round
    scores[scores[["round"]] == round & scores[["player"]] == player_name, "score"] <- score
    scores[scores[["round"]] == round & scores[["player"]] == player_name, "score_time"] <- Sys.time()
    
    # Add the new score to the current score data
    current_game_scores(scores)
    
    # Check if a round has been completed. If so, check if the round has surpassed max rounds if applicable
    player_count <- 
      current_game_players() %>% 
      length()
    
    # How many players scores have been logged this round?
    c_round <- round
    round_players_logged <- scores %>% 
      dplyr::filter(
        round == c_round,
        score_time != current_game_start()
      ) %>% 
      nrow()
    
    # Check if this round has been completed
    round_complete <- round_players_logged == player_count
    if (round_complete) {
      
      print("Round complete")
      
      # If this was the final round, log game, otherwise increase round #
      if (round == current_game_max_rounds() & current_game_completion_type() == 'Rounds') {
        
        print("Logging game")
        log_game(game_id)
        
      } else {
        
        print("Increasing round number")
        current_game_round(round + 1)
        
      }
    }
    
    scoreboard <- current_game_scoreboard()
    
    # Check if a player has surpassed the max points if applicable. 
    if (current_game_completion_type() == 'Score') {
      
      # If a player has hit the max points, finish the game
      hit_max_points <- 
        (scoreboard %>% 
           arrange(desc(total_score)) %>% 
           purrr::pluck("total_score", 1)
        ) >= 
        current_game_max_score()
      
      if (hit_max_points) {
        
        print("Logging game")
        log_game(game_id)
        
      }
    }
  }
  
  # Render the previous games table
  output$recent_games <- renderReactable(
    reactable(
      previous_games_span_table(),
      highlight = TRUE,
      bordered = TRUE,
      columns = list(
        game_id = colDef(
          show = FALSE
        ),
        game_dttm = colDef(
          name = "Date Played",
          format = colFormat(date = TRUE),
          style = list(fontSize="19px"),
          align = "center",
          vAlign = "center"
        ),
        game_type = colDef(
          name = "Game Type",
          align = "center",
          html = TRUE,
          vAlign = "center"
        ),
        rounds = colDef(
          name = "Rounds Played",
          align = "center",
          vAlign = "center",
          style = list(fontSize="19px")
        ),
        winner = colDef(
          name = "Game Winner",
          html = TRUE,
          align = "center",
          vAlign = "center"
        ),
        scoreboard = colDef(
          name = "Final Scores",
          html = TRUE,
          align = "center",
          vAlign = "center"
        )
      ),
      theme = reactableTheme(
        borderColor = "#C6A15B",
        backgroundColor = "#C6A15B",
        borderWidth = "5px",
        headerStyle = list(
          backgroundColor = "#586F7C",
          color = "#F4F4F9"
        ),
        cellStyle = list(
          backgroundColor = "#F2E3BC",
          color = "#586F7C"
        ),
        highlightColor = "#f0f5f9",
        cellPadding = "8px 12px",
        #style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
        searchInputStyle = list(width = "100%")
      )
    )
  )
}

shinyApp(ui = ui, server = server)
