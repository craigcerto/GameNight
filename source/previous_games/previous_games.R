
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