library( tidyverse)

intersect(readLines( "wordle_solution.csv"),readLines( "wordle_valid.csv"))

wordle_solution_words <- sort(readLines( "wordle_solution.csv"))
wordle_valid_words <- sort(readLines( "wordle_valid.csv"))

wordle_solution <- str_split(wordle_solution_words, "" )
wordle_valid    <- str_split(wordle_valid_words, "")


includes_let <- map( letters, ~ str_subset( wordle_solution_words, .)) 
excludes_let <- map( letters, ~ str_subset( wordle_solution_words, ., negate=T) )
names( includes_let ) <- names( excludes_let ) <- letters

update_state <- function( game_state, guess, solution )
{
  game_state$in_solution <- union( game_state$in_solution, guess[guess %in% solution] )
  game_state$black <- union( game_state$black, guess[ !( guess %in% solution )])
  game_state$green[guess==solution] = guess[guess==solution]
  game_state$yellow <- if_else( guess %in% solution & guess != solution, paste0( game_state$yellow, guess), game_state$yellow )
  game_state
}

filter_solutions_slow <- function( possible_solutions, game_state )
{
  possible_solutions <- keep( possible_solutions, ~ all( game_state$green == "" | . == game_state$green ))
  possible_solutions <- keep( possible_solutions, ~ all( game_state$in_solution %in% . ))
  possible_solutions <- keep( possible_solutions, ~ !any( . %in% game_state$black ))
  possible_solutions <- keep( possible_solutions, ~ !any( str_detect( game_state$yellow, . )))
  possible_solutions
}

filter_solutions1st <- function( guess, solution )
{
  possible_solutions <- str_split( reduce( c( includes_let[ intersect( guess, solution )], excludes_let[ setdiff( guess, solution )] ), intersect ), "")
  green <- guess==solution
  possible_solutions <- keep( possible_solutions, ~ all( .[green] == solution[green] ))
  yellow <- guess %in% solution & guess != solution
  possible_solutions <- keep( possible_solutions, ~ all( .[yellow] != guess[yellow] ))
  possible_solutions
}


filter_solutions <- function( guess, solution )
{
  possible_solutions <- str_split( reduce( c( includes_let[ intersect( guess, solution )], excludes_let[ setdiff( guess, solution )] ), intersect ), "")
  green <- guess==solution
  possible_solutions <- keep( possible_solutions, ~ all( .[green] == solution[green] ))
  yellow <- guess %in% solution & guess != solution
  possible_solutions <- keep( possible_solutions, ~ all( .[yellow] != guess[yellow] ))
  possible_solutions
}

initial_game_state <- list( black = c(), in_solution = c(),yellow=rep("",5), green=rep("",5))

game_state_from_feedback <- function( guess_log, feedback_log, game_state = initial_game_state )
{
  guess_log    <- str_split( guess_log, "")
  feedback_log <- str_split( feedback_log, "")
  
  game_state_by_guess <- tibble( guess_num = 1:length( guess_log), guess_log = guess_log, feedback_log = feedback_log) %>%
    mutate( 
      black = map2( guess_log, feedback_log, ~ .x[ .y == "b"] ),
      in_solution = map2( guess_log, feedback_log, ~ .x[ .y == "y" | .y=="g"] ),
      yellow = map2( guess_log, feedback_log, ~ if_else( .y == "y", .x, "" )),
      green = map2( guess_log, feedback_log, ~ if_else( .y == "g", .x, "" )),
    )
  
  
  
  game_state <- list(
    #black = reduce( c( game_state_by_guess$black, list( game_state$yellow)), union),
    black = reduce( game_state_by_guess$black, union),
    in_solution = reduce( game_state_by_guess$in_solution, union),
    yellow = reduce( game_state_by_guess$yellow, paste0),
    green = reduce( game_state_by_guess$green, paste0)
  )
  
  game_state
}



