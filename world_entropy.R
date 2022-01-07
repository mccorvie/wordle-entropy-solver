library( tidyverse)
library( doParallel)

wordle_solution <- str_split(sort(readLines( "wordle_solution.csv")), "" )
wordle_valid    <- str_split(sort(readLines( "wordle_valid.csv")), "")

wordle_solution_words <- sort(readLines( "wordle_solution.csv"))
includes_let <- map( letters, ~ str_subset( wordle_solution_words, .)) 
excludes_let <- map( letters, ~ str_subset( wordle_solution_words, ., negate=T) )
names( includes_let ) <- names( excludes_let ) <- letters


wordle_freq <- sort(table(unlist(map( str_split( wordle_valid, ""), unique))), decreasing=T)
#wordle_valid_full <- keep( str_split( wordle_valid, ""), ~ length( unique(.))==5) 
#keep( wordle_valid_full, ~ all( . %in% names(wordle_freq)[1:5]))

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


# 
# solution <- c( "s", "l", "u", "m", "p")
# game_state <- list( black = NULL, in_solution = NULL, yellow = rep( "", 5), green = rep( "", 5))
# possible_solutions <- wordle_solution
# 
# guess <- c( "a","e","r","o","s")
# game_state <- update_state( game_state, guess, solution )
# game_state
# possible_solutions <- filter_solutions2(  game_state )
# length( possible_solutions)
# 
# guess <- c( "s", "p", "i", "l", "t" )
# game_state <- update_state( game_state, guess, solution )
# game_state
# possible_solutions <- filter_solutions2( game_state )
# length( possible_solutions)
# possible_solutions
# 
# guess <- c( "l","u","n","c","h")
# game_state <- update_state( game_state, guess, solution )
# game_state
# possible_solutions <- filter_solutions( possible_solutions, game_state )
# length( possible_solutions)
# 
# guess <- c( "m","i","s","t","y")
# game_state <- update_state( game_state, guess, solution )
# possible_solutions <- filter_solutions( possible_solutions, game_state )
# length( possible_solutions)
# game_state


game_state <- list( black = NULL, in_solution = NULL, yellow = rep( "", 5), green = rep( "", 5))
possible_solutions <- wordle_solution
possible_guesses   <- wordle_solution


# 1 / 2315 a  b  a  c  k = 451.6238 ( 31186 minutes to completion)
# 2 / 2315 a  b  a  s  e = 176.1248 ( 22020 minutes to completion)
# 3 / 2315 a  b  a  t  e = 170.3071 ( 18925 minutes to completion)
# 4 / 2315 a  b  b  e  y = 239.362 ( 18119 minutes to completion)


ll <- matrix( 0, nrow = length( possible_guesses), ncol = length( possible_solutions) )
st <- Sys.time()
cat( format(st), "\n")


cl <- makeCluster(8)
clusterExport(cl, c("n")) # Export max number of iteration to workers
registerDoParallel(cl)
  

ll <- foreach( idx = 1:length(possible_guesses), .combine = rbind, .packages = c("tidyverse","tcltk" )) %dopar%
{
  if(!exists("pb")) pb <- tkProgressBar("Parallel task", min=1, max=n)
  setTkProgressBar(pb, idx)
  
  #cat( idx, "/", length( possible_guesses), paste(  possible_guesses[[idx]], collpase=""))
  map( possible_solutions, ~ length( filter_solutions1st( possible_guesses[[idx]], .)))
  #  ll[idx,] <- map_dbl( possible_solutions, ~ length( filter_solutions_slow( possible_solutions, update_state( game_state, possible_guesses[[idx]], . ))))
}


stopCluster(cl)

for( idx in 1:length(possible_guesses))
{
  cat( idx, "/", length( possible_guesses), paste(  possible_guesses[[idx]], collpase=""))
  ll[idx,] <- map_dbl( possible_solutions, ~ length( filter_solutions1st( possible_guesses[[idx]], . )))
#  ll[idx,] <- map_dbl( possible_solutions, ~ length( filter_solutions_slow( possible_solutions, update_state( game_state, possible_guesses[[idx]], . ))))
  cat(  "=",mean( ll[idx,]), "(", round( as.numeric(Sys.time()-st)/idx * ( length(possible_guesses)-idx) ), "minutes to completion)\n" )
}


# 
# policy <- apply( ll, 2, mean)
# summary( policy)
# possible_guesses[ policy==min(policy)]
# 




