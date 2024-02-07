#' @title
#'   Step 01: Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#' Try at least three times to ensure randomization is happening.
#' ##CODE CHUNK 1 UNIT TEST#' create_game()#' create_game()#' create_game()
#'
#' @export
create_game <- function()  {  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F)  return( a.game )}



#' @title
#' Step 02: Contestant makes an initial door selection.
#' @description
#' 'select_door()' generates an initial door selection in which the contestant chooses a door but does not know what is behind it.
#' @details
#' This function should only return one number, representing a blind choice by the contestant.
#' @param 
#' Numeric
#' @return 
#' The function returns a number length one numeric between 1 and 3.
#' @examples
#' Try running 'select_door()' at least three times to ensure that return is random.
#' ##CODE CHUNK 2 UNIT TEST#' select_door()#' select_door()#' select_door()
#' @export
select_door <- function(){  doors <- c(1,2,3)  a.pick <- sample(doors, size=1, replace = FALSE)    return ( a.pick )}



#' @title
#' Step 03: Host opens 'Goat' door.
#' @description
#' 'open_goat_door()' selects one of the remaining two doors that has a goat behind it.
#' @details
#' This function simulates how the host will open a door with a goat behind it once the contestant makes the initial selection. 
#' The contestant, however, does not get to open the door of the initial selection at this point.
#' @param
#' Character, Numeric 
#' @return
#' The function returns a number length one numeric between 1 and 3. 
#' @examples
#' Try running 'open_goat_door()' at least three times to ensure that return is random.
#' open_goat_door()
#' ##CODE CHUNK 3 UNIT TEST#'#' this.game <- create_game()#'#' this.game#'#' my.initial.pick <- select_door()#'#' my.initial.pick#'#' open_goat_door( this.game, my.initial.pick)#'#' open_goat_door( this.game, my.initial.pick)#'#' open_goat_door( this.game, my.initial.pick)
#' @export
open_goat_door <- function( a.game, a.pick){  doors <- c(1,2,3)  if( a.game [ a.pick ] == "car" )  {    goat.doors <- doors [ a.game != "car" ]    opened.door <- sample( goat.doors, size=1)  }  if( a.game [ a.pick ] == "goat" )  {    opened.door <- doors[ a.game != "car" & doors != a.pick ]  }  return( opened.door )}



#' @title
#' Step 04: Contestant has opportunity to change door selection.
#' @description
#' 'change_door()' allows the contestant to adpot a stratgy to either 'Stay' or 'Switch' to increase chances of winning.
#' @details
#' This function simulates a contestant making a final pick, choosing either to 'Stay' with the initial door selection, or 
#' choosing to 'Switch and forfeit the initial selection in favor of the third, remaining door in hopes of selecting the 
#' door with the car behind it.
#' @param
#' Numeric 
#' @return
#' If the contestant decides to stay (stay=T), the initial pick number is returned. If the contestant decides to switch (stay=F), the final pick is returned.
#' @examples
#' change_door()
#' ##CODE CHUNK 4 UNIT TEST#' #' opened.door <- open_goat_door( this.game, my.initial.pick)#' #' change_door( stay=T,#'              opened.door=opened.door,#'              a.pick=my.initial.pick )#' change_door( stay=F,#'              opened.door=opened.door,#'              a.pick=my.initial.pick )#' #' my.final.pick <- change_door( stay=F,#'                               opened.door=opened.door,#'                               a.pick=my.initial.pick)#' #' this.game#' my.initial.pick#' my.final.pick
#' @export
change_door <- function( stay=T, opened.door, a.pick ){   doors <- c(1,2,3)       if( stay )   {     final.pick <- a.pick   }   if( ! stay )   {     final.pick <- doors[ doors != opened.door & doors != a.pick ]    }     return( final.pick )}



#' @title
#' Step 05: Winner is determined.
#' @description
#' 'determine_winner()' returns game results.
#' @details
#' The function uses the 'Stay' and 'Switch' arguments along with a contestant's initial (or final) pick to determine if the contestant's choice will result in winning a car or selecting a goat door and losing the game.
#' @param Numeric, Character
#' @return
#' Character result of either 'WIN' or 'LOSE'. 
#' @examples
#' this.game#' my.initial.pick#'#' my.final.pick <- change_door( stay=T,#'                               opened.door=opened.door,#'                              a.pick=my.initial.pick)#' #' determine_winner( final.pick=my.final.pick,#'                   a.game=this.game )#' #' my.final.pick <- change_door( stay=F,#'                               opened.door=opened.door,#'                               a.pick=my.initial.pick)#' #' determine_winner( final.pick=my.final.pick,#'                   a.game=this.game )
#' @export
determine_winner <- function(final.pick, game) determine_winner <- function( final.pick, a.game ){   if( a.game[ final.pick ] == "car" )   {      return( "WIN" )   }   if( a.game[ final.pick ] == "goat" )   {      return( "LOSE" )   }}





#' @title
#' Step 06: Run full game.
#' @description
#' This function runs the game from beginning to end.
#' @details
#' 'play_game()' simluates a contestant choosing to STAY and SWITCH their initial door selection after starting a new randomized game.
#' @param
#' Character, Numeric 
#' @return 
#' 'play_game()' returns a 2-column, 2-row table that provides both contestant strategy options in Column 1 (STAY, SWITCH), and 
#' the result (WIN, LOSE) based on the game created and contestant choice.
#' @examples
#' play_game()
#' @export
play_game <- function( ){  new.game <- create_game()  first.pick <- select_door()  opened.door <- open_goat_door( new.game, first.pick )  final.pick.stay <- change_door( stay=T, opened.door, first.pick )  final.pick.switch <- change_door( stay=F, opened.door, first.pick )  outcome.stay <- determine_winner( final.pick.stay, new.game  )  outcome.switch <- determine_winner( final.pick.switch, new.game )    # game.results <- bundle the results  # return( <<< game.results >>> )    strategy <- c("stay","switch")  outcome <- c(outcome.stay,outcome.switch)  game.results <- data.frame( strategy, outcome,                              stringsAsFactors=F )  return( game.results )}






#' @title
#' Step 07: Multiple Game Simulations
#' @description
#' 'play_n_games()' runs 100 simulations of the game in Step 06.
#' @details
#' This function produces a table reporting the results of 100 simulations using pairs of results for the STAY/SWITCH strategies.
#' @param 
#' Vector
#' @return
#' This function returns a 2-column, 200-row table that provides both contestant strategy options in Column 1 (STAY, SWITCH), and 
#' the result (WIN, LOSE) based on the game created and contestant choice.
#' @examples
#' play_n_games()
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}

