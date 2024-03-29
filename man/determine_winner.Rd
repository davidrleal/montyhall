% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-pkg.R
\name{determine_winner}
\alias{determine_winner}
\title{Step 05: Winner is determined.}
\usage{
determine_winner(final.pick, game)
}
\arguments{
\item{Numeric, }{Character}
}
\value{
Character result of either 'WIN' or 'LOSE'.
}
\description{
'determine_winner()' returns game results.
}
\details{
The function uses the 'Stay' and 'Switch' arguments along with a contestant's initial (or final) pick to determine if the contestant's choice will result in winning a car or selecting a goat door and losing the game.
}
\examples{
this.game
my.initial.pick

my.final.pick <- change_door( stay=T,
                              opened.door=opened.door,
                             a.pick=my.initial.pick)

determine_winner( final.pick=my.final.pick,
                  a.game=this.game )

my.final.pick <- change_door( stay=F,
                              opened.door=opened.door,
                              a.pick=my.initial.pick)

determine_winner( final.pick=my.final.pick,
                  a.game=this.game )
}
