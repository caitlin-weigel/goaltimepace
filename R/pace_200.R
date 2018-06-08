#' Calculate race pace 50's depending on stroke and goal time for 200 yard or meter, specified by user.
#'
#' @param goal_time The time that the swimmer wants to achieve in their race.
#' @param stroke The stroke the swimmer is swimming in the race.
#'
#' @return Print race pace times.
#' @export
#'
#' @examples
#' pace_200(128, "breast")
#' pace_200(110, "freestyle")
pace_200 <- function(goal_time, stroke){
  stroke <- tolower(stroke)
  if(stroke == "free" || stroke == "freestyle" || stroke == "back" || stroke == "backstroke"){
    temp <- goal_time/4
    print(paste("Your first 50 should be", temp - 1.5, "seconds."))
    print(paste("Your second, third, and fourth 50 should each be around", temp + 0.5, "seconds."))
  }
  else if(stroke == "breast" || stroke == "breaststroke" || stroke == "fly" || stroke == "butterfly"){
    temp <- goal_time / 4
    first_50 <- temp - 2.5
    other_50s <- (goal_time - first_50)/ 3
    print(paste("Your first 50 should be", first_50, "seconds."))
    print(sprintf("Your second, third, and fourth 50 should each be around %.2f seconds.", other_50s))
  }
  else {stop("Invalid stroke style.", call. = FALSE)
  }
}
