#' Calculate race pace 50's depending on stroke and goal time for 100 yard or meter, specified by user.
#'
#' @param goal_time The time that the swimmer wants to achieve in their race.
#' @param stroke The stroke the swimmer is swimming in the race.
#'
#' @return Print pace time.
#' @export
#'
#' @examples
#' pace(55, "butterfly")
#' pace(60, "free")
#' pace(70, "breast")
pace_100 <- function(goal_time, stroke){
  stroke <- tolower(stroke)
  if(stroke == "free" || stroke == "freestyle" || stroke == "back" || stroke == "backstroke"){
    temp <- goal_time/2
    print(paste("Your first 50 should be", temp - 1, "seconds."))
    print(paste("Your second 50 should be", temp + 1, "seconds."))
  }
  else if(stroke == "breast" || stroke == "breaststroke" || stroke == "fly" || stroke == "butterfly"){
    temp <- goal_time / 2
    print(paste("Your first 50 should be", temp - 2, "seconds."))
    print(paste("Your second 50 should be", temp + 2, "seconds."))
  }
  else {stop("Invalid stroke style.", call. = FALSE)
  }
}
