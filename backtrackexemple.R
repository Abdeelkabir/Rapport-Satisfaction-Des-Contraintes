library(hash)
d <- hash() 
d[["x"]] <-list(1, 2)
d[["y"]] <- list(2, 3, 4)
d[["z"]] <- list(4, 7)

v<- c("x", "y", "z")
Assignment<- hash() 
Assignment[["x"]] <- 3
Assignment[["y"]] <- 1
Assignment[["z"]] <- 4

is_complete <- function(Assignment) {
  for (x in values(Assignment)){
    if (is.null(x)){
      return(FALSE)
    }
  }
  return(TRUE)
}


init_Assignment <- function(v) { 
  Assignment <- hash() 
  for (x in v) {
    .set(Assignment, x, NULL)
  }
  return( Assignment )
}


constraint_1 <- function(Assignment) { 
  return((Assignment[["x"]]!= Assignment[["y"]]) & (Assignment[["x"]]!= Assignment[["z"]]) & (Assignment[["z"]]!= Assignment[["y"]]) )
}

constraint_2 <- function(Assignment) { 
  return((Assignment[["x"]] + Assignment[["y"]]) == Assignment[["z"]]) 
}

select_unassigned_variable <- function(v, Assignment) { 
  for (var in v){
    if (is.null(Assignment[[var]])){
      return(var)
    }
  } 
}

is_consistent <- function(Assignment) {
 if (!is_complete(Assignment)){
   return(TRUE)
 }  
 return(constraint_1(Assignment) & constraint_2(Assignment))
}

sol<-list(NA)
append(sol, 1)
backtrack_search <- function(v, d, Assignment) { 
  
  if (is_complete(Assignment)){
    sol<<- append(sol, Assignment) 
    return(Assignment)
  }
  
  var <- select_unassigned_variable(v,Assignment)
  for( value in d[[var]]){
    Assignment[[var]]<- value
    print(Assignment) # utile pour observer le retour en arrière en action
    if ( is_consistent(Assignment) ){
      result <- backtrack_search(v, d, Assignment)
      if (!is.null(result)){
        return(result)
      }
    }
    .set(Assignment, var , NULL)
  }
  
  return(NULL)
}



a<- init_Assignment(v)


backtrack_search(v, d, a)
sol
