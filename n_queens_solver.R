  InitializeBoard <- function(n.queens) {
    chess.board <- vector("list", 0)
    chess.board$queens <- vector(mode = "integer", length = n.queens)
    chess.board$column.j <- 0
    chess.board$column <- rep(TRUE, n.queens)
    chess.board$diagonal.up <- rep(TRUE, 2 * n.queens - 1)
    chess.board$diagonal.down <- rep(TRUE, 2 * n.queens - 1)
    return(chess.board)
  }
  
  # Vérifier si une reine peut être placée sur la case actuelle
  SquareIsFree <- function(chess.board, n.queens, row.i) {
    return(chess.board$column[row.i + 1] &
             chess.board$diagonal.up[n.queens + chess.board$column.j - row.i] &
             chess.board$diagonal.down[chess.board$column.j + row.i + 1])
  }
  
  # Place une reine sur l'échiquier NxN dans la colonne donnée
  SetQueen <- function(chess.board, n.queens, row.i) {
    chess.board$queens[chess.board$column.j + 1] <- row.i + 1
    chess.board$column[row.i + 1] <- FALSE
    chess.board$diagonal.up[n.queens + chess.board$column.j - row.i] <- FALSE
    chess.board$diagonal.down[chess.board$column.j + row.i + 1] <- FALSE
    chess.board$column.j <- chess.board$column.j + 1
    return(chess.board)
  }
  
  # Supprime une reine de l'échiquier NxN dans la colonne donnée pour revenir en arrière
  RemoveQueen <- function(chess.board, n.queens, row.i) {
    chess.board$column.j <- chess.board$column.j - 1
    chess.board$column[row.i + 1] <- TRUE
    chess.board$diagonal.up[n.queens + chess.board$column.j - row.i] <- TRUE
    chess.board$diagonal.down[chess.board$column.j + row.i + 1] <- TRUE
    return(chess.board)
  }
  
  # Fonction récursive pour trouver des placements de reine valides sur l'échiquier
  PlaceNextQueen <- function(chess.board, n.queens, scope) {
    for (row.i in 0:(n.queens-1)) {
      if (SquareIsFree(chess.board, n.queens, row.i)) {
        chess.board <- SetQueen(chess.board, n.queens, row.i)
        if (chess.board$column.j == n.queens) {
          solved.list <- get('solutions', envir = scope)
          solved.list[length(solved.list) + 1] <- paste(chess.board$queens,
                                                        collapse = " ", sep = "")
          assign('solutions', solved.list, envir = scope)
        } else {
          PlaceNextQueen(chess.board, n.queens, scope)
        }
        chess.board <- RemoveQueen(chess.board, n.queens, row.i)
      }
    }
  }
  
  # Formate la sortie des solutions
  OutputSolutions <- function(solutions, n.queens, filename) {
    if (length(solutions) == 0) {
      cat(paste("No solutions were found for the ", n.queens,
                "-Queens problem:\n", collapse = "", sep = ""))
    } else {
      s<- solutions[1]
      cat(paste("The solver found ", length(solutions), " solutions for the ",
                n.queens, "-Queens problem\n", collapse = "", sep = ""),
          file = filename, append = FALSE)
      solutions <- paste("Solution ", seq(1, length(solutions)), ":\t", solutions,
                         collapse = NULL, sep = "")
      cat(solutions, file = filename, sep = "\n", append = TRUE)
    }
    
    g<-matrix(0, n.queens, n.queens)
    i <- 1
    a<- 1
    while (i < 2*n.queens) {
      
      j<- substr(s, i, i)
      j<-strtoi(j)
      g[a, j]<- 1
      
      i <- i + 2
      a <- a + 1
    }
    cat("afficher la première solution (1 représente une reine) :", "\n")
    print(g)
    
  }
  
  
  # Point de départ du solveur N-Queens
  SolveNQueens <- function(n.queens, filename = "") {
    if (n.queens > 0) {
      solver.env <- new.env()
      solver.env$solutions <- character(0)
      PlaceNextQueen(InitializeBoard(n.queens), n.queens, solver.env)
      OutputSolutions(solver.env$solutions, n.queens, filename)
    }
  }
  SolveNQueens(5)
