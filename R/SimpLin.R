

SimpLinR <- function(x,y) {

if (length(x) != length(y)){

  stop(("X and Y do not have the same length"))

} else if (is.numeric(x) = "FALSE"){

  stop(("X is not numeric"))

} else if (is.numeric(y) = "FALSE"){

  stop(("Y is not numeric"))

} else{

  return(SimpLin(x,y))
}

}

