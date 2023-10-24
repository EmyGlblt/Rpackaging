


#' Create a random gradient
#'
#'
#' @param n_col integer. Number of columns in the image.
#' @param n_row integer. Number of rows in the image.
#' @param plot logical. Whether the function plot the image. Default is TRUE.
#'
#' @return A pixel image (object of class "im")
#' @export
#'
#' @examples
#' RandomGradient(20, 20)
#'
#' @importFrom spatstat.geom as.im
#' @import glue
RandomGradient <- function(n_col, n_row, plot = TRUE){

  #create the image matrix
  XY = expand.grid(seq(0, n_col-1, 1), seq(0, n_row-1, 1))
  X = XY[,1]
  Y = XY[,2]

  # randomize a gradient function
  math_op <- c("-", "+")
  exp_size <- c("^1", "^2", "^3")

  op_1 <- sample(math_op, 1)
  op_2 <- sample(math_op, 1)
  op_3 <- sample(math_op, 1)
  op_4 <- sample(math_op, 1)

  exp_x <- sample(exp_size, 1)
  exp_y <- sample(exp_size, 1)

  val_1 <- sample(seq(0, 100, 1), 1)
  val_2 <- sample(seq(0, 100, 1), 1)
  val_3 <- sample(seq(0, 1, 0.01), 1)

  # set the gradient function
  str_expression <-  glue(
    "(X{exp_x} {op_1} {val_1}) {op_2} (Y{exp_y} {op_3} {val_2}) {op_4} ({val_3}*X*Y)"
  )

  # Create the raster image
  v1 <- eval(parse(text = str_expression))
  v1 = scale(v1)

  raster.im = spatstat.geom::as.im(data.frame(x = X, y = Y, z = v1))

  if(plot) plot(raster.im, main = "RandomGradient")

  raster.im

}

