library(terra)
library(spatstat)
library(glue)



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
  
  raster.im = as.im(data.frame(x = X, y = Y, z = v1))
  
  if(plot) plot(raster.im, main = "RandomGradient")
  
  raster.im
  
}

AddNoise <- function(Rast, noise = 0.1, plot = TRUE){
  l_vec <- length(Rast$v)
  add_noise <- rnorm(l_vec, 0, noise)
  
  Rast$v <- Rast$v + add_noise
  
  if(plot) plot(Rast, main = "AddNoise")
  Rast
}

MirrorImage = function(Rast, direction = "horizontal",  plot = TRUE){
  
  if(direction == "horizontal"){
    mat1 <- apply(Rast$v, 2, rev)
    m_raster = as.im(t(mat1))
    
  }
  
  if(direction == "vertical"){
    mat1 <- apply(Rast$v, 2, rev)
    m_raster = as.im(mat1)
  }
 
  if(plot) plot(m_raster, main = "MirrorImage")
  
  m_raster
  
}

set.seed(254)
rast_img <- RandomGradient(20,20)

set.seed(1125)
rast_img <- RandomGradient(20,20)

set.seed(5678)
rast_img <- RandomGradient(20,20)


  MirrorImage(rast_img, direction = "vertical") |> 
    AddNoise(.001)
