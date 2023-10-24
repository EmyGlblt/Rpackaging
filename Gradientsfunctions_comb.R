library(terra)
library(spatstat)
library(glue)


### create image/raster

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
  return(raster.im)
}

# specific direction or steepness of the gradient
WiggleImage = function(Rast, x=NULL, y=NULL, z=NULL, plot = TRUE){
  Grad.w = Rast
  
  if(!is.null(x)){
    X = Grad.w$xcol
    Grad.w$v = Grad.w$v + x*X
  }
  
  if(!is.null(y)){
    Y = Grad.w$yrow
    Grad.w$v = Grad.w$v + x*X + y*Y
  }
  
  Grad.w2 = Rast
  if(!is.null(z)){
    X = Grad.w2$xcol
    Y = Grad.w2$yrow
    
    Grad.w2$v = Grad.w2$v + z*X*Y
  }
  
  if(plot){
    par(mfrow=c(1,2))
    plot(Rast, main = "Original")
    
    if(!is.null(z)){
      plot(Grad.w2, main = "wiggle")
    }else{
      plot(Grad.w, main = "wiggle")
    }
    
  }
  return(Grad.w)
}

# general noise
AddNoise <- function(Rast, noise = 0.1, plot = TRUE){
  
  Rast.i = Rast
  l_vec <- length(Rast$v)
  add_noise <- rnorm(l_vec, 0, noise)
  
  Rast$v <- Rast$v + add_noise
  
  if(plot){
    par(mfrow=c(1,2))
    plot(Rast.i, main = "Original")
    plot(Rast, main = "AddNoise")
  }
  Rast
  return(Rast)
}


# mirror image
MirrorImage = function(Rast, direction = c("horizontal", "vertical"),  
                       plot = TRUE){
  
  if(direction == "horizontal"){
    mat1 <- apply(Rast$v, 2, rev)
    m_raster = as.im(t(mat1))
    
  }
  
  if(direction == "vertical"){
    mat1 <- apply(Rast$v, 1, rev)
    m_raster = as.im(t(mat1))
  }
  
  if(plot){
    par(mfrow=c(1,2))
    plot(Rast, main = "Original")
    plot(m_raster, main = "MirrorImage")
  }
  
  m_raster
  return(m_raster)
}

