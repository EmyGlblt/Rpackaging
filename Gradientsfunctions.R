library(spatstat)
library(lattice)


set.seed(113)
XY = expand.grid(seq(0, 100, 1), seq(0, 100, 1))
X = XY[,1]
Y = XY[,2]

v1 = (X - 30)^2 + (Y + 80)^2 - 0.5*X*Y
v1 = -1*scale(v1)

raster.im = as.im(data.frame(x = X, y = Y, z = v1))
plot(raster.im)


### create image/raster
CreateRaster = function(x, y){

  XY = expand.grid(seq(0, 100, 1), seq(0, 100, 1))
  X = XY[,1]
  Y = XY[,2]
  
  v1 = (X - x)^2 + (Y + y)^2 - 0.5*X*Y
  v1 = -1*scale(v1)
  
  raster.im = as.im(data.frame(x = X, y = Y, z = v1))
  #plot(raster.im)
  
  return(raster.im)
}

Rast1 = CreateRaster(x=30, y=80)
plot(Rast1)


## transpose image
Rast2 = CreateRaster(x=-30, y=80)
plot(Rast2)


TransposeImage = function(Rast){
  
  mat1 <- apply(Rast$v, 2, rev)
  rast.trasnp = as.im(mat1)
  #plot(rast.trasnp)
  
  return(rast.trasnp)
}

Rast2.tr = TransposeImage(Rast2)
plot(Rast2.tr)



# make it blurry / change direction of the gradient

WiggleImage = function(Rast, x, y){
  Rast.w = Rast
  
  XY = expand.grid(seq(0, 100, 1), seq(0, 100, 1))
  X = XY[,1]
  Y = XY[,2]
  
  Rast.w$v = Rast.w$v + x*X + y*Y
  
  #plot(Rast.w)
  
  return(Rast.w)
}


Rast_change = WiggleImage(Rast = Rast1, x = -0.5, y = 0.3)
plot(Rast_change)
