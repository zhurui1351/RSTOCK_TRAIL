library(imager)
im <- load.image(system.file('extdata/parrots.png',package='imager'))
nhood <- expand.grid(dx=-2:2,dy=-2:2) #We want to include all pixels in a square 5x5 neighbourhood
im.s <- alply(nhood,1,function(d) imshift(im,d$dx,d$dy))
max.filt <- do.call(pmax,im.s)
min.filt <- do.call(pmin,im.s)