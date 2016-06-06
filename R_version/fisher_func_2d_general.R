library("plotrix")
# fisher equation for two dimensions
# assumes homogenous habitat

#*************CALCULATIONS WITH MATRICES*********************
# http://www.r-bloggers.com/fast-conways-game-of-life-in-r/
# make 4 copies of the original matrix
# in copy 1- delete leftmost column and add column of zeros to the very right
# in copy 2- do the opposite
# in copy 3- delete uppermost row and add row of zeros to bottom
# in copy 4- delete the row at hte bottom and add a row of zeros to the very top
#  stack all the copies (sum them up - gives a new array of Ni values)
#***********************************************************

#**************************FUNCTIONS******************************
shift.matrix <- function( mat=flies ) { # matrix of numbers of tsetse
  matN <- rbind( mat[1,], mat[-nrow(mat),] ) # shift matrix down 1 cell - append the first row onto the top - reflective boundary
  matS <- rbind( mat[-1,], mat[nrow(mat),] ) # shift matrix up 1 cell
  matE <- cbind( mat[,-1], mat[,ncol(mat)] ) # shift matrix left 1 cell
  matW <- cbind( mat[,1], mat[,-ncol(mat)] ) # shift matrix right 1 cell
  return(array( data=c(matN,matS,matE,matW), # stack the matrices into an array
                dim=c(length(matN[,1]), length(matN[1,]),4),
                dimnames=list(NULL,NULL,c("N","S","E","W"))))
}

fisher.func <- function(flies, # matrix of numbers of flies at time t
                        flies.shift, # array resulting from shift.matrix
                        alpha=diff.coef, # diffusion coefficient
                        r=growth.rate, # growth rate of tsetse
                        K=carry.cap){ # carrying capacity
  growth <- flies * r * (1 - (flies / K))
  diffusion <- flies + 
    alpha * flies.shift[,,"N"] +
    alpha * flies.shift[,,"S"] +
    alpha * flies.shift[,,"E"] +
    alpha * flies.shift[,,"W"] -
    alpha*4*flies
  
  new_vals <- growth + diffusion
  return(new_vals)
}
#*****************************PARAMETERS*****************************
# assume 1km grid cells
# m2 displacement xm / day diffusion coef therefore x / 4

area.length <- 50 # length and width of area in km
area.width <- 50

starting.flies <- 10 # number of flies in each cell at start

growth.rate <- 0.015 # per day
carry.cap <- 100
diff.coef <- 0.125 # could be replaced by habtiat map coded with cell-specific diff coefs
days <- 365 # days to run simulation

output <- array(0,dim=c(area.width,area.length,days))

#*******************starting matrix of flies (day 1)***********************
output[,,1] <- matrix( rep(starting.flies,area.length*area.width),
                       nrow=area.width, ncol=area.length ) # start with 10 flies in each cell 


#**********************RUN MODEL************************************
for (i in 2:days) {
  output.shift <- shift.matrix(output[,,i-1])
  output[,,i] <- fisher.func(flies=output[,,i-1],flies.shift=output.shift)
}
#**********************PLOT*************************************

plot(1:days,output[25,25,1:days],bty="n",ylab="Numbers of flies per km2", xlab="Time (days)",
     type="l",col="blue",cex.main=0.6, main=paste("Fisher equation example with: \n starting flies =",starting.flies,
                                     "growth rate =",growth.rate,
                                     "carrying capacity =", carry.cap,
                                     "diffusion coef. =", diff.coef, sep=" "))
# currently working on spatial plots
#cellcol <- color.scale(output[,,days],c(0,1),0,c(1,0))
#color2D.matplot(output[,,days],cellcolors=cellcol)
