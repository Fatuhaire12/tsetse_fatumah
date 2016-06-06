
source("nagupande_mammals.R") # estimating numbers of mammals over time
source("nagupande_tsetse.R") # tsetse and temperature data
source("fisher_func_2d_general.R") # functions for the fisher equation

#*******************Create starting matrices*****************

# set up area simulation will take place in
# assume each cell is 1km2
area.width <- 50 # in km
area.length <- 50 

# other parameters for fisher equation
starting.flies <- 10 # number of flies in each cell at start
growth.rate <- 0.015 # per day
carry.cap <- 100
diff.coef <- 0.125 # could be replaced by habtiat map coded with cell-specific diff coefs
days <- 365 # days to run simulation

# array to store output at each time step
output <- array(0,dim=c(area.width,area.length,days))

#*******************starting matrix of flies (day 1)***********************
output[,,1] <- matrix( rep(starting.flies,area.length*area.width),
                       nrow=area.width, ncol=area.length ) # start with 10 flies in each cell 
