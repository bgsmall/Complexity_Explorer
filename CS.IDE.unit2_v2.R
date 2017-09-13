# Complexity Explorer - Introduction to Differential Equations - Unit 2 Homework
# URL: https://www.complexityexplorer.org/tutorials/31-introduction-to-differential-equations/segments/incomplete?summary
# Date: 29/03/17
# Updated: 05/04/17, 28/04/17, 16/06/17, 12/09/17

# Q3. (Advanced) Write a program that implements Euler's method for the main 
# example of this unit, dT/dt= 0.2 * (20 - T).
# Some things to try out or experiment with:
# (i) Have your program produce a plot of your Euler solution.
# (ii) Make plots of the Euler solutions for several different values of delta t.
# (iii) Compare the Euler solution with the exact solution
# T(t) = 20 - 5e^[-0.2t].
# (This analytic result is obtained via calculus, using T(0) = 15.)
# (iv) Generalize your program so that it can solve any differential equation of 
# the form dX / dt = f (X). - NOT DONE HERE

################################################################################
  # LOAD LIBRARIES, DIRECTORIES AND GENERAL PARAMETERS (Colour, shape in plots)
################################################################################

  # load libraries
  library(deSolve)
  library(ggplot2)
  library(reshape2)
  library(grid)
  library(gridExtra)

  # The palette with black and grey and additional deep red ("#990000"):
  cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", 
                  "#F0E442", "#0072B2", "#D55E00", 
                  "#CC79A7", "#990000","#000000", 
                  "#999999")

  # Set working directory 
  setwd(" ") # Insert working directory location
  
  # load data
  #load("CS.IDE.unit2.RData") # Uncomment this line if you want to load data 

################################################################################
  # PARAMETERS
################################################################################

  parameters <- c(x <- 0.2,
                  y <- 20)
  
################################################################################
  # STATE VARIABLES - ENSURE assignment operator is '=' NOT '<-' within 'state'
################################################################################
  
  state <- c(T = 10)
  
################################################################################
  # MODEL
################################################################################
  
  temp <- function (t, state, parameters){
    with(as.list(c(state, parameters)),{ #this line converts the 'state' and 
                                         #'parameter' vectors to lists
      #rate of change
      dT <- x * (y - T)
      # return the rates of change
      return(list(dT))
    })  #end with list
  }

  # Q3(i)
  # Define times over which simulation is run
  # TIME OF SIMULATION
  times1 <- seq(0,40, by = 1)
  
  # Q3(ii)
  # Now define the vector of timesteps to try:
  stepvec <- c(0.1,1.0,10)
  
  # and define a list to hold the results:
  res <- vector(length(stepvec),mode="list")
  
  # take each value of 'x' and create a sequence dependent on 'x'
  for(k in seq_along(stepvec)){
    res[[k]] <- ode(y=state, 
                    times=(seq(0,100,by=stepvec[k])),
                    func=temp,
                    parms=parameters)
  }
  
################################################################################
  # OUTPUT
################################################################################
  
  # Q3(i) - 'out' is the dataframe containing the results fro Q3(i)
  out <- ode(state, times1, temp, parameters)
  df.out <- as.data.frame(out)
  
  # Q3(ii) - 'dd' is the dataframe containing the results for Q3(ii)
  names(res) <- stepvec  ## to get stepvec values incorporated in results
  dd <- dplyr::bind_rows(lapply(res,as.data.frame),.id="step")
  dd$step <- as.numeric(dd$step)
  
################################################################################
  # ANALYTIC RESULT 
################################################################################
  
  # time vector
  times2 <- seq(1,100,0.1)
  
  # function
  temp.analy <- function(y){
    T = 20 - (5 * (exp(-0.2 * y)))
    print(T)
  }
  
  # using sapply to apply 'temp.analy' over time vector 'y'
  temp.res <- sapply(times2, temp.analy)
  
  # changes to ANALYTICAL dataset for combing with numerical
  temp.res2 <- cbind.data.frame(times2,temp.res)
  colnames(temp.res2) <- c("time","T")
  temp.res2["code"] <- "analytical"
  
  # changes to NUMERICAL dataset for combing with analytical
  dd.sub <- (dd[1:1001,])
  dd.sub <- dd.sub[,-1]
  dd.sub["code"] <- "numerical" 
  
  # combine ANALYTICAL dataset (by row) with NUMERICAL dataset
  comb.d <- rbind.data.frame(temp.res2,dd.sub)

################################################################################
  # PLOTTING
################################################################################
  
  #Q3(i)
  temp.p <- ggplot(df.out, aes(time,T)) + 
  geom_point()
  
  #Q3(ii)
  temp.p2 <- ggplot(dd, aes(time,T,colour=factor(step))) + 
    geom_point() +
    scale_color_discrete()
  
  #Q3(ii)
  temp.p3 <- ggplot(dd, aes(time,T,colour=factor(step))) + 
    geom_point() +
    scale_color_discrete() +
    facet_wrap(~step)
  
  # Analytic result plot
  temp.res2.p <- ggplot(temp.res2, aes(times2,temp.res)) +
    geom_point() 
  
  # Comparison plot
  comb.p <- ggplot(comb.d, aes(time,T,colour=code)) +
    geom_point() +
    scale_color_discrete()
   

################################################################################
  # SAVE IMAGE
################################################################################
  
  save.image("CS.IDE.unit2.RData")

################################################################################

################################################################################
