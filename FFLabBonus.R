cat("\014") # clear screen
rm(list=ls()) # clear global environment
setwd("/Users/Nicholas/Documents/MDrive-Nick/ISA321")
fantasyFootball <- read.csv("FanDuel-NFL-2017-11-19-21941-players-list.csv")
install.packages("lpSolve")
install.packages("lpSolveAPI")
library(lpSolve)
library(lpSolveAPI)

QB <- rep.int(0, times = 502)
RB <- rep.int(0, times = 502)
WR <- rep.int(0, times = 502)
TE <- rep.int(0, times = 502)
K <- rep.int(0, times = 502)
Def <- rep.int(0, times = 502)
count <-0

################ START OF SETTING UP THE COEFFICIENTS FOR QB, RB, WR, TE, DEF, FLEX #######################


Position <- c(fantasyFootball[,"Position"])
Position
Cost <- c(fantasyFootball[,"Salary"])
Points <- c(fantasyFootball[,"FPPG"])

for(i in Position){
  count = count + 1
  if(i == 3){
    QB[count] = 1
  }else if(i == 5){
    TE[count] = 1
  }else if(i == 6){
    WR[count] = 1
  }else if(i == 4){
    RB[count] = 1
  }else if(i == 1){
    Def[count] = 1
  }else if(i == 2){
    K[count] = 1
  }
}
QB1 <- data.frame(QB)
RB1 <- data.frame(RB)
WR1 <- data.frame(WR)
TE1 <- data.frame(TE)
K1 <- data.frame(K)
Def1 <- data.frame(Def)

################ END OF SETTING UP THE COEFFICIENTS FOR QB, RB, WR, TE, DEF, FLEX #######################
newTeam <- rep.int(0,times = 502)
newTeam[3]= 1
newTeam[6]=1
newTeam[18]=1
newTeam[22]=1
newTeam[31]=1
newTeam[167]=1
newTeam[219]=1
newTeam[240]=1
newTeam[485] =1



f.con <- matrix(c(Cost,  QB,  RB,  WR,  TE,  Def,  K, newTeam), nrow = 8, byrow = TRUE)
f.obj <- Points
f.dir <- c("<=", "=", "=", "=", "=", "=", "=", "<=")
f.rhs <- c(60000, 1, 2, 3, 1, 1, 1,9)

LP_Solution <- lp ("max", f.obj, f.con, f.dir, f.rhs,all.bin=T)
LP_Solution$objval  
LP_Solution$solution
Team <- data.frame(LP_Solution$solution)

