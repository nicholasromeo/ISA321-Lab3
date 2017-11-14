cat("\014") # clear screen
setwd("M:/ISA321")
fantasyFootball <- read.csv("18-fantasy-football-class-example.csv")
install.packages("lpSolve")
install.packages("lpSolveAPI")
library(lpSolve)
library(lpSolveAPI)

################ START OF SETTING UP THE COEFFICIENTS FOR QB, RB, WR, TE, DEF, FLEX #######################
QB <- {}
QB <- rep.int(1, times = 24)
QB[25:200] = rep.int(0,times = 176)
QB_Con <- data.frame(QB)
RB <- {}
RB <- rep.int(0, times = 24)
RB[25:88] = rep.int(1,times = 59)
RB[89:200] = rep.int(0, times = 117)
RB[198] = -1
RB_Con <- data.frame(RB)
WR <- {};
WR <- rep.int(0, times = 88)
WR[89:159] = rep.int(1, times = 66)
WR[160:200] = rep.int(0, times = 40)
WR[199] = -1
WR_Con <- data.frame(WR)
TE <- {};
TE <- rep.int(0,times = 159)
TE[160:185] = rep.int(1, times = 24)
TE[186:200] = rep.int(0, times = 14)
TE[200] = -1
TE_Con <- data.frame(TE)
DEF <- {}
DEF <- rep.int(0, times = 185)
DEF[186:197] = rep.int(1, times = 11)
DEF[198:200] = rep.int(0, times = 3)
DEF_Con <- data.frame(DEF)
FLEX <- {}
FLEX <- rep.int(0,times = 197)
FLEX[198:200] = rep.int(1, times = 3)
FLEX_Con <- data.frame(FLEX)
################ END OF SETTING UP THE COEFFICIENTS FOR QB, RB, WR, TE, DEF, FLEX #######################
#we first only had 7 constraints...Once we found our most optimal team, in order to develop
#other optimal teams we made a new constraint called newTeam whose coefficinets correlated
#to our most optimal team. We added this constraint to our matrix so that the next team
#produced was different than the optimal team. We repeated this 9 times, adjusting what the value
#for the new team should be less than or equal to in order to get different players than the previously 
#made team


NewTeam <- rep.int(0, times = 200)
NewTeam[2] <- 1
NewTeam[35] = 1
NewTeam[43] = 1
NewTeam[89] = 1
NewTeam[91] = 1
NewTeam[96] = 1
NewTeam[101] = 1
NewTeam[162] = 1
NewTeam[186] = 1

Projpoints <- c(fantasyFootball[,"Projected.Points"])
Cost <- c(fantasyFootball[,"Cost"])
f.con <- matrix(c(Cost,  QB,  RB,  WR,  TE,  DEF,  FLEX, NewTeam), nrow = 8, byrow = TRUE)
f.obj <- Projpoints
f.dir <- c("<=", "=", "=", "=", "=", "=", "=","<=")
f.rhs <- c(200, 1, 2, 3, 1, 1, 1,8)  #having the last constraint number be 8 means that our new optimal team should be at least one person different than the original optimal team

LP_Solution <- lp ("max", f.obj, f.con, f.dir, f.rhs,all.bin=T)
LP_Solution$objval  
LP_Solution$solution
Team <- data.frame(LP_Solution$solution)
