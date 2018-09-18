## MP 2 - TRAFFIC
# Libraries:
library(XLConnectJars)
library(foreign)
library(XLConnect)
library(car)

#Big M for Large numbers:
M <- 1000000

# 1) Coordinates of each action in km:
length_section <- 5
S0_2_C <- 0.2 
S0_6_E9 <- 0.6
S1_1_E10 <- 1.1
S1_7_radar <- 1.7
S2_9_S11 <- 2.9
S3_9_E12 <- 3.9
S5_0_D <- 5
# amount of lanes:
lanes_AC <- 2
lanes_BC <- 1
lanes_CD <- 3

# 2) Traffic parameters:
#Capacity q in veh/h/lane:
q <- 2000
#Jam density Kj in veh/km/lane
kj <- 140
#Free flow speed in km/h
vf <- 100
#Characteristic wave speed in km/h
w <- q/((q/vf)-kj)
#Slope of char wave speed in newell's notation: w/vf = delta
delta <- w/vf

# 3) import data from excel file:###########################################################
load_data <- loadWorkbook("MP2_Volumes.xlsx")
demand <- na.omit(readWorksheet(load_data, sheet = 1, startRow = 2))

# 4) fix time
Time <- matrix(c(""), nrow = length(demand[,1]), ncol = 1)
for(i in 1:length(demand[,1])){
	Time[i,1] <- toString(demand[i,1])
	next
}
Time <- strptime(Time, "%Y-%m-%d %H:%M:%S", tz = "UTC")+50*60+39
Time <- format(Time, "%H:%M")
head(Time)
demand[,1] <- Time
head(demand)

#5) Determine the timestep for a given length of the interval:
# veh/min -> veh/timestap:
# delta(X) >= vf*delta(t), choose delta(x):
delta_x <- 100 #m which is the max delta X that can be taken to have homogeneous cells
delta_t <- ((3600*(delta_x/1000))/(vf)) #in seconds

#6) Adjusted matrix with right Mlows:
adj_demand <- matrix(c(0.0),nrow = length(demand[,1])*60/delta_t, ncol = length(demand[1,])-1)
colnames(adj_demand) <- colnames(demand[,c(2:length(demand[1,]))])
for(i in 1:length(adj_demand[,1])){
	X <- (floor(i*delta_t/60)+1) #Determines in which timestamp of the original data, the flow is taken
	Y <- ((i*delta_t)%%60) #Determines the modulus of the i'th timestap in comparison to length of 1 timestamp (60secs)
	#Go through all measured flows A B E9 E10 E12
	for(k in 1:length(adj_demand[1,])){
		if(Y < delta_t & Y != 0){
			adj_demand[i,k] <- ((Y*demand[X,k+1])+((delta_t-Y)*demand[X-1,k+1]))/60
		} else if(Y == 0){
			adj_demand[i,k] <- delta_t*demand[X-1,k+1]/60
		} else if(Y >= delta_t){
			adj_demand[i,k] <- delta_t*demand[X,k+1]/60
		}
		next
	}
	next
}
head(adj_demand,n=18)

#6) Given data of fundamental diagram per section:
#section AC:
Q_AC <- q*(delta_t/3600)*lanes_AC
N_AC <- kj*(delta_x/1000)*lanes_AC
slope_AC <- round(abs(Q_AC/(Q_AC-N_AC)),5)

#section BC:
Q_BC <- q*(delta_t/3600)*lanes_BC
N_BC <- kj*(delta_x/1000)*lanes_BC
slope_BC <- round(abs(Q_BC/(Q_BC-N_BC)),5)

#section CD:
Q_CD <- q*(delta_t/3600)*lanes_CD
N_CD <- kj*(delta_x/1000)*lanes_CD
slope_CD <- round(abs(Q_CD/(Q_CD-N_CD)),5)

#7) Boundary conditions for Mlows freeway/on-ramps (i = 0):
# a) Source Node:
# Capacity in terms of vehicles is equal to the Mlow of the first cell at timestep 1:
Q_source_A <- adj_demand[,1]
Q_source_B <- adj_demand[,2]
Q_source_E9 <- adj_demand[,3]
Q_source_E10 <- adj_demand[,4]
Q_source_E12 <- adj_demand[,5]
# Jam density in terms of vehicles:
N_source <- M
n_source <- M
# b) Sink Node:
Q_sink <- M
Q_onramp <- M
N_sink <- M
N_onramp <- M
n_sink <- 0

# Make dataframe of given data for any cell and flow:
# cells A - C = 2 cells, 3 flows:
cells_AC <- matrix(c(0.0),nrow=(length(adj_demand[,1])),ncol=round(S0_2_C/(delta_x/1000)))
flow_AC <- matrix(c(0.0),nrow=(length(adj_demand[,1])),ncol=round(S0_2_C/(delta_x/1000))+1)
S_AC <- matrix(c(0.0),nrow=(length(adj_demand[,1])),ncol=round(S0_2_C/(delta_x/1000))+1)
R_AC <- matrix(c(0.0),nrow=(length(adj_demand[,1])),ncol=round(S0_2_C/(delta_x/1000))+1)
# cells B - C = 2 cells, 3 flows:
cells_BC <- matrix(c(0.0),nrow=(length(adj_demand[,1])),ncol=round(S0_2_C/(delta_x/1000)))
flow_BC <- matrix(c(0.0),nrow=(length(adj_demand[,1])),ncol=round(S0_2_C/(delta_x/1000))+1)
S_BC <- matrix(c(0.0),nrow=(length(adj_demand[,1])),ncol=round(S0_2_C/(delta_x/1000))+1)
R_BC <- matrix(c(0.0),nrow=(length(adj_demand[,1])),ncol=round(S0_2_C/(delta_x/1000))+1)

# cells C - D = 48 cells, 49 flows:
cells_CD <- matrix(c(0.0),nrow=(length(adj_demand[,1])),ncol=round((S5_0_D-S0_2_C)/(delta_x/1000)))
flow_CD <- matrix(c(0.0),nrow=(length(adj_demand[,1])),ncol=round((S5_0_D-S0_2_C)/(delta_x/1000))+1)
S_CD <- matrix(c(0.0),nrow=(length(adj_demand[,1])),ncol=round((S5_0_D-S0_2_C)/(delta_x/1000))+1)
R_CD <- matrix(c(0.0),nrow=(length(adj_demand[,1])),ncol=round((S5_0_D-S0_2_C)/(delta_x/1000))+1)

# On/Off-ramp cells: 1 Cell, 2 flows:
cells_E9 <- matrix(c(0.0),nrow=(length(adj_demand[,1])),ncol=1)
flow_E9 <- matrix(c(0.0),nrow=(length(adj_demand[,1])),ncol=2)
S_E9 <- matrix(c(0.0),nrow=(length(adj_demand[,1])),ncol=2)
R_E9 <- matrix(c(0.0),nrow=(length(adj_demand[,1])),ncol=2)

cells_E10 <- matrix(c(0.0),nrow=(length(adj_demand[,1])),ncol=1)
flow_E10 <- matrix(c(0.0),nrow=(length(adj_demand[,1])),ncol=2)
S_E10 <- matrix(c(0.0),nrow=(length(adj_demand[,1])),ncol=2)
R_E10 <- matrix(c(0.0),nrow=(length(adj_demand[,1])),ncol=2)

cells_S11 <- matrix(c(0.0),nrow=(length(adj_demand[,1])),ncol=1)
flow_S11 <- matrix(c(0.0),nrow=(length(adj_demand[,1])),ncol=2)
S_S11 <- matrix(c(0.0),nrow=(length(adj_demand[,1])),ncol=2)
R_S11 <- matrix(c(0.0),nrow=(length(adj_demand[,1])),ncol=2)

cells_E12 <- matrix(c(0.0),nrow=(length(adj_demand[,1])),ncol=1)
flow_E12 <- matrix(c(0.0),nrow=(length(adj_demand[,1])),ncol=2)
S_E12 <- matrix(c(0.0),nrow=(length(adj_demand[,1])),ncol=2)
R_E12 <- matrix(c(0.0),nrow=(length(adj_demand[,1])),ncol=2)

# 5) Merging/diverging priorities:
# Merging of AC and BC = ratio of lanes:
# AC = 2 lanes; BC = 1 lane:
mp_AC <- round(lanes_AC/(lanes_AC+lanes_BC),5)
mp_BC <- round(lanes_BC/(lanes_AC+lanes_BC),5)
# Merging of all on-ramps: E9 E10 and E12:
mp_E9 <- 0.2
mp_E10 <- 0.2
mp_E12 <- 0.2
# Off-ramp at S11:
# Variable capacity and variable split ratio:
sr_S11 <- matrix(c(0),nrow=(length(adj_demand[,1])),ncol=1)
Q_S11 <- matrix(c(0),nrow=(length(adj_demand[,1])),ncol=1)
#Fill in the split ratio's and capacities of the off-ramp S11
#iterate over all t
for(t in 1:length(adj_demand[,1])){
  # If-statements for later use to adapt values:
  abs_time <- round(t*delta_t,digits=2)
  if(abs_time > (3180) & abs_time <= (3900)){
    sr_S11[t] <- 0.25
    Q_S11[t] <- 1400*(delta_t/3600)
  } else if(abs_time > (4920) & abs_time <= (7800)){
    sr_S11[t] <- 0.25
    Q_S11[t] <- 1085*(delta_t/3600)
  } else if(abs_time > (9900) & abs_time <= (10500)){
    sr_S11[t] <- 0.25
    Q_S11[t] <- 900*(delta_t/3600)
  } else {
    sr_S11[t] <- 0.2
    Q_S11[t] <- 1400*(delta_t/3600)
  }
}

############################ START CTM ########################################
# 1) Initialize parameters for functions:
# a) Starting Cell bk:
n_bk <- 0
Q_bk <- 0
S_bk <- 0
# b) Arriving Cell ek: 
n_ek <- 0
Q_ek <- 0
slope_ek <- 0
N_ek <- 0
beta_ek <- 0
R_ek <- 0
# c) Diverging Cell ck:
n_ck
Q_ck <- 0
N_ck
beta_ck <- 0
R_ck <- 0
S_ck <- 0
slope_ck <- 0
# d) flows:
y_k <- 0
y_ck <- 0
# e) position of the i'th cell:
position_cell <- 0
# f) Priorities:
P_k <- 0
P_ck <- 0

# 2) Functions of flow:
# a) Flow in homogeneous sections
hom_flow <- function(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek){
  S_bk <- pmin(n_bk,Q_bk)
  R_ek <- pmin(Q_ek,slope_ek*(N_ek-n_ek))
  y_k <- pmin(S_bk,R_ek)
  return(c(S_bk,R_ek,y_k))
}

# b) Flow in merging sections
merge_flow <- function(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek,P_k,P_ck,n_ck,Q_ck){
  S_bk <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[1]
  R_ek <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[2]
  S_ck <- hom_flow(n_ck,Q_ck,Q_ek,slope_ek,N_ek,n_ek)[1]
  if (R_ek > (S_bk + S_ck)){
    y_k <- S_bk
    y_ck <- S_ck
  }  else if (R_ek <= (S_bk + S_ck)) {
    y_k <- median(c(S_bk,R_ek-S_ck,P_k*R_ek))
    y_ck <- median(c(S_ck,R_ek-S_bk,P_ck*R_ek))
  }
  return(c(S_bk,S_ck,R_ek,y_k,y_ck))
}

# c) Flow in diverging sections
diverge_flow <- function(n_bk,n_ek,n_ck,Q_bk,Q_ek,Q_ck,slope_ek,slope_ck,N_ek,N_ck,beta_ek,beta_ck){
  S_bk <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[1]
  R_ek <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[2]
  R_ck <- hom_flow(n_bk,Q_bk,Q_ck,slope_ck,N_ck,n_ck)[2]
  y_bk <- pmin(S_bk,R_ek/beta_ek,R_ck/beta_ck)
  y_k <- beta_ek*y_bk
  y_ck <- beta_ck*y_bk
  return(c(S_bk,R_ek,R_ck,y_bk,y_k,y_ck))
}

#50 cells in total minus this shift of #cells before C:
cell_shift <- length(cells_AC[1,])

# 2) CTM - model: For every t, go through every cell i:
for(t in 1:length(adj_demand[,1])){
	for(i in (length_section/(delta_x/1000)):0){
		position_cell <- round(i*(delta_x/1000),digits=1)
		
		if(position_cell == S5_0_D){
		  #Sink node at D: i=[50]
		  #Sending cell 50:
		  n_bk <- cells_CD[t,i-cell_shift]
		  Q_bk <- Q_CD
		  #Receiving cell Sink:
		  Q_ek <- Q_sink
		  slope_ek <- M
		  N_ek <- N_sink
		  n_ek <- n_sink
		  #Flow over link 51 between cell 50 and sink:
		  S_CD[t,i-cell_shift+1] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[1]
		  R_CD[t,i-cell_shift+1] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[2]
		  flow_CD[t,i-cell_shift+1] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[3]
		  #Update amount of vehicles in cell 50: outgoing flow
		  cells_CD[t,i-cell_shift] <- cells_CD[t,i-cell_shift] - flow_CD[t,i-cell_shift+1]
		  
		} else if((position_cell < S5_0_D) & (position_cell > S3_9_E12)){
		  #Every cell between the on-ramp at E12 and The last cell before D: i=[40-49]
		  #Sending cells 40 - 49:
		  n_bk <- cells_CD[t,i-cell_shift]
		  Q_bk <- Q_CD
		  #Receiving cells 41-50:
		  n_ek <- cells_CD[t,i-cell_shift+1]
		  Q_ek <- Q_CD
		  slope_ek <- slope_CD
		  N_ek <- N_CD
		  #Flow over link 41-50 between cells 40-50:
		  S_CD[t,i-cell_shift+1] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[1]
		  R_CD[t,i-cell_shift+1] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[2]
		  flow_CD[t,i-cell_shift+1] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[3]
		  #Update amount of vehicles in cells 40-49: outgoing flow
		  cells_CD[t,i-cell_shift] <- cells_CD[t,i-cell_shift] - flow_CD[t,i-cell_shift+1]
		  #Update amount of vehicles in cells 41-50: incoming flow
		  cells_CD[t,i-cell_shift+1] <- cells_CD[t,i-cell_shift+1] + flow_CD[t,i-cell_shift+1]
		  
		} else if(position_cell == S3_9_E12){
  	  #Source at on-ramp E12: i=[39]
		  #Sending sink at on-ramp:
		  n_bk <- n_source
		  Q_bk <- Q_source_E12[t]
		  #receiving cell E12
		  n_ek <- cells_E12[t]
		  Q_ek <- Q_onramp
		  slope_ek <- M
		  N_ek <- N_onramp
		  #Flow over link 1 from sink to E12:
		  S_E12[t,1] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[1]
		  R_E12[t,1] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[2]
		  flow_E12[t,1] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[3]
		  #Update amount of vehicles in E12: incoming flow
		  cells_E12[t] <- cells_E12[t] + flow_E12[t,1]
		  
		  #Merge freeway and E12 on-ramp: i=[39]
		  #Sending cell 39:
		  n_bk <- cells_CD[t,i-cell_shift]
		  Q_bk <- Q_CD
		  P_k <- (1-mp_E12)
		  #Sending cell E12 on-ramp:
		  n_ck <- cells_E12[t]
		  Q_ck <- Q_onramp
		  P_ck <- mp_E12
		  #Receiving cell 40:
		  n_ek <- cells_CD[t,i-cell_shift+1]
		  Q_ek <- Q_CD
		  N_ek <- N_CD
		  slope_ek <- slope_CD
		  #Flow over link 40 between cells 39 and 40:
		  S_CD[t,i-cell_shift+1] <- merge_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek,P_k,P_ck,n_ck,Q_ck)[1]
		  R_CD[t,i-cell_shift+1] <- P_k*merge_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek,P_k,P_ck,n_ck,Q_ck)[3]
		  flow_CD[t,i-cell_shift+1] <- merge_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek,P_k,P_ck,n_ck,Q_ck)[4]
		  #Flow over link 2 between E12 and cell 40:
		  S_E12[t,2] <- merge_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek,P_k,P_ck,n_ck,Q_ck)[2]
		  R_E12[t,2] <- P_ck*merge_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek,P_k,P_ck,n_ck,Q_ck)[3]
		  flow_E12[t,2] <- merge_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek,P_k,P_ck,n_ck,Q_ck)[5]
		  #Update amount of vehicles in cells 39 and E12: outgoing flow
		  cells_CD[t,i-cell_shift] <- cells_CD[t,i-cell_shift] - flow_CD[t,i-cell_shift+1]
		  cells_E12[t] <- cells_E12[t] - flow_E12[t,2]
		  #Update amount of vehicles in cells 40: incoming flow
		  cells_CD[t,i-cell_shift+1] <- cells_CD[t,i-cell_shift+1] + flow_CD[t,i-cell_shift+1] + flow_E12[t,2]
		  
		} else if(position_cell < S3_9_E12 & position_cell > S2_9_S11){
		  
		  #Every cell between the off-ramp at S11 and the on-ramp at E12
		  n_bk <- cells_CD[t,i-cell_shift]
		  Q_bk <- Q_CD
		  Q_ek <- Q_CD
		  slope_ek <- slope_CD
		  N_ek <- N_CD
		  n_ek <- cells_CD[t,i+1-cell_shift]
		  S_CD[t,i+1-cell_shift] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[1]
		  R_CD[t,i+1-cell_shift] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[2]
		  flow_CD[t,i+1-cell_shift] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[3]
		  cells_CD[t,i-cell_shift] <- cells_CD[t,i-cell_shift] - flow_CD[t,i+1-cell_shift]
		  cells_CD[t,i+1-cell_shift] <- cells_CD[t,i+1-cell_shift] + flow_CD[t,i+1-cell_shift]
		  
		} else if(position_cell == S2_9_S11){
		  
		  #Sink at off-ramp S11
		  n_bk <- cells_S11[t]
		  Q_bk <- Q_sink
		  Q_ek <- Q_sink
		  slope_ek <- M
		  N_ek <- N_sink
		  n_ek <- 0
		  S_S11[t,2] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[1]
		  R_S11[t,2] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[2]
		  flow_S11[t,2] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[3]
		  cells_S11[t] <- cells_S11[t] - flow_S11[t,2]
		  
		  #Diverge at S11 off-ramp and freeway
		  n_bk <- cells_CD[t,i-cell_shift]
		  Q_bk <- Q_CD
		 
		  n_ek <- cells_CD[t,i-cell_shift+1]
		  Q_ek <- Q_CD
		  slope_ek <- slope_CD
		  N_ek <- N_CD
		  beta_ek <- 1-sr_S11[t]
		  
		  n_ck <- cells_S11[t]
		  Q_ck <- Q_S11[t]
		  slope_ck <- M
		  N_ck <- M
		  beta_ck <- sr_S11[t]
		  
		  S_CD[t,i+1-cell_shift] <- beta_ek*diverge_flow(n_bk,n_ek,n_ck,Q_bk,Q_ek,Q_ck,slope_ek,slope_ck,N_ek,N_ck,beta_ek,beta_ck)[1]
		  R_CD[t,i+1-cell_shift] <- diverge_flow(n_bk,n_ek,n_ck,Q_bk,Q_ek,Q_ck,slope_ek,slope_ck,N_ek,N_ck,beta_ek,beta_ck)[2]
		  S_S11[t,1] <- beta_ck*diverge_flow(n_bk,n_ek,n_ck,Q_bk,Q_ek,Q_ck,slope_ek,slope_ck,N_ek,N_ck,beta_ek,beta_ck)[1]
		  R_S11[t,1] <- diverge_flow(n_bk,n_ek,n_ck,Q_bk,Q_ek,Q_ck,slope_ek,slope_ck,N_ek,N_ck,beta_ek,beta_ck)[3]
		  flow_CD[t,i+1-cell_shift] <- diverge_flow(n_bk,n_ek,n_ck,Q_bk,Q_ek,Q_ck,slope_ek,slope_ck,N_ek,N_ck,beta_ek,beta_ck)[5]
		  flow_S11[t,1] <- diverge_flow(n_bk,n_ek,n_ck,Q_bk,Q_ek,Q_ck,slope_ek,slope_ck,N_ek,N_ck,beta_ek,beta_ck)[6]
		  cells_CD[t,i-cell_shift] <- cells_CD[t,i-cell_shift] - diverge_flow(n_bk,n_ek,n_ck,Q_bk,Q_ek,Q_ck,slope_ek,slope_ck,N_ek,N_ck,beta_ek,beta_ck)[4]
		  cells_CD[t,i-cell_shift+1] <- cells_CD[t,i-cell_shift+1] + flow_CD[t,i+1-cell_shift]
		  cells_S11[t] <- cells_S11[t] + flow_S11[t,1]
		  
		} else if(position_cell < S2_9_S11 & position_cell > S1_1_E10){
		  
		  #Every cell between the on-ramp at E10 and The off-ramp at S11
		  n_bk <- cells_CD[t,i-cell_shift]
		  Q_bk <- Q_CD
		  Q_ek <- Q_CD
		  slope_ek <- slope_CD
		  N_ek <- N_CD
		  n_ek <- cells_CD[t,i+1-cell_shift]
		  S_CD[t,i+1-cell_shift] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[1]
		  R_CD[t,i+1-cell_shift] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[2]
		  flow_CD[t,i+1-cell_shift] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[3]
		  cells_CD[t,i-cell_shift] <- cells_CD[t,i-cell_shift] - flow_CD[t,i+1-cell_shift]
		  cells_CD[t,i+1-cell_shift] <- cells_CD[t,i+1-cell_shift] + flow_CD[t,i+1-cell_shift]
		  
		} else if(position_cell == S1_1_E10){
		  
		  #Source at on-ramp E10
		  n_bk <- n_source
		  Q_bk <- Q_source_E10[t]
		  Q_ek <- Q_source_E10[t]
		  slope_ek <- M
		  N_ek <- N_onramp
		  n_ek <- cells_E10[t]
		  S_E10[t,1] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[1]
		  R_E10[t,1] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[2]
		  flow_E10[t,1] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[3]
		  cells_E10[t] <- cells_E10[t] + flow_E10[t,1]
		  
		  #Merge freeway and E10 on-ramp
		  n_bk <- cells_CD[t,i-cell_shift]
		  Q_bk <- Q_CD
		  P_k <- 1-mp_E10
		  
		  n_ck <- cells_E10[t]
		  Q_ck <- Q_onramp
		  P_ck <- mp_E10
		  
		  n_ek <- cells_CD[t,i+1-cell_shift]
		  Q_ek <- Q_CD
		  N_ek <- N_CD
		  slope_ek <- slope_CD
		  
		  S_CD[t,i+1-cell_shift] <- merge_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek,P_k,P_ck,n_ck,Q_ck)[1]
		  S_E10[t,2] <- merge_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek,P_k,P_ck,n_ck,Q_ck)[2]
		  R_CD[t,i+1-cell_shift] <- merge_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek,P_k,P_ck,n_ck,Q_ck)[3]
		  R_E10[t,2] <- R_CD[t,i+1-cell_shift]
		  
		  flow_CD[t,i+1-cell_shift] <- merge_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek,P_k,P_ck,n_ck,Q_ck)[4]
		  flow_E10[t,2] <- merge_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek,P_k,P_ck,n_ck,Q_ck)[5]
		  
		  cells_CD[t,i-cell_shift] <- cells_CD[t,i-cell_shift] - flow_CD[t,i+1-cell_shift]
		  cells_E10[t] <- cells_E10[t] - flow_E10[t,2]
		  cells_CD[t,i+1-cell_shift] <- cells_CD[t,i+1-cell_shift] + flow_CD[t,i+1-cell_shift] + flow_E10[t,2]
		  
		} else if(position_cell < S1_1_E10 & position_cell > S0_6_E9){
		  
		  #Every cell between the on-ramp at E9 and The on-ramp at E10
		  n_bk <- cells_CD[t,i-cell_shift]
		  Q_bk <- Q_CD
		  Q_ek <- Q_CD
		  slope_ek <- slope_CD
		  N_ek <- N_CD
		  n_ek <- cells_CD[t,i+1-cell_shift]
		  S_CD[t,i+1-cell_shift] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[1]
		  R_CD[t,i+1-cell_shift] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[2]
		  flow_CD[t,i+1-cell_shift] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[3]
		  cells_CD[t,i-cell_shift] <- cells_CD[t,i-cell_shift] - flow_CD[t,i+1-cell_shift]
		  cells_CD[t,i+1-cell_shift] <- cells_CD[t,i+1-cell_shift] + flow_CD[t,i+1-cell_shift]
		  
		} else if(position_cell == S0_6_E9){
		  
		  #Source at on-ramp E9
		  n_bk <- n_source
		  Q_bk <- Q_source_E9[t]
		  Q_ek <- Q_source_E9[t]
		  slope_ek <- M
		  N_ek <- N_onramp
		  n_ek <- cells_E9[t]
		  S_E9[t,1] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[1]
		  R_E9[t,1] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[2]
		  flow_E9[t,1] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[3]
		  cells_E9[t] <- cells_E9[t] + flow_E9[t,1]
		  
		  #Merge freeway and E9 on-ramp
		  n_bk <- cells_CD[t,i-cell_shift]
		  Q_bk <- Q_CD
		  P_k <- 1-mp_E9
		  
		  n_ck <- cells_E9[t]
		  Q_ck <- Q_onramp
		  P_ck <- mp_E9
		  
		  n_ek <- cells_CD[t,i+1-cell_shift]
		  Q_ek <- Q_CD
		  N_ek <- N_CD
		  slope_ek <- slope_CD
		  
		  S_CD[t,i+1-cell_shift] <- merge_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek,P_k,P_ck,n_ck,Q_ck)[1]
		  S_E9[t,2] <- merge_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek,P_k,P_ck,n_ck,Q_ck)[2]
		  R_CD[t,i+1-cell_shift] <- merge_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek,P_k,P_ck,n_ck,Q_ck)[3]
		  R_E9[t,2] <- R_CD[t,i+1-cell_shift]
		  
		  flow_CD[t,i+1-cell_shift] <- merge_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek,P_k,P_ck,n_ck,Q_ck)[4]
		  flow_E9[t,2] <- merge_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek,P_k,P_ck,n_ck,Q_ck)[5]
		  
		  cells_CD[t,i-cell_shift] <- cells_CD[t,i-cell_shift] - flow_CD[t,i+1-cell_shift]
		  cells_E9[t] <- cells_E9[t] - flow_E9[t,2]
		  cells_CD[t,i+1-cell_shift] <- cells_CD[t,i+1-cell_shift] + flow_CD[t,i+1-cell_shift] + flow_E9[t,2]
		  
		} else if(position_cell < S0_6_E9 & position_cell > S0_2_C){
		  
		  #Every cell between the merge at C and The on-ramp at E9
		  n_bk <- cells_CD[t,i-cell_shift]
		  Q_bk <- Q_CD
		  Q_ek <- Q_CD
		  slope_ek <- slope_CD
		  N_ek <- N_CD
		  n_ek <- cells_CD[t,i+1-cell_shift]
		  S_CD[t,i+1-cell_shift] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[1]
		  R_CD[t,i+1-cell_shift] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[2]
		  flow_CD[t,i+1-cell_shift] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[3]
		  cells_CD[t,i-cell_shift] <- cells_CD[t,i-cell_shift] - flow_CD[t,i+1-cell_shift]
		  cells_CD[t,i+1-cell_shift] <- cells_CD[t,i+1-cell_shift] + flow_CD[t,i+1-cell_shift]
		  
		} else if(position_cell == S0_2_C){
		  
		  #Merge at C between A and B
		  n_bk <- cells_AC[t,i]
		  Q_bk <- Q_AC
		  P_k <- mp_AC
		  
		  n_ck <- cells_BC[t,i]
		  Q_ck <- Q_BC
		  P_ck <- mp_BC
		  
		  n_ek <- cells_CD[t,i-length(cells_AC[1,])+1]
		  Q_ek <- Q_CD
		  N_ek <- N_CD
		  slope_ek <- slope_CD
		  
		  S_AC[t,i+1] <- merge_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek,P_k,P_ck,n_ck,Q_ck)[1]
		  S_BC[t,i+1] <- merge_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek,P_k,P_ck,n_ck,Q_ck)[2]
		  S_CD[t,i-length(cells_AC[1,])+1] <- S_AC[t,i+1] + S_BC[t,i+1]
		  R_AC[t,i+1] <- merge_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek,P_k,P_ck,n_ck,Q_ck)[3]
		  R_BC[t,i+1] <- R_AC[t,i+1]
		  R_CD[t,i-length(cells_AC[1,])+1] <- R_AC[t,i+1]
		  flow_AC[t,i+1] <- merge_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek,P_k,P_ck,n_ck,Q_ck)[4]
		  flow_BC[t,i+1] <- merge_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek,P_k,P_ck,n_ck,Q_ck)[5]
		  flow_CD[t,i-length(cells_AC[1,])+1] <- flow_AC[t,i+1] + flow_BC[t,i+1]
		  cells_AC[t,i] <- cells_AC[t,i] - flow_AC[t,i+1]
		  cells_BC[t,i] <- cells_BC[t,i] - flow_BC[t,i+1]
		  cells_CD[t,i-length(cells_AC[1,])+1] <- cells_CD[t,i-length(cells_AC[1,])+1] + flow_CD[t,i-length(cells_AC[1,])+1]
		  
		} else if(position_cell < S0_2_C & position_cell > 0){
		  #Source node at A
		  n_bk <- cells_AC[t,i]
		  Q_bk <- Q_AC
		  Q_ek <- Q_AC
		  slope_ek <- slope_AC
		  N_ek <- N_AC
		  n_ek <- cells_AC[t,i+1]
		  S_AC[t,i+1] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[1]
		  R_AC[t,i+1] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[2]
		  flow_AC[t,i+1] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[3]
		  cells_AC[t,i] <- cells_AC[t,i] - flow_AC[t,i+1]	
		  cells_AC[t,i+1] <- cells_AC[t,i+1] + flow_AC[t,i+1]
		  
		  #Source node at B		
		  n_bk <- cells_BC[t,i]
		  Q_bk <- Q_BC
		  Q_ek <- Q_BC
		  slope_ek <- slope_BC
		  N_ek <- N_BC
		  n_ek <- cells_BC[t,i+1]
		  S_BC[t,i+1] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[1]
		  R_BC[t,i+1] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[2]
		  flow_BC[t,i+1] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[3]
		  cells_BC[t,i] <- cells_BC[t,i] - flow_BC[t,i+1]	
		  cells_BC[t,i+1] <- cells_BC[t,i+1] + flow_BC[t,i+1]
		  
		} else if(position_cell == 0){
		  #Source node at A
		  n_bk <- n_source
		  Q_bk <- Q_source_A[t]
		  Q_ek <- Q_AC
		  slope_ek <- slope_AC
		  N_ek <- N_AC
		  n_ek <- cells_AC[t,i+1]
		  S_AC[t,i+1] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[1]
		  R_AC[t,i+1] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[2]
		  flow_AC[t,i+1] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[3]
		  cells_AC[t,i+1] <- cells_AC[t,i+1] + flow_AC[t,i+1]		
		  
		  #Source node at B
		  n_bk <- n_source
		  Q_bk <- Q_source_B[t]
		  Q_ek <- Q_BC
		  slope_ek <- slope_BC
		  N_ek <- N_BC
		  n_ek <- cells_BC[t,i+1]
		  S_BC[t,i+1] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[1]
		  R_BC[t,i+1] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[2]
		  flow_BC[t,i+1] <- hom_flow(n_bk,Q_bk,Q_ek,slope_ek,N_ek,n_ek)[3]
		  cells_BC[t,i+1] <- cells_BC[t,i+1] + flow_BC[t,i+1]
		}
		next
	}
  if(t < length(adj_demand[,1])){
    #update all cells for next time step:
    cells_AC[t+1,] <- cells_AC[t,]
    cells_BC[t+1,] <- cells_BC[t,]
    cells_CD[t+1,] <- cells_CD[t,]
    cells_E9[t+1] <- cells_E9[t]
    cells_E10[t+1] <- cells_E10[t]
    cells_S11[t+1] <- cells_S11[t]
    cells_E12[t+1] <- cells_E12[t]
  }
  next
}

##### Make contourplots ##############
#A) Freeway section
# 1) Cars in cells in C to D
par(mar=c(5,6,5,2),mfrow=c(1,1))
x <- 1:nrow(cells_CD)
y <- 1:ncol(cells_CD)
filled.contour(x,y,cells_CD,
               color=topo.colors,
               main="Cars in cells from C to D plot",
               xlab="timestap [per 3.6 s]",
               ylab="Cell location",
               line=2.5,
               plot.axes = {axis(1,at=seq(0,nrow(cells_CD),100), labels=seq(0,nrow(cells_CD),100), pos=c(1,0))
                 axis(2, at=seq(0,ncol(cells_CD),2),labels=seq(0,ncol(cells_CD),2),pos=c(0,0))},
               xlim=c(0,nrow(cells_CD)),ylim=c(1,ncol(cells_CD)))
# 2) Cars in cells A to C
par(mar=c(5,6,5,2))
x <- 1:nrow(cells_AC)
y <- 1:ncol(cells_AC)
filled.contour(x,y,cells_AC,
               color=topo.colors,
               main="Cars in cells from A to C plot",
               xlab="timestap [per 3.6 s]",
               ylab="Cell location",
               line=2.5,
               plot.axes = {axis(1,at=seq(0,nrow(cells_AC),100), labels=seq(0,nrow(cells_AC),100), pos=c(1,0))
                 axis(2, at=seq(0,ncol(cells_AC),1),labels=seq(0,ncol(cells_AC),1),pos=c(0,0))},
               xlim=c(0,nrow(cells_AC)),ylim=c(1,ncol(cells_AC)))
#3) Together A to D:
par(mar=c(5,6,5,2))
x <- 1:nrow(cells_AC)
y <- 1:(ncol(cells_AC)+ncol(cells_CD))
filled.contour(x,y,cbind(cells_AC,cells_CD),
               color=topo.colors,
               main="Cars in cells from A to D plot",
               xlab="timestap [per 3.6 s]",
               ylab="Cell location",
               line=2.5,
               plot.axes = {axis(1,at=seq(0,nrow(cells_AC),100), labels=seq(0,nrow(cells_AC),100), pos=c(1,0))
                 axis(2, at=seq(0,ncol(cells_AC)+ncol(cells_CD),5),labels=seq(0,ncol(cells_AC)+ncol(cells_CD),5),pos=c(0,0))},
               xlim=c(0,nrow(cells_AC)),ylim=c(1,ncol(cells_AC)+ncol(cells_CD)))

# 4) Cars in cells B to C
par(mar=c(5,6,5,2))
x <- 1:nrow(cells_BC)
y <- 1:ncol(cells_BC)
filled.contour(x,y,cells_BC,
               color=topo.colors,
               main="Cars in cells from B to C plot",
               xlab="timestap [per 3.6 s]",
               ylab="Cell location",
               line=2.5,
               plot.axes = {axis(1,at=seq(0,nrow(cells_BC),100), labels=seq(0,nrow(cells_BC),100), pos=c(1,0))
                 axis(2, at=seq(0,ncol(cells_BC),1),labels=seq(0,ncol(cells_BC),1),pos=c(0,0))},
               xlim=c(0,nrow(cells_BC)),ylim=c(1,ncol(cells_BC)))
# 5) Together B to D:
par(mar=c(5,6,5,2))
x <- 1:nrow(cells_BC)
y <- 1:(ncol(cells_BC)+ncol(cells_CD))
filled.contour(x,y,cbind(cells_BC,cells_CD),
               color=topo.colors,
               main="Cars in cells from B to D plot",
               xlab="timestap [per 3.6 s]",
               ylab="Cell location",
               line=2.5,
               plot.axes = {axis(1,at=seq(0,nrow(cells_BC),100), labels=seq(0,nrow(cells_BC),100), pos=c(1,0))
                 axis(2, at=seq(0,ncol(cells_BC)+ncol(cells_CD),5),labels=seq(0,ncol(cells_BC)+ncol(cells_CD),5),pos=c(0,0))},
               xlim=c(0,nrow(cells_BC)),ylim=c(1,ncol(cells_BC)+ncol(cells_CD)))
#B) ON-ramps
# 1) E9:
par(mar=c(4,4, 3,4),mfrow=c(1,1))
x <- seq(1,nrow(cells_E9),1)
plot(x,y=cells_E9,
     type = 'h',
     col = 'grey',
     main="Cars in cell on-ramp E9 plot",
     xlab="timestep [per 3.6 s]",
     ylab="N [vehicles]",
     line=1.5,
     xlim=c(0,nrow(cells_E9)),
     ylim=c(0,pmax(max(cells_E9),max(flow_E9))+0.6),
     yaxt = 'n',
     xaxt = 'n',
     frame.plot = FALSE)
axis(1,at=seq(0,nrow(cells_E9),100), labels=seq(0,nrow(cells_E9),100),pos=c(0,0))
axis(2,at=seq(0,max(cells_E9)+1,1),labels=seq(0,max(cells_E9)+1,1),pos=c(0,0))
axis(4,at=seq(0,max(flow_E9)+1,1),labels=seq(0,max(flow_E9)+1,1),pos=c(nrow(cells_E9),0))
mtext("q [vehicles/timestep]", side=4, cex.lab=1,las=0, col="black",line=0.9)
lines(x,y=flow_E9[,1],col='blue',lty = 1,lwd = 1)
lines(x,y=flow_E9[,2],col='red', lty = 1, lwd = 1)
legend(x = nrow(cells_E9)/2,
       y = pmax(max(cells_E9),max(flow_E9)),
       col=c("grey","blue","red"),
       legend=c("Vehicles in on-ramp cell","Flow incoming vehicles","Flow outgoing vehicles"),
       lty = 1,
       xjust = 0.5,
       yjust = 0.2)

# 2) E10:
par(mar=c(4,4, 3,4),mfrow=c(1,1))
x <- seq(1,nrow(cells_E10),1)
plot(x,y=cells_E10,
     type = 'h',
     col = 'grey',
     main="Cars in cell on-ramp E10 plot",
     xlab="timestep [per 3.6 s]",
     ylab="N [vehicles]",
     line=1.5,
     xlim=c(0,nrow(cells_E10)),
     ylim=c(0,pmax(max(cells_E10),max(flow_E10))+0.25),
     yaxt = 'n',
     xaxt = 'n',
     frame.plot = FALSE)
axis(1,at=seq(0,nrow(cells_E10),100), labels=seq(0,nrow(cells_E10),100),pos=c(0,0))
axis(2,at=seq(0,max(cells_E10)+1,1),labels=seq(0,max(cells_E10)+1,1),pos=c(0,0))
axis(4,at=seq(0,max(flow_E10)+1,1),labels=seq(0,max(flow_E10)+1,1),pos=c(nrow(cells_E10),0))
mtext("q [vehicles/timestep]", side=4, cex.lab=1,las=0, col="black",line=0.9)
lines(x,y=flow_E10[,1],col='blue',lty = 1,lwd = 1)
lines(x,y=flow_E10[,2],col='red', lty = 2, lwd = 1)
legend(x = nrow(cells_E10)/2,
       y = pmax(max(cells_E10),max(flow_E10)),
       col=c("grey","blue","red"),
       legend=c("Vehicles in on-ramp cell","Flow incoming vehicles","Flow outgoing vehicles"),
       lty = 1,
       xjust = 0.5,
       yjust = 0.2)

# 3) E12:
par(mar=c(4,4, 3,4),mfrow=c(1,1))
x <- seq(1,nrow(cells_E12),1)
plot(x,y=cells_E12,
     type = 'h',
     col = 'grey',
     main="Cars in cell on-ramp E12 plot",
     xlab="timestep [per 3.6 s]",
     ylab="N [vehicles]",
     line=1.5,
     xlim=c(0,nrow(cells_E12)),
     ylim=c(0,pmax(max(cells_E12),max(flow_E12))+0.25),
     yaxt = 'n',
     xaxt = 'n',
     frame.plot = FALSE)
axis(1,at=seq(0,nrow(cells_E12),100), labels=seq(0,nrow(cells_E12),100),pos=c(0,0))
axis(2,at=seq(0,max(cells_E12)+1,1),labels=seq(0,max(cells_E12)+1,1),pos=c(0,0))
axis(4,at=seq(0,max(flow_E12)+1,1),labels=seq(0,max(flow_E12)+1,1),pos=c(nrow(cells_E12),0))
mtext("q [vehicles/timestep]", side=4, cex.lab=1,las=0, col="black",line=0.9)
lines(x,y=flow_E12[,1],col='blue',lty = 1,lwd = 1)
lines(x,y=flow_E12[,2],col='red', lty = 1, lwd = 1)
legend(x = nrow(cells_E12)/2,
       y = pmax(max(cells_E12),max(flow_E12)),
       col=c("grey","blue","red"),
       legend=c("Vehicles in on-ramp cell","Flow incoming vehicles","Flow outgoing vehicles"),
       lty = 1,
       xjust = 0.5,
       yjust = 1)

#C) Off-ramp
# 1) offramp at S11
par(mar=c(4,4, 3,4),mfrow=c(1,1))
x <- seq(1,nrow(cells_S11),1)
plot(x,y=cells_S11,
     type = 'h',
     col = 'grey',
     main="Cars in cell off-ramp S11 plot",
     xlab="timestep [per 3.6 s]",
     ylab="N [vehicles]",
     line=1.5,
     xlim=c(0,nrow(cells_S11)),
     ylim=c(0,pmax(max(cells_S11),max(flow_S11))+0.25),
     yaxt = 'n',
     xaxt = 'n',
     frame.plot = FALSE)
axis(1,at=seq(0,nrow(cells_S11),100), labels=seq(0,nrow(cells_S11),100),pos=c(0,0))
axis(2,at=seq(0,max(cells_S11)+1,1),labels=seq(0,max(cells_S11)+1,1),pos=c(0,0))
axis(4,at=seq(0,max(flow_S11)+1,1),labels=seq(0,max(flow_S11)+1,1),pos=c(nrow(cells_S11),0))
mtext("q [vehicles/timestep]", side=4, cex.lab=1,las=0, col="black",line=0.9)
lines(x,y=flow_S11[,1],col='blue',lty = 1,lwd = 1)
lines(x,y=flow_S11[,2],col='red', lty = 2, lwd = 1)
legend(x = nrow(cells_S11)/2*1.3,
       y = pmax(max(cells_S11),max(flow_S11)),
       col=c("grey","blue","red"),
       legend=c("Vehicles in off-ramp cell","Flow incoming vehicles","Flow outgoing vehicles"),
       lty = 1,
       xjust = 0.5,
       yjust = 0.35)

############# Flow-curves of traffic states ######################
#1) Section CD
par(mar=c(2, 2, 3,2),mfrow=c(2,2))
x <- seq(1,nrow(S_CD),1)
for(r in 4:1){
  plot(x,y=S_CD[,r],
     type = 'l',
     col = 'grey',
     main=paste("Sending vs. receiving flows (between cell",r+cell_shift-1,"and",r+cell_shift,")"),
     xlab="timestep [per 3.6 s]",
     ylab=paste("q [vehicles/timestep]"),
     line=1,
     xlim=c(0,nrow(S_CD)),
     ylim=c(0,pmax(max(S_CD[,r]),max(R_CD[,r]),max(flow_CD[,r]))+2),
     yaxt = 'n',
     xaxt = 'n',
     frame.plot = FALSE,
     lty = 1,
     lwd = 2)
  axis(1,at=seq(0,nrow(S_CD),100), labels=seq(0,nrow(S_CD),100),pos=c(0,0))
  axis(2,at=seq(0,pmax(max(S_CD[,r]),max(R_CD[,r]),max(flow_CD[,r]))+1,1),labels=seq(0,pmax(max(S_CD[,r]),max(R_CD[,r]),max(flow_CD[,r]))+1,1),pos=c(0,0))
  lines(x,y=R_CD[,r],col='blue',lty = 2,lwd = 2)
  lines(x,y=flow_CD[,r],col='red', lty = 3, lwd = 2)
  #legend(x = nrow(S_CD)/2,
   #    y = pmax(max(S_CD[,r]),max(R_CD[,r]),max(flow_CD[,r])),
    #   col=c("grey","blue","red"),
     #  legend=c("Sending function","Receiving function","Flow = min(S,R)"),
      # lty = c(1,2,3),
       #cex = 0.8,
       #xjust = 0.5,
       #yjust = -0.2)
  first_cong <- 0
  for(t in 2:nrow(flow_CD)){
    #In case of congestion:
    if(round(S_CD[t,r],2) > round(R_CD[t,r],2) & round(S_CD[t-1,r],2) <= round(R_CD[t-1,r],2)){
      first_cong  <- t
      lines(x=c(t,t),y=c(0,flow_CD[t,r]),col="darkgreen",lty=1,lwd=1)
      text(x=t-90,y=1,label=paste("timestep",first_cong),col="darkgreen",srt=90,cex=0.8)
    }
    #In case of clearance of queue:
    if(round(S_CD[t,r],2) <= round(R_CD[t,r],2) & round(S_CD[t-1,r],2) > round(R_CD[t-1,r],2)){
      lines(x=c(t,t),y=c(0,flow_CD[t,r]),col="darkgreen",lty=1,lwd=1)
      text(x=t+90,y=1,label=paste("timestep",t),col="darkgreen",srt=90,cex=0.8)
      if((t-first_cong)>100){
        arrows(x0=first_cong,y0=0.9,x1=t,y1=0.9,length = 0.1,angle=20,code=3,col="black",lty=1)
        text(x=0.5*first_cong+0.5*t,y=0.9,labels=paste(t - first_cong,"[s]"),col="black",pos=3)
      }
    }
  }
}

#2) Cells AC:
par(mar=c(2, 2, 3,2),mfrow=c(2,2))
x <- seq(1,nrow(S_AC),1)
for(r in 3:2){
  plot(x,y=S_AC[,r],
       type = 'l',
       col = 'grey',
       main=paste("A to C: Sending vs. receiving flows (between cell",r-1,"and",r,")"),
       xlab="timestep [per 3.6 s]",
       ylab=paste("q [vehicles/timestep]"),
       line=1,
       xlim=c(0,nrow(S_AC)),
       ylim=c(0,pmax(max(S_AC[,r]),max(R_AC[,r]),max(flow_AC[,r]))+2),
       yaxt = 'n',
       xaxt = 'n',
       frame.plot = FALSE,
       lty = 1,
       lwd = 2)
  axis(1,at=seq(0,nrow(S_AC),100), labels=seq(0,nrow(S_AC),100),pos=c(0,0))
  axis(2,at=seq(0,pmax(max(S_AC[,r]),max(R_AC[,r]),max(flow_AC[,r]))+1,1),labels=seq(0,pmax(max(S_AC[,r]),max(R_AC[,r]),max(flow_AC[,r]))+1,1),pos=c(0,0))
  lines(x,y=R_AC[,r],col='blue',lty = 2,lwd = 2)
  lines(x,y=flow_AC[,r],col='red', lty = 3, lwd = 2)
  first_cong <- 0
  for(t in 2:nrow(flow_AC)){
    #In case of congestion:
    if(round(S_AC[t,r],2) > round(R_AC[t,r],2) & round(S_AC[t-1,r],2) <= round(R_AC[t-1,r],2)){
      first_cong  <- t
      lines(x=c(t,t),y=c(0,flow_AC[t,r]),col="darkgreen",lty=1,lwd=1)
      text(x=t-90,y=1,label=paste("timestep",first_cong),col="darkgreen",srt=90,cex=0.8)
    }
    #In case of clearance of queue:
    if(round(S_AC[t,r],2) <= round(R_AC[t,r],2) & round(S_AC[t-1,r],2) > round(R_AC[t-1,r],2)){
      lines(x=c(t,t),y=c(0,flow_AC[t,r]),col="darkgreen",lty=1,lwd=1)
      text(x=t+90,y=1,label=paste("timestep",t),col="darkgreen",srt=90,cex=0.8)
      if((t-first_cong)>100){
        arrows(x0=first_cong,y0=0.9,x1=t,y1=0.9,length = 0.1,angle=20,code=3,col="black",lty=1)
        text(x=0.5*first_cong+0.5*t,y=0.9,labels=paste(t - first_cong,"[s]"),col="black",pos=3)
      }
    }
  }
}

#3) Cells BC:
x <- seq(1,nrow(S_BC),1)
for(r in 3:2){
  plot(x,y=S_BC[,r],
       type = 'l',
       col = 'grey',
       main=paste("B to C: Sending vs. receiving flows (between cell",r-1,"and",r,")"),
       xlab="timestep [per 3.6 s]",
       ylab=paste("q [vehicles/timestep]"),
       line=1,
       xlim=c(0,nrow(S_BC)),
       ylim=c(0,pmax(max(S_BC[,r]),max(R_BC[,r]),max(flow_BC[,r]))+2),
       yaxt = 'n',
       xaxt = 'n',
       frame.plot = FALSE,
       lty = 1,
       lwd = 2)
  axis(1,at=seq(0,nrow(S_BC),100), labels=seq(0,nrow(S_BC),100),pos=c(0,0))
  axis(2,at=seq(0,pmax(max(S_BC[,r]),max(R_BC[,r]),max(flow_BC[,r]))+1,1),labels=seq(0,pmax(max(S_BC[,r]),max(R_BC[,r]),max(flow_BC[,r]))+1,1),pos=c(0,0))
  lines(x,y=R_BC[,r],col='blue',lty = 2,lwd = 2)
  lines(x,y=flow_BC[,r],col='red', lty = 3, lwd = 2)
  first_cong <- 0
  for(t in 2:nrow(flow_BC)){
    #In case of congestion:
    if(round(S_BC[t,r],2) > round(R_BC[t,r],2) & round(S_BC[t-1,r],2) <= round(R_BC[t-1,r],2)){
      first_cong  <- t
      lines(x=c(t,t),y=c(0,flow_BC[t,r]),col="darkgreen",lty=1,lwd=1)
      text(x=t-90,y=1,label=paste("timestep",first_cong),col="darkgreen",srt=90,cex=0.8)
    }
    #In case of clearance of queue:
    if(round(S_BC[t,r],2) <= round(R_BC[t,r],2) & round(S_BC[t-1,r],2) > round(R_BC[t-1,r],2)){
      lines(x=c(t,t),y=c(0,flow_BC[t,r]),col="darkgreen",lty=1,lwd=1)
      text(x=t+90,y=1,label=paste("timestep",t),col="darkgreen",srt=90,cex=0.8)
      if((t-first_cong)>100){
        arrows(x0=first_cong,y0=0.9,x1=t,y1=0.9,length = 0.1,angle=20,code=3,col="black",lty=1)
        text(x=0.5*first_cong+0.5*t,y=0.9,labels=paste(t - first_cong,"[s]"),col="black",pos=3)
      }
    }
  }
}

#4) on-ramp E12:
par(mar=c(2, 2, 3,2),mfrow=c(2,2))
x <- seq(1,nrow(S_E12),1)
for(r in 2){
  if(r == 2){
    header <- (1000/delta_x)*S3_9_E12
  } else {
    header <- r-1
  }
  plot(x,y=S_E12[,r],
       type = 'l',
       col = 'grey',
       main=paste("Sending vs. receiving flows (on-ramp E12 to cell",header,")"),
       xlab="timestep [per 3.6 s]",
       ylab=paste("q [vehicles/timestep]"),
       line=1,
       xlim=c(0,nrow(S_E12)),
       ylim=c(0,pmax(max(S_E12[,r]),max(flow_E12[,r]))+0.5),
       yaxt = 'n',
       xaxt = 'n',
       frame.plot = FALSE,
       lty = 1,
       lwd = 2)
  axis(1,at=seq(0,nrow(S_E12),100), labels=seq(0,nrow(S_E12),100),pos=c(0,0))
  axis(2,at=seq(0,pmax(max(S_E12[,r]),max(flow_E12[,r]))+1,1),labels=seq(0,pmax(max(S_E12[,r]),max(flow_E12[,r]))+1,1),pos=c(0,0))
  lines(x,y=(mp_E12)*R_E12[,r],col='blue',lty = 2,lwd = 2)
  lines(x,y=flow_E12[,r],col='red', lty = 3, lwd = 2)
  first_cong <- 0
  for(t in 2:nrow(flow_E12)){
    #In case of congestion:
    if(round(S_E12[t,r],2) > round(R_E12[t,r],2) & round(S_E12[t-1,r],2) <= round(R_E12[t-1,r],2)){
      first_cong  <- t
      lines(x=c(t,t),y=c(0,flow_E12[t,r]),col="darkgreen",lty=1,lwd=1)
      text(x=t-90,y=1,label=paste("timestep",first_cong),col="darkgreen",srt=90,cex=0.8)
    }
    #In case of clearance of queue:
    if(round(S_E12[t,r],2) <= round(R_E12[t,r],2) & round(S_E12[t-1,r],2) > round(R_E12[t-1,r],2)){
      lines(x=c(t,t),y=c(0,flow_E12[t,r]),col="darkgreen",lty=1,lwd=1)
      text(x=t+90,y=1,label=paste("timestep",t),col="darkgreen",srt=90,cex=0.8)
      if((t-first_cong)>100){
        arrows(x0=first_cong,y0=0.9,x1=t,y1=0.9,length = 0.1,angle=20,code=3,col="black",lty=1)
        text(x=0.5*first_cong+0.5*t,y=0.9,labels=paste(t - first_cong,"[s]"),col="black",pos=3)
      }
    }
  }
}

#5) off-ramp S11:
x <- seq(1,nrow(S_S11),1)
for(r in 1:1){
  if(r == 1){
    header <- (1000/delta_x)*S2_9_S11
  } else {
    header <- r-1
  }
  plot(x,y=S_S11[,r],
       type = 'l',
       col = 'grey',
       main=paste("Sending vs. receiving flows (cell",header,"to off-ramp S11)"),
       xlab="timestep [per 3.6 s]",
       ylab=paste("q [vehicles/timestep]"),
       line=1,
       xlim=c(0,nrow(S_S11)),
       ylim=c(0,pmax(max(S_S11[,r]),max(flow_S11[,r]))+0.5),
       yaxt = 'n',
       xaxt = 'n',
       frame.plot = FALSE,
       lty = 1,
       lwd = 2)
  axis(1,at=seq(0,nrow(S_S11),100), labels=seq(0,nrow(S_S11),100),pos=c(0,0))
  axis(2,at=seq(0,pmax(max(S_S11[,r]),max(flow_S11[,r]))+1,1),labels=seq(0,pmax(max(S_S11[,r]),max(flow_S11[,r]))+1,1),pos=c(0,0))
  lines(x,y=R_S11[,r],col='blue',lty = 2,lwd = 2)
  lines(x,y=flow_S11[,r],col='red', lty = 3, lwd = 2)
  first_cong <- 0
  for(t in 2:nrow(flow_S11)){
    #In case of congestion:
    if(round(S_S11[t,r],2) > round(R_S11[t,r],2) & round(S_S11[t-1,r],2) <= round(R_S11[t-1,r],2)){
      first_cong  <- t
      lines(x=c(t,t),y=c(0,flow_S11[t,r]),col="darkgreen",lty=1,lwd=1)
      text(x=t-90,y=1,label=paste("timestep",first_cong),col="darkgreen",srt=90,cex=0.8)
    }
    #In case of clearance of queue:
    if(round(S_S11[t,r],2) <= round(R_S11[t,r],2) & round(S_S11[t-1,r],2) > round(R_S11[t-1,r],2)){
      lines(x=c(t,t),y=c(0,flow_S11[t,r]),col="darkgreen",lty=1,lwd=1)
      text(x=t+90,y=1,label=paste("timestep",t),col="darkgreen",srt=90,cex=0.8)
      if((t-first_cong)>100){
        arrows(x0=first_cong,y0=0.9,x1=t,y1=0.9,length = 0.1,angle=20,code=3,col="black",lty=1)
        text(x=0.5*first_cong+0.5*t,y=0.9,labels=paste(t - first_cong,"[s]"),col="black",pos=3)
      }
    }
  }
}

#6) on-ramp E10:
x <- seq(1,nrow(S_E10),1)
for(r in 2){
  if(r == 2){
    header <- (1000/delta_x)*S1_1_E10
  } else {
    header <- r-1
  }
  plot(x,y=S_E10[,r],
       type = 'l',
       col = 'grey',
       main=paste("Sending vs. receiving flows (on-ramp E10 to cell",header,")"),
       xlab="timestep [per 3.6 s]",
       ylab=paste("q [vehicles/timestep]"),
       line=1,
       xlim=c(0,nrow(S_E10)),
       ylim=c(0,pmax(max(S_E10[,r]),max(flow_E10[,r]))+0.5),
       yaxt = 'n',
       xaxt = 'n',
       frame.plot = FALSE,
       lty = 1,
       lwd = 2)
  axis(1,at=seq(0,nrow(S_E10),100), labels=seq(0,nrow(S_E10),100),pos=c(0,0))
  axis(2,at=seq(0,pmax(max(S_E10[,r]),max(flow_E10[,r]))+1,1),labels=seq(0,pmax(max(S_E10[,r]),max(flow_E10[,r]))+1,1),pos=c(0,0))
  lines(x,y=(mp_E10)*R_E10[,r],col='blue',lty = 2,lwd = 2)
  lines(x,y=flow_E10[,r],col='red', lty = 3, lwd = 2)
  first_cong <- 0
  for(t in 2:nrow(flow_E10)){
    #In case of congestion:
    if(round(S_E10[t,r],2) > round(R_E10[t,r],2) & round(S_E10[t-1,r],2) <= round(R_E10[t-1,r],2)){
      first_cong  <- t
      lines(x=c(t,t),y=c(0,flow_E10[t,r]),col="darkgreen",lty=1,lwd=1)
      text(x=t-90,y=1,label=paste("timestep",first_cong),col="darkgreen",srt=90,cex=0.8)
    }
    #In case of clearance of queue:
    if(round(S_E10[t,r],2) <= round(R_E10[t,r],2) & round(S_E10[t-1,r],2) > round(R_E10[t-1,r],2)){
      lines(x=c(t,t),y=c(0,flow_E10[t,r]),col="darkgreen",lty=1,lwd=1)
      text(x=t+90,y=1,label=paste("timestep",t),col="darkgreen",srt=90,cex=0.8)
      if((t-first_cong)>100){
        arrows(x0=first_cong,y0=0.9,x1=t,y1=0.9,length = 0.1,angle=20,code=3,col="black",lty=1)
        text(x=0.5*first_cong+0.5*t,y=0.9,labels=paste(t - first_cong,"[s]"),col="black",pos=3)
      }
    }
  }
}

#7) on-ramp E9:
x <- seq(1,nrow(S_E9),1)
for(r in 2){
  if(r == 2){
    header <- (1000/delta_x)*S0_6_E9
  } else {
    header <- r-1
  }
  plot(x,y=S_E9[,r],
       type = 'l',
       col = 'grey',
       main=paste("Sending vs. receiving flows (on-ramp E9 to cell",header,")"),
       xlab="timestep [per 3.6 s]",
       ylab=paste("q [vehicles/timestep]"),
       line=1,
       xlim=c(0,nrow(S_E9)),
       ylim=c(0,pmax(max(S_E9[,r]),max(flow_E9[,r]))+0.5),
       yaxt = 'n',
       xaxt = 'n',
       frame.plot = FALSE,
       lty = 1,
       lwd = 2)
  axis(1,at=seq(0,nrow(S_E9),100), labels=seq(0,nrow(S_E9),100),pos=c(0,0))
  axis(2,at=seq(0,pmax(max(S_E9[,r]),max(flow_E9[,r]))+1,1),labels=seq(0,pmax(max(S_E9[,r]),max(flow_E9[,r]))+1,1),pos=c(0,0))
  lines(x,y=mp_E9*R_E9[,r],col='blue',lty = 2,lwd = 2)
  lines(x,y=flow_E9[,r],col='red', lty = 3, lwd = 2)
  first_cong <- 0
  for(t in 2:nrow(flow_E9)){
    #In case of congestion:
    if(round(S_E9[t,r],2) > round(mp_E9*R_E9[t,r],2) & round(S_E9[t-1,r],2) <= round(mp_E9*R_E9[t-1,r],2)){
      first_cong  <- t
      lines(x=c(t,t),y=c(0,flow_E9[t,r]),col="darkgreen",lty=1,lwd=1)
      text(x=t-90,y=1,label=paste("timestep",first_cong),col="darkgreen",srt=90,cex=0.8)
    }
    #In case of clearance of queue:
    if(round(S_E9[t,r],2) <= round(mp_E9*R_E9[t,r],2) & round(S_E9[t-1,r],2) > round(mp_E9*R_E9[t-1,r],2)){
      lines(x=c(t,t),y=c(0,flow_E9[t,r]),col="darkgreen",lty=1,lwd=1)
      text(x=t+90,y=1,label=paste("timestep",t),col="darkgreen",srt=90,cex=0.8)
      if((t-first_cong)>100){
        arrows(x0=first_cong,y0=0.9,x1=t,y1=0.9,length = 0.1,angle=20,code=3,col="black",lty=1)
        text(x=0.5*first_cong+0.5*t,y=0.9,labels=paste(t - first_cong,"[s]"),col="black",pos=3)
      }
    }
  }
}

