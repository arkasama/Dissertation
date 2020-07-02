#This code has been developed by Arkasama Bandyopadhyay at the University of Texas at Austin in 2019
#This code represents a convex optimization model which minimizes costs (capital, O&M, and energy costs) 
#and discomfort for a residential customer with solar panels, lithium-ion batteries, ice CTESs, and smart 
#thermostats under five residential pricing structures: tiered rates, real-time prices, time-of-use rates, 
#critical peak pricing, and variable peak pricing
#This code accompanies the paper: "Joint Use of Distributed Energy Resources and Price-Based Demand Response 
#to Reduce Residential Peak Loads" by Arkasama Bandyopadhyay, Benjamin D. Leibowicz, and Michael E. Webber

#-----------------------------------------------
#Input parameters
#-----------------------------------------------

#-----Time interval of analysis----------

delta_t <- 1*60*60 #1 hour converted to seconds

#-----Distribution line capacity bound----------
P_g_max <- 20 #kW #capacity of the wires

#-----Residential Austin Energy tiered rate (replace with other pricing structures as needed)----------

#Energy charge - 5 tiers
c_t_1 <- 2.80 #cents/kWh
c_t_2 <- 5.83 #cents/kWh
c_t_3 <- 7.81 #cents/kWh
c_t_4 <- 9.31 #cents/kWh

#Power Supply Adjustment
psa<- 3.14 #cents/kWh

#Community Benefit Charges
cbc <- (0.154+0.124+0.335) #cents/kWh

#Regulatory Charges
reg <- 1.252 #cents/kWh

#Customer Charge
cust_charge <- 10 #$/month

#Overall Electricity Cost (except customer charge)
cost_1 <- c_t_1+psa+cbc+reg #cents/kWh - Tier 1
cost_2 <- c_t_2+psa+cbc+reg #cents/kWh - Tier 2
cost_3 <- c_t_3+psa+cbc+reg #cents/kWh - Tier 3
cost_4 <- c_t_4+psa+cbc+reg #cents/kWh - Tier 4


#Value of Solar
VOS <- 9.7 #cents/kWh
#----------------------------

#Input Parameters of Solar Panels
cap_solar <- 2350 # $/kW_dc - capital cost of solar panels
lifetime_solar <- 25 #years
i <- 7/100 #amortization rate/year
CRF_solar <- i*(1+i)^lifetime_solar/((1+i)^lifetime_solar-1) #capital recovery factor
cap_solar_year<- cap_solar*CRF_solar
om_solar <- 20 # $/kW_dc-year - operations and maintenance cost
size_solar <- 6 # kW_dc - size of solar panels
#----------------------------

#Input Parameters of lithium-ion batteries
cap_bat <- 933 #$/kWh - capital cost of lithium-ion battery
om_bat <- 10 # $/KWh-year - operations and maintenance cost
lifetime_bat <- 15 #years
i <- 7/100 #amortization rate/year
CRF_bat <- i*(1+i)^lifetime_bat/((1+i)^lifetime_bat-1) #capital recovery factor
cap_bat_year<- cap_bat*CRF_bat
size_bat <- 13.5 #kWh - size of lithium-ion battery
E_in <- 6.75 #kWh - initial energy capacity
E_min <- 1.35 #kWh - minimum energy capacity
E_max <- 13.5 #kWh - maximum energy capacity
eff_bat <- 90/100 #round-trip charging and discharging efficiency of the lithium-ion battery
loss_bat <- 0
R_charge <- 5 #kW -maxmium charging efficiency of the lithium-ion battery
R_discharge <- 5 #kW -maxmium discharging efficiency of the lithium-ionbattery
#------------------------

#Input Parameters of ice batteries or ice-CTES (cold thermal energy storage)
cap_ib <- 15000 #$ - capital cost 
om_ib <- 1.5*cap_ib/100 #$/year - operations and maintenance cost
lifetime_ib <- 20 #years
i <- 7/100 #amortization rate/year
CRF_ib <- i*(1+i)^lifetime_ib/((1+i)^lifetime_ib-1) #capital recovery factor
cap_ib_year<- cap_ib*CRF_ib
eff_CTES <- 0.95 #round-trip charging efficiency of the ice battery
R_charge_CTES <- 3*3.516 #kW_th #charging efficiency
R_discharge_CTES <- 3*3.516 #kW_th #discharging efficiency
P_max_HC <- 3*3.516 #kW_th #maximum power output of the heating and cooling engine
loss_CTES <- 0 #% per 1 hour interval
E_in_CTES <- 0*3.516 #kWh_th #initial energy capacity of the ice battery
E_min_CTES<- 0*3.516 #kWh_th #minimum energy capacity of the ice battery
E_max_CTES<- 10*3.516 #kWh_th #maximum energy capacity of the ice battery
COP_cool <- -0.0581*T_amb+6.67 #T_amb should be in Celsius #Coefficient of performance of the heating and cooling engine while cooling
COP_ice <- 0.80*COP_cool #Coefficient of performance of the heating and cooling engine while making ice
COP_heat <- 0.1181*T_amb+3.795 #T_amb should be in Celsius #Coefficient of performance of the heating and cooling engine while heating
#------------------------

#Rebates for solar 
itc_solar <- 26/100 #Investment Tax Credit for solar panels
rebate_solar <- 2500 #$ #Rebates for residential solar panels from Austin Energy

#Controllable HVAC/Smart Thermostat
cap_nest <- 250 #$ - capital cost of Nest thermostat
lifetime_nest <- 10 #years - lifetime of Nest thermostat
i <- 7/100 #amortization rate/year
CRF_nest <- i*(1+i)^lifetime_nest/((1+i)^lifetime_nest-1) #capital recovery factor
lifetime_nest <- 10 #years
cap_nest_year<- cap_nest*CRF_nest
thermostat_rebate<- 25 #$ - rebate offered by Austin Energy

#Discomfort Parameters of HVAC load
no_of_dec_var <-1*24*12 #1 day*24 hours *12 months

alpha_HVAC <- matrix((100*291*4*0.25 *(9/5)^2/10^6), nrow=no_of_dec_var, ncol=1) #$/degree C^2 or $/K^2

for (i in 1:12){ #from 9 am to 4 pm each day
  x=i-1 #from 9 am to 4 pm each day
  alpha_HVAC[(x*24+10):(x*24+16),1] <-100*155*4*0.25*(9/5)^2/10^6 #$/degree C^2 or $/K^2 
}

T_r_sp_t <- matrix((22.2+273), nrow=no_of_dec_var, ncol=1) # HVAC room set-point temperature 72 F
T_r_in <- (22.5+273) #K - initial room temperature
T_r_min <- (19.44+273) #K - minimum room temperature
T_r_max <- (27.78+273) #K - maximum room temperature


#------------------------------------------------
#Setting up the objective function
#------------------------------------------------
#install.packages('CVXR')
library(CVXR)
#no of decision variables 
no_of_dec_var <-(24*1*12) #no of hourly prices for each representative day of each month in 1 year
#Linear portion of the objective function
mu<- matrix(0, nrow=33*no_of_dec_var, ncol=1)
mu[,1] <- c(rep(0.000001, (4*no_of_dec_var)), rep((cost_1*delta_t/(100*3600)), no_of_dec_var), rep((cost_2*delta_t/(100*3600)), no_of_dec_var), rep((cost_3*delta_t/(100*3600)), no_of_dec_var),rep((cost_4*delta_t/(100*3600)), no_of_dec_var),rep((cost_1*delta_t/(100*3600)), no_of_dec_var), rep((cost_2*delta_t/(100*3600)), no_of_dec_var), rep((cost_3*delta_t/(100*3600)), no_of_dec_var),rep((cost_4*delta_t/(100*3600)), no_of_dec_var),rep((-cost_1*delta_t/(100*3600)), no_of_dec_var), rep((-cost_2*delta_t/(100*3600)), no_of_dec_var), rep((-cost_3*delta_t/(100*3600)), no_of_dec_var),rep((-cost_4*delta_t/(100*3600)), no_of_dec_var),rep((cost_1*delta_t/(100*3600)), no_of_dec_var)/COP_cool, rep((cost_2*delta_t/(100*3600)), no_of_dec_var)/COP_cool, rep((cost_3*delta_t/(100*3600)), no_of_dec_var)/COP_cool,rep((cost_4*delta_t/(100*3600)), no_of_dec_var)/COP_cool, rep((cost_1*delta_t/(100*3600)), no_of_dec_var)/COP_heat, rep((cost_2*delta_t/(100*3600)), no_of_dec_var)/COP_heat, rep((cost_3*delta_t/(100*3600)), no_of_dec_var)/COP_heat,rep((cost_4*delta_t/(100*3600)), no_of_dec_var)/COP_heat, rep(0, (4*no_of_dec_var)),rep((cost_1*delta_t/(100*3600)), no_of_dec_var)/COP_ice, rep((cost_2*delta_t/(100*3600)), no_of_dec_var)/COP_ice, rep((cost_3*delta_t/(100*3600)), no_of_dec_var)/COP_ice,rep((cost_4*delta_t/(100*3600)), no_of_dec_var)/COP_ice, rep(0, no_of_dec_var))
#Quadratic portion of the objective function
sigma_interm<- matrix(0, nrow=33*no_of_dec_var, ncol=1)
sigma_interm <- c(rep(0, (32*no_of_dec_var)), alpha_HVAC)
sigma <- diag(sigma_interm)
w=Variable(33*no_of_dec_var)
part_1 <- t(mu) %*% w
part_2 <- quad_form(w, sigma)
obj <- part_1 + part_2


#------------------------------------------------
#Constraints
#------------------------------------------------

#------------------------------------------------
#Uncontrollable power (Sum of uncontrollable power from each of the tiers at every time step must be equal to total uncontrollable electricity demand at those time steps)
#------------------------------------------------

mat_0 <- diag (0, nrow= no_of_dec_var, ncol = no_of_dec_var)
mat_1 <- diag (1,  nrow= no_of_dec_var, ncol = no_of_dec_var)
mat_constraint_uncontrol <- cbind( mat_0, mat_0, mat_0, mat_0, mat_1,mat_1, mat_1, mat_1, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0,mat_0,mat_0, mat_0, mat_0,mat_0,mat_0, mat_0, mat_0,mat_0,mat_0, mat_0, mat_0,mat_0,mat_0, mat_0, mat_0,mat_0, mat_0)
#P_uncontrol_use is empirical 1 hour interval uncontrollable power demand data (not provided as a part of this code)
mat_RHS_uncontrol <- P_uncontrol_use
#------------------------------------------------
#Limits on power usage (Power usage in the home must be less than or equal to sum of power bought from the grid and solar generation)
#Power usage is the sum of uncontrollable power, electricity consumed by the H&C engine (to heat/cool the home and/or make ice), 
#and the power lost due to effciency losses in the lithium-ion battery.
#------------------------------------------------
P_IB_home_cold_int <- diag((1/COP_cool), nrow=no_of_dec_var, ncol=no_of_dec_var)
P_IB_home_hot_int <- diag((1/COP_heat), nrow=no_of_dec_var, ncol=no_of_dec_var)
P_IB_home_ice_int <- diag((1/COP_ice), nrow=no_of_dec_var, ncol=no_of_dec_var)
mat_constraint_solar <- cbind(-mat_1,-mat_1, -mat_1, -mat_1, mat_1, mat_1, mat_1, mat_1, mat_1, mat_1, mat_1, mat_1, -mat_1, -mat_1, -mat_1, -mat_1, P_IB_home_cold_int, P_IB_home_cold_int,P_IB_home_cold_int,P_IB_home_cold_int,P_IB_home_hot_int, P_IB_home_hot_int, P_IB_home_hot_int, P_IB_home_hot_int, mat_0, mat_0, mat_0, mat_0,P_IB_home_ice_int, P_IB_home_ice_int,P_IB_home_ice_int,P_IB_home_ice_int, mat_0)
mat_RHS_solar <- matrix(0, nrow=no_of_dec_var, ncol=1)
Solar_gen[Solar_gen<0] <- 0
#Solar_gen is empirical 1 hour interval solar generation data (not provided as a part of this code)
mat_RHS_solar[,1] <- (Solar_gen) #kW

#------------------------------------------------
#Limits on power usage (Power usage in the home must be greater than or equal to zero. Power usage is the sum of uncontrollable power,
#electricity consumed by the H&C engine (to heat/cool the home and/or make ice), and the power lost due to effciency losses in the lithium-ion
#battery)
#------------------------------------------------
mat_0 <- diag (0, nrow= no_of_dec_var, ncol = no_of_dec_var)
mat_1 <- diag (1,  nrow= no_of_dec_var, ncol = no_of_dec_var)
P_IB_home_cold_int <- diag((1/COP_cool), nrow=no_of_dec_var, ncol=no_of_dec_var)
P_IB_home_hot_int <- diag((1/COP_heat), nrow=no_of_dec_var, ncol=no_of_dec_var)
P_IB_home_ice_int <- diag((1/COP_ice), nrow=no_of_dec_var, ncol=no_of_dec_var)

mat_constraint_usage <- cbind( mat_0, mat_0, mat_0, mat_0, mat_1, mat_1, mat_1, mat_1, mat_1, mat_1, mat_1, mat_1,-mat_1, -mat_1,-mat_1,-mat_1,P_IB_home_cold_int,P_IB_home_cold_int,P_IB_home_cold_int,P_IB_home_cold_int, P_IB_home_hot_int, P_IB_home_hot_int, P_IB_home_hot_int, P_IB_home_hot_int, mat_0, mat_0, mat_0, mat_0, P_IB_home_ice_int,P_IB_home_ice_int,P_IB_home_ice_int,P_IB_home_ice_int, mat_0)
mat_RHS_usage<- matrix(0, nrow=no_of_dec_var, ncol=1)

#------------------------------------------------
#Lithium-ion battery (Energy capacity of the lithium-ion battery at each time step depends on the power flowing in and out; also must lie between maximum and minimum energy capacity limits)
#------------------------------------------------
mat_charge <- diag (1*eff_bat,  nrow= no_of_dec_var, ncol = no_of_dec_var)
mat_discharge <- diag (-1/eff_bat,  nrow= no_of_dec_var, ncol = no_of_dec_var)
for (i in 2:no_of_dec_var){
  for (j in 1:(i-1)){
    mat_charge[i,j] <- (1-loss_bat)^(i-j)*eff_bat
  }
}
for (i in 2:no_of_dec_var){
  for (j in 1:(i-1)){
    mat_discharge[i,j] <- -(1-loss_bat)^(i-j)/eff_bat
  }
}

mat_constraint_bat<-cbind( mat_0, mat_0, mat_0, mat_0, mat_0,mat_0, mat_0, mat_0,  mat_charge,  mat_charge, mat_charge, mat_charge,mat_discharge, mat_discharge, mat_discharge, mat_discharge, mat_0,  mat_0, mat_0, mat_0, mat_0,mat_0, mat_0, mat_0,mat_0,  mat_0, mat_0, mat_0, mat_0,mat_0, mat_0, mat_0, mat_0)

mat_bat_LHS<- matrix(0, nrow=no_of_dec_var, ncol=1)
for (i in 1:no_of_dec_var){
  mat_bat_LHS<- (E_min-(1-loss_bat)^i*E_in)/(delta_t/3600)
}  
mat_bat_RHS<- matrix(0, nrow=no_of_dec_var, ncol=1)
for (i in 1:no_of_dec_var){
  mat_bat_RHS<- (E_max-(1-loss_bat)^i*E_in)/(delta_t/3600)
}


#------------------------------------------------
#Ice CTES or Ice Battery (Thermal energy capacity of the ice CTES at each time step depends on the thermal power flowing in and out; also must lie between maximum and minimum thermal energy capacity limits)
#------------------------------------------------
loss_CTES=0
mat_charge_CTES <- diag (1*eff_CTES,  nrow= no_of_dec_var, ncol = no_of_dec_var)
mat_discharge_CTES <- diag (-1/eff_CTES,  nrow= no_of_dec_var, ncol = no_of_dec_var)
for (i in 2:no_of_dec_var){
  for (j in 1:(i-1)){
    mat_charge_CTES[i,j] <- (1-loss_CTES)^(i-j)*eff_CTES
  }
}
for (i in 2:no_of_dec_var){
  for (j in 1:(i-1)){
    mat_discharge_CTES[i,j] <- -(1-loss_CTES)^(i-j)/eff_CTES
  }
}

mat_constraint_CTES<-cbind( mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0,mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0,mat_0, mat_0,mat_0, mat_0 , mat_discharge_CTES, mat_discharge_CTES, mat_discharge_CTES, mat_discharge_CTES, mat_charge_CTES,mat_charge_CTES,mat_charge_CTES,mat_charge_CTES, mat_0)

mat_CTES_LHS<- matrix(0, nrow=no_of_dec_var, ncol=1)
for (i in 1:no_of_dec_var){
  mat_CTES_LHS<- (E_min_CTES-(1-loss_CTES)^i*E_in_CTES)/(delta_t/3600)
}  
mat_CTES_RHS<- matrix(0, nrow=no_of_dec_var, ncol=1)
for (i in 1:no_of_dec_var){
  mat_CTES_RHS<- (E_max_CTES-(1-loss_CTES)^i*E_in_CTES)/(delta_t/3600)
}

#------------------------------------------------
#H&C Engine (Sum of the thermal power output of the heating and cooling engine while heating or cooling the home and/or making ice should be less than the maximum power output)
#------------------------------------------------
mat_constraint_HC_2 <- cbind( mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0,mat_0, mat_0, mat_0,mat_0, mat_0, mat_0,mat_1,mat_1,mat_1,mat_1,mat_1,mat_1,mat_1,mat_1,mat_0, mat_0,mat_0, mat_0,mat_1, mat_1, mat_1, mat_1, mat_0)
mat_RHS_HC_2 <- matrix(P_max_HC, nrow=no_of_dec_var, ncol=1)

#------------------------------------------------
#One-parameter thermal model for HVAC
#------------------------------------------------
MaCa <- 30*10^6 #J/K
R_wall <- 0.005 #K/W
first_term = (1-(delta_t/(MaCa*R_wall))) 
second_term = delta_t/(MaCa*R_wall) 
third_term <- 1000*delta_t/MaCa 

mat_HVAC <- diag ((1/third_term), nrow= no_of_dec_var, ncol = no_of_dec_var)
for (i in 2:no_of_dec_var){
  mat_HVAC[i, (i-1)] <- -first_term/third_term
}

#matrix for min and max
#Note: Obtain T_amb (ambient temperature) from empirical data (not provided as part of this code)
T_amb_K <- T_amb+273 #convert from celsius to K
mat_amb <- matrix(0, nrow <- no_of_dec_var, ncol <- 1)
mat_amb[1,1] <- (first_term*T_r_in/third_term) +second_term *T_amb_K[1]/third_term
for (i in 2:no_of_dec_var){
  mat_amb[i,1] <- second_term *T_amb_K[i]/third_term
}
mat_amb_2 <- mat_amb -mat_HVAC%*%T_r_sp_t
mat_0 =matrix(0, ncol=no_of_dec_var, nrow=no_of_dec_var)
mat_1 =diag(1, ncol=no_of_dec_var, nrow=no_of_dec_var)
mat_HVAC_2<- cbind( mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0,mat_0, mat_0, mat_0,mat_0, mat_0, mat_0,mat_1,mat_1,mat_1,mat_1,-mat_1,-mat_1,-mat_1,-mat_1,mat_1,mat_1,mat_1,mat_1,mat_0,mat_0, mat_0, mat_0, mat_HVAC)

#------------------------------------------------
#Tiered pricing - 1  (1st tier involves 0-500 kWh of energy bought from grid - divided by 30 to adjust from month-scale to day-scale)
#------------------------------------------------
mat_0 <- matrix(0, nrow=12, ncol=no_of_dec_var)
mat_1 <- matrix(0, nrow=12,ncol=no_of_dec_var)
mat_cool <- matrix(0, nrow=12,ncol=no_of_dec_var)
mat_heat <- matrix(0, nrow=12,ncol=no_of_dec_var)
mat_ice <- matrix(0, nrow=12,ncol=no_of_dec_var)
k=1
for (i in 1:12){
  mat_1[i,k:(24*i) ] <- 1
  k=k+24
}
k=1
for (i in 1:12){
  mat_cool[i,k:(24*i) ] <- 1/COP_cool[k:(24*i)]
  k=k+24
}
k=1
for (i in 1:12){
  mat_heat[i,k:(24*i) ] <- 1/COP_heat[k:(24*i)]
  k=k+24
}
k=1
for (i in 1:12){
  mat_ice[i,k:(24*i) ] <- 1/COP_ice[k:(24*i)]
  k=k+24
}
mat_tier_1 <- cbind( mat_0, mat_0, mat_0, mat_0, mat_1, mat_0, mat_0, mat_0, mat_1, mat_0, mat_0, mat_0,-mat_1, mat_0, mat_0, mat_0,mat_cool, mat_0, mat_0,mat_0, mat_heat, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_ice, mat_0, mat_0, mat_0, mat_0)
mat_tier_1_RHS <- matrix(500/30, nrow <- 12, ncol <-1)
#------------------------------------------------
#Tiered pricing - 2 (2nd tier involves 501-1000 kWh of energy bought from grid - divided by 30 to adjust from month-scale to day-scale)
#------------------------------------------------
mat_tier_2 <- cbind( mat_0, mat_0, mat_0, mat_0, mat_1, mat_1, mat_0,mat_0, mat_1, mat_1, mat_0,mat_0,-mat_1, -mat_1, mat_0,mat_0,mat_cool, mat_cool, mat_0,mat_0, mat_heat, mat_heat, mat_0, mat_0,mat_0, mat_0, mat_0, mat_0, mat_ice, mat_ice, mat_0, mat_0, mat_0)
mat_tier_2_RHS <- matrix(1000/30, nrow <- 12, ncol <-1)

#------------------------------------------------
#Tiered pricing - 3 (3rd tier involves 1001-1500 kWh of energy bought from grid - divided by 30 to adjust from month-scale to day-scale)
#------------------------------------------------
mat_tier_3 <- cbind( mat_0, mat_0, mat_0, mat_0, mat_1, mat_1, mat_1, mat_0,mat_1, mat_1, mat_1, mat_0,-mat_1, -mat_1,-mat_1, mat_0, mat_cool, mat_cool, mat_cool, mat_0, mat_heat, mat_heat, mat_heat, mat_0,mat_0, mat_0, mat_0, mat_0, mat_ice, mat_ice, mat_ice, mat_0, mat_0)
mat_tier_3_RHS <- matrix(1500/30, nrow <- 12, ncol <-1)

#------------------------------------------------
#Extra lithium-ion battery constraints
#------------------------------------------------
mat_0 <- diag (0, nrow= no_of_dec_var, ncol = no_of_dec_var)
mat_1 <- diag (1,  nrow= no_of_dec_var, ncol = no_of_dec_var)
mat_bat_1 <- cbind( mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_1, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0,mat_0, mat_0,mat_0, mat_0, mat_0, mat_0, mat_0, mat_0,mat_0, mat_0, mat_0)
mat_bat_2<- cbind( mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_1, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0,mat_0, mat_0,mat_0, mat_0, mat_0, mat_0, mat_0, mat_0,mat_0, mat_0, mat_0)
mat_bat_3<- cbind( mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0,  mat_1,mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0,mat_0, mat_0,mat_0, mat_0, mat_0, mat_0, mat_0, mat_0,mat_0, mat_0, mat_0)
mat_bat_4<- cbind( mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0, mat_0,  mat_0, mat_1,mat_0, mat_0, mat_0, mat_0, mat_0, mat_0,mat_0, mat_0,mat_0, mat_0, mat_0, mat_0, mat_0, mat_0,mat_0, mat_0, mat_0)

#------------------------------------------------
#Bounds for the decision variables
#------------------------------------------------
#Note ice battery is only allowed to operate from May to September
upper_bound <- matrix(0, nrow=33*no_of_dec_var, ncol=1)
upper_bound[,1] <- c(rep(P_g_max, (8*no_of_dec_var)), rep(R_charge, (4*no_of_dec_var)),rep(R_discharge, (4*no_of_dec_var)),rep(P_max_HC, 8*no_of_dec_var), rep(0, 96), rep(R_discharge_CTES, 120),rep(0, 72),rep(0, 96), rep(R_discharge_CTES, 120),rep(0, 72),rep(0, 96), rep(R_discharge_CTES, 120),rep(0, 72),rep(0, 96), rep(R_discharge_CTES, 120),rep(0, 72),rep(0, 96), rep(R_charge_CTES, 120), rep(0,72), rep(0, 96), rep(R_charge_CTES, 120), rep(0,72), rep(0, 96), rep(R_charge_CTES, 120), rep(0,72), rep(0, 96), rep(R_charge_CTES, 120), rep(0,72), (rep(T_r_max, (no_of_dec_var))-T_r_sp_t))
lower_bound <- matrix(0, nrow=33*no_of_dec_var, ncol=1)
lower_bound[,1] <- c(rep(0, 32*no_of_dec_var), (rep(T_r_min, (no_of_dec_var))-T_r_sp_t))

#------------------------------------------------
#Solve the problem
#------------------------------------------------
constr <- list(w >= lower_bound, w<=upper_bound, mat_constraint_solar%*%w<=mat_RHS_solar,mat_constraint_CTES%*%w>=mat_CTES_LHS, mat_constraint_CTES%*%w<=mat_CTES_RHS, (mat_bat_1-mat_bat_2)%*%w>=0, (mat_bat_2-mat_bat_3)%*%w>=0,(mat_bat_3-mat_bat_4)%*%w>=0,mat_constraint_uncontrol%*%w==mat_RHS_uncontrol,mat_constraint_usage%*%w>=mat_RHS_usage,mat_constraint_bat%*%w>=mat_bat_LHS, mat_constraint_bat%*%w<=mat_bat_RHS,  mat_constraint_HC_2%*%w<=mat_RHS_HC_2, mat_HVAC_2%*%w==mat_amb_2,mat_tier_1%*%w<=mat_tier_1_RHS, mat_tier_2%*%w<=mat_tier_2_RHS, mat_tier_3%*%w<=mat_tier_3_RHS)
prob <- Problem(Minimize(obj), constr)
result <- solve(prob)
result$status

#------------------------------------------------
#Processing the outputs
#------------------------------------------------
solution=result$getValue(w)
#--------------------------------------------------
#Decision variables
P_grid_home_T1<- solution[1:288] #Power flowing from the grid to home in Tier 1
P_grid_home_T2<- solution[289:576]#Power flowing from the grid to home in Tier 2
P_grid_home_T3<- solution[577:864]#Power flowing from the grid to home in Tier 3
P_grid_home_T4<- solution[865:1152]#Power flowing from the grid to home in Tier 4
P_uncontrol_T1<- solution[1153:1440]#Uncontrollable power in Tier 1
#--------------------------------------------------
P_uncontrol_T2 <- solution[1441:1728]#Uncontrollable power in Tier 2
P_uncontrol_T3 <- solution[1729:2016]#Uncontrollable power in Tier 3
P_uncontrol_T4 <- solution[2017:2304]#Uncontrollable power in Tier 4
P_charge_bat_T1 <- solution[2305:2592]#Power used to charge the lithium-ion battery in Tier 1
P_charge_bat_T2<- solution[2593:2880]#Power used to charge the lithium-ion battery in Tier 2
#--------------------------------------------------
P_charge_bat_T3 <- solution[2881:3168]#Power used to charge the lithium-ion battery in Tier 3
P_charge_bat_T4 <- solution[3169:3456]#Power used to charge the lithium-ion battery in Tier 4
P_discharge_bat_T1<- solution[3457:3744]#Power discharged by the lithium-ion battery in Tier 1
P_discharge_bat_T2 <- solution[3745:4032]#Power discharged by the lithium-ion battery in Tier 2
P_discharge_bat_T3 <- solution[4033:4320]#Power discharged by the lithium-ion battery in Tier 3
#--------------------------------------------------
P_discharge_bat_T4 <- solution[4321:4608] #Power discharged by the lithium-ion battery in Tier 4
P_IB_home_cold_T1<- solution[4609:4896]  # Thermal power flowing from the heating and cooling engine to the home to meet cooling demand in Tier 1
P_IB_home_cold_T2 <- solution[4897:5184]#Thermal power flowing from the heating and cooling engine to the home to meet cooling demand in Tier 2
P_IB_home_cold_T3 <- solution[5185:5472] #Thermal power flowing from the heating and cooling engine to the home to meet cooling demand in Tier 3
P_IB_home_cold_T4<- solution[5473:5760] #Thermal power flowing from the heating and cooling engine to the home to meet cooling demand in Tier 4
P_IB_home_hot_T1 <- solution[5761:6048]# Thermal power flowing from the heating and cooling engine to the home to meet heating demand in Tier 1
P_IB_home_hot_T2<- solution[6049:6336]# Thermal power flowing from the heating and cooling engine to the home to meet heating demand in Tier 2
P_IB_home_hot_T3  <- solution[6337:6624]# Thermal power flowing from the heating and cooling engine to the home to meet heating demand in Tier 3
P_IB_home_hot_T4  <- solution[6625:6912]# Thermal power flowing from the heating and cooling engine to the home to meet heating demand in Tier 4
P_CTES_home_T1 <- solution[6913:7200]#Thermal power flowing from the ice CTES to the home in Tier 1
P_CTES_home_T2 <-solution[7201:7488]#Thermal power flowing from the ice CTES to the home in Tier 2
P_CTES_home_T3 <-solution[7489:7776]#Thermal power flowing from the ice CTES to the home in Tier 3
P_CTES_home_T4 <-solution[7777:8064]#Thermal power flowing from the ice CTES to the home in Tier 4
P_IB_CTES_T1  <-solution[8065:8352]#Thermal power flowing from the heating and cooling engine to the ice CTES in Tier 1
P_IB_CTES_T2<-solution[8353:8640]#Thermal power flowing from the heating and cooling engine to the ice CTES in Tier 2
P_IB_CTES_T3 <-solution[8641:8928]#Thermal power flowing from the heating and cooling engine to the ice CTES in Tier 3
P_IB_CTES_T4 <-solution[8929:9216]#Thermal power flowing from the heating and cooling engine to the ice CTES in Tier 4
T_r<-solution[9217:9504]+T_r_sp_t#Room temperature




#Total power flowing from the grid to the home
P_grid_home <- P_grid_home_T1+P_grid_home_T2+P_grid_home_T3+P_grid_home_T4
#Total thermal power flowing from the H&C engine to cool the home 
P_IB_home_cold <- P_IB_home_cold_T1+P_IB_home_cold_T2+P_IB_home_cold_T3+P_IB_home_cold_T4
#Total thermal power flowing from the H&C engine to heat the home 
P_IB_home_hot <- P_IB_home_hot_T1+P_IB_home_hot_T2+P_IB_home_hot_T3+P_IB_home_hot_T4
#Total thermal power flowing from the H&C engine to make ice
P_IB_CTES<- P_IB_CTES_T1+P_IB_CTES_T2+P_IB_CTES_T3+P_IB_CTES_T4
#Total power used to charge the lithium-ion battery
P_charge_bat <- P_charge_bat_T1+P_charge_bat_T2+P_charge_bat_T3+P_charge_bat_T4
#Total power discharged by the lithium-ion battery
P_discharge_bat <- P_discharge_bat_T1+P_discharge_bat_T2+P_discharge_bat_T3+P_discharge_bat_T4


#Total power bought from the grid
P_grid <- (P_grid_home_T1+P_grid_home_T2+P_grid_home_T3+P_grid_home_T4)
#Total power bought from the grid throughout the year
P_grid_year <- sum(P_grid)*30 #kWh

#Total power consumption in the home
P_usage <- (P_uncontrol_use+P_charge_bat-P_discharge_bat+P_IB_home_cold/COP_cool+P_IB_home_hot/COP_heat+P_IB_CTES/COP_ice)
#Total power consumption in the home over the year
P_usage_year <- sum(P_usage)*30

#Capital Cost
cap <- cap_bat_year*size_bat+cap_ib_year+(cap_solar_year)*size_solar- itc_solar*cap_solar_year*size_solar-rebate_solar*CRF_solar+cap_nest_year
#Operations and Maintenance Cost
om <- om_solar*size_solar+om_bat*size_bat+om_ib
#Energy/Electricity Cost 
energy_cost <- result$value*30+ cust_charge*12+(-VOS/100)*sum(Solar_gen)*30

