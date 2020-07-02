#This code has been developed by Arkasama Bandyopadhyay at the University of Texas at Austin
#This code represents a generalized tool to forecast the change in 4CP (4-coincident peak) loads and payments (TCOS obligations) 
#in response to varying amounts of solar and storage capacity for utilities within ERCOT
#This code supplements the paper "How Solar and Storage Can Reduce Coincident Peak Loads and Payments: A Case Study in 
#Austin, TX" by Arkasama Bandyopadhyay, Joshua D. Rhodes, Julia P. Conger, and Michael E. Webber

#Place this file in the same folder as all other input files needed
#Set the working directory
dir=getwd()
setwd(dir)


# Reading the raw ERCOT historical load files for years 2011-2017 (files not provided as a part of this code)
ERCOT_2011_raw <- read.csv('2011_ERCOT_Hourly_Load_Data.csv', header = TRUE)
ERCOT_2012_raw <- read.csv('2012_ERCOT_Hourly_Load_Data.csv', header = TRUE)
ERCOT_2013_raw <- read.csv('2013_ERCOT_Hourly_Load_Data.csv', header = TRUE)
ERCOT_2014_raw <- read.csv('2014_ERCOT_Hourly_Load_Data.csv', header = TRUE)
ERCOT_2015_raw <- read.csv('2015_ERCOT_Hourly_Load_Data.csv', header = TRUE)
ERCOT_2016_raw <- read.csv('2016_ERCOT_Hourly_Load_Data.csv', header = TRUE)
ERCOT_2017_raw <- read.csv('2017_ERCOT_Hourly_Load_Data.csv', header = TRUE)

# Getting the load column values from the ERCOT files
ERCOT_2011_hourly <- ERCOT_2011_raw[, 10]
ERCOT_2012_hourly <- ERCOT_2012_raw[, 10]
ERCOT_2013_hourly <- ERCOT_2013_raw[, 10]
ERCOT_2014_hourly <- ERCOT_2014_raw[, 10]
ERCOT_2015_hourly <- ERCOT_2015_raw[, 10]
ERCOT_2016_hourly <- ERCOT_2016_raw[, 10]
ERCOT_2017_hourly <- ERCOT_2017_raw[, 10]

#Getting the month values
month_2011 <- ERCOT_2011_raw[, 11]
month_2012 <- ERCOT_2012_raw[, 11]
month_2013 <- ERCOT_2013_raw[, 11]
month_2014 <- ERCOT_2014_raw[, 11]
month_2015 <- ERCOT_2015_raw[, 11]
month_2016 <- ERCOT_2016_raw[, 11]
month_2017 <- ERCOT_2017_raw[, 11] 

# --------------------------------------------------------------------------
# Processing 2011 data
# -------------------------------------------------------------------------
# Separating the different months
ERCOT_2011_june_hourly <- ERCOT_2011_hourly[which(month_2011==6)]
ERCOT_2011_july_hourly <- ERCOT_2011_hourly[which(month_2011==7)]
ERCOT_2011_aug_hourly <- ERCOT_2011_hourly[which(month_2011==8)]
ERCOT_2011_sept_hourly <- ERCOT_2011_hourly[which(month_2011==9)]
ERCOT_2011_oct_hourly <- ERCOT_2011_hourly[which(month_2011==10)]

# Creating new values for june, july, august and september by adding the first load value from the next month - this is done to get the last value for interpolation
ERCOT_2011_june_hourly <- c(ERCOT_2011_june_hourly, ERCOT_2011_july_hourly[1] )
ERCOT_2011_july_hourly <- c(ERCOT_2011_july_hourly, ERCOT_2011_aug_hourly[1] )
ERCOT_2011_aug_hourly <- c(ERCOT_2011_aug_hourly, ERCOT_2011_sept_hourly[1] )
ERCOT_2011_sept_hourly <- c(ERCOT_2011_sept_hourly, ERCOT_2011_oct_hourly[1])

#Interpolating hourly values to get 15 minute values

#June
x <- c(1: length(ERCOT_2011_june_hourly))
xout <- seq(from = 1, to = length(ERCOT_2011_june_hourly), by = 0.25)
interp_2011_june_15 <- approx(x, ERCOT_2011_june_hourly, xout, method = "linear")
ERCOT_2011_june_15 <-as.data.frame(interp_2011_june_15[2])
ERCOT_2011_june_15 <-ERCOT_2011_june_15[-nrow(ERCOT_2011_june_15),]

#July
x <- c(1: length(ERCOT_2011_july_hourly))
xout <- seq(from = 1, to = length(ERCOT_2011_july_hourly), by = 0.25)
interp_2011_july_15 <- approx(x, ERCOT_2011_july_hourly, xout, method = "linear")
ERCOT_2011_july_15 <-as.data.frame(interp_2011_july_15[2])
ERCOT_2011_july_15 <-ERCOT_2011_july_15[-nrow(ERCOT_2011_july_15),]

#August
x <- c(1: length(ERCOT_2011_aug_hourly))
xout <- seq(from = 1, to = length(ERCOT_2011_aug_hourly), by = 0.25)
interp_2011_aug_15 <- approx(x, ERCOT_2011_aug_hourly, xout, method = "linear")
ERCOT_2011_aug_15 <-as.data.frame(interp_2011_aug_15[2])
ERCOT_2011_aug_15 <-ERCOT_2011_aug_15[-nrow(ERCOT_2011_aug_15),]

#September
x <- c(1: length(ERCOT_2011_sept_hourly))
xout <- seq(from = 1, to = length(ERCOT_2011_sept_hourly), by = 0.25)
interp_2011_sept_15 <- approx(x, ERCOT_2011_sept_hourly, xout, method = "linear")
ERCOT_2011_sept_15 <-as.data.frame(interp_2011_sept_15[2])
ERCOT_2011_sept_15 <-ERCOT_2011_sept_15[-nrow(ERCOT_2011_sept_15),]

# --------------------------------------------------------------------------
# Processing 2012 data
# -------------------------------------------------------------------------
# Separating the different months
ERCOT_2012_june_hourly <- ERCOT_2012_hourly[which(month_2012==6)]
ERCOT_2012_july_hourly <- ERCOT_2012_hourly[which(month_2012==7)]
ERCOT_2012_aug_hourly <- ERCOT_2012_hourly[which(month_2012==8)]
ERCOT_2012_sept_hourly <- ERCOT_2012_hourly[which(month_2012==9)]
ERCOT_2012_oct_hourly <- ERCOT_2012_hourly[which(month_2012==10)]

# Creating new values for june, july, august and september by adding the first load value from the next month
ERCOT_2012_june_hourly <- c(ERCOT_2012_june_hourly, ERCOT_2012_july_hourly[1] )
ERCOT_2012_july_hourly <- c(ERCOT_2012_july_hourly, ERCOT_2012_aug_hourly[1] )
ERCOT_2012_aug_hourly <- c(ERCOT_2012_aug_hourly, ERCOT_2012_sept_hourly[1] )
ERCOT_2012_sept_hourly <- c(ERCOT_2012_sept_hourly, ERCOT_2012_oct_hourly[1] )

#Interpolating hourly values to get 15 minute values

#June
x <- c(1: length(ERCOT_2012_june_hourly))
xout <- seq(from = 1, to = length(ERCOT_2012_june_hourly), by = 0.25)
interp_2012_june_15 <- approx(x, ERCOT_2012_june_hourly, xout, method = "linear")
ERCOT_2012_june_15 <-as.data.frame(interp_2012_june_15[2])
ERCOT_2012_june_15 <-ERCOT_2012_june_15[-nrow(ERCOT_2012_june_15),]

#July
x <- c(1: length(ERCOT_2012_july_hourly))
xout <- seq(from = 1, to = length(ERCOT_2012_july_hourly), by = 0.25)
interp_2012_july_15 <- approx(x, ERCOT_2012_july_hourly, xout, method = "linear")
ERCOT_2012_july_15 <-as.data.frame(interp_2012_july_15[2])
ERCOT_2012_july_15 <-ERCOT_2012_july_15[-nrow(ERCOT_2012_july_15),]

#August
x <- c(1: length(ERCOT_2012_aug_hourly))
xout <- seq(from = 1, to = length(ERCOT_2012_aug_hourly), by = 0.25)
interp_2012_aug_15 <- approx(x, ERCOT_2012_aug_hourly, xout, method = "linear")
ERCOT_2012_aug_15 <-as.data.frame(interp_2012_aug_15[2])
ERCOT_2012_aug_15 <-ERCOT_2012_aug_15[-nrow(ERCOT_2012_aug_15),]

#September
x <- c(1: length(ERCOT_2012_sept_hourly))
xout <- seq(from = 1, to = length(ERCOT_2012_sept_hourly), by = 0.25)
interp_2012_sept_15 <- approx(x, ERCOT_2012_sept_hourly, xout, method = "linear")
ERCOT_2012_sept_15 <-as.data.frame(interp_2012_sept_15[2])
ERCOT_2012_sept_15 <-ERCOT_2012_sept_15[-nrow(ERCOT_2012_sept_15),]

# --------------------------------------------------------------------------
# Processing 2013 data
# -------------------------------------------------------------------------
# Separating the different months
ERCOT_2013_june_hourly <- ERCOT_2013_hourly[which(month_2013==6)]
ERCOT_2013_july_hourly <- ERCOT_2013_hourly[which(month_2013==7)]
ERCOT_2013_aug_hourly <- ERCOT_2013_hourly[which(month_2013==8)]
ERCOT_2013_sept_hourly <- ERCOT_2013_hourly[which(month_2013==9)]
ERCOT_2013_oct_hourly <- ERCOT_2013_hourly[which(month_2013==10)]

# Creating new values for june, july, august and september by adding the first load value from the next month for interpolation
ERCOT_2013_june_hourly <- c(ERCOT_2013_june_hourly, ERCOT_2013_july_hourly[1] )
ERCOT_2013_july_hourly <- c(ERCOT_2013_july_hourly, ERCOT_2013_aug_hourly[1] )
ERCOT_2013_aug_hourly <- c(ERCOT_2013_aug_hourly, ERCOT_2013_sept_hourly[1] )
ERCOT_2013_sept_hourly <- c(ERCOT_2013_sept_hourly, ERCOT_2013_oct_hourly[1] )

#Interpolating hourly values to get 15 minute values

#June
x <- c(1: length(ERCOT_2013_june_hourly))
xout <- seq(from = 1, to = length(ERCOT_2013_june_hourly), by = 0.25)
interp_2013_june_15 <- approx(x, ERCOT_2013_june_hourly, xout, method = "linear")
ERCOT_2013_june_15 <-as.data.frame(interp_2013_june_15[2])
ERCOT_2013_june_15 <-ERCOT_2013_june_15[-nrow(ERCOT_2013_june_15),]

#July
x <- c(1: length(ERCOT_2013_july_hourly))
xout <- seq(from = 1, to = length(ERCOT_2013_july_hourly), by = 0.25)
interp_2013_july_15 <- approx(x, ERCOT_2013_july_hourly, xout, method = "linear")
ERCOT_2013_july_15 <-as.data.frame(interp_2013_july_15[2])
ERCOT_2013_july_15 <-ERCOT_2013_july_15[-nrow(ERCOT_2013_july_15),]

#August
x <- c(1: length(ERCOT_2013_aug_hourly))
xout <- seq(from = 1, to = length(ERCOT_2013_aug_hourly), by = 0.25)
interp_2013_aug_15 <- approx(x, ERCOT_2013_aug_hourly, xout, method = "linear")
ERCOT_2013_aug_15 <-as.data.frame(interp_2013_aug_15[2])
ERCOT_2013_aug_15 <-ERCOT_2013_aug_15[-nrow(ERCOT_2013_aug_15),]

#September
x <- c(1: length(ERCOT_2013_sept_hourly))
xout <- seq(from = 1, to = length(ERCOT_2013_sept_hourly), by = 0.25)
interp_2013_sept_15 <- approx(x, ERCOT_2013_sept_hourly, xout, method = "linear")
ERCOT_2013_sept_15 <-as.data.frame(interp_2013_sept_15[2])
ERCOT_2013_sept_15 <-ERCOT_2013_sept_15[-nrow(ERCOT_2013_sept_15),]

# --------------------------------------------------------------------------
# Processing 2014 data
# -------------------------------------------------------------------------
# Separating the different months
ERCOT_2014_june_hourly <- ERCOT_2014_hourly[which(month_2014==6)]
ERCOT_2014_july_hourly <- ERCOT_2014_hourly[which(month_2014==7)]
ERCOT_2014_aug_hourly <- ERCOT_2014_hourly[which(month_2014==8)]
ERCOT_2014_sept_hourly <- ERCOT_2014_hourly[which(month_2014==9)]
ERCOT_2014_oct_hourly <- ERCOT_2014_hourly[which(month_2014==10)]

# Creating new values for june, july, august and september by adding the first load value from the next month
ERCOT_2014_june_hourly <- c(ERCOT_2014_june_hourly, ERCOT_2014_july_hourly[1] )
ERCOT_2014_july_hourly <- c(ERCOT_2014_july_hourly, ERCOT_2014_aug_hourly[1] )
ERCOT_2014_aug_hourly <- c(ERCOT_2014_aug_hourly, ERCOT_2014_sept_hourly[1] )
ERCOT_2014_sept_hourly <- c(ERCOT_2014_sept_hourly, ERCOT_2014_oct_hourly[1] )

#Interpolating hourly values to get 15 minute values

#June
x <- c(1: length(ERCOT_2014_june_hourly))
xout <- seq(from = 1, to = length(ERCOT_2014_june_hourly), by = 0.25)
interp_2014_june_15 <- approx(x, ERCOT_2014_june_hourly, xout, method = "linear")
ERCOT_2014_june_15 <-as.data.frame(interp_2014_june_15[2])
ERCOT_2014_june_15 <-ERCOT_2014_june_15[-nrow(ERCOT_2014_june_15),]

#July
x <- c(1: length(ERCOT_2014_july_hourly))
xout <- seq(from = 1, to = length(ERCOT_2014_july_hourly), by = 0.25)
interp_2014_july_15 <- approx(x, ERCOT_2014_july_hourly, xout, method = "linear")
ERCOT_2014_july_15 <-as.data.frame(interp_2014_july_15[2])
ERCOT_2014_july_15 <-ERCOT_2014_july_15[-nrow(ERCOT_2014_july_15),]

#August
x <- c(1: length(ERCOT_2014_aug_hourly))
xout <- seq(from = 1, to = length(ERCOT_2014_aug_hourly), by = 0.25)
interp_2014_aug_15 <- approx(x, ERCOT_2014_aug_hourly, xout, method = "linear")
ERCOT_2014_aug_15 <-as.data.frame(interp_2014_aug_15[2])
ERCOT_2014_aug_15 <-ERCOT_2014_aug_15[-nrow(ERCOT_2014_aug_15),]

#September
x <- c(1: length(ERCOT_2014_sept_hourly))
xout <- seq(from = 1, to = length(ERCOT_2014_sept_hourly), by = 0.25)
interp_2014_sept_15 <- approx(x, ERCOT_2014_sept_hourly, xout, method = "linear")
ERCOT_2014_sept_15 <-as.data.frame(interp_2014_sept_15[2])
ERCOT_2014_sept_15 <-ERCOT_2014_sept_15[-nrow(ERCOT_2014_sept_15),]

# --------------------------------------------------------------------------
# Processing 2015 data
# -------------------------------------------------------------------------
# Separating the different months
ERCOT_2015_june_hourly <- ERCOT_2015_hourly[which(month_2015==6)]
ERCOT_2015_july_hourly <- ERCOT_2015_hourly[which(month_2015==7)]
ERCOT_2015_aug_hourly <- ERCOT_2015_hourly[which(month_2015==8)]
ERCOT_2015_sept_hourly <- ERCOT_2015_hourly[which(month_2015==9)]
ERCOT_2015_oct_hourly <- ERCOT_2015_hourly[which(month_2015==10)]

# Creating new values for june, july, august and september by adding the first load value from the next month
ERCOT_2015_june_hourly <- c(ERCOT_2015_june_hourly, ERCOT_2015_july_hourly[1] )
ERCOT_2015_july_hourly <- c(ERCOT_2015_july_hourly, ERCOT_2015_aug_hourly[1] )
ERCOT_2015_aug_hourly <- c(ERCOT_2015_aug_hourly, ERCOT_2015_sept_hourly[1] )
ERCOT_2015_sept_hourly <- c(ERCOT_2015_sept_hourly, ERCOT_2015_oct_hourly[1] )

#Interpolating hourly values to get 15 minute values

#June
x <- c(1: length(ERCOT_2015_june_hourly))
xout <- seq(from = 1, to = length(ERCOT_2015_june_hourly), by = 0.25)
interp_2015_june_15 <- approx(x, ERCOT_2015_june_hourly, xout, method = "linear")
ERCOT_2015_june_15 <-as.data.frame(interp_2015_june_15[2])
ERCOT_2015_june_15 <-ERCOT_2015_june_15[-nrow(ERCOT_2015_june_15),]

#July
x <- c(1: length(ERCOT_2015_july_hourly))
xout <- seq(from = 1, to = length(ERCOT_2015_july_hourly), by = 0.25)
interp_2015_july_15 <- approx(x, ERCOT_2015_july_hourly, xout, method = "linear")
ERCOT_2015_july_15 <-as.data.frame(interp_2015_july_15[2])
ERCOT_2015_july_15 <-ERCOT_2015_july_15[-nrow(ERCOT_2015_july_15),]

#August
x <- c(1: length(ERCOT_2015_aug_hourly))
xout <- seq(from = 1, to = length(ERCOT_2015_aug_hourly), by = 0.25)
interp_2015_aug_15 <- approx(x, ERCOT_2015_aug_hourly, xout, method = "linear")
ERCOT_2015_aug_15 <-as.data.frame(interp_2015_aug_15[2])
ERCOT_2015_aug_15 <-ERCOT_2015_aug_15[-nrow(ERCOT_2015_aug_15),]

#September
x <- c(1: length(ERCOT_2015_sept_hourly))
xout <- seq(from = 1, to = length(ERCOT_2015_sept_hourly), by = 0.25)
interp_2015_sept_15 <- approx(x, ERCOT_2015_sept_hourly, xout, method = "linear")
ERCOT_2015_sept_15 <-as.data.frame(interp_2015_sept_15[2])
ERCOT_2015_sept_15 <-ERCOT_2015_sept_15[-nrow(ERCOT_2015_sept_15),]

# --------------------------------------------------------------------------
# Processing 2016 data
# -------------------------------------------------------------------------
# Separating the different months
ERCOT_2016_june_hourly <- ERCOT_2016_hourly[which(month_2016==6)]
ERCOT_2016_july_hourly <- ERCOT_2016_hourly[which(month_2016==7)]
ERCOT_2016_aug_hourly <- ERCOT_2016_hourly[which(month_2016==8)]
ERCOT_2016_sept_hourly <- ERCOT_2016_hourly[which(month_2016==9)]
ERCOT_2016_oct_hourly <- ERCOT_2016_hourly[which(month_2016==10)]

# Creating new values for june, july, august and september by adding the first load value from the next month
ERCOT_2016_june_hourly <- c(ERCOT_2016_june_hourly, ERCOT_2016_july_hourly[1] )
ERCOT_2016_july_hourly <- c(ERCOT_2016_july_hourly, ERCOT_2016_aug_hourly[1] )
ERCOT_2016_aug_hourly <- c(ERCOT_2016_aug_hourly, ERCOT_2016_sept_hourly[1] )
ERCOT_2016_sept_hourly <- c(ERCOT_2016_sept_hourly, ERCOT_2016_oct_hourly[1] )

#Interpolating hourly values to get 15 minute values

#June
x <- c(1: length(ERCOT_2016_june_hourly))
xout <- seq(from = 1, to = length(ERCOT_2016_june_hourly), by = 0.25)
interp_2016_june_15 <- approx(x, ERCOT_2016_june_hourly, xout, method = "linear")
ERCOT_2016_june_15 <-as.data.frame(interp_2016_june_15[2])
ERCOT_2016_june_15 <-ERCOT_2016_june_15[-nrow(ERCOT_2016_june_15),]

#July
x <- c(1: length(ERCOT_2016_july_hourly))
xout <- seq(from = 1, to = length(ERCOT_2016_july_hourly), by = 0.25)
interp_2016_july_15 <- approx(x, ERCOT_2016_july_hourly, xout, method = "linear")
ERCOT_2016_july_15 <-as.data.frame(interp_2016_july_15[2])
ERCOT_2016_july_15 <-ERCOT_2016_july_15[-nrow(ERCOT_2016_july_15),]

#August
x <- c(1: length(ERCOT_2016_aug_hourly))
xout <- seq(from = 1, to = length(ERCOT_2016_aug_hourly), by = 0.25)
interp_2016_aug_15 <- approx(x, ERCOT_2016_aug_hourly, xout, method = "linear")
ERCOT_2016_aug_15 <-as.data.frame(interp_2016_aug_15[2])
ERCOT_2016_aug_15 <-ERCOT_2016_aug_15[-nrow(ERCOT_2016_aug_15),]

#September
x <- c(1: length(ERCOT_2016_sept_hourly))
xout <- seq(from = 1, to = length(ERCOT_2016_sept_hourly), by = 0.25)
interp_2016_sept_15 <- approx(x, ERCOT_2016_sept_hourly, xout, method = "linear")
ERCOT_2016_sept_15 <-as.data.frame(interp_2016_sept_15[2])
ERCOT_2016_sept_15 <-ERCOT_2016_sept_15[-nrow(ERCOT_2016_sept_15),]

# --------------------------------------------------------------------------
# Processing 2017 data
# -------------------------------------------------------------------------
# Separating the different months
ERCOT_2017_june_hourly <- ERCOT_2017_hourly[which(month_2017==6)]
ERCOT_2017_july_hourly <- ERCOT_2017_hourly[which(month_2017==7)]
ERCOT_2017_aug_hourly <- ERCOT_2017_hourly[which(month_2017==8)]
ERCOT_2017_sept_hourly <- ERCOT_2017_hourly[which(month_2017==9)]
ERCOT_2017_oct_hourly <- ERCOT_2017_hourly[which(month_2017==10)]

# Creating new values for june, july, august and september by adding the first load value from the next month
ERCOT_2017_june_hourly <- c(ERCOT_2017_june_hourly, ERCOT_2017_july_hourly[1] )
ERCOT_2017_july_hourly <- c(ERCOT_2017_july_hourly, ERCOT_2017_aug_hourly[1] )
ERCOT_2017_aug_hourly <- c(ERCOT_2017_aug_hourly, ERCOT_2017_sept_hourly[1] )
ERCOT_2017_sept_hourly <- c(ERCOT_2017_sept_hourly, ERCOT_2017_oct_hourly[1] )

#Interpolating hourly values to get 15 minute values

#June
x <- c(1: length(ERCOT_2017_june_hourly))
xout <- seq(from = 1, to = length(ERCOT_2017_june_hourly), by = 0.25)
interp_2017_june_15 <- approx(x, ERCOT_2017_june_hourly, xout, method = "linear")
ERCOT_2017_june_15 <-as.data.frame(interp_2017_june_15[2])
ERCOT_2017_june_15 <-ERCOT_2017_june_15[-nrow(ERCOT_2017_june_15),]

#July
x <- c(1: length(ERCOT_2017_july_hourly))
xout <- seq(from = 1, to = length(ERCOT_2017_july_hourly), by = 0.25)
interp_2017_july_15 <- approx(x, ERCOT_2017_july_hourly, xout, method = "linear")
ERCOT_2017_july_15 <-as.data.frame(interp_2017_july_15[2])
ERCOT_2017_july_15 <-ERCOT_2017_july_15[-nrow(ERCOT_2017_july_15),]

#August
x <- c(1: length(ERCOT_2017_aug_hourly))
xout <- seq(from = 1, to = length(ERCOT_2017_aug_hourly), by = 0.25)
interp_2017_aug_15 <- approx(x, ERCOT_2017_aug_hourly, xout, method = "linear")
ERCOT_2017_aug_15 <-as.data.frame(interp_2017_aug_15[2])
ERCOT_2017_aug_15 <-ERCOT_2017_aug_15[-nrow(ERCOT_2017_aug_15),]

#September
x <- c(1: length(ERCOT_2017_sept_hourly))
xout <- seq(from = 1, to = length(ERCOT_2017_sept_hourly), by = 0.25)
interp_2017_sept_15 <- approx(x, ERCOT_2017_sept_hourly, xout, method = "linear")
ERCOT_2017_sept_15 <-as.data.frame(interp_2017_sept_15[2])
ERCOT_2017_sept_15 <-ERCOT_2017_sept_15[-nrow(ERCOT_2017_sept_15),]

#--------------------------------------------------------------------------------

#Averaging over all 7 years

ERCOT_june_15<-(ERCOT_2011_june_15+ERCOT_2012_june_15+ERCOT_2013_june_15+ERCOT_2014_june_15+ERCOT_2015_june_15+ERCOT_2016_june_15+ERCOT_2017_june_15)/7
ERCOT_july_15 <-(ERCOT_2011_july_15+ERCOT_2012_july_15+ERCOT_2013_july_15+ERCOT_2014_july_15+ERCOT_2015_july_15+ERCOT_2016_july_15+ERCOT_2017_july_15)/7
ERCOT_aug_15<-(ERCOT_2011_aug_15+ERCOT_2012_aug_15+ERCOT_2013_aug_15+ERCOT_2014_aug_15+ERCOT_2015_aug_15+ERCOT_2016_aug_15+ERCOT_2017_aug_15)/7
ERCOT_sept_15<-(ERCOT_2011_sept_15+ERCOT_2012_sept_15+ERCOT_2013_sept_15+ERCOT_2014_sept_15+ERCOT_2015_sept_15+ERCOT_2016_sept_15+ERCOT_2017_sept_15)/7

ERCOT = rbind(c(ERCOT_june_15, ERCOT_july_15, ERCOT_aug_15, ERCOT_sept_15))

#----------------------------------------------------------------------------------
#Scaling ERCOT load with annual energy forecasts from ERCOT
ratio_ERCOT = c(1.10, 1.13, 1.18, 1.21, 1.25, 1.28, 1.32, 1.34, 1.37, 1.40)
ERCOT_forecast <- matrix(0, nrow=11712, ncol=10)
  for (j in 1:10){
    ERCOT_forecast[,j] <- ERCOT*ratio_ERCOT[j]
}

#----------------------------------------------------------------------------------
#Dividing forecasted ERCOT loads into months

ERCOT_forecast_june <-ERCOT_forecast[1:2880, 1:10]
ERCOT_forecast_july <-ERCOT_forecast[2881:5856, 1:10]
ERCOT_forecast_aug <-ERCOT_forecast[5857:8832, 1:10]
ERCOT_forecast_sept <-ERCOT_forecast[8833:11712, 1:10]

#----------------------------------------------------------------------------------
#Finding the maximum  and position of the maximum from each month i.e. find the maximum ERCOT load and timing of the maximum ERCOT load for each of the summer months from June to September
I_0_june <- apply(ERCOT_forecast_june, 2, which.max)
M_0_june <- apply(ERCOT_forecast_june, 2, max)
I_0_july <- apply(ERCOT_forecast_july, 2, which.max)
M_0_july <- apply(ERCOT_forecast_july, 2, max)
I_0_aug <- apply(ERCOT_forecast_aug, 2, which.max)
M_0_aug <- apply(ERCOT_forecast_aug, 2, max)
I_0_sept <- apply(ERCOT_forecast_sept, 2, which.max)
M_0_sept <- apply(ERCOT_forecast_sept, 2, max)

#----------------------------------------------------------------------------------
#Austin Energy Load
#----------------------------------------------------------------------------------
#reading in the raw file with 15-minute interval historical demand data (input file not provided as a part of this code)
AE_raw <- read.csv('AE_Load.csv')
#getting the load, month and year values
AE_load<- AE_raw[, 7]
month_AE <- AE_raw[, 10]
year_AE <- AE_raw[, 9]

#getting the load values for June, July, August and September
#2011
AE_2011_june <- AE_load[which(year_AE==2011 & month_AE==6 )]
AE_2011_july <- AE_load[which(year_AE==2011 & month_AE==7 )]
AE_2011_aug <- AE_load[which(year_AE==2011 & month_AE==8 )]
AE_2011_sept <- AE_load[which(year_AE==2011 & month_AE==9 )]

#2012
AE_2012_june <- AE_load[which(year_AE==2012 & month_AE==6 )]
AE_2012_july <- AE_load[which(year_AE==2012 & month_AE==7 )]
AE_2012_aug <- AE_load[which(year_AE==2012 & month_AE==8 )]
AE_2012_sept <- AE_load[which(year_AE==2012 & month_AE==9 )]

#2013
AE_2013_june <- AE_load[which(year_AE==2013 & month_AE==6 )]
AE_2013_july <- AE_load[which(year_AE==2013 & month_AE==7 )]
AE_2013_aug <- AE_load[which(year_AE==2013 & month_AE==8 )]
AE_2013_sept <- AE_load[which(year_AE==2013 & month_AE==9 )]

#2014
AE_2014_june <- AE_load[which(year_AE==2014 & month_AE==6 )]
AE_2014_july <- AE_load[which(year_AE==2014 & month_AE==7 )]
AE_2014_aug <- AE_load[which(year_AE==2014 & month_AE==8 )]
AE_2014_sept <- AE_load[which(year_AE==2014 & month_AE==9 )]

#2015
AE_2015_june <- AE_load[which(year_AE==2015 & month_AE==6 )]
AE_2015_july <- AE_load[which(year_AE==2015 & month_AE==7 )]
AE_2015_aug <- AE_load[which(year_AE==2015 & month_AE==8 )]
AE_2015_sept <- AE_load[which(year_AE==2015 & month_AE==9 )]

#2016
AE_2016_june <- AE_load[which(year_AE==2016 & month_AE==6 )]
AE_2016_july <- AE_load[which(year_AE==2016 & month_AE==7 )]
AE_2016_aug <- AE_load[which(year_AE==2016 & month_AE==8 )]
AE_2016_sept <- AE_load[which(year_AE==2016 & month_AE==9 )]

#2017
AE_2017_june <- AE_load[which(year_AE==2017 & month_AE==6 )]
AE_2017_july <- AE_load[which(year_AE==2017 & month_AE==7 )]
AE_2017_aug <- AE_load[which(year_AE==2017 & month_AE==8 )]
AE_2017_sept <- AE_load[which(year_AE==2017 & month_AE==9 )]

#----------------------------------------------------------------------------------
#Averaging the values
AE_june= (AE_2011_june + AE_2012_june + AE_2013_june + AE_2014_june + AE_2015_june + AE_2016_june + AE_2017_june)/7
AE_july= (AE_2011_july + AE_2012_july + AE_2013_july + AE_2014_july + AE_2015_july + AE_2016_july + AE_2017_july)/7
AE_aug= (AE_2011_aug + AE_2012_aug + AE_2013_aug + AE_2014_aug + AE_2015_aug + AE_2016_aug + AE_2017_aug)/7
AE_sept= (AE_2011_sept + AE_2012_sept + AE_2013_sept + AE_2014_sept + AE_2015_sept + AE_2016_sept + AE_2017_sept)/7

AE = rbind(c(AE_june, AE_july, AE_aug, AE_sept))

#----------------------------------------------------------------------------------
#Scaling AE load with projected annual energy (from AE - available publicly)
ratio_AE <- c(1.05, 1.05, 1.05, 1.04, 1.05, 1.05, 1.06, 1.07, 1.07, 1.08)
AE_forecast <- matrix(0, nrow=11712, ncol=10)
  for (j in 1:10){
    AE_forecast[,j] <- AE*ratio_AE[j]
}
#----------------------------------------------------------------------------------
#Dividing forecasted AE loads into months

AE_forecast_june <-AE_forecast[1:2880, 1:10]
AE_forecast_july <-AE_forecast[2881:5856, 1:10]
AE_forecast_aug <-AE_forecast[5857:8832, 1:10]
AE_forecast_sept <-AE_forecast[8833:11712, 1:10]


#----------------------------------------------------------------------------------
#Solar Data 80 MW - Typical meteorological year from NREL PVWatts calculator
#----------------------------------------------------------------------------------
#setwd("/Users/arkasama/Google Drive/Dissertation_Chapter_1")
#Importing values (input files not provided as a part of this code)
Solar_80_raw <- read.csv("80MW_pvwatts_hourly.csv")
Solar_80_raw <- Solar_80_raw[-c(1:17),]
Solar_80_power <- as.numeric(as.character(Solar_80_raw [,11]))
month_solar <- as.numeric(as.character(Solar_80_raw [,1]))
#getting values for June, July, August and September
Solar_80_june<- Solar_80_power[which(month_solar==6)]
Solar_80_july<- Solar_80_power[which(month_solar==7)]
Solar_80_aug<- Solar_80_power[which(month_solar==8)]
Solar_80_sept<- Solar_80_power[which(month_solar==9)]
Solar_80_oct<- Solar_80_power[which(month_solar==10)]
#for interpolation, the first two values of october needs to be used
Solar_80 <-rbind(c(Solar_80_june[1:720], Solar_80_july, Solar_80_aug, Solar_80_sept, Solar_80_oct[1]))

#Interpolating
x <- c(1: length(Solar_80))
xout <- seq(from = 1, to = length(Solar_80), by = 0.25)
interp_solar_80 <- approx(x, Solar_80, xout, method = "linear")
Solar_80_monthly <-as.data.frame(interp_solar_80[2])
Solar_80_monthly <-Solar_80_monthly[-nrow(Solar_80_monthly),]

#dividing solar data into months
Solar_80_june <-Solar_80_monthly[1:2880]
Solar_80_july <-Solar_80_monthly[2881:5856]
Solar_80_aug <-Solar_80_monthly[5857:8832]
Solar_80_sept <-Solar_80_monthly[8833:11712]


#----------------------------------------------------------------------------------
#Solar Data 100 MW
#----------------------------------------------------------------------------------
#setwd("/Users/arkasama/Google Drive/Dissertation_Chapter_1")
#Importing values(input files not provided as a part of this code)
Solar_100_raw <- read.csv("100MW_pvwatts_hourly.csv")
Solar_100_raw <- Solar_100_raw[-c(1:17),]
Solar_100_power <- as.numeric(as.character(Solar_100_raw [,11]))
month_solar <- as.numeric(as.character(Solar_100_raw [,1]))
#getting values for June, July, August and September
Solar_100_june<- Solar_100_power[which(month_solar==6)]
Solar_100_july<- Solar_100_power[which(month_solar==7)]
Solar_100_aug<- Solar_100_power[which(month_solar==8)]
Solar_100_sept<- Solar_100_power[which(month_solar==9)]
Solar_100_oct<- Solar_100_power[which(month_solar==10)]
#for interpolation, the first two values of october needs to be used
Solar_100 <-rbind(c(Solar_100_june[1:720], Solar_100_july, Solar_100_aug, Solar_100_sept, Solar_100_oct[1]))

#Interpolating
x <- c(1: length(Solar_100))
xout <- seq(from = 1, to = length(Solar_100), by = 0.25)
interp_solar_100 <- approx(x, Solar_100, xout, method = "linear")
Solar_100_monthly <-as.data.frame(interp_solar_100[2])
Solar_100_monthly <-Solar_100_monthly[-nrow(Solar_100_monthly),]

#dividing solar data into months
Solar_100_june <-Solar_100_monthly[1:2880]
Solar_100_july <-Solar_100_monthly[2881:5856]
Solar_100_aug <-Solar_100_monthly[5857:8832]
Solar_100_sept <-Solar_100_monthly[8833:11712]

#----------------------------------------------------------------------------------
#Solar Data 120 MW
#----------------------------------------------------------------------------------
#setwd("/Users/arkasama/Google Drive/Dissertation_Chapter_1")
#Importing values (input files not provided as a part of this code)
Solar_120_raw <- read.csv("120MW_pvwatts_hourly.csv")
Solar_120_raw <- Solar_120_raw[-c(1:17),]
Solar_120_power <- as.numeric(as.character(Solar_120_raw [,11]))
month_solar <- as.numeric(as.character(Solar_120_raw [,1]))
#getting values for June, July, August and September
Solar_120_june<- Solar_120_power[which(month_solar==6)]
Solar_120_july<- Solar_120_power[which(month_solar==7)]
Solar_120_aug<- Solar_120_power[which(month_solar==8)]
Solar_120_sept<- Solar_120_power[which(month_solar==9)]
Solar_120_oct<- Solar_120_power[which(month_solar==10)]
#for interpolation, the first two values of october need to be used
Solar_120 <-rbind(c(Solar_120_june[1:720], Solar_120_july, Solar_120_aug, Solar_120_sept, Solar_120_oct[1]))

#Interpolating
x <- c(1: length(Solar_120))
xout <- seq(from = 1, to = length(Solar_120), by = 0.25)
interp_solar_120 <- approx(x, Solar_120, xout, method = "linear")
Solar_120_monthly <-as.data.frame(interp_solar_120[2])
Solar_120_monthly <-Solar_120_monthly[-nrow(Solar_120_monthly),]

#dividing solar data into months
Solar_120_june <-Solar_120_monthly[1:2880]
Solar_120_july <-Solar_120_monthly[2881:5856]
Solar_120_aug <-Solar_120_monthly[5857:8832]
Solar_120_sept <-Solar_120_monthly[8833:11712]

#----------------------------------------------------------------------------------
#Solar Data 140 MW
#----------------------------------------------------------------------------------
#setwd("/Users/arkasama/Google Drive/Dissertation_Chapter_1")
#Importing values (input files not provided as a part of this code)
Solar_140_raw <- read.csv("140MW_pvwatts_hourly.csv")
Solar_140_raw <- Solar_140_raw[-c(1:17),]
Solar_140_power <- as.numeric(as.character(Solar_140_raw [,11]))
month_solar <- as.numeric(as.character(Solar_140_raw [,1]))
#getting values for June, July, August and September
Solar_140_june<- Solar_140_power[which(month_solar==6)]
Solar_140_july<- Solar_140_power[which(month_solar==7)]
Solar_140_aug<- Solar_140_power[which(month_solar==8)]
Solar_140_sept<- Solar_140_power[which(month_solar==9)]
Solar_140_oct<- Solar_140_power[which(month_solar==10)]
#for interpolation, the first two values of october need to be used
Solar_140 <-rbind(c(Solar_140_june[1:720], Solar_140_july, Solar_140_aug, Solar_140_sept, Solar_140_oct[1]))

#Interpolating
x <- c(1: length(Solar_140))
xout <- seq(from = 1, to = length(Solar_140), by = 0.25)
interp_solar_140 <- approx(x, Solar_140, xout, method = "linear")
Solar_140_monthly <-as.data.frame(interp_solar_140[2])
Solar_140_monthly <-Solar_140_monthly[-nrow(Solar_140_monthly),]

#dividing solar data into months
Solar_140_june <-Solar_140_monthly[1:2880]
Solar_140_july <-Solar_140_monthly[2881:5856]
Solar_140_aug <-Solar_140_monthly[5857:8832]
Solar_140_sept <-Solar_140_monthly[8833:11712]
#----------------------------------------------------------------------------------
#Solar Data 160 MW
#----------------------------------------------------------------------------------
#setwd("/Users/arkasama/Google Drive/Dissertation_Chapter_1")
#Importing values (input files not provided as a part of this code)
Solar_160_raw <- read.csv("160MW_pvwatts_hourly.csv")
Solar_160_raw <- Solar_160_raw[-c(1:17),]
Solar_160_power <- as.numeric(as.character(Solar_160_raw [,11]))
month_solar <- as.numeric(as.character(Solar_160_raw [,1]))
#getting values for June, July, August and September
Solar_160_june<- Solar_160_power[which(month_solar==6)]
Solar_160_july<- Solar_160_power[which(month_solar==7)]
Solar_160_aug<- Solar_160_power[which(month_solar==8)]
Solar_160_sept<- Solar_160_power[which(month_solar==9)]
Solar_160_oct<- Solar_160_power[which(month_solar==10)]
#for interpolation, the first two values of october need to be used
Solar_160 <-rbind(c(Solar_160_june[1:720], Solar_160_july, Solar_160_aug, Solar_160_sept, Solar_160_oct[1]))

#Interpolating
x <- c(1: length(Solar_160))
xout <- seq(from = 1, to = length(Solar_160), by = 0.25)
interp_solar_160 <- approx(x, Solar_160, xout, method = "linear")
Solar_160_monthly <-as.data.frame(interp_solar_160[2])
Solar_160_monthly <-Solar_160_monthly[-nrow(Solar_160_monthly),]

#dividing solar data into months
Solar_160_june <-Solar_160_monthly[1:2880]
Solar_160_july <-Solar_160_monthly[2881:5856]
Solar_160_aug <-Solar_160_monthly[5857:8832]
Solar_160_sept <-Solar_160_monthly[8833:11712]

#----------------------------------------------------------------------------------
#Solar Data 180 MW
#----------------------------------------------------------------------------------
#setwd("/Users/arkasama/Google Drive/Dissertation_Chapter_1")
#Importing values (input files not provided as a part of this code)
Solar_180_raw <- read.csv("180MW_pvwatts_hourly.csv")
Solar_180_raw <- Solar_180_raw[-c(1:17),]
Solar_180_power <- as.numeric(as.character(Solar_180_raw [,11]))
month_solar <- as.numeric(as.character(Solar_180_raw [,1]))
#getting values for June, July, August and September
Solar_180_june<- Solar_180_power[which(month_solar==6)]
Solar_180_july<- Solar_180_power[which(month_solar==7)]
Solar_180_aug<- Solar_180_power[which(month_solar==8)]
Solar_180_sept<- Solar_180_power[which(month_solar==9)]
Solar_180_oct<- Solar_180_power[which(month_solar==10)]
#for interpolation, the first two values of october need to be used
Solar_180 <-rbind(c(Solar_180_june[1:720], Solar_180_july, Solar_180_aug, Solar_180_sept, Solar_180_oct[1]))

#Interpolating
x <- c(1: length(Solar_180))
xout <- seq(from = 1, to = length(Solar_180), by = 0.25)
interp_solar_180 <- approx(x, Solar_180, xout, method = "linear")
Solar_180_monthly <-as.data.frame(interp_solar_180[2])
Solar_180_monthly <-Solar_180_monthly[-nrow(Solar_180_monthly),]

#dividing solar data into months
Solar_180_june <-Solar_180_monthly[1:2880]
Solar_180_july <-Solar_180_monthly[2881:5856]
Solar_180_aug <-Solar_180_monthly[5857:8832]
Solar_180_sept <-Solar_180_monthly[8833:11712]

#----------------------------------------------------------------------------------
#Base Case: Load calculation Solar: 80 MW, Storage: 5 MW (10 MWh)
#----------------------------------------------------------------------------------
storage_base <- 5; # MW in 15 mins
#defining matrices
AE_4CP_june_base <- matrix(0, nrow=1, ncol=10)
AE_4CP_july_base <- matrix(0, nrow=1, ncol=10)
AE_4CP_aug_base <- matrix(0, nrow=1, ncol=10)
AE_4CP_sept_base <-matrix(0, nrow=1, ncol=10)

#4CP Load = Load when ERCOT peaks - solar - storage
for (i in 1:10) {
  AE_4CP_june_base[,i] <- AE_forecast_june[I_0_june[i], i] - Solar_80_june[I_0_june[i]]/1000000 - storage_base # Solar data is in W and so divided by 10^6 to convert to MW
  AE_4CP_july_base[,i] <- AE_forecast_july[I_0_july[i], i]- Solar_80_july[I_0_july[i]]/1000000 - storage_base
  AE_4CP_aug_base[,i] <- AE_forecast_aug[I_0_aug[i], i]- Solar_80_aug[I_0_aug[i]]/1000000 - storage_base
  AE_4CP_sept_base[,i] <- AE_forecast_sept[I_0_sept[i], i]- Solar_80_sept[I_0_sept[i]]/1000000- storage_base
}
AE_4CP_base_interm <- rbind(AE_4CP_june_base, AE_4CP_july_base, AE_4CP_aug_base, AE_4CP_sept_base)
#average 4CP load from the 4 months
AE_4CP_base=apply(AE_4CP_base_interm,2, sum)/4


#----------------------------------------------------------------------------------
#Base Case: 4CP Payment Solar: 80 MW, Storage: 5 MW (10 MWh)
#----------------------------------------------------------------------------------
#defining the matrices
ERCOT_1_june_base <- matrix(0, nrow=1, ncol=10)
ERCOT_1_july_base <- matrix(0, nrow=1, ncol=10)
ERCOT_1_aug_base <- matrix(0, nrow=1, ncol=10)
ERCOT_1_sept_base <- matrix(0, nrow=1, ncol=10)
AE_4CP_Payment_base <-matrix(0, nrow=1, ncol=10)

#ERCOT 4CP Load: = max ERCOT 4CP - solar - storage 
for (i in 1:10){
  ERCOT_1_june_base[,i]=M_0_june[i] - Solar_80_june[I_0_june[i]]/1000000 - storage_base
  ERCOT_1_july_base[,i]=M_0_july[i] - Solar_80_july[I_0_july[i]]/1000000 - storage_base
  ERCOT_1_aug_base[,i]=M_0_aug[i] - Solar_80_aug[(I_0_aug[i])]/1000000 - storage_base
  ERCOT_1_sept_base[,i]=M_0_sept[i] - Solar_80_sept[(I_0_sept[i])] /1000000- storage_base
}
#Averaging across the four months
ERCOT_4CP_interm <- rbind(ERCOT_1_june_base, ERCOT_1_july_base, ERCOT_1_aug_base, ERCOT_1_sept_base)
ERCOT_4CP<-apply(ERCOT_4CP_interm, 2,sum)/4
access_fee_base <- 1.187*1000; #$/MW #Austin Energy's transmission rate as of 2018
#Future projections of transmission rates
TSP_new <- c(55.68, 59.44, 63.20, 66.976, 70.72, 74.48, 78.24, 82, 85.76, 89.52, 93.28) #linear regression using historical transmission rates from the transmission charge matrix available publicly on the PUCT website
sum_access_fee <- matrix(c(TSP_new[2:11]),nrow=1, ncol=10) #$/kW
for  (i in 1:10){
  AE_4CP_Payment_base[,i] <- -ERCOT_4CP[i] * access_fee_base + AE_4CP_base[i]*sum_access_fee[i]*1000 #multiplied by 1000 to convert from $/kW to $/MW
}

#===========================================================================================
#Calculating 4CP Loads (MW) - Solar sensitivity: 80 MW, 100 MW, 120 MW, 140 MW, 160 MW, 180 MW, Storage = 5 MW (10 MWh) 
#===========================================================================================
#Initializing matrices for 80 MW
AE_4CP_june_solar_80 <-matrix(0, nrow=1, ncol=10)
AE_4CP_july_solar_80 <-matrix(0, nrow=1, ncol=10)
AE_4CP_aug_solar_80 <-matrix(0, nrow=1, ncol=10)
AE_4CP_sept_solar_80 <-matrix(0, nrow=1, ncol=10)

#Initializing matrices for 100 MW
AE_4CP_june_solar_100 <-matrix(0, nrow=1, ncol=10)
AE_4CP_july_solar_100 <-matrix(0, nrow=1, ncol=10)
AE_4CP_aug_solar_100 <-matrix(0, nrow=1, ncol=10)
AE_4CP_sept_solar_100 <-matrix(0, nrow=1, ncol=10)

#Initializing matrices for 120 MW
AE_4CP_june_solar_120 <-matrix(0, nrow=1, ncol=10)
AE_4CP_july_solar_120 <-matrix(0, nrow=1, ncol=10)
AE_4CP_aug_solar_120 <-matrix(0, nrow=1, ncol=10)
AE_4CP_sept_solar_120 <-matrix(0, nrow=1, ncol=10)

#Initializing matrices for 140 MW
AE_4CP_june_solar_140 <-matrix(0, nrow=1, ncol=10)
AE_4CP_july_solar_140 <-matrix(0, nrow=1, ncol=10)
AE_4CP_aug_solar_140 <-matrix(0, nrow=1, ncol=10)
AE_4CP_sept_solar_140 <-matrix(0, nrow=1, ncol=10)

#Initializing matrices for 160 MW
AE_4CP_june_solar_160 <-matrix(0, nrow=1, ncol=10)
AE_4CP_july_solar_160 <-matrix(0, nrow=1, ncol=10)
AE_4CP_aug_solar_160 <-matrix(0, nrow=1, ncol=10)
AE_4CP_sept_solar_160 <-matrix(0, nrow=1, ncol=10)

#Initializing matrices for 180 MW
AE_4CP_june_solar_180 <-matrix(0, nrow=1, ncol=10)
AE_4CP_july_solar_180 <-matrix(0, nrow=1, ncol=10)
AE_4CP_aug_solar_180 <-matrix(0, nrow=1, ncol=10)
AE_4CP_sept_solar_180 <-matrix(0, nrow=1, ncol=10)


#4CP Load = Load when ERCOT peaks - solar - storage
for (i in 1:10){
  AE_4CP_june_solar_80[i]<-AE_forecast_june[I_0_june[i], i] - Solar_80_june[I_0_june[i]]/1000000 - storage_base
  AE_4CP_july_solar_80[i]<-AE_forecast_july[I_0_july[i], i] - Solar_80_july[I_0_july[i]]/1000000 - storage_base
  AE_4CP_aug_solar_80[i]<-AE_forecast_aug[I_0_aug[i], i] - Solar_80_aug[I_0_aug[i]]/1000000 - storage_base
  AE_4CP_sept_solar_80[i]<-AE_forecast_sept[I_0_sept[i], i] - Solar_80_sept[I_0_sept[i]]/1000000 - storage_base
  
  AE_4CP_june_solar_100[i]<-AE_forecast_june[I_0_june[i], i] - Solar_100_june[I_0_june[i]]/1000000 - storage_base
  AE_4CP_july_solar_100[i]<-AE_forecast_july[I_0_july[i], i] - Solar_100_july[I_0_july[i]] /1000000- storage_base
  AE_4CP_aug_solar_100[i]<-AE_forecast_aug[I_0_aug[i], i] - Solar_100_aug[I_0_aug[i]]/1000000 - storage_base
  AE_4CP_sept_solar_100[i]<-AE_forecast_sept[I_0_sept[i], i] - Solar_100_sept[I_0_sept[i]]/1000000 - storage_base
  
  AE_4CP_june_solar_120[i]<-AE_forecast_june[I_0_june[i], i] - Solar_120_june[I_0_june[i]]/1000000 - storage_base
  AE_4CP_july_solar_120[i]<-AE_forecast_july[I_0_july[i], i] - Solar_120_july[I_0_july[i]]/1000000 - storage_base
  AE_4CP_aug_solar_120[i]<-AE_forecast_aug[I_0_aug[i], i] - Solar_120_aug[I_0_aug[i]] /1000000- storage_base
  AE_4CP_sept_solar_120[i]<-AE_forecast_sept[I_0_sept[i], i] - Solar_120_sept[I_0_sept[i]]/1000000 - storage_base

  AE_4CP_june_solar_140[i]<-AE_forecast_june[I_0_june[i], i] - Solar_140_june[I_0_june[i]]/1000000 - storage_base
  AE_4CP_july_solar_140[i]<-AE_forecast_july[I_0_july[i], i] - Solar_140_july[I_0_july[i]]/1000000 - storage_base
  AE_4CP_aug_solar_140[i]<-AE_forecast_aug[I_0_aug[i], i] - Solar_140_aug[I_0_aug[i]] /1000000- storage_base
  AE_4CP_sept_solar_140[i]<-AE_forecast_sept[I_0_sept[i], i] - Solar_140_sept[I_0_sept[i]]/1000000 - storage_base
  
  AE_4CP_june_solar_160[i]<-AE_forecast_june[I_0_june[i], i] - Solar_160_june[I_0_june[i]]/1000000 - storage_base
  AE_4CP_july_solar_160[i]<-AE_forecast_july[I_0_july[i], i] - Solar_160_july[I_0_july[i]]/1000000 - storage_base
  AE_4CP_aug_solar_160[i]<-AE_forecast_aug[I_0_aug[i], i] - Solar_160_aug[I_0_aug[i]] /1000000- storage_base
  AE_4CP_sept_solar_160[i]<-AE_forecast_sept[I_0_sept[i], i] - Solar_160_sept[I_0_sept[i]]/1000000 - storage_base
  
  AE_4CP_june_solar_180[i]<-AE_forecast_june[I_0_june[i], i] - Solar_180_june[I_0_june[i]]/1000000 - storage_base
  AE_4CP_july_solar_180[i]<-AE_forecast_july[I_0_july[i], i] - Solar_180_july[I_0_july[i]]/1000000 - storage_base
  AE_4CP_aug_solar_180[i]<-AE_forecast_aug[I_0_aug[i], i] - Solar_180_aug[I_0_aug[i]] /1000000- storage_base
  AE_4CP_sept_solar_180[i]<-AE_forecast_sept[I_0_sept[i], i] - Solar_180_sept[I_0_sept[i]]/1000000 - storage_base
  
  }
#Averaging over the four summer months to find the 4CP load
AE_4CP_solar_80_interm <- rbind(AE_4CP_june_solar_80, AE_4CP_july_solar_80, AE_4CP_aug_solar_80, AE_4CP_sept_solar_80)
AE_4CP_solar_80 <- apply(AE_4CP_solar_80_interm,2, sum)/4

AE_4CP_solar_100_interm <- rbind(AE_4CP_june_solar_100, AE_4CP_july_solar_100, AE_4CP_aug_solar_100, AE_4CP_sept_solar_100)
AE_4CP_solar_100 <- apply(AE_4CP_solar_100_interm,2, sum)/4

AE_4CP_solar_120_interm <- rbind(AE_4CP_june_solar_120, AE_4CP_july_solar_120, AE_4CP_aug_solar_120, AE_4CP_sept_solar_120)
AE_4CP_solar_120 <- apply(AE_4CP_solar_120_interm,2, sum)/4

AE_4CP_solar_140_interm <- rbind(AE_4CP_june_solar_140, AE_4CP_july_solar_140, AE_4CP_aug_solar_140, AE_4CP_sept_solar_140)
AE_4CP_solar_140 <- apply(AE_4CP_solar_140_interm,2, sum)/4

AE_4CP_solar_160_interm <- rbind(AE_4CP_june_solar_160, AE_4CP_july_solar_160, AE_4CP_aug_solar_160, AE_4CP_sept_solar_160)
AE_4CP_solar_160 <- apply(AE_4CP_solar_160_interm,2, sum)/4

AE_4CP_solar_180_interm <- rbind(AE_4CP_june_solar_180, AE_4CP_july_solar_180, AE_4CP_aug_solar_180, AE_4CP_sept_solar_180)
AE_4CP_solar_180 <- apply(AE_4CP_solar_180_interm,2, sum)/4

#===========================================================================================
#Calculating 4CP Payments - Solar sensitivity:80 MW, 100 MW, 120 MW, 140 MW, 160 MW, 180 MW, Storage = 5 MW (10 MWh) 
#===========================================================================================
#Initializing matrices to calculate 4CP load for ERCOT for 80 MW
ERCOT_1_june_solar_80 <-matrix(0, nrow=1, ncol=10)
ERCOT_1_july_solar_80 <-matrix(0, nrow=1, ncol=10)
ERCOT_1_aug_solar_80 <-matrix(0, nrow=1, ncol=10)
ERCOT_1_sept_solar_80 <-matrix(0, nrow=1, ncol=10)

#Initializing matrices to calculate 4CP load for ERCOT for 100 MW
ERCOT_1_june_solar_100 <-matrix(0, nrow=1, ncol=10)
ERCOT_1_july_solar_100 <-matrix(0, nrow=1, ncol=10)
ERCOT_1_aug_solar_100 <-matrix(0, nrow=1, ncol=10)
ERCOT_1_sept_solar_100 <-matrix(0, nrow=1, ncol=10)

#Initializing matrices to calculate 4CP load for ERCOT for 120 MW
ERCOT_1_june_solar_120 <-matrix(0, nrow=1, ncol=10)
ERCOT_1_july_solar_120 <-matrix(0, nrow=1, ncol=10)
ERCOT_1_aug_solar_120 <-matrix(0, nrow=1, ncol=10)
ERCOT_1_sept_solar_120 <-matrix(0, nrow=1, ncol=10)

#Initializing matrices to calculate 4CP load for ERCOT for 140 MW
ERCOT_1_june_solar_140 <-matrix(0, nrow=1, ncol=10)
ERCOT_1_july_solar_140 <-matrix(0, nrow=1, ncol=10)
ERCOT_1_aug_solar_140 <-matrix(0, nrow=1, ncol=10)
ERCOT_1_sept_solar_140 <-matrix(0, nrow=1, ncol=10)

#Initializing matrices to calculate 4CP load for ERCOT for 160 MW
ERCOT_1_june_solar_160 <-matrix(0, nrow=1, ncol=10)
ERCOT_1_july_solar_160 <-matrix(0, nrow=1, ncol=10)
ERCOT_1_aug_solar_160 <-matrix(0, nrow=1, ncol=10)
ERCOT_1_sept_solar_160 <-matrix(0, nrow=1, ncol=10)

#Initializing matrices to calculate 4CP load for ERCOT for 180 MW
ERCOT_1_june_solar_180 <-matrix(0, nrow=1, ncol=10)
ERCOT_1_july_solar_180 <-matrix(0, nrow=1, ncol=10)
ERCOT_1_aug_solar_180 <-matrix(0, nrow=1, ncol=10)
ERCOT_1_sept_solar_180 <-matrix(0, nrow=1, ncol=10)

#Initializing matrices to calculate 4CP Payments for Austin Energy for different solar capacities
AE_4CP_Payment_solar_80<-matrix(0, nrow=1, ncol=10)
AE_4CP_Payment_solar_100<-matrix(0, nrow=1, ncol=10)
AE_4CP_Payment_solar_120<-matrix(0, nrow=1, ncol=10)
AE_4CP_Payment_solar_140<-matrix(0, nrow=1, ncol=10)
AE_4CP_Payment_solar_160<-matrix(0, nrow=1, ncol=10)
AE_4CP_Payment_solar_180<-matrix(0, nrow=1, ncol=10)


for (i in 1:10){
  ERCOT_1_june_solar_80[i] <-M_0_june[i] - Solar_80_june[I_0_june[i]]/1000000 - storage_base
  ERCOT_1_july_solar_80[i]<-M_0_july[i] - Solar_80_july[I_0_july[i]]/1000000 - storage_base
  ERCOT_1_aug_solar_80[i]<-M_0_aug[i] - Solar_80_aug[I_0_aug[i]]/1000000 - storage_base
  ERCOT_1_sept_solar_80[i]<-M_0_sept[i] - Solar_80_sept[I_0_sept[i]]/1000000 - storage_base
  
  ERCOT_1_june_solar_100[i] <-M_0_june[i] - Solar_100_june[I_0_june[i]]/1000000 - storage_base
  ERCOT_1_july_solar_100[i]<-M_0_july[i] - Solar_100_july[I_0_july[i]]/1000000 - storage_base
  ERCOT_1_aug_solar_100[i]<-M_0_aug[i] - Solar_100_aug[I_0_aug[i]]/1000000 - storage_base
  ERCOT_1_sept_solar_100[i]<-M_0_sept[i] - Solar_100_sept[I_0_sept[i]]/1000000 - storage_base
  
  ERCOT_1_june_solar_120[i]<-M_0_june[i] - Solar_120_june[I_0_june[i]]/1000000 - storage_base
  ERCOT_1_july_solar_120[i]<-M_0_july[i] - Solar_120_july[I_0_july[i]]/1000000 - storage_base
  ERCOT_1_aug_solar_120[i]<-M_0_aug[i] - Solar_120_aug[I_0_aug[i]]/1000000- storage_base
  ERCOT_1_sept_solar_120[i]<-M_0_sept[i] - Solar_120_sept[I_0_sept[i]]/1000000 - storage_base
  
  ERCOT_1_june_solar_140[i] <-M_0_june[i] - Solar_140_june[I_0_june[i]]/1000000 - storage_base
  ERCOT_1_july_solar_140[i]<-M_0_july[i] - Solar_140_july[I_0_july[i]]/1000000 - storage_base
  ERCOT_1_aug_solar_140[i]<-M_0_aug[i] - Solar_140_aug[I_0_aug[i]]/1000000 - storage_base
  ERCOT_1_sept_solar_140[i]<-M_0_sept[i] - Solar_140_sept[I_0_sept[i]]/1000000 - storage_base
  
  ERCOT_1_june_solar_160[i] <-M_0_june[i] - Solar_160_june[I_0_june[i]]/1000000 - storage_base
  ERCOT_1_july_solar_160[i]<-M_0_july[i] - Solar_160_july[I_0_july[i]]/1000000 - storage_base
  ERCOT_1_aug_solar_160[i]<-M_0_aug[i] - Solar_160_aug[I_0_aug[i]]/1000000 - storage_base
  ERCOT_1_sept_solar_160[i]<-M_0_sept[i] - Solar_160_sept[I_0_sept[i]]/1000000 - storage_base
  
  ERCOT_1_june_solar_180[i] <-M_0_june[i] - Solar_180_june[I_0_june[i]]/1000000 - storage_base
  ERCOT_1_july_solar_180[i]<-M_0_july[i] - Solar_180_july[I_0_july[i]]/1000000 - storage_base
  ERCOT_1_aug_solar_180[i]<-M_0_aug[i] - Solar_180_aug[I_0_aug[i]]/1000000 - storage_base
  ERCOT_1_sept_solar_180[i]<-M_0_sept[i] - Solar_180_sept[I_0_sept[i]]/1000000 - storage_base
  
}

#Averaging across the four summer months
ERCOT_4CP_solar_80_interm<-rbind(ERCOT_1_june_solar_80, ERCOT_1_july_solar_80, ERCOT_1_aug_solar_80, ERCOT_1_sept_solar_80)
ERCOT_4CP_solar_80<-apply(ERCOT_4CP_solar_80_interm, 2, sum)/4

ERCOT_4CP_solar_100_interm<-rbind(ERCOT_1_june_solar_100, ERCOT_1_july_solar_100, ERCOT_1_aug_solar_100, ERCOT_1_sept_solar_100)
ERCOT_4CP_solar_100<-apply(ERCOT_4CP_solar_100_interm, 2, sum)/4
ERCOT_4CP_solar_120_interm<-rbind(ERCOT_1_june_solar_120, ERCOT_1_july_solar_120, ERCOT_1_aug_solar_120, ERCOT_1_sept_solar_120)
ERCOT_4CP_solar_120<-apply(ERCOT_4CP_solar_120_interm, 2, sum)/4
ERCOT_4CP_solar_140_interm<-rbind(ERCOT_1_june_solar_140, ERCOT_1_july_solar_140, ERCOT_1_aug_solar_140, ERCOT_1_sept_solar_140)
ERCOT_4CP_solar_140<-apply(ERCOT_4CP_solar_140_interm, 2, sum)/4
ERCOT_4CP_solar_160_interm<-rbind(ERCOT_1_june_solar_160, ERCOT_1_july_solar_160, ERCOT_1_aug_solar_160, ERCOT_1_sept_solar_160)
ERCOT_4CP_solar_160<-apply(ERCOT_4CP_solar_160_interm, 2, sum)/4
ERCOT_4CP_solar_180_interm<-rbind(ERCOT_1_june_solar_180, ERCOT_1_july_solar_180, ERCOT_1_aug_solar_180, ERCOT_1_sept_solar_180)
ERCOT_4CP_solar_180<-apply(ERCOT_4CP_solar_180_interm, 2, sum)/4

for (i in 1:10) { #for each of the 10 years of forecast
  AE_4CP_Payment_solar_80[i]<- -ERCOT_4CP_solar_80[i] * access_fee_base + AE_4CP_solar_80[i]*sum_access_fee[i]*1000
  AE_4CP_Payment_solar_100[i]<- -ERCOT_4CP_solar_100[i] * access_fee_base + AE_4CP_solar_100[i]*sum_access_fee[i]*1000;
  AE_4CP_Payment_solar_120[i]<- -ERCOT_4CP_solar_120[i] * access_fee_base + AE_4CP_solar_120[i]*sum_access_fee[i]*1000;
  AE_4CP_Payment_solar_140[i]<- -ERCOT_4CP_solar_140[i] * access_fee_base + AE_4CP_solar_140[i]*sum_access_fee[i]*1000;
  AE_4CP_Payment_solar_160[i]<- -ERCOT_4CP_solar_160[i] * access_fee_base + AE_4CP_solar_160[i]*sum_access_fee[i]*1000;
  AE_4CP_Payment_solar_180[i]<- -ERCOT_4CP_solar_180[i] * access_fee_base + AE_4CP_solar_180[i]*sum_access_fee[i]*1000;
  
  
}
#Diff in Austin Energy's 4CP Payments (units of thousands of dollars) from the base case 
diff_1 <- (-AE_4CP_Payment_solar_100+AE_4CP_Payment_solar_80)/10^3
diff_2 <- (-AE_4CP_Payment_solar_120+AE_4CP_Payment_solar_80)/10^3
diff_3 <- (-AE_4CP_Payment_solar_140+AE_4CP_Payment_solar_80)/10^3
diff_4 <- (-AE_4CP_Payment_solar_160+AE_4CP_Payment_solar_80)/10^3
diff_5 <- (-AE_4CP_Payment_solar_180+AE_4CP_Payment_solar_80)/10^3
#===========================================================================================
#Calculating 4CP Loads (MW) - Storage sensitivity: 5 MW (10 MWh), 10 MW (20 MWh), 15 MW (30 MWh), 20 MW (30 MWh), 25 MW (50 MWh), 30 MW (60 MWh), Solar = 80 MW
#===========================================================================================
#Initializing matrices for 4CP loads for 5 MW (10 MWh)
AE_4CP_june_storage_5 <-matrix(0, nrow=1, ncol=10)
AE_4CP_july_storage_5 <-matrix(0, nrow=1, ncol=10)
AE_4CP_aug_storage_5 <-matrix(0, nrow=1, ncol=10)
AE_4CP_sept_storage_5 <-matrix(0, nrow=1, ncol=10)

#Initializing matrices for 4CP loads for 10 MW (20 MWh)
AE_4CP_june_storage_10 <-matrix(0, nrow=1, ncol=10)
AE_4CP_july_storage_10 <-matrix(0, nrow=1, ncol=10)
AE_4CP_aug_storage_10 <-matrix(0, nrow=1, ncol=10)
AE_4CP_sept_storage_10 <-matrix(0, nrow=1, ncol=10)

#Initializing matrices for 4CP loads for 15 MW (30 MWh)
AE_4CP_june_storage_15 <-matrix(0, nrow=1, ncol=10)
AE_4CP_july_storage_15 <-matrix(0, nrow=1, ncol=10)
AE_4CP_aug_storage_15 <-matrix(0, nrow=1, ncol=10)
AE_4CP_sept_storage_15 <-matrix(0, nrow=1, ncol=10)

#Initializing matrices for 4CP loads for 20 MW (40 MWh)
AE_4CP_june_storage_20 <-matrix(0, nrow=1, ncol=10)
AE_4CP_july_storage_20 <-matrix(0, nrow=1, ncol=10)
AE_4CP_aug_storage_20 <-matrix(0, nrow=1, ncol=10)
AE_4CP_sept_storage_20 <-matrix(0, nrow=1, ncol=10)

#Initializing matrices for 4CP loads for 25 MW (50 MWh)
AE_4CP_june_storage_25 <-matrix(0, nrow=1, ncol=10)
AE_4CP_july_storage_25 <-matrix(0, nrow=1, ncol=10)
AE_4CP_aug_storage_25 <-matrix(0, nrow=1, ncol=10)
AE_4CP_sept_storage_25 <-matrix(0, nrow=1, ncol=10)

#Initializing matrices for 4CP loads for 30 MW (60 MWh)
AE_4CP_june_storage_30 <-matrix(0, nrow=1, ncol=10)
AE_4CP_july_storage_30 <-matrix(0, nrow=1, ncol=10)
AE_4CP_aug_storage_30 <-matrix(0, nrow=1, ncol=10)
AE_4CP_sept_storage_30 <-matrix(0, nrow=1, ncol=10)
for (i in 1:10){
  AE_4CP_june_storage_5[i]<-AE_forecast_june[I_0_june[i], i] - Solar_80_june[I_0_june[i]]/1000000 - storage_base
  AE_4CP_july_storage_5[i]<-AE_forecast_july[I_0_july[i], i] - Solar_80_july[I_0_july[i]]/1000000 - storage_base
  AE_4CP_aug_storage_5[i]<-AE_forecast_aug[I_0_aug[i], i] - Solar_80_aug[I_0_aug[i]]/1000000 - storage_base
  AE_4CP_sept_storage_5[i]<-AE_forecast_sept[I_0_sept[i], i] - Solar_80_sept[I_0_sept[i]]/1000000 - storage_base
  
  AE_4CP_june_storage_10[i]<-AE_forecast_june[I_0_june[i], i] - Solar_80_june[I_0_june[i]]/1000000 - 10
  AE_4CP_july_storage_10[i]<-AE_forecast_july[I_0_july[i], i] - Solar_80_july[I_0_july[i]] /1000000- 10
  AE_4CP_aug_storage_10[i]<-AE_forecast_aug[I_0_aug[i], i] - Solar_80_aug[I_0_aug[i]]/1000000 - 10
  AE_4CP_sept_storage_10[i]<-AE_forecast_sept[I_0_sept[i], i] - Solar_80_sept[I_0_sept[i]]/1000000 - 10
  
  AE_4CP_june_storage_15[i]<-AE_forecast_june[I_0_june[i], i] - Solar_80_june[I_0_june[i]]/1000000 - 15
  AE_4CP_july_storage_15[i]<-AE_forecast_july[I_0_july[i], i] - Solar_80_july[I_0_july[i]]/1000000 - 15
  AE_4CP_aug_storage_15[i]<-AE_forecast_aug[I_0_aug[i], i] - Solar_80_aug[I_0_aug[i]] /1000000- 15
  AE_4CP_sept_storage_15[i]<-AE_forecast_sept[I_0_sept[i], i] - Solar_80_sept[I_0_sept[i]]/1000000 - 15
  
  AE_4CP_june_storage_20[i]<-AE_forecast_june[I_0_june[i], i] - Solar_80_june[I_0_june[i]]/1000000 - 20
  AE_4CP_july_storage_20[i]<-AE_forecast_july[I_0_july[i], i] - Solar_80_july[I_0_july[i]]/1000000 - 20
  AE_4CP_aug_storage_20[i]<-AE_forecast_aug[I_0_aug[i], i] - Solar_80_aug[I_0_aug[i]] /1000000- 20
  AE_4CP_sept_storage_20[i]<-AE_forecast_sept[I_0_sept[i], i] - Solar_80_sept[I_0_sept[i]]/1000000 - 20
  
  AE_4CP_june_storage_25[i]<-AE_forecast_june[I_0_june[i], i] - Solar_80_june[I_0_june[i]]/1000000 - 25
  AE_4CP_july_storage_25[i]<-AE_forecast_july[I_0_july[i], i] - Solar_80_july[I_0_july[i]]/1000000 - 25
  AE_4CP_aug_storage_25[i]<-AE_forecast_aug[I_0_aug[i], i] - Solar_80_aug[I_0_aug[i]] /1000000- 25
  AE_4CP_sept_storage_25[i]<-AE_forecast_sept[I_0_sept[i], i] - Solar_80_sept[I_0_sept[i]]/1000000 - 25
  
  AE_4CP_june_storage_30[i]<-AE_forecast_june[I_0_june[i], i] - Solar_80_june[I_0_june[i]]/1000000 - 30
  AE_4CP_july_storage_30[i]<-AE_forecast_july[I_0_july[i], i] - Solar_80_july[I_0_july[i]]/1000000 - 30
  AE_4CP_aug_storage_30[i]<-AE_forecast_aug[I_0_aug[i], i] - Solar_80_aug[I_0_aug[i]] /1000000- 30
  AE_4CP_sept_storage_30[i]<-AE_forecast_sept[I_0_sept[i], i] - Solar_80_sept[I_0_sept[i]]/1000000 - 30
}
#Averaging across the four summer months
AE_4CP_storage_5_interm <- rbind(AE_4CP_june_storage_5, AE_4CP_july_storage_5, AE_4CP_aug_storage_5, AE_4CP_sept_storage_5)
AE_4CP_storage_5 <- apply(AE_4CP_storage_5_interm,2, sum)/4

AE_4CP_storage_10_interm <- rbind(AE_4CP_june_storage_10, AE_4CP_july_storage_10, AE_4CP_aug_storage_10, AE_4CP_sept_storage_10)
AE_4CP_storage_10 <- apply(AE_4CP_storage_10_interm,2, sum)/4

AE_4CP_storage_15_interm <- rbind(AE_4CP_june_storage_15, AE_4CP_july_storage_15, AE_4CP_aug_storage_15, AE_4CP_sept_storage_15)
AE_4CP_storage_15 <- apply(AE_4CP_storage_15_interm,2, sum)/4

AE_4CP_storage_20_interm <- rbind(AE_4CP_june_storage_20, AE_4CP_july_storage_20, AE_4CP_aug_storage_20, AE_4CP_sept_storage_20)
AE_4CP_storage_20 <- apply(AE_4CP_storage_20_interm,2, sum)/4

AE_4CP_storage_25_interm <- rbind(AE_4CP_june_storage_25, AE_4CP_july_storage_25, AE_4CP_aug_storage_25, AE_4CP_sept_storage_25)
AE_4CP_storage_25 <- apply(AE_4CP_storage_25_interm,2, sum)/4

AE_4CP_storage_30_interm <- rbind(AE_4CP_june_storage_30, AE_4CP_july_storage_30, AE_4CP_aug_storage_30, AE_4CP_sept_storage_30)
AE_4CP_storage_30 <- apply(AE_4CP_storage_30_interm,2, sum)/4
#===========================================================================================
#Calculating 4CP Payments - Storage sensitivity: 5 MW (10 MWh), 10 MW (20 MWh), 15 MW (30 MWh), 20 MW (30 MWh), 25 MW (50 MWh), 30 MW (60 MWh), Solar = 80 MW
#===========================================================================================
ERCOT_1_june_storage_10 <-matrix(0, nrow=1, ncol=10)
ERCOT_1_july_storage_10 <-matrix(0, nrow=1, ncol=10)
ERCOT_1_aug_storage_10 <-matrix(0, nrow=1, ncol=10)
ERCOT_1_sept_storage_10 <-matrix(0, nrow=1, ncol=10)

ERCOT_1_june_storage_15 <-matrix(0, nrow=1, ncol=10)
ERCOT_1_july_storage_15 <-matrix(0, nrow=1, ncol=10)
ERCOT_1_aug_storage_15 <-matrix(0, nrow=1, ncol=10)
ERCOT_1_sept_storage_15 <-matrix(0, nrow=1, ncol=10)

ERCOT_1_june_storage_20 <-matrix(0, nrow=1, ncol=10)
ERCOT_1_july_storage_20<-matrix(0, nrow=1, ncol=10)
ERCOT_1_aug_storage_20 <-matrix(0, nrow=1, ncol=10)
ERCOT_1_sept_storage_20 <-matrix(0, nrow=1, ncol=10)

ERCOT_1_june_storage_25 <-matrix(0, nrow=1, ncol=10)
ERCOT_1_july_storage_25 <-matrix(0, nrow=1, ncol=10)
ERCOT_1_aug_storage_25 <-matrix(0, nrow=1, ncol=10)
ERCOT_1_sept_storage_25 <-matrix(0, nrow=1, ncol=10)

ERCOT_1_june_storage_30 <-matrix(0, nrow=1, ncol=10)
ERCOT_1_july_storage_30 <-matrix(0, nrow=1, ncol=10)
ERCOT_1_aug_storage_30 <-matrix(0, nrow=1, ncol=10)
ERCOT_1_sept_storage_30 <-matrix(0, nrow=1, ncol=10)

AE_4CP_Payment_storage_10<-matrix(0, nrow=1, ncol=10)
AE_4CP_Payment_storage_15<-matrix(0, nrow=1, ncol=10)
AE_4CP_Payment_storage_20<-matrix(0, nrow=1, ncol=10)
AE_4CP_Payment_storage_25<-matrix(0, nrow=1, ncol=10)
AE_4CP_Payment_storage_30<-matrix(0, nrow=1, ncol=10)


percent_payment_10 <- matrix(0, nrow=1, ncol=10)
percent_payment_15 <- matrix(0, nrow=1, ncol=10)
percent_payment_20 <- matrix(0, nrow=1, ncol=10)
percent_payment_25 <- matrix(0, nrow=1, ncol=10)
percent_payment_30 <- matrix(0, nrow=1, ncol=10)

#Calculating ERCOT's 4CP load for each of the storage capacity scenarios
for (i in 1:10){
  ERCOT_1_june_storage_10[i] <-M_0_june[i] - Solar_80_june[I_0_june[i]]/1000000 - 10
  ERCOT_1_july_storage_10[i]<-M_0_july[i] - Solar_80_july[I_0_july[i]]/1000000 - 10
  ERCOT_1_aug_storage_10[i]<-M_0_aug[i] - Solar_80_aug[I_0_aug[i]]/1000000 - 10
  ERCOT_1_sept_storage_10[i]<-M_0_sept[i] - Solar_80_sept[I_0_sept[i]]/1000000 - 10
  
  ERCOT_1_june_storage_15[i]<-M_0_june[i] - Solar_80_june[I_0_june[i]]/1000000 - 15
  ERCOT_1_july_storage_15[i]<-M_0_july[i] - Solar_80_july[I_0_july[i]]/1000000 - 15
  ERCOT_1_aug_storage_15[i]<-M_0_aug[i] - Solar_80_aug[I_0_aug[i]]/1000000- 15
  ERCOT_1_sept_storage_15[i]<-M_0_sept[i] - Solar_80_sept[I_0_sept[i]]/1000000 - 15
  
  ERCOT_1_june_storage_20[i]<-M_0_june[i] - Solar_80_june[I_0_june[i]]/1000000 - 20
  ERCOT_1_july_storage_20[i]<-M_0_july[i] - Solar_80_july[I_0_july[i]]/1000000 - 20
  ERCOT_1_aug_storage_20[i]<-M_0_aug[i] - Solar_80_aug[I_0_aug[i]]/1000000- 20
  ERCOT_1_sept_storage_20[i]<-M_0_sept[i] - Solar_80_sept[I_0_sept[i]]/1000000 - 20
  
  ERCOT_1_june_storage_25[i]<-M_0_june[i] - Solar_80_june[I_0_june[i]]/1000000 - 25
  ERCOT_1_july_storage_25[i]<-M_0_july[i] - Solar_80_july[I_0_july[i]]/1000000 - 25
  ERCOT_1_aug_storage_25[i]<-M_0_aug[i] - Solar_80_aug[I_0_aug[i]]/1000000- 25
  ERCOT_1_sept_storage_25[i]<-M_0_sept[i] - Solar_80_sept[I_0_sept[i]]/1000000 - 25
  
  ERCOT_1_june_storage_30[i]<-M_0_june[i] - Solar_80_june[I_0_june[i]]/1000000 - 30
  ERCOT_1_july_storage_30[i]<-M_0_july[i] - Solar_80_july[I_0_july[i]]/1000000 - 30
  ERCOT_1_aug_storage_30[i]<-M_0_aug[i] - Solar_80_aug[I_0_aug[i]]/1000000- 30
  ERCOT_1_sept_storage_30[i]<-M_0_sept[i] - Solar_80_sept[I_0_sept[i]]/1000000 - 30
}

#Averaging across the four summer months
ERCOT_4CP_storage_10_interm<-rbind(ERCOT_1_june_storage_10, ERCOT_1_july_storage_10, ERCOT_1_aug_storage_10, ERCOT_1_sept_storage_10)
ERCOT_4CP_storage_10<-apply(ERCOT_4CP_storage_10_interm, 2, sum)/4
ERCOT_4CP_storage_15_interm<-rbind(ERCOT_1_june_storage_15, ERCOT_1_july_storage_15, ERCOT_1_aug_storage_15, ERCOT_1_sept_storage_15)
ERCOT_4CP_storage_15<-apply(ERCOT_4CP_storage_15_interm, 2, sum)/4
ERCOT_4CP_storage_20_interm<-rbind(ERCOT_1_june_storage_20, ERCOT_1_july_storage_20, ERCOT_1_aug_storage_20, ERCOT_1_sept_storage_20)
ERCOT_4CP_storage_20<-apply(ERCOT_4CP_storage_20_interm, 2, sum)/4
ERCOT_4CP_storage_25_interm<-rbind(ERCOT_1_june_storage_25, ERCOT_1_july_storage_25, ERCOT_1_aug_storage_25, ERCOT_1_sept_storage_25)
ERCOT_4CP_storage_25<-apply(ERCOT_4CP_storage_25_interm, 2, sum)/4
ERCOT_4CP_storage_30_interm<-rbind(ERCOT_1_june_storage_30, ERCOT_1_july_storage_30, ERCOT_1_aug_storage_30, ERCOT_1_sept_storage_30)
ERCOT_4CP_storage_30<-apply(ERCOT_4CP_storage_30_interm, 2, sum)/4
for (i in 1:10) { #for each of the 10 forecasted years
  AE_4CP_Payment_storage_10[i]<- -ERCOT_4CP_storage_10[i] * access_fee_base + AE_4CP_storage_10[i]*sum_access_fee[i]*1000;
  AE_4CP_Payment_storage_15[i]<- -ERCOT_4CP_storage_15[i] * access_fee_base + AE_4CP_storage_15[i]*sum_access_fee[i]*1000;
  AE_4CP_Payment_storage_20[i]<- -ERCOT_4CP_storage_20[i] * access_fee_base + AE_4CP_storage_20[i]*sum_access_fee[i]*1000;
  AE_4CP_Payment_storage_25[i]<- -ERCOT_4CP_storage_25[i] * access_fee_base + AE_4CP_storage_25[i]*sum_access_fee[i]*1000;
  AE_4CP_Payment_storage_30[i]<- -ERCOT_4CP_storage_30[i] * access_fee_base + AE_4CP_storage_30[i]*sum_access_fee[i]*1000;
  AE_4CP_Payment_base[i]<- -ERCOT_4CP[i] * access_fee_base + AE_4CP_base[i]*sum_access_fee[i]*1000;
}
#Diff in Austin Energy's 4CP Payments from the base case 
for (i in 1:10) {
  percent_payment_10[i]<-AE_4CP_Payment_base[i]-AE_4CP_Payment_storage_10[i];
  percent_payment_15[i]<-AE_4CP_Payment_base[i]-AE_4CP_Payment_storage_15[i];
  percent_payment_20[i]<-AE_4CP_Payment_base[i]-AE_4CP_Payment_storage_20[i];
  percent_payment_25[i]<-AE_4CP_Payment_base[i]-AE_4CP_Payment_storage_25[i];
  percent_payment_30[i]<-AE_4CP_Payment_base[i]-AE_4CP_Payment_storage_30[i];
}
#===========================================================================================
#Calculating 4CP Payment (MW) - Access fee/Transmission Rate Sensitivity (this does not have an effect on 4CP load)
#===========================================================================================
#Initializing matrices
AE_4CP_Payment_access_1 <- matrix(0, nrow=1, ncol=10)
AE_4CP_Payment_access_2 <- matrix(0, nrow=1, ncol=10)

percent_payment_access_1 <- matrix(0, nrow=1, ncol=10)
percent_payment_access_2 <- matrix(0, nrow=1, ncol=10)
year_TSP <- c(seq(2008,2028))
TSP_new_2 <- c(11:21)*3.14+10.43 #linear regression on historical rates to project future rates - 90% confidence interval fit lower bound
TSP_new_3 <- c(11:21)*4.38+18.19 #linear regression on historical rates to project future rates- 90% confidence interval fit upper bound
sum_access_fee_1<- matrix(c(TSP_new_2[2:11]),nrow=1, ncol=10) #$/kW
sum_access_fee_2<- matrix(c(TSP_new_3[2:11]),nrow=1, ncol=10) #$/kW
for (i in 1:10) { #for each of 10 years
  AE_4CP_Payment_access_1[i]= -ERCOT_4CP_solar_80[i] * access_fee_base + AE_4CP_base[i]*(sum_access_fee_1[i])*1000;
  AE_4CP_Payment_access_2[i]= -ERCOT_4CP_solar_80[i] * access_fee_base + AE_4CP_base[i]*(sum_access_fee_2[i])*1000;
}


