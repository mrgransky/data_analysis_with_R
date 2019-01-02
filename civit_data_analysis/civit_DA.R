#=========================================================
#clear the screen
#=========================================================
cat("\014")
rm(list=ls())
options(digits=15)
dev.off()

#=========================================================
#FIRST OPERATIONS
#=========================================================

civit_data <- read.csv(file="/home/xenial/Datasets/CIVIT/Nov_27/gps_imu_data/largeLoop.csv",head=TRUE,sep=",")
#civit_data <- read.csv(file="/home/farid/datasets/Jussi/civit/smlLoop.csv",head=TRUE,sep=",")

#Assign variables for large loop
gpsT <- civit_data[[1]]
corrT <- civit_data[[2]]

HzSpeed  <- civit_data[[3]]

latitude  <- civit_data[[4]]
longitude  <- civit_data[[5]]
H_ell <- civit_data[[6]]

heading <- civit_data[[7]]
pitch <- civit_data[[8]]
roll <- civit_data[[9]]

headingSD <- civit_data[[10]]
pitchSD <- civit_data[[11]]
rollSD <- civit_data[[12]]

SD_VE <- civit_data[[13]]
SD_VN <- civit_data[[14]]
SD_VH <- civit_data[[15]]

# long-lat 2 x y z:
R <- 6378137
f <- 1.0 / 298.257224
c <- 1 / sqrt(cos(latitude*pi/180) * cos(latitude*pi/180) + (1 - f) * (1 - f) * sin(latitude*pi/180) * sin(latitude*pi/180))
s <- (1.0 - f) * (1.0 - f) * c
h <- 0

x <- (R*c + h) *(cos(latitude*pi/180))*cos(longitude*pi/180)
y <- (R*c + h) *(cos(latitude*pi/180))*sin(longitude*pi/180)
z <- (R*c + h) *(sin(latitude*pi/180))

# 3d trajectory
install.packages("plot3D")
library("plot3D")
scatter3D(longitude,latitude,H_ell, pch = 20,  cex = 0.2, theta = 40, phi = 5,
          main = "3D trajectory", clab = c("Altitide", "[m]"),
          xlab = "Longitude", ylab ="Latitude", zlab = "Altitude", ticktype = "detailed")
dev.off()


#pdf(file = "orientation.pdf", paper = "a4r")
attach(mtcars)
par(mfrow=c(1,2))
boxplot(heading, main = "civit heading boxplot")
plot(gpsT, heading, main = "heading [deg]")
dev.off()

#pdf(file = "positioning.pdf", paper = "a4r")
attach(mtcars)
par(mfrow=c(3,1))
plot(gpsT,latitude, cex = 0.002, main = "civit_latitude")
plot(gpsT,longitude, cex = 0.002, main = "civit_longitude")
plot(gpsT,H_ell, cex = 0.002, main = "civit_altitude")

dev.off()
plot(x, y, cex = 0.002, main = "civit cartesian coordinate")


plot(longitude, latitude, col = "blue",cex = 0.002, main = "civit 2D trajectory")
points(longitude[1], latitude[1],  pch = 5, cex = 4, col = "red" )
points(longitude[length(longitude)], latitude[length(latitude)],  pch = 19, cex = 1.5, col = "green" )

legend("topright", legend=c("trajectory", "start", "end"), inset=.02, 
       col=c("blue", "red", "green"), lty=1:2, cex=.9)
dev.off()



# ------------------------ IMU data Analysis -----------------------
imu_large <- read.csv(file="/home/farid/datasets/Jussi/civit/imu_large.csv",head=TRUE,sep=",")
imu_small <- read.csv(file="/home/farid/datasets/Jussi/civit/imu_small.csv",head=TRUE,sep=",")

t_s_imu <- imu_large[[1]]

gyro_x <- imu_large[[2]]
gyro_y <- imu_large[[3]]
gyro_z <- imu_large[[4]]

accX <- imu_large[[5]]
accY <- imu_large[[6]]
accZ <- imu_large[[7]]



# motion tracking with IMU acceleration data:

#pdf(file = "acc_data_analysis.pdf", paper = "a4r")

attach(mtcars)
par(mfrow=c(3,2))
plot(t_s_imu,accX, cex = 0.002, main = "civit acc_X")
hist(accX, breaks = 1000,main = "Histogram accX")

plot(t_s_imu,accY, cex = 0.002, main = "civit acc_Y")
hist(accY, breaks = 1000,main = "Histogram accY")

plot(t_s_imu,accZ, cex = 0.002, main = "civit acc_Z")
hist(accZ, breaks = 1000,main = "Histogram accZ")

dev.off()