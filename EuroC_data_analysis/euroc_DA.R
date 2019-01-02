#=========================================================
#clear the screen
#=========================================================
cat("\014")
rm(list=ls())
options(digits=15)

dev.off()

# Nonlinear Least Square:
xvalues <- c(1.6,2.1,2,2.23,3.71,3.25,3.4,3.86,1.19,2.21)
yvalues <- c(5.19,7.43,6.94,8.11,18.75,14.88,16.06,19.12,3.21,7.58)

# Give the chart file a name.
#png(file = "nls.png")
pdf(file = "nls.pdf", paper = "a4r")

# Plot these values.
plot(xvalues,yvalues)


# Take the assumed values and fit into the model.
model <- nls(yvalues ~ b1*xvalues^2+b2,start = list(b1 = 1,b2 = 3))

# Plot the chart with new data by fitting it to a prediction from 100 data points.
new.data <- data.frame(xvalues = seq(min(xvalues),max(xvalues),len = 100))
lines(new.data$xvalues,predict(model,newdata = new.data))

# Save the file.
dev.off()

# Get the sum of the squared residuals.
print(sum(resid(model)^2))

# Get the confidence intervals on the chosen values of the coefficients.
print(confint(model))

#=========================================================
#FIRST OPERATIONS
#=========================================================

# ----------------- IMU data --------------------
euroc_imu <- read.csv(file="/home/farid/datasets/EuRoC/mav0/imu0/data.csv",head=TRUE,sep=",")

timeSt <- euroc_imu[[1]]

omegaX <- euroc_imu[[2]]
omegaY <- euroc_imu[[3]]
omegaZ <- euroc_imu[[4]]

accX <- euroc_imu[[5]]
accY <- euroc_imu[[6]]
accZ <- euroc_imu[[7]]

#pdf(file = "IMU_data_analysis.pdf",  paper = "a4r")

# motion tracking with IMU acceleration data:

attach(mtcars)
par(mfrow=c(3,2))

plot(timeSt,accX, main = "acceleration X")
hist(accX, breaks = 1000,main = "Histogram accX")

plot(timeSt,accY, main = "acceleration Y")
hist(accY, breaks = 1000,main = "Histogram accY")

plot(timeSt,accZ, main = "acceleration Z")
hist(accZ, breaks = 1000,main = "Histogram accZ")

dev.off()

# ------------------- Leica Nova Position Measurement M550----------------------

euroc_leica <- read.csv(file="/home/farid/datasets/EuRoC/mav0/leica0/data.csv",head=TRUE,sep=",")
ts_leica <- euroc_leica[[1]]

px_leica <- euroc_leica[[2]]
py_leica <- euroc_leica[[3]]
pz_leica <- euroc_leica[[4]]

pdf(file = "Leica_data_analysis.pdf")

attach(mtcars)
par(mfrow=c(3,1))

plot(ts_leica, px_leica, main = "poseX")
plot(ts_leica, py_leica, main = "poseY")
plot(ts_leica, pz_leica, main = "poseZ")

dev.off()


# ------------------- Ground-Truth:----------------------
euroc_GT <- read.csv(file="/home/farid/datasets/EuRoC/mav0/state_groundtruth_estimate0/data.csv",head=TRUE,sep=",")

ts_imu <- euroc_GT[[1]]

# position
px <- euroc_GT[[2]]
py <- euroc_GT[[3]]
pz <- euroc_GT[[4]]


# quaternion
qw <- euroc_GT[[5]]
qx <- euroc_GT[[6]]
qy <- euroc_GT[[7]]
qz <- euroc_GT[[8]]

# velocity
vx <- euroc_GT[[9]]
vy <- euroc_GT[[10]]
vz <- euroc_GT[[11]]

# bias
bwx <- euroc_GT[[12]]
bwy <- euroc_GT[[13]]
bwz <- euroc_GT[[14]]


bax <- euroc_GT[[15]]
bay <- euroc_GT[[16]]
baz <- euroc_GT[[17]]

#pdf(file = "GT_data_analysis.pdf",  paper = "a4r")

attach(mtcars)
par(mfrow=c(3,3))

plot(ts_imu, px, main = "poseX")
plot(ts_imu, vx, main = "velX")
plot(ts_imu, qx, main = "quatX")

plot(ts_imu, py, main = "poseY")
plot(ts_imu, vy, main = "velY")
plot(ts_imu, qy, main = "quatY")

plot(ts_imu, pz, main = "poseZ")
plot(ts_imu, vz, main = "velZ")
plot(ts_imu, qz, main = "quatZ")

dev.off()

# boxplot
# Translational error [m]
boxplot(px, main = "x translational error" )
# Rotational error [deg]