#Part A
#TASK 1 - Understand the data
#Reading the data
the.data <- as.matrix(read.table("ENB18data.txt"))

#Taking sample data from the original data
my.data <- the.data[sample(1:768,300),c(1:5,6)]

# Histogram and Scatter Plots
par(mfrow=c(1,2))
hist(my.data[,1],xlab = "Relative Compactness",main = "Histogram for Relative Compactness" , col=rainbow(3))
plot(my.data[,1],my.data[,6],xlab = "Relative Compactness",ylab = "Heating Load",main = "Scatter Plot for Relative Compact vs Heating Load" , col=rainbow(3))
hist(my.data[,2],xlab = "Surface Area",main = "Histogram for Surface Area",col= rainbow(2))
plot(my.data[,2],my.data[,6],xlab = "Surface Area",ylab = "Heating Load", main = "Scatter Plot for Surface Area vs Heating Load ",col=rainbow(2))
hist(my.data[,3],xlab = "Wall Area",main = "Histogram for Wall Area", col = rainbow(4))
plot(my.data[,3],my.data[,6],xlab = "Wall Area",ylab = "Heating Load",main = "Scatter Plot for Wall Area vs Heating Load " , col= rainbow(4))
hist(my.data[,4],xlab = "Roof Area",main = "Histogram for Roof Area",col = rainbow(2))
plot(my.data[,4],my.data[,6],xlab = "Roof Area",ylab = "Heating Load",main = "Scatter Plot for Roof Area vs Heating Load ",col=rainbow(2))
hist(my.data[,5],xlab = "Overall Height",main = "Histogram for Overall Height",col=rainbow(5))
plot(my.data[,5],my.data[,6],xlab = "Overall Height",ylab = "Heating Load",main = "Scatter Plot for Overall Height vs Heating Load ",col=rainbow(5))
hist(my.data[,6],xlab = "Heating Load",main = "Histogram for Heating load",col=rainbow(1))

#Task 2 - Transform the data
V <- my.data
your.data <- array(0,c(300,5))  

#Feature Scaling
#Transformations for X1
V[,1] <- V[,1]^0.2 
V[,1] <-  (V[,1]-min(V[,1])) / (max(V[,1])- min(V[,1]))
your.data[,1] <- (1 - V[,1])

#Transformations for X2
your.data[,2]<- ((V[,2] - mean(V[,2])) / sd(V[,2]))*0.15 + 0.5  

#Transformations for X3
your.data[,3] <-  ((V[,3] - mean(V[,3])) / sd(V[,3]))*0.15 + 0.5

#Transformations for X4
V[,4] <- V[,4]^-1
your.data[,4] <-  (V[,4]-min(V[,4])) / (max(V[,4])- min(V[,4]))

#Transformations for Y1
V[,6] <- V[,6]^0.22
your.data[,5] <-  ((V[,6]-mean(V[,6])) / sd(V[,6]))*0.15 + 0.5


your.data
write.table(your.data,"manidhar-transformed.txt")

#Task 3 - Building Models
source("AggWaFit718.R")


fit.QAM(your.data,"QAM_Output.txt","QAM_Stats.txt")

#Weighted power Means
fit.QAM(your.data,g= PM05,g.inv = invPM05, "Power_Mean1_Output1.txt","Power_Mean1_Stats1.txt")

fit.QAM(your.data,g=QM, g.inv = invQM,"Power_Mean2_output2.txt","Power_Mean2_Stats2.txt")

#ordered weight
fit.OWA(your.data,"OWA_Output","OWA_Stats.txt")

fit.choquet(your.data,"Choquet_Output.txt","Choquet_Stats.txt")

V <- my.data
#Transformations for input variable X1
a <-  (0.82-min(V[,1])) / (max(V[,1])- min(V[,1]))
a <- (1 - a)
a

#Transformations for input variable X2
b<- ((612.5- mean(V[,2])) / sd(V[,2]))*0.15 + 0.5  

#Transformations for input variable X3
c <-  ((318.5 - mean(V[,3])) / sd(V[,3]))*0.15 + 0.5

#Transformations for input variable X4
V[,4] <- V[,4]^-1
d <-  (1/147-min(V[,4])) / (max(V[,4])- min(V[,4]))

#Predicting the Y1 value dfor the given inputs
predict = choquet(c(a,b,c,d),c(0,0.597779681545826,0.597779681545826,0.597779681545816,
                               0.597779681545816,0.597779681545824,0.597779681545824,0.49656172462836,0.49656172462836,
                               1.00000000000003,1.00000000000003,0.697619312682649,0.697619312682649,
                               1.00000000000001,1.00000000000001))

V <- my.data
predict
V[,6] <- V[,6]^(0.22)
predict_1 <-  (((predict -0.5) /0.15)* sd(V[,6]) + mean(V[,6]) ) ^(1/0.22)
  
predict_1

