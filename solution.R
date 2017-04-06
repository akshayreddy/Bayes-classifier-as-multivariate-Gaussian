
X<-read.table("glass.data",sep=",")
d<-X[(X$V11==1 | X$V11==2 | X$V11==7),] #Considering class 1,2 and 7

# Spliting data into training and testing data equally for each class

training_data=d[d$V11==100,]
testing_data=d[d$V11==100,]

for(i in 0:2){
   if(i==0){
     t=1
   }else if(i==1){
     t=2
   }else if(i==2){
     t=7
   } 
  train=sample(1:nrow(d[d$V11==t,]),nrow(d[d$V11==t,])/2)  
  test= -train
  training_data=rbind(training_data,d[d$V11==t,][train,])
  testing_data=rbind(testing_data,d[d$V11==t,][test,])
}


#a.)
#modeling the class-conditional distributions as multivariate Gaussian on the predictive variables.
#First calulating the mean, Variance and Standar deviation of all the predictive variable with
#respect to each of the 3 classes


p_1_MEAN=vector(mode="integer",length = 10)
p_1_VARIANCE=vector(mode="integer",length = 10)
p_1_SD=vector(mode="integer",length = 10)

p_2_MEAN=vector(mode="integer",length = 10)
p_2_VARIANCE=vector(mode="integer",length = 10)
p_2_SD=vector(mode="integer",length = 10)

p_7_MEAN=vector(mode="integer",length = 10)
p_7_VARIANCE=vector(mode="integer",length = 10)
p_7_SD=vector(mode="integer",length = 10)

index=c(1,2,7)

#Using the data in the formula
for(i in 1:3){
  temp_data=training_data[training_data$V11==index[i],]
  r=nrow(temp_data)
  for(j in 1:10){
    if(i==1){
      p_1_MEAN[j]=mean(temp_data[,j])
      p_1_VARIANCE[j]=var(temp_data[,j])
      p_1_SD[j]=sd(temp_data[,j])                         #Formula 
    }else if(i==2){
      p_2_MEAN[j]=mean(temp_data[,j])
      p_2_VARIANCE[j]=var(temp_data[,j])
      p_2_SD[j]=sd(temp_data[,j])
    }else{
      p_7_MEAN[j]=mean(temp_data[,j])
      p_7_VARIANCE[j]=var(temp_data[,j])
      p_7_SD[j]=sd(temp_data[,j])
    }
  }
}

#b.) Testing the classifier on the testing set and computing the error rate. 

result=vector(mode="integer",length = nrow(testing_data))
Constant=sqrt(2*3.14)

for(i in 1:nrow(testing_data)){
  p1=p2=p3=1
  temp_data=testing_data[i,1:10]
  
  for(j in 1:3){
    
    if(j==1){
      for(z in 1:10){
        POW=((temp_data[z]-p_1_MEAN[z])^2)/(2*p_1_VARIANCE[z])
        p1=p1*(1/Constant*p_1_SD[z])*exp(-POW)                          
      }
      
    }else if(j==2){
      for(z in 1:10){
        POW=((temp_data[z]-p_2_MEAN[z])^2)/(2*p_2_VARIANCE[z])
        p2=p2*(1/Constant*p_2_SD[z])*exp(-POW)
      }
      
    }else{
      for(z in 1:10){
        POW=((temp_data[z]-p_7_MEAN[z])^2)/(2*p_7_VARIANCE[z])
        p3=p3*(1/Constant*p_7_SD[z])*exp(-POW)
      }
    }
  }
  
  if(p1>p2 & p1>p3){
    result[i]=1
  }
  if(p2>p1 & p2>p3){
    result[i]=2
  }
  if(p3>p1 & p3>p2){
    result[i]=7
  }
}

table(result, testing_data$V11,dnn=c("Prediction","Actual"))

#Calculating the error rate
count=0
r=nrow(testing_data)
for(i in 1:r){
  if(result[i]==testing_data$V11[i])
    count=count+1
}

print(100-(count/r)*100)



#c.) Testing the classifier on the training set and computing the error rate. 

result=vector(mode="integer",length = nrow(training_data))
Constant=sqrt(2*3.14)

for(i in 1:nrow(training_data)){
  p1=p2=p3=1
  temp_data=training_data[i,1:10]
  
  for(j in 1:3){
    
    if(j==1){
      for(z in 1:10){
        POW=((temp_data[z]-p_1_MEAN[z])^2)/(2*p_1_VARIANCE[z])
        p1=p1*(1/Constant*p_1_SD[z])*exp(-POW)
      }
      
    }else if(j==2){
      for(z in 1:10){
        POW=((temp_data[z]-p_2_MEAN[z])^2)/(2*p_2_VARIANCE[z])
        p2=p2*(1/Constant*p_2_SD[z])*exp(-POW)
      }
      
    }else{
      for(z in 1:10){
        POW=((temp_data[z]-p_7_MEAN[z])^2)/(2*p_7_VARIANCE[z])
        p3=p3*(1/Constant*p_7_SD[z])*exp(-POW)
      }
    }
  }
  
  if(p1>p2 & p1>p3){
    result[i]=1
  }
  if(p2>p1 & p2>p3){
    result[i]=2
  }
  if(p3>p1 & p3>p2){
    result[i]=7
  }
}

table(result, training_data$V11,dnn=c("Prediction","Actual"))

#Calculating the error rate

count=0
r=nrow(training_data)
for(i in 1:r){
  if(result[i]==training_data$V11[i])
    count=count+1
}

print(100-(count/r)*100)




