library(dlookr)
library(dplyr)

data1=read.csv("listens.csv")
colnames(data1) <- c("l")
data1$lNum <-  as.numeric(data1$l)
data1$lNumTransformed <- data1$lNum / 10000
data1 <- na.omit(data1)
datax <- data1
bin <- binning(datax$lNumTransformed, nbins = 10)
plot(bin)
hist(datax$lNumTransformed, breaks = c(-10,0,10,20,30,40,50,60,70 ))
#head(data1)

data <- data1[which( data1$lNumTransformed < 60),3]
mysample$z <-  sample(data,10)
View(mysample)
#median(data)
#data

mysample <- as.data.frame(c(1,2,3,4,5))
colnames(mysample) <- c('val')

data1$lNum
data1 = as.data.frame(as.numeric(data1$lNumTransformed))
data1 %>% 
 # arrange(desc(`data1$lNumTransformed`))  
arrange(desc(data1$lNum) ) %>%
  slice(1:100)
#%>%
  #top_n(10,`data1$lNumTransformed`)
      #arrange(val,desc) %>% 
      # filter(`data1$lNumTransformed` > 0)  %>% 
      # arrange( data1$lNumTransformed, desc)
  
      # slice(1:5000)  %>%
      # top_n(50)


  #top_n(lNumTransformed, 5) 

bin <- binning(data, nbins = 5)
plot(bin)
hist(data)

carseats <- ISLR::Carseats
carseats[sample(seq(NROW(carseats)), 20), "Income"] <- NA
carseats[sample(seq(NROW(carseats)), 5), "Urban"] <- NA
dataI <- carseats$Income
# Binning the carat variable. default type argument is "quantile"
bin <- binning(dataI, nbins = 5)
plot(bin)
