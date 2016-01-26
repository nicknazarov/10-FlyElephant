
#!/usr/bin/env Rscript
#Вычисляем лучшую стратегию для пятничных цен закрытия для разных стран


###########################################################################################
P_R <- function (R,T,q){
  
  t=R
  Tetta <- c()
  Tetta[t-R+1] <- sample(R:T, 1)
  
  while(1){
    t <- t+1
    if(t>T) break
    U <- runif(1, 0, 1)
    if(U<q) {
      Tetta[t-R+1] <- sample(R:T, 1)
    }
    
    if(U>=q) {
      Tetta[t-R+1] <- Tetta[t-R]+1  
      
      if(Tetta[t-R+1]>T) {
        Tetta[t-R+1] <- R
      } 
    }
  } 
  
  Tetta   
  
}



###########################################################################################

ret.winner <- function (p1, p2, p3, STEP, N, d2, UP1, UP2, percent)
{
  
  
  # набор дельт  - ans
  ans <- c() 
  i<-UP1*4+1+UP2
  m <- 1 
  while(i < N){
    
    d<- d2[,d2[i-4*p1-p2,]!=0 & d2[i,]!=0 & d2[i-p2,]!=0]
    
    temp1 <- (as.numeric(d[i-p2,]) - as.numeric(d[i-4*p1-p2,]))/as.numeric(d[i-4*p1-p2,])
    # temp1 <- (t (d)[,i-p2] - t (d) [,i-4*p1-p2 ])/t (d)[,i-4*p1-p2]
    
    temp2 <- d[,order(-temp1)]
    
    
    #######################################################################################################   
    # Для случая нахождения нулей в середине столбца с ценами закрытия - меняем ноль на последний ненулевой   
    
    for(k in 1:length(temp2)){
      if(temp2[i+p3*4, k]==0){
        
        count<-1
        while(temp2[i+p3*4-count,k]==0){
          count<-count+1
        }
        temp2[i+p3*4, k] <- temp2[i+p3*4-count, k] 
        
      }
    }
    #######################################################################################################  
    
    
    temp3 <- (as.numeric(temp2[i+p3*4,])- as.numeric(temp2[i,]))/as.numeric(temp2[i,])
    
    if(percent==0.5){
      ans[m] <- (sum(temp3[1:floor(length(temp3)*percent)] )/ floor(length(temp3)*percent))/p3
    }
    else{
      ans[m] <- (sum(temp3[1:ceiling(length(temp3)*percent)])/ceiling(length(temp3)*percent))/p3
      
      #print("########################")
      #print(length(temp3) - ceiling(length(temp3)*(1-percent))+1  )
      #print(ceiling(length(temp3)*percent))   
      #print("########################")
      
    }
    
    names(ans)[m] <- row.names(d)[i]    
    
    m <- m+1
    i<-STEP+i  
  }   
  return(ans)
}


###########################################################################################

ret.loser <- function (p1, p2, p3, STEP, N, d2, UP1, UP2, percent)
{
  
  # набор дельт  - ans
  ans <- c() 
  i<-UP1*4+1+UP2
  m <- 1 
  while(i < N){
    
    d<- d2[,d2[i-4*p1-p2,]!=0 & d2[i,]!=0 & d2[i-p2,]!=0]
    
    temp1 <- (as.numeric(d[i-p2,]) - as.numeric(d[i-4*p1-p2,]))/as.numeric(d[i-4*p1-p2,])
    #temp1 <- (t (d)[,i-p2] - t (d) [,i-4*p1-p2 ])/t (d)[,i-4*p1-p2]
    
    temp2 <- d[,order(-temp1)]
    
    #######################################################################################################   
    # Для случая нахождения нулей в середине столбца с ценами закрытия - меняем ноль на последний ненулевой   
    
    for(k in 1:length(temp2)){
      if(temp2[i+p3*4, k]==0){
        
        count<-1
        while(temp2[i+p3*4-count,k]==0){
          count<-count+1
        }
        temp2[i+p3*4, k] <- temp2[i+p3*4-count, k] 
        
      }
    }
    #######################################################################################################  
    
    
    
    temp3 <- (as.numeric(temp2[i+p3*4,])- as.numeric(temp2[i,]))/as.numeric(temp2[i,])
    
    if(percent==0.5){
      ans[m] <- (sum(temp3[(floor(length(temp3)*percent)+1):(length(temp3))]) /(length(temp3)-floor(length(temp3)*percent)))/p3
    }
    else{
      ans[m] <- (sum(temp3[ceiling(length(temp3)*(1-percent)):(length(temp3))]) /ceiling(length(temp3)*percent))/p3
      
      #print("########################")
      #print(length(temp3) - ceiling(length(temp3)*(1-percent))+1  )
      #print(ceiling(length(temp3)*percent))   
      #print("########################")
      
    }
    
    names(ans)[m] <- row.names(d)[i]    
    
    m <- m+1
    i<-STEP+i  
  }   
  return(ans)
}

ret.mmvb <- function (p1, p2, p3, STEP, N, d, UP1, UP2)
{
  
  # набор дельт  - ans
  ans <- c() 
  i<-UP1*4+1+UP2
  m <- 1 
  while(i < N){
    ans[m]  <- (as.numeric(d[i+p3*4])- as.numeric(d[i]))/as.numeric(d[i])        
    names(ans)[m] <- row.names(d)[i]    
    m <- m+1
    i<-STEP+i  
  }   
  return(ans)
}



######################################################################################################

ret <- function (p1, p2, p3, STEP, N, d2, UP1, UP2, percent)
{
  
  # набор дельт  - ans
  ans <- c() 
  i<-UP1*4+1+UP2
  m <- 1 
  while(i < N){
    
    d<- d2[,d2[i-4*p1-p2,]!=0 & d2[i,]!=0 & d2[i-p2,]!=0]
    
    temp1 <- (as.numeric(d[i-p2,]) - as.numeric(d[i-4*p1-p2,]))/as.numeric(d[i-4*p1-p2,])
    #temp1 <- ( t(d)[,i-p2] - t(d)[,i-4*p1-p2 ]) / t(d)[,i-4*p1-p2]
    
    temp2 <- d[,order(-temp1)]
    
    #######################################################################################################   
    # Для случая нахождения нулей в середине столбца с ценами закрытия - меняем ноль на последний ненулевой   
    
    for(k in 1:ncol(temp2)){
      if(temp2[i+p3*4, k]==0){
        
        count<-1
        while(temp2[i+p3*4-count,k]==0){
          count<-count+1
        }
        temp2[i+p3*4, k] <- temp2[i+p3*4-count, k] 
        
      }
    }
    #######################################################################################################  
    
    
    temp3 <- (as.numeric(temp2[i+p3*4,])- as.numeric(temp2[i,]))/as.numeric(temp2[i,])
    
    if(percent==0.5){
      ans[m] <- (sum(temp3[1:floor(length(temp3)*percent)] )/ floor(length(temp3)*percent)- 
                   sum(temp3[(floor(length(temp3)*percent)+1):(length(temp3))]) /(length(temp3)-floor(length(temp3)*percent)))/p3
    }
    else{
      ans[m] <- (sum(temp3[1:ceiling(length(temp3)*percent)])/ceiling(length(temp3)*percent)- 
                   sum(temp3[ceiling(length(temp3)*(1-percent)):(length(temp3))]) /ceiling(length(temp3)*percent))/p3
      
      #print("########################")
      #print(length(temp3) - ceiling(length(temp3)*(1-percent))+1  )
      #print(ceiling(length(temp3)*percent))   
      #print("########################")
      
    }
    
    names(ans)[m] <- row.names(d)[i]    
    
    m <- m+1
    i<-STEP+i  
  }   
  return(ans)
}

ret.companies <- function (p1, p2, p3, STEP, N, d, UP1, UP2, percent)
{
  
  companies <- data.frame ( Ticket=rep(0,ncol(d)), In_winners=rep(0,ncol(d)),  In_losers= rep(0,ncol(d)))
  
  companies[,1] <- colnames(d)
  companies[,2] <- rep(0,ncol(d))
  companies[,3] <- rep(0,ncol(d)) 
  
  i<-UP1*4+1+UP2
  m <- 1 
  while(i < N){
    
    temp1 <- (as.numeric(d[i-p2,]) - as.numeric(d[i-4*p1-p2,]))/as.numeric(d[i-4*p1-p2,])
    comp.order <- 1:ncol(d)
    # comp.order <- comp.order[]
    comp.winners <- rep(0,ncol(d))
    comp.losers <- rep(0,ncol(d))
    
    #temp2 <- d[,order(-temp1)]
    
    #d<- d2[,d2[i-4*p1-p2,]!=0]
    temp1 <- temp1 [d[i-4*p1-p2,]!=0]
    comp.order <- comp.order [d[i-4*p1-p2,]!=0]  
    
    comp.order <- comp.order[order(-temp1)]
    
    if(percent==0.5){   
      comp.winners[comp.order[1:floor(length(temp1)*percent)]] <- 1
      comp.losers[comp.order[(floor(length(temp1)*percent)+1):(length(temp1))]]<- 1              
    }
    else{ 
      comp.winners[comp.order[1:ceiling(length(temp1)*percent)]]<- 1
      comp.losers [comp.order[ceiling(length(temp1)*(1-percent)):(length(temp1))]] <- 1 
    }
    
    companies[,2] <- companies[,2] + comp.winners
    companies[,3] <- companies[,3] + comp.losers
    m <- m+1
    i<-STEP+i  
  }   
  return(companies)
}

if (!require("parallel")) install.packages("parallel")
#library(parallel)
#setwd("/home/nazarov/02-fmlab.hse.ru/05 - reverse/")
#source("R/reality_func2.R")
#source("~/workdir/reality_func2.R")
#############################################################################
# Параметры, которые зависят от изучаемой страны
country_name_eng <- "china"
#china
T <- 520


#############################################################################
# Загрузка 
#############################################################################
price_d5<- read.csv(file="~/workdir/china_price_Fri.csv", header=TRUE)
#price_d5<- read.csv(file="/home/nazarov/02-fmlab.hse.ru/05 - reverse/data/5 days china/china_price_Fri.csv", header=TRUE)
row.names(price_d5) <- price_d5[,1]
price_d5 <-price_d5[,-1]
#############################################################################
# Константы
# перебор от 1 до 12 мес , ждем от 0 до 8 недель, держим от 1 до 24 мес
# таким образом  последние 25 месяцев понадобятся только для одной модели
# Шаг для подсчета разниц в группах победителей и проигравших
STEP=1
# Периоды отбора (в месяцах), удержания (в неделях), инвестирования (в месяцах)
UP1=12
UP2=8
UP3=24
# N - с учетом отступа
#n <- T-R+1
#############################################################################
# Процедура формирования портфелей, подсчета статистик и bootstrap
start_time <- Sys.time()
start_time
resultDataFull <- price_d5
N <- (nrow(resultDataFull)-(2+UP3*4))%/%STEP 

print("Параллельное выполнение")
cl <- makeCluster(getOption("cl.cores", 8)) # создание кластера из четырёх ядер процессора
clusterExport(cl,"infert") # передача данных внутрь кластера
clusterEvalQ(cl,source("~/workdir/reality_func2.R")) # загрузка функций в кластер
start_time <- Sys.time()
temp1 <- parLapply(cl,  1:8, function(temp_p3, UP1, UP2, UP3, STEP, resultDataFull, N) # параллельная версия sapply
{    m <- 1  
     realityCheckData <- data.frame(1,1,1,1,1,1,1,1,1,1)
     low <- (temp_p3-1)*3+1
     up <- temp_p3*3
     for (p3 in low:up) {  
       for (percent in c(0.5,0.3,0.2,0.1) ){
         for (p1 in 1:UP1 ){   
           for (p2 in 0:UP2 ){  
           #вектор дельт    
           temp <- ret(p1, p2, p3, STEP, N, resultDataFull, UP1, UP2, percent) 
           return.winner<- ret.winner(p1, p2, p3, STEP, N, resultDataFull, UP1, UP2, percent) 
           return.loser<- ret.loser(p1, p2, p3, STEP, N, resultDataFull, UP1, UP2, percent) 
           n <- length(temp)
           realityCheckData[m, ] <- list(mean(temp),abs(mean(temp))/sd(temp)*sqrt(n), (1-pt(q = abs(mean(temp))/sd(temp)*sqrt(n),df = n-1))*2 ,p1*4, p2, p3*4, percent,
                                         mean(return.winner), mean(return.loser),length(temp[temp<0]))    
           m <- m+1      
          }
         }
       }       
     }
     return (realityCheckData)
     
}, UP1, UP2, UP3, STEP, resultDataFull, N)
stopCluster(cl)

end_time <- Sys.time()
end_time

temp2 <-do.call("rbind", temp1)
colnames(temp2) <-c("mean","t","p-value","hist_per","moment_per","invest_per","percent","winners","losers", "Amount_of_negative")



#Сохранение результатов
results <- list(data=temp2, num=N)  # список ценных объектов
saveRDS(file = "~/workdir/china_best.RDS",results) # сохраняем всё ценное в файл







