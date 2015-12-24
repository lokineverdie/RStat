myStat <- function (file, exp, groups, N){  #myStat('test2.csv', 3,5,2)
     
     #check if the arguments are empty
     
     checkArgs <- c(missing(file), missing(exp), missing(groups), missing(N))
     
     if(any(checkArgs)){
          stop('Empty args! file_name.csv, number of experiments,  number of groups, N')
     }
     
     #clean data if NA is exist
     
     input <- read.csv(file, header = F, sep = ';')
     data <- data.frame(input)
     
     # input data is the life and dead cell count
     # lets calculate a viability as life cell count/total cell count
     
     m <- matrix(ncol=groups, nrow = nrow(data)); a<-1
     
     for (i in seq(1,ncol(data),N)) {
          x <-(data[,i]/(data[,i]+data[,i+1])*100)
          m[,a]<-x
          a<-a+1
     }
     
     
     # lets split data and calculate mean and standart deviation
     
     for(i in 1:exp) {
          vect<-c(m[,i])
          factor<-gl(groups,exp*N)
          mn<- tapply(vect, factor, mean)
          sd<- tapply(vect, factor, sd)
          print(paste0((round(mn,2)), '±', (round(sd,2))))
     }
     
     
}

