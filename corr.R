corr <- function(directory = "specdata", threshold = 0) {
        
        id <- 1:332
        
        directory <- paste("/Users/briannasmrke/datasciencecoursera/", directory, sep = "")
        cdir <- setwd(directory)
        
        corrV <- numeric()
        
        
        ix <- 1
        
        for (item in id) {
                
                
                if (item >= 100){
                        
                        filepath <- paste(cdir, "/", item, ".csv", sep = "")
                        #print(filepath) #just so we can see what's going on
                        data <- read.csv(filepath) #create data frame
                        if (sum(complete.cases(data)) >= threshold) {
                                corry <- cor(data["sulfate"], data["nitrate"], use = "na.or.complete") #calculate correlation
                                corrV[ix] <- corry #add to corrV
                                ix <- ix + 1 #advance the index counter
                        }
                        
                }
                
                else if (item >= 10) {
                        
                        filepath <- paste(cdir, "/", "0", item, ".csv", sep = "")
                        #print(filepath) #just so we can see what's going on
                        data <- read.csv(filepath) #create data frame
                        if (sum(complete.cases(data)) >= threshold) {
                                corry <- cor(data["sulfate"], data["nitrate"], use = "na.or.complete") #calculate correlation
                                corrV[ix] <- corry #add to corrV
                                ix <- ix + 1 #advance the index counter
                        }
                        
                }
                
                else {
                        
                        filepath <- paste(cdir, "/", "00", item, ".csv", sep = "")
                        #print(filepath) #just so we can see what's going on
                        data <- read.csv(filepath) #create data frame
                        if (sum(complete.cases(data)) >= threshold) {
                                corry <- cor(data["sulfate"], data["nitrate"], use = "na.or.complete") #calculate correlation
                                corrV[ix] <- corry #add to corrV
                                ix <- ix + 1 #advance the index counter
                        }
                        
                        
                }
        } 
        
        corrV
}