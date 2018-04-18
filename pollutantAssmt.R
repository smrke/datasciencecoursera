pollutantmean <- function(directory = "/Users/briannasmrke/datasciencecoursera/specdata", pollutant, id = 1:332) {
        
        currentdir <- setwd(directory)
        
        means <- c(0)
        
        nOB <- c(0)
        
        ix <- 1
        
        for (item in id) {
                
                if (item >= 100){
                        
                        filepath <- paste(currentdir, "/", item, ".csv", sep = "")
                        print(filepath) #just so we can see what's going on
                        data <- read.csv(filepath) #create data frame
                        polldata <- data[pollutant] #select out the pollutant
                        polldataOB <- polldata[!is.na(polldata)] #select the non-NA values
                        meanOB <- mean(polldataOB) #compute the mean
                        means[ix] <- meanOB #add the mean to our vector
                        nOB[ix] <- length(polldataOB) #add the number of OBs
                        ix <- ix + 1 #advance the index counter
                        
                }
                
                else if (item >= 10) {
                        
                        filepath <- paste(currentdir, "/", "0", item, ".csv", sep = "")
                        print(filepath) #just so we can see what's going on
                        data <- read.csv(filepath) #create data frame
                        polldata <- data[pollutant] #select out the pollutant
                        polldataOB <- polldata[!is.na(polldata)] #select the non-NA values
                        meanOB <- mean(polldataOB) #compute the mean
                        means[ix] <- meanOB #add the mean to our vector
                        nOB[ix] <- length(polldataOB)
                        ix <- ix + 1 #advance the index counter
                        
                        
                       
                }
                
                else {
                        
                        filepath <- paste(currentdir, "/", "00", item, ".csv", sep = "")
                        print(filepath) #just so we can see what's going on
                        data <- read.csv(filepath) #create data frame
                        polldata <- data[pollutant] #select out the pollutant
                        polldataOB <- polldata[!is.na(polldata)] #select the non-NA values
                        meanOB <- mean(polldataOB) #compute the mean
                        means[ix] <- meanOB #add the mean to our vector
                        nOB[ix] <- length(polldataOB)
                        ix <- ix + 1 #advance the index counter
                        
          
                        
                }
        
        }
        
       weightedOBS <- (sum(means * nOB))/(sum(nOB, na.rm = TRUE)) #weighting the means - multiply each by number of Obs

print(length(polldataOB))
       weightedOBS
}
