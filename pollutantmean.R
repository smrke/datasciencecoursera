pollutantmean <- function(directory = "specdata", pollutant, id = 1:332) {
        
        directory <- paste("/Users/briannasmrke/datasciencecoursera/", directory, sep = "")
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
                        if (length(polldataOB) > 0) {
                                meanOB <- mean(polldataOB) #compute the mean
                                means[ix] <- meanOB #add the mean to our vector
                                nOB[ix] <- length(polldataOB) #add the number of OBs
                                ix <- ix + 1 #advance the index counter
                        }
                        
                }
                
                else if (item >= 10) {
                        
                        filepath <- paste(currentdir, "/", "0", item, ".csv", sep = "")
                        print(filepath) #just so we can see what's going on
                        data <- read.csv(filepath) #create data frame
                        polldata <- data[pollutant] #select out the pollutant
                        polldataOB <- polldata[!is.na(polldata)] #select the non-NA values
                        if (length(polldataOB) > 0) {
                                meanOB <- mean(polldataOB) #compute the mean
                                means[ix] <- meanOB #add the mean to our vector
                                nOB[ix] <- length(polldataOB) #add the number of OBs
                                ix <- ix + 1 #advance the index counter
                        }
                        
                       
                }
                
                else {
                        
                        filepath <- paste(currentdir, "/", "00", item, ".csv", sep = "")
                        print(filepath) #just so we can see what's going on
                        data <- read.csv(filepath) #create data frame
                        polldata <- data[pollutant] #select out the pollutant
                        polldataOB <- polldata[!is.na(polldata)] #select the non-NA values
                        if (length(polldataOB) > 0) {
                                meanOB <- mean(polldataOB) #compute the mean
                                means[ix] <- meanOB #add the mean to our vector
                                nOB[ix] <- length(polldataOB) #add the number of OBs
                                ix <- ix + 1 #advance the index counter
                        }
          
                        
                }
        
        }
        

        weightedOBS <- (sum(means * nOB))/(sum(nOB, na.rm = TRUE)) #weighting the means - multiply each by number of Obs

print(length(polldataOB))
       weightedOBS
}

complete <- function(directory = "specdata", id = 1:332) {
        
        directory <- paste("/Users/briannasmrke/datasciencecoursera/", directory, sep = "")
        
        cdir <- setwd(directory)
        
        cOBS <- numeric()
        
        idN <- numeric()
        
        ix <- 1
        
        for (item in id) {
                
                
                if (item >= 100){
                        
                        filepath <- paste(cdir, "/", item, ".csv", sep = "")
                        #print(filepath) #just so we can see what's going on
                        data <- read.csv(filepath) #create data frame
                        nCC <- sum(complete.cases(data))
                        cOBS[ix] <- nCC #add the number of complete cases to our vector
                        idN[ix] <- item #add the id number
                        ix <- ix + 1 #advance the index counter
                        
                }
                
                else if (item >= 10) {
                        
                        filepath <- paste(cdir, "/", "0", item, ".csv", sep = "")
                        #print(filepath) #just so we can see what's going on
                        data <- read.csv(filepath) #create data frame
                        nCC <- sum(complete.cases(data))
                        cOBS[ix] <- nCC #add the number of complete cases to our vector
                        idN[ix] <- item #add the id number
                        ix <- ix + 1 #advance the index counter
                        
                        
                        
                }
                
                else {
                        
                        filepath <- paste(cdir, "/", "00", item, ".csv", sep = "")
                        #print(filepath) #just so we can see what's going on
                        data <- read.csv(filepath) #create data frame
                        nCC <- sum(complete.cases(data))
                        cOBS[ix] <- nCC #add the number of complete cases to our vector
                        idN[ix] <- item #add the id number
                        ix <- ix + 1 #advance the index counter
                        
                        
                        
                }
        } 
        
        nobs <- cOBS
        id <- idN
        dframe <- data.frame(id, nobs)
        dframe
}

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

makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}
cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}