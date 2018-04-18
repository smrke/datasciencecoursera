complete <- function(directory = "/Users/briannasmrke/datasciencecoursera/specdata", id = 1:3) {
        
        currentdir <- setwd(directory)
        
        cOBS <- numeric()
        
        idN <- numeric()
        
        ix <- 1
        
        for (item in id) {
                
                
                if (item >= 100){
                        
                        filepath <- paste(currentdir, "/", item, ".csv", sep = "")
                        print(filepath) #just so we can see what's going on
                        data <- read.csv(filepath) #create data frame
                        nCC <- sum(complete.cases(data))
                        cOBS[ix] <- nCC #add the number of complete cases to our vector
                        idN[ix] <- item #add the id number
                        ix <- ix + 1 #advance the index counter
                        
                }
                
                else if (item >= 10) {
                        
                        filepath <- paste(currentdir, "/", "0", item, ".csv", sep = "")
                        print(filepath) #just so we can see what's going on
                        data <- read.csv(filepath) #create data frame
                        nCC <- sum(complete.cases(data))
                        cOBS[ix] <- nCC #add the number of complete cases to our vector
                        idN[ix] <- item #add the id number
                        ix <- ix + 1 #advance the index counter
                        
                        
                        
                }
                
                else {
                        
                        filepath <- paste(currentdir, "/", "00", item, ".csv", sep = "")
                        print(filepath) #just so we can see what's going on
                        data <- read.csv(filepath) #create data frame
                        nCC <- sum(complete.cases(data))
                        cOBS[ix] <- nCC #add the number of complete cases to our vector
                        idN[ix] <- item #add the id number
                        ix <- ix + 1 #advance the index counter
                        
                        
                        
                }
        } 
     print(cOBS)
     print(idN)
}