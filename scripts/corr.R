corr <- function(directory, threshold = 0, id = 1:332) {
        
        filelist <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
        corrs <- numeric()
        
        
        for(i in id){
                data <- read.csv(filelist[i])
                data <- data[complete.cases(data),]
                
                if (nrow(data) >= threshold){
                        corrs <- c(corrs, cor(data[['sulfate']],data[['nitrate']]))
                }

        }
        
        corrs
}