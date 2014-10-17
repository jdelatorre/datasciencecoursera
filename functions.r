add2 <- function(x,y) {
        x + y
}

above10 <- function(x){
        use <- x>10
        x[use]
}

above <- function(x, n=10) {
        use <- x > n
        x[use]
}

columnmean <- function(y) {
        nc <- ncol(y)
        means <- numeric(nc)
        for(i in 1:nc) {
                means[i] <- mean(y[, i])
        }
        means
}

columnmean <- function(y, removeNA = TRUE) {
        nc <- ncol(y)
        means <- numeric(nc)
        for(i in 1:nc) {
                means[i] <- mean(y[, i], na.rm = removeNA)
        }
        means
}

weightmedian <- function(directory, day) {
        
        files_list <- list.files(directory, full.names = TRUE)
        dat <- data.frame()
        for (i in 1:5) {
                dat <- rbind(dat, read.csv(files_list[i]))
        }
        dat_subset <- dat[which(dat[, "Day"] == day),]
        median(dat_subset[, "Weight"], na.rm = TRUE)
        
}