# Kevin O'Neill
# math-m 447
# Cats Problem



pickLetter <- function() {
    r <- runif(1) # runif(1) generates a random number between 0 and 1
    if (r >= 0 & r < .3) {letter <- "C"}
    if (r >= .3 & r < .4) {letter <- "A"}
    if (r >= .4 & r < .7) {letter <- "T"}
    if (r >= .7 & r <= 1) {letter <- "S"}
    return(letter)
}

# add a letter to current set
addLetter <- function(set, letter) {
    if (any(set == letter)) {
        return(set)
    }
    else {
        set <- append(letter, set)
        return(set)
    }
}

# build set until you get all the letters in the CATS set
getCats <- function() {
    catSet <- c("C","A","T","S")
    mycats <- c()
    iterations <- 0
    while (!setequal(mycats, catSet)) {
        newLetter <- pickLetter()
        mycats <- addLetter(mycats, newLetter)
        iterations <- iterations + 1
    }
    return(iterations) 
}

# build a vector of the number of iterations in each run
collect_iterations <- function() {
    iter_vec <- c()
    for (i in 1:10000) {
        iter_vec <- append(iter_vec,getCats())
    }
    return(iter_vec)
}

# run collect iterations and save as results as vector
iter_vec <- collect_iterations()
# summary to display info from vector
summary(iter_vec)

### OUTPUT
# > iter_vec <- collect_iterations()
# > summary(iter_vec)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4.00    7.00    9.00   11.88   15.00   74.00 
# > iter_vec <- collect_iterations()
# > summary(iter_vec)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4.0     6.0     9.0    12.5    15.0    76.0 
# > iter_vec <- collect_iterations()
# > summary(iter_vec)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4.00    6.00    9.00   11.98   15.00   84.00 
# > iter_vec <- collect_iterations()
# > summary(iter_vec)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4.00    6.00    9.00   11.66   14.00   73.00 