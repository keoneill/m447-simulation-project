# Part A
> A <- matrix(c(20,30,500, 15,40,200, 6,6,120, 20,10,300), 4, 3, byrow=TRUE)
> A
     [,1] [,2] [,3]
[1,]   20   30  500
[2,]   15   40  200
[3,]    6    6  120
[4,]   20   10  300

> b <- c(2400,1600,750,900)
> objVec <- c(12,35,205)

# maximize 12a + 35b + 205c subject to AX <= b
> lp('max', objVec, A, "<=", b)
Success: the objective function is 1460 
> lp('max', objVec, A, "<=", b)$solution
[1]  0 30  2

##############
## Part B
# dual problem: minimize 2400r1 + 1600r2 + 750r3 + 900r4 
> c <- objVec # old objective vectore becomes c in "(A^t)y >= c" equation 
> At <- t(A) # Transpose A
> objVec_dual <- b

> objVec_dual
[1] 2400 1600  750  900

> lp('min', objVec_dual, At, ">=", c) # dual problem
Success: the objective function is 1460 
> lp('min', objVec_dual, At, ">=", c)$solution
[1] 0.000 0.845 0.000 0.120

############
##  Part C
> A <- matrix(c(20,30,500,0, 15,40,200,0, 6,6,120,0, 20,10,300,-1 ), 4, 4, byrow=TRUE)
> A
     [,1] [,2] [,3] [,4]
[1,]   20   30  500    0
[2,]   15   40  200    0
[3,]    6    6  120    0
[4,]   20   10  300   -1

# Testing with q = .1
> objVec_q1 <- c(12,35,205,-.1)
> lp('max', objVec_q1, A, "<=", b)
Success: the objective function is 1467.143 
> lp('max', objVec_q1, A, "<=", b)$solution
[1]   0.000000  22.857143   3.428571 357.142857

# Testing with q = .05
> objVec_q2 <- c(12,35,205,-.05)
> lp('max', objVec_q2, A, "<=", b)
Success: the objective function is 1485 
> lp('max', objVec_q2, A, "<=", b)$solution
[1]   0.000000  22.857143   3.428571 357.142857
