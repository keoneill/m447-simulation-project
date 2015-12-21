library(lpSolve) #Wake up the package

#The original linear programming x dot c  restricted on Ax<= b
#f.obj = c(12,35,205) #Define c
#f.con = matrix(c(20,15,6,20,30,40,6,10,500,200,120,300),4,3) # Define A
#f.dir = c("<=","<=","<=","<=") #Assign the ineqalities
#f.rhs = c(2400,1600,750,900) # Define b
#Show the maximum
lp("max",f.obj,f.con,f.dir,f.rhs,all.int=TRUE)
#Show the solution
lp("max",f.obj,f.con,f.dir,f.rhs,all.int=TRUE)$solution 


#The dual
lp("min",f.rhs,t(f.con),c(">=",">=",">="),f.obj)
lp("min",f.rhs,t(f.con),c(">=",">=",">="),f.obj)$solution

#Part c
f.obj = c(12,35,250,-0.1) 
f.con = matrix(c(20,15,6,20,30,40,6,10,500,200,120,300,0,0,0,-1),4,4) #Add one more column
f.dir = c("<=","<=","<=","<=")
f.rhs = c(2400,1600,750,900)
lp("max",f.obj,f.con,f.dir,f.rhs,all.int=TRUE)
lp("max",f.obj,f.con,f.dir,f.rhs,all.int=TRUE)$solution
