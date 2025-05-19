library(lpSolve)
options(scipen = 999) ### para que no se vea el valor exponencial

#Establish the coefficients of our objective function
C <- c(3,2)

# Create constraint matrix B
A <- matrix(c(2, 1,
              2, 3,
              3, 1), nrow=3, byrow=TRUE)

# Right hand side for the constraints
B <- c(18,42,24)

# Direction of the constraints
constranints_direction  <- c("<=", "<=", "<=")

# Find the optimal solution
optimum <-  lp(direction="max",
               objective.in = C,
               const.mat = A,
               const.dir = constranints_direction,
               const.rhs = B,
               all.int = T)

str(optimum)

# Print status: 0 = success, 2 = no feasible solution
print(optimum$status)

# Display the optimum values for x_4p, x_3p and x_w
best_sol <- optimum$solution
names(best_sol) <- c("x", "y") 
print(best_sol)

print(paste("Total cost: ", optimum$objval, sep=""))

### Sensibility Analysis

# Find the optimal solution
sensitivity <-  lp(direction="max",
                   objective.in = C,
                   const.mat = A,
                   const.dir = constranints_direction,
                   const.rhs = B,
                   #all.int = T,
                   compute.sens = TRUE)


## variation ranges for coefficients of the objetive function
print(sensitivity$sens.coef.from)
print(sensitivity$sens.coef.to)

## variation ranges for restrictions (right side)
print(sensitivity$duals)
print(sensitivity$duals.from)
print(sensitivity$duals.to)
