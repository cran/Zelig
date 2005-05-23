#####
##### robust estimation of covariance matrix
#####
 
#####  Example 1: linear least squares regression with
#####             heteroskedasticity consistent standard errors (default) 
# Attach sample data and variable names:  
data(macro)

# Fit the model with robust standard error
user.prompt()
z.out1 <- zelig(unem ~ gdp + capmob + trade, model = "ls", data = macro, robust = TRUE)
user.prompt()
print(summary(z.out1))

# usual procedure applies
user.prompt()
x <- setx(z.out1)
user.prompt()
s.out1 <- sim(z.out1, x = x)
user.prompt()
print(summary(s.out1))
user.prompt()
plot(s.out1)

#####  Example 2: linear least squares regression with
#####             heteroskedasticity and autocorrelation consistent standard errors 

# Attach sample data and variable names:
data(hoff)
# Fit the model with robust standard error
user.prompt()
z.out1 <- zelig(L2SocSec ~ Just503D + Just503R + Just503D:RGovDumy +
                Just503R:I(1-RGovDumy), model = "ls", data = hoff,
                robust = list(method="vcovHAC", order.by=hoff$year, adjust=TRUE))
user.prompt()
print(summary(z.out1))
