library(MatchIt)
data(lalonde)
user.prompt()
## an example for propensity score matching
match.out1 <- matchit(treat ~ age + educ + black + hispan + married + 
                      nodegree + re74 + re75, data = lalonde)
user.prompt()

z.out1 <- zelig(re78 ~ pscore, data = match.data(match.out1, "control"), model = "ls")
user.prompt()

x.out1 <- setx(z.out1, fn = NULL, data = match.data(match.out1, "treat"), cond = TRUE)
user.prompt()

s.out1 <- sim(z.out1, x = x.out1)
user.prompt()
summary(s.out1)
user.prompt()

## an example for subclassification
match.out2 <- matchit(treat ~ age + educ + black + hispan + married +
                      nodegree + re74 + re75, data = lalonde,
                      replace = FALSE, subclass = 3) 
user.prompt()

z.out2 <- zelig(re78 ~ pscore, data = match.data(match.out2, "control"),
                model="ls", by="psclass")
user.prompt()

x.out2 <- setx(z.out2, fn = NULL, data = match.data(match.out2, "treat"), cond = TRUE)
user.prompt()

s.out2 <- sim(z.out2, x = x.out2)
user.prompt()

summary(s.out2) # overall results
user.prompt()

summary(s.out2, subset = 2) # subclass 2



