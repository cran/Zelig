## Attaching the sample friendship data:
data(friendship)

##### Example 1: Simple Example

## Generating empirical estimates:
user.prompt()
z.out <- zelig(friends ~ advice + prestige + perpower, model = "netlogit", data=friendship)
user.prompt()

## Viewing the regression output:
summary(z.out)

## Using setx to generate baseline and alternative values for the 
## explanatory variables:
user.prompt()
x.out <- setx(z.out)

## Simulating quantities of interest:
user.prompt()
s.out <- sim(z.out, x = x.out)
user.prompt()

## Summmarizing the simulated quantities of interest:
summary(s.out)

## Diagnostic plot of the s.out:
user.prompt()
plot(s.out)


## Example 2: First Differences
user.prompt()
x.high <- setx(z.out, perpower = quantile(friendship$perpower, prob=0.75))
x.low <- setx(z.out, perpower = quantile(friendship$perpower, prob=0.25))

user.prompt()
s.out2 <- sim(z.out, x = x.high, x1 = x.low)
user.prompt()
summary(s.out2)
user.prompt()
plot(s.out2)
