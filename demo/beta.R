data(house)

z.out <- zelig(dpct86 ~ dpct84 + dwin86 + incum86, data = house, model = "beta")

user.prompt()

summary(z.out)

user.prompt()

x0 <- setx(z.out, incum86 = 0)
x1 <- setx(z.out, incum86 = 1)

user.prompt()

s.out <- sim(z.out, x = x0, x1 = x1)

user.prompt()

summary(s.out)

user.prompt()

plot(s.out)
