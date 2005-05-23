data(mexico)
user.prompt()
z.out <- zelig(vote88 ~ pristr + othcok + othsocok, model = "mlogit", 
               data = mexico)
user.prompt()
print(summary(z.out))

user.prompt()
x.weak <- setx(z.out, pristr = 1)
x.strong <- setx(z.out, pristr = 3)

user.prompt()
s.out <- sim(z.out, x = x.strong, x1 = x.weak)
user.prompt()
print(summary(s.out))

user.prompt()
ev.weak <- s.out$qi$ev + s.out$qi$fd

user.prompt()
library(vcd)
ternaryplot(s.out$qi$ev, pch = ".", col = "blue",
            main = "1988 Mexican Presidential Election")
user.prompt()
ternarypoints(ev.weak, pch = ".", col = "red")


