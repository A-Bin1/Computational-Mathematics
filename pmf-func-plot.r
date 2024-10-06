# Task 1: Construct a user define probability mass function.

checksum <- function(p){
    #making sure inputs are correct, sum of prob = 1, referenced in trycatch block in pmf function
    p_sum = Reduce('+',p)
    result = p_sum
    if (result != 1){
        return('FALSE')
    } else {
        return(p_sum)
    }
}

XX <- 1:10
PP <- c(.01,.12,.13,.14,.2,.2,.1,.05,.04,.01)
pmf_func <- function(rv, p){
    tryCatch(
        {
        result = Map('*',rv,checksum(p))
        return(result)
        },
        error=function(e) {
            message('Probability not equal to 1')
            
        },
        warning=function(w) {
            message('A Warning Occurred')
            print(w)
            return(NA)
        }
    )
}

print(pmf_func(XX,PP))


# d) The following puts two graphs one on top of the other
require(graphics)
par(mfrow = c(2, 1))

# e) Now plot the pmf you defined above (the specified option “h” means that the plot is vertical lines

plot(XX,PP,type="h",col=2,main="Pmf list",xlab="x",ylab="p(x)")
points(XX,PP,col=2);abline(h=0,col=3)

# # f) Obtain the cumulative probability distribution function (cdf)

QQ <- cumsum(PP)

# # g) Print all these values together
c(XX, PP, QQ)

# # h) Now plot the cdf, the option “s” means that the plot should be a staircase type. The c(0, XX)
# # and c(0, QQ) adds a zero starting point.
plot(c(1,XX),c(0,QQ),type="s", ylab="F(x)",col=2,xlab="x",
main="Cdf for user defined dist.")
abline(h=0:1,col=4)

# Task 2:
# Use the above approach to calculate and plot the pmf and the cdf for the first 11 values (x=0, 1, 2, ...10) of the:

# a) Binomial distribution with n=10 and p=0.6
PP1 <- pbinom(XX, size =10,prob =0.6) # note that n stands for ‘size’
#PMF
plot(XX,PP1,type="h",col=2,main="Pmf list",xlab="x",ylab="p(x)")
points(XX,PP1,col=2);abline(h=0,col=3)

#CDF
QQ1 <- cumsum(PP1)
plot(c(1,XX),c(0,QQ1),type="s", ylab="F(x)",col=2,xlab="x",
main="Cdf Binomial")
abline(h=0:1,col=4)

# dbinom
PP2 <- dbinom(XX, size =10,prob =0.6)
#PMF
plot(XX,PP2,type="h",col=2,main="Pmf list",xlab="x",ylab="p(x)")
points(XX,PP2,col=2);abline(h=0,col=3)

#CDF
QQ2 <- cumsum(PP2)
plot(c(1,XX),c(0,QQ2),type="s", ylab="F(x)",col=2,xlab="x",
main="Cdf DBinomial")
abline(h=0:1,col=4)

# # b) and of the Poisson distribution with lambda=6 (corresponding to n=10 and p=0.6)
PP3 <- ppois(XX, lambda = 6)
#PMF
plot(XX,PP3,type="h",col=2,main="Pmf list",xlab="x",ylab="p(x)")
points(XX,PP3,col=2);abline(h=0,col=3)

#CDF
QQ3 <- cumsum(PP3)
plot(c(1,XX),c(0,QQ3),type="s", ylab="F(x)",col=2,xlab="x",
main="Cdf Poisson Lambda = 6")
abline(h=0:1,col=4)

# dpois
PP4 <- dpois(XX, lambda = 6)
#PMF
plot(XX,PP4,type="h",col=2,main="Pmf list",xlab="x",ylab="p(x)")
points(XX,PP4,col=2);abline(h=0,col=3)
 
#CDF
QQ4 <- cumsum(PP4)
plot(c(1,XX),c(0,QQ4),type="s", ylab="F(x)",col=2,xlab="x",
main="Cdf DPoisson Lambda = 6")
abline(h=0:1,col=4)

