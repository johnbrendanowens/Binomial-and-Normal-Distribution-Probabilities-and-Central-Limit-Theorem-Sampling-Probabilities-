#4.4 For the car safety (UsingR) data set, make a scatterplot of the variable Driver. 
#deaths versus Other. deaths. Use pch=as. numeric (type) to change the plot character based on 
#the value of type. Label any outliers with their make or model using identify (). 
#Do you notice any trends?

library(UsingR)
data("carsafety")
attach(carsafety)
plot (Driver.deaths, Other.deaths, pch = as.numeric(type), main="Scatterplot of Driver Deaths vs. Other Deaths", 
      xlab="Driver Deaths ", ylab="Other Deaths ")
table(as.numeric(type),type)
legend(35, 135,c("SUV", "compact", "large", "midsize", "minivan", "pickup", "subcompact"), pch=1:7)

detach(carsafety)

#4.7


data("mtcars")
attach(mtcars)
newcars <- mtcars[order(-wt),]

mtcars[which.max(mpg), ] #max mpg car
mtcars[which.min(mpg), ] #min mpg car

df = data.frame(mtcars)
import = df [c(1:3, 8:14, 18:21, 26:28, 30:32), 1]
import
domestic = df[c(4:7, 15:17, 22:25, 25), 1]
domestic


boxplot(import, domestic, main = "MPG of Import Versus Domestic Cars", 
        names=c("Import","Domestic"), ylab="Miles Per Gallon")

plot (wt, mpg, pch = as.numeric(cyl), main="Weight vs. Miles Per Gallon", 
      xlab="Weight ", ylab="MPG ")
legend (1.5, 20, c("4 Cylinder", "6 Cylinder", "8 Cylinder"), pch=1:3)

detach(mtcars)

#4.21

library(MASS)
data("UScereal")
attach(UScereal)
pairs(~ calories + carbo + protein + fat+fibre+sugars, data = UScereal)
detach (UScereal)

#4.25
#The data set ToothGrowth contains measurements of tooth growth for 
#different dosages of a supplement. Use the function bwplot() to make 
#boxplots of len for each level of dose broken up by the levels of supp. 
#You need to make dose a factor first. Also, repeat the graphic after 
#reversing the role of the two factors.

data("ToothGrowth")
attach(ToothGrowth)

bwplot(len ~ factor(dose) | supp, data = ToothGrowth, xlab = "Dose",
       ylab = "Len", cex.lab=1.5)

bwplot(len ~ factor(supp) | dose, data = ToothGrowth, xlab = "Len",
       ylab = "Supp", cex.lab=1.5)

detach(ToothGrowth)

#5.12
#Use the binomial distribution to decide which is more likely: rolling two dice
#twenty-four times and getting at least one double sixes, or rolling one 
#die four times and getting at least one six?

1- pbinom(0, 24, 1/36) 
k= 2:12
sample(k, 24, replace=TRUE)

1-pbinom(0, 4, 1/6) 
k= 1:6
sample(k, 4, replace=TRUE)

#5.15
#Suppose that the population of adult, male black bears has 
#weights that are approximately distributed as Normal(350,75). 
#What is the probability that a randomly observed male bear weighs more 
#than 450 pounds?

1- pnorm(450, 350, 75) #pnorm calculates 450 or less, so 1-pnorm gives the 
                      #probability that it is greater than 450


#5.21
#For the f height variable in the father. son (UsingR) data set, 
#compute what percent of the data is within 1, 2, and 3 standard deviations 
#from the mean. Compare to the percentages 68%, 95%, and 99.7%.
#Not sure how to use scale

data("father.son")
attach(father.son)

z.father = scale(fheight, center=TRUE, scale= TRUE)
z.father
count.one= sum (abs(z.father)<1)/(length(z.father))
count.one
count.two = sum (abs(z.father)<2)/(length(z.father))
count.two
count.three = sum (abs(z.father)<3)/(length(z.father))
count.three

detach(father.son)

#5.23 For a Uniform(0, 1) random variable, the mean and variance are 1/2 and 1/12. 
#Find the area within 1, 2, and 3 standard deviations from the mean and compare to 
#68%, 95%, and 99.7%. Do the same for the Exponential(l/5) distribution with mean 
#and standard deviation of 5.

std.1.uni = punif(.5+1*(sqrt(1/12))) - punif(.5-1*(sqrt(1/12)))
std.1.uni
std.2.uni = punif(.5+2*(sqrt(1/12))) - punif(.5-2*(sqrt(1/12)))
std.2.uni
std.3.uni = punif(.5+3*(sqrt(1/12))) - punif(.5-3*(sqrt(1/12)))
std.3.uni

std.1.exp = pexp(5, 1/5)
std.1.exp
std.2.exp = pexp(5, 2*1/5)
std.2.exp
std.3.exp = pexp(5, 3*1/5)
std.3.exp

#5.24 A q-q plot is an excellent way to investigate whether a distribution is 
#approximately normal. For the symmetric distributions Uniform(0, 1), 
#Normal(0, 1) and t with 3 degrees of freedom, take a random sample of size 
#100 and plot a quantile-normal plot using qqnorm(). Compare the three and 
#comment on the curve of the plot as it relates to the tail length. (The 
#uniform is short-tailed; the t-distribution with 3 degrees of freedom is 
#long-tailed.)

uni = runif(100)
qqnorm(uni, main="Uniform Distribution")
qqline(uni)
t.3 = rt(100, 3)
qqnorm(t.3, main="T distribution")
qqline(t.3)
norm = rnorm(100)
qqnorm(norm, main="Normal distribution")
qqline(norm)

#5.27
#Compare the exact probability of getting 42 or fewer heads in 
#100 coin tosses to the probability given by the normal approximation.

pbinom(42, 100, .5) 
pnorm(42, 50, 5) 

#5.30
#An elevator can safely hold 3,500 pounds. A sign in the elevator limits 
#the passenger count to 15. If the adult population has a mean weight of 
#180 pounds with a 25-pound standard deviation, how unusual would it be, 
#if the central limit theorem applied, that an elevator holding 15 people 
#would be carrying more than 3,500 pounds?

mu = 180 #avg weight of person
sigma = 25 #standard deviation
n=15 #sample size
test = 3500/15 #average weight to be over 3500 lbs
test
1- pnorm(test, mu, sigma/(sqrt(n))) 
