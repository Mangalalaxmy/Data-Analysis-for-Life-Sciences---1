install.packages("rafalib")
library(rafalib)
install.packages("swirl")
library(swirl)

v = c(2.23,3.45,1.87,2.11,7.33,18.34,19.23)
mean(v)
for i in 1:25:
  print(i^2)
b = 0
for (i in 1:25) {
  a = sum(i^2)
  b = a+b
  print(b)
}
cars
class(cars)
nrow(cars)
colnames(cars[2])
mean(cars[,2])
which(cars$dist==85)


dat = read.csv("femaleMiceWeights.csv")
str(dat)
dat[12,2]
dat$Bodyweight[11]
hfdat = subset(dat, Diet=="hf")
mean(hfdat$Bodyweight)

set.seed(1)
s = sample(dat[13:24,2],1)
s

sleep = read.csv("msleep_ggplot2.csv")
class(sleep)
str(sleep)

table(sleep$order == "Primates")
primates = filter(sleep, order=="Primates")
nrow(primates)
class(primates)

primates = filter(sleep, order=="Primates") %>% select(sleep_total) %>% unlist %>% mean
class(primates)
primates

install.packages("downloader")
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

mean(x)
set.seed(1)
abs(mean(sample(x,5))-mean(x))

set.seed(5)

library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

set.seed(1)
n=1000
msam <- vector("numeric", n)
for (i in 1:n) {
  msam[i] <- mean(sample(x,5))
}

mean(abs(msam-mean(x)) > 1)

set.seed(1)
n=10000
msam <- vector("numeric", n)
for (i in 1:n) {
  msam[i] <- mean(sample(x,5))
}

mean(abs(msam-mean(x)) > 1)

set.seed(1)
n=1000
msam <- vector("numeric", n)
for (i in 1:n) {
  msam[i] <- mean(sample(x,50))
}

mean(abs(msam-mean(x)) > 1)

install.packages("gapminder")
library(gapminder)
data(gapminder)
head(gapminder)

install.packages("dplyr")
library(dplyr)
x = filter(gapminder,year==1952)%>%select(lifeExp)%>%unlist
x
hist(x)
mean(x<=40)
mean(x<=60)-mean(40>=x)

prop = function(q){
  mean(x<=q)
}

prop(40)

qs = seq(from=min(x), to=max(x), length=20)
qs

props = sapply(qs,prop)
props
plot(qs,props)
props = sapply(qs, function(q) mean(x <= q))
props
plot(ecdf(x))

library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

set.seed(1)
n=1000
msam1 <- vector("numeric", n)
for (i in 1:n) {
  msam1[i] <- mean(sample(x,5))
}

set.seed(1)
n=1000
msam2 <- vector("numeric", n)
for (i in 1:n) {
  msam2[i] <- mean(sample(x,50))
}

mypar(1,2)
hist(msam1,xlim=c(18,30))
hist(msam2,xlim=c(18,30))

hist(msam2)
mean(msam2<=25)-mean(23>=msam2)

(23-23.9)/0.43
(25-23.9)/0.43

pnorm( (25-23.9) / 0.43)  - pnorm( (23-23.9) / 0.43) 

library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- read.csv(filename) 
dat <- na.omit( dat )
head(dat)
x = filter(dat,Sex=="M" & Diet=="chow")%>%select("Bodyweight")%>%unlist
mean(x)
popsd(x)
set.seed(1)
sam = sample(x,25)
mean(sam)
y = filter(dat,Sex=="M" & Diet=="hf")%>%select("Bodyweight")%>%unlist
mean(y)
popsd(y)
set.seed(1)
samy = sample(y,25)
mean(samy)
abs(mean(y)-mean(x))-abs(mean(samy)-mean(sam)
)
xf = filter(dat,Sex=="F" & Diet=="chow")%>%select("Bodyweight")%>%unlist
mean(xf)
popsd(xf)
set.seed(1)
samf = sample(xf,25)
mean(samf)
yf = filter(dat,Sex=="F" & Diet=="hf")%>%select("Bodyweight")%>%unlist
mean(yf)
popsd(yf)
set.seed(1)
samyf = sample(yf,25)
mean(samyf)
abs(mean(yf)-mean(xf))-abs(mean(samyf)-mean(samf))

library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- na.omit( read.csv(filename) )

pnorm(1)-pnorm(-1)
pnorm(2)-pnorm(-2)
pnorm(3)-pnorm(-3)

y = filter(dat, Sex=="M" & Diet=="chow")%>%select("Bodyweight")%>%unlist
popsd(y)
mean(y)
z = (y-mean(y))/popsd(y)
mean(abs(z) <=1)
mean(abs(z) <=2)
mean(abs(z) <=3)
qqnorm(z)
abline(0,1)

mypar(2,2)
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="M" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)

y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
avgs <- replicate(10000, mean( sample(y, 25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)
mean(avgs)
sd(avgs)

library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv"
if(!file.exists("femaleMiceWeights.csv")) download(url,destfile=filename)
dat <- read.csv(filename)

set.seed(1)
n <- 100
sides <- 6
p <- 1/sides
zs <- replicate(10000,{
  x <- sample(1:sides,n,replace=TRUE)
  (mean(x==6) - p) / sqrt(p*(1-p)/n)
}) 
qqnorm(zs)
abline(0,1)
mean(abs(zs) > 2)

ps <- c(0.5,0.5,0.01,0.01)
ns <- c(5,30,30,100)
library(rafalib)
mypar(4,2)
for(i in 1:4){
  p <- ps[i]
  sides <- 1/p
  n <- ns[i]
  zs <- replicate(10000,{
    x <- sample(1:sides,n,replace=TRUE)
    (mean(x==1) - p) / sqrt(p*(1-p)/n)
  }) 
  hist(zs,nclass=7)
  qqnorm(zs)
  abline(0,1)
}

X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist
mean(X)
sd(X)
2 * ( 1-pnorm(2/sd(X) * sqrt(12) ) )
sqrt( sd(X)^2/12 + sd(Y)^2/12 )
t.test(Y, X)
Z <- ( mean(Y) - mean(X) ) / sqrt( var(X)/12 + var(Y)/12)
2*( 1-pnorm(Z)) 

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist
library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

set.seed(1)
dat.ns = sample(bwt.nonsmoke, 25)
dat.s = sample(bwt.smoke, 25)
t.test(dat.s, dat.ns)

X.ns <- mean(dat.ns)
sd.ns <- sd(dat.ns)
X.s <- mean(dat.s)
sd.s <- sd(dat.s)
sd.diff <- sqrt(sd.ns^2/25+sd.s^2/25)
tval <- (X.s - X.ns)/sd.diff
tval
pval = 1 - (pnorm(abs(tval))-pnorm(-abs(tval)))
pval
qnorm(.995)*sd.diff



set.seed(1)
tdat.ns = sample(bwt.nonsmoke, 25)
tdat.s = sample(bwt.smoke, 25)
X.ns <- mean(dat.ns)
sd.ns <- sd(dat.ns)
X.s <- mean(dat.s)
sd.s <- sd(dat.s)
sd.diff <- sqrt(sd.ns^2/25+sd.s^2/25)
t.test(tdat.s, tdat.ns)
tval <- (X.s - X.ns)/sd.diff
tval
pval = 1 - (pt(abs(tval), 48)-pt(-abs(tval), 48))
pval
qt(.995, 48)*sd.diff

set.seed(1)
tdat.ns = sample(bwt.nonsmoke, 5)
tdat.s = sample(bwt.smoke, 5)
X.ns <- mean(dat.ns)
sd.ns <- sd(dat.ns)
X.s <- mean(dat.s)
sd.s <- sd(dat.s)
sd.diff <- sqrt(sd.ns^2/5+sd.s^2/5)
t.test(tdat.s, tdat.ns)

N = 5
B = 10000
alpha = 0.01
set.seed(1)
reject = function(N,alpha=0.01){
  dat.ns = sample(bwt.nonsmoke, N)
  dat.s = sample(bwt.smoke, N)
  pval = t.test(dat.s, dat.ns)$p.value
  pval < alpha
}
rejections = replicate(B, reject(N))
mean(rejections)
ss = c(30, 60, 90, 120)
power = sapply(ss, function(N){
  rejections = replicate(B, reject(N))
  mean(rejections)
})
mypar(1,1)
plot(ss, power, type="b")

mypar(3,2)
set.seed(1)
Ns = seq(5, 30,5)
B = 1000
mypar(3,2)
for(N in Ns){
         tstats = replicate(B,{
    X = rnorm(N)
    t = (sqrt(N)*mean(X))/sd(X)
    })
    ps = seq(1/(B+1), 1-1/(B+1),len=B)
    q = qt(ps,df=N-1)
    qqplot(q,tstats)
    abline(0,1)
}

mean(tstats>2)
1-pt(2, df=4)

Ns<-seq(5,30,5)
B <- 1000
mypar(3,2)
LIM <- c(-4.5,4.5)
for(N in Ns){
   ts <- replicate(B, {
    X <- rnorm(N)
    sqrt(N)*mean(X)/sd(X)
    })
  ps <- seq(1/(B+1),1-1/(B+1),len=B)
  qqplot(qt(ps,df=N-1),ts,main=N,
         xlab="Theoretical",ylab="Observed",
         xlim=LIM, ylim=LIM)
  abline(0,1)
 } 

Ns<-seq(5,30,5)
B <- 1000
mypar(3,2)
LIM <- c(-4.5,4.5)
for(N in Ns){
  ts <- replicate(B,{
    x <- rnorm(N)
    y <- rnorm(N)
    t.test(x,y, var.equal = TRUE)$stat
  })
  ps <- seq(1/(B+1),1-1/(B+1),len=B)
  qqplot(qt(ps,df=2*N-2),ts,main=N,
         xlab="Theoretical",ylab="Observed",
         xlim=LIM, ylim=LIM)
  abline(0,1)
}  
set.seed(1)
N <- 15
B <- 10000
tstats <- replicate(B,{
  X <- sample(c(-1,1), N, replace=TRUE)
  sqrt(N)*mean(X)/sd(X)
})
ps=seq(1/(B+1), 1-1/(B+1), len=B) 
qqplot(qt(ps,N-1), tstats, xlim=range(tstats))
abline(0,1)

set.seed(1)
N <- 1000
B <- 10000
tstats <- replicate(B,{
  X <- sample(c(-1,1), N, replace=TRUE)
  sqrt(N)*mean(X)/sd(X)
})
ps=seq(1/(B+1), 1-1/(B+1), len=B) 
qqplot(qt(ps,N-1), tstats, xlim=range(tstats))
abline(0,1)


url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

N=10
set.seed(1)
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
obs <- mean(smokers) - mean(nonsmokers)

set.seed(1)
diff = replicate(1000,{
  dat <- c(smokers,nonsmokers)
  shuffle <- sample( dat )
  smokersstar <- shuffle[1:N]
  nonsmokersstar <- shuffle[(N+1):(2*N)]
  mean(smokersstar)-mean(nonsmokersstar)
  })
mean(abs(diff)>abs(obs))

N=10
set.seed(1)
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
obs <- median(smokers) - median(nonsmokers)

set.seed(1)
diff = replicate(1000,{
  dat <- c(smokers,nonsmokers)
  shuffle <- sample( dat )
  smokersstar <- shuffle[1:N]
  nonsmokersstar <- shuffle[(N+1):(2*N)]
  median(smokersstar)-median(nonsmokersstar)
})
mean(abs(diff)>abs(obs))

d= read.csv("assoctest.csv")
table(d$allele, d$case)
table(d)
chisq.test(table(d))
fisher.test(table(d))

load("skew.RData")
dim(dat)
par(mfrow = c(3,3))
for (i in 1:9) {
  qqnorm(dat[,i])
}
hist(dat[,4])
hist(dat[,9])

mypar(2,1)
head(InsectSprays)
boxplot(InsectSprays$count~InsectSprays$spray)
boxplot(split(InsectSprays$count, InsectSprays$spray))
library(dplyr)
install.packages("UsingR")
data(nym.2002, package="UsingR")
head(nym.2002)
mypar(1,1)
boxplot(nym.2002$time~nym.2002$gender)

mypar(1,3)
males <- filter(nym.2002, gender=="Male") %>% select(time) %>% unlist
females <- filter(nym.2002, gender=="Female") %>% select(time) %>% unlist
boxplot(females, males)
hist(females,xlim=c(range( nym.2002$time)))
hist(males,xlim=c(range( nym.2002$time)))

males <- filter(nym.2002, gender=="Male") 
females <- filter(nym.2002, gender=="Female") 
cor(males$age, males$time)
cor(females$age, females$time)

mypar(2,1)
plot(nym.2002$age, nym.2002$time)
boxplot(nym.2002$time~nym.2002$age)

time = sort(nym.2002$time)
min(time)/median(time)
max(time)/median(time)

plot(time/median(time), ylim=c(1/4,4))
abline(h=c(1/2,1,2))

plot(log2(time/median(time)),ylim=c(-2,2))
abline(h=-1:1)

data(ChickWeight)
head(ChickWeight)
plot( ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)
chick = reshape(ChickWeight, idvar=c("Chick","Diet"), timevar="Time",direction="wide")
head(chick)
chick = na.omit(chick)

day4 = chick$weight.4
ct4 = length(chick$weight.4)
navg = sum(day4)/ct4
navg
sday4= sum(day4)
oavg = (sday4+c(3000))/(ct4+c(1))
oavg
oavg/navg

summary(chick$weight.4)
summary(c(chick$weight.4,3000))
mean(c(chick$weight.4,3000))/mean(chick$weight.4) 
median(c(chick$weight.4,3000))/median(chick$weight.4) 
sd(c(chick$weight.4,3000))/sd(chick$weight.4)
mad(c(chick$weight.4,3000))/mad(chick$weight.4)

mypar(1,1)
plot(chick$weight.4,chick$weight.21)

pearson_No_Outlier<-cor(chick$weight.4,chick$weight.21,method="pearson")
pearson_With_Outlier<-cor(c(chick$weight.4,3000),c(chick$weight.21,3000),method="pearson")
pearson_With_Outlier/pearson_No_Outlier

spearman_No_Outlier<-cor(chick$weight.4,chick$weight.21,method="spearman")
spearman_With_Outlier<-cor(c(chick$weight.4,3000),c(chick$weight.21,3000),method="spearman")
spearman_With_Outlier/spearman_No_Outlier

x = chick %>% filter(Diet==1) %>% select(weight.4) %>% unlist
y = chick %>% filter(Diet==4) %>% select(weight.4) %>% unlist
t.test(x,y)
wilcox.test(x,y)
x2=c(x,200)
t.test(x2,y)
wilcox.test(x2,y)

library(rafalib)
mypar(1,3)
boxplot(x,y)
boxplot(x,y+10)
boxplot(x,y+100)

a=t.test(x,(y+10))$statistic
b=t.test(x,(y+100))$statistic
a
b
a-b
wilcox.test(c(1,2,3), c(4,5,6))
wilcox.test(c(1,2,3), c(400,500,600))
