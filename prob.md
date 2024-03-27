---
title: "확률의 개념과 응용"
format: html
editor: visual
---

## 제 1 장

```{r, warning=FALSE, echo=FALSE}
#install.packages("ggplot2")
#install.packages("magrittr")
library(ggplot2)
library(magrittr)
```

### (1) 주사위 눈금별 상대도수에 대한 히스토그램

```{r}
RDice = function(nn)  { return (table(factor(sample(1:6, nn, replace=TRUE), levels = 1:6)) / nn) }
```

```{r}
set.seed(1234567)
dice_1 = RDice(12) 
dice_2 = RDice(120)
dice_3 = RDice(1200)
dice_4 = RDice(12000)
```

```{r}
# data transformation
dice = c(as.numeric(dice_1), as.numeric(dice_2), as.numeric(dice_3), 
         as.numeric(dice_4))
nn   = c(rep("(a) n=12",6),rep("(b) n=120",6), rep("(c) n=1,200",6), 
         rep("(d) n=12,000",6)) 
num = c(rep(1:6,4))
dice_result = data.frame(nn, num, dice)
```

```{r}
# ggplot2
ggplot(data = dice_result, aes(x = num, y = dice)) +
 geom_bar(stat = "identity", alpha = 0.8) +
 xlab("\n Result on Die") + ylab("Relative Frequency\n") +
 geom_hline(yintercept = 1/6, colour=2, lty=2) +
 ylim(0,0.5) + facet_wrap(~nn, ncol = 2)
```

## 제 2 장

```{r}
library(gtools)
x = c('a', 'b', 'c')
```
### (1) 복원, 순서고려 2개 추출
```{r}
print("복원, 순서고려 2개 추출") 
rr1 = permutations(n=3,r=2,v=x,repeats.allowed=T)
nrow(rr1)
print(rr1)
```
### (2) 복원, 순서비고려 2개 추출
```{r}
print("복원, 순서비고려 2개 추출") 
rr2 = combinations(n=3,r=2,v=x,repeats.allowed=T)
nrow(rr2)
print(rr2)
```
### (3) 비복원, 순서비고려 2개 추출
```{r}
print("비복원, 순서비고려 2개 추출") 
rr3 = combinations(n=3,r=2,v=x,repeats.allowed=F)
nrow(rr3)
choose(3,2)
print(rr3)
```

## 제 3 장

### (1) 베이즈 정리 계산
```{r}
library(LaplacesDemon)
# 질병 확률 : 0.05
D=0.05 ; Dc = 1 - D
# 질병일 때 양성 확률 : 0.98, 비질병일 때 음성 확률 :0.90
Tp_D = 0.98    ; Tm_Dc = 0.90
Tm_D = 1 - Tp_D; Tp_Dc = 1 - Tm_Dc
# 확률 정리
Dp   = c(D, Dc)
D_Tp = c(Tp_D, Tp_Dc) 
# 양성일 때 질병 확률
print(BayesTheorem(Dp, D_Tp)[1])
print(Tp_D*D/(Tp_D*D+Tp_Dc*Dc))

Dc_Tm = c(Tm_D, Tm_Dc) 
# 음성일 때 질병 아닐 확률
print(BayesTheorem(Dp, Dc_Tm)[2])
print(Tm_Dc*Dc/(Tm_D*D+Tm_Dc*Dc))
```

## 제 4 장

### (1) 기댓값과 분산
```{r}
#install.packages("distrEx")
library(distrEx)
X = DiscreteDistribution(supp = c(1:6), prob = rep(1/6,6))
plot(X)
E(X)
var(X)
sd(X)
E(2*X+5)
var(2*X+5)
sd(2*X+5)
```

## 제 5 장

```{r}
library(distrEx)
```

### (1) 이산형 균등분포

```{r}
par(mfrow=c(1,2))
X = DiscreteDistribution(supp = c(1:6), prob = rep(1/6,6))
# 확률질량함수
plot(X, cex.points = 1, to.draw.arg=c("d"), mfColRow = FALSE, inner=" ", xlab="", ylim=c(0,0.5))
# 누적분포함수
plot(X, cex.points = 1, to.draw.arg=c("p"), mfColRow = FALSE, inner=" ", xlab="")
E(X)
var(X)
```

### (2) 베르누이분포

```{r}
par(mfrow=c(1,2))
X = Binom(1, 0.5)
# 확률질량함수
plot(X, cex.points = 1, to.draw.arg=c("d"), mfColRow = FALSE, inner=" ", xlab="", ylim=c(0,1))
# 누적분포함수
plot(X, cex.points = 1, to.draw.arg=c("p"), mfColRow = FALSE, inner=" ", xlab="")
E(X)
var(X)
```

### (2-1) 이항분포

```{r}
par(mfrow=c(1,2))
X = Binom(10, 0.5)
# 확률질량함수
plot(X, cex.points = 1, to.draw.arg=c("d"), mfColRow = FALSE, inner=" ", xlab="", ylim=c(0,0.5))
# 누적분포함수
plot(X, cex.points = 1, to.draw.arg=c("p"), mfColRow = FALSE, inner=" ", xlab="")
```

### (2-2) 이항분포 : n과 p를 달리한 분포

```{r}
par(mfrow=c(1,3))
X1 = Binom(10, 0.3)
X2 = Binom(20, 0.3)
X3 = Binom(50, 0.3)

Y1 = Binom(10, 0.5)
Y2 = Binom(20, 0.5)
Y3 = Binom(50, 0.5)
# 확률질량함수
plot(X1, cex.points = 1, to.draw.arg=c("d"), mfColRow = FALSE, inner="B(10,0.3)", xlab="", ylim=c(0,0.3)) 
plot(X2, cex.points = 1, to.draw.arg=c("d"), mfColRow = FALSE, inner="B(20,0.3)", xlab="", ylim=c(0,0.3))
plot(X3, cex.points = 1, to.draw.arg=c("d"),  mfColRow = FALSE, inner="B(50,0.3)", xlab="", ylim=c(0,0.3))
# 
plot(Y1, cex.points = 1, to.draw.arg=c("d"), mfColRow = FALSE, inner="B(10,0.5)", xlab="", ylim=c(0,0.3)) 
plot(Y2, cex.points = 1, to.draw.arg=c("d"), mfColRow = FALSE, inner="B(20,0.5)", xlab="", ylim=c(0,0.3))
plot(Y3, cex.points = 1, to.draw.arg=c("d"),  mfColRow = FALSE, inner="B(50,0.5)", xlab="", ylim=c(0,0.3))
```

### (3-1) 포아송분포

```{r}
par(mfrow=c(1,2))
X = Pois(2)
# 확률질량함수
plot(X, cex.points = 1, to.draw.arg=c("d"), mfColRow = FALSE, inner=" ", xlab="", ylim=c(0,0.3))
# 누적분포함수
plot(X, cex.points = 1, to.draw.arg=c("p"), mfColRow = FALSE, inner=" ", xlab="")
```

### (3-2) 포아송분포 : n을 달리한 분포

```{r}
par(mfrow=c(1,3))
X1 = Pois(2)
X2 = Pois(5)
X3 = Pois(15)

# 확률질량함수
plot(X1, cex.points = 1, to.draw.arg=c("d"), mfColRow = FALSE, inner="Poisson(2)", xlab="", ylim=c(0,0.3)) 
plot(X2, cex.points = 1, to.draw.arg=c("d"), mfColRow = FALSE, inner="Poisson(5)", xlab="", ylim=c(0,0.3))
plot(X3, cex.points = 1, to.draw.arg=c("d"),  mfColRow = FALSE, inner="Poisson(15)", xlab="", ylim=c(0,0.3))
```

### (4-1) 초기하분포

```{r}
par(mfrow=c(1,2))
# N=4, n=2, D=2, N-D=2 -> Hyper(D, N-D, n)
N = 4; D=2; n=2
X = Hyper(D,N-D,n)  
# 확률질량함수
plot(X, cex.points = 1, to.draw.arg=c("d"), mfColRow = FALSE, inner=" ", xlab="", ylim=c(0,0.7))
# 누적분포함수
plot(X, cex.points = 1, to.draw.arg=c("p"), mfColRow = FALSE, inner=" ", xlab="")
```

### (5) R 실습

```{r}
library(distrEx)
print('1. 이항분포를 이용한 확률계산')
X=Binom(4,0.8)
d(X)(2) 
E(X) 
distrEx::var(X) 

print('2. 포아송분포를 이용한 확률계산')
X=Pois(1)
1-p(X)(2)
# 이항분포를 이용한 확률계산
X1=Binom(1000,0.001)
1-p(X1)(2)
E(X)
distrEx::var(X)

print('3. 초기하분포를 이용한 확률계산')
X=Hyper(2,2,2)
d(X)(0)
d(X)(1)
d(X)(2)
# 기댓값과 분산
E(X)
distrEx::var(X)
```

## 제 6 장

### (1) 연속형 균등분포

```{r}
library(distrEx)
par(mfrow=c(1,2))
 X = Unif(1,5)
# 확률밀도함수
plot(X, cex.points = 1, to.draw.arg=c("d"), mfColRow = FALSE, inner=" ", xlab="", ylim=c(0,0.5))
# 누적분포함수
plot(X, cex.points = 1, to.draw.arg=c("p"), mfColRow = FALSE, inner=" ", xlab="")
1-p(X)(2)
p(X)(4) - p(X)(2)
E(X)
distrEx::var(X)
```

### (2) 지수분포

```{r}
library(distrEx)
par(mfrow=c(1,2))
 X2 = Exp(1)
# 확률밀도함수
plot(X2, cex.points = 1, to.draw.arg=c("d"), mfColRow = FALSE, inner="Exp(1)", xlab="",  xlim=c(0, 10), ylim=c(0,1))
# 누적분포함수
plot(X2, cex.points = 1, to.draw.arg=c("p"), mfColRow = FALSE, inner="Exp(1)", xlab="",  xlim=c(0, 10), ylim=c(0,1))
```

```{r}
library(distrEx)
par(mfrow=c(1,3))
 X1 = Exp(0.5)
 X2 = Exp(1)
 X3 = Exp(2)
# 확률밀도함수
plot(X1, cex.points = 1, to.draw.arg=c("d"), mfColRow = FALSE, inner="Exp(0.5)", xlab="", xlim=c(0, 10), ylim=c(0,2))
plot(X2, cex.points = 1, to.draw.arg=c("d"), mfColRow = FALSE, inner="Exp(1)", xlab="",  xlim=c(0, 10), ylim=c(0,2))
plot(X3, cex.points = 1, to.draw.arg=c("d"), mfColRow = FALSE, inner="Exp(2) ", xlab="",  xlim=c(0, 10), ylim=c(0,2))

# 누적분포함수
plot(X1, cex.points = 1, to.draw.arg=c("p"), mfColRow = FALSE, inner="Exp(0.5)", xlab="", xlim=c(0, 10), ylim=c(0,1))
plot(X2, cex.points = 1, to.draw.arg=c("p"), mfColRow = FALSE, inner="Exp(1)", xlab="",  xlim=c(0, 10), ylim=c(0,1))
plot(X3, cex.points = 1, to.draw.arg=c("p"), mfColRow = FALSE, inner="Exp(2) ", xlab="",  xlim=c(0, 10), ylim=c(0,1))
```

### (3-1) 정규분포

```{r}
library(distrEx)
par(mfrow=c(1,2))
 X = Norm()
# 확률밀도함수
plot(X, cex.points = 1, to.draw.arg=c("d"), mfColRow = FALSE, inner="Norm(0,1)", xlab="",  xlim=c(-5, 5), ylim=c(0,0.5))
# 누적분포함수
plot(X, cex.points = 1, to.draw.arg=c("p"), mfColRow = FALSE, inner="Norm(0,1)", xlab="",  xlim=c(-5, 5), ylim=c(0,1))
```

```{r}
library(distrEx)
par(mfrow=c(2,3))
 X1 = Norm(0,sd=1)
 X2 = Norm(0,sd=0.5)
 X3 = Norm(0,sd=2)
# 확률밀도함수
plot(X1, cex.points = 1, to.draw.arg=c("d"), mfColRow = FALSE, 
     inner=FALSE, xlab="", xlim=c(-5, 5), ylim=c(0,1))
plot(X2, cex.points = 1, to.draw.arg=c("d"), mfColRow = FALSE,  
     inner=FALSE, xlab="",  xlim=c(-5, 5), ylim=c(0,1))
plot(X3, cex.points = 1, to.draw.arg=c("d"), mfColRow = FALSE, 
     inner=FALSE, xlab="",  xlim=c(-5, 5), ylim=c(0,1))

 Y1 = Norm(0,sd=1)
 Y2 = Norm(-1,sd=0.5)
 Y3 = Norm(1,sd=2)
# 확률밀도함수
plot(Y1, cex.points = 1, to.draw.arg=c("d"), mfColRow = FALSE, 
     inner=FALSE, xlab="", xlim=c(-5, 5), ylim=c(0,1))
plot(Y2, cex.points = 1, to.draw.arg=c("d"), mfColRow = FALSE,  
     inner=FALSE, xlab="",  xlim=c(-5, 5), ylim=c(0,1))
plot(Y3, cex.points = 1, to.draw.arg=c("d"), mfColRow = FALSE, 
     inner=FALSE, xlab="",  xlim=c(-5, 5), ylim=c(0,1))
```
### (4) 감마 분포

```{r}
library(distrEx)
par(mfrow=c(1,3))
G1 = Gammad(shape=1,scale=1) 
G2 = Gammad(shape=1,scale=1/2) 
G3 = Gammad(shape=2,scale=1) 

# 확률밀도함수
plot(G1, cex.points = 1, to.draw.arg=c("d"), mfColRow = FALSE, 
     inner=FALSE, xlab="", xlim=c(0, 12), ylim=c(0,1))
plot(G2, cex.points = 1, to.draw.arg=c("d"), mfColRow = FALSE,  
     inner=FALSE, xlab="",  xlim=c(0, 12), ylim=c(0,1))
plot(G3, cex.points = 1, to.draw.arg=c("d"), mfColRow = FALSE, 
     inner=FALSE, xlab="",  xlim=c(0, 12), ylim=c(0,1))
```

### (5) R 실습

```{r}
# 예제 6-9
X = Unif(1,5)
1-p(X)(2)
p(X)(4) - p(X)(2)
plot(X)
E(X)
var(X)
```
```{r}
# 예제 6-10
X = Exp(3)
p(X)(1)
plot(X)
E(X)
var(X)
```
```{r}
# 예제 6-11
X = Norm(15,5)
p(X)(25)
plot(X)
E(X)
var(X)
```
```{r}
# 예제 6-12
X = Gammad(2,1/3)
p(X)(1)
plot(X, inner=c("pdf of Gamma(2,3)", "cdf of Gamma(2,3)", "Quatile ftn of of Gamma(2,3)"))
E(X)
var(X)
```
## 제 7 장

### (1) 이변량 정규분포
```{r}
#install.packages("mvtnorm")
library(mvtnorm)
 x = seq(0,6, length=51)
 y = seq(-1,11, length=51)
 f = matrix(0, nrow=length(x), ncol=length(y))
 m = c(3,5)
 S = matrix(c(1,-1,-1,4), nrow=2, ncol=2)
# 결합확률밀도함수의 계산
 for (i in 1:length(x)) 
    for (j in 1:length(y))
           f[i,j] = dmvnorm(c(x[i], y[j]), mean=m, sigma=S)
# 3차원 그림 그리기
 persp(x, y, f)
```
### (2) 다항분포
```{r}
dmultinom(c(3,5,2), size=10, prob=c(0.25,0.5,0.25))
# 다항분포에서 난수 생성
rmultinom(n=5, size=10, prob=c(0.25,0.5,0.25))
```
## 제 8 장

### (1) 이항분포의 정규근사

```{r}
 par(mfrow=c(2,2))
 p=0.1
 X1 = Binom(5, p)
 X2 = Binom(10, p)
 X3 = Binom(30, p)
 X4 = Binom(100, p)
# 확률질량함수
plot(X1, cex.points = 1, to.draw.arg=c("d"), mfColRow = FALSE, inner="B(5,0.1)", xlab="") 
plot(X2, cex.points = 1, to.draw.arg=c("d"), mfColRow = FALSE, inner="B(10,0.1)", xlab="")
plot(X3, cex.points = 1, to.draw.arg=c("d"),  mfColRow = FALSE, inner="B(30,0.1)", xlab="")
plot(X4, cex.points = 1, to.draw.arg=c("d"),  mfColRow = FALSE, inner="B(100,0.1)", xlab="")

p=0.5
 X1 = Binom(5, p)
 X2 = Binom(10, p)
 X3 = Binom(30, p)
 X4 = Binom(100, p)
# 확률질량함수
plot(X1, cex.points = 1, to.draw.arg=c("d"), mfColRow = FALSE, inner="B(5,0.5)", xlab="") 
plot(X2, cex.points = 1, to.draw.arg=c("d"), mfColRow = FALSE, inner="B(10,0.5)", xlab="")
plot(X3, cex.points = 1, to.draw.arg=c("d"),  mfColRow = FALSE, inner="B(30,0.5)", xlab="")
plot(X4, cex.points = 1, to.draw.arg=c("d"),  mfColRow = FALSE, inner="B(100,0.5)", xlab="")

```
### (2) 대수의 법칙
```{r}
#install.packages("distrTeach")
library(distrTeach)
illustrateLLN(Distr = Unif(), main=NULL, withCover = F)
```
### (3) 대수의 법칙
```{r}
#install.packages("TeachingDemos")
library(TeachingDemos)
clt.examp(2, nclass=20)
clt.examp(5, nclass=20)
clt.examp(10, nclass=20)
clt.examp(30, nclass=20)
```
## 9장

### (1) KOSPI 그래프

```{r}
library(xts)
library(quantmod)
getSymbols('^KS11')
 chartSeries(KS11, type="candlesticks", theme=chartTheme('white'), TA=NULL, minor.ticks=FALSE, subset='last 3 months')
```


### (2) 2단계 전이확률행렬의 계산
```{r}
P = matrix(c(0.4, 0.5, 0.6, 0.5), nrow=2)
P2 = P %*% P
P2
```

```{r}
library(fOptions)
CRRTree = BinomialTreeOption(TypeFlag = "pe", S = 1000, X = 1050,Time = 0.5, r = 0.06, b = 0.06, sigma = 0.10, n = 2)
BinomialTreePlot(CRRTree, cex = 2, xlim=c(0.5,3.5), xlab = " ", 
                   ylab = " ")
```
## 10 장

### (1) 연속형 균등분포 난수의 생성

```{r}
set.seed(1234567)
hist(runif(100), xlab="", main="n=100", freq =FALSE, 
     ylim=c(0, 1.2), breaks = c(0,0.2,0.4,0.6,0.8,1.0), 
     col="steelblue")
hist(runif(10000), xlab="", main="n=10,000", freq =FALSE, 
     ylim=c(0, 1.2), breaks = c(0,0.2,0.4,0.6,0.8,1.0), 
     col="steelblue")
```

### (2) 역함수를 이용한 지수분포 난수의 생성

```{r}
set.seed(1234567)
n = 10000
u1 = runif(n)
e1 = -log(1-u1)

hist(e1, freq=FALSE, breaks=40, ylim=c(0, 1), 
     xlab="", main="", col="steelblue")
 curve(dexp(x,1), xlim = c(0, 7), add=TRUE, col=2, lwd=2)
```

### (3) 기각법에 의한 난수 생성

```{r}
set.seed(1234567)
n = 10000       
x1 = rep(NA, n)  

ii = 1
while (ii <= n) {
  y1 = rexp(1) 
  ratio = exp(-(y1-1)^2/2) 
  u1 = runif(1)
  u2 = runif(1)   
  if(u1 < ratio){ 
    x1[ii] = ifelse(u2 > 0.5, y1, -y1)
    ii = ii + 1
  }
}
hist(x1, freq = FALSE, breaks=40, xlab="", main="", 
     col="steelblue", xlim=c(-4,4), ylim=c(0, 0.4))
 curve(dnorm(x), add=TRUE, 
       col=2,  lwd=2)
```

### (4) 극좌표변환을 이용한 정규분포 난수의 생성

```{r}
set.seed(1234567)
u1 = runif(10000)
u2 = runif(10000)
x1 = sqrt(-2*log(u1))*cos(2*pi*u2)
x2 = sqrt(-2*log(u1))*sin(2*pi*u2)
hist(x1, freq=FALSE, xlim=c(-4,4), breaks=40,ylim=c(0,0.4), 
       xlab="X1", main="", col="steelblue")
 curve(dnorm(x), add=TRUE, col=2, lwd=2)
 
hist(x2, freq=FALSE, xlim=c(-4,4), breaks=40,ylim=c(0,0.4), 
        xlab="X2", main="", col="steelblue")
 curve(dnorm(x), add=TRUE, col=2, lwd=2)
```

### (5) 이항분포 난수의 생성

```{r}
set.seed(1234567) 
# 이항분포 B(10, 0.25) 생성
bn1 = rep(NA,1000)
 for(i in 1:1000) {bn1[i] = sum(runif(10) < 0.25)}
 hist(bn1, freq=FALSE, xlab="", main="B(10, 0.25)", 
      col="steelblue")

 # 이항분포 B(100, 0.25) 생성
bn2 = rep(NA,1000)
for(i in 1:1000) {bn2[i] = sum(runif(100)<0.25)}

hist(bn2, freq=FALSE, xlab="", main="B(100, 0.25)", 
     col="steelblue")
```

### (6) 포아송분포 난수의 생성

```{r}
set.seed(1234567)

poisson = function(lambda) {
  el = exp(-lambda)
  Y = 0
  up = 1
  repeat {
    Y = Y + 1
    up = up * runif(1)
    if (up <= el) {
      break
    }
  }
  return(Y-1)
}

n = 1000
lambda1 = 3
lambda2 = 10
Y1 = rep(NA, n)
Y2 = rep(NA, n)

for (i in 1:n){Y1[i] = poisson(lambda1)}
for (i in 1:n){Y2[i] = poisson(lambda2)}

hist(Y1, freq=FALSE, xlab="", main="Poisson(3)", 
     col="steelblue")
hist(Y2, freq=FALSE, xlab="", main="Poisson(10)", 
     col="steelblue")

```

```{r}
set.seed(1234567)

poisson = function(lambda) {
  el = exp(-lambda)
  Y = 0
  up = 1
  while(up >= el) {
    Y = Y + 1
    up = up * runif(1)
  }
  return(Y-1)
}

n = 10000
lambda1 = 3
lambda2 = 10
Y1 = rep(NA, n)
Y2 = rep(NA, n)

for (i in 1:n){Y1[i] = poisson(lambda1)}
for (i in 1:n){Y2[i] = poisson(lambda2)}

hist(Y1, freq=FALSE, xlab="", main="Poisson(3)", 
     col="steelblue")
hist(Y2, freq=FALSE, xlab="", main="Poisson(10)", 
     col="steelblue")

```

### (7) 원주율의 계산

```{r}
set.seed(1234567) 
 pical = function(n){
    u1 = runif(n, -1, 1)
    u2 = runif(n, -1, 1)
    x1 = rep(0,n)
    x1[u1^2 + u2^2 <= 1] = 1
    pi1 = mean(x1)*4
    return(pi1)
 }
pical(100) 
pical(10000) 
pical(1000000)
```

### (8) 함수의 면적 계산

```{r}
par(mfrow=c(1,2))
set.seed(1234567) 
 
rint = function(f,gg,n){
  sam = matrix(runif(2*n), ncol=2)
  qq = gg(sam[,1], sam[,2])

plot(sam[!qq,1], sam[!qq,2], col='steelblue', pch=1, 
       xlab="", ylab="", xlim=c(0,1), ylim=c(0,1))
  points(sam[qq,1], sam[qq,2], col='gray', pch=1)
 curve(f, 0,1, n=100, col='black', add=TRUE, lwd=2)
   return(length(qq[qq]) / n)
 }
 
f1 = function(x) x^3
g1 = function(x,y) y <= x^3

f2 = function(x) sqrt(1-x^2)*x
g2 = function(x,y) y <= sqrt(1 - x^2)*x

integrate(f1,0,1)
rint(f1, g1, 100)
rint(f1, g1, 10000)

integrate(f2,0,1)
rint(f2, g2, 100)
rint(f2, g2, 10000)
```
