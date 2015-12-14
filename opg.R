# Opg 1.1
data <- read.table("avit.txt", header=TRUE)
nm <- sum(data$sex==1)
nk <- sum(data$sex==2)

avitM = data$avit[data$sex==1]
logAvitM = log(avitM)

# Opg 1.2
medianAvitM = median(avitM)
meanAvitM = mean(avitM)
varAvitM = var(avitM)
sdAvitM = sd(avitM)

medianLogAvitM = median(logAvitM)
meanLogAvitM = mean(logAvitM)
varLogAvitM = var(logAvitM)
sdLogAvitM = sd(logAvitM)

# Opg 1.3
plot <- 1
if (plot==1) {
    pdf("opg13_1.pdf", height=6, width=6)
    hist(avitM, breaks=30, prob=TRUE, main="Histogram af avitM")
    x1 <- seq(0, max(avitM), max(avitM)/nm)
    f1 <- dnorm(x1, mean=meanAvitM, sd=sdAvitM)
    lines(x1, f1, col="red")
    dev.off()
    
    pdf("opg13_2.pdf", height=6, width=6)
    hist(logAvitM, breaks=30, prob=TRUE, main="Histogram af log(avitM)")
    x2 <- seq(0, max(logAvitM), max(logAvitM)/nm)
    f2 <- dnorm(x2, mean=meanLogAvitM, sd=sdLogAvitM)
    lines(x2, f2, col="red")
    dev.off()
}
# Opg 1.4
pnorm(2000, meanAvitM, sdAvitM)

# Opg 1.6
f <- function(y, s, u) {
    r1 <- 1 / (y * sqrt(2 * pi * s^2))
    r2 <- exp(-(log(y) - u)^2 / (2*s^2))
    return (r1*r2)
}

if (plot==1) {
    pdf("opg16.pdf", height=6, width=6)
    hist(avitM, breaks=30, prob=TRUE, main="Histogram af avitM")
    x1 <- seq(0, max(avitM), max(avitM)/nm)
    f1 <- f(x1, sdLogAvitM, meanLogAvitM)
    lines(x1, f1, col="red")
    dev.off()
}

# Opg 2.8
G <- function(y) {
    if (0 < y && y <= 1) {
        r <- y
    } else if (1 < y) {
        r <- y^(-3)
    } else {
        r <- 0
    }
    return(r)
}

Ginv <- function(y) {
    if (0 < y && y <= 0.5) {
        r <- sqrt(2*y)
    } else if (0.5 < y) {
        r <- 1 / sqrt(2-2*y)
    } else {
        r <- 0
    }
    return(r)
}

Y <- runif(10000, 0, 1)
GinvY <- sapply(Y, Ginv, simplify=array)
x1 <- seq(from=0, to=100, length.out=10000)
G1 <- sapply(x1, G, simplify=array)
mean(GinvY)

if (plot==1) {
    pdf("opg28_1.pdf", height=6, width=6)
    hist(GinvY, prob=TRUE, breaks=100, main="Histogram af 10000 simuleringer af Y")
    dev.off()

    pdf("opg28_2.pdf", height=6, width=6)
    hist(GinvY, prob=TRUE, breaks=max(GinvY*4), xlim=c(0,6), ylim=c(0,1), main="Histogram af 10000 simuleringer af Y")
    lines(x1, G1, col="red")
    dev.off()
}
