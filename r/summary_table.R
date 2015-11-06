# analyze data for gss year 2

library(dplyr)
library(ggplot2)
library(dplyr)
library(VGAM)
library(xtable)

# generates an mlsn value for a vector

mlsn <- function(x) {
  fit.x <- vglm(x ~ 1, fisk)
  z <- Coef(fit.x)
  new.mlsn <- qfisk(0.1, z[1], z[2])
  return(round(new.mlsn, 0))
}

# calculates values for a parameter for use in summary table
sum.x <- function(x) {
  Count <- length(x)
  Minimum <- min(x)
  Median <- median(x)
  Mean <- mean(x)
  Maximum <- max(x)
  MLSN <- mlsn(x)
  return(c(Count, Minimum, Median, Mean, Maximum, MLSN))
}

# work with the GSS data through 31 Aug 2015
# for the GSS annual report prepared in Nov 2015

gss <- read.csv("data/20151011_gss.csv",
                header = TRUE)

# make a cut for Ca only, to eliminate apparent calcareous at CaM3 > 3000
# this is <= 3000 or pH <= 7.7; the opposite, or what is filtered out, must
# satisfy both > 3000 and pH > 7.7
gssCa <- filter(gss, CaM3 <= 3000 | pH <= 7.7)

# make a table that can be printed pretty in report to summarize

# sum.x function will calculate n, min, max, mean, median, calculated MLSN
# do this for a table to include pH, OM, K, P, Ca, Mg, S

pH <- sum.x(gss$pH)
pH[4] <- "NA"
pH[6] <- "NA"
as.integer(pH)
OM <- prettyNum(sum.x(gss$OM360), digits = 2)
OM[6] <- "NA"
K <- sum.x(gss$KM3)
P <- sum.x(gss$PM3)
Ca <- sum.x(gssCa$CaM3)
Mg <- sum.x(gss$MgM3)
S <- sum.x(gss$SM3)

parameters <- c("pH", "OM %", "K ppm",
                "P ppm", "Ca ppm", "Mg ppm", "S ppm")

sum.table <- as.data.frame(rbind(pH, OM, K, P, Ca, Mg, S))

sum.table <- cbind(parameters, sum.table)

colnames(sum.table) <- c("Soil parameter", "n", "Min", 
                         "Median", "Mean", "Max", "GSS")

sum.table$Mean <- round(as.numeric(as.character(sum.table$Mean)), 0)
sum.table$Mean <- as.factor(sum.table$Mean)
sum.table$GSS <- as.numeric(as.character(sum.table$GSS))