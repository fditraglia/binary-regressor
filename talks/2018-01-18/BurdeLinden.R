setwd("~/binary-regressor/examples/Afghanistan/")
library(foreign)
kids <- read.dta("afghanistan_anonymized_data.dta")

labels <- attributes(kids)$var.labels
labels <- cbind(names(kids), labels)
labels

# Note: there are some mistakes in the data dictionary!


# The variable "treatment" is actually our instrument
# --- living in a village that gets a school
z <- kids$treatment

# The variable "f08_formal_school" is our treatment
# --- enrolled in formal school, Spring 2008
Tobs <- kids$s08_formal_school

# The variable "s08_formal_school" is our outcome
# --- total normalized test score, Spring 2008
y <- kids$s08_both_norma_total

dat <- data.frame(Tobs, y, z)
dat <- na.omit(dat)
dat0 <- subset(dat, z == 0)
dat1 <- subset(dat, z == 1)
rm(y, Tobs, z)

table(dat0$Tobs)
table(dat1$Tobs)

par(mfrow = c(1,2))
hist(dat0$y)
hist(dat1$y)
par(mfrow = c(1,1))

with(dat, plot(density(y[Tobs == 0 & z == 0])))
with(dat, plot(density(y[Tobs == 1 & z == 0])))
with(dat, plot(density(y[Tobs == 1 & z == 1])))
with(dat, plot(density(y[Tobs == 0 & z == 1])))
with(dat, hist(y[Tobs == 1 & z == 1]))
