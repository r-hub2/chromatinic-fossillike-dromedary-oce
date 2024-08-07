## ----echo=FALSE---------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(oce)

## ----eval=FALSE---------------------------------------------------------------
#  d <- read.oce(f)

## ----eval=FALSE---------------------------------------------------------------
#  d <- read.adp(f)

## ----eval=FALSE---------------------------------------------------------------
#  f <- "/data/archive/sleiwex/2008/moorings/m09/adp/rdi_2615/raw/adp_rdi_2615.000"
#  dall <- read.oce(f)

## ----eval=FALSE---------------------------------------------------------------
#  d100 <- read.oce(f, by = 100)

## ----eval=FALSE---------------------------------------------------------------
#  read.oce(f,
#      from = as.POSIXct("2008-06-26", tz = "UTC"),
#      to = as.POSIXct("2008-06-27", tz = "UTC"),
#      by = "60:00",
#      latitude = 47.88126, longitude = -69.73433
#  )

## ----results="hide"-----------------------------------------------------------
data(adp)
summary(adp)

## ----eval=FALSE---------------------------------------------------------------
#  beam <- read.oce(f)
#  xyx <- beamToXyz(beam)
#  enu <- xyzToEnu(xyz, declination = -18.1)

## ----fig.height=7, fig.width=4.5, dev.args=list(pointsize=13)-----------------
plot(adp)

## ----fig.height=7, fig.width=4.5, dev.args=list(pointsize=13)-----------------
plot(subset(adp, distance < 20))

## -----------------------------------------------------------------------------
time <- adp[["time"]]
distance <- adp[["distance"]]

## -----------------------------------------------------------------------------
v <- adp[["v"]]

## -----------------------------------------------------------------------------
a <- adp[["a", "numeric"]]

## ----results="hide"-----------------------------------------------------------
sort(names(adp[["metadata"]]))

## -----------------------------------------------------------------------------
adp[["originalCoordinate"]]
adp[["oceCoordinate"]]

## -----------------------------------------------------------------------------
processingLogShow(adp)

## ----fig.width=3, fig.height=3, dev.args=list(pointsize=9)--------------------
plot(adp, which = "uv")

## ----eval=FALSE, message=FALSE, warning=FALSE, error=FALSE--------------------
#  library(oce)
#  adcp <- read.adp("COR2019002_20190818T064815_007_000000.ENS")
#  enu <- toEnu(adcp)
#  removeShipSpeed <- subtractBottomVelocity(enu)
#  plot(removeShipSpeed, which = 1:3)

## ----eval=FALSE---------------------------------------------------------------
#  plot(subset(adp, time < median(adp[["time"]])))

## ----fig.height=2, dev.args=list(pointsize=9)---------------------------------
time <- adp[["time"]]
v <- adp[["v"]]
# The second index is for bin number, the third for beam number
midIndex <- dim(v)[2] / 2
eastMid <- v[, midIndex, 1] # third index is beam
distance <- adp[["distance"]][midIndex]
oce.plot.ts(time, eastMid, ylab = "Eastward velocity [m/s]")
# Depth mean; note that na.rm, is passed by apply() to mean()
eastMean <- apply(v[, , 1], 1, mean, na.rm = TRUE)
lines(time, eastMean, col = 2)

## -----------------------------------------------------------------------------
u <- adp[["v"]][, , 1]
v <- adp[["v"]][, , 2]
ok <- is.finite(u) & is.finite(v) # remove NA values
u <- u[ok]
v <- v[ok]
eigen(cov(data.frame(u, v)))

## -----------------------------------------------------------------------------
pr <- prcomp(data.frame(u, v))

## -----------------------------------------------------------------------------
pr

## ----fig.height=2, dev.args=list(pointsize=9)---------------------------------
time <- adp[["time"]]
pressure <- adp[["pressure"]]
oce.plot.ts(time, pressure)

## -----------------------------------------------------------------------------
m <- tidem(as.sealevel(pressure, time))

## -----------------------------------------------------------------------------
summary(m)

## ----fig.height=2, dev.args=list(pointsize=9)---------------------------------
oce.plot.ts(time, pressure, type = "p", col = "blue")
timePredict <- seq(min(time), max(time), length.out = 200)
pressurePredict <- predict(m, timePredict)
lines(timePredict, pressurePredict, col = "red")

