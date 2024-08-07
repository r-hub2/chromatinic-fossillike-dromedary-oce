# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Solar Angle as Function of Space and Time
#'
#' This calculates solar angle, based on a NASA-provided Fortran
#' program, which (according to comments in the code) is in turn
#' based on "The Astronomical Almanac".
#' Note that `time` may be a single value or a vector
#' of values; in the latter case, `longitude`, `latitude` and `useRefraction`
#' are all made to be of the same length as `time`, by calling [rep()]. This
#' addresses the case of finding a time-series of angles at a given spatial
#' location. For other cases of arguments that are not single values,
#' either call `sunAngle()` repeatedly or, if that is too slow,
#' use `expand.grid()` to set up values of uniform length that
#' are then supplied to `sunAngle()`.
#'
#' @param t time, either a POSIXt object (converted to timezone `"UTC"`,
#' if it is not already in that timezone), or a value (character or numeric)
#' that can be converted to a time with [as.POSIXct()], assuming the
#' timezone to be `"UTC"`.
#'
#' @param longitude observer longitude in degrees east.
#'
#' @param latitude observer latitude in degrees north.
#'
#' @param useRefraction boolean, set to `TRUE` to apply a correction for
#' atmospheric refraction.
#'
#' @return A list containing the following:
#' * `time` the time
#' * `azimuth`, in degrees eastward of north, from 0 to 360.
#' * `altitude`, in degrees above the horizon,  ranging from -90 to 90.
#' * `diameter`, solar diameter, in degrees.
#' * `distance` to sun, in astronomical units.
#' * `declination` angle in degrees, computed with [sunDeclinationRightAscension()].
#' * `rightAscension` angle in degrees, computed with [sunDeclinationRightAscension()].
#' \if{html}{\figure{starCoords.png}{options: width="400"}}
#'
#' @seealso The corresponding function for the moon is [moonAngle()].
#'
#' @references Regarding `declination` and `rightAscension`, see
#' references in the documentation for [sunDeclinationRightAscension()].
#' The other items are based on Fortran code retrieved from
#' the file `sunae.f`, downloaded from the ftp site
#' \code{climate1.gsfc.nasa.gov/wiscombe/Solar_Rad/SunAngles}
#' on 2009-11-1.  Comments in that code list as references:
#'
#' Michalsky, J., 1988: The Astronomical Almanac's algorithm for approximate
#' solar position (1950-2050), Solar Energy 40, 227-235
#'
#' The Astronomical Almanac, U.S. Gov't Printing Office, Washington, D.C.
#' (published every year).
#'
#' The code comments suggest that the appendix in Michalsky (1988) contains
#' errors, and declares the use of the following formulae in the 1995 version
#' the Almanac:
#' * p. A12: approximation to sunrise/set times
#' * p. B61: solar altitude (AKA elevation) and azimuth
#' * p. B62: refraction correction
#' * p. C24: mean longitude, mean anomaly, ecliptic
#' longitude, obliquity of ecliptic, right ascension, declination, Earth-Sun
#' distance, angular diameter of Sun
#' * p. L2: Greenwich mean sidereal time (ignoring T^2, T^3 terms)
#'
#' The code lists authors as Dr. Joe Michalsky and Dr. Lee Harrison (State
#' University of New York), with modifications by Dr. Warren Wiscombe (NASA
#' Goddard Space Flight Center).
#'
#' @examples
#'
#' rise <- as.POSIXct("2011-03-03 06:49:00", tz = "UTC") + 4 * 3600
#' set <- as.POSIXct("2011-03-03 18:04:00", tz = "UTC") + 4 * 3600
#' mismatch <- function(lonlat) {
#'     sunAngle(rise, lonlat[1], lonlat[2])$altitude^2 + sunAngle(set, lonlat[1], lonlat[2])$altitude^2
#' }
#' result <- optim(c(1, 1), mismatch)
#' lonHfx <- (-63.55274)
#' latHfx <- 44.65
#' dist <- geodDist(result$par[1], result$par[2], lonHfx, latHfx)
#' cat(sprintf(
#'     "Infer Halifax latitude %.2f and longitude %.2f; distance mismatch %.0f km",
#'     result$par[2], result$par[1], dist
#' ))
#'
#' @family things related to astronomy
#'
#' @author Dan Kelley
sunAngle <- function(t, longitude = 0.0, latitude = 0.0, useRefraction = FALSE) {
    if (missing(t)) {
        stop("must provide t")
    } else {
        if (is.character(t)) {
            t <- as.POSIXct(t, tz = "UTC")
        }
        if (inherits(t, "Date")) {
            t <- as.POSIXct(t)
        }
        if (!inherits(t, "POSIXt")) {
            if (is.numeric(t)) {
                tref <- as.POSIXct("2000-01-01 00:00:00", tz = "UTC") # arbitrary
                t <- t - as.numeric(tref) + tref
            } else {
                stop("t must be POSIXt or a number corresponding to POSIXt (in UTC)")
            }
        }
    }
    if (!is.logical(useRefraction)) {
        stop("useRefraction must be a logical value")
    }
    t <- as.POSIXct(t) # so length() will return what we want
    nt <- length(t)
    nlongitude <- length(longitude)
    nlatitude <- length(latitude)
    nuseRefraction <- length(useRefraction)
    if (nlongitude != nt) {
        longitude <- rep(longitude, length.out = nt)
    }
    if (nlatitude != nt) {
        latitude <- rep(latitude, length.out = nt)
    }
    if (nuseRefraction != nt) {
        useRefraction <- rep(useRefraction, length.out = nt)
    }
    # Ensure that the timezone is UTC. Note that Sys.Date() gives a NULL tzone.
    tzone <- attr(as.POSIXct(t[1]), "tzone")
    if (is.null(tzone) || "UTC" != tzone) {
        attributes(t)$tzone <- "UTC"
    }
    ok <- is.finite(t)
    if (!all(ok)) {
        warning("removing ", sum(!ok), " data, for which time is not finite")
        t <- t[ok]
        latitude <- latitude[ok]
        longitude <- longitude[ok]
        useRefraction <- useRefraction[ok]
    }
    # the code below is derived from fortran code, downloaded 2009-11-1 from
    # ftp://climate1.gsfc.nasa.gov/wiscombe/Solar_Rad/SunAngles/sunae.f
    t <- as.POSIXlt(t) # use this so we can work on hours, etc
    if ("UTC" != attr(as.POSIXct(t[1]), "tzone")) {
        stop("t must be in UTC")
    }
    year <- t$year + 1900
    if (any(year < 1950) || any(year > 2050)) {
        warning("year=", year[year < 1950 | year > 2050][1], " (and possibly others) is outside the acceptable range of 1950-2050")
    }
    day <- t$yday + 1
    if (any(day < 1) || any(day > 366)) {
        stop("day is not in range 1 to 366")
    }
    hour <- t$hour + t$min / 60 + t$sec / 3600
    if (any(hour < -13) || any(hour > 36)) {
        stop("hour outside range -13 to 36")
    }
    if (any(latitude < -90)) {
        warning("latitude(s) trimmed to range -90 to 90")
        latitude[latitude < -90] <- -90
    }
    if (any(latitude > 90)) {
        warning("latitude(s) trimmed to range -90 to 90")
        latitude[latitude > 90] <- 90
    }
    if (any(longitude < -180)) {
        warning("longitude(s) trimmed to range -180 to 180")
        longitude[longitude < -180] <- -180
    }
    if (any(longitude > 180)) {
        warning("longitude(s) trimmed to range -180 to 180")
        longitude[longitude > 180] <- 180
    }
    delta <- year - 1949
    leap <- delta %/% 4
    # IXME: using fortran-style int and mod here; must check for leap-year cases
    jd <- 32916.5 + (delta * 365 + leap + day) + hour / 24
    jd <- jd + ifelse(0 == (year %% 100) & 0 != (year %% 400), 1, 0)
    time <- jd - 51545
    mnlong <- 280.460 + 0.9856474 * time
    mnlong <- mnlong %% 360
    mnlong <- mnlong + ifelse(mnlong < 0, 360, 0)
    mnanom <- 357.528 + 0.9856003 * time
    mnanom <- mnanom %% 360
    mnanom <- mnanom + ifelse(mnanom < 0, 360, 0)
    rpd <- pi / 180
    mnanom <- mnanom * rpd
    eclong <- mnlong + 1.915 * sin(mnanom) + 0.020 * sin(2 * mnanom)
    eclong <- eclong %% 360
    eclong <- eclong + ifelse(eclong < 0, 360, 0)
    oblqec <- 23.439 - 0.0000004 * time
    eclong <- eclong * rpd
    oblqec <- oblqec * rpd
    num <- cos(oblqec) * sin(eclong)
    den <- cos(eclong)
    ra <- atan(num / den)
    ra <- ra + ifelse(den < 0, pi, ifelse(num < 0, 2 * pi, 0))
    dec <- asin(sin(oblqec) * sin(eclong))
    gmst <- 6.697375 + 0.0657098242 * time + hour
    gmst <- gmst %% 24
    gmst <- gmst + ifelse(gmst < 0, 24, 0)
    lmst <- gmst + longitude / 15
    lmst <- lmst %% 24
    lmst <- lmst + ifelse(lmst < 0, 24, 0)
    lmst <- lmst * 15 * rpd
    ha <- lmst - ra
    ha <- ha + ifelse(ha < (-pi), 2 * pi, 0)
    ha <- ha - ifelse(ha > pi, 2 * pi, 0)
    el <- asin(sin(dec) * sin(latitude * rpd) + cos(dec) * cos(latitude * rpd) * cos(ha))
    # pin the arg to range -1 to 1 (issue 1004)
    sinAz <- -cos(dec) * sin(ha) / cos(el)
    az <- ifelse(sinAz < (-1), -pi / 2,
        ifelse(sinAz > 1, pi / 2,
            asin(sinAz)
        )
    )
    az <- ifelse(sin(dec) - sin(el) * sin(latitude * rpd) > 0,
        ifelse(sin(az) < 0, az + 2 * pi, az),
        pi - az
    )
    el <- el / rpd
    az <- az / rpd
    el <- el + ifelse(useRefraction,
        ifelse(el >= 19.225,
            0.00452 * 3.51823 / tan(el * rpd),
            ifelse(el > (-0.766) & el < 19.225,
                3.51823 * (0.1594 + el * (0.0196 + 0.00002 * el)) / (1 + el * (0.505 + 0.0845 * el)),
                0
            )
        ),
        0
    )
    soldst <- 1.00014 - 0.01671 * cos(mnanom) - 0.00014 * cos(2 * mnanom)
    soldia <- 0.5332 / soldst
    sunDRA <- sunDeclinationRightAscension(t, apparent = FALSE)
    list(
        time = t, azimuth = az, altitude = el, diameter = soldia, distance = soldst,
        declination = sunDRA$declination, rightAscension = sunDRA$rightAscension
    )
}

#' Sun Declination and Right Ascension
#'
#' The formulae are from Meeus (1991), chapter 24 (which uses chapter 21).
#'
#' @param time a POSIXct time. This ought to be in UTC timezone; if not,
#' the behaviour of this function is unlikely to be correct.
#'
#' @param apparent logical value indicating whether to return
#' the 'apparent' angles.
#'
#' @return A list containing `declination` and `rightAscension`, in degrees.
#'
#' @examples
#' # Example 24.a in Meeus (1991) (page 158 PDF, 153 print)
#' time <- as.POSIXct("1992-10-13 00:00:00", tz = "UTC")
#' a <- sunDeclinationRightAscension(time, apparent = TRUE)
#' stopifnot(abs(a$declination - (-7.78507)) < 0.00004)
#' stopifnot(abs(a$rightAscension - (-161.61919)) < 0.00003)
#' b <- sunDeclinationRightAscension(time)
#' # check against previous results, to protect aginst code-drift errors
#' stopifnot(abs(b$declination - (-7.785464443)) < 0.000000001)
#' stopifnot(abs(b$rightAscension - (-161.6183305)) < 0.0000001)
#'
#' @references
#' * Meeus, Jean. Astronomical Algorithms. Second Edition.
#' Richmond, Virginia, USA: Willmann-Bell, 1991.
#'
#' @family things related to astronomy
#' @author Dan Kelley, based on formulae in Meeus (1991).
sunDeclinationRightAscension <- function(time, apparent = FALSE) {
    k <- 2 * pi / 360 # k*degrees == radians
    # Meeus 1991 pdf page 158, print page 153
    JD <- julianDay(time)
    # nolint start T_and_F_symbol_lintr
    T <- (JD - 2451545.0) / 36525 # Meeus (1991) eq (24.1)
    # L0 = geometric mean longitude of sun, referred to mean equinox of date
    L0 <- 280.46645 + 36000.76983 * T + 0.0003032 * T^2 # Meeus (1991) eq (24.2)
    L0 <- L0 %% 360
    # mean anomaly of sun
    M <- 357.52910 + 35999.05030 * T - 0.0001558 * T^2 - 0.00000048 * T^3
    M <- M %% 360
    # e = eccentricity of earth's orbit, Meeus (1991) first unnumbered eqn after (24.4)
    #<UNUSED> e <- 0.016708617 - 0.000042037*T - 0.0000001236*T^2
    # sun equation of center C
    C <- (1.914600 - 0.004817 * T - 0.000014 * T^2) * sin(k * M) + (0.019993 - 0.000101 * T) * sin(k * 2 * M) + 0.000290 * sin(3 * k * M)
    # sun true longitude: Meeus (1991) second eqn after eqn (24.4)
    Theta <- L0 + C
    # sun true anomaly
    #<UNUSED> nu <- M + C
    #> sun radius vector (dist earth to sun, in AU)
    #<UNUSED> R <- (1.000001018 * (1 - e^2)) / (1 + e * cos(k * nu))
    # first unnumbered eqn after Meeus (1991) eqn (24.5)
    Omega <- 125.04 - 1934.136 * T
    # second unnumbered eqn after Meeus (1991) eqn (24.5)
    lambda <- Theta - 0.00569 - 0.00478 * sin(k * Omega)
    #<UNUSED> Theta2000 <- Theta - 0.01397 * (year - 2000)
    # epsilon: Meeus (1991) eqn (21.1) (PDF page 140, print page 135)
    # mean obliquity of ecliptic
    epsilon0 <- 23 + (26 + 21.448 / 60) / 60 - 46.8150 / 60^2 * T - 0.00059 / 60^2 * T^2 + 0.001813 / 60^2 * T^3
    L <- 280.4665 + 36000.7698 * T # Meeus (1991) PDF page 137, print page 132
    Lprime <- 218.3165 + 481267.8813 * T # Meeus (1991) PDF page 137, print page 132
    # NOT same Omega as above, but we are following the trail of equations step by step,
    # and the following actually passes the test above so perhaps the previous eqn for
    # Omega was an approximation.
    # Omega <- 125.04452 - 1934.136261*T + 0.0020708*T^2 + T^3/450000 # Meeus (1991) PDF page 137, print page 132
    # But he then says to drop the T^2 and T^3 terms before giving the DeltaEpsilon eqn, so we do that.
    Omega <- 125.04452 - 1934.136261 * T # Meeus (1991) PDF page 137, print page 132
    # nolint end T_and_F_symbol_lintr
    DeltaEpsilon <- 9.20 / 60^2 * cos(k * Omega) + 0.57 / 60^2 * cos(k * 2 * L) + 0.10 / 60^2 * cos(k * 2 * Lprime) - 0.09 / 60^2 * cos(k * 2 * Omega)
    epsilon <- epsilon0 + DeltaEpsilon
    # alpha Meeus (1991)  eqn (24.6) PDF page 158, print page 153
    if (apparent) {
        epsilonA <- epsilon + 0.00256 * cos(k * Omega)
        alpha <- 1 / k * atan2(cos(k * epsilonA) * sin(k * lambda), cos(k * lambda))
        delta <- 1 / k * asin(sin(k * epsilonA) * sin(k * lambda)) # Meeus (1991) eqn (24.7) (PDF 1ige 58 print page 153)
    } else {
        alpha <- 1 / k * atan2(cos(k * epsilon) * sin(k * Theta), cos(k * Theta))
        delta <- 1 / k * asin(sin(k * epsilon) * sin(k * Theta)) # Meeus (1991) eqn (24.7) (PDF 1ige 58 print page 153)
    }
    list(declination = delta, rightAscension = alpha)
}
