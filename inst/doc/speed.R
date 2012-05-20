# Example for comparing speed of
#    R-2.9.2
#    R-2.9.2 + library(dataframe) (version 2.1)
#    R-2.14.2
#    R-2.14.2 + library(dataframe) (version 2.3 without byte compilation)
#    R-2.14.2 + library(dataframe) (version 2.3 with byte compilation)

# The R-2.9.2 tests were run about 2009-10-1.
# The R-2.14.2 tests were run 2012-03-14 on the same machine,
# a Dell T3400 running a Google version of Ubuntu.

#--------------------------------------------------
# Summary

#                      -------  seconds (multiple repetitions) -------
#                      creation/column subscripting     row subscripting
# R-2.9.2            : 34.2 43.9 43.3                   10.6 13.0
# R-2.9.2+(2.1)      : 22.5 21.8 21.8                    9.7  9.5  9.5
# R-2.14.2           : 11.2 11.1 11.3                    3.6  3.6  3.5
# R-2.14.2+(2.3)     : 10.0  9.7  9.6                    3.2  3.2  3.2
# R-2.14.2+(2.3bc)   :  8.9  8.8  8.9                    3.0  3.1  3.1

# Start R using 'R --vanilla'
# Numbers show are "user" times.
# All timings are after the code has been run once, to cache functions.
# This uses a standard version of R (without --enable-memory-profiling).

n <- 10^4
y <- 1:n + 0.0
z <- 1:n + 0i
Z <- list(z=z)
l <- list(a = y)

# Creation, and subscripting columns
system.time(for(i in 1:10^3) {
  d <- as.data.frame(y)
  d <- data.frame(y, z)
  d <- data.frame(y)
  d <- as.data.frame(l)
  d <- data.frame(l)
  d$z <- z
  d <- data.frame(y)
  d[["z"]] <- z
  d <- data.frame(y)
  d[, "z"] <- z
  d <- data.frame(y)
  d["z"] <- z
  d <- data.frame(y)
  d["z"] <- list(z=z)
  d <- data.frame(y)
  d["z"] <- Z
  d <- data.frame(y)
  a <- d["y"]
  d <- data.frame(y)
  a <- d[, "y", drop=F]
}, gc = TRUE)

# Subscripting rows
d <- data.frame(y, z)
i1 <- rep(TRUE, n)
i2 <- rep(c(TRUE,FALSE), length = n)
i3 <- which(runif(n) < .5)
system.time(for(i in 1:10^3) {
  a <- d[i1, ]
  a <- d[i2, ]
  a <- d[i3, ]
  a <- d[-i3, ]
}, gc = TRUE)
