# This file contains some example commands using data frame code,
# to test memory allocations.

# Run this file using a version of R compiled with --enable-memory-profiling
#   * once with existing data frame code
#   * once with this dataframe package

# Each example contains comments indicating how much memory is allocated
# using existing code (from R-2.9.2 or R-2.14.2) and when the
# dataframe package is loaded.

# Summary
#                               R-2.14.2                library(dataframe)
#       as.data.frame(y)        3                       1
#       data.frame(y)           6                       3
#       data.frame(y, z)        7 each                  3 each
#       as.data.frame(l)        8                       3
#       data.frame(l)           13                      5
#       d$z <- z                3,2                     1,1
#       d[["z"]] <- z           4,3                     2,1
#       d[, "z"] <- z           6,4,2                   2,2,1
#       d["z"] <- z             6,5,2                   2,2,1
#       d["z"] <- list(z=z)     6,3,2                   2,2,1
#       d["z"] <- Z #list(z=z)  6,2,2                   2,1,1
#       a <- d["y"]             2                       1
#       a <- d[, "y", drop=F]   2                       1
# Where two numbers are given, they refer to:
#   (copies of the old data frame),
#   (copies of the new column)
# A third number refers to numbers of
#   (copies made of an integer vector of row names)

# Below, "other large object" means at least 16K.
# These other allocations do not occur if the same code is run with again.

# --------------------------------------------------
# Define objects

# Use objects of different sizes in order to distinguish them.
# Avoid objects the same size as an integer vector of row names.
y <- 1:10^4 + 0.0
z <- 1:10^4 + 0i
Z <- list(z = z)
l <- list(a = y)
object.size(y)  #  80040
object.size(z)  # 160040
object.size(1:10^4)  # 40040
object.size(as.character(1:10^4))  # 560040

# --------------------------------------------------
# as.data.frame(y)

Rprofmem("temp.out", threshold = 10^4)
d <- as.data.frame(y)
Rprofmem(NULL); system("cat temp.out")

#       2.9.2   2.9.2+  2.14.2  2.14.2+
#       4       1       3       1


# --------------------------------------------------
# data.frame(y)

Rprofmem("temp.out", threshold = 10^3)
d <- data.frame(y)
Rprofmem(NULL); system("cat temp.out")

#       2.9.2   2.9.2+  2.14.2  2.14.2+
#       8       3       6       3
#                       2*37K   2*16K            # other large object


# --------------------------------------------------
# data.frame(y, z)

Rprofmem("temp.out", threshold = 10^3)
d <- data.frame(y, z)
Rprofmem(NULL); system("cat temp.out")

#       2.9.2   2.9.2+  2.14.2  2.14.2+
#       8       3       7       3               # of each, y and z


# --------------------------------------------------
# as.data.frame(l)

Rprofmem("temp.out", threshold = 10^3)
d <- as.data.frame(l)
Rprofmem(NULL); system("cat temp.out")

#       2.9.2   2.9.2+  2.14.2  2.14.2+
#       10      3       8       3


# --------------------------------------------------
# data.frame(l)

Rprofmem("temp.out", threshold = 10^3)
d <- data.frame(l)
Rprofmem(NULL); system("cat temp.out")

#       2.9.2   2.9.2+  2.14.2  2.14.2+
#       15      5       13      5


#--------------------------------------------------
# d$z <- z

d <- data.frame(y)
Rprofmem("temp.out", threshold = 10^4)
d$z <- z
Rprofmem(NULL); system("cat temp.out")

#       2.9.2   2.9.2+  2.14.2  2.14.2+
# y     3       1       3       1
# z     2       1       2       1


#--------------------------------------------------
# d[["z"]] <- z

d <- data.frame(y)
Rprofmem("temp.out", threshold = 10^4)
d[["z"]] <- z
Rprofmem(NULL); system("cat temp.out")

#       2.9.2   2.9.2+  2.14.2  2.14.2+
# y     4       1       4       1
# z     3       1       3       1
# other                 2*20K   0

#--------------------------------------------------
# d[, "z"] <- z

d <- data.frame(y)
Rprofmem("temp.out", threshold = 10^4)
d[, "z"] <- z
Rprofmem(NULL); system("cat temp.out")

#       2.9.2   2.9.2+  2.14.2  2.14.2+
# y     6       2       6       2
# z     4       2       4       2
# i     2       1       2       1       (integer row names)
# other                 3       0       (3 includes: 2*56K + 1*23K)


#--------------------------------------------------
# d["z"] <- z

d <- data.frame(y)
Rprofmem("temp.out", threshold = 10^4)
d["z"] <- z
Rprofmem(NULL); system("cat temp.out")

#       2.9.2   2.9.2+  2.14.2  2.14.2+
# y     6       2       6       2
# z     5       2       5       2
# i     2       1       2       1   (integer row names)


#--------------------------------------------------
# d["z"] <- list(z=z)

d <- data.frame(y)
Rprofmem("temp.out", threshold = 10^4)
d["z"] <- list(z=z)
Rprofmem(NULL); system("cat temp.out")

#       2.9.2   2.9.2+  2.14.2  2.14.2+
# y     6       2       6       2
# z     3       2       3       2
# i     2       1       2       1   (integer row names)


#--------------------------------------------------
# d["z"] <- Z # list(z=z)

d <- data.frame(y)
Rprofmem("temp.out", threshold = 10^4)
d["z"] <- Z
Rprofmem(NULL); system("cat temp.out")

#       2.9.2   2.9.2+  2.14.2  2.14.2+
# y     6       2       6       2
# z     2       1       2       1
# i     2       1       2       1   (integer row names)


# --------------------------------------------------
# a <- d["y"]

d <- data.frame(y)
Rprofmem("temp.out", threshold = 10^4)
a <- d["y"]
Rprofmem(NULL); system("cat temp.out")

#       2.9.2   2.9.2+  2.14.2  2.14.2+
#       2       1       2       1
# other                 2*30K   0

# --------------------------------------------------
# a <- d[, "y", drop=F]

d <- data.frame(y)
Rprofmem("temp.out", threshold = 10^4)
a <- d[, "y", drop=F]
Rprofmem(NULL); system("cat temp.out")

#       2.9.2   2.9.2+  2.14.2  2.14.2+
#       2       1       2       1
