# File dataframe/tests/dollarGets.t
# Part of the dataframe package.
# Copyright 2009 Google, author Tim Hesterberg
# Distributed under GPL 2 or later

# Tests for "$<-.data.frame"

{ # Original version
  oldF <-
    function (x, i, value)
      {
        cl <- oldClass(x)
        class(x) <- NULL
        nrows <- .row_names_info(x, 2L)
        if (!is.null(value)) {
          N <- NROW(value)
          if (N > nrows)
            stop(gettextf("replacement has %d rows, data has %d",
                          N, nrows), domain = NA)
          if (N < nrows && N > 0L)
            if (nrows%%N == 0L && length(dim(value)) <= 1L)
              value <- rep(value, length.out = nrows)
            else
              stop(gettextf("replacement has %d rows, data has %d",
                            N, nrows), domain = NA)
          if (is.atomic(value))
            names(value) <- NULL
        }
        x[[i]] <- value
        class(x) <- cl
        return(x)
      }
  x1 <- data.frame(a=1:4, b=2:5)
  TRUE
}

{ # assignment to new variable
  x <- x1
  y <- oldF(x1, "c", 3:6)
  x$c <- 3:6
  allTrue(all.equal(x, y),
          all.equal(names(x), c("a","b","c")),
          all.equal(dim(x), c(4,3)),
          all.equal(rownames(x), as.character(1:4)))
}

{ # assignment to existing variable
  x <- x1
  y <- oldF(x1, "b", 3:6)
  x$b <- 3:6
  allTrue(all.equal(x, y),
          all.equal(names(x), c("a","b")),
          all.equal(dim(x), c(4,2)),
          all.equal(rownames(x), as.character(1:4)))
}

{ # remove a variable
  x <- x1
  y <- oldF(x1, "b", NULL)
  x$b <- NULL
  allTrue(all.equal(x, y),
          all.equal(names(x), c("a")),
          all.equal(dim(x), c(4,1)),
          all.equal(rownames(x), as.character(1:4)))
}

{ # assignment of a scalar
  x <- x1
  y <- oldF(x1, "c", 3)
  x$c <- 3
  allTrue(all.equal(x, y),
          all.equal(names(x), c("a","b","c")),
          all.equal(dim(x), c(4,3)),
          all.equal(rownames(x), as.character(1:4)))
}

{ # extra class on x
  x <- x1
  class(x) <- c("foo", "data.frame")
  x$c <- 3:6
  all.equal(class(x), c("foo", "data.frame"))
}

{ # attribute on x
  x <- x1
  attr(x, "fish") <- "carp"
  x$c <- 3:6
  all.equal(attr(x, "fish"), "carp")
}

{ # names on value
  x <- x1
  value <- c(g=3, h=4, i=5, k=6)
  y <- oldF(x1, "c", value)
  x$c <- value
  allTrue(all.equal(x, y),
          all.equal(names(x), c("a","b","c")),
          all.equal(dim(x), c(4,3)),
          all.equal(rownames(x), as.character(1:4)))
}
