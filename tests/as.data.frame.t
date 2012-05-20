# File dataframe/tests/as.data.frame.t
# Part of the dataframe package.
# Copyright 2009 Google, author Tim Hesterberg
# Distributed under GPL 2 or later

# Tests for as.data.frame

{ # old version (same method for: Date POSIXct complex difftime factor
  #              integer logical numeric numeric_version ordered raw)

  old.as.data.frame.vector <-
    function (x, row.names = NULL, optional = FALSE, ..., nm = paste(deparse(substitute(x),
                                                            width.cutoff = 500L), collapse = " "))
      {
        force(nm)
        nrows <- length(x)
        if (is.null(row.names)) {
          if (nrows == 0L)
            row.names <- character(0L)
          else if (length(row.names <- names(x)) == nrows && !any(duplicated(row.names))) {
          }
          else row.names <- .set_row_names(nrows)
        }
        names(x) <- NULL
        value <- list(x)
        if (!optional)
          names(value) <- nm
        attr(value, "row.names") <- row.names
        class(value) <- "data.frame"
        value
      }
  TRUE
}

{ # integer
  x <- 1:10
  all.equal(old.as.data.frame.vector(x), as.data.frame(x))
}

{ # numeric
  x <- 1:10 + 0.0
  all.equal(old.as.data.frame.vector(x), as.data.frame(x))
}

{ # logical
  x <- c(TRUE,FALSE,TRUE)
  all.equal(old.as.data.frame.vector(x), as.data.frame(x))
}

{ # factor
  x <- factor(c("a","b","a"))
  all.equal(old.as.data.frame.vector(x), as.data.frame(x))
}

{ # with names
  x <- c(a = 1.2, b = 3.4)
  all.equal(old.as.data.frame.vector(x), as.data.frame(x))
}
