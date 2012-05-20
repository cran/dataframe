# File dataframe/tests/dSubGets.t
# Part of the dataframe package.
# Copyright 2009 Google, author Tim Hesterberg
# Distributed under GPL 2 or later

# Tests for [[<-.data.frame

{ # Original version, from R 2.9.0
  oldF <-
    function (x, i, j, value)
      {
        cl <- oldClass(x)
        class(x) <- NULL
        nrows <- .row_names_info(x, 2L)
        if (is.atomic(value))
          names(value) <- NULL
        if (nargs() < 4L) {
          nc <- length(x)
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
          }
          x[[i]] <- value
          if (length(x) > nc) {
            nc <- length(x)
            if (names(x)[nc] == "")
              names(x)[nc] <- paste("V", nc, sep = "")
            names(x) <- make.unique(names(x))
          }
          class(x) <- cl
          return(x)
        }
        if (missing(i) || missing(j))
          stop("only valid calls are x[[j]] <- value or x[[i,j]] <- value")
        rows <- attr(x, "row.names")
        nvars <- length(x)
        if (n <- is.character(i)) {
          ii <- match(i, rows)
          n <- sum(new.rows <- is.na(ii))
          if (n > 0L) {
            ii[new.rows] <- seq.int(from = nrows + 1L, length.out = n)
            new.rows <- i[new.rows]
          }
          i <- ii
        }
        if (all(i >= 0L) && (nn <- max(i)) > nrows) {
          if (n == 0L) {
            nrr <- (nrows + 1L):nn
            if (inherits(value, "data.frame") && (dim(value)[1L]) >=
                length(nrr)) {
              new.rows <- attr(value, "row.names")[seq_len(nrr)]
              repl <- duplicated(new.rows) | match(new.rows,
                                                   rows, 0L)
              if (any(repl))
                new.rows[repl] <- nrr[repl]
            } else
            new.rows <- nrr
          }
          x <- xpdrows.data.frame(x, rows, new.rows)
          rows <- attr(x, "row.names")
          nrows <- length(rows)
        }
        iseq <- seq_len(nrows)[i]
        if (any(is.na(iseq)))
          stop("non-existent rows not allowed")
        if (is.character(j)) {
          if ("" %in% j)
            stop("column name \"\" cannot match any column")
          jseq <- match(j, names(x))
          if (any(is.na(jseq)))
            stop("replacing element in non-existent column: ",
                 j[is.na(jseq)])
        } else if (is.logical(j) || min(j) < 0L) {
          jseq <- seq_along(x)[j]
        } else {
          jseq <- j
          if (max(jseq) > nvars)
            stop("replacing element in non-existent column: ",
                 jseq[jseq > nvars])
        }
        if (length(iseq) > 1L || length(jseq) > 1L)
          stop("only a single element should be replaced")
        x[[jseq]][[iseq]] <- value
        class(x) <- cl
        x
      }
  x1 <- data.frame(a=1:4, b=2:5)
  TRUE
}


{ # assignment to new variable
  x <- x1
  y <- oldF(x1, "c", value = 3:6)
  x[["c"]] <- 3:6
  allTrue(all.equal(x, y),
          all.equal(names(x), c("a","b","c")),
          all.equal(dim(x), c(4,3)),
          all.equal(rownames(x), as.character(1:4)))
}

{ # assignment to existing variable
  x <- x1
  y <- oldF(x1, "b", value = 3:6)
  x[["b"]] <- 3:6
  allTrue(all.equal(x, y),
          all.equal(names(x), c("a","b")),
          all.equal(dim(x), c(4,2)),
          all.equal(rownames(x), as.character(1:4)))
}

{ # remove a variable
  x <- x1
  y <- oldF(x1, "b", value = NULL)
  x[["b"]] <- NULL
  allTrue(all.equal(x, y),
          all.equal(names(x), c("a")),
          all.equal(dim(x), c(4,1)),
          all.equal(rownames(x), as.character(1:4)))
}

{ # assignment of a scalar
  x <- x1
  y <- oldF(x1, "c", value = 3)
  x[["c"]] <- 3
  allTrue(all.equal(x, y),
          all.equal(names(x), c("a","b","c")),
          all.equal(dim(x), c(4,3)),
          all.equal(rownames(x), as.character(1:4)))
}

{ # extra class on x
  x <- x1
  class(x) <- c("foo", "data.frame")
  x[["c"]] <- 3:6
  all.equal(class(x), c("foo", "data.frame"))
}

{ # attribute on x
  x <- x1
  attr(x, "fish") <- "carp"
  x[["c"]] <- 3:6
  all.equal(attr(x, "fish"), "carp")
}

{ # names on value
  x <- x1
  value <- c(g=3, h=4, i=5, k=6)
  y <- oldF(x1, "c", value = value)
  x[["c"]] <- value
  allTrue(all.equal(x, y),
          all.equal(names(x), c("a","b","c")),
          all.equal(dim(x), c(4,3)),
          all.equal(rownames(x), as.character(1:4)))
}

{ # assignment by number to new column
  x <- x1
  y <- oldF(x1, 3, value = 3:6)
  x[[3]] <- 3:6
  allTrue(all.equal(x, y),
          all.equal(names(x), c("a","b","V3")),
          all.equal(dim(x), c(4,3)),
          all.equal(rownames(x), as.character(1:4)))
}

{ # assignment by number to existing
  x <- x1
  y <- oldF(x1, 2, value = 3:6)
  x[[2]] <- 3:6
  allTrue(all.equal(x, y),
          all.equal(names(x), c("a","b")),
          all.equal(dim(x), c(4,2)),
          all.equal(rownames(x), as.character(1:4)))
}

{ # remove a column by number
  x <- x1
  y <- oldF(x1, 2, value = NULL)
  x[[2]] <- NULL
  allTrue(all.equal(x, y),
          all.equal(names(x), c("a")),
          all.equal(dim(x), c(4,1)),
          all.equal(rownames(x), as.character(1:4)))
}
