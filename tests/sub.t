# File dataframe/tests/sub.t
# Part of the dataframe package.
# Copyright 2009 Google, author Tim Hesterberg
# Distributed under GPL 2 or later

# Tests for [.data.frame

{ # original "[.data.frame"
  # From R 2.9.1 source
  # /usr/local/google/R/R-2.9.1/src/library/base/R/dataframe.R

  oldF <-
    function(x, i, j, drop = if(missing(i)) TRUE else length(cols) == 1) {
      mdrop <- missing(drop)
      Narg <- nargs() - !mdrop # number of arg from x,i,j that were specified
      has.j <- !missing(j)

      if(Narg < 3L) {          # list-like indexing or matrix indexing
        if(!mdrop) warning("drop argument will be ignored")
        if(missing(i)) return(x)
        if(is.matrix(i))
          return(as.matrix(x)[i])       # desperate measures
        ## zero-column data frames prior to 2.4.0 had no names.
        nm <- names(x); if(is.null(nm)) nm <- character(0L)
        ## if we have NA names, character indexing should always fail
        ## (for positive index length)
        if(!is.character(i) && any(is.na(nm))) { # less efficient version
          names(nm) <- names(x) <- seq_along(x)
          y <- NextMethod("[")
          cols <- names(y)
          if(any(is.na(cols))) stop("undefined columns selected")
          cols <- names(y) <- nm[cols]
        } else {
          y <- NextMethod("[")
          cols <- names(y)
          if(!is.null(cols) && any(is.na(cols)))
            stop("undefined columns selected")
        }
        ## added in 1.8.0
        if(any(duplicated(cols))) names(y) <- make.unique(cols)
        ## since we have not touched the rows, copy over the raw row.names
        return(structure(y, class = oldClass(x),
                         row.names = .row_names_info(x, 0L)))
      }

      if(missing(i)) {                  # df[, j] or df[ , ]
        ## not quite the same as the 1/2-arg case, as 'drop' is used.
        if(missing(j) && drop && length(x) == 1L) return(.subset2(x, 1L))
        nm <- names(x); if(is.null(nm)) nm <- character(0L)
        if(!missing(j) && !is.character(j) && any(is.na(nm))) {
          ## less efficient version
          names(nm) <- names(x) <- seq_along(x)
          y <- if(missing(j)) x else .subset(x, j)
          cols <- names(y)
          if(any(is.na(cols))) stop("undefined columns selected")
          cols <- names(y) <- nm[cols]
        } else {
          y <- if(missing(j)) x else .subset(x, j)
          cols <- names(y)
          if(any(is.na(cols))) stop("undefined columns selected")
        }
        if(drop && length(y) == 1L) return(.subset2(y, 1L))
        if(any(duplicated(cols))) names(y) <- make.unique(cols)
        nrow <- .row_names_info(x, 2L)
        if(drop && !mdrop && nrow == 1L)
          return(structure(y, class = NULL, row.names = NULL))
        else
          return(structure(y, class = oldClass(x),
                           row.names = .row_names_info(x, 0L)))
      }

      ### df[i, j] or df[i , ]
      ## rewritten for R 2.5.0 to avoid duplicating x.
      xx <- x
      cols <- names(xx)         # needed for computation of 'drop' arg
      ## make a shallow copy
      x <- vector("list", length(x))
      ## attributes(x) <- attributes(xx) expands row names
      x <- .Call("R_copyDFattr", xx, x, PACKAGE="base")
      oldClass(x) <- attr(x, "row.names") <- NULL

      if(!missing(j)) {                 # df[i, j]
        nm <- names(x); if(is.null(nm)) nm <- character(0L)
        if(!is.character(j) && any(is.na(nm)))
          names(nm) <- names(x) <- seq_along(x)
        x <- x[j]
        cols <- names(x)                # needed for 'drop'
        if(drop && length(x) == 1L) {
          ## for consistency with [, <length-1>]
          if(is.character(i)) {
            rows <- attr(xx, "row.names")
            i <- pmatch(i, rows, duplicates.ok = TRUE)
          }
          ## need to figure which col was selected:
          ## cannot use .subset2 directly as that may
          ## use recursive selection for a logical index.
          xj <- .subset2(.subset(xx, j), 1L)
          return(if(length(dim(xj)) != 2L) xj[i] else xj[i, , drop = FALSE])
        }
        if(any(is.na(cols))) stop("undefined columns selected")
        ## fix up names if we altered them.
        if(!is.null(names(nm))) cols <- names(x) <- nm[cols]
        ## sxx <- match(cols, names(xx)) fails with duplicate names
        nxx <- structure(seq_along(xx), names=names(xx))
        sxx <- match(nxx[j], seq_along(xx))
      } else sxx <- seq_along(x)

      rows <- NULL    # placeholder: only create row names when needed
      # as this can be expensive.
      if(is.character(i)) {
        rows <- attr(xx, "row.names")
        i <- pmatch(i, rows, duplicates.ok = TRUE)
      }
      for(j in seq_along(x)) {
        xj <- xx[[ sxx[j] ]]
        ## had drop = drop prior to 1.8.0
        x[[j]] <- if(length(dim(xj)) != 2L) xj[i] else xj[i, , drop = FALSE]
      }

      if(drop) {
        n <- length(x)
        if(n == 1L) return(x[[1L]])     # drops attributes
        if(n > 1L) {
          xj <- x[[1L]]
          nrow <- if(length(dim(xj)) == 2L) dim(xj)[1L] else length(xj)
          ## for consistency with S: don't drop (to a list)
          ## if only one row, unless explicitly asked for
          drop <- !mdrop && nrow == 1L
        } else drop <- FALSE ## for n == 0
      }

      if(!drop) {      # not else as previous section might reset drop
        ## row names might have NAs.
        if(is.null(rows)) rows <- attr(xx, "row.names")
        rows <- rows[i]
        if((ina <- any(is.na(rows))) | (dup <- any(duplicated(rows)))) {
          ## both will coerce integer 'rows' to character:
          if (!dup && is.character(rows)) dup <- "NA" %in% rows
          if(ina)
            rows[is.na(rows)] <- "NA"
          if(dup)
            rows <- make.unique(as.character(rows))
        }
        ## new in 1.8.0  -- might have duplicate columns
        if(has.j && any(duplicated(nm <- names(x))))
          names(x) <- make.unique(nm)
        if(is.null(rows)) rows <- attr(xx, "row.names")[i]
        attr(x, "row.names") <- rows
        oldClass(x) <- oldClass(xx)
      }
      x
    }
  x <- data.frame(a=1:4, b=2:5)
  y <- data.frame(a=1:4, b=2:5, row.names = letters[1:4])
  TRUE
}

##### Single subscript
{ # single numerical subscript
  allTrue(all.equal(x[2], oldF(x, 2)),
          is.data.frame(x[2]))
}

{ # single numerical subscript, for data frame with row names
  all.equal(y[2], oldF(y, 2))
}

{ # single numerical vector subscript
  all.equal(x[2:1], oldF(x, 2:1))
}

{ # single character subscript
  all.equal(x["a"], oldF(x, "a"))
}

{ # single logical subscript
  all.equal(x[c(TRUE,FALSE)], oldF(x, c(TRUE,FALSE)))
}


##### Column subscript
{ # column numerical subscript
  allTrue(all.equal(x[, 2], oldF(x, , 2)),
          !is.data.frame(x[, 2]))
}

{ # drop = FALSE
  allTrue(all.equal(x[, 2, drop=FALSE], oldF(x, , 2, drop=FALSE)),
          is.data.frame(x[, 2, drop=FALSE]))
}

{ # column numerical subscript, for data frame with row names
  all.equal(y[, 2], oldF(y, , 2))
}

{ # column numerical vector subscript
  allTrue(all.equal(x[, 2:1], oldF(x, , 2:1)),
          is.data.frame(x[, 2:1]))
}

{ # column character subscript
  all.equal(x[, "a"], oldF(x, , "a"))
}

{ # column character subscript
  all.equal(x[,  c(TRUE,FALSE)], oldF(x, , c(TRUE,FALSE)))
}

##### Row subscript
{ # row numerical subscript
  allTrue(all.equal(x[2, ], oldF(x, 2, )),
          is.data.frame(x[2, ]))
}

{ # row numerical subscript, for data frame with row names
  all.equal(y[2, ], oldF(y, 2, ))
}

{ # row numerical vector subscript
  all.equal(x[2:1, ], oldF(x, 2:1, ))
}

{ # row character subscript
  all.equal(y["a"], oldF(y, "a"))
}

{ # row logical subscript
  all.equal(x[c(TRUE,FALSE,TRUE,FALSE),], oldF(x, c(TRUE,FALSE,TRUE,FALSE),))
}


##### Row and column subscripts
{ # row numerical subscript
  all.equal(x[2:3, 1:2], oldF(x, 2:3, 1:2))
}

##### Logical matrix
{ # row numerical subscript
  all.equal(x[x > 2], oldF(x, x > 2))
}
