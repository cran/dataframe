# File dataframe/tests/data.frame.t
# Part of the dataframe package.
# Copyright 2009 Google, author Tim Hesterberg
# Distributed under GPL 2 or later

# Tests for data.frame

{ # original version
  old.data.frame <-
    function (..., row.names = NULL, check.rows = FALSE, check.names = TRUE,
              stringsAsFactors = default.stringsAsFactors())
      {
        data.row.names <- if (check.rows && is.null(row.names))
          function(current, new, i) {
            if (is.character(current))
              new <- as.character(new)
            if (is.character(new))
              current <- as.character(current)
            if (any(duplicated(new)))
              return(current)
            if (is.null(current))
              return(new)
            if (all(current == new) || all(current == ""))
              return(new)
            stop(gettextf("mismatch of row names in arguments of 'data.frame', item %d",
                          i), domain = NA)
          }
        else
          function(current, new, i) {
            if (is.null(current)) {
              if (any(dup <- duplicated(new))) {
                warning("some row.names duplicated: ",
                        paste(which(dup), collapse = ","), " --> row.names NOT used")
                current
              }
              else new
            }
            else current
          }
        object <- as.list(substitute(list(...)))[-1L]
        mrn <- is.null(row.names)
        x <- list(...)
        n <- length(x)
        if (n < 1L) {
          if (!mrn) {
            if (is.object(row.names) || !is.integer(row.names))
              row.names <- as.character(row.names)
            if (any(is.na(row.names)))
              stop("row names contain missing values")
            if (any(duplicated(row.names)))
              stop("duplicate row.names: ",
                   paste(unique(row.names[duplicated(row.names)]), collapse = ", "))
          }
          else
            row.names <- integer(0L)
          return(structure(list(), names = character(0L), row.names = row.names,
                           class = "data.frame"))
        }
        vnames <- names(x)
        if (length(vnames) != n)
          vnames <- character(n)
        no.vn <- !nzchar(vnames)
        vlist <- vnames <- as.list(vnames)
        nrows <- ncols <- integer(n)
        for (i in seq_len(n)) {
          xi <- if (is.character(x[[i]]) || is.list(x[[i]]))
            as.data.frame(x[[i]], optional = TRUE,
                          stringsAsFactors = stringsAsFactors)
          else
            as.data.frame(x[[i]], optional = TRUE)
          nrows[i] <- .row_names_info(xi)
          ncols[i] <- length(xi)
          namesi <- names(xi)
          if (ncols[i] > 1L) {
            if (length(namesi) == 0L)
              namesi <- seq_len(ncols[i])
            if (no.vn[i])
              vnames[[i]] <- namesi
            else
              vnames[[i]] <- paste(vnames[[i]], namesi, sep = ".")
          }
          else {
            if (length(namesi))
              vnames[[i]] <- namesi
            else if (no.vn[[i]]) {
              tmpname <- deparse(object[[i]])[1L]
              if (substr(tmpname, 1L, 2L) == "I(") {
                ntmpn <- nchar(tmpname, "c")
                if (substr(tmpname, ntmpn, ntmpn) == ")")
                  tmpname <- substr(tmpname, 3L, ntmpn - 1L)
              }
              vnames[[i]] <- tmpname
            }
          }
          if (missing(row.names) && nrows[i] > 0L) {
            rowsi <- attr(xi, "row.names")
            nc <- nchar(rowsi, allowNA = FALSE)
            nc <- nc[!is.na(nc)]
            if (length(nc) && any(nc))
              row.names <- data.row.names(row.names, rowsi,
                                          i)
          }
          nrows[i] <- abs(nrows[i])
          vlist[[i]] <- xi
        }
        nr <- max(nrows)
        for (i in seq_len(n)[nrows < nr]) {
          xi <- vlist[[i]]
          if (nrows[i] > 0L && (nr%%nrows[i] == 0L)) {
            xi <- unclass(xi)
            fixed <- TRUE
            for (j in seq_along(xi)) {
              xi1 <- xi[[j]]
              if (is.vector(xi1) || is.factor(xi1))
                xi[[j]] <- rep(xi1, length.out = nr)
              else if (is.character(xi1) && class(xi1) == "AsIs")
                xi[[j]] <- structure(rep(xi1, length.out = nr),
                                     class = class(xi1))
              else
                if (inherits(xi1, "Date") || inherits(xi1, "POSIXct"))
                  xi[[j]] <- rep(xi1, length.out = nr)
                else {
                  fixed <- FALSE
                  break
                }
            }
            if (fixed) {
              vlist[[i]] <- xi
              next
            }
          }
          stop("arguments imply differing number of rows: ",
               paste(unique(nrows), collapse = ", "))
        }
        value <- unlist(vlist, recursive = FALSE, use.names = FALSE)
        vnames <- unlist(vnames[ncols > 0L])
        noname <- !nzchar(vnames)
        if (any(noname))
          vnames[noname] <- paste("Var", seq_along(vnames), sep = ".")[noname]
        if (check.names)
          vnames <- make.names(vnames, unique = TRUE)
        names(value) <- vnames
        if (!mrn) {
          if (length(row.names) == 1L && nr != 1L) {
            if (is.character(row.names))
              row.names <- match(row.names, vnames, 0L)
            if (length(row.names) != 1L || row.names < 1L ||
                row.names > length(vnames))
              stop("row.names should specify one of the variables")
            i <- row.names
            row.names <- value[[i]]
            value <- value[-i]
          }
          else if (!is.null(row.names) && length(row.names) !=
                   nr)
            stop("row names supplied are of the wrong length")
        }
        else if (!is.null(row.names) && length(row.names) != nr) {
          warning("row names were found from a short variable and have been discarded")
          row.names <- NULL
        }
        if (is.null(row.names))
          row.names <- .set_row_names(nr)
        else {
          if (is.object(row.names) || !is.integer(row.names))
            row.names <- as.character(row.names)
          if (any(is.na(row.names)))
            stop("row names contain missing values")
          if (any(duplicated(row.names)))
            stop("duplicate row.names: ",
                 paste(unique(row.names[duplicated(row.names)]), collapse = ", "))
        }
        attr(value, "row.names") <- row.names
        attr(value, "class") <- "data.frame"
        value
      }
  TRUE
}

##### Single vectors

{ # integer
  x <- 1:10
  all.equal(old.data.frame(x), data.frame(x))
}

{ # numeric
  x <- 1:10 + 0.0
  all.equal(old.data.frame(x), data.frame(x))
}

{ # logical
  x <- c(TRUE, FALSE, TRUE)
  all.equal(old.data.frame(x), data.frame(x))
}

{ # factor
  x <- factor(c("a", "b", "a"))
  all.equal(old.data.frame(x), data.frame(x))
}

{ # with names
  x <- c(a = 1.2, b = 3.4)
  all.equal(old.data.frame(x), data.frame(x))
}

{ # with an attribute
  x <- 1:4; attr(x, "fish") <- "carp"
  all.equal(old.data.frame(x), data.frame(x))
}

##### Multiple arguments
{ # multiple arguments without row names or column names
  x <- 1:6
  y <- factor(rep(letters[3:1], 2))
  all.equal(old.data.frame(x, y), data.frame(x, y))
}

{ # multiple arguments with column names
  x <- 1:6
  y <- factor(rep(letters[3:1], 2))
  all.equal(old.data.frame(a = x, b = y), data.frame(a = x, b = y))
}

{ # multiple arguments created on the fly
  all.equal(old.data.frame(a = 1:5, 2:6), data.frame(a = 1:5, 2:6))
}

{ # multiple arguments with names on vector
  x <- c(a = 1.2, b = 3.4)
  y <- 2:3
  all.equal(old.data.frame(x, y), data.frame(x, y))
}


##### Inputs with multiple columns

{ # list
  x <- list(a=1:4, b=2:5)
  all.equal(old.data.frame(x), data.frame(x))
}

{ # matrix
  x <- cbind(a=1:3, b=2:4)
  all.equal(old.data.frame(x), data.frame(x))
}

{ # data.frame
  x <- data.frame(a=1:3, b=2:4)
  y <- data.frame(c=3:5, d=4:6)
  all.equal(old.data.frame(x, y), data.frame(x, y))
}

##### Inputs with zero columns
{ # matrix
  x <- array(numeric(0), c(3,0))
  allTrue(all.equal(old.data.frame(a = 1:3, x), data.frame(a = 1:3, x)),
          all.equal(old.data.frame(x, a = 1:3), data.frame(x, a = 1:3)))
}

{ # matrix, with specified name
  x <- array(numeric(0), c(3,0))
  allTrue(all.equal(old.data.frame(a = 1:3, X = x), data.frame(a = 1:3, X = x)),
          all.equal(old.data.frame(X = x, a = 1:3), data.frame(X = x, a = 1:3)))
}
