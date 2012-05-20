# Tests for as.data.frame.list

{ # Original version, from R 2.9.2

  old.as.data.frame.list <-
    function(x, row.names = NULL, optional = FALSE, ...,
             stringsAsFactors = default.stringsAsFactors())
      {
        ## need to protect names in x.
        cn <- names(x)
        m <- match(c("row.names", "check.rows", "check.names", "stringsAsFactors"),
                   cn, 0L)
        if(any(m)) {
          cn[m] <- paste("..adfl.", cn[m], sep="")
          names(x) <- cn
        }
        x <- eval(as.call(c(expression(data.frame), x, check.names = !optional,
                            stringsAsFactors = stringsAsFactors)))
        if(any(m)) names(x) <- sub("^\\.\\.adfl\\.", "", names(x))
        if(!is.null(row.names)) {
          # row.names <- as.character(row.names)
          if(length(row.names) != dim(x)[[1L]])
            stop(gettextf("supplied %d row names for %d rows",
                          length(row.names), dim(x)[[1L]]), domain = NA)
          attr(x, "row.names") <- row.names
        }
        x
      }
  TRUE
}

{
  l <- list(a=1:5, b=letters[2:6])
  l2 <- l; names(l2$a) <- letters[1:5]
  TRUE
}


{ # test dispatching
  all.equal(as.data.frame(l),
            as.data.frame.list(l))
}

{ # basic test
  all.equal(old.as.data.frame.list(l),
            as.data.frame.list(l))
}

{ # object created on the fly
  all.equal(old.as.data.frame.list(list(a=1:5, b=2:6)),
            as.data.frame.list(list(a=1:5, b=2:6)))
}

{ # row.names
  all.equal(old.as.data.frame.list(l, row.names = letters[1:5]),
            as.data.frame.list(l, row.names = letters[1:5]))
}

{ # row.names in object
  all.equal(old.as.data.frame.list(l2),
            as.data.frame.list(l2))
}

{ # optional = TRUE
  all.equal(old.as.data.frame.list(l, optional = TRUE),
            as.data.frame.list(l, optional = TRUE))
}

{ # stringsAsFactors = FALSE
  all.equal(old.as.data.frame.list(l, stringsAsFactors = FALSE),
            as.data.frame.list(l, stringsAsFactors = FALSE))
}

{ # funny names
  all.equal(old.as.data.frame.list(list("a h"=1:5, b=2:6)),
            as.data.frame.list(list("a h"=1:5, b=2:6)))
}

{ # no names
  all.equal(old.as.data.frame.list(list(1:5, "a")),
            as.data.frame.list(list(1:5, "a")))
}
