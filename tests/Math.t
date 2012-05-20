{ # Test for Math.data.frame.
  # This should run without an error.
  expectWarnings(all.equal(data.frame(a = c(1,1,2,2), b = letters[3:6]),
                           round(data.frame(a = 3:6 / 3, b = letters[3:6]))),
                 "skipping non-numeric variable in data frame")
}

