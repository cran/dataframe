# File dataframe/tests/runTests.R
# Part of the dataframe package.
# Copyright 2009 Google, author Tim Hesterberg
# Distributed under GPL 2 or later

# Run all .t tests in tests/

require("splus2R")
library(dataframe, warn.conflicts = FALSE)

files <- list.files(pattern = "\\.t$")

for(file in files)
  do.test(file)
