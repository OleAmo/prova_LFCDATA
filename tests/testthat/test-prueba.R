
ejemplo <- function(a,b){
  res <- a+b
  return(res)
}

# ejemplo(7,5)

test_that("foofy() does this", {
  # skip_if(today_is_a_monday())

  dat <- data.frame(x = c("a", "b", "c"), y = c(1, 2, 3))

  expect_equal(dat, dat)
})
