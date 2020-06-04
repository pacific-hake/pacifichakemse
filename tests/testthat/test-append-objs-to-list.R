context("Test the append_objs_to_list() function")

lst <- list(a = 1,
            b = c(4, 5),
            list(5, 6))

lst2 <- list(a = "a",
             b = "b",
             vec1 = c("c", "d"),
             lst21 = list(lst22 = list(1, 2), z = "z"))

lst3 <- list(d = "a",
             e = "b",
             vec1 = c("c", "d"),
             lst21 = list(lst22 = list(1, 2), z = "z"))

test_that("append_objs_to_list() - Tests for argument errors", {
  expect_error(append_objs_to_list(df = NULL))
  expect_error(append_objs_to_list(df = NA))
})

test_that("append_objs_to_list() - Tests for same name elements", {
  expect_error(append_objs_to_list(lst, lst2$a, lst2$b))
})

test_that("append_objs_to_list() - Tests for correct output", {
  max_age <- 15
  out <- append_objs_to_list(lst, lst3$d, lst3$e, lst3$vec1, lst3$lst21$lst22, lst3$lst21$z, max_age)
  expect_equal(out[[1]], 1)
  expect_equal(out[[2]], c(4, 5))
  expect_equal(out[[3]], list(5, 6))
  expect_equal(out$d, "a")
  expect_equal(out$e, "b")
  expect_equal(out$vec1, c("c", "d"))
  expect_equal(out$lst22, list(1, 2))
  expect_equal(out$z, "z")
  expect_equal(out$max_age, 15)

})

