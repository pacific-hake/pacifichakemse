context("Test the round_list() function")

simp <- 3.141597978978926253
simp_lst <- list(3.141597978978926253)
vec1 <- c(1.98321456568821234, 2.31225478586736129485, 3.141597978978926253, 4.0207897891010303)
vec2 <- vec1 * 2
vec3 <- vec1 * 3

df1 <- data.frame(a = vec1, b = vec2)
df2 <- data.frame(a = vec1, b = vec2, d = vec3)
m1 <- as.matrix(df1)
m2 <- as.matrix(df2)
lst <- list(lst_aa = vec1,
            lst_bb = df1)
lst_lst <- list(lst_lst_aa = vec1,
                lst_lst_bb = lst)
lst1 <- list(lst1_aa = vec1,
             lst1_bb = df1,
             lst1_cc = m1)
lst_double <- list(lst, lst1)
lst2 <- list(lst2_aa = lst,
             lst2_bb = df2,
             lst2_cc = list(lst, lst1))
err_lst <- list(a = 1.2345, b = c("a", "b", "c"))

test_that("round_list() - Tests for correct output", {
  expect_equal(round_list(simp, 2), 3.14)
  j <- round_list(simp_lst, 2)
  expect_equivalent(str(j), str(simp_lst))
  expect_equal(j[[1]], 3.14)
  j <- round_list(lst, 2)
  expect_equivalent(str(j), str(lst))
  j <- round_list(lst2, 2)
  expect_equivalent(str(j), str(lst2))
  j <- round_list(lst_lst, 2)
  expect_equivalent(str(j), str(lst_lst))
  j <- round_list(lst_double, 2)
  expect_equivalent(str(j), str(lst_double))
  expect_error(round_list(err_lst, 2))
})

