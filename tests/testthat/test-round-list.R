context("Test the round_list() and round_data_frame() functions")

scalar <- 3.1415926535897
simp_lst <- list(scalar)
vec1 <- c(1.9832145656882,
          2.3122547858673,
          scalar,
          4.0207897891010)
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

df <- data.frame(a = vec1,
                 b = c("Hello", "World", "!", "!"))
df_char <- df
df_char$b <- as.character(df_char$b)

arr_3d <- array(data = rep(vec1, 10),
                dim = c(2, 4, 5),
                dimnames = list(c("a1", "a2"),
                                c("b1", "b2", "b3", "b4"),
                                c("c1", "c2", "c3", "c4", "c5")))

arr_4d <- array(data = rep(vec1, 20),
                dim = c(2, 4, 5, 2),
                dimnames = list(c("a1", "a2"),
                                c("b1", "b2", "b3", "b4"),
                                c("c1", "c2", "c3", "c4", "c5"),
                                c("d1", "d2")))

arr_5d <- array(data = rep(vec1, 60),
                dim = c(2, 4, 5, 2, 3),
                dimnames = list(c("a1", "a2"),
                                c("b1", "b2", "b3", "b4"),
                                c("c1", "c2", "c3", "c4", "c5"),
                                c("d1", "d2"),
                                c("e1", "e2", "e3")))

lst_with_arr <- list(vec = vec1,
                     lst = lst,
                     arr3 = arr_3d,
                     arr4 = arr_4d,
                     arr5 = arr_5d)

test_that("round_data_frame() - Tests for correct output", {
  # Test data frame with a factor column
  j <- round_data_frame(df, 2)
  expect_equivalent(str(j), str(df))
  expect_equal(j$a, c(1.98, 2.31, 3.14, 4.02))
  # Test data frame with a character column
  j <- round_data_frame(df_char, 2)
  expect_equivalent(str(j), str(df_char))
  expect_equal(j$a, c(1.98, 2.31, 3.14, 4.02))
})

test_that("round_3d_array() - Tests for correct output", {
  j <- round_3d_array(arr_3d, 2)
  expect_equivalent(str(j), str(arr_3d))
})

test_that("round_4d_array() - Tests for correct output", {
  j <- round_4d_array(arr_4d, 2)
  expect_equivalent(str(j), str(arr_4d))
})

test_that("round_5d_array() - Tests for correct output", {
  j <- round_5d_array(arr_5d, 2)
  expect_equivalent(str(j), str(arr_5d))
})

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
  j <- round_list(lst_with_arr, 2)
  expect_equivalent(str(j), str(lst_with_arr))
})

