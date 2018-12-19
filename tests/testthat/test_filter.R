### Test filter functions
context("Filter functions")

# Test data -------------------------------------------------------------------

mem_a_csv <- "
    person_id,  membership_id,  start_date,     end_date
    p1,         a1,             2001-01-01,     2001-12-31
    p1,         a2,             2005-01-01,     2005-12-31
    p1,         a3,             2006-01-01,     2006-12-31
    p1,         a4,             2010-01-01,     2010-12-31
    p2,         a5,             2005-01-01,     2005-12-31
    p2,         a6,             2006-01-01,     2006-12-31
    p2,         a7,             2010-01-01,     2010-12-31
    p2,         a8,             2015-01-01,     2015-12-31
"

mem_b_csv <- "
    person_id,  membership_id,  start_date,     end_date
    p1,         b1,             2001-06-01,     2002-06-30
    p1,         b2,             2004-01-01,     2004-12-31
    p1,         b3,             2006-01-01,     2006-12-31
    p1,         b4,             2011-01-01,     2011-12-31
    p2,         b5,             2004-01-01,     2004-12-31
    p2,         b6,             2006-01-01,     2006-12-31
    p2,         b7,             2011-01-01,     2011-12-31
    p2,         b8,             2015-06-01,     2016-06-30
"

mem_a <- readr::read_csv(mem_a_csv,
                         trim_ws = TRUE,
                         col_types = readr::cols(
                             person_id = readr::col_character(),
                             membership_id = readr::col_character(),
                             start_date = readr::col_date(),
                             end_date = readr::col_date()))

mem_b <- readr::read_csv(mem_b_csv,
                         trim_ws = TRUE,
                         col_types = readr::cols(
                             person_id = readr::col_character(),
                             membership_id = readr::col_character(),
                             start_date = readr::col_date(),
                             end_date = readr::col_date()))

# Test filter_dates -----------------------------------------------------------

test_that("filter_dates raises a missing column error.", {

    expect_error(
        filter_dates(
            mem_a,
            start_col = "no_such_column",
            end_col = "end_date"),
        "Could not find a column called no_such_column")

    expect_error(
        filter_dates(
            mem_a,
            start_col = "start_date",
            end_col = "no_such_column"),
        "Could not find a column called no_such_column")
})

test_that("filter_dates raises an error when dates are out of sequence.", {
    expect_error(
        filter_dates(
            mem_a,
            start_col = "start_date",
            end_col = "end_date",
            from_date = "2010-01-01",
            to_date = "2009-12-31"),
        "to_date is before from_date")
})

test_that("filter_dates raises a date format error.", {

    expect_error(
        filter_dates(
            mem_a,
            start_col = "start_date",
            end_col = "end_date",
            from_date = "2010-01-XX",
            to_date = "2010-12-31"),
        "2010-01-XX is not a valid Date or date string")

    expect_error(
        filter_dates(
            mem_a,
            start_col = "start_date",
            end_col = "end_date",
            from_date = "2010-01-01",
            to_date = "2010-12-XX"),
        "2010-12-XX is not a valid Date or date string")
})

test_that("filter_dates does not filter without dates.", {

    f_mem_a <- filter_dates(
        mem_a,
        start_col = "start_date",
        end_col = "end_date")

    expect_equal(nrow(f_mem_a), nrow(mem_a))
    expect_equal(ncol(f_mem_a), ncol(mem_a))
    expect_equal(all(f_mem_a == mem_a), TRUE)
})

test_that("filter_dates excludes rows before from_date.", {

    f_mem_a <- filter_dates(
        mem_a,
        start_col = "start_date",
        end_col = "end_date",
        from_date = "2004-12-31")

    expect_equal(nrow(f_mem_a), nrow(mem_a) - 1)
    expect_equal(ncol(f_mem_a), ncol(mem_a))
    expect_equal(f_mem_a[1, ]$person_id, "p1")
    expect_equal(f_mem_a[1, ]$membership_id, "a2")
    expect_equal(f_mem_a[1, ]$start_date, as.Date("2005-01-01"))
    expect_equal(f_mem_a[1, ]$end_date, as.Date("2005-12-31"))
})

test_that("filter_dates excludes rows after to_date.", {

    f_mem_a <- filter_dates(
        mem_a,
        start_col = "start_date",
        end_col = "end_date",
        to_date = "2011-01-01")

    expect_equal(nrow(f_mem_a), nrow(mem_a) - 1)
    expect_equal(ncol(f_mem_a), ncol(mem_a))
    expect_equal(f_mem_a[nrow(f_mem_a), ]$person_id, "p2")
    expect_equal(f_mem_a[nrow(f_mem_a), ]$membership_id, "a7")
    expect_equal(f_mem_a[nrow(f_mem_a), ]$start_date, as.Date("2010-01-01"))
    expect_equal(f_mem_a[nrow(f_mem_a), ]$end_date, as.Date("2010-12-31"))
})

test_that("filter_dates excludes rows outside both dates.", {

    f_mem_a <- filter_dates(
        mem_a,
        start_col = "start_date",
        end_col = "end_date",
        from_date = "2004-12-31",
        to_date = "2011-01-01")

    expect_equal(nrow(f_mem_a), nrow(mem_a) - 2)
    expect_equal(ncol(f_mem_a), ncol(mem_a))

    expect_equal(f_mem_a[1, ]$person_id, "p1")
    expect_equal(f_mem_a[1, ]$membership_id, "a2")
    expect_equal(f_mem_a[1, ]$start_date, as.Date("2005-01-01"))
    expect_equal(f_mem_a[1, ]$end_date, as.Date("2005-12-31"))

    expect_equal(f_mem_a[nrow(f_mem_a), ]$person_id, "p2")
    expect_equal(f_mem_a[nrow(f_mem_a), ]$membership_id, "a7")
    expect_equal(f_mem_a[nrow(f_mem_a), ]$start_date, as.Date("2010-01-01"))
    expect_equal(f_mem_a[nrow(f_mem_a), ]$end_date, as.Date("2010-12-31"))
})

test_that("filter_dates includes rows with partial instersection.", {

    f_mem_a <- filter_dates(
        mem_a,
        start_col = "start_date",
        end_col = "end_date",
        from_date = "2005-06-30",
        to_date = "2010-06-30")

    expect_equal(nrow(f_mem_a), nrow(mem_a) - 2)
    expect_equal(ncol(f_mem_a), ncol(mem_a))

    expect_equal(f_mem_a[1, ]$person_id, "p1")
    expect_equal(f_mem_a[1, ]$membership_id, "a2")
    expect_equal(f_mem_a[1, ]$start_date, as.Date("2005-01-01"))
    expect_equal(f_mem_a[1, ]$end_date, as.Date("2005-12-31"))

    expect_equal(f_mem_a[nrow(f_mem_a), ]$person_id, "p2")
    expect_equal(f_mem_a[nrow(f_mem_a), ]$membership_id, "a7")
    expect_equal(f_mem_a[nrow(f_mem_a), ]$start_date, as.Date("2010-01-01"))
    expect_equal(f_mem_a[nrow(f_mem_a), ]$end_date, as.Date("2010-12-31"))
})

test_that("filter_dates includes rows with enclosing dates.", {

    f_mem_a <- filter_dates(
        mem_a,
        start_col = "start_date",
        end_col = "end_date",
        from_date = "2005-06-30",
        to_date = "2005-06-30")

    expect_equal(nrow(f_mem_a), 2)
    expect_equal(ncol(f_mem_a), ncol(mem_a))

    expect_equal(f_mem_a[1, ]$person_id, "p1")
    expect_equal(f_mem_a[1, ]$membership_id, "a2")
    expect_equal(f_mem_a[1, ]$start_date, as.Date("2005-01-01"))
    expect_equal(f_mem_a[1, ]$end_date, as.Date("2005-12-31"))

    expect_equal(f_mem_a[nrow(f_mem_a), ]$person_id, "p2")
    expect_equal(f_mem_a[nrow(f_mem_a), ]$membership_id, "a5")
    expect_equal(f_mem_a[nrow(f_mem_a), ]$start_date, as.Date("2005-01-01"))
    expect_equal(f_mem_a[nrow(f_mem_a), ]$end_date, as.Date("2005-12-31"))
})

# Test filter_memberships -----------------------------------------------------

test_that("filter_memberships raises a missing column error.", {

    expect_error(
        filter_memberships(
            tm = mem_a,
            fm = mem_b,
            tm_id_col = "no_such_column",
            tm_start_col = "start_date",
            tm_end_col = "end_date",
            fm_start_col = "start_date",
            fm_end_col = "end_date",
            join_col = "person_id"),
        "Could not find a column called no_such_column")

    expect_error(
        filter_memberships(
            tm = mem_a,
            fm = mem_b,
            tm_id_col = "membership_id",
            tm_start_col = "no_such_column",
            tm_end_col = "end_date",
            fm_start_col = "start_date",
            fm_end_col = "end_date",
            join_col = "person_id"),
        "Could not find a column called no_such_column")

    expect_error(
        filter_memberships(
            tm = mem_a,
            fm = mem_b,
            tm_id_col = "membership_id",
            tm_start_col = "start_date",
            tm_end_col = "no_such_column",
            fm_start_col = "start_date",
            fm_end_col = "end_date",
            join_col = "person_id"),
        "Could not find a column called no_such_column")

    expect_error(
        filter_memberships(
            tm = mem_a,
            fm = mem_b,
            tm_id_col = "membership_id",
            tm_start_col = "start_date",
            tm_end_col = "end_date",
            fm_start_col = "no_such_column",
            fm_end_col = "end_date",
            join_col = "person_id"),
        "Could not find a column called no_such_column")

    expect_error(
        filter_memberships(
            tm = mem_a,
            fm = mem_b,
            tm_id_col = "membership_id",
            tm_start_col = "start_date",
            tm_end_col = "end_date",
            fm_start_col = "start_date",
            fm_end_col = "no_such_column",
            join_col = "person_id"),
        "Could not find a column called no_such_column")

    expect_error(
        filter_memberships(
            tm = mem_a,
            fm = mem_b,
            tm_id_col = "membership_id",
            tm_start_col = "start_date",
            tm_end_col = "end_date",
            fm_start_col = "start_date",
            fm_end_col = "end_date",
            join_col = "no_such_column"),
        "Could not find a column called no_such_column")
})

test_that("filter_memberships filters the correct memberships.", {

    f_mem_a <- filter_memberships(
        tm = mem_a,
        fm = mem_b,
        tm_id_col = "membership_id",
        tm_start_col = "start_date",
        tm_end_col = "end_date",
        fm_start_col = "start_date",
        fm_end_col = "end_date",
        join_col = "person_id")

    expect_equal(nrow(f_mem_a), 4)
    expect_equal(ncol(f_mem_a), ncol(mem_a))
    expect_equal(colnames(f_mem_a), colnames(mem_a))
    expect_equal(f_mem_a$person_id, c("p1", "p1", "p2", "p2"))
    expect_equal(f_mem_a$membership_id, c("a1", "a3", "a6", "a8"))
    expect_equal(f_mem_a$start_date, c(
        as.Date("2001-01-01"),
        as.Date("2006-01-01"),
        as.Date("2006-01-01"),
        as.Date("2015-01-01")))
    expect_equal(f_mem_a$end_date, c(
        as.Date("2001-12-31"),
        as.Date("2006-12-31"),
        as.Date("2006-12-31"),
        as.Date("2015-12-31")))
})
