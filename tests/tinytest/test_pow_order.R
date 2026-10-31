library(ROI)
library(ROI.plugin.scs)

## calc_pow_dims has to return the exponents in the same (sorted by id)
## order as the rows of A, even if the cone ids come in unsorted.
## cone codes: powp = 7L, powd = 8L (see scs_cones in R/plugin.R)
calc_pow_dims <- ROI.plugin.scs:::calc_pow_dims
cones <- list(cone = c(7L, 7L, 8L, 8L),
              id = c(2L, 1L, 2L, 1L),
              params = list("1" = c(a = 0.3), "2" = c(a = 0.5)))
expect_equal(calc_pow_dims(cones), c(0.3, 0.5, -0.3, -0.5),
             info = "pow_order@unsorted ids")

cones$id <- c(1L, 2L, 1L, 2L)
expect_equal(calc_pow_dims(cones), c(0.3, 0.5, -0.3, -0.5),
             info = "pow_order@sorted ids")

## End to end: rbind() of constraints created in the opposite order gives
## unsorted cone ids. Block 1 (vars 1:3, a = 0.3) is created first, so it has
## the smaller id, but it comes last in the constraint.
## max z1 + 2 z2  s.t.  x1 = x2 = 8, y1 = y2 = 1, x^a y^(1-a) >= |z|
## optimum 8^0.3 + 2 * 8^0.5 swapped exponents give 8^0.5 + 2 * 8^0.3
c1 <- C_constraint(L = cbind(-diag(3), matrix(0, 3, 3)), cones = K_powp(0.3), rhs = rep(0, 3))
c2 <- C_constraint(L = cbind(matrix(0, 3, 3), -diag(3)), cones = K_powp(0.5), rhs = rep(0, 3))
cz <- C_constraint(L = rbind(c(1, 0, 0, 0, 0, 0), c(0, 1, 0, 0, 0, 0),
                             c(0, 0, 0, 1, 0, 0), c(0, 0, 0, 0, 1, 0)),
                   cones = K_zero(4), rhs = c(8, 1, 8, 1))
x <- OP(c(0, 0, 1, 0, 0, 2), rbind(cz, c2, c1), maximum = TRUE)
opt <- ROI_solve(x, solver = "scs")
expect_equal(opt$objval, 8^0.3 + 2 * 8^0.5, tolerance = 1e-4,
             info = "pow_order@rbind unsorted ids")
