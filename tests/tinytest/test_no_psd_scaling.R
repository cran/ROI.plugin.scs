library(ROI)
library(ROI.plugin.scs)

## NO_PSD_SCALING used to fail with "object 'roi_cones' not found" when
## PSD cones were present.
x <- OP(objective = c(1, 1, 1),
        constraints = C_constraint(L = diag(-1, 3), cones = K_psd(3), rhs = rep(-1, 3)),
        maximum = FALSE)

## default scaling gives the known optimum x = (1, 1, 1)
opt <- ROI_solve(x, solver = "scs")
expect_true(equal(opt$objval, 3), info = "NO_PSD_SCALING@default objval")

## with scaling disabled the solve must still run and return the PSD block
opt <- suppressWarnings(ROI_solve(x, solver = "scs", NO_PSD_SCALING = TRUE))
expect_equal(length(solution(opt, "psd")), 1L, info = "NO_PSD_SCALING@psd length")
