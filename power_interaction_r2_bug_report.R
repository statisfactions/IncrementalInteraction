
library(reprex)

library(InteractionPoweR)
pwr_calc = power_interaction_r2(N = 500,
                     r.x1.y = -0.18,
                     r.x2.y = -0.03,
                     r.x1.x2 = 0.64,
                     r.x1x2.y = 0.1,
                     alpha = 0.05,
                     detailed_results = TRUE)

totalr2 = with(pwr_calc, (b1*r.x1.y) + (b2*r.x2.y) + (b3*r.x1x2.y))
null2 = with(pwr_calc, (b1*r.x1.y) + (b2*r.x2.y))

## This should be r.x1x2.y^2 = 0.1
(r2change = totalr2 - null2)
