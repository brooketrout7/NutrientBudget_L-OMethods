# ==============================================================================
# Estimate denitrification
#
# Purpose:
#   Estimate weekly nitrogen loss from Lake McDonald through denitrification
#   using published areal denitrification rates and restricting 
#   to the estimated sediment-accumulation area.
#
# Notes:
#   - Denitrification rate is from:
#     https://doi.org/10.1021/acs.est.1c07602
#   - Based on hypolimnetic No3 concentration
# ==============================================================================


# 1. Define lake geometry ------------------------------------------------------

# Lake McDonald surface area (m2)

SA_m2 <- 27810670.1791


# 3. Define denitrification rates ----------------------------------------------

# Best estimate of areal denitrification rate based 
#DN = 21.3·[NO3–]bottom – 3.88 & [NO3–]bottom in lake mcdonald is 0.1 
# 185 ppb, 0.185 ppm
# (g-N m-2 yr-1)

dn_best <- 0.1


# Lower estimate of areal denitrification rate
# (g-N m-2 yr-1); 183 ppb, 0.183 ppm lowest possible value without being zero

dn_lwr <- 0.01


# Upper estimate of areal denitrification rate
# (g-N m-2 yr-1)

dn_upr <- 1


# 4. Calculate weekly denitrification ------------------------------------------

# Weekly denitrification:
#
#   denitrification rate (g-N m-2 yr-1)
#   * accumulation area (m2)
#   * 1 kg / 1000 g
#   * 1 yr / 52 weeks


DN_kg_wk_best <-
  dn_best *
  SA_m2 *
  (1 / 1000) *
  (1 / 52)


DN_kg_wk_lwr <-
  dn_lwr *
  SA_m2 *
  (1 / 1000) *
  (1 / 52)


DN_kg_wk_upr <-
  dn_upr *
  SA_m2 *
  (1 / 1000) *
  (1 / 52)