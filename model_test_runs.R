# sprüher -> menge an pestiziden -> kosten für pestiziden
#                                -> Wc  -> 
#                                -> Bio -> 
#                                -> Di  -> Fs
# 
# ascs -> menge pestizide reduziert -> kosten pestizide reduziert
# 
#                                   -> Wc reduziert -> GC reduziert (no event)
#                                   -> Bio erhöht   -> GC reduziert (no event)
#                                                                                            -> Gc -> Fp gleich
# 
#                                   -> Wc reduziert -> Cu erhöht
#                                   -> Bio erhöht   -> Cu erhöht
#                                                                 -> cu -> Fs erhöht
# 
#                                   -> Di gleich                        -> Fs gleich
#                                                                                     -> Fs erhöht -> Fp erhöht 
# 
# 
# 
# 
# normal -> menge pestizide gleich  -> kosten pestizide gleich
# 
#                                   -> Wc gleich    -> GC gleich (event will happen more likely)
#                                   -> Bio gleich   -> GC gleich (event will happen more likely)
#                                                                                           -> Gc -> Fp gleich
# 
#                                   -> Wc gleich    -> Cu gleich
#                                   -> Bio gleich   -> Cu gleich
#                                                                 -> cu -> Fs erhöht
# 
#                                   -> Di gleich                        -> Fs gleich
#                                                                                     -> Fs erhöht -> Fp erhöht 
# 
# 
#
#
#
#
#
#
#
#
#
# 
# 
# ascs_decision_model <- function(x, varnames)
# {
#   
#   
#   intervention_profit  <- fruit_sales_int - pesticide_cost_int - initial_int_cost
#   manual_profit        <- fruit_sales_man - pesticide_cost_man - initial_man_cost
#   
#   pesticide_cost_int <- pesticide_quantity_int * pesticide_price
#   pesticide_cost_man <- pesticide_quantity_man * pesticide_price
#   
#   
#   fruit_sales_int <- (yield_int * market_price) * customers_int
#   fruit_sales_man <- (yield_man * market_price) * customers_man
# 
#   yield_int <- estimated_yield * (estimated_plant_damage * factor_plant_damage_int)
#   yield_man <- estimated_yield * (estimated_plant_damage * factor_plant_damage_man)
#   
#   factor_plant_damage_int <- biodiversity_int + pesticide_quantity_int/pesticide_quantity_int
#   factor_plant_damage_man <- biodiversity_man + pesticide_quantity_man/pesticide_quantity_int
# 
#   biodiversity_int <- estimated_biodiversity * pesticide_quantity_int
#   biodiversity_man <- estimated_biodiversity * pesticide_quantity_man
#     
#   customers_int <- 
#   customers_man <- 
#   
# }
# 
# mcSimulation(estimate = as.estimate(ascs_decision_model),
#              model_function = example_decision_model,
#              numberOfModelRuns = 100,
#              functionSyntax = "plainNames")
# 
# 
# 
# 
# 

library(readxl)
library(decisionSupport)


ascs_estimates <- read_excel("data/input_data_02.xlsx")

ascs_decision_model <- function(){

#  vv(yield_int, var_CV, n_years)
#  vv(yield_man, var_CV, n_years)
  
  vv(market_price, var_CV, n_years)

  vv(yield, var_CV, n_years)

  vv(pesticide_cost, var_CV, n_years)
  
#  vv(pesticide_quantity_int, var_CV, n_years)
#  vv(pesticide_quantity_man, var_CV, n_years)
  
  vv(initial_int_cost, var_CV, n_years)
#  vv(initial_man_cost, var_CV, n_years)
  
#  fruit_sales_int <- yield_int * market_price
#  fruit_sales_man <- yield_man * market_price
  
#  pesticide_cost_int <- pesticide_quantity_int * pestice_cost
#  pesticide_cost_man <- pesticide_quantity_man * pestice_cost
  
  fruit_sales <- yield * market_price
  
  NPV_intervention  <- fruit_sales - pesticide_cost - initial_int_cost
  NPV_manual        <- fruit_sales - pesticide_cost
  
  NPV_decision <- NPV_intervention - NPV_manual
  
  return(list(NPV_intervention = NPV_intervention,
            NPV_manual = NPV_manual,
            NPV_decision = NPV_decision))
}         
    

mcSimulation_results <- mcSimulation(estimate = as.estimate(ascs_estimates),
                                     model_function = ascs_decision_model,
                                     numberOfModelRuns = 100,
                                     functionSyntax = "plainNames")

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_intervention", "NPV_manual"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)


