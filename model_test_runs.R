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

gompertz_yield(
  max_harvest = 0.8,
  time_to_first_yield_estimate = 3,
  time_to_second_yield_estimate = 10,
  first_yield_estimate_percent = 20,
  second_yield_estimate_percent = 100,
  n_years = 10,
  var_CV = 0,
  no_yield_before_first_estimate = TRUE
)

vv(
  var_mean,
  var_CV,
  n,
  distribution = "normal",
  absolute_trend = NA,
  relative_trend = NA,
  lower_limit = NA,
  upper_limit = NA
)


vv(
  var_mean = biodiversity,
  var_CV = var_CV,
  n = n_years,
  distribution = "normal",
  absolute_trend = bio_change,
  lower_limit = 0.4,
  upper_limit = 0.6
)

bio_change = biodiversity * pesticides
  
chance_event(
  chance,
  value_if = 1,
  value_if_not = 0,
  n = 1,
  CV_if = 0,
  CV_if_not = CV_if,
  one_draw = FALSE
)




ascs_estimates <- read_excel("data/input_data_02.xlsx")

ascs_decision_model <- function(){

  # machine costs are applied only once so n = 1
machine_cost <- rep( c(initial_cost, rep(0, 9)) )
  
  # vv(var_mean = initital_cost,
  # var_CV = var_CV,
  # n = 1)    

  # costs for pesticides can vary over the years
pesticide_cost <- vv(
  var_mean = pesticide_costs,
  var_CV = var_CV,
  n = n_years)
  # the amount of used pesticides can vary depending on weather and infestation/disease levels
pesticide_quantity <- vv(
  var_mean = pesticide_application_quantity,
  var_CV = var_CV,
  n = n_years)
  # application efficiency (tree distance) the efficiency is fixed per model run (n=1)
pesticide_app_reduction <- vv(
  var_mean = reduction,
  var_CV = var_Cv,
  n = 1)

  #  --- fixed values for both decisions and one run
  # machine costs will be depreciated over the time the machine is in use
fixed_machine_cost_yearly = 0 - machine_cost/n_years
  # pesticide 
fixed_pesticide_cost_yearly = pesticide_cost * pesticide_quantity



  #  --- values for intervention
  # pesticide yearly cost, if automation of machine is applied (no intervention)
pesticide_cy_int = pesticide_cost_yearly * pesticide_app_reduction



  #  --- values if no intervention
pesticide_cy_int = 

  
  
  #  --- NPV   
farmers_profit_int = fruit_sales - pesticide_cost_yearly - machine_cost_yearly
farmers_profit_no_int = fruit_sales - pesticide_cost_yearly

return(list(Interv_NPV = farmers_profit_int,
            NO_Interv_NPV = farmers_profit_no_int,
            NPV_decision_do = farmers_profit_int - farmers_profit_no_int,))
}


ascs_estimates[ascs_estimates$variable=="yield","upper"]

vv(
  var_mean = biodiversity,
  var_CV = var_CV,
  n = n_years,
  distribution = "normal",
  absolute_trend = bio_change,
  lower_limit = 0.4,
  upper_limit = 0.6
)





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


