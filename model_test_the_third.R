ascs_decision_model <- function(x, varnames){
  
  # machine costs are applied only once so n = 1
  machine_upg_cost <- c( initial_upg_cost, rep(0, (n_years-1)) )

  # application efficiency (tree distance and how much it saves to not spray inbetween)
  # the efficiency is fix over the years per model run 
  pest_red_fact <- rep( pesticide_reduction_auto, n_years )

  # potential plant damage
  pot_plant_damage <- vv( potential_plant_damage, var_CV, n_years ) # 0.7 - 1.7
  
  # costs for pesticides per ha can vary over the years
  pest_cost_ha_man <- vv( pesticide_cost, var_CV, n_years )
  pest_cost_ha_aut <- pest_cost_ha_man * pest_red_fact
  
  # the amount of used pesticides can vary depending on weather forcasts (factor from 0.7 - 1.3)
  # and infestation/disease levels but this is integrated through a correlation with plant_damage
  # quantity varies for manuell and automated sprayer 
  
  pest_quant_man <- vv( pesticide_quantity, var_CV, n_years )
  pest_quant_aut <- pest_quant_man * pest_red_fact 

  yield_t_ha <- vv(yield, var_CV, n_years)
  market_price_e_t <- vv(market_price, var_CV, n_years)
  
  fruit_sales_e_ha <- yield_t_ha * market_price_e_t
  
  #  --- profit   
  farmers_profit_aut = fruit_sales_e_ha - pesticide_cost_ha_aut - machine_cost
  farmers_profit_man = fruit_sales_e_ha - pesticide_cost_ha_man

  #  --- NPV   
  NPV_aut = discount(farmers_profit_aut, discount_rate, calculate_NPV = TRUE)
  NPV_man = discount(farmers_profit_man, discount_rate, calculate_NPV = TRUE)

  
  return(list(Interv_NPV = NPV_aut,
              NO_Interv_NPV = NPV_man,
              NPV_decision_do = NPV_aut - NPV_man,
              Cashflow_decision_do = farmers_profit_aut - farmers_profit_man))
}





## Importing input table 
input_table <- read.csv("data/input_data_03.csv", sep = ";")


## Perform a Monte Carlo simulation 
mcSimulation_results <- decisionSupport::mcSimulation(
  estimate = decisionSupport::estimate_read_csv("data/input_data_03.csv"),
  model_function = ascs_decision_model,
  numberOfModelRuns = 200,
  functionSyntax = "plainNames"
)


## Plot Net Present Value (NPV) distributions 
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("Interv_NPV", "NO_Interv_NPV"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("Interv_NPV",
                                             "NO_Interv_NPV"),
                                    method = 'boxplot')

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = "NPV_decision_do",
                                    method = 'boxplot_density')


## Cashflow analysis 
plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision_do")


## Projection to Latent Structures (PLS) analysis 
pls_result <- plsr.mcSimulation(object = mcSimulation_results,
                                resultName = names(mcSimulation_results$y)[3], ncomp = 1)

plot_pls(pls_result, input_table = input_table, threshold = 0)


## Value of information (VoI) analysis 

# Here we subset the outputs from the mcSimulation function (y) by selecting the correct variables
# choose this carefully and be sure to run the multi_EVPI only on the variables that the you want
mcSimulation_table <- data.frame(mcSimulation_results$x, mcSimulation_results$y[1:3])

evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "Interv_NPV")

plot_evpi(evpi, decision_vars = "NPV_decision_do")

compound_figure(mcSimulation_object = mcSimulation_results, 
                input_table = input_table, plsrResults = pls_result, 
                EVPIresults = evpi, decision_var_name = "NPV_decision_do", 
                xcashflow_var_name = "Cashflow_decision_do", 
                base_size = 7)
