# do we really have an influence on the government concerns/interventions if only one farmer changes to automated section control sprayers?
# is our question maybe more like " what if all farmers in a specific region change to automated section ..." ?
# and the " what if " looks at the specific environmental changes and the profit at the end ( will there be the same profit compared to no automated ... )

# or do we just need to assume that the farm is so big that it would have considerable impact on government concerns/interventions?

ascs_decision_function <- function(x, varnames){

government_pest_event_aut <-
  chance_event( chance = c(government_pest_event,rep(government_pest_event*government_pest_event_factor_aut,4), rep(0, 6)),
                1, # pesticide_quantity_gov, 
                0, # pesticide_quantity_aut,
                n = 10 )

government_pest_event_man <-
  chance_event( chance = c(government_pest_event,rep(government_pest_event*government_pest_event_factor_man,4), rep(0, 5)),
                1, # pesticide_quantity_gov, 
                0, # pesticide_quantity_man,
                n = 10 )

pesticide_quantity_aut_min <- vv(pesticide_application_quantity_aut, var_CV, n_years) # minimum application quantity (automated) to get no plant damage, will be influenced by (?)
pesticide_quantity_man_min <- vv(pesticide_application_quantity_man, var_CV, n_years) # minimum application quantity (not automated) to get no plant damage, will be influenced by (?)

government_pest_event_factor_aut <- pesticide_quantity_aut_min/pesticide_quantity_gov
government_pest_event_factor_man <- pesticide_quantity_man_min/pesticide_quantity_gov



rel_water_cont_dec <- vv(rel_water_contamination,
                         5, #var_CV,
                         n_years,
                         relative_trend = -5, # we assume the water cont decreases every year by 5% without any influence
                         lower_limit = 0)

rel_biodiversity_inc <- vv(rel_biodiversity,
                           5, #var_CV,
                           n_years,
                           relative_trend = 15, #absolute_trend = bio_change,
                           lower_limit = 0.2,
                           upper_limit = 0.8)

rel_government_concern <- vv(government_concern,
                             var_CV,
                             n_years)

machine_cost <- vv(c(initial_cost, rep(0, 9)),
                   var_CV,
                   n_years)



for (decision_intervention_machine in c(FALSE,TRUE))
{
  # ------ basic connections
    if (decision_intervention_machine) {  # ----------- automated section controlled sprayer system
    water_cont_change <- TRUE
    biodiversity_change <- TRUE
    plant_damage <- FALSE # False for both since we spray as much as we need to have 0 plant damage
    factor_government_concern <- TRUE # True for both but smaller for the automated system
  } else                                  # ----------- section controlled sprayer system
    water_cont_change <- FALSE
    biodiversity_change <- FALSE
    plant_damage <- FALSE
    factor_government_concern <- TRUE 
  
    
  # ------ calculations (mostly depending on the systems)      
  if (water_cont_change) {
    water_cont <- rel_water_cont_dec + # we would have a natural decrease in contamination if we didnt spray
      ((pesticide_quantity_aut_min - pesticide_quantity_gov)*0.1) # assuming that the amount that we spray more then the government guideline influences the water contamination by 10%
  } else 
    water_cont <- rel_water_cont_dec +
      ((pesticide_quantity_man_min - pesticide_quantity_gov)*0.1) 
    
  if (biodiversity_change) {
    biodiversity <- rel_biodiversity_inc - # we would have a natural increase in biodiversity if we didnt spray
      (pesticide_quantity_aut_min*0.1) # assuming that the amount that we spray influences the biodiversity by 10%
  } else 
    biodiversity <- rel_biodiversity_inc
      (pesticide_quantity_man_min*0.1) # assuming that the amount that we spray influences the biodiversity by 10%
    
  if (plant_damage) {
    plant_damage <- water_cont * biodiversity # +?  +/* the difference of pesticide_quantity_gov - pesticide_quantity_aut/man_min? only applies for the government interventions
  } else
    plant_damage <- 0 # !!!!!!!!!!!!!!!! we wont have plant damage in the case that the government wont intervene. the code above (cont and bio) is to calculate the influence on the government concern. HOW ?

    
    
  # !!!!!!!!  still in question how to use this to influence the chance vector in our chance events      
  if (factor_government_concern){
    factor_gc <- water_cont + biodiversity + rel_government_concern
  }  

    
    
  # government intervention influence on automated system
  if ( (government_pest_event_aut)&(decision_intervention_machine) ) {
    pesticide_quantity_aut_min <- vv(c(rep(pesticide_application_quantity_aut,5), rep(pesticide_quantity_gov, 5)), # if it occured only in the 4th year it would not count for the second half of our n_years but only for the 9th and 10th (the 5 must be a variable determined by the first occurance of the chance event)
                                   var_CV = 0, # varies for the years without gov. intervention but will be capped at max (pesticide_quantity_gov) for the rest
                                   n = 10)
    water_cont <- c( rep((rel_water_cont_dec + ((pesticide_quantity_aut_min - pesticide_quantity_gov)*0.1)),5), rep(rel_water_contamination,5))  
    biodiversity_increase <- TRUE # yes but it could change a little after the implementation of the government guideline ( the automated one reduces the application quantity but might still be a little above the guideline)
    plant_damage <- TRUE # not for the years without the guideline. yes, if the the automated application quantity (the quantity to have 0 plant damage) was above the government application quantity
    factor_government_concern <- TRUE
  }
  
  # government intervention influence on non automated system
  if ( (government_pest_event_man)&(!decision_intervention_machine) ) {
    pesticide_quantity_man_min <- vv(c(rep(pesticide_application_quantity_man,5), rep(pesticide_quantity_gov, 5)), # if it occured only in the 4th year it would not count for the second half of our n_years but only for the 9th and 10th (the 5 must be a variable determined by the first occurance of the chance event)
                                   var_CV = 0, # varies for the years without gov. intervention but will be capped at max (pesticide_quantity_gov) for the rest
                                   n = 10)
    water_cont <- c( rep((rel_water_cont_dec + ((pesticide_quantity_man_min - pesticide_quantity_gov)*0.1)),5), rep(rel_water_contamination,5))
                         rel_biodiversity_inc - (pesticide_quantity_man_min*0.1) # same assumptions and problems as above
    biodiversity <- c( rep((rel_biodiversity_inc - (pesticide_quantity_man_min*0.1)),5), rep(rel_water_contamination,5))  # but only after the implementation of the government guideline ( first occurrence of chance event +5 )
    plant_damage <- TRUE # not for the years without the guideline, but for the years where the manual application quantity (the quantity to have 0 plant damage) was above the government application quantity
  }

}
cost_aut = pesticide_quantity_aut_min * pesticide_cost + machine_cost
cost_man = pesticide_quantity_man_min * pesticide_cost

fruit_sales = estimated_sales * plant_damage

NPV_int <-
  fruit_sales - cost_aut
NPV_no_int <-
  fruit_sales - cost_man

return(list(Interv_NPV = NPV_int,
            NO_Interv_NPV = NPV_no_int,
            NPV_decision_do = NPV_int - NPV_no_int))
            #Cashflow_decision_do = result_interv - result_n_interv))

# NPV_interv <-
#   discount(result_interv, discount_rate, calculate_NPV = TRUE)
# 
# NPV_n_interv <-
#   discount(result_n_interv, discount_rate, calculate_NPV = TRUE)

}  
