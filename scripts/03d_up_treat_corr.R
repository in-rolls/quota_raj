
load("data/up/up_all_recoded.RData")

# Correlation across treatment statuses

model2 <- feols(treat_2010 ~ treat_2005, data = up_all)
model3 <- feols(treat_2015 ~ treat_2010, data = up_all)
model4 <- feols(treat_2015 ~ treat_2005 + treat_2010, data = up_all)

models <- list(model2, model3, model4)

etable(models, 
       tex = TRUE, 
       digits = 3,
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       interaction.combine = " $\\times $ ",
       file = "tables/up_treatment_reg.tex",
       dict = c( 'treat_2005' = "Treatment 2005",
                 'treat_2010' = "Treatment  2010",
                 'treat_2015' = "Treatment 2015"), 
       replace = TRUE)

