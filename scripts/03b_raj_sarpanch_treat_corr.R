

load("data/rajasthan/sarpanch_election_data/raj_panch.RData")

# Correlation across treatment statuses

model1 <- feols(treat_2010 ~ treat_2005, data = raj_panch)
model2 <- feols(treat_2015 ~ treat_2010, data = raj_panch)
model3 <- feols(treat_2020 ~ treat_2015, data = raj_panch)
model4 <- feols(treat_2020 ~ treat_2005 + treat_2010 + treat_2015, data = raj_panch)

models <- list(model1, model2, model3, model4)

etable(models, 
       tex = TRUE, 
       style.tex = style.tex("aer",model.format = "[i]",depvar.style = "*"),
       digits = 3,
       interaction.combine = " $\\times $ ",
       file = "tables/raj_treatment_reg.tex",
       dict = c( 'treat_2005' = "Treatment 2005",
                 'treat_2010' = "Treatment  2010",
                 'treat_2015' = "Treatment 2015",
                 'treat_2020' = "Treatment 2020"), 
       replace = TRUE)
