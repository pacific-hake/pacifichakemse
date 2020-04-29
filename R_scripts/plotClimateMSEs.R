## Load MSE's and compare ##
# Climate Scenarios
load('results/Climate/MSErun_move_JMC_climate_0_HYBR_TAC1.Rdata')
ls.0 <- ls.save
load('results/Climate/MSErun_move_JMC_climate_0_02_HYBR_TAC1.Rdata')
ls.002 <- ls.save
load('results/Climate/MSErun_move_JMC_climate_0_04_HYBR_TAC1.Rdata')
ls.004 <- ls.save


simyears <- 50
yr <- 1966:(2017+simyears-1)
nruns <- 100
df <- load_data_seasons(nseason = 4, nspace = 2) # Prepare data for operating model
sim.data <- run.agebased.true.catch(df)

ls.plot <- list('base model' = ls.0,
                'medium change' = ls.002,
                'high change' = ls.004)

fn_plot_MSE(ls.plot, sim.data,plotfolder = 'Figs/Climate/',plotexp = TRUE)


# Plot the realized catch vs the quota

