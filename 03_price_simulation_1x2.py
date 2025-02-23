import numpy as np
import pandas as pd
from itertools import product
import price_simulation_func as ps

save_path = "C:/Users/Nicolas/OneDrive - Zeppelin-University gGmbH/Dokumente/Studium/Masterthesis/upstreamness_pricevolatility/output/"

#####################################################################################
################################### 2 sector model ################################
###################################################################################

### Generate 2-sector IO-table

## Dominant diagonal - fuzzy pictures
#Z = np.array([[700,100],
#              [100,500]])
#F = np.array([200,400])
#X = Z.sum(axis=1) + F
## Dominant off-diagonal - reverse upstream and downstream
#Z = np.array([[200,700],
#              [300,200]])
#F = np.array([100,500])
#X = Z.sum(axis=1) + F

### Create list of IO tables
IO_tabs = [[np.array([[200,500],
                      [300,200]]), 
            np.array([300,500])],
           [np.array([[700,100],
                      [100,500]]),
            np.array([200,400])]]

### Create list for transformed inputs
yr_var_list = []
intermediates_list = []
prod_pos_list = []

### Loop over list of IO tables 
for year,tab in enumerate(IO_tabs):

    ## Assign basic IO elements
    Z = tab[0]
    F = tab[1]
    
    ## Compute IO components
    X = Z.sum(axis=1) + F
    X_hat = np.diag(1/X)
    I = np.identity(len(X))
    cs = np.array(["s1","s2"])
    VA = X - Z.sum(axis=0)
    INT = Z.sum(axis=0)
    lab_share = VA@X_hat
    final_share = F/X

    ## Compute IO matrices
    A = Z @ X_hat
    L = np.linalg.inv(I-A)
    B = X_hat @ Z
    G = np.linalg.inv(I-B)

    ## Bring inputs in shape for function
    years = np.repeat(year,len(cs))
    yr_var = [cs,years,F,INT,lab_share,final_share,X]
    yr_var_cols = ["country_sector","year","final_demand","inter_sales",
                   "nonint_share","demand_share","total_output"]
    yr_var_iter = pd.DataFrame(dict(zip(yr_var_cols,yr_var)))

    ## Bring intermediate shares in shape for function
    intermediates = [cs,years,Z[:,0],Z[:,1]]
    int_cols = ["country_sector","year"]+cs.tolist()
    intermediates_iter = pd.DataFrame(dict(zip(int_cols,intermediates)))

    ## Create dataframe of upstreamness and downstreamness
    prod_pos_iter = pd.DataFrame({"year":year,
                                  "country_sector": cs, 
                                  "tot_bwd": L.T.sum(axis=1),
                                  "tot_fwd": G.sum(axis=1)})
    
    ## Append on lists of tranformed inputs
    yr_var_list.append(yr_var_iter)
    intermediates_list.append(intermediates_iter)
    prod_pos_list.append(prod_pos_iter)

### Concat lists of transformed inputs into tables
yr_var_df = pd.concat(yr_var_list,ignore_index=True,axis=0)
intermediates_df = pd.concat(intermediates_list,ignore_index=True,axis=0)
prod_pos_df = pd.concat(prod_pos_list,ignore_index=True,axis=0)


#################### Shock Simulation Schedules ###############################

### Set simulation parameters - cost shock
years = np.arange(0,len(IO_tabs))
c_var1 = [[0,0,0],[0,0,0.05],[0,0,0.1],[0,0,0.15]] # Diff i-shocks
d_var1 = [[0,0,0]]
p_var1 = [[0,0],[0,0.05],[0,0.1],[0,0.15]]
c_shock_vars = list(product(*[c_var1,d_var1,p_var1]))
c_shock_list = ps.shock_specs(shockvar_list=c_shock_vars,dist="normal",
                            dim=(50,1,2),labels=cs)

scales = list(product(*[np.arange(0.8,1.3,0.2),np.arange(0.8,1.3,0.2)]))
scales.extend([(0.9,0.9),(1.1,1.1)])
ret_scale = [pd.DataFrame({"country_sector": cs,
                           "rs_ind": rs}) for rs in scales]

cost_params = list(product(*[c_shock_list,ret_scale,years]))
cost_params = sorted(cost_params, key=lambda x: str(x[1].iloc[0,1])+str(x[1].iloc[1,1])+str(x[2]), reverse=False)


### Run simulation schedule - cost shock
c_change_df, c_volat_df = ps.simulation_schedule_para(
    param_list=cost_params,yearly_variables=yr_var_df,
    intermediates_df=intermediates_df,stages=5,propagation="down",
    core_cap=1,include_shocks=True,scenario_index=True)

### Merge prod_pos on on volat_df
c_volat_df = c_volat_df.merge(prod_pos_df,on=["year","country_sector"],how="left")

### Save results
c_change_df.to_csv(save_path+"changes_cost_shock_1x2.csv",index=False)
c_volat_df.to_csv(save_path+"results_cost_shock_1x2.csv",index=False)


### Set simulation parameters - demand shock
years = np.arange(0,len(IO_tabs))
c_var2 = [[0,0,0]]
d_var2 = [[0,0,0],[0,0,0.05],[0,0,0.1],[0,0,0.15]] # Diff i-shocks
p_var2 = [[0,0],[0,0.05],[0,0.1],[0,0.15]]
d_shock_vars = list(product(*[c_var2,d_var2,p_var2]))
d_shock_list = ps.shock_specs(shockvar_list=d_shock_vars,dist="normal",
                            dim=(50,1,2),labels=cs)

scales = list(product(*[np.arange(0.8,1.3,0.2),np.arange(0.8,1.3,0.2)]))
scales.extend([(0.9,0.9),(1.1,1.1)])
ret_scale = [pd.DataFrame({"country_sector": cs,
                           "rs_ind": rs}) for rs in scales]

demand_params = list(product(*[d_shock_list,ret_scale,years]))
demand_params = sorted(demand_params, key=lambda x: str(x[1].iloc[0,1])+str(x[1].iloc[1,1])+str(x[2]), reverse=False)


### Run simulation schedule - demand shock
d_change_df, d_volat_df = ps.simulation_schedule_para(
    param_list=demand_params,yearly_variables=yr_var_df,
    intermediates_df=intermediates_df,stages=5,propagation="up",
    core_cap=1,include_shocks=True,scenario_index=True)

### Merge prod_pos on on volat_df
d_volat_df = d_volat_df.merge(prod_pos_df,on=["year","country_sector"],how="left")

### Save results
d_change_df.to_csv(save_path+"changes_demand_shock_1x2.csv",index=False)
d_volat_df.to_csv(save_path+"results_demand_shock_1x2.csv",index=False)
