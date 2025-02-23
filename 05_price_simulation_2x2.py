import numpy as np
import pandas as pd
from itertools import product
import price_simulation_func as ps

save_path = "C:/Users/Nicolas/OneDrive - Zeppelin-University gGmbH/Dokumente/Studium/Masterthesis/upstreamness_pricevolatility/output/"

###################################################################################
########################## 2 country 2 sector model ###############################
###################################################################################

### Generate 2x2 IO-table framework

# (1) Intra-country sectoral inqualities
#Z = np.array([[250,200,150,0],
#              [150,100,100,50],
#              [200,50,300,250],
#              [150,50,200,150]])
#F = np.array([400,100,400,150])
#X = Z.sum(axis=1) + F

### This was the most recent one - Think about what features I wanna explore
#Z = np.array([[400,300,100,200],
#              [250,450,150,150],
#              [200,0,350,350],
#              [0,250,300,450]])
#F = np.array([200,300,300,500])
#X = Z.sum(axis=1) + F


### Create list of IO tables
IO_tabs = [
    # IO table with dominant off-(block)diag and grouped prod_pos 
    [np.array([[100,50,150,100],
               [200,50,200,150],
               [200,100,150,50],
               [200,300,100,100]]), np.array([600,400,500,300])],
    # IO table with dominant (block)diag and grouped prod_pos
    [np.array([[200,150,200,50],
               [150,100,100,50],
               [100,100,200,300],
               [150,50,200,100]]), np.array([400,600,300,500])],
    # Fully equal table
    [np.array([[200,200,200,200],
               [200,200,200,200],
               [200,200,200,200],
               [200,200,200,200]]), np.array([200,200,200,200])]]


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
    cs = np.array(["A_s1","A_s2","B_s1","B_s2"])
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
    intermediates = [cs,years,Z[:,0],Z[:,1],Z[:,2],Z[:,3]]
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
c_shock_var = [[0,0,0],[0,0,0.05],[0,0,0.1],[0,0,0.15], # Diff magnit. of i-shock
               [0,0.05,0],[0,0.1,0],[0,0.15,0], # Diff magnit. of s-shock
               [0.05,0,0],[0.1,0,0],[0.15,0,0], # Diff magnit. of c-shock
               [0,0.04,0.01],[0,0.025,0.025],[0,0.01,0.04], # Diff ratio of si-shock
               [0.04,0,0.01],[0.025,0,0.025],[0.01,0,0.04]] # Diff ratio of ci-shock
d_shock_var = [[0,0,0]]
p_shock_var = [[0,0],[0,0.05],[0,0.1],[0,0.15]]
shock_vars = list(product(*[c_shock_var,d_shock_var,p_shock_var]))
shock_list = ps.shock_specs(shockvar_list=shock_vars,dist="normal",
                            dim=(50,2,2),labels=cs)

ret_scale = [pd.DataFrame({"country_sector": cs,
                           "rs_ind": rs}) for rs in np.arange(0.8,1.3,0.1)]
cost_params = list(product(*[shock_list,ret_scale,years]))

### Run simulation schedule - cost shock
change_df, volat_df = ps.simulation_schedule_para(
    param_list=cost_params,yearly_variables=yr_var_df,
    intermediates_df=intermediates_df,stages=5,propagation="down",
    core_cap=1,include_shocks=True)

### Merge prod_pos on on volat_df
volat_df = volat_df.merge(prod_pos_df,on=["year","country_sector"],how="left")

### Save results
change_df.to_csv(save_path+"changes_cost_shock_2x2.csv",index=False)
volat_df.to_csv(save_path+"results_cost_shock_2x2.csv",index=False)


### Set simulation parameters - demand shock
years = np.arange(0,len(IO_tabs))
c_shock_var = [[0,0,0]]
d_shock_var = [[0,0,0],[0,0,0.05],[0,0,0.1],[0,0,0.15], # Diff magnit. of i-shock
               [0,0.05,0],[0,0.1,0],[0,0.15,0], # Diff magnit. of s-shock
               [0.05,0,0],[0.1,0,0],[0.15,0,0], # Diff magnit. of c-shock
               [0,0.04,0.01],[0,0.025,0.025],[0,0.01,0.04], # Diff ratio of si-shock
               [0.04,0,0.01],[0.025,0,0.025],[0.01,0,0.04]] # Diff ratio of ci-shock
p_shock_var = [[0,0],[0,0.05],[0,0.1],[0,0.15]]
shock_vars = list(product(*[c_shock_var,d_shock_var,p_shock_var]))
shock_list = ps.shock_specs(shockvar_list=shock_vars,dist="normal",
                            dim=(50,2,2),labels=cs)

ret_scale = [pd.DataFrame({"country_sector": cs,
                           "rs_ind": rs}) for rs in np.arange(0.8,1.3,0.1)]
demand_params = list(product(*[shock_list,ret_scale,years]))

### Run simulation schedule - demand shock
change_df, volat_df = ps.simulation_schedule_para(
    param_list=demand_params,yearly_variables=yr_var_df,
    intermediates_df=intermediates_df,stages=5,propagation="up",
    core_cap=1,include_shocks=True)

### Merge prod_pos on on volat_df
volat_df = volat_df.merge(prod_pos_df,on=["year","country_sector"],how="left")

### Save results
change_df.to_csv(save_path+"changes_demand_shock_2x2.csv",index=False)
volat_df.to_csv(save_path+"results_demand_shock_2x2.csv",index=False)
        

#Run different combinations of returns to scale
#   - All CRS, all DRS, all IRS
#   - downstream CRS, upstream DRS - should be more volatile 
#     by default since actually vulnerable to demand shocks
#   - downstream CRS, upstream IRS - for upstream products, 
#     scaling is much more worth it

# Run shock on country and sector level
#   - Are single country shocks interesting? - need network dependence measures
#   - Might need to be able to set variance levels for countries/ sectors - coding!!!!