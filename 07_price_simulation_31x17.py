import numpy as np
import pandas as pd
from itertools import product
import price_simulation_func as ps

#####################################################################################
############################# 31country, 17 sector model ############################
#####################################################################################

base_path = "C:/Users/Nicolas/OneDrive - Zeppelin-University gGmbH/Dokumente/Studium/Masterthesis/upstreamness_pricevolatility/"
save_path = base_path+"/output/"

### Set baseline param lists
years = np.arange(1995,2012)
#years = np.arange(1995,2012,2)

### Import final demand and total inter use sales - annual
wiod_long = pd.read_stata(base_path+"base/ALS_replication/analysis/WIOD/reduced_WIOD.dta")
yr_var_df = pd.read_csv(base_path+"output/yearly_input_variables.csv")
yr_var_df = yr_var_df.loc[yr_var_df["year"].isin(years),]

### Create country-sector list
countries = wiod_long.country.unique()
#countries = countries[countries != "ROW"]
sectors = wiod_long.country_ind.unique()
country_sectors = np.array(list(product(*[countries,sectors])))
country_sectors = [i[0]+"_"+i[1] for i in country_sectors]

### Import intermediate shares - depending on normalisation
int_norm = "labor"
if int_norm == "labor":
    int_shares_norm_df = pd.read_csv(save_path+"intermediate_shares_labornorm.csv")
    int_shares_norm_df = int_shares_norm_df.loc[int_shares_norm_df["year"].isin(years),]
    yr_var_df.rename(columns={"labor_share_norm":"nonint_share"},inplace=True)
elif int_norm == "va":
    int_shares_norm_df = pd.read_csv(save_path+"intermediate_shares_vanorm.csv")
    int_shares_norm_df = int_shares_norm_df.loc[int_shares_norm_df["year"].isin(years),]
    yr_var_df.rename(columns={"va_share_norm":"nonint_share"},inplace=True)


####################### Shock Simulation Schedules ###############################

### Set simulation parameters - cost shock
d_shock_var = [[0,0,0]]
p_shock_var = [[0,0],[0,0.05],[0,0.1],[0,0.15]]
ret_scale = [pd.DataFrame({"country_sector": country_sectors,
                           "rs_ind": rs}) for rs in np.arange(0.8,1.3,0.1)]

### Create shock list 1 - idiosyncratic shocks
c_var1 = [[0,0,0],[0,0,0.05],[0,0,0.1],[0,0,0.15]]#, # Diff magnit. of i-shock
c_shock_vars1 = list(product(*[c_var1,d_shock_var,p_shock_var]))
c_shock_list1 = ps.shock_specs(shockvar_list=c_shock_vars1,dist="normal",
                               dim=(50,len(countries),len(sectors)),
                               labels=country_sectors)
cost_params1 = list(product(*[c_shock_list1,ret_scale,years]))

### Run simulation schedule - cost shock 1
ps.simulation_schedule_para(param_list=cost_params1,yearly_variables=yr_var_df,
                            int_shares_df=int_shares_norm_df,stages=3,
                            core_cap=0.75,save_path=save_path,
                            name_mod="idio_cost_shock_31x17")

### Create shock list 2 - country and sector shocks
c_var2 = [[0,0.05,0],[0,0.1,0],[0,0.15,0], # Diff magnit. of s-shock
          [0.05,0,0],[0.1,0,0],[0.15,0,0]]#, # Diff magnit. of c-shock
c_shock_vars2 = list(product(*[c_var2,d_shock_var,p_shock_var]))
c_shock_list2 = ps.shock_specs(shockvar_list=c_shock_vars2,dist="normal",
                               dim=(50,len(countries),len(sectors)),
                               labels=country_sectors)
cost_params2 = list(product(*[c_shock_list2,ret_scale,years]))

### Run simulation schedule - cost shock 1
ps.simulation_schedule_para(param_list=cost_params2,yearly_variables=yr_var_df,
                            int_shares_df=int_shares_norm_df,stages=3,
                            core_cap=0.75,save_path=save_path,
                            name_mod="country_sector_cost_shock_31x17")

### Create shock list 3 - mixed origins
c_var3 = [[0,0.04,0.01],[0,0.025,0.025],[0,0.01,0.04], # Diff ratio of si-shock
          [0.04,0,0.01],[0.025,0,0.025],[0.01,0,0.04]] # Diff ratio of ci-shock
c_shock_vars3 = list(product(*[c_var3,d_shock_var,p_shock_var]))
c_shock_list3 = ps.shock_specs(shockvar_list=c_shock_vars3,dist="normal",
                               dim=(50,len(countries),len(sectors)),
                               labels=country_sectors)
cost_params3 = list(product(*[c_shock_list3,ret_scale,years]))

### Run simulation schedule - cost shock 3
ps.simulation_schedule_para(param_list=cost_params3,yearly_variables=yr_var_df,
                            int_shares_df=int_shares_norm_df,stages=3,
                            core_cap=0.75,save_path=save_path,
                            name_mod="mixed_origin_cost_shock_31x17")


### Set simulation parameters - demand shock
c_shock_var = [[0,0,0]]
p_shock_var = [[0.05,0.05],[0.05,0.1],[0.05,0.15]]
ret_scale = [pd.DataFrame({"country_sector": country_sectors,
                           "rs_ind": rs}) for rs in np.arange(0.8,1.3,0.1)]

### Create shock list 1 - idiosyncratic shocks
d_var1 = [[0,0,0],[0,0,0.05],[0,0,0.1],[0,0,0.15]] # Diff ratio of i-shock

d_shock_vars1 = list(product(*[c_shock_var,d_var1,p_shock_var]))
d_shock_list1 = ps.shock_specs(shockvar_list=d_shock_vars1,dist="normal",
                               dim=(50,len(countries),len(sectors)),
                               labels=country_sectors)
demand_params1 = list(product(*[d_shock_list1,ret_scale,years]))

### Run simulation schedule - demand shock 1
ps.simulation_schedule_para(param_list=demand_params1,yearly_variables=yr_var_df,
                            int_shares_df=int_shares_norm_df,stages=3,
                            core_cap=0.75,save_path=save_path,
                            name_mod="idio_demand_shock_31x17")

### Create shock list 2 - country and sector shocks
d_var2 = [[0,0.05,0],[0,0.1,0],[0,0.15,0], # Diff magnit. of s-shock
          [0.05,0,0],[0.1,0,0],[0.15,0,0]] # Diff magnit. of c-shock
               
d_shock_vars2 = list(product(*[c_shock_var,d_var2,p_shock_var]))
d_shock_list2 = ps.shock_specs(shockvar_list=d_shock_vars2,dist="normal",
                               dim=(50,len(countries),len(sectors)),
                               labels=country_sectors)
demand_params2 = list(product(*[d_shock_list2,ret_scale,years]))

### Run simulation schedule - demand shock 2
ps.simulation_schedule_para(param_list=demand_params2,yearly_variables=yr_var_df,
                            int_shares_df=int_shares_norm_df,stages=3,
                            core_cap=0.75,save_path=save_path,
                            name_mod="country_sector_demand_shock_31x17")

### Create shock list 3 - mixed origin shocks
d_var3 = [[0,0.04,0.01],[0,0.025,0.025],[0,0.01,0.04], # Diff ratio of si-shock
          [0.04,0,0.01],[0.025,0,0.025],[0.01,0,0.04]] # Diff ratio of ci-shock

d_shock_vars3 = list(product(*[c_shock_var,d_var3,p_shock_var]))
d_shock_list3 = ps.shock_specs(shockvar_list=d_shock_vars3,dist="normal",
                               dim=(50,len(countries),len(sectors)),
                               labels=country_sectors)
demand_params3 = list(product(*[d_shock_list3,ret_scale,years]))

### Run simulation schedule - demand shock 3
ps.simulation_schedule_para(param_list=demand_params3,yearly_variables=yr_var_df,
                            int_shares_df=int_shares_norm_df,stages=3,
                            core_cap=0.75,save_path=save_path,
                            name_mod="mixed_origin_demand_shock_31x17")

### Set simulation parameters - mixed shock
#c_shock_var = [[0,0,0.025],[0,0,0.05],[0,0,0.075],
#               [0,0.025,0],[0,0.05,0],[0,0.075,0],
#               [0.025,0,0],[0.05,0,0],[0.075,0,0]]
#d_shock_var = [[0,0,0.025],[0,0,0.05],[0,0,0.075],
#               [0,0.025,0],[0,0.05,0],[0,0.075,0],
#               [0.025,0,0],[0.05,0,0],[0.075,0,0]]
#p_shock_var = [[0.05,0.05],[0.05,0.1],[0.05,0.15]]
#m_shock_var = list(zip(c_shock_var,d_shock_var))
#shock_vars = list(product(*[m_shock_var,p_shock_var]))
#shock_vars = [[*i[0],i[1]] for i in shock_vars]
#shock_vars = list(product(*[c_shock_var,d_shock_var,p_shock_var]))
#shock_list = ps.shock_specs(shockvar_list=shock_vars,dist="normal",
#                            dim=(50,len(countries),len(sectors)),
#                            labels=country_sectors)
#
#ret_scale = [pd.DataFrame({"country_sector": country_sectors,
#                           "rs_ind": rs}) for rs in np.arange(0.8,1.3,0.1)]
#mixed_params = list(product(*[shock_list,ret_scale,years]))
#mixed_params = [[*i[0],i[1],i[2]] for i in mixed_params]
#
#### Run simulation schedule - mixed shock
#ps.simulation_schedule_para(param_list=mixed_params,yearly_variables=yr_var_df,
#                            int_shares_df=int_shares_norm_df,save_path=save_path,
#                            name_mod="mixed_shock_31x17",core_cap=0.75)


### Do I just want cross-sections or do I want trend
##would be interesting to show: pattern in data, simple model, condition in order to be consistent with pattern
