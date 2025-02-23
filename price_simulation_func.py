import numpy as np
import pandas as pd
from multiprocessing import cpu_count
from multiprocessing.pool import ThreadPool
from functools import partial
import tqdm

import warnings

warnings.filterwarnings('ignore')

### Define functions

def block_dominance(A,k,axis=0):

    ### Create empty list for results
    block_diag = []
    block_offdiag = []
    
    ### Iterate over matrix blocks
    for i in range(int(A.shape[0]/k)):
        ## Diagonal blocks
        diag_iter = A[(k*i):(k*(i+1)),(k*i):(k*(i+1))]
        block_diag.append(diag_iter)
        ## Off-diagonal blocks
        if axis == 0:
            offdiag_iter = np.concatenate([A[0:(k*i),(k*i):(k*(i+1))],
                                           A[k*(i+1):,(k*i):(k*(i+1))]],axis=0)
            block_offdiag.append(offdiag_iter)
        elif axis== 1:
            offdiag_iter = np.concatenate([A[(k*i):(k*(i+1)),0:(k*i)],
                                           A[(k*i):(k*(i+1)),k*(i+1):]],axis=1)
            block_offdiag.append(offdiag_iter)
    
    ### Create results as arrays    
    block_diag = np.array(block_diag)
    block_offdiag = np.array(block_offdiag)
    
    ### Concatenate and summ arrays along correct axis
    if axis == 0:
        block_diag = np.concatenate(block_diag,axis=1).sum(axis=0)
        block_offdiag = np.concatenate(block_offdiag,axis=1).sum(axis=0)
    elif axis == 1:
        block_diag = np.concatenate(block_diag,axis=0).sum(axis=1)
        block_offdiag = np.concatenate(block_offdiag,axis=0).sum(axis=1)
            
    return(block_diag,block_offdiag)
 
def var_prep(df,var,format,year=None):
    ### Copy input dataset
    data = df.copy()
    
    ### Check if var is str and wrap in list if yes
    if isinstance(var,str):
        var = [var]
    
    ### Subset df for values only in year, if year is present
    if pd.notna(year): 
        data = data.loc[data["year"] == year,]
    
    ### Transform all values to float
    data[var] = data[var].astype(float)
    
    ### If column is present, sort values for country_sector order
    if "country_sector" in data.columns:
        data.sort_values(by="country_sector",inplace=True,axis=0)
    
    ### Convert df to matrix 
    if format == "long": # If df is long, apply transpose
        data = data[var].to_numpy().T
    elif format == "wide": # If df is wide do not
        data = data[var].to_numpy()
    
    return(data)

def normal_shock(i_shock,dim,c_shock,s_shock,axis=1,labels=[None]):
    
    np.random.seed(123)
    
    ### Run shock draws for country, sector and idiosyncratic components
    if axis ==1: # Make draws in time dimension
        country_list = [np.random.normal(c_shock[0],c_shock[1],size=(dim[0],1)) for i in range(0,dim[1])]
        sector_list = [np.random.normal(s_shock[0],s_shock[1],size=(dim[0],1)) for i in range(0,dim[2])]
        idiosyn_list = [np.random.normal(i_shock[0],i_shock[1],size=(dim[0],1)) for i in range(0,dim[1]*dim[2])]

        country_draw = np.concatenate(country_list,axis=1)
        sector_draw = np.concatenate(sector_list,axis=1)
        idiosyn_draw = np.concatenate(idiosyn_list,axis=1)
        
    elif axis == 0: # Make draws in cross-sectional dimension
        country_list = [np.random.normal(c_shock[0],c_shock[1],size=(1,dim[1])) for i in range(0,dim[0])]
        sector_list = [np.random.normal(s_shock[0],s_shock[1],size=(1,dim[2])) for i in range(0,dim[0])]
        idiosyn_list = [np.random.normal(i_shock[0],i_shock[1],size=(1,dim[1]*dim[2])) for i in range(0,dim[0])]

        country_draw = np.concatenate(country_list,axis=0)
        sector_draw = np.concatenate(sector_list,axis=0)
        sector_draw = np.concatenate(idiosyn_list,axis=0)
        
    ### Replace <-1 with -1
    country_draw[country_draw < -1] = -1
    sector_draw[sector_draw < -1] = -1
    sector_draw[sector_draw < -1] = -1

    ### Stretch components for country and sector 
    country_mat = np.repeat(country_draw,repeats=dim[2],axis=1)
    sector_mat = np.tile(sector_draw,reps=(1,dim[1]))

    ### Add all components together
    shock_mat = country_mat + sector_mat + idiosyn_draw    

    ### Check for existence of labels
    if any(pd.isna(labels)): # If not, create basic labels
        labels = ["s"+str(i) for i in np.arange(1,dim[1]*dim[2]+1)]
    elif len(labels) != (dim[1]*dim[2]): # If wrong number of labels, stop
        print(dim[1]*dim[2])
        print("Wrong number of labels.")
        return(None)
    
    ### Create shock dataframe
    shock_df = pd.DataFrame(shock_mat,columns=labels)
    ### Create variable to indicate draws rounds
    shock_df.reset_index(drop=False,inplace=True)
    shock_df.rename(columns={"index":"round"},inplace=True)
        
    return(shock_df)

def uniform_shock(dim,c_shock,s_shock,i_shock,axis=1,labels=[None]):
    
    ### Set up basic shock matrix dimensions
    shock_mat = np.zeros((dim[0],dim[1]*dim[2]))
    
    ### Run shock draws for country, sector and idiosyncratic components
    if axis ==1: # Make draws in time dimension
        country_list = [np.random.uniform(c_shock[0],c_shock[1],size=(dim[0],1)) for i in range(0,dim[1])]
        sector_list = [np.random.uniform(s_shock[0],s_shock[1],size=(dim[0],1)) for i in range(0,dim[2])]
        idiosyn_list = [np.random.uniform(i_shock[0],i_shock[1],size=(dim[0],1)) for i in range(0,dim[1]*dim[2])]

        country_draw = np.concatenate(country_list,axis=1)
        sector_draw = np.concatenate(sector_list,axis=1)
        idiosyn_draw = np.concatenate(idiosyn_list,axis=1)
        
    elif axis == 0: # Make draws in cross-sectional dimension
        country_list = [np.random.uniform(c_shock[0],c_shock[1],size=(1,dim[1])) for i in range(0,dim[0])]
        sector_list = [np.random.uniform(s_shock[0],s_shock[1],size=(1,dim[2])) for i in range(0,dim[0])]
        idiosyn_list = [np.random.uniform(i_shock[0],i_shock[1],size=(1,dim[1]*dim[2])) for i in range(0,dim[0])]

        country_draw = np.concatenate(country_list,axis=0)
        sector_draw = np.concatenate(sector_list,axis=0)
        idiosyn_draw = np.concatenate(idiosyn_list,axis=0)
         
    ### Stretch components for country and sector 
    country_mat = np.repeat(country_draw,repeats=dim[2],axis=1)
    sector_mat = np.tile(sector_draw,reps=(1,dim[1]))

    ### Add all components together
    shock_mat = country_mat + sector_mat + idiosyn_draw    

    ### Check for existence of labels
    if any(pd.isna(labels)): # If not, create basic labels
        labels = ["s"+str(i) for i in np.arange(1,dim[1]*dim[2]+1)]
    elif len(labels) != (dim[1]*dim[2]): # If wrong number of labels, stop
        print(dim[1]*dim[2])
        print("Wrong number of labels.")
        return(None)
    
    ### Create shock dataframe
    shock_df = pd.DataFrame(shock_mat,columns=labels)
    ### Create variable to indicate draws rounds
    shock_df.reset_index(drop=False,inplace=True,names="round")
    shock_df.rename(columns={"index":"round"},inplace=True)
    
    return(shock_df)

def shock_specs(shockvar_list,dist,dim,seed=123,labels=[None]):
    
    shock_list = []
    
    for spec in shockvar_list:
        
        ### Set random seed
        np.random.seed(seed)
        
        ### Generate shock spec variables
        c_shock_vars = [(0,spec[0][0]),(0,spec[0][1]),(0,spec[0][2])]
        d_shock_vars = [(0,spec[1][0]),(0,spec[1][1]),(0,spec[1][2])]
        p_shock_vars = (spec[2][0],spec[2][1])
        
        ### Generate shock draws according to spec
        if dist == "normal":
            cost_shock_df = normal_shock(dim=dim,c_shock=c_shock_vars[0],
                                         s_shock=c_shock_vars[1],
                                         i_shock=c_shock_vars[2],
                                         labels=labels)
            demand_shock_df = normal_shock(dim=dim,c_shock=d_shock_vars[0],
                                           s_shock=d_shock_vars[1],
                                           i_shock=d_shock_vars[2],
                                           labels=labels)
            price_shock_df = normal_shock(dim=dim,c_shock=(0,0),s_shock=(0,0),
                                          i_shock=p_shock_vars,axis=0,
                                          labels=labels)
        elif dist == "uniform":
            cost_shock_df = uniform_shock(dim=dim,c_shock=c_shock_vars[0],
                                          s_shock=c_shock_vars[1],
                                          i_shock=c_shock_vars[2],
                                          labels=labels)
            demand_shock_df = uniform_shock(dim=dim,c_shock=d_shock_vars[0],
                                            s_shock=d_shock_vars[1],
                                            i_shock=d_shock_vars[2],
                                            labels=labels)
            price_shock_df = uniform_shock(dim=dim,c_shock=(0,0),s_shock=(0,0),
                                           i_shock=p_shock_vars,axis=0,
                                           labels=labels)
        
        ### Generate shock labels
        c_var = [dist,str(c_shock_vars[0][1]),str(c_shock_vars[1][1]),
             str(c_shock_vars[2][1])]
        d_var = [dist,str(d_shock_vars[0][1]),str(d_shock_vars[1][1]),
             str(d_shock_vars[2][1])]
        p_var = "N("+str(p_shock_vars[0])+", "+str(p_shock_vars[1])+")"
            
        ### Bring shock draws together in dictionary with labels
        shock_dict = {"cost_label": "-".join(c_var),
                      "cost_shock": cost_shock_df,
                      "demand_label": "-".join(d_var),
                      "demand_shock": demand_shock_df,
                      "price_label": p_var,
                      "price_shock": price_shock_df}
        
        ### Append iter on shoc
        shock_list.append(shock_dict)

    return(shock_list)

def buyer_change(supplier_change,demand_shock,cost_shock,non_int_shares,
                 demand_shares,intermediates,output,scale_returns):
    
    ### Prepare matrix formats
    I = np.identity(scale_returns.shape[1])
    Gamma = I * scale_returns
    Gamma_inv = np.linalg.inv(Gamma)
    Beta0 = I * non_int_shares
    Psi = I * demand_shares
    X_hat = np.linalg.inv(I * output)
    A = intermediates @ X_hat
    #print(A)

    ### Calculate components
    d_demand = (Gamma_inv - I) @ Psi @ demand_shock
    d_cost = Gamma_inv @ Beta0 @ cost_shock
    d_prices = supplier_change @ A @ Gamma_inv
    #d_demand = (Gamma_inv - I) @ Psi @ demand_shock
    #d_cost = Gamma_inv @ Beta0 @ cost_shock
    #d_prices = Gamma_inv @ int_shares @ supplier_change
    #print("Price changes: %d" %d_prices)
    
    ### Compute actual buyer price changes
    buyer_change = d_demand + d_cost + d_prices
    
    return(buyer_change)

def supplier_change(buyer_change,demand_shock,cost_shock,non_int_shares,
                    demand_shares,intermediates,output,scale_returns):
    
    ### Prepare matrix formats
    I = np.identity(scale_returns.shape[1])
    Gamma = I * scale_returns
    Gamma_inv = np.linalg.inv(Gamma)
    Beta0 = I * non_int_shares
    Psi = I * demand_shares
    X_hat = np.linalg.inv(I * output)
    B = X_hat @ intermediates
    
    ### Calculate components
    d_demand = (Gamma_inv - I) @ Psi @ demand_shock
    #old_demand = Gamma_inv @ (I - Gamma) @ demand_shock
    #print("New D comp: %s; Old D comp: %s" %(d_demand,old_demand))
    d_cost = Gamma_inv @ Beta0 @ cost_shock
    #inverse_comp = Gamma @ np.linalg.inv(int_shares)
    prop_comp = Gamma_inv @ B
    
    ### Compute actual supplier price changes
    supplier_change = prop_comp @ (buyer_change - d_demand - d_cost) #@ inverse_comp
    
    return(supplier_change)

def simulation_changes(params,yearly_variables,intermediates_df,propagation="down",
                       stages=1,include_shocks=True,seed=123):

    ### Set random seed
    np.random.seed(seed)

    ### Assign shock parameters
    cost_shock_df = params[0]["cost_shock"]
    demand_shock_df = params[0]["demand_shock"]
    price_shock_df = params[0]["price_shock"]
    rs_ind_df = params[1]
    year = params[2]
    
    ### Create indicator list if all inputs have a year column
    has_year = []
    for df in [yearly_variables,intermediates_df]:
        if "year" in df.columns:
            has_year.append(True)
        else:
            has_year.append(False)

    ### Format input treatment based on year columns       
    if not(all(has_year)): # If no year set to dummy
        yearly_variables["year"] = 1
        intermediates_df["year"] = 1
            
    ### Create list of unique country-sectors
    countrysector_list = np.sort(yearly_variables["country_sector"].unique())
     
    ### Subset simulation inputs
    demand_shares = var_prep(df=yearly_variables,var="demand_share",format="long",year=year)
    nonint_shares = var_prep(df=yearly_variables,var="nonint_share",format="long",year=year)
    output = var_prep(df=yearly_variables,var="total_output",format="long",year=year)
    intermediates = var_prep(df=intermediates_df,var=countrysector_list,format="wide",year=year)
    
    ### Subset shock variables, sort for country_sector and create as vector
    price_shock = var_prep(price_shock_df,countrysector_list,"wide")
    demand_shock = var_prep(demand_shock_df,countrysector_list,"wide")
    cost_shock = var_prep(cost_shock_df,countrysector_list,"wide")
    
    ### Returns-to-scale indicators
    rs_ind = var_prep(rs_ind_df,"rs_ind","long")

    ### Create array for price changes
    sim_change = np.zeros((stages+1,price_shock.shape[0],price_shock.shape[1]))
    sim_change[0,:,:] = price_shock

    ### Create datasets to merge on
    sim_change_df = pd.DataFrame({"year":year,
                                  "round": np.repeat(np.arange(0,price_shock.shape[0]),len(countrysector_list)),
                                  "country_sector":np.tile(countrysector_list,price_shock.shape[0])})
    sim_volat_df = pd.DataFrame({"year":year,
                                 "country_sector":countrysector_list})
    
    ### Iterate over stages up or down the chain
    for stg in range(1,stages+1):
        ### Iterate over various price and shock draws
        for iter in range(0,price_shock.shape[0]):
            ### Compute downstream changes
            if propagation == "down":
                #print(sim_change[stg-1,iter,:])
                sim_change[stg,iter,:] = buyer_change(supplier_change=sim_change[stg-1,iter,:],
                                                      demand_shock=demand_shock[iter,:],
                                                      cost_shock=cost_shock[iter,:],
                                                      non_int_shares=nonint_shares,
                                                      demand_shares=demand_shares,
                                                      intermediates=intermediates,
                                                      output=output,
                                                      scale_returns=rs_ind)
            ### Compute upstream changes
            elif propagation == "up":
                sim_change[stg,iter,:] = supplier_change(buyer_change=sim_change[stg-1,iter,:],
                                                         demand_shock=demand_shock[iter,:],
                                                         cost_shock=cost_shock[iter,:],
                                                         non_int_shares=nonint_shares,
                                                         demand_shares=demand_shares,
                                                         intermediates=intermediates,
                                                         output=output,
                                                         scale_returns=rs_ind)
        
        ### Create dataframe from matrix
        stg_change_df = pd.DataFrame(sim_change[stg,:,:],columns=countrysector_list)
        ### Create variable to indicate draws rounds
        stg_change_df.reset_index(drop=False,inplace=True)
        stg_change_df.rename(columns={"index":"round"},inplace=True)
        ### Melt Table in long form
        stg_change_df = stg_change_df.melt(id_vars=["round"],value_vars=countrysector_list,
                                           value_name="price_change",var_name="country_sector")
              
        ### Transform relative change data into volatilities
        stg_volat_df = stg_change_df[["country_sector","price_change"]].groupby(
                ["country_sector"]).agg("std")
        stg_volat_df.reset_index(drop=False,inplace=True)
        stg_volat_df.rename(columns={"price_change":"price_volat"},inplace=True)
        
        ### If iteration occurs for more than one stage, rename the price variables
        if stages > 1:
            stg_change_df.rename(columns={"price_change":"price_change_"+str(stg)},inplace=True)
            stg_volat_df.rename(columns={"price_volat":"price_volat_"+str(stg)},inplace=True)
    
        ### Merge stage tables on complete tables
        sim_change_df = sim_change_df.merge(stg_change_df,how="left",on=["country_sector","round"])
        sim_volat_df = sim_volat_df.merge(stg_volat_df,how="left",on=["country_sector"])
        
    ### Merge shock variable draws on table if wanted
    if include_shocks:
        ## Melt shock tables into long form and merge together
        cost_shock_df = cost_shock_df.melt(id_vars=["round"],value_vars=countrysector_list,
                                           value_name="cost_shock",var_name="country_sector")
        demand_shock_df = demand_shock_df.melt(id_vars=["round"],value_vars=countrysector_list,
                                               value_name="demand_shock",var_name="country_sector")
        price_shock_df = price_shock_df.melt(id_vars=["round"],value_vars=countrysector_list,
                                             value_name="price_shock",var_name="country_sector")
        shock_df = cost_shock_df.merge(demand_shock_df,how="left",on=["round","country_sector"])
        shock_df = shock_df.merge(price_shock_df,how="left",on=["round","country_sector"])
        ## Merge on price change table (no fillna necessary)
        sim_change_df = sim_change_df.merge(shock_df,how="left",on=["round","country_sector"])
        
    ### Merge Returns-to-scale indicator on completed table
    sim_change_df = sim_change_df.merge(rs_ind_df,how="left",on="country_sector")
    sim_volat_df = sim_volat_df.merge(rs_ind_df,how="left",on="country_sector")
        
    ### Create shock variable columns
    sim_change_df["c_shock_var"] = params[0]["cost_label"]
    sim_change_df["d_shock_var"] = params[0]["demand_label"]
    sim_change_df["p_shock_var"] = params[0]["price_label"]
    
    sim_volat_df["c_shock_var"] = params[0]["cost_label"]
    sim_volat_df["d_shock_var"] = params[0]["demand_label"]
    sim_volat_df["p_shock_var"] = params[0]["price_label"]
        
    return((sim_change_df,sim_volat_df))

def simulation_schedule_para(param_list,yearly_variables,intermediates_df,
                             stages=1,propagation="down",save_path=None,
                             name_mod=None,seed=123,include_shocks=True,
                             core_cap=0.25,scenario_index=False):
    
    ### Parallelize processes over param_list
    #if __name__ == "__main__":
    p = ThreadPool(processes=int(cpu_count()*core_cap)) #Choose number of processes according to number of available cores
    results = list(tqdm.tqdm(p.imap_unordered(partial(simulation_changes,
        yearly_variables=yearly_variables,intermediates_df=intermediates_df,
        stages=stages,seed=seed,propagation=propagation,
        include_shocks=include_shocks), #fixed params in the partial()
        param_list), #process-variable params in this list
        total=len(param_list))) # Include process measuring
    p.close()
    p.join()
    
    ### Separate results into respective lists
    if scenario_index: # Add scenario index if needed
        change_list = [tab[0].assign(scenario = idx) for idx,tab in enumerate(results)]
        volat_list = [tab[1].assign(scenario = idx) for idx,tab in enumerate(results)]
    else: # Else leave it out
        change_list = [tab[0] for tab in results]
        volat_list = [tab[1] for tab in results]

    ### Concat result lists
    change_df = pd.concat(change_list,axis=0,ignore_index=True)
    volat_df = pd.concat(volat_list,axis=0,ignore_index=True)
    
    ### Save result tables
    if pd.notna(save_path) & pd.notna(name_mod):
        change_df.to_csv(save_path+"changes_"+name_mod+".csv",index=False)
        volat_df.to_csv(save_path+"results_"+name_mod+".csv",index=False)
        return()
    else:
        return(change_df,volat_df)



#import numpy as np
#import pandas as pd
#from itertools import product

#import price_simulation_func as ps

#####################################################################################
############################# 31country, 17 sector model ############################
#####################################################################################

#base_path = "C:/Users/Nicolas/OneDrive - Zeppelin-University gGmbH/Dokumente/Studium/Masterthesis/upstreamness_pricevolatility/"
#save_path = base_path+"/output/"
#
#### Set baseline param lists
#years = np.arange(1995,2012)
#
#### Import final demand and total inter use sales - annual
#wiod_long = pd.read_stata(base_path+"base/ALS_replication/analysis/WIOD/reduced_WIOD.dta")
#yr_var_df = pd.read_csv(base_path+"output/yearly_input_variables.csv")
#yr_var_df = yr_var_df.loc[yr_var_df["year"].isin(years),]
#
#### Create country-sector list
#countries = wiod_long.country.unique()
##countries = countries[countries != "ROW"]
#sectors = wiod_long.country_ind.unique()
#country_sectors = np.array(list(product(*[countries,sectors])))
#country_sectors = [i[0]+"_"+i[1] for i in country_sectors]
#
#### Import intermediate shares - depending on normalisation
#int_norm = "labor"
#if int_norm == "labor":
#    int_shares_norm_df = pd.read_csv(save_path+"intermediate_shares_labornorm.csv")
#    yr_var_df.rename(columns={"labor_share_norm":"nonint_share"},inplace=True)
#elif int_norm == "va":
#    int_shares_norm_df = pd.read_csv(save_path+"intermediate_shares_vanorm.csv")
#    yr_var_df.rename(columns={"va_share_norm":"nonint_share"},inplace=True)
#
#
##################### Shock Simulation Schedules ###############################
#
#### Set simulation parameters - cost shock
#c_shock_var = [[0,0,0.05],[0,0,0.1],[0,0,0.15], # Diff magnit. of i-shock
#               [0,0.05,0],[0,0.1,0],[0,0.15,0], # Diff magnit. of s-shock
#               [0.05,0,0],[0.1,0,0],[0.15,0,0], # Diff magnit. of c-shock
#               [0,0.04,0.01],[0,0.025,0.025],[0,0.01,0.04], # Diff ratio of si-shock
#               [0.04,0,0.01],[0.025,0,0.025],[0.01,0,0.04]] # Diff ratio of ci-shock
#ret_scale = [pd.DataFrame({"country_sector": country_sectors,
#                           "rs_ind": rs}) for rs in np.arange(0.8,1.3,0.1)]
#shock_vars = [["normal",(50,len(countries),len(sectors)),
#              [(0,shock[0]),(0,shock[1]),(0,shock[2])],
#              [(0,0),(0,0),(0,0)],(0,0.05)] for shock in c_shock_var]
#params = list(product(*[shock_vars,ret_scale,years]))
#
#change,volat = simulation_changes(params[0],yr_var_df,int_shares_norm_df,stages=3)

#if __name__ == "__main__":
#    p = ThreadPool(processes=int(cpu_count()*0.75)) # Choose number of processes according to number of available cores
#    results = list(tqdm.tqdm(p.imap_unordered(partial(simulation_changes,
#        yearly_variables=yr_var_df,int_shares_df=int_shares_norm_df,
#        include_shocks=True), # Fixed params in the partial()
#        params),  # Process-variable params in this list
#        total=len(params))) # Include process measuring
#    p.close()
#    p.join()
#    
#change_list = [i[0] for i in results]
#volat_list = [i[1] for i in results]
#
#### Concat result lists
#change_df = pd.concat(change_list,axis=0,ignore_index=True)
#volat_df = pd.concat(volat_list,axis=0,ignore_index=True)

### Run simulation schedule - cost shock
#test1,test2 = simulation_schedule_para(param_list=params,yearly_variables=yr_var_df,
#                                       int_shares_df=int_shares_norm_df,
#                                       save_path=save_path,name_mod="test",
#                                       core_cap=0.75)

#,shock_set=shock_set,save_path=save_path,name_mod="cost_shock_31x17"

############################# Test 2x2 model
#import numpy as np
#import pandas as pd
#from itertools import product
#import price_simulation_func as ps

#Z = np.array([[250,200,150,0],
#              [150,100,100,50],
#              [200,50,300,250],
#              [150,50,200,150]])
#
#cs = np.array(["A_s1","A_s2","B_s1","B_s2"])
#
#X = np.array([1000,500,1200,700])
#F = np.array([400,100,400,150])
#X_hat = np.diag(1/X)
#I = np.identity(len(X))
#
#A = Z @ X_hat
#L = np.linalg.inv(I-A)
#B = X_hat @ Z
#G = np.linalg.inv(I-B)
#
#VA = X - Z.sum(axis=0)
#INT = Z.sum(axis=0)
#
#prod = np.array([1,1,1,1])
#lab_share = VA@X_hat
#
#P = L.T @ lab_share
#wage = np.array([1,1,1,1])
#
##### Bring inputs in shape for function
#
#save_path = "C:/Users/Nicolas/OneDrive - Zeppelin-University gGmbH/Dokumente/Studium/Masterthesis/upstreamness_pricevolatility/output/"
#c_level_df = pd.read_csv(save_path+"cost_levels_2x2.csv")
#c_level_df = c_level_df.loc[c_level_df["c_shock_var"] == "normal-0-0-0.1",]
#price_series = c_level_df.pivot_table(columns="country_sector",
#                                    values="price_level",
#                                    index=["month","rs_ind"])
#price_series.reset_index(inplace=True,drop=False)
#price_series.rename_axis('',axis=1,inplace=True)
#
#yr_var = np.concatenate([cs,F,INT,lab_share,wage,prod])
#yr_var = np.resize(yr_var,(6,4)).T
#yr_var_df = pd.DataFrame(yr_var,
#                         columns=["country_sector","final_demand",
#                                  "inter_sales","nonint_share","wage",
#                                  "prod"])
#
#int_shares = np.concatenate([np.array([cs]).T,A],axis=1)
#cols = np.concatenate([["country_sector"],cs])
#int_shares_df = pd.DataFrame(int_shares, columns=cols)
#

##### Technical questions:
## Are 1-month lags the most appropriate option (YoY might be better)
## Are different timeframes than years to give the yearly variables need - e.g. 5Y
## Same as (2) but for volatility calculations
        
