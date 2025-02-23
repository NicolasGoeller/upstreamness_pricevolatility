import numpy as np
import pandas as pd

def marginal_cost(prices,wage,productivity,final_demand,int_sales,input_shares,
                  cost_shock,demand_shock):
    
    ## Compute sum of input shares and separate share for labor and intermediates
    rs= np.sum(input_shares)
    labor_share = input_shares[0]
    int_shares = input_shares[1:]
    
    ## Look for input shares strictly 0 and adpat arrays
    null_inputs = int_shares != 0
    int_shares = int_shares[null_inputs]
    prices = prices[null_inputs.flatten()]
    
    ## Compute individual components to marginal cost function
    y = (demand_shock*final_demand + int_sales)**((1-rs)/rs)
    z = (1/productivity)**(1/rs)
    labor_cost = (wage*cost_shock)**(labor_share/rs)
    geom_prices = np.prod(prices**(int_shares/rs))
    geom_input = np.prod(1/input_shares**(input_shares/rs))

    ## Compute actual marginal cost function
    mc = y * z * labor_cost * geom_prices * geom_input
    
    return(mc)

def marginal_cost_vec(prices,wage,productivity,final_demand,int_sales,input_shares,
                      scale_returns,cost_shock,demand_shock):
    
    ## Compute sum of input shares and separate share for labor and intermediates
    nonint_share = input_shares[0,:]
    int_shares = input_shares[1:,:]
    dim_mat = np.ones(int_shares.shape)
    
    ## Compute individual components to marginal cost function
    y = (np.exp(demand_shock)*final_demand + int_sales)**((1-scale_returns)/scale_returns)
    #y = (demand_shock*final_demand+int_sales)**((1-scale_returns)/scale_returns)
    z = (1/productivity)**(nonint_share/scale_returns) ## Labor enhancing prod
    z = np.nan_to_num(z,copy=False, nan=1)
    labor_cost = (wage*np.exp(cost_shock))**(nonint_share/scale_returns)
    #labor_cost = (wage*cost_shock)**(nonint_share/scale_returns)

    geom_prices = np.prod((dim_mat*prices).T**(int_shares/scale_returns),axis=0)
    geom_prices = np.nan_to_num(geom_prices,copy=False, nan=1)
    geom_input = np.prod((1/input_shares)**(input_shares/scale_returns),axis=0)
    geom_input = np.nan_to_num(geom_input,copy=False, nan=1)

    #print("Demand: %s" %y)
    #print("Prod: %s" %z[0,0:4])
    #print("Labor cost: %s" %labor_cost[0,0:4])
    #print("Geom Price: %s" %geom_prices[0:4])
    #print("Geom Input: %s" %geom_input[0:4])

    ## Compute actual marginal cost function
    mc = y * z * labor_cost * geom_prices * geom_input
    
    return(mc)


def simulation_cost(params,yearly_variables,int_shares_df,#rs_ind_df,shock_vars,
                    start_price=np.array([]),conv_crit=0.001,attach_conv=False,
                    include_shocks=False, seed=123):
    
    ### Assign shock parameters
    shock_vars = params[0]
    rs_ind_df = params[1]
    
    #print("Cost: %s; Demand: %s; RS: %s" 
    #      %(str((shock_vars[2][0][1],shock_vars[2][1][1],shock_vars[2][2][1])), 
    #        str((shock_vars[3][0][1],shock_vars[3][1][1],shock_vars[3][2][1])),
    #        str(rs_ind_df["rs_ind"].unique())))
    
    ### Create indicator list if all inputs have a year column
    has_year = []
    for df in [yearly_variables,int_shares_df]:
        
        if "year" in df.columns[0]:
            has_year.append(True)
        else:
            has_year.append(False)
    
    ### Format input treatment based on year columns       
    if all(has_year): # If more than one year, create minimum set 
        year_list = np.intersect1d(yearly_variables["year"], int_shares_df["year"])
    elif not(all(has_year)): # If no year set to dummy
        year_list = [1]
        yearly_variables["year"] = 1
        int_shares_df["year"] = 1
    else: # If ambiguous years, reject inputs and stop
        print("unclear year indication.")
        return(None) 
    #print("test1")
    ### Set year indicator
    rs_ind_df["year"] = 1
            
    ### Create list of unique country-sectors
    countrysector_list = np.sort(yearly_variables["country_sector"].unique())
            
    ### Generate shock variables
    c_shock_vars = shock_vars[2]
    d_shock_vars = shock_vars[3]
    no_shock = np.array([np.repeat(0,len(countrysector_list))])

    if shock_vars[0] == "normal":
        cost_shock_df = normal_shock(dim=shock_vars[1],c_shock=c_shock_vars[0],
                                     s_shock=c_shock_vars[1],i_shock=c_shock_vars[2],
                                     labels=countrysector_list,years=year_list,seed=seed)
        demand_shock_df = normal_shock(dim=shock_vars[1],c_shock=d_shock_vars[0],
                                       s_shock=d_shock_vars[1],i_shock=d_shock_vars[2],
                                       labels=countrysector_list,years=year_list,seed=seed)
    elif shock_vars[0] == "uniform":
        cost_shock_df = uniform_shock(dim=shock_vars[1],c_shock=c_shock_vars[0],
                                      s_shock=c_shock_vars[1],i_shock=c_shock_vars[2],
                                      labels=countrysector_list,years=year_list,seed=seed)
        demand_shock_df = uniform_shock(dim=shock_vars[1],c_shock=d_shock_vars[0],
                                        s_shock=d_shock_vars[1],i_shock=d_shock_vars[2],
                                        labels=countrysector_list,years=year_list,seed=seed)

    ### Create simulated price dataset list
    psim_df_list = list()
    psim_change_list = list()
    #print("test2")
    for yr in year_list:
        #print(yr)
        ### Subset year of variables, sort for country_sector and create as vector
        wages = var_prep(yearly_variables,"wage",yr,"long")
        prod = var_prep(yearly_variables,"prod",yr,"long")
        final_demand = var_prep(yearly_variables,"final_demand",yr,"long")
        inter_sales = var_prep(yearly_variables,"inter_sales",yr,"long")
        nonint_shares = var_prep(yearly_variables,"nonint_share",yr,"long")
        int_shares = var_prep(int_shares_df,countrysector_list,yr,"wide") 
        
        ### Subset shock variables, sort for country_sector and create as vector
        cost_shock = var_prep(cost_shock_df,countrysector_list,yr,"wide")
        demand_shock = var_prep(demand_shock_df,countrysector_list,yr,"wide")
        
        ### Returns-to-scale indicators
        rs_ind = var_prep(rs_ind_df,"rs_ind",1,"long")
        
        ### Merge labor and intermediate shares
        input_shares = np.concatenate([nonint_shares,int_shares],axis=0)
        input_shares = input_shares * rs_ind
        #print("test3")
        ### Set length of iterations for each year
        if (len(year_list) == 1) & (year_list[0] == 1):
            iter_duration = shock_vars[1][0]+1
        else:
            iter_duration = 13
        
        ### If no start price given, run price convergence
        if start_price.size == 0:
            ## Create matrix for preliminary price convergence
            psim_prep = np.ones((1,len(countrysector_list)))*50
            psim_prepnext = marginal_cost_vec(psim_prep[0,:],wages,prod,final_demand,
                                              inter_sales,input_shares,rs_ind,
                                              no_shock,no_shock)
            psim_prep = np.concatenate([psim_prep,psim_prepnext],axis=0)
            price_diff = abs(psim_prep[-1,:] - psim_prep[-2,:])

            ## Run preliminary simulation until stable price level
            while any(price_diff > conv_crit):
                psim_prepnext = marginal_cost_vec(psim_prep[-1,:],wages,prod,final_demand,
                                                  inter_sales,input_shares,rs_ind,
                                                  no_shock,no_shock)
                psim_prep = np.concatenate([psim_prep,psim_prepnext])
                price_diff = abs(psim_prep[-1,:] - psim_prep[-2,:])
        else: 
            ## Use start_price if given
            psim_prep= start_price 
        #print("test4")
        ### Create empty matrix for actual price simulations
        psim = np.concatenate([np.array([np.arange(0,iter_duration)]).T,
                               np.zeros((iter_duration,len(countrysector_list)))],
                              axis=1)
        ### Attach last preliminary iteration as starting price
        psim[0,1:] = psim_prep[-1,:]            

        ### Run price simulations
        for mon in range(0,iter_duration-1):
            
            psim[mon+1,1:] = marginal_cost_vec(psim[mon,1:],wages,prod,final_demand,
                                               inter_sales,input_shares,rs_ind,
                                               cost_shock[mon,:],demand_shock[mon,:])
        
        ### Execute on indicator if convergence prices should be kept
        if attach_conv: # If yes, concat preliminary simulation on empty matrix
            psim_prep = np.concatenate([np.array([np.arange(-1*psim_prep.shape[0],0)+1]).T,
                                        psim_prep],axis=1)
            psim = np.concatenate([psim_prep[:-1,:], psim],axis=0)
            
        ### Create dataframe from simulation result matrix
        colnames = np.concatenate([["year","month"], countrysector_list])        
        psim = np.concatenate([np.array([np.repeat(yr,psim.shape[0])]).T,
                               psim],axis=1)
        psim_df = pd.DataFrame(psim,columns=colnames)
        
        ### Compute price changes
        change_df = psim_df.copy()

        change_df[countrysector_list] = change_df[countrysector_list].pct_change(1)
        ### Include only month above 0 for price changes (those affected by shocks)
        change_df = change_df.loc[change_df["month"] > 0,]
        
        ### Append dataframe to list
        psim_df_list.append(psim_df)
        psim_change_list.append(change_df)
    
    ### Merge list into one dataframe
    psim_level_df = pd.concat(psim_df_list, axis=0)
    psim_change_df = pd.concat(psim_change_list, axis=0)
    #print("test5")
    ### Transform relative change data into volatilities
    psim_volat_df = psim_change_df.groupby(["year"]).agg("std")
    psim_volat_df.reset_index(inplace=True, drop=False)
    psim_volat_df = psim_volat_df.melt(id_vars="year",value_vars=countrysector_list,
                                       value_name="price_volatility",var_name="country_sector")
    
    ### Pivot longer tables for level and change
    psim_level_df = psim_level_df.melt(id_vars=["year","month"],value_vars=countrysector_list,
                                       value_name="price_level",var_name="country_sector")
    psim_change_df = psim_change_df.melt(id_vars=["year","month"],value_vars=countrysector_list,
                                         value_name="price_change",var_name="country_sector")

    ### Merge shock variable draws on table if wanted
    if include_shocks:
        ## Melt shock tables into long form and merge together
        cost_shock_df = cost_shock_df.melt(id_vars=["year","month"],value_vars=countrysector_list,
                                           value_name="cost_shock",var_name="country_sector")
        demand_shock_df = demand_shock_df.melt(id_vars=["year","month"],value_vars=countrysector_list,
                                               value_name="demand_shock",var_name="country_sector")
        shock_df = cost_shock_df.merge(demand_shock_df,how="left",on=["year","month","country_sector"])
        ## Merge on price level table and fill NAs with 1 (for months < 0)
        psim_level_df = psim_level_df.merge(shock_df,how="left",on=["year","month","country_sector"])
        psim_level_df[["cost_shock","demand_shock"]].fillna(1,inplace=True)
        ## Merge on price change table (no fillna necessary)
        psim_change_df = psim_change_df.merge(shock_df,how="left",on=["year","month","country_sector"])
    #print("test6")
    ### If less than two distinct years given, exclude year variable
    if ((len(year_list) == 1) & (year_list[0] == 1)):
        psim_level_df.pop("year")
        psim_change_df.pop("year")
        psim_volat_df.pop("year")
    
    ### Remove year variable from rs_ind_df
    rs_ind_df.pop("year")
        
    ### Merge Returns-to-scale indicator on completed table
    psim_level_df = psim_level_df.merge(rs_ind_df,how="left",on="country_sector")
    psim_change_df = psim_change_df.merge(rs_ind_df,how="left",on="country_sector")
    psim_volat_df = psim_volat_df.merge(rs_ind_df,how="left",on="country_sector")
        
    ### Create shock variable columns
    psim_level_df = shock_labels(psim_level_df,shock_vars)
    psim_change_df = shock_labels(psim_change_df,shock_vars)
    psim_volat_df = shock_labels(psim_volat_df,shock_vars)
           
    return((psim_level_df,psim_change_df,psim_volat_df))
    #return(psim_volat_df)


def simulation_schedule(param_list,yearly_variables,int_shares_df,save_path,
                        name_mod,conv_crit=0.001,attach_conv=False,seed=123,
                        include_shocks=True):
    
    ### Set up result lists
    level_list = [pd.DataFrame() for i in param_list]
    change_list = [pd.DataFrame() for i in param_list]
    volat_list = [pd.DataFrame() for i in param_list]
    
    ### Generate country-sector list
    #country_sectors = input_data[0]["country_sector"].unique()
    
    ### Loop over elements of param_list
    for idx,par in enumerate(param_list):
        print("Parameter setting N° %s" %idx)
        #print(par[0],par[1],np.unique(par[2]))
        # Parameters
        #c_shocks = par[0]
        #d_shocks = par[1]
        #rs_ind_df = pd.DataFrame({"country_sector": country_sectors,
        #                          "rs_ind": par[2]})
    #
        #shock_vars = [shock_set[0],(shock_set[1],shock_set[2],shock_set[3]),
        #              [(0,c_shocks[0]),(0,c_shocks[1]),(0,c_shocks[2])],
        #              [(0,d_shocks[0]),(0,d_shocks[1]),(0,d_shocks[2])]]
    
        level_list[idx], change_list[idx], volat_list[idx] = simulation_cost(
            par,yearly_variables=yearly_variables,int_shares_df=int_shares_df,
            conv_crit=conv_crit,attach_conv=attach_conv,seed=seed,
            include_shocks=include_shocks)
        
    ### Concat result lists
    level_df = pd.concat(level_list,axis=0,ignore_index=True)
    change_df = pd.concat(change_list,axis=0,ignore_index=True)
    volat_df = pd.concat(volat_list,axis=0,ignore_index=True)
    
    ### Save result tables
    #level_df.to_csv(save_path+"levels_"+name_mod+".csv",index=False)
    #change_df.to_csv(save_path+"changes_"+name_mod+".csv",index=False)
    #volat_df.to_csv(save_path+"results_"+name_mod+".csv",index=False)
    
    return(level_df,change_df,volat_df)
