import numpy as np
from itertools import product
import pandas as pd
import price_simulation_func as ps

base_path = "C:/Users/Nicolas/OneDrive - Zeppelin-University gGmbH/Dokumente/Studium/Masterthesis/upstreamness_pricevolatility/"
save_path = base_path+"/output/"

############################################ Production network position #################################

### Prepare base table
years = np.arange(1995,2012)

prod_pos_list = [pd.DataFrame() for i in years]

for yr in years:
    print(yr)
    
    ### Read IO table and format
    path = base_path+"base/ALS_replication/data/3_WIOD/Intercountry Input-Output Table for "+str(yr)+".csv"
    wiod = pd.read_csv(path)
    wiod = wiod.loc[wiod["COUNTRY"].notna() & wiod["CODE"].notna(),]
    wiod.loc[wiod["COUNTRY"] == "ROU","COUNTRY"] = "ROM"
    
    ### Compile country-sectors
    country_sectors = wiod["COUNTRY"] + "_" + wiod["CODE"].str.lower()
    
    ### Recover intermediate inputs and total output
    X = wiod["TOT"].to_numpy()
    X_hat = np.diag(1/X)
    X_hat = np.nan_to_num(X_hat,0)
    Z = wiod[wiod.columns[
        wiod.columns.str.startswith("INT_")]].to_numpy()

    ### Calculate technical coefficients
    A = Z @ X_hat
    B = X_hat @ Z
    
    ### Invert to L,G matrices    
    I = np.identity(len(country_sectors))
    L = np.linalg.inv((I-A))
    G = np.linalg.inv((I-B))
    
    ### Compute block-diagonal measures
    dir_bwd_dom, dir_bwd_for = ps.block_dominance(A.T,k=35,axis=1)
    dir_fwd_dom, dir_fwd_for = ps.block_dominance(B,k=35,axis=1)
    
    ### Calculate sums, retain country_sectors, year
    prod_pos = pd.DataFrame({"year": yr,
                             "country_sector": country_sectors, 
                             "tot_bwd": L.T.sum(axis=1),
                             "tot_fwd": G.sum(axis=1),
                             "dir_bwd": A.T.sum(axis=1),
                             "dir_fwd": B.sum(axis=1),
                             "dir_bwd_dom":dir_bwd_dom,
                             "dir_fwd_dom":dir_fwd_dom,
                             "dir_bwd_for":dir_bwd_for,
                             "dir_fwd_for":dir_fwd_for,
                             "top3_dir_bwd": np.sort(A.T,axis=1)[:,-3:].sum(axis=1),
                             "top3_dir_fwd": np.sort(B,axis=1)[:,-3:].sum(axis=1),
                             "max_dir_bwd": A.T.max(axis=1),
                             "max_dir_bwd": B.max(axis=1)})
    
    ### Merge on results table
    prod_pos_list.append(prod_pos)
    
### Melt results table in long form again
prod_pos_complete = pd.concat(prod_pos_list,axis=0)

prod_pos_complete["country"] = prod_pos_complete["country_sector"].str[:3]
prod_pos_complete["country_ind"] = prod_pos_complete["country_sector"].str[4:]

### Save output
prod_pos_complete.to_csv(save_path+"production_network_position.csv",index=False)

################################## Yearly Input Variables #########################################

### Import final demand and total inter use sales - annual
wiod_long = pd.read_stata(base_path+"base/ALS_replication/analysis/WIOD/reduced_WIOD.dta")

### Set baseline param lists
months = np.arange(1,13)
countries = wiod_long.country.unique()
#countries = countries[countries != "ROW"]
sectors = wiod_long.country_ind.unique()
country_sectors = np.array(list(product(*[countries,sectors])))
country_sectors = [i[0]+"_"+i[1] for i in country_sectors]

### Process output and inter_sales
inter_sales = wiod_long[
    ["year","partner","partner_ind","inter_use"]
    ].groupby(["year","partner","partner_ind"]).agg("sum")
inter_sales.reset_index(inplace=True,drop=False)
inter_sales.rename(columns={"inter_use":"inter_sales"},inplace=True)
total_output = wiod_long[
    ["year","country","country_ind","total_output","tot_val_add","tot_inter_cons"]
    ].groupby(["year","country","country_ind"]).agg("mean")
total_output.reset_index(inplace=True,drop=False)

### Import labor productivities, wages and labor shares
wiod_ses = pd.read_excel(base_path+"data/Socio_Economic_Accounts_July14.xlsx",
                         sheet_name= "DATA")
labor_df = wiod_ses.loc[wiod_ses["Variable"].isin(["LAB","H_EMP","GO"])]
labor_df["Code"] = labor_df["Code"].str.lower()
labor_df["Description"] = labor_df["Description"].str.strip().str.capitalize()
labor_df.loc[labor_df["Country"] == "ROU","Country"] = "ROM"
labor_df = labor_df.loc[(labor_df["Country"].isin(countries)) & 
                        (labor_df["Code"].isin(sectors)),]
labor_df = labor_df.melt(id_vars=["Country","Code","Description","Variable"], 
                         value_vars=labor_df.columns[4:21], 
                         var_name='year', value_name='value')
labor_df = labor_df.pivot(index=["Country","Code","Description","year"],columns="Variable",
                          values="value")
labor_df.reset_index(inplace=True,drop=False)
labor_df.rename_axis('',axis=1,inplace=True)
labor_df.rename(str.lower,axis='columns',inplace=True)
labor_df["year"] = labor_df["year"].str[1:].astype(int)

### Load Exchange rates
xr_rate_df = pd.read_excel(base_path+"data/Exchange_Rates.xlsx",
                           sheet_name="EXR",header=3)
xr_rate_df = xr_rate_df.melt(id_vars=["Country","Acronym"],
                             value_vars=xr_rate_df.columns[2:19], 
                             var_name='year',value_name='xr')
xr_rate_df["year"] = xr_rate_df["year"].str[1:].astype(int)
xr_rate_df.rename(str.lower,axis='columns',inplace=True)
xr_rate_df.pop("country")
xr_rate_df.loc[xr_rate_df["acronym"] == "ROU","acronym"] = "ROM"

### Assemble dataframe of yearly variables
yr_var_df = pd.merge(total_output,inter_sales, left_on=["year","country","country_ind"],
                     right_on=["year","partner","partner_ind"])

yr_var_df.drop(["partner","partner_ind"], axis=1, inplace=True)

yr_var_df = yr_var_df.merge(xr_rate_df,how="left",
                            left_on=["year","country"],
                            right_on=["year","acronym"])

yr_var_df = yr_var_df.merge(labor_df,how="left",
                            left_on=["year","country","country_ind"],
                            right_on=["year","country","code"])

### Impute missing values in lab, h_emp with 2009 from same country (2010,2011 are missing)
fillna_df = yr_var_df.loc[(yr_var_df["country"].isin(
    ['AUS','CHN','JPN','KOR','MEX','RUS','TWN','USA','CAN'])) & 
                      (yr_var_df["year"] ==2009),["h_emp","lab","country","country_ind"]]
fillna_df.rename(columns={"h_emp":"h_emp_fill","lab":"lab_fill"},inplace=True)
yr_var_df = yr_var_df.merge(fillna_df,how="left",on=["country","country_ind"])
yr_var_df["h_emp"] = yr_var_df[["h_emp","h_emp_fill"]].bfill(axis=1).iloc[:, 0]
yr_var_df["lab"] = yr_var_df[["lab","lab_fill"]].bfill(axis=1).iloc[:, 0]

### Converting variables in local currency to US-Dollars
yr_var_df["lab_xr"] = yr_var_df["lab"]*yr_var_df["xr"]

### Impute missing ROW values with year mean of all countries
fillrow_df = yr_var_df[["year","country_ind","h_emp","lab_xr"]].groupby(
    ["year","country_ind"]).agg("mean")
fillrow_df.reset_index(inplace=True,drop=False)
fillrow_df.rename(columns={"h_emp":"h_emp_row","lab_xr":"lab_xr_row"},inplace=True)
fillrow_df["country"] = "ROW"
yr_var_df = yr_var_df.merge(fillrow_df,how="left",on=["year","country","country_ind"])
yr_var_df["h_emp"] = yr_var_df[["h_emp","h_emp_row"]].bfill(axis=1).iloc[:, 0]
yr_var_df["lab_xr"] = yr_var_df[["lab_xr","lab_xr_row"]].bfill(axis=1).iloc[:, 0]

### Calculate variables
yr_var_df["final_demand"] = yr_var_df["total_output"] - yr_var_df["inter_sales"]
yr_var_df["wage"] = yr_var_df["lab_xr"]/yr_var_df["h_emp"]
yr_var_df["prod"] = yr_var_df["total_output"]/yr_var_df["h_emp"]
yr_var_df["labor_share"] = yr_var_df["lab_xr"]/yr_var_df["total_output"]
yr_var_df["va_share"] = yr_var_df["tot_val_add"]/yr_var_df["total_output"]
yr_var_df["final_share"] = yr_var_df["final_demand"]/yr_var_df["total_output"]

### Drop redundant columns
yr_var_df.loc[yr_var_df["labor_share"].isna() & (yr_var_df["country"] != "ROW"),
          ["labor_share","wage","prod"]]=0
yr_var_df.drop(["code","acronym","h_emp_fill","lab_fill","lab_xr_row","h_emp_row"],
               inplace=True,axis=1)
yr_var_df["country_sector"] = yr_var_df["country"] + "_" + yr_var_df["country_ind"]

##### Create tables of input shares for normalisation
int_shares_df = wiod_long.copy()
int_shares_df = int_shares_df.loc[int_shares_df["partner"].isin(countries),]
int_shares_df = int_shares_df.loc[int_shares_df["country"].isin(countries),]
int_shares_df = int_shares_df.loc[int_shares_df["partner_ind"].isin(sectors),]
int_shares_df = int_shares_df.loc[int_shares_df["country_ind"].isin(sectors),]
int_shares_df["partner_cs"] = int_shares_df["partner"]+"_"+int_shares_df["partner_ind"]
int_shares_df["country_cs"] = int_shares_df["country"]+"_"+int_shares_df["country_ind"]
int_shares_df["int_share"] = int_shares_df["inter_use"]/int_shares_df["total_output"]
int_shares_df = int_shares_df.pivot(index=["year","partner_cs"], columns="country_cs",values="int_share")
int_shares_df.reset_index(inplace=True,drop=False)
int_shares_df.rename_axis('',axis=1,inplace=True)
int_shares_df.fillna(0,inplace=True)

### Generate base database
base = np.array(list(product(*[years,countries,sectors])))
base_df = pd.DataFrame(base, columns=["year","country","sector"])
base_df = base_df.astype({'year': 'int32'})
base_df["country_sector"] = base_df["country"] + "_" + base_df["sector"]

### Generate empty lists and columns for normalised input shares
int_shares_vanorm = []
int_shares_labornorm = []
yr_var_df["va_share_norm"] = 0
yr_var_df["labor_share_norm"] = 0

for i in years:
    ## Subset for intermediate shares from relevant year
    year_int_shares = int_shares_df.loc[int_shares_df["year"] == i,
                                        int_shares_df.columns[2:]]
    
    ## Combine labor shares and intermediate shares
    labor_shares = yr_var_df.loc[yr_var_df["year"] == i,"labor_share",].to_numpy()
    year_int_shares_labor = np.concatenate([np.array([labor_shares]),
                                            np.array(year_int_shares)],axis=0)
    ## Calculate sum of input shares (with labor)
    emp_labor_rs = np.sum(np.array(year_int_shares_labor),axis=0)
    ## Normalise input shares (with labor)
    labor_normalised_shares = np.array(year_int_shares_labor)*(1/emp_labor_rs)
    labor_normalised_shares[np.isnan(labor_normalised_shares)] = 0
    ## Create new tables with labor-normalised share values
    year_int_labornorm = pd.DataFrame(labor_normalised_shares[1:,:],
                                      columns=country_sectors)
    int_shares_labornorm.append(year_int_labornorm)
    yr_var_df.loc[yr_var_df["year"] == i,"labor_share_norm"] = labor_normalised_shares[0,:]
    
    ## Combine va shares and intermediate shares
    va_shares = yr_var_df.loc[yr_var_df["year"] == i,"va_share",].to_numpy()
    year_int_shares_va = np.concatenate([np.array([va_shares]),
                                         np.array(year_int_shares)],axis=0)
    ## Calculate sum of input shares (with value added)
    emp_va_rs = np.sum(np.array(year_int_shares_va),axis=0)
    ## Normalise input shares (with value added)
    va_normalised_shares = np.array(year_int_shares_va)*(1/emp_va_rs)
    va_normalised_shares[np.isnan(va_normalised_shares)] = 0
    ## Create new tables with value added-normalised share values
    year_int_vanorm = pd.DataFrame(va_normalised_shares[1:,:],
                                   columns=country_sectors)
    int_shares_vanorm.append(year_int_vanorm)
    yr_var_df.loc[yr_var_df["year"] == i,"va_share_norm"] = va_normalised_shares[0,:]

### Concat lists of normalised intermediate shares into tables  
int_shares_labornorm_df = pd.concat(int_shares_labornorm,axis=0)
int_shares_labornorm_df.reset_index(drop=True,inplace=True)
int_shares_labornorm_df = pd.concat([base_df[["year","country_sector"]],
                                     int_shares_labornorm_df],axis=1) 

int_shares_vanorm_df = pd.concat(int_shares_vanorm,axis=0)
int_shares_vanorm_df.reset_index(drop=True,inplace=True)
int_shares_vanorm_df = pd.concat([base_df[["year","country_sector"]],
                                  int_shares_vanorm_df],axis=1) 

### Save results to table
yr_var_df.to_csv(save_path+"yearly_input_variables.csv",index=False)
int_shares_labornorm_df.to_csv(save_path+"intermediate_shares_labornorm.csv",index=False)
int_shares_vanorm_df.to_csv(save_path+"intermediate_shares_vanorm.csv",index=False)


################################## Price Changes & Volatilities #########################################

# Set base path
data_path = base_path+"base/ALS_replication/analysis/PPI/MatlabFiles/"

years = np.arange(1995,2012)
months = np.arange(1,13)

### Define empty datasets for looping
ppi_list = []

### Loop over years and months
for yr in years:
    for mon in months:

        # Read 17 sec PPI file for yrm and merge on table
        iter_df = pd.read_csv(data_path+"PPI_"+str(yr)+str(mon)+".csv")
        iter_df.rename(columns={"chng_ppi":"ppi_chng"},inplace=True)
        ppi_list.append(iter_df)
    
### Concat ppi list into one table
ppi_df =pd.concat(ppi_list,axis=0,ignore_index=True)

### Calculate different ppi changes
ppi_df["ppi_chng_pct"] = ppi_df["ppi_chng"]*100
ppi_df = ppi_df[["year","month","country","country_ind","ppi_chng","ppi_chng_pct"]]

### Calculate price volatility
ppi_volat_df = ppi_df.groupby(["year","country","country_ind"]).agg(
    {'ppi_chng' : ['var', 'std'], 'ppi_chng_pct' : ['var', 'std']})
ppi_volat_df.columns = ppi_volat_df.columns.map('_'.join).str.replace("chng","volat")
ppi_volat_df.reset_index(drop=False,inplace=True)

### Save results
ppi_df.to_csv(save_path+"ppi_changes.csv",index=False)
ppi_volat_df.to_csv(save_path+"ppi_volatilities.csv",index=False)

########################## Combine files to full dataset#####################

### Read in the various dataset 
ppi_volat_df = pd.read_csv(save_path+"ppi_volatilities.csv")
prod_pos_df = pd.read_csv(save_path+"production_network_position.csv")
yearly_vars = pd.read_csv(save_path+"yearly_input_variables.csv")

### Remove country_sector variable from this table
yearly_vars.drop("country_sector",inplace=True,axis=1)

### Join tables together
cs_info_df = ppi_volat_df.merge(prod_pos_df,on=["year","country","country_ind"],
                                how="left")
cs_info_df = cs_info_df.merge(yearly_vars,on=["year","country","country_ind"],
                              how="left")

### Remove ROW country
cs_info_df = cs_info_df.loc[cs_info_df["country"] != "ROW",]

### Write table to file
cs_info_df.to_csv(save_path+"countrysector_information.csv",index=False)

