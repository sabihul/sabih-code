# sabih-code
my code
#%%
MIN_DATE = "20200101"
MAX_DATE = "20211231"

# %%
date_range_df = pd.DataFrame(pd.date_range(start=MIN_DATE, end=MAX_DATE, freq="D"), columns=["Date"])
date_range_df

# %%
def master_df(date_range_df, df):
    # Master Data for Storing Results:
    master_df_final = pd.DataFrame()
    cust_id_list = df["Customer ID"].unique().tolist()

    # Looping across all customers:
    for cust_id in cust_id_list:
        inter_df = pd.DataFrame()
        df_cust_id = df[df["Customer ID"]==cust_id]
        df_cust_id = df_cust_id.reset_index(drop=True)
        inter_df = pd.concat([inter_df, date_range_df])
        inter_df["Customer ID"] = cust_id
        inter_df = pd.merge(inter_df, df_cust_id, how="left", on=["Customer ID", "Date"])
        inter_df = inter_df.set_index("Date")
        inter_df = inter_df.sort_index()
        inter_df["Amount"] = inter_df["Amount"].fillna(0)
        inter_df['mean_30D'] = inter_df.groupby('Customer ID')['Amount'].rolling('30D').mean().reset_index(0,drop=True)
        inter_df = inter_df[inter_df["Amount"]!=0]

        # Final Master data with only existing data columns
        master_df_final = pd.concat([master_df_final, inter_df])
    return master_df_final
# %%
df_ = master_df(date_range_df, df=data_cust_date)
df_
