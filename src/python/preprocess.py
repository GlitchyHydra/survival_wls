
def col_to_int(df, col_names):
    for col_name in col_names:
        df[col_name] = df[col_name].astype('int64')

def insert_death_status(df):
    df.loc[df.z_deatyr == -2, 'death_status'] = False
    df.loc[df.z_deatyr != -2, 'death_status'] = True 

def transform_year_to_age(df):
    df['surv_years'] = -1

    predicat = (df.z_deatyr == -2) & (df.z_age75 > 0)
    df.loc[predicat, 'surv_years'] = df.loc[predicat, 'z_age75']

    predicat = (df.z_deatyr == -2) & (df.z_ra029re > 0)
    df.loc[predicat, 'surv_years'] = df.loc[predicat, 'z_ra029re']

    predicat = (df.z_deatyr == -2) & (df.z_ga003re.isna() == False)
    df.loc[predicat, 'surv_years'] = df.loc[predicat, 'z_ga003re']

    predicat = (df.z_deatyr == -2) & (df.z_ha003re.isna() == False)
    df.loc[predicat, 'surv_years'] = df.loc[predicat, 'z_ha003re']

    predicat = (df.z_deatyr == -2) & (df.z_q1a003re.isna() == False)
    df.loc[predicat, 'surv_years'] = df.loc[predicat, 'z_q1a003re']

    predicat = df.z_deatyr != -2
    df.loc[predicat, 'surv_years'] = df[predicat].z_deatyr - df[predicat].z_brdxdy
    df.surv_years = df.surv_years.astype(int)