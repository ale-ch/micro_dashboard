# "/Volumes/T7 Shield/Downloads/raw_data/ITA"


import pandas as pd
import glob
import re
from functools import reduce
import os

# "/Volumes/T7 Shield/Downloads/raw_data/ITA/ITA_PANEL_995001_1000000.xlsx"

import pandas as pd
import re

def to_snake_case(name):
    # Convert to string and strip whitespace
    name = str(name).strip()
    # Replace spaces and special characters with underscores
    name = re.sub(r'[^a-zA-Z0-9]', '_', name)
    # Convert to lowercase
    name = name.lower()
    # Replace multiple underscores with single underscore
    name = re.sub(r'_+', '_', name)
    # Remove leading/trailing underscores
    name = name.strip('_')
    return name

df = pd.read_excel("/Volumes/T7 Shield/Downloads/raw_data/ITA/ITA_PANEL_995001_1000000.xlsx", sheet_name=1)

# Remove column at index 0 (first column)
df = df.drop(df.columns[0], axis=1)

# Standardize column names to snake_case
df.columns = [to_snake_case(col) for col in df.columns]

# df = process_firm_data(df)


def wide_to_long_panel(df, id_columns):
    """
    Transform wide dataframe to long panel using pd.wide_to_long.
    Replaces "n.d." strings with NaN and converts yearly columns to numeric.
    """
    df = df.copy()
    df = df.replace("n.d.", pd.NA)
    
    for col in df.columns:
        if col not in id_columns:
            df[col] = pd.to_numeric(df[col], errors='coerce')
    
    stubnames = [
        'totale_valore_della_produzione_migl_usd',
        'numero_dipendenti',
        'fatturato_lordo_migl_usd',
        'fatturato_netto_migl_usd'
    ]
    
    df_long = pd.wide_to_long(
        df,
        stubnames=stubnames,
        i=id_columns,
        j='year',
        sep='_',
        suffix=r'\d{4}'
    ).reset_index()
    
    df_long['year'] = df_long['year'].astype(int)
    return df_long


def aggregate_by_year_city(df_long, keep_original=False):
    """
    Aggregate by year and city. Removes numbers and leading spaces from city names.
    Converts No/Sì to 0/1, sums binary columns, renames 'inactive' to 'active'.
    """
    df_long = df_long.copy()
    
    # Convert binary columns
    binary_cols = ['inactive', 'quoted', 'branch', 'owndata', 'woco']
    for col in binary_cols:
        if col in df_long.columns:
            df_long[col] = df_long[col].map({'No': 0, 'Sì': 1})
    
    # Rename inactive to active
    if 'inactive' in df_long.columns:
        df_long['active'] = 1 - df_long['inactive']
        df_long = df_long.drop('inactive', axis=1)
    
    # Keep original city name if requested
    if keep_original:
        df_long['citt_latin_alphabet_original'] = df_long['citt_latin_alphabet']
    
    # Clean city names
    df_long['citt_latin_alphabet'] = df_long['citt_latin_alphabet'].apply(
        lambda x: re.sub(r'[0-9]', '', str(x)) if pd.notna(x) else x
    )
    df_long['citt_latin_alphabet'] = df_long['citt_latin_alphabet'].apply(
        lambda x: x.lstrip() if isinstance(x, str) else x
    )
    df_long = df_long[df_long['citt_latin_alphabet'].str.len() > 0]
    
    # Define aggregation
    yearly_cols = [
        'totale_valore_della_produzione_migl_usd',
        'numero_dipendenti',
        'fatturato_lordo_migl_usd',
        'fatturato_netto_migl_usd'
    ]
    
    binary_cols_sum = ['active', 'quoted', 'branch', 'owndata', 'woco']
    binary_cols_sum = [col for col in binary_cols_sum if col in df_long.columns]
    
    agg_dict = {col: 'sum' for col in yearly_cols}
    agg_dict.update({col: 'sum' for col in binary_cols_sum})
    agg_dict.update({
        'ragione_socialecaratteri_latini': 'nunique',
        'nuts1': 'first',
        'nuts2': 'first',
        'nuts3': 'first'
    })
    
    if keep_original:
        agg_dict['citt_latin_alphabet_original'] = 'first'
    
    result = df_long.groupby(['year', 'citt_latin_alphabet']).agg(agg_dict).reset_index()
    result = result.rename(columns={'ragione_socialecaratteri_latini': 'unique_companies_count'})
    
    return result




# Complete workflow example:

# USAGE EXAMPLE with your specific column names:

# Define your ID columns (all non-year columns from your dataframe)
id_columns = [
    'ragione_socialecaratteri_latini',
    'inactive',
    'quoted', 
    'branch',
    'owndata',
    'woco',
    'citt_latin_alphabet',
    'codice_iso_paese',
    'codice_nace_rev_2_core_code_4_cifre',
    'codice_di_consolidamento',
    'nuts1',
    'nuts2',
    'nuts3',
    'latitudine',
    'longitudine',
    'indirizzo_i_aggiuntivo_i_latitudine',
    'indirizzo_i_aggiuntivo_i_longitudine',
    'descrizione_dell_attivit_in_inglese'
]

# Assuming your dataframe is called 'df'
df_long = wide_to_long_panel(df, id_columns)

# Check the result
print(df_long.head())
print(df_long.columns.tolist())
print(df_long['year'].unique())
print(df_long[['totale_valore_della_produzione_migl_usd', 'numero_dipendenti']].isna().sum())
print(df_long.dtypes)

# df_long.head(1000).to_csv("df_long.csv")
df_long.head(1000).to_csv("df_long.csv")

df_long_filtered = df_long[df_long['year'] >= 2014]

df_aggregated = aggregate_by_year_city(df_long_filtered)

# Step 4: View results
print("#### AGGREGATED ####")
print(df_aggregated.head(10))
print(f"\nShape: {df_aggregated.shape}")
print(f"\nYears range: {df_aggregated['year'].min()} - {df_aggregated['year'].max()}")
print(f"\nUnique cities: {df_aggregated['citt_latin_alphabet'].nunique()}")

df_aggregated.head(1000).to_csv("df_aggr.csv")
df_aggregated.to_csv("df_aggr_full.csv", index=False)

# Write standardized column names to txt file
# with open('column_names.txt', 'w') as f:
#     for col in df.columns:
#         f.write(col + '\n')

# print(f"Column names standardized and written to column_names.txt")
# print(f"Total columns: {len(df.columns)}")
# print(f"Standardized column names: {list(df.columns)}")