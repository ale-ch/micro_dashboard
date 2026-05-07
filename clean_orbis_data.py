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



import pandas as pd
import re

def wide_to_long_panel(df, id_columns):
    """
    Transform wide dataframe to long panel using pd.wide_to_long.
    Replaces "n.d." strings with NaN and converts yearly columns to numeric.
    
    Parameters:
    -----------
    df : pandas.DataFrame
        Input dataframe with yearly columns
    id_columns : list
        List of ID column names
    
    Returns:
    --------
    pandas.DataFrame
        Long panel with id_cols, yearly_cols, and 'year' column
    """
    
    # Make a copy to avoid modifying original
    df = df.copy()
    
    # Define the stubnames (base names of yearly columns without the year suffix)
    stubnames = [
        'totale_valore_della_produzione_migl_usd',
        'numero_dipendenti',
        'fatturato_lordo_migl_usd',
        'fatturato_netto_migl_usd'
    ]
    
    # Replace "n.d." strings with NaN in ALL columns (including yearly ones)
    df = df.replace("n.d.", pd.NA)
    
    # Convert yearly columns to numeric (coerce errors to NaN)
    for col in df.columns:
        if col not in id_columns:
            df[col] = pd.to_numeric(df[col], errors='coerce')
    
    # Use wide_to_long to reshape
    df_long = pd.wide_to_long(
        df,
        stubnames=stubnames,
        i=id_columns,
        j='year',
        sep='_',
        suffix=r'\d{4}'
    ).reset_index()
    
    # Convert year to int
    df_long['year'] = df_long['year'].astype(int)
    
    return df_long


def aggregate_by_year_city(df_long):
    """
    Aggregate the long panel dataframe by year and city.
    Calculates mean of yearly columns (excluding NA values) and count of unique companies.
    Keeps nuts1, nuts2, nuts3 columns (taking first value since they're constant per city).
    
    Parameters:
    -----------
    df_long : pandas.DataFrame
        Long panel dataframe from wide_to_long_panel function
    
    Returns:
    --------
    pandas.DataFrame
        Aggregated dataframe with year, city, nuts columns, means of yearly columns,
        and count of unique companies
    """
    
    # Define the yearly columns to aggregate
    yearly_columns = [
        'totale_valore_della_produzione_migl_usd',
        'numero_dipendenti',
        'fatturato_lordo_migl_usd',
        'fatturato_netto_migl_usd'
    ]
    
    # Group by year and city, keep nuts columns (take first since they're constant per city)
    result = df_long.groupby(['year', 'citt_latin_alphabet']).agg({
        'totale_valore_della_produzione_migl_usd': 'mean',
        'numero_dipendenti': 'mean',
        'fatturato_lordo_migl_usd': 'mean',
        'fatturato_netto_migl_usd': 'mean',
        'ragione_socialecaratteri_latini': 'nunique',
        'nuts1': 'first',
        'nuts2': 'first',
        'nuts3': 'first'
    }).reset_index()
    
    # Rename the column for clarity
    result = result.rename(columns={
        'ragione_socialecaratteri_latini': 'unique_companies_count'
    })
    
    # Reorder columns for better readability
    column_order = [
        'year',
        'citt_latin_alphabet',
        'nuts1',
        'nuts2',
        'nuts3',
        'totale_valore_della_produzione_migl_usd',
        'numero_dipendenti',
        'fatturato_lordo_migl_usd',
        'fatturato_netto_migl_usd',
        'unique_companies_count'
    ]
    result = result[column_order]
    
    return result


# Complete workflow example:

# Step 1: Define ID columns (including nuts1, nuts2, nuts3)
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

df_long.head(1000).to_csv("df_long.csv")

df_aggregated = aggregate_by_year_city(df_long)

# Step 4: View results
print("#### AGGREGATED ####")
print(df_aggregated.head(10))
print(f"\nShape: {df_aggregated.shape}")
print(f"\nYears range: {df_aggregated['year'].min()} - {df_aggregated['year'].max()}")
print(f"\nUnique cities: {df_aggregated['citt_latin_alphabet'].nunique()}")

df_aggregated.head(1000).to_csv("df_aggr.csv")

# Write standardized column names to txt file
# with open('column_names.txt', 'w') as f:
#     for col in df.columns:
#         f.write(col + '\n')

# print(f"Column names standardized and written to column_names.txt")
# print(f"Total columns: {len(df.columns)}")
# print(f"Standardized column names: {list(df.columns)}")