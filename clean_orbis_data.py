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
    
    # Define the stubnames (base names of yearly columns without the year suffix)
    stubnames = [
        'totale_valore_della_produzione_migl_usd',
        'numero_dipendenti',
        'fatturato_lordo_migl_usd',
        'fatturato_netto_migl_usd'
    ]
    
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

df_long.head(1000).to_csv("df_long.csv")

# Write standardized column names to txt file
# with open('column_names.txt', 'w') as f:
#     for col in df.columns:
#         f.write(col + '\n')

# print(f"Column names standardized and written to column_names.txt")
# print(f"Total columns: {len(df.columns)}")
# print(f"Standardized column names: {list(df.columns)}")