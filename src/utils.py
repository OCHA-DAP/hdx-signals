from pathlib import Path

import pandas as pd

from src import constants


def get_idmc_data(filepath_prefix=Path("..")):
    df_idmc = pd.read_csv(filepath_prefix / constants.idmc_output_filename)
    for column in [
        "displacement_date",
        "displacement_start_date",
        "displacement_end_date",
        "event_start_date",
        "event_end_date",
    ]:
        df_idmc[column] = pd.to_datetime(df_idmc[column])
    return df_idmc
