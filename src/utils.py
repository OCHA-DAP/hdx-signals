from pathlib import Path

import pandas as pd

from src import constants


def get_idmc_data(filepath_prefix=Path("..")):
    return pd.read_csv(filepath_prefix / constants.idmc_output_filename)
