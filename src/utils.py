from pathlib import Path

import pandas as pd
from hdx.location.country import Country

from src import constants

_ = Country.countriesdata(use_live=False)


DEFAULT_FILEPATH_PREFIX = Path("..")

CERF_EM_TYPES = [
    # Most certain to be related to conflict
    "Displacement",
    "Refugees",
    # May have displacement component
    "Human Rights",
    "Post-conflict Needs",
    "Violence/Clashes",
    "Multiple Emergencies",
]


def get_idmc_data(
    filepath_prefix: Path = DEFAULT_FILEPATH_PREFIX,
) -> pd.DataFrame:
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


def get_cerf_hdx_data(
    filepath_prefix: Path = DEFAULT_FILEPATH_PREFIX,
) -> pd.DataFrame:
    df_cerf = pd.read_csv(filepath_prefix / constants.cerf_hdx_filename)
    df_cerf["dateUSGSignature"] = pd.to_datetime(df_cerf["dateUSGSignature"])
    df_cerf = df_cerf[df_cerf["emergencyTypeName"].isin(CERF_EM_TYPES)]
    return df_cerf


def get_cerf_nicolas_data_by_em_type(
    filepath_prefix: Path = DEFAULT_FILEPATH_PREFIX,
) -> pd.DataFrame:
    df_cerf = _get_cerf_nicolas_data(filepath_prefix=filepath_prefix)
    df_cerf = df_cerf[df_cerf["Emergency Type"].isin(CERF_EM_TYPES)]
    return df_cerf


def get_cerf_nicolas_data_by_people(
    filepath_prefix: Path = DEFAULT_FILEPATH_PREFIX,
) -> pd.DataFrame:
    df_cerf = _get_cerf_nicolas_data(filepath_prefix=filepath_prefix)
    df_cerf = df_cerf[
        (df_cerf["Number of Refugees"] > 0)
        | (df_cerf["Number of IDPs"] > 0)
        | (df_cerf["Number of Returnees"] > 0)
    ]
    return df_cerf


def _get_cerf_nicolas_data(filepath_prefix: Path) -> pd.DataFrame:
    df_cerf = pd.read_excel(
        filepath_prefix / constants.cerf_nicolas_filename, header=1
    ).rename(
        columns={
            "Total Amount Requested\n(for the overall response, "
            "not just CERF)": "amount_requested",
            "Date of ERC Endorsement": "date_endorsed",
        }
    )
    df_cerf["date_endorsed"] = pd.to_datetime(df_cerf["date_endorsed"])
    df_cerf["iso3"] = df_cerf[["Country"]].applymap(
        lambda x: Country.get_iso3_country_code_fuzzy(x)[0]
    )
    df_cerf = df_cerf.dropna(subset="iso3")
    return df_cerf
