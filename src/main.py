import argparse
import logging
from pathlib import Path

import pandas as pd
import requests
from aatoolbox.utils.hdx_api import load_dataset_from_hdx

from src import constants  # noqa: F401

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

IDMC_URL = "https://backend.idmcdb.org/data/idus_view_all_flat_cached_ochachd"
POP_FILENAME = Path("data/input/API_SP.POP.TOTL_DS2_en_csv_v2_3731322.csv")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("-c", "--clobber", action="store_true")
    parser.add_argument("-d", "--debug", action="store_true")
    return parser.parse_args()


def run_pipeline(clobber: bool = False):
    if clobber or (not Path(constants.idmc_output_filename).exists()):
        logger.info("Downloading latest IDMC data")
        df_pop = get_pop_data()
        df_idmc = get_idmc_data()
        df_idmc_pop = pd.merge(df_idmc, df_pop, how="left", on="iso3")
        logger.info(f"Writing to {constants.idmc_output_filename}")
        df_idmc_pop.to_csv(constants.idmc_output_filename, index=False)
    if clobber or (not Path(constants.cerf_hdx_filename).exists()):
        logger.info("Downloading latest CERF allocations")
        get_cerf_data()


def get_pop_data() -> pd.DataFrame:
    # Get the pop data. Use forward fill to fill missing years for Eritrea.
    df_pop = (
        pd.read_csv(POP_FILENAME)
        .ffill(axis=1)
        .rename(columns={"2020": "pop", "Country Code": "iso3"})
    )[["iso3", "pop"]]
    df_pop = df_pop.loc[df_pop["iso3"] != "INX"]  # Drop this weird non-country
    df_pop["pop"] = df_pop["pop"].astype(int)
    return df_pop


def get_idmc_data() -> pd.DataFrame:
    df_idmc = pd.DataFrame(requests.get(IDMC_URL).json())
    return df_idmc


def get_cerf_data() -> pd.DataFrame():
    cerf_filename = load_dataset_from_hdx(
        hdx_address="cerf-allocations",
        hdx_dataset_name="CERF Allocations.csv",
        output_filepath=Path(constants.cerf_hdx_filename),
    )
    return pd.read_csv(cerf_filename)


if __name__ == "__main__":
    args = parse_args()
    if args.debug:
        logging.getLogger().setLevel(logging.DEBUG)
    run_pipeline(clobber=args.clobber)
