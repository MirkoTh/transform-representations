from functools import reduce

import pandas as pd


def simulation_conditions():
    """create dataframe with simulation conditions

    Returns:
        pd.DataFrame: a data frame with the conditions
    """
    s_conditions = pd.Series(["smooth", "rough"], name="conditions")
    s_prior_sd = pd.Series([.1, .75], name="prior_sd")
    s_sampling = pd.Series(["metropolis-hastings", "improvement"], name="sampling")
    s_constrain_space = pd.Series([True, False], name="constrain_space")
    df_info = reduce(
        lambda x, y: pd.merge(x, y, how="cross"),
        [s_conditions, s_prior_sd, s_sampling, s_constrain_space]
    )
    return df_info