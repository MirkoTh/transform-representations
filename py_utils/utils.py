from functools import reduce

import numpy as np
import pandas as pd


def simulation_conditions(dict_variables: dict):
    """create dataframe and list with simulation conditions

    Args:
        dict_variables (dict): dictionary with variables as keys and levels as values

    Returns:
         [df_info] pd.DataFrame: a data frame with the conditions
         [l_info] list: list with conditions
    """
    l = list()
    for k, v in dict_variables.items():
        l.append(
            pd.Series(v, name=k, dtype="category").cat.set_categories(v, ordered=True)
        )
    df_info = reduce(lambda x, y: pd.merge(x, y, how="cross"), l)
    l_info = list()
    for df_r in df_info.itertuples():
        l_info.append(df_r)
    return df_info, l_info


def make_stimuli(t_info: pd.core.frame) -> pd.DataFrame:
    """create x and y values of stimuli shown during training

    Args:
        t_info (pd.core.frame): named vector with variables of simulation study

    Returns:
        pd.DataFrame: x (1, 2, ...) and y values, each in a column
    """

    # access value from named tuple for example like t_info.prior_sd
    l_x = list(
        map(
            lambda a, b, c, d: pd.Series(
                np.linspace(a, b, c), name=f"""x_{int(d)}"""
            ).round(1),
            list(np.repeat(t_info.space_edge_min, t_info.n_features)),
            list(np.repeat(t_info.space_edge_max - 1, t_info.n_features)),
            list(np.repeat(t_info.space_edge_max, t_info.n_features)),
            list(np.linspace(1, t_info.n_features, t_info.n_features)),
        )
    )
    df_xy = reduce(lambda a, b: pd.merge(a, b, how="cross"), l_x)
    if t_info.condition == "smooth":
        mult = 1
    elif t_info.condition == "rough":
        mult = 3
    ivs = df_xy.columns
    df_xy["y"] = (np.sin(df_xy[ivs]) * mult).sum(axis=1)
    df_xy["stim_id"] = df_xy.index
    df_xy = df_xy[["stim_id", "x_1", "x_2", "y"]]

    return df_xy


def perceive_stimulus(df_test: pd.DataFrame, l_info: list) -> tuple:
    """perceive a stimulus from the 2D grid using prior_sd from l_info

    Args:
        df_test (pd.DataFrame): data frame with stimuli not shown during training
        l_info (list): experimental parameter list

    Returns:
        tuple: 1-row data frame with stimulus perceived
    """
    idx_stimulus = np.random.choice(df_test.index)
    df_stim = pd.DataFrame(df_test.loc[idx_stimulus,].copy()).T
    df_stim["x_1"] = np.random.normal(df_stim["x_1"], l_info[0].prior_sd, 1)
    df_stim["x_2"] = np.random.normal(df_stim["x_2"], l_info[0].prior_sd, 1)
    return (df_stim, idx_stimulus)
    # NOTE: indexing on l_info has yet to be changed
