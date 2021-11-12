from functools import reduce
from tqdm import tqdm

import numpy as np
import pandas as pd

from sklearn.gaussian_process import GaussianProcessRegressor
from sklearn.gaussian_process.kernels import RBF
from statsmodels.distributions import ECDF


def simulation_conditions(dict_variables: dict) -> tuple:
    """create dataframe and list with simulation conditions

    Args:
        dict_variables (dict): dictionary with variables as keys and levels as values

    Returns:
         [df_info] pd.DataFrame: a data frame with the conditions
         [l_info] list: list with conditions as dicts
    """
    l = list()
    for k, v in dict_variables.items():
        l.append(
            pd.Series(v, name=k, dtype="category").cat.set_categories(v, ordered=True)
        )
    df_info = reduce(lambda x, y: pd.merge(x, y, how="cross"), l)
    l_info = list()
    for i in range(0, df_info.shape[0]):
        l_info.append(df_info.loc[i,].to_dict())
    return (df_info, l_info)


def make_stimuli(dict_info: pd.core.frame) -> pd.DataFrame:
    """create x and y values of stimuli shown during training

    Args:
        dict_info (dict): dict with variables of simulation study

    Returns:
        pd.DataFrame: x (1, 2, ...) and y values, each in a column
    """

    l_x = list(
        map(
            lambda a, b, c, d: pd.Series(
                np.linspace(a, b, c), name=f"""x_{int(d)}"""
            ).round(1),
            list(np.repeat(dict_info["space_edge_min"], dict_info["n_features"])),
            list(np.repeat(dict_info["space_edge_max"] - 1, dict_info["n_features"])),
            list(np.repeat(dict_info["space_edge_max"], dict_info["n_features"])),
            list(np.linspace(1, dict_info["n_features"], dict_info["n_features"])),
        )
    )
    df_xy = reduce(lambda a, b: pd.merge(a, b, how="cross"), l_x)
    if dict_info["condition"] == "smooth":
        mult = 1
    elif dict_info["condition"] == "rough":
        mult = 3
    ivs = df_xy.columns
    df_xy["y"] = (np.sin(df_xy[ivs]) * mult).sum(axis=1)
    df_xy["stim_id"] = df_xy.index
    df_xy = df_xy[["stim_id", "x_1", "x_2", "y"]]

    return df_xy


def perceive_stimulus(df_test: pd.DataFrame, dict_info: dict) -> tuple:
    """perceive a stimulus from the 2D grid using prior_sd from dict_info

    Args:
        df_test (pd.DataFrame): data frame with stimuli not shown during training
        dict_info (dict): experimental parameter dict

    Returns:
        tuple: 1-row data frame with stimulus perceived
    """
    idx_stimulus = np.random.choice(df_test.index)
    df_stim = pd.DataFrame(df_test.loc[idx_stimulus,].copy()).T
    df_stim["x_1"] = np.random.normal(df_stim["x_1"], dict_info["prior_sd"], 1)
    df_stim["x_2"] = np.random.normal(df_stim["x_2"], dict_info["prior_sd"], 1)
    df_stim.reset_index(inplace=True)
    return (df_stim, idx_stimulus)


def fit_on_train(df_train: pd.DataFrame) -> GaussianProcessRegressor:
    """fit GP model on train data and return fitted model object

    Args:
        df_train (pd.DataFrame): data frame with stimuli shown during training

    Returns:
        GaussianProcessRegressor: the fitted sklearn gp object
    """
    kernel = RBF(10, (0.001, 10))
    gp = GaussianProcessRegressor(kernel=kernel, n_restarts_optimizer=9)
    gp.fit(df_train[["x_1", "x_2"]], df_train["y"])
    return gp


def predict_on_test(df: pd.DataFrame, gp: GaussianProcessRegressor) -> pd.DataFrame:
    """predict on some new data given fitted GP model

    Args:
        df (pd.DataFrame): the data frame with x values to predict on

    Returns:
        pd.DataFrame: the data frame with predictions added
    """
    y_pred_mn, y_pred_sigma = gp.predict(df[["x_1", "x_2"]], return_std=True)
    df["y_pred_mn"] = y_pred_mn
    df["y_pred_sd"] = y_pred_sigma
    return df


def run_perception(
    dict_info: dict, df_train: pd.DataFrame, df_test: pd.DataFrame
) -> pd.DataFrame:
    """iterate over trials noisily perceiving stimuli from test dataset and
    predicting function values using the gp model

    Args:
        df_train (pd.DataFrame): data frame with stimuli shown during training
        df_test (pd.DataFrame): data frame with stimuli not shown during training
        dict_info (dict): experimental parameter dict

    Returns:
        pd.DataFrame: test data frame with accepted samples appended to df_test
    """
    gp = fit_on_train(df_train)
    df_test = predict_on_test(df_test, gp)
    df_new_test = df_test.copy()
    df_new_train = df_train.copy()
    for i in tqdm(range(0, dict_info["n_runs"])):
        df_stim, idx_stimulus = perceive_stimulus(df_test, dict_info)
        x1_in = (
            df_stim.loc[0, "x_1"] > dict_info["space_edge_min"]
            and df_stim.loc[0, "x_1"] < dict_info["space_edge_max"]
        )
        x2_in = (
            df_stim.loc[0, "x_2"] > dict_info["space_edge_min"]
            and df_stim.loc[0, "x_2"] < dict_info["space_edge_max"]
        )
        # propose a new posterior
        df_stim = predict_on_test(df_stim, gp)
        deviation_test = np.abs(
            df_test.loc[idx_stimulus,]["y_pred_mn"] - df_test.loc[idx_stimulus,]["y"]
        )
        deviation_trial = float(
            np.abs(df_stim["y_pred_mn"] - df_test.loc[idx_stimulus,]["y"])
        )
        if dict_info["sampling"] == "improvement":
            if deviation_trial < deviation_test:
                if dict_info["constrain_space"]:
                    if x1_in and x2_in:
                        df_new_test = df_new_test.append(df_stim, ignore_index=True)
                        df_new_train = df_new_train.append(df_stim, ignore_index=True)
                        gp = fit_on_train(df_new_train)
                else:
                    df_new_test = df_new_test.append(df_stim, ignore_index=True)
                    df_new_train = df_new_train.append(df_stim, ignore_index=True)
                    gp = fit_on_train(df_new_train)
        elif dict_info["sampling"] == "metropolis-hastings":
            ecdf = ECDF(df_test["y_pred_sd"])
            prop_deviation = ecdf(deviation_trial)
            sample_uniform = np.random.uniform()
            if sample_uniform < prop_deviation:
                if dict_info["constrain_space"]:
                    if x1_in and x2_in:
                        df_new_test = df_new_test.append(df_stim, ignore_index=True)
                        df_new_train = df_new_train.append(df_stim, ignore_index=True)
                        gp = fit_on_train(df_new_train)
                else:
                    df_new_test = df_new_test.append(df_stim, ignore_index=True)
                    df_new_train = df_new_train.append(df_stim, ignore_index=True)
                    gp = fit_on_train(df_new_train)
    df_new_test = df_new_test.merge(
        df_test[["stim_id", "x_1", "x_2"]],
        how="left",
        on="stim_id",
        suffixes=["_sample", "_orig"],
    )
    df_new_test["x_deviation"] = np.sqrt(
        (df_new_test["x_1_orig"] - df_new_test["x_1_sample"]) ** 2
        + (df_new_test["x_2_orig"] - df_new_test["x_2_sample"]) ** 2
    )
    return df_new_test


def split_train_test(dict_variables: dict, l_df_xy: list, l_idx: int) -> tuple:
    """create train and test dataset

    Args:
        dict_variables (dict): dict with parameter info
        l_df_xy (list): list with data frames per simulation condition
        l_idx (int): condition index of data frame to use

    Returns:
        tuple
            df_train (pd.DataFrame): data frame with train data
            df_test (pd.DataFrame): data frame with prediction on test data
    """
    np.random.seed(12433)
    idx_train = np.random.choice(
        np.arange(0, dict_variables["space_edge_max"][0] ** 2),
        size=dict_variables["n_training"][0],
        replace=False,
    )
    idx_test = l_df_xy[l_idx].index[~l_df_xy[l_idx].index.isin(idx_train)]
    df_train = l_df_xy[l_idx].iloc[idx_train,].sort_index()
    df_test = l_df_xy[l_idx].iloc[idx_test,].sort_index()
    return (df_train, df_test)
