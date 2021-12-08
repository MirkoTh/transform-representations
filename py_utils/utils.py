from functools import reduce, partial

from numpy.lib.shape_base import split
from tqdm import tqdm

import numpy as np
import pandas as pd

from sklearn.gaussian_process import GaussianProcessRegressor
from sklearn.gaussian_process.kernels import RBF
from sklearn.preprocessing import StandardScaler
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
    df_info["length_scale"] = [
        0.15 if bl else 0.5 for bl in (df_info["condition"] == "smooth").to_list()
    ]
    l_info = list()
    for i in range(0, df_info.shape[0]):
        l_info.append(df_info.loc[i,].to_dict())
    l_titles = [
        f"""\
        Condition: {df_info.loc[idx_plot, "condition"]}, Prior SD: {df_info.loc[idx_plot, "prior_sd"]},
        Sampling: {df_info.loc[idx_plot, "sampling"]}, Constrain Space: {df_info.loc[idx_plot, "constrain_space"]}
        """.strip()
        for idx_plot in range(df_info.shape[0])
    ]
    return (df_info, l_info, l_titles)


def make_stimuli(dict_info: pd.core.frame, gen_model: str = "GP") -> pd.DataFrame:
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
    # to be changed: add option to sample from GP prior instead of sinus function
    if gen_model != "GP":
        if dict_info["condition"] == "smooth":
            mult = 1
        elif dict_info["condition"] == "rough":
            mult = 3
        ivs = df_xy.columns
        df_xy["y"] = (np.sin(df_xy[ivs]) * mult).sum(axis=1)
    elif gen_model == "GP":
        l_df_xy_1 = list(np.repeat(df_xy.to_numpy(), df_xy.shape[0], 0))
        l_df_xy_2 = list(np.tile(df_xy.to_numpy().T, df_xy.shape[0]).T)
        kernel_rbf_partial = partial(
            kernel_rbf, sigma=1, length_scale=dict_info["length_scale"]
        )
        l_similarities = list(map(kernel_rbf_partial, l_df_xy_1, l_df_xy_2))
        df_similarities = pd.DataFrame(
            np.reshape(np.array(l_similarities), (df_xy.shape[0], df_xy.shape[0]))
        )
        np.random.seed(8376)
        df_xy["y"] = np.random.multivariate_normal(
            np.repeat(0, df_similarities.shape[0]), df_similarities.to_numpy()
        )
    df_xy["stim_id"] = df_xy.index
    df_xy = df_xy[["stim_id", "x_1", "x_2", "y"]]
    df_xy["trial_nr"] = 0
    return df_xy


def kernel_rbf(
    X1: np.array, X2: np.array, sigma: float, length_scale: float
) -> np.array:
    """rbf kernel implementation ref. Schulz et al. (2018) jmp tutorial paper
    uses euclidian distance metric

    Args:
        X1 (np.array): 1xn array whereby n = n feature dimensions
        X2 (np.array): 1xn array whereby n = n feature dimensions
        sigma (float): signal variance
        length_scale (float): length scale

    Returns:
        np.array: a 1x1 array with the similarity between the two x points
    """
    dist_eucl = np.sqrt(np.sum(np.abs(X1 - X2) ** 2))
    return sigma ** 2 * np.exp(-(dist_eucl / 2 * length_scale ** 2))


def perceive_stimulus(df_test: pd.DataFrame, dict_info: dict, i: int) -> tuple:
    """perceive a stimulus from the 2D grid using prior_sd from dict_info

    Args:
        df_test (pd.DataFrame): data frame with stimuli not shown during training
        dict_info (dict): experimental parameter dict
        i (int): trial number in the function-learning task

    Returns:
        tuple: 1-row data frame with stimulus perceived
    """
    idx_stimulus = np.random.choice(df_test.index)
    df_stim = pd.DataFrame(df_test.loc[idx_stimulus,].copy()).T
    df_stim["x_1"] = np.random.normal(df_stim["x_1"], dict_info["prior_sd"], 1)
    df_stim["x_2"] = np.random.normal(df_stim["x_2"], dict_info["prior_sd"], 1)
    df_stim["trial_nr"] = i
    df_stim.reset_index(inplace=True)
    return (df_stim, idx_stimulus)


def scale_ivs(df: pd.DataFrame) -> tuple:
    """z-scale/standardize the independent variables of df

    Args:
        df (pd.DataFrame): data frame with Xs

    Returns:
        tuple
            pd.DataFrame: data frame with standardized Xs
            list: names of scaled ivs
    """
    cols = df.columns
    cols_ivs_filter = cols.str.startswith("x")
    cols_ivs = cols[cols_ivs_filter]
    scaler = StandardScaler(with_mean=True, with_std=True).fit(df[cols_ivs])
    df_X_scaled = pd.DataFrame(scaler.transform(df[cols_ivs]))
    cols_ivs_z = list()
    for idx, c in enumerate(cols_ivs):
        cols_ivs_z.append(f"""{c}_z""")
    df_X_scaled.columns = cols_ivs_z
    df = pd.concat([df, df_X_scaled], axis=1)
    return (df, cols_ivs, scaler)


def fit_on_train(
    df_train: pd.DataFrame, l_ivs: list, dict_info: dict, fit_length_scale: bool = False
) -> GaussianProcessRegressor:
    """fit GP model on train data and return fitted model object

    Args:
        df_train (pd.DataFrame): data frame with stimuli shown during training
        l_ivs_z (list): names of scaled ivs
        dict_info (dict): infos about simulation experiment
        fit_length_scale (bool): should the length scale be fit? default to False

    Returns:
        GaussianProcessRegressor: the fitted sklearn gp object
    """
    l_ivs_z = [f"""{iv}_z""" for iv in l_ivs]
    kernel = RBF(dict_info["length_scale"], length_scale_bounds="fixed")
    gp = GaussianProcessRegressor(kernel=kernel, n_restarts_optimizer=9)
    gp.fit(df_train[l_ivs_z], df_train["y"])
    return gp


def predict_on_test(
    df: pd.DataFrame, gp: GaussianProcessRegressor, l_ivs: list
) -> pd.DataFrame:
    """predict on some new data given fitted GP model

    Args:
        df (pd.DataFrame): the data frame with x values to predict on
        gp (GaussianProcessRegressor): the fitted gp model
        l_ivs_z (list): names of scaled ivs

    Returns:
        pd.DataFrame: the data frame with predictions added
    """
    l_ivs_z = [f"""{iv}_z""" for iv in l_ivs]
    y_pred_mn, y_pred_sigma = gp.predict(df[l_ivs_z], return_std=True)
    df["y_pred_mn"] = y_pred_mn
    df["y_pred_sd"] = y_pred_sigma
    return df


def run_perception(dict_info: dict, df_xy: pd.DataFrame) -> pd.DataFrame:
    """iterate over trials noisily perceiving stimuli from test dataset and
    predicting function values using the gp model

    Args:
        dict_info (dict): experimental parameter dict
        df_xy (list): data frame with x y values of simulation condition

    Returns:
        pd.DataFrame: test data frame with accepted samples appended to df_test
    """
    df_xy, l_ivs, scaler = scale_ivs(df_xy)
    df_train, df_test = split_train_test(dict_info, df_xy)
    gp = fit_on_train(df_train, l_ivs)
    df_test = predict_on_test(df_test, gp, l_ivs)
    df_new_test = df_test.copy()
    df_new_train = df_train.copy()
    l_kernel = list()
    l_kernel.append(gp.kernel_.length_scale)
    for i in tqdm(range(0, dict_info["n_runs"])):
        df_stim, idx_stimulus = perceive_stimulus(df_test, dict_info, i)
        df_stim.drop([f"""{iv}_z""" for iv in l_ivs], axis=1, inplace=True)
        df_stim_scaled = pd.DataFrame(scaler.transform(df_stim[l_ivs]))
        df_stim_scaled.columns = [f"""{iv}_z""" for iv in l_ivs]
        df_stim = pd.concat([df_stim, df_stim_scaled], axis=1)
        x1_in = (
            df_stim.loc[0, "x_1"] > dict_info["space_edge_min"]
            and df_stim.loc[0, "x_1"] < dict_info["space_edge_max"]
        )
        x2_in = (
            df_stim.loc[0, "x_2"] > dict_info["space_edge_min"]
            and df_stim.loc[0, "x_2"] < dict_info["space_edge_max"]
        )

        # propose a new posterior
        df_stim = predict_on_test(df_stim, gp, l_ivs)
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
                        gp = fit_on_train(df_new_train, l_ivs)
                else:
                    df_new_test = df_new_test.append(df_stim, ignore_index=True)
                    df_new_train = df_new_train.append(df_stim, ignore_index=True)
                    gp = fit_on_train(df_new_train, l_ivs)
        elif dict_info["sampling"] == "metropolis-hastings":
            ecdf = ECDF(df_test["y_pred_sd"])
            prop_deviation = ecdf(deviation_trial)
            sample_uniform = np.random.uniform()
            if sample_uniform < prop_deviation:
                if dict_info["constrain_space"]:
                    if x1_in and x2_in:
                        df_new_test = df_new_test.append(df_stim, ignore_index=True)
                        df_new_train = df_new_train.append(df_stim, ignore_index=True)
                        gp = fit_on_train(df_new_train, l_ivs)
                else:
                    df_new_test = df_new_test.append(df_stim, ignore_index=True)
                    df_new_train = df_new_train.append(df_stim, ignore_index=True)
                    gp = fit_on_train(df_new_train, l_ivs)
        l_kernel.append(gp.kernel_.length_scale)
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
    return (df_new_test, l_kernel)


def split_train_test(dict_variables: dict, df_xy: pd.DataFrame) -> tuple:
    """create train and test dataset

    Args:
        dict_variables (dict): dict with parameter info
        df_xy (list): data frame with x y values of simulation condition

    Returns:
        tuple
            df_train (pd.DataFrame): data frame with train data
            df_test (pd.DataFrame): data frame with prediction on test data
    """
    np.random.seed(12433)
    idx_train = np.random.choice(
        np.arange(0, dict_variables["space_edge_max"] ** 2),
        size=dict_variables["n_training"],
        replace=False,
    )
    idx_test = df_xy.index[~df_xy.index.isin(idx_train)]
    df_train = df_xy.iloc[idx_train,].sort_index()
    df_test = df_xy.iloc[idx_test,].sort_index()
    return (df_train, df_test)


def add_angle_of_movements(df_movements: pd.DataFrame) -> pd.DataFrame:
    """add angle of movement from prior to posterior

    Args:
        df_movements (pd.DataFrame): data frame with original and accepted locations

    Returns:
        pd.DataFrame: data frame with added columns angle, and movements along axes
    """
    df_movements = (
        df_movements[df_movements["index"].notnull()]
        .sort_values(["stim_id", "index"])
        .groupby("stim_id")[["x_1_orig", "x_2_orig", "x_1_sample", "x_2_sample"]]
        .mean()
    )
    df_movements.eval("x_1_move = x_1_sample - x_1_orig", inplace=True)
    df_movements.eval("x_2_move = x_2_sample - x_2_orig", inplace=True)
    df_movements["angle"] = np.rad2deg(
        df_movements["x_2_move"]
        / np.sqrt(
            np.abs(df_movements["x_2_move"] ** 2)
            + np.abs(df_movements["x_1_move"] ** 2)
        )
    )
    df_movements.loc[df_movements["x_1_move"] < 0, "angle"] = 180 - (
        df_movements.loc[df_movements["x_1_move"] < 0, "angle"]
    )
    df_movements.loc[
        (df_movements["x_1_move"] > 0) & (df_movements["x_2_move"] < 0), "angle"
    ] = (
        360
        + (
            df_movements.loc[
                (df_movements["x_1_move"]) > 0 & (df_movements["x_2_move"] < 0), "angle"
            ]
        )
    )
    return df_movements
