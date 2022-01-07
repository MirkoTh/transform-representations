from functools import reduce, partial
from re import sub

from numpy.lib.shape_base import split
from pandas.core.frame import DataFrame
from tqdm import tqdm

import numpy as np
import pandas as pd
import time

from sklearn.gaussian_process import GaussianProcessRegressor
from sklearn.gaussian_process.kernels import RBF, StationaryKernelMixin
from sklearn.preprocessing import StandardScaler
from statsmodels.distributions import ECDF


def simulation_conditions(dict_variables: dict, task: str) -> tuple:
    """create dataframe and list with simulation conditions

    Args:
        dict_variables (dict): dictionary with variables as keys and levels as values
        task (str): "function" or "reward" learning

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
        3 if bl else 1 for bl in (df_info["condition"] == "smooth").to_list()
    ]
    l_info = list()
    for i in range(0, df_info.shape[0]):
        l_info.append(df_info.loc[i,].to_dict())
    variables = [
        "condition",
        "prior_sd",
        "sampling",
        "constrain_space",
    ]
    if task == "reward":
        variables.append("sampling_strategy")
        variables.append("beta_softmax")
    l_titles = make_nice_titles(df_info, variables)
    return (df_info, l_info, l_titles)


def make_nice_titles(df_info: pd.DataFrame, variables: list) -> list:
    """create plot titles for the simulation conditions

    Args:
        df_info (pd.DataFrame): simulation conditions
        variables (list): varied variables

    Returns:
        list: with plot titles as strings
    """
    l_varied = list()
    for v in variables:
        unique_entries = list(df_info[v].unique())
        if len(unique_entries) > 1:
            l_varied.append(v)

    # retrieved from: https://stackoverflow.com/questions/36300158/split-text-after-the-second-occurrence-of-character
    def split(strng, sep, pos):
        strng = strng.split(sep)
        return sep.join(strng[:pos]), sep.join(strng[pos:])

    l_titles = list()
    for row in df_info[l_varied].itertuples(index=False):
        title = list()
        for col in range(len(row)):
            title.append(f"""{l_varied[col]} = {row[col]}""")
        l_titles.append(", ".join(title))
        l_titles[-1] = "\n".join(split(l_titles[-1], ",", 2))
    return l_titles


def make_stimuli(
    dict_info: pd.core.frame, gen_model: str = "GP", map_to_reward: bool = False
) -> pd.DataFrame:
    """create x and y values of stimuli shown during training

    Args:
        dict_info (dict): dict with variables of simulation study
        gen_model (str): uses "GP" to sample from GP prior by default, 
                         but can be changed to sinus function using string != "GP"
        map_to_reward (bool): should y values be mapped to positive rewards between 5 and 95?

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
    if map_to_reward:
        y = df_xy["y"].copy()
        min_y = df_xy["y"].min()
        if min_y < 0:
            y = y + abs(min_y)
        range_y = y.max() - y.min()
        slope = 90 / y.max()
        y = 5 + slope * y
        df_xy["y"] = y.round() - 50
    return df_xy


def kernel_rbf(
    X1: np.array, X2: np.array, sigma: float, length_scale: float
) -> np.array:
    """rbf kernel implementation ref. Schulz et al. (2018) jmp tutorial paper
    n.b., uses euclidian distance metric

    Args:
        X1 (np.array): 1xn array whereby n = n feature dimensions
        X2 (np.array): 1xn array whereby n = n feature dimensions
        sigma (float): signal variance
        length_scale (float): length scale

    Returns:
        np.array: a 1x1 array with the similarity between the two x points
    """
    dist_eucl = np.sqrt(np.sum(np.abs(X1 - X2) ** 2))
    return (sigma ** 2) * np.exp(-((dist_eucl ** 2) / (2 * (length_scale ** 2))))


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
    df_stim["trial_nr"] = i + 1
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
    df_train: pd.DataFrame,
    l_ivs: list,
    dict_info: dict,
    fit_length_scale: bool = False,
    update_length_scale: bool = False,
) -> GaussianProcessRegressor:
    """fit GP model on train data and return fitted model object

    Args:
        df_train (pd.DataFrame): data frame with stimuli shown during training
        l_ivs_z (list): names of scaled ivs
        dict_info (dict): infos about simulation experiment
        fit_length_scale (bool): should the length scale be fit? default to False
        update_length_scale (bool): should the length scale param be written back into dict_info?

    Returns:
        GaussianProcessRegressor: the fitted sklearn gp object
    """
    l_ivs_z = [f"""{iv}_z""" for iv in l_ivs]
    if fit_length_scale:
        kernel = RBF(10, (0.01, 100))
    else:
        kernel = RBF(dict_info["length_scale"], length_scale_bounds="fixed")
    gp = GaussianProcessRegressor(kernel=kernel, n_restarts_optimizer=9)
    gp.fit(df_train[l_ivs_z], df_train["y"])
    if update_length_scale:
        dict_info["length_scale"] = gp.kernel_.length_scale
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


def run_perception_pairs(
    dict_info: dict,
    df_xy: pd.DataFrame,
    refit_gp: bool = False,
    readjust_representations: bool = False,
) -> tuple:
    """iterate over trials noisily perceiving pairs of stimuli from test dataset
    feed GP predicted y values into logistic regression predicting probability of accepting
    the second of the two presented stimuli

    Args:
        dict_info (dict): experimental parameter dict
        df_xy (list): data frame with x y values of simulation condition
        refit_gp (bool): should the GP model be refit after miniblock of samples has been accepted

    Returns:
        tuple
            df_test_true (pd.DataFrame)
            df_rewards (pd.DataFrame)
            df_test_new (pd.DataFrame)
            l_kernel (list)
    """
    df_xy, l_ivs, scaler = scale_ivs(df_xy)
    df_train, df_test = split_train_test(dict_info, df_xy)
    if dict_info["n_training"] > 0:
        # in that case people are trained on a proportion of the true x, y values
        # prediction on df_test is done also on the true x, y values
        gp = fit_on_train(
            df_train, l_ivs, dict_info, fit_length_scale=True, update_length_scale=False
        )
        df_test = predict_on_test(df_test, gp, l_ivs)
    else:
        # here, people already start by perceiving noisy samples
        # from the prior and predict
        df_train = perceive_block_stim(df_xy, scaler, dict_info, 1234, "sample")
        df_train.drop_duplicates(subset=["x_1", "x_2"], inplace=True)
        gp = fit_on_train(
            df_train, l_ivs, dict_info, fit_length_scale=True, update_length_scale=False
        )
        df_test = df_xy

    df_test_true = df_test.copy()
    df_test_new = df_test.copy()
    df_rewards = pd.DataFrame()
    df_accepted = pd.DataFrame()
    l_kernel = list()
    l_kernel.append(gp.kernel_.length_scale)
    for i in np.arange(dict_info["n_runs"]) + 1:
        np.random.seed()
        seeds = np.random.randint(0, 10000, 2)
        df_reward_fixed, reward_fixed, _, _ = total_reward(
            [df_test, gp, scaler], dict_info, seeds, "fixed"
        )
        if i == 0:
            df_reward_fixed["trial_nr"] = 0
            df_rewards = pd.concat([df_rewards, df_reward_fixed])
        df_reward_sample, reward_sample, df_l, df_r = total_reward(
            [df_test, gp, scaler], dict_info, seeds, "sample"
        )
        df_reward_sample["trial_nr"] = i
        print(
            "block: ",
            i,
            ", reward fixed: ",
            reward_fixed,
            ", reward sampled: ",
            reward_sample,
        )
        if reward_sample > reward_fixed:
            print(f"""got closer in block {i}""")
            df_accepted = pd.concat([df_l, df_r], axis=0)
            df_accepted["trial_nr"] = i
            df_accepted.drop_duplicates(subset=["stim_id"], inplace=True)
            df_test_new = pd.concat([df_test_new, df_accepted], axis=0)
            df_rewards = pd.concat([df_rewards, df_reward_sample])
            if refit_gp:
                gp = fit_on_train(
                    df_test_new,
                    l_ivs,
                    dict_info,
                    fit_length_scale=True,
                    update_length_scale=False,
                )
            l_kernel.append(gp.kernel_.length_scale)
            if readjust_representations:
                # move points in df_test to accepted samples
                keep = ~df_test["stim_id"].isin(df_accepted["stim_id"])
                df_test = pd.concat([df_test[keep], df_accepted], axis=0)

    if df_accepted.shape[0] > 0:
        # if sampling from the prior helped:
        df_test_new = add_x_deviation(df_test_new, df_test_true)
        df_test_true = add_x_deviation(df_accepted, df_test_true)
    else:
        # if no samples were accepted
        df_test_new = add_x_deviation(df_test_true, df_test_true)
        df_test_true = add_x_deviation(df_test_true, df_test_true)

    return df_test_true, df_rewards, df_test_new, l_kernel


def add_x_deviation(df_left: pd.DataFrame, df_right: pd.DataFrame) -> pd.DataFrame:
    """join df with perceived values on true/orginal values and 
        calculate deviation in x coordinates

        Args:
            df_left (pd.DataFrame): df with percevied representations
            df_right (pd.DataFrame): df with original/true stimulus locations

        Returns:
            pd.DataFrame: df with x deviation as added column
        """

    df_out = df_left.merge(
        df_right[["stim_id", "x_1", "x_2"]],
        how="left",
        on="stim_id",
        suffixes=["_sample", "_orig"],
    )
    df_out["x_deviation"] = np.sqrt(
        (df_out["x_1_orig"] - df_out["x_1_sample"]) ** 2
        + (df_out["x_2_orig"] - df_out["x_2_sample"]) ** 2
    )
    return df_out


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
    np.random.seed()
    # or just call np.random.seed() without argument, which also resets the seed to a pseudorandom value
    gp = fit_on_train(
        df_train, l_ivs, dict_info, fit_length_scale=True, update_length_scale=False
    )
    df_test = predict_on_test(df_test, gp, l_ivs)
    df_new_test = df_test.copy()
    df_new_train = df_train.copy()
    l_kernel = list()
    l_kernel.append(gp.kernel_.length_scale)
    for i in tqdm(range(0, dict_info["n_runs"])):
        df_stim, idx_stimulus = perceive_stimulus(df_test, dict_info, i)
        stim_id_curr = int(df_stim.loc[0, "stim_id"])
        # drop prior mean x values ...
        df_stim.drop([f"""{iv}_z""" for iv in l_ivs], axis=1, inplace=True)
        # ... and replace them with z values of sampled x values
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
            df_new_test.query(f"""stim_id == {stim_id_curr}""")["y_pred_mn"].mean()
            - df_test.loc[idx_stimulus,]["y"]
        )
        # deviation_test = np.abs(
        #     df_new_test.loc[idx_stimulus,]["y_pred_mn"]
        #     - df_test.loc[idx_stimulus,]["y"]
        # )
        deviation_trial = float(
            np.abs(df_stim["y_pred_mn"] - df_test.loc[idx_stimulus,]["y"])
        )
        if dict_info["sampling"] == "improvement":
            if deviation_trial < deviation_test:
                if dict_info["constrain_space"]:
                    if x1_in and x2_in:
                        df_new_test = df_new_test.append(df_stim, ignore_index=True)
                        df_new_train = df_new_train.append(df_stim, ignore_index=True)
                        gp = fit_on_train(
                            df_new_train, l_ivs, dict_info, fit_length_scale=True
                        )
                else:
                    df_new_test = df_new_test.append(df_stim, ignore_index=True)
                    df_new_train = df_new_train.append(df_stim, ignore_index=True)
                    gp = fit_on_train(
                        df_new_train, l_ivs, dict_info, fit_length_scale=True
                    )
        elif dict_info["sampling"] == "metropolis-hastings":
            ecdf = ECDF(df_test["y_pred_sd"])
            prop_deviation = ecdf(df_stim["y_pred_sd"])
            sample_uniform = np.random.uniform()
            if sample_uniform < (1 - prop_deviation):
                if dict_info["constrain_space"]:
                    if x1_in and x2_in:
                        df_new_test = df_new_test.append(df_stim, ignore_index=True)
                        df_new_train = df_new_train.append(df_stim, ignore_index=True)
                        gp = fit_on_train(
                            df_new_train, l_ivs, dict_info, fit_length_scale=True
                        )
                else:
                    df_new_test = df_new_test.append(df_stim, ignore_index=True)
                    df_new_train = df_new_train.append(df_stim, ignore_index=True)
                    gp = fit_on_train(
                        df_new_train, l_ivs, dict_info, fit_length_scale=True
                    )
        l_kernel.append(gp.kernel_.length_scale)
    df_new_test = add_x_deviation(df_new_test, df_test)
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
    df_movements.reset_index(drop=False, inplace=True)
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
                (df_movements["x_1_move"] > 0) & (df_movements["x_2_move"] < 0), "angle"
            ]
        )
    )
    df_movements["angle"].fillna(0, inplace=True)
    return df_movements


def add_max_gradient(df: pd.DataFrame) -> pd.DataFrame:
    """add maximum y-stepsize for every datapoint (aka. derivative)

    Args:
        df (pd.DataFrame): dataframe with training points and stored samples during testing

    Returns:
        pd.DataFrame: same as input with added column max_gradient
    """
    l = list()
    for i in range(df.shape[0]):
        df_distance = pd.merge(
            df.loc[[i]][["x_1_orig", "x_2_orig", "y"]],
            df[["x_1_orig", "x_2_orig", "y"]],
            how="cross",
        )
        df_distance["dist_eucl"] = np.sqrt(
            df_distance.eval(
                "(x_1_orig_x - x_1_orig_y)**2 + (x_2_orig_x - x_2_orig_y)**2"
            )
        )
        df_distance["y_step"] = np.abs(df_distance.eval("y_x - y_y"))
        l.append(df_distance.query("dist_eucl == 1")["y_step"].max())
    df["max_gradient"] = l
    return df


def apply_trained_scaler(df, scaler) -> pd.DataFrame:
    """apply the scaler fit on training data

    Args:
        df ([type]): df with x vars
        scaler ([type]): fitted scaler object

    Returns:
        pd.DataFrame: df with scaled x vars as added columns
    """
    df.columns = ["x_1", "x_2"]
    df.reset_index(drop=True, inplace=True)
    df_tf = pd.DataFrame(scaler.transform(df), columns=["x_1_z", "x_2_z"])
    df.columns = ["x_1_sample", "x_2_sample"]
    return pd.concat([df, df_tf], axis=1)


def perceive_block_stim(
    df_test: pd.DataFrame,
    scaler: StandardScaler,
    dict_info: dict,
    seed: int,
    perceive: str = "sample",
) -> pd.DataFrame:
    """randomly sample from prior for n samples with replacement

    Args:
        df_test (pd.DataFrame): [description]
        scaler (StandardScaler): fitted Scaler object
        dict_info (dict): simulation parameters
        seed (int): seed of pandas sample call
        perceive (str): sample from prior or use true x value; str can be "sample" or "fixed"

    Returns:
        pd.DataFrame: [description]
    """
    l_vars = ["stim_id", "x_1", "x_2", "y"]
    df = df_test.copy().reset_index(drop=True)
    if perceive == "sample":
        np.random.seed(seed)
        # take a larger sample to drop out overlaps between left and right side
        if dict_info["sampling_strategy"] == "stimulus":
            df[["move_x_1", "move_x_2"]] = np.random.normal(
                scale=dict_info["prior_sd"], size=(df.shape[0], 2)
            )
            df["x_1_sample"] = df["x_1"] + df["move_x_1"]
            df["x_2_sample"] = df["x_2"] + df["move_x_2"]
            df.drop(columns=["move_x_1", "move_x_2"], inplace=True)
            df = (
                df[l_vars + ["x_1_sample", "x_2_sample"]]
                .sample(
                    int(dict_info["n_samples_block"] * 1.25),
                    replace=True,
                    random_state=seed,
                )
                .reset_index(drop=True)
            )
        if dict_info["sampling_strategy"] == "trial":
            df = (
                df[l_vars]
                .sample(
                    int(dict_info["n_samples_block"] * 1.25),
                    replace=True,
                    random_state=seed,
                )
                .reset_index(drop=True)
            )
            df[["x_1_sample", "x_2_sample"]] = np.random.normal(
                df[["x_1", "x_2"]], scale=dict_info["prior_sd"]
            )
    elif perceive == "fixed":
        df[["x_1_sample", "x_2_sample"]] = df[["x_1", "x_2"]]
        df = df.sample(
            int(dict_info["n_samples_block"] * 1.25), replace=True, random_state=seed
        ).reset_index(drop=True)
    df_perceived = apply_trained_scaler(df[["x_1_sample", "x_2_sample"]], scaler)
    df = pd.concat([df[l_vars], df_perceived], axis=1)
    return df


def make_decision_df(
    df_l: pd.DataFrame, df_r: pd.DataFrame, df_test: pd.DataFrame, dict_info: dict
) -> tuple:
    """clean decision df and remove duplicates from left and right dfs

    Args:
        df_l (pd.DataFrame): df sampled from left arm of bandit
        df_r (pd.DataFrame): df sampled from right arm of bandit
        df_test (pd.DataFrame): [description]
        dict_info (dict): [description]

    Returns:
        tuple
            df_decide (pd.DataFrame): df with prediction difference between left and right arm
            df_l_xpos (pd.DataFrame): df with unique stimuli of left arm of bandit
            df_r_xpos (pd.DataFrame): df with unique stimuli of right arm of bandit
    """
    l_vars_decide = ["stim_id", "y_pred_mn", "y"]
    df_decide = pd.concat([df_l[l_vars_decide], df_r[l_vars_decide]], axis=1)
    df_decide.columns = ["stim_id_l", "y_pred_l", "y_l", "stim_id_r", "y_pred_r", "y_r"]
    df_decide.eval("y_pred_diff = y_pred_r - y_pred_l", inplace=True)
    df_decide.query("stim_id_l != stim_id_r", inplace=True)
    idxs_keep = df_decide.index.values
    df_decide.reset_index(inplace=True)
    df_decide = df_decide.iloc[
        0 : dict_info["n_samples_block"],
    ]
    cols = [i for i in df_test.columns.values if i != "trial_nr"]
    df_l_xpos = pd.concat([df_l[cols]])
    df_r_xpos = pd.concat([df_r[cols]])
    df_l_xpos = df_l_xpos.iloc[idxs_keep, :].reset_index(drop=True)
    df_r_xpos = df_r_xpos.iloc[idxs_keep, :].reset_index(drop=True)
    df_l_xpos = df_l_xpos.iloc[0 : dict_info["n_samples_block"]]
    df_r_xpos = df_r_xpos.iloc[0 : dict_info["n_samples_block"]]

    if dict_info["sampling_strategy"] == "stimulus":
        df_l_xpos.drop_duplicates(subset=["x_1", "x_2"], inplace=True)
        df_r_xpos.drop_duplicates(subset=["x_1", "x_2"], inplace=True)

    return df_decide, df_l_xpos, df_r_xpos


def predict_miniblock(df: pd.DataFrame, m_gp: GaussianProcessRegressor) -> pd.DataFrame:
    """predict y values given df with x values and GP model

    Args:
        df (pd.DataFrame): df to predict on
        m_gp (GaussianProcessRegressor): fitted GP object

    Returns:
        pd.DataFrame: df with predictions added as columns (predictions: mean and sd)
    """
    df["y_pred_mn"], df["y_pred_sd"] = m_gp.predict(
        df[["x_1_z", "x_2_z"]], return_std=True
    )
    df.reset_index(drop=True, inplace=True)
    df.drop(columns=["x_1", "x_2"], inplace=True)
    df.rename(columns={"x_1_sample": "x_1", "x_2_sample": "x_2"}, inplace=True)
    return df


def softmax(df_lr: pd.DataFrame, beta: float) -> np.array:
    """softmax probabilities given pair of expected rewards and beta aka temperature parameter

    Args:
        df_lr (pd.DataFrame): df with expected rewards (left and right) in columns
        beta (float): temperature parameter of softmax

    Returns:
        np.array: with probabilities choosing left or right option
    """
    above = np.exp(beta * df_lr.loc[:, "y_pred_r"])
    below = np.sum(np.exp(beta * df_lr.loc[:, ["y_pred_r", "y_pred_l"]]), axis=1)
    return above / below


def total_reward(
    list_test: list, dict_info: dict, seeds: np.array, perceive: str
) -> tuple:
    """sum of received rewards for n pairs of perceived stimuli

    Args:
        list_test (list): list with df_test, m_gp, and scaler
        dict_info (pd.DataFrame): simulation parameters
        seeds (np.array): two seed values determining which pairs of x values are sampled from unseen points
        perceive (str): deterministic perception ("fixed") or perception from prior ("sample")

    Returns:
        tuple:
            df (pd.DataFrame): df with all values from left and right stimuli
            df["reward"].sum() (float): total reward for batch
            df_l_xpos (pd.DataFrame): df with left x vals
            df_r_xpos (pd.DataFrame): df with right x vals
    """
    df_test, m_gp, scaler = list_test
    df_l = perceive_block_stim(df_test, scaler, dict_info, seeds[0], perceive=perceive)
    df_r = perceive_block_stim(df_test, scaler, dict_info, seeds[0], perceive=perceive)
    df_l = predict_miniblock(df_l, m_gp)
    df_r = predict_miniblock(df_r, m_gp)
    df_r = df_r.sample(df_r.shape[0], replace=False, random_state=seeds[1]).reset_index(
        drop=True
    )
    df_decide, df_l_xpos, df_r_xpos = make_decision_df(df_l, df_r, df_test, dict_info)
    df_decide["choose_r"] = np.random.binomial(
        1, softmax(df_decide, dict_info["beta_softmax"])
    )
    df_decide[["choose_r", "choose_l"]] = np.array(
        [df_decide["choose_r"], 1 - df_decide["choose_r"]]
    ).T
    df_decide["reward"] = df_decide[["y_r", "y_l"]].to_numpy()[
        df_decide[["choose_r", "choose_l"]].astype(bool).to_numpy()
    ]
    return df_decide, df_decide["reward"].sum(), df_l_xpos, df_r_xpos
