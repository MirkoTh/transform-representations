import sys

sys.path.append("..")

import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib as mpl
import matplotlib.pyplot as plt
import matplotlib.collections as mc

import py_utils.utils as utils


def plot_heatmaps(l_info: list, idxs: list = None, map_to_reward: bool = False) -> None:
    """plot the heatmap of a smooth and a rough condition

    Args:
        l_info (list): list of Pandas core frames with simulation conditions
        idxs (list, optional): inides of rows from l_info to plot. Defaults to None. In that case,
        the first and the last observation from l_info are picked
        map_to_reward (bool): should y values be mapped to positive rewards between 5 and 95?

    """

    def pivot_xy(idx, l=l_info):
        df_xy = utils.make_stimuli(l[idx], map_to_reward=map_to_reward)
        df_heatmap = df_xy.pivot(index="x_1", columns="x_2", values="y")
        return df_heatmap

    def plot_one_heatmap(df, ax, vmin, vmax, title):
        sns.heatmap(
            df.round(1),
            annot=True,
            linecolor="white",
            linewidths=0.5,
            ax=ax,
            cmap="viridis",
            vmin=vmin,
            vmax=vmax,
            annot_kws={"size": 15},
        )
        ax.invert_yaxis()
        ax.set_xlabel("$x_{1}$")
        ax.set_ylabel("$x_{2}$")
        ax.tick_params(axis="both", which="major")
        ax.set_title(title, size=18)

    if idxs == None:
        idxs = [0, len(l_info) - 3]

    f, axes = plt.subplots(2, 1, sharex=True, sharey=True, figsize=(13, 20))
    l_heatmap = list(map(pivot_xy, idxs))
    vmin, vmax = (
        min(l_heatmap[0].melt()["value"].min(), l_heatmap[1].melt()["value"].min()),
        max(l_heatmap[0].melt()["value"].max(), l_heatmap[1].melt()["value"].max()),
    )
    list(map(
        plot_one_heatmap,
        l_heatmap,
        axes,
        [vmin, vmin],
        [vmax, vmax],
        ["Smooth", "Rough"],
    ))


def plot_1d_waves(l_info: list) -> None:
    """plot rough and smooth marginals on x1

    Args:
        l_info (list): list of Pandas core frames with simulation conditions
    """
    f, ax = plt.subplots(1, 1, figsize=(8, 6))
    idx_max = len(l_info) - 1
    df_plot1 = pd.DataFrame(utils.make_stimuli(l_info[0]).groupby("x_1")["y"].mean()).reset_index()
    df_plot1["Condition"] = "Smooth"
    df_plot2 = pd.DataFrame(utils.make_stimuli(l_info[idx_max]).groupby("x_1")["y"].mean()
                           ).reset_index()
    df_plot2["Condition"] = "Rough"
    df_plot = pd.concat([df_plot1, df_plot2], axis=0).reset_index(drop=True)
    sns.lineplot(x="x_1", y="y", data=df_plot, hue="Condition", zorder=5, ax=ax, legend=False)
    sns.scatterplot(
        x="x_1",
        y="y",
        data=df_plot,
        s=100,
        color="white",
        zorder=10,
        ax=ax,
    )
    sns.scatterplot(
        x="x_1",
        y="y",
        data=df_plot,
        s=35,
        hue="Condition",
        zorder=15,
        ax=ax,
    )
    ax.legend(prop={"size": 15})
    ax.set_xlabel("$x_{1}$")
    ax.set_ylabel("y")
    ax.tick_params(axis="both", which="major")
    ax.set_title("X-Y Relationship: $x_{1}$Margin")


def two_d_uncertainty_bubbles(
    df: pd.DataFrame,
    ax: plt.Axes,
    show_colorbar: bool,
    min_val: float = False,
    max_val: float = False,
) -> plt.Axes:
    """plot sd of test 2d test data points

    Args:
        df (pd.DataFrame): test df with x coordinates and predicted y means and sds
        ax (plt.Axes): plt axis object
        min_val (float): min value to scale pointsize
        max_val (float): max value to scale pointsize

    Returns:
        plt.Axes: plt axis object
    """
    scale_min = 1 / df["y_pred_sd"].min()
    scale_max = scale_min * 2
    sns.scatterplot(
        x="x_1",
        y="x_2",
        hue="y_pred_sd",
        size="y_pred_sd",
        sizes=(scale_min * df["y_pred_sd"].min(), scale_max * df["y_pred_sd"].max()),
        data=df,
        palette="viridis",
        ax=ax,
        alpha=0.5,
    )
    ax.set_title("Uncertainty of Test Data After Training")
    ax.set_xlabel("$x_{1}$")
    ax.set_ylabel("$x_{2}$")
    # replace ugly legend with colorbar
    if min_val is False:
        norm = plt.Normalize(df["y_pred_sd"].min(), df["y_pred_sd"].max())
    else:
        norm = plt.Normalize(min_val, max_val)
    sm = plt.cm.ScalarMappable(cmap="viridis", norm=norm)
    sm.set_array([])
    ax.get_legend().remove()
    if show_colorbar:
        ax.figure.colorbar(sm, orientation="vertical", pad=0.2)
    return ax


def hist_uncertainty(df: pd.DataFrame, ax: plt.Axes) -> plt.Axes:
    """plot distribution of sds as a histogram

    Args:
        df (pd.DataFrame): test df with x coordinates and predicted y means and sds
        ax (plt.Axes): plt axis object

    Returns:
        plt.Axes: plt axis object
    """
    sns.histplot(df["y_pred_sd"], ax=ax)
    ax.set_xlabel("Prediction SD")
    ax.set_title("Distribution of SDs on Test Data")
    return ax


def plot_gp_deviations(
    axes: plt.Axes,
    l_idxs: list,
    l_plots: list,
    l_titles: list,
    s_id: int = None,
) -> None:
    """plot deviations of gp model after testing phase as histograms
    Args:
        axes (plt.Axes): axes of the figure to plot in
        l_idxs (list): indices of the conditions to plot
        l_plots (list): result list containing data frames from simulations
        l_tites (list): list with condition titles
        s_id (int): selected stimulus to show

    Returns:
        [type]: [description]
    """
    for idx, ax in enumerate(axes.flat):
        cond_id = l_idxs[idx]
        df_plt = l_plots[cond_id]
        df_plt = df_plt.query("trial_nr != 0").copy()
        if s_id is not None:
            df_plt = df_plt[df_plt["stim_id"].isin([s_id])]
        sns.histplot(df_plt["x_deviation"], ax=ax)
        ax.set_title(l_titles[cond_id])
        ax.set_xlabel("Deviation in X Coordinates")
    return axes


def uncertainty_on_test_data(
    df_train: pd.DataFrame,
    df_test: pd.DataFrame,
    dict_info: dict,
    l_ivs: list,
) -> pd.DataFrame:
    """plot histogram of sds on test data and 2d visualization of individual sds

    Args:
        df_train (pd.DataFrame): train data
        df_test (pd.DataFrame): test data
        dict_info (dict): infos about simulation experiment
        l_ivs (list): names of ivs
        axes (plt.Axes): respective axis object
        show_colorbar (bool): stating whether individual colorbars should be shown
    """
    gp = utils.fit_on_train(
        df_train, l_ivs, dict_info, fit_length_scale=False, update_length_scale=True
    )
    df_test = utils.predict_on_test(df_test, gp, l_ivs)
    return df_test


def plot_uncertainty_on_test_data(
    df_test: pd.DataFrame,
    axes: plt.Axes,
    show_colorbar: bool,
    min_val: float,
    max_val: float,
) -> None:
    """plot histogram of model uncertainty and uncertainties per test point in 2d space

    Args:
        df_test (pd.DataFrame): test data frame with x, y, and predictions
        axes ([type]): Axes object
        show_colorbar (bool): should individual colorbars be shown per plot?
        min_val (float): min value to scale pointsize
        max_val (float): max value to scale pointsize
    """
    two_d_uncertainty_bubbles(df_test, axes[1], show_colorbar, min_val, max_val)
    hist_uncertainty(df_test, axes[0])


def plot_moves(df_movements: pd.DataFrame, ax: plt.Axes, title: str) -> plt.Axes:
    """plot average movement from prior to posterior

    Args:
        df_movements (pd.DataFrame): data frame with average original and accepted (x1, x2) values
        ax (plt.Axes): Axes object
        title (str): formatted title as a string

    Returns:
        plt.Axes: Axes object
    """
    l_x_start, l_y_start, l_x_end, l_y_end, angle, l_color = (
        list(),
        list(),
        list(),
        list(),
        list(),
        list(),
    )
    cmap = mpl.cm.get_cmap("viridis")

    for idx, r in enumerate(df_movements.itertuples()):
        l_x_start.append(r.x_1_orig)
        l_y_start.append(r.x_2_orig)
        l_x_end.append(r.x_1_sample - r.x_1_orig)
        l_y_end.append(r.x_2_sample - r.x_2_orig)
        angle.append(
            np.rad2deg(l_y_end[idx] / np.sqrt(np.abs(l_y_end[idx]**2) + np.abs(l_x_end[idx]**2)))
        )
        if l_x_end[idx] < 0:
            angle[idx] = +180 - angle[idx]
        elif l_x_end[idx] > 0 and l_y_end[idx] < 0:
            angle[idx] = +360 + angle[idx]
        l_color.append(cmap(angle[idx] / 360))

    ax.scatter("x_1_orig", "x_2_orig", data=df_movements, c="white", s=50, zorder=5)
    ax.scatter("x_1_orig", "x_2_orig", data=df_movements, c=l_color, zorder=10)
    widths = np.repeat(2, len(l_x_start))
    ax.quiver(
        l_x_start,
        l_y_start,
        l_x_end,
        l_y_end,
        color=l_color,
        angles="xy",
        scale_units="xy",
        scale=1,
        linewidths=widths,
        zorder=4,
    )
    ax.set_title(title)
    return ax


def plot_moves_one_condition(
    idx_plot: int, ax: plt.Axes, list_dfs_new: list, df_info: pd.DataFrame
) -> plt.Axes:
    """extract accepted samples from given condition, calculate mean, 
    and plot movement in space

    Args:
        idx_plot (int): the condition to be plotted
        ax (plt.Axes): the axes object to plot in
        list_dfs_new (list): list with all results
        df_info (pd.DataFrame): info about simulation conditions

    Returns:
        plt.Axes: the axes object with the content added
    """ """"""
    df_movements = (
        list_dfs_new[idx_plot][list_dfs_new[idx_plot]["index"].notnull()].sort_values(
            ["stim_id", "index"]
        ).groupby("stim_id")[["x_1_orig", "x_2_orig", "x_1_sample", "x_2_sample"]].mean()
    )
    title = f"""\
    Condition: {df_info.loc[idx_plot, "condition"]}, Prior SD: {df_info.loc[idx_plot, "prior_sd"]},
    Sampling: {df_info.loc[idx_plot, "sampling"]}, Constrain Space: {df_info.loc[idx_plot, "constrain_space"]}
    """
    ax = plot_moves(df_movements, ax, title)
    return ax


def plot_proportion_accepted_samples(
    df: pd.DataFrame, title: str, ax: plt.Axes, n_runs: int
) -> plt.Axes:
    """plot proportion of accepted samples over total number of trials (binned)

    Args:
        df (pd.DataFrame): data frame with all accepted samples
        title (str): title of the condition to be plotted
        ax (plt.Axes): axes object
        n_runs (int): nr of runs of the function-learning task in the simulation

    Returns:
        plt.Axes: axes object with plot added
    """
    # throw out training examples
    df = df.query("trial_nr != 0").copy()

    sns.histplot(x="trial_nr", data=df, stat="proportion", ax=ax)
    ax.set_xlabel("Trial Nr.")
    ax.set_title(title)
    return ax


def plot_uncertainty_over_test(df: pd.DataFrame, title: str, ax: plt.Axes) -> plt.Axes:
    """plot SD of GP model over test trials

    Args:
        df (pd.DataFrame): data frame with all accepted samples
        title (str): title of the condition to be plotted
        ax (plt.Axes): axes object

    Returns:
        plt.Axes: axes object with plot added
    """

    df = df.query("trial_nr != 0").copy()
    df["trial_nr_bin"] = pd.cut(df["trial_nr"], 10, labels=range(0, 10))
    df_agg = (df.groupby("trial_nr_bin")["y_pred_sd"].aggregate({"mean", np.std}).reset_index())
    ax.errorbar(
        x="trial_nr_bin",
        y="mean",
        yerr="std",
        data=df_agg,
        elinewidth=1.5,
        capsize=5,
        zorder=5,
    )
    ax.scatter(x="trial_nr_bin", y="mean", data=df_agg, s=100, c="white", zorder=7)
    ax.scatter(x="trial_nr_bin", y="mean", data=df_agg, s=25, zorder=10)
    ax.axhline(xmin=0, xmax=9, color="r", linewidth=2)
    ax.set_xlabel("Trial Nr. Binned")
    ax.set_title(title)
    return ax


def regplot_y(df, title, ax):
    sns.regplot(x="y", y="x_deviation", data=df.query("trial_nr != 0"), ax=ax)
    ax.set_ylabel("Deviation in x Coordinates")
    ax.set_xlabel("True y Value")
    ax.set_title(title)


def regplot_max_gradient(df, title, ax):
    sns.regplot(x="max_gradient", y="x_deviation", data=df.query("trial_nr != 0"), ax=ax)
    ax.set_ylabel("Deviation in x Coordinates")
    ax.set_xlabel("Max y Gradient")
    ax.set_title(title)


def regplot_start_uncertainty(df, title, ax):
    df_test_start = df.query("trial_nr == 0").copy()[["stim_id", "y_pred_sd", "x_deviation"]]
    df_test_sampled = df.query("trial_nr > 0").copy()[["stim_id", "y_pred_sd", "x_deviation"]]
    df_plot = pd.merge(
        df_test_start, df_test_sampled, on="stim_id", how="left", suffixes=["_start", "_sampled"]
    )
    sns.regplot(x="y_pred_sd_start", y="x_deviation_sampled", data=df_plot, ax=ax)
    ax.set_ylabel("Deviation in x Coordinates")
    ax.set_xlabel("Prediction Uncertainty After Training")
    ax.set_title(title)
