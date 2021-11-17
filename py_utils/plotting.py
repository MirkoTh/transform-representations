import sys

sys.path.append("..")

import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib as mpl
import matplotlib.pyplot as plt
import matplotlib.collections as mc

import py_utils.utils as utils


def plot_heatmaps(l_info: list, idxs: list = None) -> None:
    """plot the heatmap of a smooth and a rough condition

    Args:
        l_info (list): list of Pandas core frames with simulation conditions
        idxs (list, optional): inides of rows from l_info to plot. Defaults to None. In that case,
        the first and the last observation from l_info are picked
    """

    def pivot_xy(idx, l=l_info):
        df_xy = utils.make_stimuli(l[idx])
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
    list(
        map(
            plot_one_heatmap,
            l_heatmap,
            axes,
            [vmin, vmin],
            [vmax, vmax],
            ["Smooth", "Rough"],
        )
    )


def plot_1d_waves(l_info: list) -> None:
    """plot rough and smooth marginals on x1

    Args:
        l_info (list): list of Pandas core frames with simulation conditions
    """
    f, ax = plt.subplots(1, 1, figsize=(8, 6))
    idx_max = len(l_info) - 1
    df_plot1 = pd.DataFrame(
        utils.make_stimuli(l_info[0]).groupby("x_1")["y"].mean()
    ).reset_index()
    df_plot1["Condition"] = "Smooth"
    df_plot2 = pd.DataFrame(
        utils.make_stimuli(l_info[idx_max]).groupby("x_1")["y"].mean()
    ).reset_index()
    df_plot2["Condition"] = "Rough"
    df_plot = pd.concat([df_plot1, df_plot2], axis=0).reset_index(drop=True)
    sns.lineplot(x="x_1", y="y", data=df_plot, hue="Condition", marker="o")
    ax.legend(prop={"size": 15})
    ax.set_xlabel("$x_{1}$")
    ax.set_ylabel("y")
    ax.tick_params(axis="both", which="major")
    ax.set_title("X-Y Relationship: 1D Margin")


def two_d_uncertainty_bubbles(
    df: pd.DataFrame, ax: plt.Axes, show_colorbar: bool
) -> plt.Axes:
    """plot sd of test 2d test data points

    Args:
        df (pd.DataFrame): test df with x coordinates and predicted y means and sds
        ax (plt.Axes): plt axis object

    Returns:
        plt.Axes: plt axis object
    """
    sns.scatterplot(
        x="x_1",
        y="x_2",
        hue="y_pred_sd",
        size="y_pred_sd",
        sizes=(50, 200),
        data=df,
        palette="viridis",
        ax=ax,
        alpha=0.5,
    )
    ax.set_title("Uncertainty of Test Data After Training")
    ax.set_xlabel("$x_{1}$")
    ax.set_ylabel("$x_{2}$")
    # replace ugly legend with colorbar
    norm = plt.Normalize(df["y_pred_sd"].min(), df["y_pred_sd"].max())
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
    df_info: pd.DataFrame,
    s_id: int = None,
) -> None:
    """plot deviations of gp model after testing phase as histograms
    Args:
        axes (plt.Axes): axes of the figure to plot in
        l_idxs (list): indices of the conditions to plot
        l_plots (list): result list containing data frames from simulations
        df_info (pd.DataFrame): simulation information data frame

    Returns:
        [type]: [description]
    """
    for idx, ax in enumerate(axes.flat):
        cond_id = l_idxs[idx]
        df_plt = l_plots[cond_id]
        if s_id is not None:
            df_plt = df_plt[df_plt["stim_id"].isin([s_id])]
        sns.histplot(df_plt["x_deviation"], ax=ax)
        ax.set_title(
            f"""{str(df_info.loc[cond_id, "condition"])}\nprior_sd={str(df_info.loc[cond_id, "prior_sd"])}\nconstrain_space={df_info.loc[cond_id, "constrain_space"]}\nsampling={df_info.loc[cond_id, "sampling"]}"""
        )
        ax.set_xlabel("Deviation in X Coordinates")
    return axes


def uncertainty_on_test_data(
    df_train: pd.DataFrame,
    df_test: pd.DataFrame,
    l_ivs: list,
    axes: plt.Axes,
    show_colorbar: bool,
) -> pd.DataFrame:
    """plot histogram of sds on test data and 2d visualization of individual sds

    Args:
        df_train (pd.DataFrame): train data
        df_test (pd.DataFrame): test data
        l_ivs (list): names of ivs
        axes (plt.Axes): respective axis object
        show_colorbar (bool): stating whether individual colorbars should be shown
    """
    gp = utils.fit_on_train(df_train, l_ivs)
    df_test = utils.predict_on_test(df_test, gp, l_ivs)
    two_d_uncertainty_bubbles(df_test, axes[1], show_colorbar)
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
            np.rad2deg(
                l_y_end[idx]
                / np.sqrt(np.abs(l_y_end[idx] ** 2) + np.abs(l_x_end[idx] ** 2))
            )
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
        list_dfs_new[idx_plot][list_dfs_new[idx_plot]["index"].notnull()]
        .sort_values(["stim_id", "index"])
        .groupby("stim_id")[["x_1_orig", "x_2_orig", "x_1_sample", "x_2_sample"]]
        .mean()
    )
    title = f"""\
    Condition: {df_info.loc[idx_plot, "condition"]}, Prior SD: {df_info.loc[idx_plot, "prior_sd"]},
    Sampling: {df_info.loc[idx_plot, "sampling"]}, Constrain Space: {df_info.loc[idx_plot, "constrain_space"]}
    """
    ax = plot_moves(df_movements, ax, title)
    return ax
