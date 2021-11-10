import sys

sys.path.append("..")

import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

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
        ax.set_xlabel("x\N{SUPERSCRIPT ONE}", size=15)
        ax.set_ylabel("x\N{SUPERSCRIPT TWO}", size=15)
        ax.set_title(title, size=15)

    if idxs == None:
        idxs = [0, len(l_info) - 1]

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
    f, ax = plt.subplots(1, 1, figsize=(6, 5))
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
    ax.legend()
    ax.set_xlabel("x\N{SUPERSCRIPT ONE}", size=15)
    ax.set_ylabel("y", size=15)
