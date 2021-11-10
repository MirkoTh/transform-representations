import sys

sys.path.append("..")

import seaborn as sns
import matplotlib.pyplot as plt

import py_utils as utils


def plot_heatmaps(l_info: list, idxs: list = None) -> None:
    """plot the heatmap of a smooth and a rough condition

    Args:
        l_info (list): list of Pandas core frames with simulation conditions per for
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

