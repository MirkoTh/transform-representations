import numpy as np
import matplotlib.pyplot as plt


def stimulus_parameters(val_steps: int):

    #%% Some stimulus parameters
    # General scaling parameters
    scale = 0.15  # size scaling factor for all the stimuli
    dim_sep = 8 * scale  # 8.33*scale    # how far are head and body from each other?
    t_dist = 1

    # Stimulus values
    d1val = np.linspace(0.17, 0.9, num=val_steps)  # 0.12, 0.825,
    d2val = np.linspace(0.1 * scale, 0.85 * scale, num=val_steps)

    # Completion continuous stimulus values
    completion_res = 4  # inference completion resolution
    continuous_steps = val_steps + (val_steps - 1) * completion_res
    d1con = np.linspace(0.17, 0.9, num=continuous_steps)  # 0.12, 0.825,
    d2con = np.linspace(
        0.1 * scale, 0.85 * scale, num=continuous_steps
    )  # 0.895*scale second value
    # d2con = np.linspace(0.1*scale, (contour_size-neck5_size[0]-0.075)*scale, num=continuous_steps)  # 0.895*scale second value

    # Head (1st Dimension)
    n_spikes = 10
    head5_size = 1.1
    head5_pos = [0, dim_sep / 2 + 0.05]
    head_line = 10
    spiky_linew = 10

    # Stomach/body with hole and contour (2nd Dimension)
    body_size = [1.5, 0.50]
    body_pos = [0.1, 0.4]
    contour_size = 1
    contour_pos = [0, -dim_sep / 2]
    radius = 1
    fill_steps = np.linspace(0.1, 0.9, num=val_steps)

    # Neck
    neck_size = [0.8, 0.05]
    neck5_size = [0.03, dim_sep - contour_size]
    neck5_pos = [0, 0]  # [0, 0.275]

    # Stimulus colors
    head_col = [-1, 0.3725, 0.4157]  # [-0.3,  0.6, -0.3]
    star_col = [-1, 0.3725, 0.4157]  # [-0.3,  0.6, -0.3]#[-0.3, -0.3,  0.6]
    body_col = [-1, 0.3725, 0.4157]  # [-0.3,  0.6, -0.3]
    hole_col = [-0.3, -0.3, 0.6]
    hole_col2 = [-0.2, -0.2, -0.2]  # [0.6, -0.3,  -0.3]
    col_default = "#22AEA9"  # [-0.3,  0.6, -0.3]
    col_different = "#4A5453"

    params = dict()
    params["head5_pos"] = head5_pos
    params["head5_size"] = head5_size
    params["scale"] = scale
    params["t_dist"] = t_dist
    params["n_spikes"] = n_spikes
    params["d1val"] = d1val
    params["radius"] = radius
    params["fill_steps"] = fill_steps
    params["col_default"] = col_default
    params["col_different"] = col_different

    return params


def points_on_circumference(center=(0, 0), r=50, n=100):
    return [
        (
            center[0] + (np.cos(2 * np.pi / n * x) * r),
            center[1] + (np.sin(2 * np.pi / n * x) * r),
        )
        for x in range(0, n + 1)
    ]


def rotate_origin_only(coord, radians):

    x = coord[0]
    y = coord[1]

    xx = x * np.cos(radians) + y * np.sin(radians)
    yy = -x * np.sin(radians) + y * np.cos(radians)

    return xx, yy


def rotate_around_point(point, radians, origin=(0, 0)):
    x, y = point
    ox, oy = origin

    qx = ox + np.cos(radians) * (x - ox) + np.sin(radians) * (y - oy)
    qy = oy + -np.sin(radians) * (x - ox) + np.cos(radians) * (y - oy)

    return qx, qy


def head_vertices(radius, params, pos=None):

    if pos is None:
        pos = params["head5_pos"]

    in_vert = points_on_circumference(
        (pos[0] * params["scale"], pos[1] * params["scale"] + params["t_dist"]),
        radius * (params["scale"] / 2),
        params["n_spikes"],
    )
    inner_vertices = []
    for c in in_vert:
        inner_vertices.append(
            rotate_around_point(c, 0.31, (pos[0], pos[1] * params["scale"]))
        )

    out_vert = points_on_circumference(
        (pos[0] * params["scale"], pos[1] * params["scale"] + params["t_dist"]),
        params["head5_size"] * (params["scale"] / 2),
        params["n_spikes"],
    )
    outer_vertices = []
    for c in out_vert:
        outer_vertices.append(
            rotate_around_point(c, 0.31, (pos[0], pos[1] * params["scale"]))
        )

    comb_vertices = [None] * (len(inner_vertices) + len(outer_vertices))
    comb_vertices[::2] = inner_vertices
    comb_vertices[1::2] = outer_vertices

    return comb_vertices


def head_vertices_psy(radius, pos, stim_dist, params):

    in_vert = points_on_circumference(
        pos, radius * (params["scale"] / 2), params["n_spikes"]
    )
    inner_vertices = []
    for c in in_vert:
        inner_vertices.append(rotate_around_point(c, 0.31, pos))

    out_vert = points_on_circumference(
        pos, params["head5_size"] * (params["scale"] / 2), params["n_spikes"]
    )
    outer_vertices = []
    for c in out_vert:
        outer_vertices.append(rotate_around_point(c, 0.31, pos))

    comb_vertices = [None] * (len(inner_vertices) + len(outer_vertices))
    comb_vertices[::2] = inner_vertices
    comb_vertices[1::2] = outer_vertices

    return comb_vertices


def belly(params, i, min_max):
    fill_steps = np.linspace(
        params["radius"] * 0.1, params["radius"] * 0.9, num=params["n_steps"]
    )
    t = np.linspace(0, 2 * np.pi, num=100)
    x_outer = params["radius"] * np.cos(t) * min_max
    y_outer = params["radius"] * np.sin(t) * min_max - 2.4 * min_max
    x_inner = params["radius"] * params["fill_steps"][i] * np.cos(t) * min_max
    y_inner = (
        params["radius"] * params["fill_steps"][i] * np.sin(t) * min_max - 2.4 * min_max
    )
    circle = {
        "x_outer": x_outer,
        "y_outer": y_outer,
        "x_inner": x_inner,
        "y_inner": y_inner,
    }
    return circle


def rotate_arms(x, y_lo, y_hi, theta_deg):
    theta_rad = (theta_deg * np.pi) / 180
    theta_sin = np.sin(theta_rad)
    theta_cos = np.cos(theta_rad)
    m_rotate = np.array([[theta_cos, -theta_sin], [theta_sin, theta_cos]])
    m_xy_hi = np.array([x, y_hi])
    m_xy_lo = np.array([x, y_lo])
    m_xy_hi = m_rotate @ m_xy_hi
    m_xy_lo = m_rotate @ m_xy_lo
    return m_xy_lo, m_xy_hi


def draw_alien(params, head_spikiness, belly_size, ax):
    # head
    coord = head_vertices_psy(head_spikiness, [0, 0], 0, params)
    val_max = np.max([np.max(np.abs(i)) for i in coord])
    ax.plot(
        [xx[0] for xx in coord],
        [yy[1] for yy in coord],
        linewidth=3,
        c=params["col_default"],
    )
    ax.axis("off")
    # belly
    belly_coord = stim_utils.belly(params, belly_size, val_max)
    ax.fill_between(
        belly_coord["x_outer"],
        np.mean(belly_coord["y_outer"]),
        belly_coord["y_outer"],
        color=params["col_default"],
    )
    belly_coord[-1] = 0
    ax.fill_between(belly_coord["x_inner"], 0, belly_coord["y_inner"])
    ax.axis("off")

    # cross
    width = val_max / 3
    height = val_max / 10
    x = np.linspace(-width / 2, width / 2, num=50)
    y = np.linspace(-height / 2, height / 2, num=50)
    ax.fill_between(
        x, -val_max * 1.2, -val_max * 1.2 + height / 2, color=params["col_default"]
    )
    ax.fill_between(
        x, -val_max * 1.2, -val_max * 1.2 - height / 2, color=params["col_default"]
    )
    ax.fill_between(
        y, -val_max * 1.2, -val_max * 1.2 + width / 2, color=params["col_default"]
    )
    ax.fill_between(
        y, -val_max * 1.2, -val_max * 1.2 - width / 2, color=params["col_default"]
    )

    # arms
    x = np.linspace(0, width * 2, num=50)
    y_hi = np.repeat(height / 1, 50)
    y_lo = np.repeat(-height / 1, 50)
    m_xy_lo, m_xy_hi = rotate_arms(x, y_lo, y_hi, 45)
    ax.fill_between(
        m_xy_hi[0, :] + val_max,
        m_xy_hi[1, :] - 2.4 * val_max,
        m_xy_lo[1, :] - 2.4 * val_max,
        color=params["col_default"],
    )
    m_xy_lo, m_xy_hi = rotate_arms(x, y_lo, y_hi, 135)
    ax.fill_between(
        m_xy_hi[0, :] - val_max,
        m_xy_hi[1, :] - 2.4 * val_max,
        m_xy_lo[1, :] - 2.4 * val_max,
        color=params["col_default"],
    )

    # legs
    m_xy_lo, m_xy_hi = rotate_arms(x, y_lo, y_hi, 315)
    ax.fill_between(
        m_xy_hi[0, :] + 0.25 * val_max,
        m_xy_hi[1, :] - 3.3 * val_max,
        m_xy_lo[1, :] - 3.3 * val_max,
        color=params["col_default"],
    )
    m_xy_lo, m_xy_hi = rotate_arms(x, y_lo, y_hi, 225)
    ax.fill_between(
        m_xy_hi[0, :] - 0.25 * val_max,
        m_xy_hi[1, :] - 3.3 * val_max,
        m_xy_lo[1, :] - 3.3 * val_max,
        color=params["col_default"],
    )
    return ax

