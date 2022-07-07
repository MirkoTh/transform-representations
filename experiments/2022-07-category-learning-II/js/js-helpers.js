if (window.location.search.indexOf('PROLIFIC_PID') > -1) {
    var participant_id = getQueryVariable('PROLIFIC_PID');
}
// If no ID is present, generate one using random numbers - this is useful for testing
else {
    var participant_id = Math.floor(Math.random() * 1000);
}
// STUDY ID
if (window.location.search.indexOf('STUDY_ID') > -1) {
    var studyID = getQueryVariable('STUDY_ID');
}


// get subject ID
function getQueryVariable(variable) {
    var query = window.location.search.substring(1);
    var vars = query.split("&");
    for (var i = 0; i < vars.length; i++) {
        var pair = vars[i].split("=");
        if (pair[0] == variable) { return pair[1]; }
    }
    return (false);
}


function setup_experiment(condition_id) {
    // experiment information
    const experiment_info = {
        n_stimuli: 100,
        n_conditions: 3, // control, 4 categories, 9 categories
        n_reproduction: 2, // baseline and after categorization
        n_practice_reproduction: 3,
        n_trials_reproduction_1: 2, //100, //5, //
        n_trials_reproduction_2: 2, //100, //5, //
        n_trials_categorization_train_target: 0, //3, // 
        n_trials_categorization: 11, //5, // 380, //
        n_trials_categorization_total: 0 + 4, // 3 + 5, //
        n_trial_categorization_lag: 3, // last n categorization trials to calculate "final" accuracy
        condition_id: condition_id,
        n_categories: n_categories,
        thx_cat_overall: .8,
        thx_cat_lag: .85
    }
    document.getElementById("n_trials_cat_lag").innerHTML = experiment_info["n_trial_categorization_lag"]
    // stim_ids of cat2 and cat3
    // randomize these ids
    // select first n_only_target_cat or n_only_target_cat/2 and append them to category_id, category_name, category_stimulus_id

    // read mapping from x1 and x2 values to categories
    const cat2map_val = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    const cat3map_val = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 3, 3, 3, 1, 2, 2, 2, 2, 1, 1, 3, 3, 3, 3, 1, 2, 2, 2, 1, 1, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    const cat0map_val = Array(experiment_info["n_stimuli"]).fill(1)
    var cat2_stim_ids_cat2 = [22, 23, 24, 32, 33, 34, 35, 42, 43, 44, 45, 46, 53, 54, 55, 56, 57, 64, 65, 66, 67, 75, 76, 77]
    var cat3_stim_ids_cat2 = [14, 15, 16, 24, 25, 26, 27, 34, 35, 36, 37, 38, 45, 46, 47, 48, 56, 57, 58]
    var cat3_stim_ids_cat3 = [41, 42, 43, 51, 52, 53, 54, 61, 62, 63, 64, 65, 72, 73, 74, 75, 83, 84, 85]
    var stim_ids_cats_tt = []
    var stim_ids_cat_nt = []
    var cat3_stim_ids_all = []
    cat2_stim_ids_cat2 = append_randomized_arrays(cat2_stim_ids_cat2, 2)
    cat2_stim_ids_cat2.length = experiment_info["n_trials_categorization_train_target"]
    cat3_stim_ids_cat2 = append_randomized_arrays(cat3_stim_ids_cat2, 2)
    cat3_stim_ids_cat3 = append_randomized_arrays(cat3_stim_ids_cat3, 2)
    cat3_stim_ids_all = cat3_stim_ids_cat2.concat(cat3_stim_ids_cat3)
    cat3_stim_ids_all = append_randomized_arrays(cat3_stim_ids_all, 1)
    cat3_stim_ids_all.length = experiment_info["n_trials_categorization_train_target"]


    // display info
    const display_info = {
        reproduction: {
            iti: 500,
            fixcross: 500,
            presentation: 250,
            ri: 200//5000
        },
        categorization: {
            iti: 500,
            fixcross: 500,
            feedbacktime_true: 500,
            feedbacktime_wrong: 1000,
            deadlinetime: 3000
        }
    }

    const n_x_steps = 10;
    var stimulus_info = {
        x1: Array(n_x_steps).fill().map((element, index) => index),
        x2: Array(n_x_steps).fill().map((element, index) => index),
        x1_x2: Array(n_x_steps * n_x_steps),
        stimulus_id: Array(n_x_steps * n_x_steps),
        category_name: ["Bukil", "Venak", "Monus", "Ladiv"],
        category_id: []
    }

    if (experiment_info["n_categories"] == 2) {
        stimulus_info["category_id"] = cat2map_val
        stim_ids_cats_tt = append_randomized_arrays(cat2_stim_ids_cat2, 1)
    } else if (experiment_info["n_categories"] == 3) {
        stimulus_info["category_id"] = cat3map_val
        stim_ids_cats_tt = append_randomized_arrays(cat3_stim_ids_all, 1)
    } else if (experiment_info["n_categories"] == 1) {
        stimulus_info["category_id"] = cat0map_val
        stim_ids_cats_tt = Array(experiment_info["n_stimuli"]).fill().map((element, index) => index)
        stim_ids_cats_tt = append_randomized_arrays(stim_ids_cats_tt, 1)
        stim_ids_cats_tt.length = experiment_info["n_trials_categorization_train_target"]
    }

    stimulus_info["n_stimuli"] = stimulus_info["x1"].length * stimulus_info["x2"].length
    var i = 0;
    for (let x1 of stimulus_info["x1"]) {
        for (let x2 of stimulus_info["x2"]) {
            // 10x10 grid of stimuli placed within finer grid of 100x100
            // with some space from each edge
            stimulus_info["x1_x2"][i] = [(x1 + 1) * 9 + 1, (x2 + 1) * 9 + 1]
            stimulus_info["stimulus_id"][i] = i
            i += 1
            if (!stim_ids_cats_tt.includes(i)) {
                stim_ids_cat_nt.push(i)
            }
        }
    }
    stim_ids_cat_nt = append_randomized_arrays(stim_ids_cat_nt, 1)

    // trial info
    var trial_info = {}
    trial_info["stimulus_id_rp"] = [...stimulus_info["stimulus_id"]]
    trial_info["stimulus_id_r1"] = [...stimulus_info["stimulus_id"]]
    trial_info["stimulus_id_r2"] = [...stimulus_info["stimulus_id"]]
    trial_info["stimulus_id_c"] = []
    trial_info["category_id"] = []
    trial_info["category_name"] = []
    trial_info["response_c"] = []

    const n_reps_practice = 1
    const n_reps_reproduction_1 = Math.ceil(experiment_info["n_trials_reproduction_1"] / stimulus_info["n_stimuli"])
    const n_reps_reproduction_2 = Math.ceil(experiment_info["n_trials_reproduction_2"] / stimulus_info["n_stimuli"])

    trial_info["stimulus_id_rp"] = append_randomized_arrays(trial_info["stimulus_id_rp"], n_reps_practice)
    trial_info["stimulus_id_r1"] = append_randomized_arrays(trial_info["stimulus_id_r1"], n_reps_reproduction_1)
    trial_info["stimulus_id_r2"] = append_randomized_arrays(trial_info["stimulus_id_r2"], n_reps_reproduction_2)

    // the following can be un-commented if number of items of reproduction task is < n different stimuli
    /* stim_ids_cats_tt_cr1 = append_randomized_arrays(stim_ids_cats_tt, 1)
    stim_ids_cat_nt_cr1 = append_randomized_arrays(stim_ids_cat_nt_cr1, 1)
    trial_info["stimulus_id_r1"] = stim_ids_cats_tt_cr1.concat(stim_ids_cat_nt_cr1)
    stim_ids_cats_tt_cr2 = append_randomized_arrays(stim_ids_cats_tt, 1)
    stim_ids_cat_nt_cr2 = append_randomized_arrays(stim_ids_cat_nt_cr1, 1)
    trial_info["stimulus_id_r2"] = stim_ids_cats_tt_cr2.concat(stim_ids_cat_nt_cr2)
    */

    trial_info["stimulus_id_rp"].length = experiment_info["n_practice_reproduction"]
    trial_info["stimulus_id_r1"].length = experiment_info["n_trials_reproduction_1"]
    trial_info["stimulus_id_r2"].length = experiment_info["n_trials_reproduction_2"]

    if (n_categories <= 3) {
        // similarity judgement and ellipse categories
        trial_info = assign_items_to_categories_ellipse(experiment_info, trial_info, stimulus_info)
    } else if (n_categories == 4) {
        // square categories (aka grid)
        trial_info = assign_items_to_categories_squares(n_x_steps, experiment_info, trial_info, stimulus_info)
    }
    console.log("printing now the stimulus ids and the associated categories for the first 50 trials:")
    console.log(trial_info["stimulus_id_c"].slice(0, 50))
    console.log(trial_info["category_id"].slice(0, 50))
    console.log(trial_info["category_name"].slice(0, 50))

    var obj_setup_expt;
    obj_setup_expt = {
        experiment_info: experiment_info,
        display_info: display_info,
        stimulus_info: stimulus_info,
        trial_info: trial_info
    }

    return obj_setup_expt
}


function append_randomized_arrays(set, n) {
    var sets_randomized = [];
    for (let i = 0; i < n; i++) {
        var set_permuted = permute(set)
        sets_randomized = sets_randomized.concat(set_permuted);
    }
    return sets_randomized
}

//permute a list
function permute(o) {
    for (var j, x, i = o.length; i; j = Math.floor(Math.random() * i), x = o[--i], o[i] = o[j], o[j] = x);
    return o;
};


function assign_items_to_categories_squares(n_x_steps, experiment_info, trial_info, stimulus_info) {
    const n_stim_repeats = Math.ceil(experiment_info["n_trials_categorization_total"] / experiment_info["n_stimuli"])
    trial_info["stimulus_id_c"] = append_randomized_arrays(stimulus_info["stimulus_id"], n_stim_repeats)
    segments_per_dim = Math.sqrt(experiment_info["n_categories"])
    console.log("segments per dim is: " + segments_per_dim)
    category_step = Math.max(...stimulus_info["x1"]) / segments_per_dim
    x1_boundaries = Array(segments_per_dim).fill().map((element, index) => (index + 1) * category_step)
    x2_boundaries = Array(segments_per_dim).fill().map((element, index) => (index + 1) * category_step)
    console.log("x1_boundaries are: " + x1_boundaries)
    console.log("x2_boundaries are: " + x2_boundaries)
    var cat_assign_x1 = Array(segments_per_dim)
    var cat_assign_x2 = Array(segments_per_dim)
    for (let i = 0; i < experiment_info["n_trials_categorization_total"]; i++) {
        cat_assign_x1 = trial_info["stimulus_id_c"][i] % n_x_steps
        cat_assign_x2 = Math.floor(trial_info["stimulus_id_c"][i] / n_x_steps)
        var x2_tmp = x2_boundaries.map(function (x) { return x < cat_assign_x2 })
        var x1_tmp = x1_boundaries.map(function (x) { return x < cat_assign_x1 });
        var x1_level = 0;
        var x2_level = 0;
        for (var j = 0; j < x1_tmp.length; j++) {
            x1_level += x1_tmp[j]
            x2_level += x2_tmp[j]
        }
        trial_info["category_id"][i] = x2_level * segments_per_dim + (x1_level + 1)
        trial_info["category_name"][i] = stimulus_info["category_name"][trial_info["category_id"][i] - 1]
    }
    return trial_info
}


function assign_items_to_categories_ellipse(experiment_info, trial_info, stimulus_info) {
    if (experiment_info["n_categories"] <= 3) {
        // equal proportion of items from the categories; this is only necessary for ellipse categories
        // but can also be used for similarity judgement, as all items "in the same category"
        const proportion_categories = 1 / experiment_info["n_categories"]
        const items_per_category_required = Math.ceil(experiment_info["n_trials_categorization"] * proportion_categories)
        const items_per_category_repeats = {}
        const items_per_category_gridpoints = {}
        var stimulus_ids_per_category = {}
        var n_items_per_cat, prop_available, repeats;
        // https://www.w3resource.com/javascript-exercises/fundamental/javascript-fundamental-exercise-70.php
        const countOccurrences = (arr, val) => arr.reduce((a, v) => (v === val ? a + 1 : a), 0);
        for (let idx = 0; idx < experiment_info["n_categories"]; idx++) {
            n_items_per_cat = countOccurrences(stimulus_info["category_id"], idx + 1)
            items_per_category_gridpoints[idx + 1] = n_items_per_cat
            prop_available = n_items_per_cat / items_per_category_required
            repeats = Math.ceil(1 / prop_available)
            items_per_category_repeats[idx + 1] = repeats
            stimulus_ids_per_category[idx + 1] = []
        }
        for (let idx = 0; idx < experiment_info["n_stimuli"]; idx++) {
            stimulus_ids_per_category[stimulus_info["category_id"][idx]].push(stimulus_info["stimulus_id"][idx])
        }
        for (let idx = 0; idx < experiment_info["n_categories"]; idx++) {
            stimulus_ids_per_category[idx + 1] = append_randomized_arrays(stimulus_ids_per_category[idx + 1], items_per_category_repeats[idx + 1])
            stimulus_ids_per_category[idx + 1].length = items_per_category_required
            trial_info["stimulus_id_c"] = trial_info["stimulus_id_c"].concat(stimulus_ids_per_category[idx + 1])
        }

        trial_info["stimulus_id_c"] = append_randomized_arrays(trial_info["stimulus_id_c"], 1)
        // add target training in the beginning

        // initial phase of train "target" category is omitted in the second experiment
        //trial_info["stimulus_id_c"] = stim_ids_cats_tt.concat(trial_info["stimulus_id_c"])

        // ellipse categories
        for (let i = 0; i < (experiment_info["n_trials_categorization_total"]); i++) {
            trial_info["category_id"][i] = stimulus_info["category_id"][trial_info["stimulus_id_c"][i]]
            trial_info["category_name"][i] = stimulus_info["category_name"][trial_info["category_id"][i] - 1]
        }
    }
    return trial_info
}

//function to hide one html div and show another
function clickStart(hide, show) {
    document.getElementById(hide).style.display = 'none';
    document.getElementById(show).style.display = 'block';
    window.scrollTo(0, 0);
}

function route_instructions(condition) {
    condition_id = document.getElementById("condition_id").innerHTML
    n_categories = document.getElementById("n_categories").innerHTML
    set_main_vars(condition)
    if (n_categories == 1) { // control
        clickStart('page1', 'page2')
    } else { clickStart('page1', 'page2') }
    var obj = { participant_id: participant_id, condition_id: setup_expt["experiment_info"]["condition_id"] };
}

function route_categorization(condition) {
    if (n_categories == 1) { // control
        clickStart('page6', 'page7b')
    } else { clickStart('page6', 'page7') }
}
async function replace_monster(slider1, slider2) {
    stimulus_id = "[" + slider1.value + "," + slider2.value + "]"
    document.getElementById("selected_monster").src = "./stimuli/stimulus" + stimulus_id + ".png"
}
async function slide_adjust() {
    var slider1 = document.getElementById("myRange1");
    slider1.oninput = function () {
        replace_monster(slider1, slider2)
    }
    var slider2 = document.getElementById("myRange2");

    slider2.oninput = function () {
        replace_monster(slider1, slider2)
    }
}

//https://stackoverflow.com/questions/951021/what-is-the-javascript-version-of-sleep
function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}


async function next_item_cr(old, i) {
    part = parseInt(document.getElementById("part_reproduction").innerHTML)
    if (part == 0) {
        i = parseInt(document.getElementById("trial_nr_cr_practice").innerHTML)
        current_stim_id = stimulus_crp_trial[i]
        current_stim = stimulus_vals[current_stim_id]
    }
    if (part == 1) {
        i = parseInt(document.getElementById("trial_nr_cr1").innerHTML)
        current_stim_id = stimulus_cr1_trial[i]
        current_stim = stimulus_vals[current_stim_id]
    } else if (part == 2) {
        i = parseInt(document.getElementById("trial_nr_cr2").innerHTML)
        current_stim_id = stimulus_cr2_trial[i]
        current_stim = stimulus_vals[current_stim_id]
    }
    clickStart(old, 'page5')


    stim_path = "stimuli/stimulus[" + current_stim + "].png"
    stim_path_mask = "stimuli/mask.png"

    // present stimuli and mask
    await sleep(setup_expt["display_info"]["reproduction"]["iti"])
    document.getElementById("item_displayed_2").src = "stimuli/fixcross.png"
    await sleep(setup_expt["display_info"]["reproduction"]["fixcross"])
    document.getElementById("item_displayed_2").src = stim_path
    await sleep(setup_expt["display_info"]["reproduction"]["presentation"])
    document.getElementById("item_displayed_2").src = stim_path_mask
    await sleep(setup_expt["display_info"]["reproduction"]["ri"])

    // increase trial nr by 1
    document.getElementById("time_var").innerHTML = Date.now()
    clickStart("page5", "page4")

}

async function log_response(rt, i, part, stimulus_ids) {
    var x1_true = parseFloat(setup_expt["stimulus_info"]["x1_x2"][stimulus_ids[i]][0])
    var x2_true = parseFloat(setup_expt["stimulus_info"]["x1_x2"][stimulus_ids[i]][1])
    var x1_response = parseFloat(document.getElementById("myRange1").value)
    var x2_response = parseFloat(document.getElementById("myRange2").value)
    var x1_start = parseFloat(document.getElementById("myRange1_start").value)
    var x2_start = parseFloat(document.getElementById("myRange2_start").value)
    var data_store = {
        participant_id: participant_id,
        n_categories: n_categories,
        session: part,
        trial_id: i,
        x1_true: x1_true,
        x2_true: x2_true,
        x1_response: x1_response,
        x2_response: x2_response,
        x1_start: x1_start,
        x2_start: x2_start,
        rt: rt
    }
    var deviation = Math.sqrt(Math.pow((x1_true - x1_response), 2) + Math.pow((x2_true - x2_response), 2))
    document.getElementById("cr_deviation_cum").innerHTML = parseFloat(document.getElementById("cr_deviation_cum").innerHTML) + deviation

    var val1 = Math.floor(Math.random() * 100);
    var val2 = Math.floor(Math.random() * 100);
    document.getElementById("myRange1").value = val1;
    document.getElementById("myRange2").value = val2;
    document.getElementById("selected_monster").src = "stimuli/stimulus[" + val1 + "," + val2 + "].png"
    saveData(JSON.stringify(data_store), "cr");
}

async function my_link() {
    var rt = Date.now() - document.getElementById("time_var").innerHTML
    var i;
    part = parseInt(document.getElementById("part_reproduction").innerHTML)
    if (part == 0) {
        i = parseInt(document.getElementById("trial_nr_cr_practice").innerHTML)
        stimulus_ids = setup_expt["trial_info"]["stimulus_id_rp"]
    }
    if (part == 1) {
        i = parseInt(document.getElementById("trial_nr_cr1").innerHTML)
        stimulus_ids = setup_expt["trial_info"]["stimulus_id_r1"]
    } else if (part == 2) {
        i = parseInt(document.getElementById("trial_nr_cr2").innerHTML)
        stimulus_ids = setup_expt["trial_info"]["stimulus_id_r2"]
    }

    if (i == total_trials0 & part == 0) { //practice
        log_response(rt, i, part, stimulus_ids);
        clickStart("page4", "page3.1")
        document.getElementById("part_reproduction").innerHTML = 1
    } else if (i == total_trials1 & part == 1) { //part 1 reproduction
        log_response(rt, i, part, stimulus_ids);
        clickStart("page4", "page6");
    } else if (i == total_trials2 & part == 2) { //part 2 reproduction
        log_response(rt, i, part, stimulus_ids);
        calculate_bonus("succeed")
        clickStart("page4", "page13");
    } else {
        log_response(rt, i, part, stimulus_ids);
        update_trial_counter(part, i)
        next_item_cr('page4');
    }
}

function update_trial_counter(part, i) {
    var i_new = i + 1
    switch (part) {
        case 0:
            document.getElementById("trial_nr_cr_practice").innerHTML = i_new
            break;
        case 1:
            document.getElementById("trial_nr_cr1").innerHTML = i_new
            break;
        case 2:
            document.getElementById("trial_nr_cr2").innerHTML = i_new
            break;
    }
}

function saveData(filedata, task) {
    var filename = "./data/" + task + "-participant-" + participant_id + ".json";
    $.post("save_data.php", { postresult: filedata + "\n", postfile: filename })
}

function download(content, fileName, contentType) {
    var a = document.createElement("a");
    var file = new Blob([content], { type: contentType });
    a.href = URL.createObjectURL(file);
    a.download = fileName;
    a.click();
}

// continue with display categorization trial
// log categorization response

function wrap_categorization(old, i) {
    if (setup_expt["experiment_info"]["condition_id"] == 1) {//control
        next_item_control(old, i)
    } else if (setup_expt["experiment_info"]["condition_id"] == 2 | setup_expt["experiment_info"]["condition_id"] == 3) {
        next_item_cat(old, i)
    }
}


async function next_item_cat(old, i) {
    document.getElementById("cat_accuracy_running_mean").style.display = 'block';
    clickStart(old, 'page9')
    current_stim_id = stimulus_cat_trial[i]
    current_stim = stimulus_vals[current_stim_id]
    stim_path = "stimuli/stimulus[" + current_stim + "].png"
    mask_path = "stimuli/mask.png"
    // present stimuli and mask
    document.getElementById("item_displayed_cat").src = mask_path
    await sleep(setup_expt["display_info"]["categorization"]["iti"])
    document.getElementById("item_displayed_cat").src = "stimuli/fixcross.png"
    await sleep(setup_expt["display_info"]["categorization"]["fixcross"])
    document.getElementById("item_displayed_cat").src = stim_path
    document.getElementById("time_var").innerHTML = Date.now()
    document.addEventListener("keydown", handle_response, false);
}


async function handle_response(e) {
    var condition_id = parseInt(document.getElementById("condition_id").innerHTML)
    if (
        n_categories == 1 & (e.keyCode >= 49 && e.keyCode <= 52) ||
        n_categories == 1 & (e.keyCode >= 97 && e.keyCode <= 100) ||
        n_categories == 3 & (e.keyCode >= 49 && e.keyCode <= 51) ||
        n_categories == 3 & (e.keyCode >= 97 && e.keyCode <= 99) ||
        n_categories == 2 & (e.keyCode >= 49 && e.keyCode <= 50) ||
        n_categories == 2 & (e.keyCode >= 97 && e.keyCode <= 98) ||
        n_categories == 4 & (e.keyCode >= 49 && e.keyCode <= 52) ||
        n_categories == 4 & (e.keyCode >= 97 && e.keyCode <= 100)
    ) {
        var break_idx = parseInt(document.getElementById("break_idx").innerHTML)
        var str_frame = "timeframe" + Math.max(break_idx, 1)
        document.getElementById("item_displayed_cat").src = "stimuli/mask.png"

        var i = parseInt(document.getElementById("trial_nr_cat").innerHTML)
        var keyCode = e.keyCode;
        document.getElementById("key_id").innerHTML = keyCode;
        rt = Date.now() - document.getElementById("time_var").innerHTML;
        document.getElementById("rt").innerHTML = rt;

        document.removeEventListener("keydown", handle_response, false);
        cat_id_response = keycode_to_integer(keyCode)
        write_cat_results(i, cat_id_response)

        // responses within deadlinetime
        if (n_categories == 1 & rt <= setup_expt["display_info"]["categorization"]["deadlinetime"]) { // control
            var str = new String("Your response was: " + cat_id_response);
            document.getElementById("feedback_cat_true").innerHTML = str
            await sleep(setup_expt["display_info"]["categorization"]["feedbacktime_true"])
            document.getElementById("feedback_cat_true").innerHTML = ""
        } else if (n_categories > 1 & rt <= setup_expt["display_info"]["categorization"]["deadlinetime"]) {
            if (cat_id_response == category_id[i]) {
                document.getElementById("feedback_cat_true").innerHTML = "Well Done: " + category_name[category_id[i] - 1] + "!"
                await sleep(setup_expt["display_info"]["categorization"]["feedbacktime_true"])
                document.getElementById("feedback_cat_true").innerHTML = ""
            } else {
                var str = new String("Category would have been: " + category_name[category_id[i] - 1]);
                document.getElementById("feedback_cat_wrong").innerHTML = str
                await sleep(setup_expt["display_info"]["categorization"]["feedbacktime_wrong"])
                document.getElementById("feedback_cat_wrong").innerHTML = ""
            }
        }
        // responses given after deadlinetime
        if (rt > setup_expt["display_info"]["categorization"]["deadlinetime"]) {
            document.getElementById("feedback_cat_wrong").innerHTML = "Too slow, please respond faster!"
            await sleep(1000)
            document.getElementById("feedback_cat_wrong").innerHTML = ""
        }
        // reset cat_continued to 0 at a point > 60 secs after a break
        if ((i + 1) % Math.ceil(setup_expt["experiment_info"]["n_trials_categorization_total"] / 4) == 40) {
            document.getElementById("cat_continued").innerHTML = 0
        }
        // update trial counter
        document.getElementById("trial_nr_cat").innerHTML = i + 1

        // handle special timepoint in the experiment
        // end of categorization part
        if (i == setup_expt["experiment_info"]["n_trials_categorization_total"] - 1) {//1) {
            document.getElementById("cat_accuracy_running_mean").style.display = 'none';
            document.getElementById("part_reproduction").innerHTML = 2;
            document.getElementById("cat_continued").innerHTML = 1;
            cat_accuracies = calculate_categorization_accuracy(responses_cat_trial, setup_expt["experiment_info"]["n_trial_categorization_lag"])
            document.getElementById("cat_accuracy_overall").innerHTML = Math.round(100 * cat_accuracies[0]) + "%";
            document.getElementById("cat_accuracy_lag").innerHTML = Math.round(100 * cat_accuracies[1]) + "%";
            console.log("categorization accuracies are: " + cat_accuracies)
            if (
                cat_accuracies[0] >= setup_expt["experiment_info"]["thx_cat_overall"] ||
                cat_accuracies[1] >= setup_expt["experiment_info"]["thx_cat_lag"] ||
                setup_expt["experiment_info"]["n_categories"] == 1
            ) {
                clickStart("page9", "page11")
            } else {
                calculate_bonus("dropout")
                clickStart("page9", "page14")
            }

        } // end of train-target trials
        else if (n_categories != 1 & i == setup_expt["experiment_info"]["n_trials_categorization_train_target"] - 1) {
            clickStart("page9", "page10b")

        } else if (
            // breaks after each quarter of the categorization trials
            (i + 1) % Math.ceil(setup_expt["experiment_info"]["n_trials_categorization_total"] / 4) == 0 &
            (i + 1) != setup_expt["experiment_info"]["n_trials_categorization_total"]
        ) {
            document.getElementById("break_idx").innerHTML = parseInt(document.getElementById("break_idx").innerHTML) + 1
            var break_idx = document.getElementById("break_idx").innerHTML
            trial_nr = i + 1
            document.getElementById("progress").innerHTML = "Your progress in the second part: " + trial_nr +
                " / " + setup_expt["experiment_info"]["n_trials_categorization_total"]
            clickStart("page9", "page10")
            str_countdown = "#time" + break_idx
            str_frame = "timeframe" + break_idx
            document.getElementById(str_frame).style.display = "block"
            var seconds = 60;
            var display = document.querySelector(str_countdown);
            startTimer(seconds, display);

            var timeout = setTimeout(function () {
                if (document.getElementById("cat_continued").innerHTML == 0) {
                    clickStart("page10", 'page9');
                    next_item_cat("page9")
                    document.getElementById("cat_continued").innerHTML = 1
                    document.getElementById(str_frame).style.display = "none"
                }
            }, 61000);

        } else {
            // default case continuing with next trial
            next_item_cat('page9', i + 1);
            document.getElementById(str_frame).style.display = "none"
        }
    }
}


function calculate_categorization_accuracy(responses_cat_trial, lag) {
    n_responses = responses_cat_trial.length
    sum_correct = responses_cat_trial.reduce((a, b) => a + b, 0)
    prop_correct_overall = ((parseFloat(sum_correct) / parseFloat(n_responses)))
    sum_correct_lag = responses_cat_trial.slice(n_responses - lag, n_responses).reduce((a, b) => a + b, 0)
    prop_correct_lag = ((parseFloat(sum_correct_lag) / parseFloat(lag)))
    console.log("calculated average is: " + prop_correct_overall)
    console.log("calculated average 'lag' is: " + prop_correct_lag)
    cat_accuracies = [prop_correct_overall, prop_correct_lag]
    return cat_accuracies
}

function startTimer(duration, display) {
    var timer = duration, minutes, seconds;
    setInterval(function () {
        minutes = parseInt(timer / 60, 10);
        seconds = parseInt(timer % 60, 10);

        minutes = minutes < 10 ? "0" + minutes : minutes;
        seconds = seconds < 10 ? "0" + seconds : seconds;

        display.textContent = minutes + ":" + seconds;

        if (--timer < 0) {
            timer = duration;
        }
    }, 1000);
}


function keycode_to_integer(kc) {
    number_codes = {
        48: 0, 49: 1, 50: 2, 51: 3, 52: 4, 53: 5, 54: 6, 55: 7, 56: 8, 57: 9,
        96: 0, 97: 1, 98: 2, 99: 3, 100: 4, 101: 5, 102: 6, 103: 7, 104: 8, 105: 9
    }
    return number_codes[kc]
}


function write_cat_results(i, r) {
    condition_id = parseInt(document.getElementById("condition_id").innerHTML)
    if (n_categories == 1) {
        accuracy = 9999
    } else if (n_categories > 1) {
        accuracy = setup_expt["trial_info"]["category_id"][i] == r;
        var accuracy_int = accuracy | 0
        responses_cat_trial.push(accuracy_int)
    }
    var data_store = {
        participant_id: participant_id,
        n_categories: n_categories,
        trial_id: i,
        x1_true: setup_expt["stimulus_info"]["x1_x2"][setup_expt["trial_info"]["stimulus_id_c"][i]][0],
        x2_true: setup_expt["stimulus_info"]["x1_x2"][setup_expt["trial_info"]["stimulus_id_c"][i]][1],
        cat_true: setup_expt["trial_info"]["category_id"][i],
        response: r,
        accuracy: accuracy,
        rt: document.getElementById("rt").innerHTML
    }
    if (accuracy === true) {
        document.getElementById("cat_accuracy_cum").innerHTML = parseInt(document.getElementById("cat_accuracy_cum").innerHTML) + 1
    }
    document.getElementById("cat_accuracy_running_mean").innerHTML = "Average Accuracy = " +
        parseInt(100 * document.getElementById("cat_accuracy_cum").innerHTML / (i + 1)) + "%"
    //download(JSON.stringify(data_store), 'json.json', 'text/plain');
    saveData(JSON.stringify(data_store), "cat")
}


// color text
function color(id, col) {
    document.getElementById(id).style.color = col;
}


function colorWrongAnswer(question, col) {
    const rbs = document.querySelectorAll('input[name="' + question + '\"]');
    for (const rb of rbs) {
        if (rb.checked) {
            color(question + rb.id, col)
            break;
        }
    }
}


function checkOnPage(page) {
    if (document.getElementById(page).style.display == 'block') { return true }
    else { return false }
}


//changes inner HTML of div with ID=x to y
function change(x, y) {
    document.getElementById(x).innerHTML = y;
}


function changeColor(element, color) {
    document.getElementById(element).style.color = color;
}


var flag = 0;
var instcounter = 0;
function instructioncheck(pg, pg_prev) {
    var ch1 = 0;
    var ch2 = 0;
    var ch3 = 0;
    var ch4 = 0;
    var ch5 = 0;
    //check if correct answers are provided
    if (document.getElementById('icheck1').checked) { var ch1 = 1; color('q1icheck1', 'green') }
    else { colorWrongAnswer("q1", 'red') }
    if (document.getElementById('icheck2').checked) { var ch2 = 1; color('q2icheck2', 'green') }
    else { colorWrongAnswer("q2", 'red') }
    if (document.getElementById('icheck3').checked) { var ch3 = 1; color('q3icheck3', 'green') }
    else { colorWrongAnswer("q3", 'red') }
    if (document.getElementById('icheck4').checked) { var ch4 = 1; color('q4icheck4', 'green') }
    else { colorWrongAnswer("q4", 'red') }
    if (document.getElementById('icheck5').checked) { var ch5 = 1; color('q5icheck5', 'green') }
    else { colorWrongAnswer("q5", 'red') }
    var checksum = ch1 + ch2 + ch3 + ch4 + ch5;
    var criterion = 5;

    // indicate correct answers
    ++flag;
    clickStart(pg, pg);
    change("check", "Continue")

    // page transition 
    if ((checksum === criterion) && (flag == 2)) {
        //if correct, continue 
        //begintrial();
        clickStart(pg, 'page3');
        // alert
        alert('Great, you have answered all of the questions correctly. The study will now start.');
    }
    else {
        if (flag == 2) {
            instcounter++;
            colorWrongAnswer("q1", '#333333')
            colorWrongAnswer("q2", '#333333')
            colorWrongAnswer("q3", '#333333')
            colorWrongAnswer("q4", '#333333')
            colorWrongAnswer("q5", '#333333')
            //if one or more answers are wrong, raise alert box
            alert('You have answered some of the questions wrong. Please try again.');
            // go back to instructions
            clickStart(pg, pg_prev);
            flag = 0;

        }
    }

}


function set_category_instruction(n_categories) {
    var text;
    const text_3 = `There are two target categories and one non-target category.<br>One target category is called <b>Bukil</b>, the other target category is called <b>Venak</b>.<br>
    In the beginning of the experiment, you are presented with ` + setup_expt["experiment_info"]["n_trials_categorization_train_target"] +
        ` monsters only from the Bukil and Venak categories to get familiar with the two target categories.<br>
    After that phase, you are presented with all types of monsters.<br>
    After every response you are given feedback whether your response was correct or not accompanied by the true category name.<br><br>
    
    <b>Responding:</b><br>
    <b>Please try to respond within 3 seconds as accurately as possible.</b> You will get feedback to respond faster if you respond too slowly!<br>
    You can use the number keys on your keyboard to give a response in the task.<br>
    The numbers correspond to the respective category:<br>
    "1" on your keyboard corresponds to the non-target category.<br>
    "2" on your keyboard corresponds to the "Bukil" category.<br>
    "3" corresponds to the "Venak" category.<br><br>
    The next trial starts immediately after the feedback message has been displayed to you.`

    const text_2 = `There are two categories to be learned.<br>
    The names of the categories are <b>Bukil</b> and <b>Venak</b>.<br>
    Throughout the experiment you are going to see monsters from both categories.<br>
    Your goal is to learn to categorize the monsters into the respective category using feedback, which is provided after every response.<br>
    The feedback tells you whether your response was correct or not accompanied by the true category name.<br><br>

    <b>Important</b><br>
    Whether you make it to the third part / bonus round of the experiment depends on your performance in the categorization task.<br>
    You make it to the third part when your performance satisfies at least one of the two evaluation criteria:<br>
    First, if at least  ` +
        parseInt(100 * setup_expt["experiment_info"]["thx_cat_overall"]) + `
    of your categorization responses are correct.<br>
    Second, if at least ` +
        parseInt(100 * setup_expt["experiment_info"]["thx_cat_lag"]) + `
    of your categorization responses in the last ` +
        setup_expt["experiment_info"]["n_trial_categorization_lag"] + ` trials are correct.<br>
    The running average of your categorization accuracy is shown to you in the upper right corner of the screen throughout the categorization task.<br>

    <b> Responding:</b> <br>
    <b>Please try to respond within 3 seconds as accurately as possible.</b> You will get feedback to respond faster if you respond too slowly!<br>
    You can use the number keys on your keyboard to give a response in the task.<br>
    The numbers correspond to the respective category:<br>
    "1" on your keyboard corresponds to the non-target category.<br>
    "2" on your keyboard corresponds to the "Bukil" category.<br><br>
    The next trial starts immediately after the feedback message has been displayed to you.`

    const text_4 = `There are four categories to be learned.<br>
    The names of the categories are <b>Bukil</b>, <b>Venak</b>, <b>Monus<b>, and <b>Ladiv<b>.<br>
    Throughout the experiment you are going to see monsters from all four categories.<br>
    Your goal is to learn to categorize the monsters into the respective category using feedback, which is provided after every response.<br>
    The feedback tells you whether your response was correct or not accompanied by the true category name.<br><br>

    <b>Important</b><br>
    Whether you make it to the third part / bonus round of the experiment depends on your performance in the categorization task.<br>
    You make it to the third part when your performance satisfies at least one of the two evaluation criteria:<br>
    First, if at least  ` +
        parseInt(100 * setup_expt["experiment_info"]["thx_cat_overall"]) + `
    of your categorization responses are correct.<br>
    Second, if at least ` +
        parseInt(100 * setup_expt["experiment_info"]["thx_cat_lag"]) + `
    of your categorization responses in the last ` +
        setup_expt["experiment_info"]["n_trial_categorization_lag"] + ` trials are correct.<br>
        The running average of your categorization accuracy is shown to you in the upper right corner of the screen throughout the categorization task.<br>

    <b> Responding:</b> <br>
    <b>Please try to respond within 3 seconds as accurately as possible.</b> You will get feedback to respond faster if you respond too slowly!<br>
    You can use the number keys on your keyboard to give a response in the task.<br>
    The numbers correspond to the respective category:<br>
    "1" on your keyboard corresponds to the "Bukil" category.<br>
    "2" on your keyboard corresponds to the "Venak" category.<br>
    "3" on your keyboard corresponds to the "Monus" category.<br>
    "4" on your keyboard corresponds to the "Ladiv" category.<br><br>
    The next trial starts immediately after the feedback message has been displayed to you.`

    if (n_categories == 2) {
        text = text_2
    } else if (n_categories == 3) {
        text = text_3
    } else if (n_categories == 1) {
        text = ""
    } else if (n_categories == 4) {
        text = text_4
    }
    return (text)
}


function load_csv() {
    var txt = d3.json("rotate-conditions.json", function (data) {
        var condition_counts;
        //condition_counts = Object.values(data);
        condition_counts = Object.keys(data).map(function (e) {
            return data[e]
        })
        var max_counts = 9999999999;
        for (var i = 0; i < condition_counts.length; i++) {
            var obj = condition_counts[i]
            if (obj < max_counts) {
                max_counts = obj
                condition_id = [1, 2, 3][i]
            }
        }
        n_categories = [1, 2, 3][(condition_id % 3)]
        const str_idx = "condition" + condition_id
        data[str_idx] += 1;
        document.getElementById("condition_id").innerHTML = condition_id
        document.getElementById("n_categories").innerHTML = n_categories
        saveConditions(JSON.stringify(data));
    });
    clickStart('page0', 'page1')
}


function condition_and_ncategories() {
    n_different_categories = 3;
    var condition_id = Math.ceil(Math.random() * n_different_categories);
    var n_categories = [1, 2, 4][(condition_id % n_different_categories)] // similarity, ellipse, & squares
    condition_id = 3
    n_categories = 4
    console.log("nr categories = " + n_categories)
    document.getElementById("condition_id").innerHTML = condition_id
    document.getElementById("n_categories").innerHTML = n_categories
    if (n_categories == 1) {
        textCond = `<b>Similarity:</b> Your task in the second part will be somewhat different.<br>
                                You are asked to judge how similar the monster presented on the current trial is to the monster presented on the previous trial.<br>
                                To do so, use the numbers from 1-4 on the keyboard.<br>
                                When the monster is exactly the same as on the previous trial, press 4.<br>
                                When the monster looks very different (i.e., head and belly differ a lot), press 1.<br>
                                <b> --> Takes approx. 45 min</b><br>
                                There will be breaks in between these 45 mins.`
    } else {
        textCond = `<b>Categorization:</b> The monsters all look somewhat similar, but they come from different tribes.<br>
                                Use the information about the spikiness of their head and the fill of their belly to gauge what tribes they are from.<br>
                                Give your response on a trial using the digit keys on your keyboard.For example, digit key "1" relates to category nr. 1.<br>
                                You are going to get a feedback after every trial, which may help getting you started.<br>
                                <b> --> Takes approx. 45 min</b><br>
                                There will be breaks in between these 45 mins.`
    }
    document.getElementById("taskTextCondition").innerHTML = textCond;
    clickStart('page0', 'page1')
}


function saveConditions(filedata) {
    var filename = "rotate-conditions.json";
    $.post("overwrite_data.php", { postresult: filedata + "\n", postfile: filename })
}


var condition_id;
var n_categories;
var participant_id;
var setup_expt;
var instruction_category;
var stimulus_crp_trial;
var stimulus_cr1_trial;
var stimulus_cr2_trial;
var stimulus_cat_trial;
var category_id;
var category_name;
var stimulus_vals;
var total_trials0;
var total_trials1;
var total_trials2;


function set_main_vars(condition_id) {
    setup_expt = setup_experiment(condition_id);
    instruction_category = set_category_instruction(setup_expt["experiment_info"]["n_categories"])

    var x = document.getElementById("page7");
    x.querySelector(".category_instruction").innerHTML = instruction_category;;

    (() => {
        document.getElementById("myText").innerHTML = document.getElementById("n_categories").innerHTML;
    })();
    stimulus_crp_trial = setup_expt["trial_info"]["stimulus_id_rp"]
    stimulus_cr1_trial = setup_expt["trial_info"]["stimulus_id_r1"]
    stimulus_cr2_trial = setup_expt["trial_info"]["stimulus_id_r2"]
    stimulus_cat_trial = setup_expt["trial_info"]["stimulus_id_c"]
    responses_cat_trial = setup_expt["trial_info"]["response_c"]
    category_id = setup_expt["trial_info"]["category_id"]
    category_name = setup_expt["stimulus_info"]["category_name"]
    stimulus_vals = setup_expt["stimulus_info"]["x1_x2"]
    total_trials0 = setup_expt["experiment_info"]["n_practice_reproduction"] - 1
    total_trials1 = setup_expt["experiment_info"]["n_trials_reproduction_1"] - 1
    total_trials2 = setup_expt["experiment_info"]["n_trials_reproduction_2"] - 1;
}


function saveBonus(filedata) {
    var filename = "./data/bonus.json";
    $.post("save_data.php", { postresult: filedata + "\n", postfile: filename })
}


function calculate_bonus(flag_performance) {
    if (flag_performance == "dropout") {
        var bonus_store = {
            participant_id: participant_id,
            bonus_cr: 0,
            bonus_cat: 0,
            bonus_total: 0
        }
    } else if (flag_performance == "succeed") {
        // bonus continuous reproduction
        const bonus_cr_max = 3.60
        var n_trials_reproduction = setup_expt["experiment_info"]["n_trials_reproduction_1"] + setup_expt["experiment_info"]["n_trials_reproduction_2"]
        var avg_deviation = parseFloat(document.getElementById("cr_deviation_cum").innerHTML) / n_trials_reproduction
        var coef_bonus = Math.min(51, avg_deviation)
        var above_chance = 51 - coef_bonus
        var prop_bonus = above_chance / 46 // anything closer than 5 from the target counts as "perfect"
        var bonus_cr = Math.round((prop_bonus * bonus_cr_max * 100)) / 100

        // bonus categorization
        var bonus_cat;
        if (setup_expt["experiment_info"]["n_categories"] == 1) {
            bonus_cat = 1.80
        } else {
            const bonus_cat_max = 3.60
            var n_trials_categorization = setup_expt["experiment_info"]["n_trials_categorization_total"]
            var prop_correct_cat = parseInt(document.getElementById("cat_accuracy_cum").innerHTML) / n_trials_categorization
            bonus_cat = Math.round((prop_correct_cat * bonus_cat_max * 100)) / 100
        }
        if (bonus_cat < 1.8) { bonus_cat = 1.8 }
        if (bonus_cr < 1.8) { bonus_cr = 1.8 }

        var bonus_total = Math.round((bonus_cr + bonus_cat) * 100) / 100;


        var bonus_store = {
            participant_id: participant_id,
            bonus_cr: bonus_cr,
            bonus_cat: bonus_cat,
            bonus_total: bonus_total
        }
    }

    saveBonus(JSON.stringify(bonus_store));

    (() => {
        document.getElementById("total_bonus").innerHTML = bonus_total;
        document.getElementById("cr_bonus").innerHTML = bonus_cr;
        document.getElementById("cat_bonus").innerHTML = bonus_cat;
    })();

}

function redirect_to_prolific() {
    window.location.href = "https://app.prolific.co/submissions/complete?cc=240D34C0";
}
