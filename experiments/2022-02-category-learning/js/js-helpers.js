if (window.location.search.indexOf('PROLIFIC_PID') > -1) {
    var participant_id = getQueryVariable('PROLIFIC_PID');
}
// If no ID is present, generate one using random numbers - this is useful for testing
else {
    var participant_id = Math.floor(Math.random() * 100);
}
// STUDY ID
if (window.location.search.indexOf('STUDY_ID') > -1) {
    var studyID = getQueryVariable('STUDY_ID');
}
// get subject ID


function setup_experiment(condition_id) {

    // experiment information
    const experiment_info = {
        n_stimuli: 144,
        n_conditions: 3, // control, 4 categories, 9 categories
        n_reproduction: 2, // baseline and after categorization
        n_practice_reproduction: 3,
        n_trials_reproduction_1: 2, //144, //10
        n_trials_reproduction_2: 2, //144, //
        n_trials_categorization_train_target: 6,
        n_trials_categorization: 10, //500, //
        n_trials_categorization_total: 6 + 10,
        condition_id: condition_id,
        n_categories: n_categories,
        file_path_stimuli: "/stimuli/",
        file_path_reproduction: "transform-reps-cat-1-reproduction.txt",
        file_path_categorization: "transform-reps-cat-1-categorization.txt",
    }
    console.log(condition_id)
    console.log(n_categories)
    // stim_ids of cat2 and cat3
    // randomize these ids
    // select first n_only_target_cat or n_only_target_cat/2 and append them to category_id, category_name, category_stimulus_id

    // read mapping from x1 and x2 values to categories
    const cat2map_val = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    const cat3map_val = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 3, 3, 3, 1, 2, 2, 2, 2, 2, 2, 1, 1, 3, 3, 3, 3, 1, 2, 2, 2, 2, 2, 1, 1, 3, 3, 3, 3, 3, 1, 2, 2, 2, 2, 1, 1, 3, 3, 3, 3, 3, 3, 1, 2, 2, 2, 1, 1, 1, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    const cat0map_val = Array(experiment_info["n_stimuli"]).fill(1)
    var cat2_stim_ids_cat2 = [26, 27, 28, 38, 39, 40, 41, 42, 50, 51, 52, 53, 54, 55, 63, 64, 65, 66, 67, 68, 75, 76, 77, 78, 79, 80, 88, 89, 90, 91, 92, 93, 101, 102, 103, 104, 105, 115, 116, 117]
    var cat3_stim_ids_cat2 = [17, 18, 19, 29, 30, 31, 32, 41, 42, 43, 44, 45, 53, 54, 55, 56, 57, 58, 66, 67, 68, 69, 70, 79, 80, 81, 82, 92, 93, 94]
    var cat3_stim_ids_cat3 = [49, 50, 51, 61, 62, 63, 64, 73, 74, 75, 76, 77, 85, 86, 87, 88, 89, 90, 98, 99, 100, 101, 102, 111, 112, 113, 114, 124, 125, 126]
    var stim_ids_cats_tt = []
    var cat3_stim_ids_all = []
    cat2_stim_ids_cat2 = append_randomized_arrays(cat2_stim_ids_cat2, 1)
    cat2_stim_ids_cat2.length = experiment_info["n_trials_categorization_train_target"]
    cat3_stim_ids_cat2 = append_randomized_arrays(cat3_stim_ids_cat2, 1)
    cat3_stim_ids_cat3 = append_randomized_arrays(cat3_stim_ids_cat3, 1)
    cat3_stim_ids_all = cat3_stim_ids_cat2.concat(cat3_stim_ids_cat3)
    cat3_stim_ids_all = append_randomized_arrays(cat3_stim_ids_all, 1)
    cat3_stim_ids_all.length = experiment_info["n_trials_categorization_train_target"]


    // display info
    const display_info = {
        reproduction: {
            iti: 500,
            fixcross: 500,
            presentation: 1000,
            ri: 1500
        },
        categorization: {
            iti: 1000,
            fixcross: 1000,
            feedbacktime_true: 500,
            feedbacktime_wrong: 1000,
            deadlinetime: 3000
        }
    }



    const n_x_steps = 12;
    var stimulus_info = {
        x1: Array(n_x_steps).fill().map((element, index) => index),
        x2: Array(n_x_steps).fill().map((element, index) => index),
        x1_x2: Array(n_x_steps * n_x_steps),
        stimulus_id: Array(n_x_steps * n_x_steps),
        category_name: ["No Target Category", "Bukil", "Venak"],
        category_id: []
    }

    if (experiment_info["n_categories"] == 2) {
        stimulus_info["category_id"] = cat2map_val
        stim_ids_cats_tt = cat2_stim_ids_cat2
        //set_category_instruction(experiment_info["n_categories"])
    } else if (experiment_info["n_categories"] == 3) {
        stimulus_info["category_id"] = cat3map_val
        stim_ids_cats_tt = cat3_stim_ids_all
        //set_category_instruction()
    } else if (experiment_info["n_categories"] == 1) {
        stimulus_info["category_id"] = cat0map_val
        stim_ids_cats_tt = []
    }
    stimulus_info["n_stimuli"] = stimulus_info["x1"].length * stimulus_info["x2"].length
    var i = 0;
    for (let x1 of stimulus_info["x1"]) {
        for (let x2 of stimulus_info["x2"]) {
            // 12x12 grid of stimuli placed within finer grid of 100x100
            // edge space of 6 units is 
            stimulus_info["x1_x2"][i] = [(x1 + 1) * 8 - 2, (x2 + 1) * 8 - 2]
            stimulus_info["stimulus_id"][i] = i
            i += 1
        }
    }

    // trial info
    const trial_info = {}
    trial_info["stimulus_id_rp"] = [...stimulus_info["stimulus_id"]]
    trial_info["stimulus_id_r1"] = [...stimulus_info["stimulus_id"]]
    trial_info["stimulus_id_r2"] = [...stimulus_info["stimulus_id"]]
    trial_info["stimulus_id_c"] = []
    trial_info["category_id"] = []
    trial_info["category_name"] = []

    const n_reps_practice = 1
    const n_reps_reproduction_1 = Math.ceil(experiment_info["n_trials_reproduction_1"] / stimulus_info["n_stimuli"])
    const n_reps_reproduction_2 = Math.ceil(experiment_info["n_trials_reproduction_2"] / stimulus_info["n_stimuli"])

    trial_info["stimulus_id_rp"] = append_randomized_arrays(trial_info["stimulus_id_rp"], n_reps_practice)
    trial_info["stimulus_id_r1"] = append_randomized_arrays(trial_info["stimulus_id_r1"], n_reps_reproduction_1)
    trial_info["stimulus_id_r2"] = append_randomized_arrays(trial_info["stimulus_id_r2"], n_reps_reproduction_2)
    trial_info["stimulus_id_rp"].length = experiment_info["n_practice_reproduction"]
    trial_info["stimulus_id_r1"].length = experiment_info["n_trials_reproduction_1"]
    trial_info["stimulus_id_r2"].length = experiment_info["n_trials_reproduction_2"]

    // stimulus information
    // create an equal proportion of items from the categories
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

    trial_info["stimulus_id_c"] = stim_ids_cats_tt.concat(trial_info["stimulus_id_c"])

    // ellipse categories
    for (let i = 0; i < (experiment_info["n_trials_categorization_train_target"] + experiment_info["n_trials_categorization"]); i++) {
        trial_info["category_id"][i] = stimulus_info["category_id"][trial_info["stimulus_id_c"][i]]
        trial_info["category_name"][i] = stimulus_info["category_name"][trial_info["category_id"][i] - 1]
    }

    // square categories
    /* segments_per_dim = Math.sqrt(experiment_info["n_categories"])
    category_step = Math.max(...stimulus_info["x1"]) / segments_per_dim
    x1_boundaries = Array(segments_per_dim).fill().map((element, index) => (index + 1) * category_step)
    x2_boundaries = Array(segments_per_dim).fill().map((element, index) => (index + 1) * category_step)
    var cat_assign_x1 = Array(segments_per_dim)
    var cat_assign_x2 = Array(segments_per_dim)
    for (let i = 0; i < experiment_info["n_trials_categorization"]; i++) {
        cat_assign_x1 = trial_info["stimulus_id_c"][i] % n_x_steps
        cat_assign_x2 = Math.ceil(trial_info["stimulus_id_c"][i] / Math.max(...stimulus_info["x1"]))
        //x1_tmp = x1_boundaries.some(function (x) { return cat_assign_x1 > x; })
        var x2_tmp = x2_boundaries.map(function (x) { return x < cat_assign_x2 })
        var x1_tmp = x1_boundaries.map(function (x) { return x < cat_assign_x1 });
        var x1_level = 0;
        var x2_level = 0;
        for (var j = 0; j < x1_tmp.length; j++) {
            x1_level += x1_tmp[j]
            x2_level += x2_tmp[j]
        }
        trial_info["category_id"][i] = x2_level * segments_per_dim + (x1_level + 1)
    } */

    function append_randomized_arrays(set, n) {
        var sets_randomized = [];
        for (let i = 0; i < n; i++) {
            var set_permuted = permute(set)
            sets_randomized = sets_randomized.concat(set_permuted);
        }
        return sets_randomized
    }


    var obj_setup_expt;
    obj_setup_expt = {
        experiment_info: experiment_info,
        display_info: display_info,
        stimulus_info: stimulus_info,
        trial_info: trial_info
    }

    return obj_setup_expt
}

//permute a list
function permute(o) {
    for (var j, x, i = o.length; i; j = Math.floor(Math.random() * i), x = o[--i], o[i] = o[j], o[j] = x);
    return o;
};

//function to hide one html div and show another
function clickStart(hide, show) {
    document.getElementById(hide).style.display = 'none';
    document.getElementById(show).style.display = 'block';
    window.scrollTo(0, 0);
}

function route_instructions(condition) {
    //condition_id = 87;
    //participant_id = 99;
    //n_categories = [1, 2, 3][(condition_id % 3)]
    condition_id = document.getElementById("condition_id").innerHTML
    n_categories = document.getElementById("n_categories").innerHTML
    set_main_vars(condition)
    if (condition == 3) { // control
        clickStart('page1', 'page2b')
    } else { clickStart('page1', 'page2') }
    var obj = { participant_id: participant_id, condition_id: setup_expt["experiment_info"]["condition_id"] };
    document.getElementById("category_instruction").innerHTML = set_category_instruction();
}

function route_categorization(condition) {
    if (condition == 3) { // control
        clickStart('page6', 'page7b')
    } else { clickStart('page6', 'page7') }
}
async function replace_monster(slider1, slider2) {
    stimulus_id = "[" + slider1.value + "," + slider2.value + "]"
    document.getElementById("selected_monster").src = "./stimuli/stimulus" + stimulus_id + ".png"
}
async function slide_adjust() {
    var slider1 = document.getElementById("myRange1");
    var output1 = document.getElementById("demo1");
    output1.innerHTML = slider1.value;
    slider1.oninput = function () {
        output1.innerHTML = this.value;
        replace_monster(slider1, slider2)
    }
    var slider2 = document.getElementById("myRange2");
    var output2 = document.getElementById("demo2");
    output2.innerHTML = slider2.value;
    slider2.oninput = function () {
        output2.innerHTML = this.value;
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
    var data_store = {
        participant_id: participant_id,
        session: part,
        trial_id: i,
        x1_true: setup_expt["stimulus_info"]["x1_x2"][stimulus_ids[i]][0],
        x2_true: setup_expt["stimulus_info"]["x1_x2"][stimulus_ids[i]][1],
        x1_response: document.getElementById("myRange1").value,
        x2_response: document.getElementById("myRange2").value,
        rt: rt
    }
    document.getElementById("myRange1").value = 50;
    document.getElementById("demo1").innerHTML = 50;
    document.getElementById("myRange2").value = 50;
    document.getElementById("demo2").innerHTML = 50;
    document.getElementById("selected_monster").src = "stimuli/stimulus[50,50].png"
    //download(JSON.stringify(data_store), 'json.json', 'text/plain');
    saveData(JSON.stringify(data_store), "cr")
}

async function my_link() {
    var rt = Date.now() - document.getElementById("time_var").innerHTML

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

    if (i == total_trials0 & part == 0) {
        log_response(rt, i, part, stimulus_ids);
        clickStart("page4", "page3.1")
        document.getElementById("part_reproduction").innerHTML = 1
    } else if (i == total_trials1 & part == 1) { //0
        log_response(rt, i, part, stimulus_ids);
        clickStart("page4", "page6");
    } else if (i == total_trials2 & part == 2) { //0
        log_response(rt, i, part, stimulus_ids);
        clickStart("page4", "page13");
    } else {
        log_response(rt, i, part, stimulus_ids);
        update_trial_counter(part, i)
        next_item_cr('page4');
    }
}

function update_trial_counter(part, i) {
    switch (part) {
        case 0:
            document.getElementById("trial_nr_cr_practice").innerHTML = i + 1
            break;
        case 1:
            document.getElementById("trial_nr_cr1").innerHTML = i + 1
            break;
        case 2:
            document.getElementById("trial_nr_cr2").innerHTML = i + 1
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
    i = parseInt(document.getElementById("trial_nr_cat").innerHTML)
    clickStart(old, 'page9')

    document.getElementById("trial_nr_cat").innerHTML = i

    current_stim_id = stimulus_cat_trial[i]
    current_stim = stimulus_vals[current_stim_id]
    stim_path = "stimuli/stimulus[" + current_stim + "].png"
    mask_path = "stimuli/mask.png"

    // present stimuli and mask
    document.getElementById("item_displayed_cat").src = mask_path
    await sleep(setup_expt["display_info"]["reproduction"]["iti"])
    document.getElementById("item_displayed_cat").src = "stimuli/fixcross.png"
    await sleep(setup_expt["display_info"]["reproduction"]["fixcross"])
    document.getElementById("item_displayed_cat").src = stim_path
    document.getElementById("time_var").innerHTML = Date.now()
    document.addEventListener("keydown", handle_response, false);
}

async function handle_response(e) {
    var break_idx = parseInt(document.getElementById("break_idx").innerHTML)
    var str_frame = "timeframe" + Math.max(break_idx, 1)
    document.getElementById("item_displayed_cat").src = "stimuli/mask.png"
    var condition_id = parseInt(document.getElementById("condition_id").innerHTML)
    var i = parseInt(document.getElementById("trial_nr_cat").innerHTML)
    var keyCode = e.keyCode;
    if ((e.keyCode >= 48 && e.keyCode <= 57) || (e.keyCode >= 96 && e.keyCode <= 105)) {
        document.getElementById("key_id").innerHTML = keyCode;
        rt = Date.now() - document.getElementById("time_var").innerHTML;
        document.getElementById("rt").innerHTML = rt;
    }
    document.removeEventListener("keydown", handle_response, false);
    cat_id_response = keycode_to_integer(keyCode)
    write_cat_results(i, cat_id_response)

    if (condition_id == 3 & rt <= setup_expt["display_info"]["categorization"]["deadlinetime"]) { // control
        var str = new String("Your response was: " + cat_id_response);
        document.getElementById("feedback_cat_true").innerHTML = str
        await sleep(setup_expt["display_info"]["categorization"]["feedbacktime_true"])
        document.getElementById("feedback_cat_true").innerHTML = ""
    } else if (condition_id == 1 | condition_id == 2 & rt <= setup_expt["display_info"]["categorization"]["deadlinetime"]) {
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
    if (rt > setup_expt["display_info"]["categorization"]["deadlinetime"]) {
        document.getElementById("feedback_cat_wrong").innerHTML = "Too slow, please respond faster!"
        await sleep(1000)
        document.getElementById("feedback_cat_wrong").innerHTML = ""
    }
    //await sleep(setup_expt["display_info"]["categorization"]["feedbacktime"])
    if ((i + 1) % Math.ceil(setup_expt["experiment_info"]["n_trials_categorization_total"] / 4) == 2) {
        document.getElementById("cat_continued").innerHTML = 0
    }

    document.getElementById("trial_nr_cat").innerHTML = i + 1
    if (i == setup_expt["experiment_info"]["n_trials_categorization_total"] - 1) {//1) {
        document.getElementById("part_reproduction").innerHTML = 2;
        clickStart("page9", "page11")
    } else if (i == setup_expt["experiment_info"]["n_trials_categorization_train_target"] - 1) {
        clickStart("page9", "page10b")
    } else if ((i + 1) % Math.ceil(setup_expt["experiment_info"]["n_trials_categorization_total"] / 4) == 0) {
        document.getElementById("break_idx").innerHTML = parseInt(document.getElementById("break_idx").innerHTML) + 1
        var break_idx = document.getElementById("break_idx").innerHTML
        trial_nr = i + 1
        document.getElementById("progress").innerHTML = "Your Categorization Progress: " + trial_nr +
            " / " + setup_expt["experiment_info"]["n_trials_categorization_total"]
        clickStart("page9", "page10")
        str_countdown = "#time" + break_idx
        str_frame = "timeframe" + break_idx
        document.getElementById(str_frame).style.display = "block"
        var seconds = 5;
        var display = document.querySelector(str_countdown);
        startTimer(seconds, display);

        var timeout = setTimeout(function () {
            if (document.getElementById("cat_continued").innerHTML == 0) {
                clickStart("page10", 'page9');
                next_item_cat("page9")
                document.getElementById("cat_continued").innerHTML = 1
                document.getElementById(str_frame).style.display = "none"
            }
        }, 6000);
    } else {
        next_item_cat('page9');

        document.getElementById(str_frame).style.display = "none"
    }
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
    if (condition_id == 1) {
        accuracy = 9
    } else if (condition_id == 2 | condition_id == 3) {
        accuracy = setup_expt["trial_info"]["category_id"][i] == r;
    }
    var data_store = {
        participant_id: participant_id,
        condition_id: condition_id,
        trial_id: i,
        x1_true: setup_expt["stimulus_info"]["x1_x2"][setup_expt["trial_info"]["stimulus_id_c"][i]][0],
        x2_true: setup_expt["stimulus_info"]["x1_x2"][setup_expt["trial_info"]["stimulus_id_c"][i]][1],
        cat_true: setup_expt["trial_info"]["category_id"][i],
        response: r,
        accuracy: accuracy,
        rt: document.getElementById("rt").innerHTML
    }
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
    var ch6 = 0;
    var ch7 = 0;
    var ch8 = 0;
    var ch9 = 0;
    var ch10 = 0;
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
    if (document.getElementById('icheck6').checked) { var ch6 = 1; color('q6icheck6', 'green') }
    else { colorWrongAnswer("q6", 'red') }
    if (document.getElementById('icheck7').checked) { var ch7 = 1; color('q7icheck7', 'green') }
    else { colorWrongAnswer("q7", 'red') }
    if (document.getElementById('icheck8').checked) { var ch8 = 1; color('q8icheck8', 'green') }
    else { colorWrongAnswer("q8", 'red') }
    if (document.getElementById('icheck9').checked) { var ch9 = 1; color('q9icheck9', 'green') }
    else { colorWrongAnswer("q9", 'red') }
    if (document.getElementById('icheck10').checked) { var ch10 = 1; color('q10icheck10', 'green') }
    else { colorWrongAnswer("q10", 'red') }
    var checksum = ch1 + ch2 + ch3 + ch4 + ch5 + ch6 + ch7 + ch8 + ch9 + ch10;
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
            colorWrongAnswer("q6", '#333333')
            colorWrongAnswer("q7", '#333333')
            colorWrongAnswer("q8", '#333333')
            colorWrongAnswer("q9", '#333333')
            colorWrongAnswer("q10", '#333333')
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
    <b>Please try to respond within 3 seconds.</b> You will get feedback to respond faster if you respond too slowly!<br>
    You can use the number keys on your keyboard to give a response in the task.<br>
    The numbers correspond to the respective category:<br>
    "1" on your keyboard corresponds to the non-target category.<br>
    "2" on your keyboard corresponds to the "Bukil" category.<br>
    "3" corresponds to the "Venak" category.<br>
    Use the enter key on your keyboard to start the next trial while the feedback message is
    displayed to you.`
    const text_2 = `There is one target category and one non-target category. The target category is called <b>Bukil</b>.<br>    
    In the beginning of the experiment, you are presented with ` + setup_expt["experiment_info"]["n_trials_categorization_train_target"] +
        ` monsters only from the Bukil category to get familiar with it.<br>
    After that phase, you are presented with all types of monsters.<br>
    After every response you are given feedback whether your response was correct or not accompanied by the true category name.<br><br>
    
    <b>Responding:</b><br>
    <b>Please try to respond within 3 seconds.</b> You will get feedback to respond faster if you respond too slowly!<br>
    You can use the number keys on your keyboard to give a response in the task.<br>
    The numbers correspond to the respective category:<br>
    "1" on your keyboard corresponds to the non-target category.<br>
    "2" on your keyboard corresponds to the "Bukil" category.<br>
    Use the enter key on your keyboard to start the next trial while the feedback message is
    displayed to you.`
    if (n_categories == 2) {
        text = text_2
    } else if (n_categories == 3) {
        text = text_3
    } else if (n_categories == 1) {
        text = ""
    }
    return (text)
}

function load_csv() {
    var txt = d3.json("rotate-conditions.json", function (data) {
        var condition_counts;
        condition_counts = Object.values(data);
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
    console.log("in set_main_vars n_categories is set to: " + setup_expt["experiment_info"]["n_categories"])
    console.log(instruction_category)

    var x = document.getElementById("page7");
    x.querySelector(".category_instruction").innerHTML = instruction_category;;

    //document.getElementById("category_instruction").innerHTML = instruction_category;
    (() => {
        document.getElementById("myText").innerHTML = document.getElementById("n_categories").innerHTML;
    })();
    stimulus_crp_trial = setup_expt["trial_info"]["stimulus_id_rp"]
    stimulus_cr1_trial = setup_expt["trial_info"]["stimulus_id_r1"]
    stimulus_cr2_trial = setup_expt["trial_info"]["stimulus_id_r2"]
    stimulus_cat_trial = setup_expt["trial_info"]["stimulus_id_c"]
    category_id = setup_expt["trial_info"]["category_id"]
    category_name = setup_expt["stimulus_info"]["category_name"]
    stimulus_vals = setup_expt["stimulus_info"]["x1_x2"]
    total_trials0 = setup_expt["experiment_info"]["n_practice_reproduction"] - 1
    total_trials1 = setup_expt["experiment_info"]["n_trials_reproduction_1"] - 1
    total_trials2 = setup_expt["experiment_info"]["n_trials_reproduction_2"] - 1;
}

