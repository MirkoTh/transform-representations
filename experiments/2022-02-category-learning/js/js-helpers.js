/* if (window.location.search.indexOf('PROLIFIC_PID') > -1) {
    var participant_id = getQueryVariable('PROLIFIC_PID');
}
// If no ID is present, generate one using random numbers - this is useful for testing
else {
    var participant_id = Math.floor(Math.random() * 100);
}
// STUDY ID
if (window.location.search.indexOf('STUDY_ID') > -1) {
    var studyID = getQueryVariable('STUDY_ID');
} */
// get subject ID
//const participant_id = 1

// make sure categories are alternated 
function csvToArray(str, delimiter = ",") {
    // slice from start of text to the first \n index
    // use split to create an array from string by delimiter
    const headers = str.slice(0, str.indexOf("\n")).split(delimiter);

    // slice from \n index + 1 to the end of the text
    // use split to create an array of each csv value row
    const rows = str.slice(str.indexOf("\n") + 1).split("\n");

    // Map the rows
    // split values from each row into an array
    // use headers.reduce to create an object
    // object properties derived from headers:values
    // the object passed as an element of the array
    const arr = rows.map(function (row) {
        const values = row.split(delimiter);
        const el = headers.reduce(function (object, header, index) {
            object[header] = values[index];
            return object;
        }, {});
        return el;
    });

    // return the array
    return arr;
}

function initialize_variables() {
    tmp = csvToArray("condition_counts.csv")
    //cond_json = [0, 0, 0]
    console.log(tmp)
    document.getElementById("check").innerHTML = tmp
    clickStart('page0', 'page1')
}
//cond_json = [6, 8, 8];
// var condition_counts = 9999999999;
// var condition_id = -99;
// for (var i = 0; i < cond_json.length; i++) {
//     var obj = cond_json[i]
//     if (obj < condition_counts) {
//         condition_counts = obj
//         condition_id = [1, 2, 3][i]
//     }
// }
// cond_json[condition_id - 1] = cond_json[condition_id - 1] + 1;
//saveCondition(cond_json)
var condition_id = 1;

function setup_experiment() {

    // experiment information
    const experiment_info = {
        n_stimuli: 144,
        n_conditions: 3, // control, 4 categories, 9 categories
        n_reproduction: 2, // baseline and after categorization
        n_practice_reproduction: 3,
        n_trials_reproduction_1: 2, //144, //10
        n_trials_reproduction_2: 2, //144, //
        n_trials_categorization: 2, //500, //
        condition_id: condition_id,
        n_categories: [1, 2, 3][condition_id - 1],
        file_path_stimuli: "/stimuli/",
        file_path_reproduction: "transform-reps-cat-1-reproduction.txt",
        file_path_categorization: "transform-reps-cat-1-categorization.txt",
    }

    // read mapping from x1 and x2 values to categories
    const cat2map_val = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    const cat3map_val = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 3, 3, 1, 2, 2, 2, 2, 2, 1, 1, 1, 3, 3, 3, 3, 1, 2, 2, 2, 2, 2, 1, 1, 3, 3, 3, 3, 3, 1, 2, 2, 2, 2, 1, 1, 1, 3, 3, 3, 3, 3, 1, 2, 2, 1, 1, 1, 1, 1, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
    const cat0map_val = Array(experiment_info["n_stimuli"]).fill(1)

    // display info
    const display_info = {
        /* screenwidth: window.innerWidth,
        screenheight: window.innerHeight,
        x_center: window.innerWidth / 2,
        y_center: window.innerHeight / 2,
        size_stimulus: window.innerHeight / 5,
        size_feedback: window.innerHeight / 10, */

        reproduction: {
            iti: 1000,
            fixcross: 1000,
            presentation: 1000,
            ri: 2000
        },
        categorization: {
            iti: 1000,
            fixcross: 1000,
            feedbacktime: 1000,
        }
    }



    const n_x_steps = 12;
    var stimulus_info = {
        x1: Array(n_x_steps).fill().map((element, index) => index),
        x2: Array(n_x_steps).fill().map((element, index) => index),
        x1_x2: Array(n_x_steps * n_x_steps),
        stimulus_id: Array(n_x_steps * n_x_steps)
    }
    if (experiment_info["n_categories"] == 2) {
        stimulus_info["category_id"] = cat2map_val
    } else if (experiment_info["n_categories"] == 4) {
        stimulus_info["category_id"] = cat3map_val
    } else if (experiment_info["n_categories"] == 1) {
        stimulus_info["category_id"] = cat0map_val
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
    //trial_info["stimulus_id_rp"].push(n_x_steps * n_x_steps - 1)
    trial_info["stimulus_id_r1"] = [...stimulus_info["stimulus_id"]]
    //trial_info["stimulus_id_r1"].push(n_x_steps * n_x_steps - 1)
    trial_info["stimulus_id_r2"] = [...stimulus_info["stimulus_id"]]
    //trial_info["stimulus_id_r2"].push(n_x_steps * n_x_steps - 1)
    trial_info["category_id"] = Array(experiment_info["n_trial"])

    const n_reps_practice = 1
    const n_reps_reproduction_1 = Math.ceil(experiment_info["n_trials_reproduction_1"] / stimulus_info["n_stimuli"])
    const n_reps_reproduction_2 = Math.ceil(experiment_info["n_trials_reproduction_2"] / stimulus_info["n_stimuli"])

    trial_info["stimulus_id_rp"] = append_randomized_arrays(trial_info["stimulus_id_rp"], n_reps_practice)
    trial_info["stimulus_id_r1"] = append_randomized_arrays(trial_info["stimulus_id_r1"], n_reps_reproduction_1)
    trial_info["stimulus_id_r2"] = append_randomized_arrays(trial_info["stimulus_id_r2"], n_reps_reproduction_2)
    trial_info["stimulus_id_rp"].length = experiment_info["n_practice_reproduction"]
    trial_info["stimulus_id_r1"].length = experiment_info["n_trials_reproduction_1"]
    trial_info["stimulus_id_r2"].length = experiment_info["n_trials_reproduction_2"]
    trial_info["stimulus_id_c"] = []

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
        //console.log(items_per_category_repeats[idx + 1])
        stimulus_ids_per_category[idx + 1] = append_randomized_arrays(stimulus_ids_per_category[idx + 1], items_per_category_repeats[idx + 1])
        stimulus_ids_per_category[idx + 1].length = items_per_category_required
        trial_info["stimulus_id_c"] = trial_info["stimulus_id_c"].concat(stimulus_ids_per_category[idx + 1])

    }
    trial_info["stimulus_id_c"] = append_randomized_arrays(trial_info["stimulus_id_c"], 1)

    // ellipse categories
    for (let i = 0; i < experiment_info["n_trials_categorization"]; i++) {
        trial_info["category_id"][i] = stimulus_info["category_id"][trial_info["stimulus_id_c"][i]]
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

    var setup_expt;
    setup_expt = {
        experiment_info: experiment_info,
        display_info: display_info,
        stimulus_info: stimulus_info,
        trial_info: trial_info
    }
    return setup_expt
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
    if (condition == 1) { // control
        clickStart('page1', 'page2b')
    } else { clickStart('page1', 'page2') }
    var obj = { participant_id: participant_id, condition_id: setup_expt["experiment_info"]["condition_id"] };
}

function route_categorization(condition) {
    if (condition == 1) { // control
        clickStart('page6', 'page7b')
    } else { clickStart('page6', 'page7') }
}
async function replace_monster(slider1, slider2) {
    stimulus_id = "[" + slider1.value + "," + slider2.value + "]"
    document.getElementById("selected_monster").src = "stimuli/stimulus" + stimulus_id + ".png"
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


function set_cond_and_cat() {
    document.getElementById("condition_id").innerHTML = setup_expt["experiment_info"]["condition_id"]
    document.getElementById("n_categories").innerHTML = setup_expt["experiment_info"]["n_categories"]
}

let setup_expt = setup_experiment();
var stimulus_crp_trial = setup_expt["trial_info"]["stimulus_id_rp"]
var stimulus_cr1_trial = setup_expt["trial_info"]["stimulus_id_r1"]
var stimulus_cr2_trial = setup_expt["trial_info"]["stimulus_id_r2"]
var stimulus_cat_trial = setup_expt["trial_info"]["stimulus_id_c"]
var category_id = setup_expt["trial_info"]["category_id"]
var stimulus_vals = setup_expt["stimulus_info"]["x1_x2"]

/* for (var idx = 0; idx < stimulus_vals.length; idx++) {
    console.log(stimulus_vals[idx])
}
for (var idx = 0; idx < stimulus_cr2_trial.length; idx++) {
    console.log(stimulus_cr2_trial[idx])
}
for (var idx = 0; idx < stimulus_cat_trial.length; idx++) {
    console.log(stimulus_cat_trial[idx])
}
for (var idx = 0; idx < category_id.length; idx++) {
    console.log(category_id[idx])
}
for (var idx = 0; idx < setup_expt["trial_info"]["stimulus_id_c"].length; idx++) {
    console.log(setup_expt["trial_info"]["stimulus_id_c"][idx])
    console.log(idx)
}
 
for (let idx = 0; idx < setup_expt["experiment_info"]["n_trials_categorization"]; idx++) {
    console.log(
        "stimulus id is: " + setup_expt["trial_info"]["stimulus_id_c"][idx] +
        " x1 x2 vals are: " + setup_expt["stimulus_info"]["x1_x2"][setup_expt["trial_info"]["stimulus_id_c"][idx]] +
        " category id is: " + setup_expt["trial_info"]["category_id"][idx]
    )
}
*/

async function next_item_cr(old, i) {
    part = document.getElementById("part_reproduction").innerHTML
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
    document.getElementById("item_displayed_2").src = "stimuli/fixcross.png"
    await sleep(setup_expt["display_info"]["reproduction"]["iti"])
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

const total_trials0 = setup_expt["experiment_info"]["n_practice_reproduction"] - 1
const total_trials1 = setup_expt["experiment_info"]["n_trials_reproduction_1"] - 1
const total_trials2 = setup_expt["experiment_info"]["n_trials_reproduction_2"] - 1

async function my_link() {
    var rt = Date.now() - document.getElementById("time_var").innerHTML

    part = document.getElementById("part_reproduction").innerHTML
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
        case "0":
            document.getElementById("trial_nr_cr_practice").innerHTML = i + 1
            break;
        case "1":
            document.getElementById("trial_nr_cr1").innerHTML = i + 1
            break;
        case "2":
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
    stim_path_mask = "stimuli/mask.png"

    // present stimuli and mask
    document.getElementById("item_displayed_cat").src = "stimuli/placeholder-white.png"
    await sleep(setup_expt["display_info"]["reproduction"]["iti"])
    document.getElementById("item_displayed_cat").src = "stimuli/fixcross.png"
    await sleep(setup_expt["display_info"]["reproduction"]["fixcross"])
    document.getElementById("item_displayed_cat").src = stim_path
    document.getElementById("time_var").innerHTML = Date.now()
    document.addEventListener("keydown", handle_response, false);
}

async function handle_response(e) {
    var condition_id = parseInt(document.getElementById("condition_id").innerHTML)
    var i = parseInt(document.getElementById("trial_nr_cat").innerHTML)
    var keyCode = e.keyCode;
    if ((e.keyCode >= 48 && e.keyCode <= 57) || (e.keyCode >= 96 && e.keyCode <= 105)) {
        document.getElementById("key_id").innerHTML = keyCode;
        document.getElementById("rt").innerHTML = Date.now() - document.getElementById("time_var").innerHTML;
    }
    document.removeEventListener("keydown", handle_response, false);
    cat_id_response = keycode_to_integer(keyCode)
    write_cat_results(i, cat_id_response)
    if (condition_id == 1) { // control
        var str = new String("Your response was: " + cat_id_response);
        alert(str);
    } else if (condition_id == 2 | condition_id == 3) {
        if (cat_id_response == category_id[i]) {
            alert("Well Done!")
        } else {
            var str = new String("Category would have been: " + category_id[i]);
            alert(str);
        }
    }
    await sleep(setup_expt["display_info"]["categorization"]["feedbacktime"])
    document.getElementById("item_displayed_cat").src = "stimuli/placeholder-white.png"

    if (i == setup_expt["experiment_info"]["n_trials_categorization"] - 1) {//1) {
        document.getElementById("trial_nr_cat").innerHTML = i + 1
        document.getElementById("part_reproduction").innerHTML = 2;
        clickStart("page9", "page11")
    } else if ((i + 1) % Math.ceil(setup_expt["experiment_info"]["n_trials_categorization"] / 4) == 0) {
        trial_nr = i + 1
        document.getElementById("progress").innerHTML = "Your Categorization Progress: " + trial_nr +
            " / " + setup_expt["experiment_info"]["n_trials_categorization"]
        document.getElementById("trial_nr_cat").innerHTML = i + 1
        clickStart("page9", "page10")
    } else {
        document.getElementById("trial_nr_cat").innerHTML = i + 1
        next_item_cat('page9')
    }
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
