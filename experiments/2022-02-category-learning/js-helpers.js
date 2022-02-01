var participant_id = 1;

function setup_experiment() {

    // experiment information
    const experiment_info = {
        n_reproduction: 2, // baseline and after categorization
        n_trials_reproduction_1: 10,
        n_trials_reproduction_2: 10,
        n_trials_categorization: 20,
        file_path_stimuli: "/stimuli/",
        file_path_reproduction: "transform-reps-cat-1-reproduction.txt",
        file_path_categorization: "transform-reps-cat-1-categorization.txt",

    }
    if (participant_id % 2 == 0) {
        experiment_info["n_categories"] = 4
    } else {
        experiment_info["n_categories"] = 9
    }
    // display info
    const display_info = {
        screenwidth: window.innerWidth,
        screenheight: window.innerHeight,
        x_center: window.innerWidth / 2,
        y_center: window.innerHeight / 2,
        size_stimulus: window.innerHeight / 5,
        size_feedback: window.innerHeight / 10,

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

    // stimulus information
    const n_x_steps = 12;
    var stimulus_info = {
        x1: Array(n_x_steps).fill().map((element, index) => index + 1),
        x2: Array(n_x_steps).fill().map((element, index) => index + 1),
        x1_x2: Array(n_x_steps * n_x_steps),
        stimulus_id: Array(n_x_steps * n_x_steps)
    }
    stimulus_info["n_stimuli"] = stimulus_info["x1"].length * stimulus_info["x2"].length
    var i = 0;
    for (x1 of stimulus_info["x1"]) {
        for (x2 of stimulus_info["x2"]) {
            stimulus_info["x1_x2"][i] = [x1, x2]
            stimulus_info["stimulus_id"][i] = i + 1
            i += 1
        }
    }
    // trial info
    const trial_info = {}
    trial_info["stimulus_id_r1"] = [...stimulus_info["stimulus_id"]]
    trial_info["stimulus_id_r1"].push(n_x_steps * n_x_steps)
    trial_info["stimulus_id_r2"] = [...stimulus_info["stimulus_id"]]
    trial_info["stimulus_id_r2"].push(n_x_steps * n_x_steps)
    trial_info["stimulus_id_c"] = [...stimulus_info["stimulus_id"]]
    trial_info["stimulus_id_c"].push(n_x_steps * n_x_steps)
    trial_info["category_id"] = Array(experiment_info["n_trial"])


    n_reps_reproduction_1 = Math.ceil(experiment_info["n_trials_reproduction_1"] / stimulus_info["n_stimuli"])
    n_reps_reproduction_2 = Math.ceil(experiment_info["n_trials_reproduction_2"] / stimulus_info["n_stimuli"])
    n_reps_categorization = Math.ceil(experiment_info["n_trials_categorization"] / stimulus_info["n_stimuli"])

    append_randomized_arrays(trial_info["stimulus_id_r1"], n_reps_reproduction_1)
    append_randomized_arrays(trial_info["stimulus_id_r2"], n_reps_reproduction_2)
    append_randomized_arrays(trial_info["stimulus_id_c"], n_reps_categorization)
    trial_info["stimulus_id_r1"].length = experiment_info["n_trials_reproduction_1"]
    trial_info["stimulus_id_r2"].length = experiment_info["n_trials_reproduction_2"]
    trial_info["stimulus_id_c"].length = experiment_info["n_trials_categorization"]

    segments_per_dim = Math.sqrt(experiment_info["n_categories"])
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
    }

    function append_randomized_arrays(set, n) {
        sets_randomized = Array()
        for (let i = 0; i < n; i++) {
            set_permuted = permute(set)
            sets_randomized.concat(set_permuted);
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

function slide_adjust() {
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

replace_monster = function (slider1, slider2) {
    stimulus_id = (parseInt(slider1.value) - 1) * 3 + parseInt(slider2.value)
    document.getElementById("selected_monster").src = "./stimuli/stimulus-" + stimulus_id + ".PNG"
}

display_trial_reproduction = function (setup, i) {
    stimulus_id = setup["trial_info"]["trial_seq_reproduction_1"][i]
}

https://stackoverflow.com/questions/951021/what-is-the-javascript-version-of-sleep
function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}


let setup_expt = setup_experiment();
var stimulus_cr1_trial = setup_expt["trial_info"]["stimulus_id_r1"]
var stimulus_cr2_trial = setup_expt["trial_info"]["stimulus_id_r2"]
var stimulus_cat_trial = setup_expt["trial_info"]["stimulus_id_c"]
var category_id = setup_expt["trial_info"]["category_id"]
console.log(category_id)
console.log(category_id[3])

async function next_item_cr(old, i) {
    i = parseInt(document.getElementById("trial_nr_cr1").innerHTML)
    clickStart(old, 'page5')

    fake_stim_id = stimulus_cr1_trial[i] % 9 + 1
    stim_path = "./stimuli/stimulus-" + fake_stim_id + ".PNG"
    stim_path_mask = "./stimuli/mask.PNG"

    // present stimuli and mask
    document.getElementById("item_displayed_2").src = "./stimuli/fixcross.PNG"
    await sleep(setup_expt["display_info"]["reproduction"]["iti"])
    document.getElementById("item_displayed_2").src = stim_path
    await sleep(setup_expt["display_info"]["reproduction"]["presentation"])
    document.getElementById("item_displayed_2").src = stim_path_mask
    await sleep(setup_expt["display_info"]["reproduction"]["ri"])

    // increase trial nr by 1
    document.getElementById("time_var").innerHTML = Date.now()
    clickStart("page5", "page4")

}

function log_response(rt) {
    i = parseInt(document.getElementById("trial_nr_cr1").innerHTML)

    var data_store = {
        participant_id: participant_id,
        session: 1,
        trial_id: i,
        x1_true: setup_expt["stimulus_info"]["x1_x2"][setup_expt["trial_info"]["stimulus_id_r1"][i]][0],
        x2_true: setup_expt["stimulus_info"]["x1_x2"][setup_expt["trial_info"]["stimulus_id_r1"][i]][1],
        x1_response: document.getElementById("demo1").value,
        x2_response: document.getElementById("demo2").value,
        rt: rt
    }
    //download(JSON.stringify(data_store), 'json.json', 'text/plain');
    //saveData(JSON.stringify(data_store))

    console.log("log_response, i is: ", i)
    document.getElementById("trial_nr_cr1").innerHTML = i + 1
}

async function my_link() {
    var rt = Date.now() - document.getElementById("time_var").innerHTML
    i = parseInt(document.getElementById("trial_nr_cr1").innerHTML)
    if (i == 1) {
        log_response(rt);
        clickStart("page4", "page6");
    } else {
        log_response(rt);
        next_item_cr('page4');
    }
}


function saveData(filedata) {
    var filename = "./data/" + participant_id + ".json";
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

async function next_item_cat(old, i) {
    i = parseInt(document.getElementById("trial_nr_cat").innerHTML)
    clickStart(old, 'page9')

    console.log(i)
    document.getElementById("trial_nr_cat").innerHTML = i
    console.log("the currently displayed stimulus_id is " + stimulus_cat_trial[i])

    fake_stim_id = stimulus_cat_trial[i] % 9 + 1
    stim_path = "./stimuli/stimulus-" + fake_stim_id + ".PNG"
    stim_path_mask = "./stimuli/mask.PNG"

    // present stimuli and mask
    document.getElementById("item_displayed_cat").src = "./stimuli/fixcross.PNG"
    await sleep(setup_expt["display_info"]["reproduction"]["iti"])
    document.getElementById("item_displayed_cat").src = stim_path
    document.getElementById("time_var").innerHTML = Date.now()
    document.addEventListener("keydown", handle_response, false);

}

async function handle_response(e) {
    var i = parseInt(document.getElementById("trial_nr_cat").innerHTML)
    var keyCode = e.keyCode;
    if ((e.keyCode >= 48 && e.keyCode <= 57) || (e.keyCode >= 96 && e.keyCode <= 105)) {
        document.getElementById("key_id").innerHTML = keyCode;
        document.getElementById("rt").innerHTML = Date.now() - document.getElementById("time_var").innerHTML;
    }
    cat_id_response = keycode_to_integer(keyCode)
    write_cat_results(i, cat_id_response)
    if (cat_id_response == category_id[i]) {
        alert("Well Done!")
    } else {
        var str = new String("Category would have been: " + category_id[i]);
        alert(str);
    }
    await sleep(setup_expt["display_info"]["categorization"]["feedbacktime"])
    document.getElementById("item_displayed_cat").src = "./stimuli/placeholder-white.PNG"

    if (i == setup_expt["experiment_info"]["n_trials_categorization"]) {
        document.getElementById("trial_nr_cat").innerHTML = i + 1
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
    accuracy = setup_expt["trial_info"]["category_id"] == r;
    var data_store = {
        participant_id: participant_id,
        trial_id: i,
        x1_true: setup_expt["stimulus_info"]["x1_x2"][setup_expt["trial_info"]["stimulus_id_c"][i]][0],
        x2_true: setup_expt["stimulus_info"]["x1_x2"][setup_expt["trial_info"]["stimulus_id_c"][i]][1],
        cat_true: setup_expt["trial_info"]["category_id"],
        cat_response: r,
        accuracy: accuracy,
        rt: document.getElementById("rt").innerHTML
    }
    //download(JSON.stringify(data_store), 'json.json', 'text/plain');
    //saveData(JSON.stringify(data_store))
}