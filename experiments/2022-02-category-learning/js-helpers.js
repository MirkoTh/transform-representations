function setup_experiment() {

    // experiment information
    const experiment_info = {
        n_reproduction: 2,
        n_trials_reproduction_1: 10,
        n_trials_reproduction_2: 10,
        n_trials_categorization: 20,
        file_path_stimuli: "/stimuli/",
        file_path_reproduction: "transform-reps-cat-1-reproduction.txt",
        file_path_categorization: "transform-reps-cat-1-categorization.txt"
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
            isi: 1000,
            fixcross: 1000,
            presentation: 1000,
            ri: 2000
        },
        categorization: {
            isi: 1000,
            fixcross: 1000,
        }
    }

    // stimulus information
    const n_x_steps = 14;
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
            stimulus_info["stimulus_id"][i] = i
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


    n_reps_reproduction_1 = Math.ceil(experiment_info["n_trials_reproduction_1"] / stimulus_info["n_stimuli"])
    n_reps_reproduction_2 = Math.ceil(experiment_info["n_trials_reproduction_2"] / stimulus_info["n_stimuli"])
    n_reps_categorization = Math.ceil(experiment_info["n_trials_categorization"] / stimulus_info["n_stimuli"])

    append_randomized_arrays(trial_info["stimulus_id_r1"], n_reps_reproduction_1)
    append_randomized_arrays(trial_info["stimulus_id_r2"], n_reps_reproduction_2)
    append_randomized_arrays(trial_info["stimulus_id_c"], n_reps_categorization)
    trial_info["stimulus_id_r1"].length = experiment_info["n_trials_reproduction_1"]
    trial_info["stimulus_id_r2"].length = experiment_info["n_trials_reproduction_2"]
    trial_info["stimulus_id_c"].length = experiment_info["n_trials_categorization"]

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
    console.log("./stimuli/stimulus-" + stimulus_id + ".PNG")
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
var stimulus_trial = setup_expt["trial_info"]["stimulus_id_r1"]
async function next_item(old, i) {

    console.log(i)
    console.log("the currently displayed stimulus_id is " + stimulus_trial[i])

    fake_stim_id = stimulus_trial[i] % 9 + 1
    stim_path = "./stimuli/stimulus-" + fake_stim_id + ".PNG"
    stim_path_mask = "./stimuli/mask.PNG"

    // present stimuli and mask
    document.getElementById("item_displayed").src = stim_path
    await sleep(setup_expt["display_info"]["reproduction"]["presentation"])
    document.getElementById("item_displayed").src = stim_path_mask
    await sleep(setup_expt["display_info"]["reproduction"]["ri"])
    // increase trial nr by 1
    document.getElementById("trial_nr_p").innerHTML = i + 1
    clickStart(old, "page4")

}

function log_response() {
    response_x1 = document.getElementById("demo1").value
    response_x2 = document.getElementById("demo2").value
    i = parseInt(document.getElementById("trial_nr_r").innerHTML)
    console.log("here, i is: ", i)
    if (i == 3) {//setup_expt["experiment_info"]["n_trials_reproduction_1"]) {
        clickStart("page4", "page66")
    } else {
        document.getElementById("trial_nr_r").innerHTML = i + 1
        clickStart("page4", "page3")
    }
}
