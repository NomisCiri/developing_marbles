/**
 * jspsych-html-slider-response
 * a jspsych plugin for free response survey questions
 *
 * SC:
 * This plugin displays a 10x10 grid with an interactive slider which participants can use to Signal their Confidence in something
 * documentation: 
 *
 */

jsPsych.plugins['html-slider-grid'] = (function() {

	var plugin = {};

	plugin.info = {
		name: 'html-slider-grid',
		description: '',
		parameters: {
			stimulus: {
				type: jsPsych.plugins.parameterType.HTML_STRING,
				pretty_name: 'Stimulus',
				default: undefined,
				description: 'The HTML string to be displayed'
			},
			safeball: {
				type: jsPsych.plugins.parameterType.IMAGE,
				pretty_name: 'safe',
				default: 'OCU/img/Marble/Blue_new.png',
				description: 'The first to be displayed'
			},
			riskyball: {
				type: jsPsych.plugins.parameterType.IMAGE,
				pretty_name: 'risk',
				default: 'OCU/img/Marble/Red_New.png',
				description: 'The first to be displayed'
			},
			min: {
				type: jsPsych.plugins.parameterType.INT,
				pretty_name: 'Min slider',
				default: 0,
				description: 'Sets the minimum value of the slider.'
			},
			max: {
				type: jsPsych.plugins.parameterType.INT,
				pretty_name: 'Max slider',
				default: 100,
				description: 'Sets the maximum value of the slider',
			},
			start: {
				type: jsPsych.plugins.parameterType.INT,
				pretty_name: 'Slider starting value',
				default: 50,
				description: 'Sets the starting value of the slider',
			},
			step: {
				type: jsPsych.plugins.parameterType.INT,
				pretty_name: 'Step',
				default: 1,
				description: 'Sets the step of the slider'
			},
			labels: {
				type: jsPsych.plugins.parameterType.KEYCODE,
				pretty_name: 'Labels',
				default: [],
				array: true,
				description: 'Labels of the slider.',
			},
			button_label: {
				type: jsPsych.plugins.parameterType.STRING,
				pretty_name: 'Button label',
				default: 'Continue',
				array: false,
				description: 'Label of the button to advance.'
			},
			prompt: {
				type: jsPsych.plugins.parameterType.STRING,
				pretty_name: 'Prompt',
				default: null,
				description: 'Any content here will be displayed below the slider.'
			},
			stimulus_duration: {
				type: jsPsych.plugins.parameterType.INT,
				pretty_name: 'Stimulus duration',
				default: null,
				description: 'How long to hide the stimulus.'
			},
			trial_duration: {
				type: jsPsych.plugins.parameterType.INT,
				pretty_name: 'Trial duration',
				default: null,
				description: 'How long to show the trial.'
			},
			response_ends_trial: {
				type: jsPsych.plugins.parameterType.BOOL,
				pretty_name: 'Response ends trial',
				default: true,
				description: 'If true, trial will end when user makes a response.'
			},
		}
	}

	//needs to be a global variable
	var gridhandle = 'gridhandle'

	//TODO: THE TABLE GETS AN ID AND I CHANGE THE TABLE 

	plugin.trial = function(display_element, trial) {

		/*THIS IS THE GRID WHICH DISPLAYS MARBLES!*/
		var color1 = 50; //Number of Blue Marbles
		var color2 = 50; //Number of Red Marbles
		var rowOpen = "    <tr>\n";
		var rowClose = "    </tr>\n";
		var cellOpen = "        <td>\n";
		var cellClose = "        </td>\n";
		var tableOpen = "<table id='" + gridhandle + "'>\n";
		var tableClose = "</table>\n";
		var tableContainer = "";

		//tableContainer contains all the generated code, then is printed at the end
		tableContainer += tableOpen;

		//loops generate each row and fill with cells
		for (i = 1; i <= 10; i++) {
			tableContainer += rowOpen;
			for (j = 1; j <= 10; j++) {
				//soo how many parts you have to fill with the blue marbles.
				if (color1 !== 0) {
					tableContainer += cellOpen + "<img src=" + trial.safeball + " width='20' height='20'></img> \n" + cellClose;
					color1 = color1 - 1; //loop until you placed all blue marbles
				} else {
					tableContainer += cellOpen + "<img src=" + trial.riskyball + " width='20' height='20'> \n" + cellClose; //place red marbles.
				}
			}
			tableContainer += rowClose;
		}
		tableContainer += tableClose;

		// draw The Grid and wait. Then draw the Fixationcross and then wait. 
		//display_element.innerHTML = tableContainer;	

		if (trial.prompt !== null) {
			html += trial.prompt;
		}
		tableContainer += '<div class="slidecontainer"><input type="range" min="0" max="100" value="50" class="slider" id="Marbleslider"><p>Value: <span id="demo"></span></p>';
		display_element.innerHTML = tableContainer; //add all the HTML stuff i wrote to the display elemet 
		// add submit button
		tableContainer += '<button id="jspsych-html-slider-response-next" class="jspsych-btn">' + trial.button_label + '</button>';
		//display elemet is like screen flip. every html markup command i saved before is now displayed and
		//you can still acess it via an ID and the query Selector
		display_element.innerHTML = tableContainer;	
		//creates a slider object
		var slider = display_element.querySelector('#Marbleslider'); // with query selector you get 
		var output = display_element.querySelector('#demo');
		var grid = display_element.querySelector('#gridhandle');
		output.innerHTML = slider.value;
		grid.innerHTML
		
		//WHAT DO YOU DO IF THE SILDER IS MOVED?????????
		slider.oninput = function() {
				output.innerHTML = this.value;
				color1 = this.value;//set one color to the current slider value
				color2 = 100 - color1; //get the other color
				//draw the same table as above and the replace it.
				var rowOpen = "    <tr>\n";
				var rowClose = "    </tr>\n";
				var cellOpen = "        <td>\n";
				var cellClose = "        </td>\n";
				var tableOpen = "<table id='" + gridhandle + "'>\n";
				var tableClose = "</table>\n";
				var tableContainer = "";
				//tableContainer contains all the generated code, then is printed at the end
				tableContainer += tableOpen;
				//loops generate each row and fill with cells
				for (i = 1; i <= 10; i++) {
					tableContainer += rowOpen;
					for (j = 1; j <= 10; j++) {
						//finding the squares to boldface them
						if (color1 !== 0) {
							tableContainer += cellOpen + "<img src=" + trial.safeball + " width='20' height='20'></img> \n" + cellClose;
							color1 = color1 - 1;
						} else {
							tableContainer += cellOpen + "<img src=" + trial.riskyball + " width='20' height='20'> \n" + cellClose;
						}
					}
					tableContainer += rowClose;
				}
				tableContainer += tableClose;
				grid.innerHTML = tableContainer;
			} // end what happens when the slider is moved. 



		var response = {
			rt: null,
			response: null
		};

		display_element.querySelector('#jspsych-html-slider-response-next').addEventListener('click', function() {
			// measure response time
			var endTime = (new Date()).getTime();

			response.rt = endTime - startTime;
			response.response = display_element.querySelector('#Marbleslider').value;

			slider.oninput = function() {
				output = this.value;
			}
			console.log(output)
			if (trial.response_ends_trial) {
				end_trial();
			} else {
				display_element.querySelector('#jspsych-html-slider-response-next').disabled = true;
			}

		});

		function end_trial() {

			jsPsych.pluginAPI.clearAllTimeouts();

			// save data
			var trialdata = {
				"rt": response.rt,
				"response": response.response,
				"stimulus": trial.stimulus
			};

			display_element.innerHTML = '';

			// next trial
			jsPsych.finishTrial(trialdata);
		}

		if (trial.stimulus_duration !== null) {
			jsPsych.pluginAPI.setTimeout(function() {
				display_element.querySelector('#jspsych-html-slider-response-stimulus').style.visibility = 'hidden';
			}, trial.stimulus_duration);
		}

		// end trial if trial_duration is set
		if (trial.trial_duration !== null) {
			jsPsych.pluginAPI.setTimeout(function() {
				end_trial();
			}, trial.trial_duration);
		}

		var startTime = (new Date()).getTime();
	};

	return plugin;
})();
