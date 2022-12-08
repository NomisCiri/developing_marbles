/**
* jspsych-html-slider-response
* a jspsych plugin for free response survey questions
*
* SC:
* This plugin displays a 10x10 grid with an interactive slider which participants can use to Signal their Confidence in something
* documentation: 
*
*/

jsPsych.plugins['html-slider-estimate'] = (function() {

	var plugin = {};

	plugin.info = {
		name: 'html-slider-estimate',
		description: '',
		parameters: {
			stimulus: {
				type: jsPsych.plugins.parameterType.HTML_STRING,
				pretty_name: 'Stimulus',
				default: "HI",
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
			condition:{
				type: jsPsych.plugins.parameterType.INT,
				pretty_name: 'Condition Index',
				default:null
			},
			button_label: {
				type: jsPsych.plugins.parameterType.STRING,
				pretty_name: 'Button label',
				default: 'Weiter',
				array: false,
				description: 'Label of the button to advance.'
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

	//this is a global variable which i use to adress the table in the responsive design.
	var gridhandle = 'gridhandle'


	plugin.trial = function(display_element, trial) {

		

		//display_element.innerHTML = tableContainer;	
		var tableContainer = "<p> <strong> Wie sicher bist du dir bei deiner Einschaetzung?</p> <p>";
		tableContainer += '<div class="slidecontainer"><input type="range" min="0" max="100" value="50" class="slider" id="Marbleslider"><p ><span align="center" id="demo"></span>%</p>';
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
		
		/*************************************EVERYTHING THAT HAPPENS IF THE SLIDER IS MOVED IS DISPLAYED HERE!
		//WHAT DO YOU DO IF THE SILDER IS MOVED?????????*/
		slider.oninput = function() {
			output.innerHTML = this.value;
				
		} // end what happens when the slider is moved. 
		
		if (trial.condition == 3)
		{
			end_trial();//end trial directly if we are in the third condition
		} 
		
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
				"HowSure": response.response,
				"stimulus": trial.stimulus,
				"trialID":jsPsych.data.get().filter({trial_type: 'showGrid'}).last().values()[0].trialID,
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
