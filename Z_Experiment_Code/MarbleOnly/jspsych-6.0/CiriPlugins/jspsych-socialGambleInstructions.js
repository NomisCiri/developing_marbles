/**
 * jspsych-image-keyboard-response
 * Josh de Leeuw
 *
 * plugin for displaying two stimuli next to one another and get visual response of the choice of the participant. 
 *
 * documentation: docs.jspsych.org
 *
 **/


jsPsych.plugins["socialGambleInstructions"] = (function() {

	var plugin = {};

	jsPsych.pluginAPI.registerPreload('socialGambleInstructions', 'stimulus', 'image');

	plugin.info = {
		name: 'socialGambleInstructions',
		description: 'this is a function to generate one style in my risky experiment....',
		parameters: {

			stimulus1: {
				type: jsPsych.plugins.parameterType.IMAGE,
				pretty_name: 'Picture to show in instructions',
				default: 'img/Marble/BlueMarbles.png',
				description: 'The first image to be displayed'
			},
			stimulus2: {
				type: jsPsych.plugins.parameterType.IMAGE,
				pretty_name: 'Picture to show in Instructions',
				default: "img/Marble/DunnoMarbles.png",
				description: 'The second to be displayed'
			},
			stimulus3: {
				type: jsPsych.plugins.parameterType.IMAGE,
				pretty_name: 'Picture to show in Instructions',
				default: "img/Marble/Blue_new.png",
				description: 'Thats my Dummytrial'
			},
			choices: {
				type: jsPsych.plugins.parameterType.KEYCODE,
				array: true,
				pretty_name: 'Choices',
				default: jsPsych.ALL_KEYS,
				description: 'The keys the subject is allowed to press to respond to the stimulus.'
			},
			prompt: {
				type: jsPsych.plugins.parameterType.STRING,
				pretty_name: 'Prompt',
				default: null,
				description: 'Any content here will be displayed below the stimulus.'
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
				description: 'How long to show trial before it ends.'
			},
			response_ends_trial: {
				type: jsPsych.plugins.parameterType.BOOL,
				pretty_name: 'Response ends trial',
				default: true,
				description: 'If true, trial will end when subject makes a response.'
			}
		}
	}

	function sleep(ms) {
		return new Promise(resolve => setTimeout(resolve, ms));
	}

	plugin.trial = async function(display_element, trial) {

		var new_html = '<div id="jspsych-html-keyboard-response-stimulus">' + trial.stimulus + '</div>';
		var showWhat=1;
		//here i check which key has been pressed last time
		try {
			var showWhat = jsPsych.data.get().last(1).values()[0].next_Screen;
		} catch (e) { //if there is an error the script doesnt have any data.
			console.log("FirstTrial Probably.")
			var showWhat = 1;
		}
		var nextScreen = showWhat;
		
				try { //i check which screen is to be displayed
					if (jsPsych.data.get().last(1).values()[0].keypress == 70) {
						showWhat = (jsPsych.data.get().last(1).values()[0].which_Screen - 1);
					} else if (jsPsych.data.get().last(1).values()[0].keypress == 74) {
						showWhat = (jsPsych.data.get().last(1).values()[0].which_Screen + 1);
					} else {
						showWhat = 1;
					}
				} catch (e) { //if there is an error the script doesnt have any data.
					console.log("FirstTrial Probably.")
				}
		
		//catch if someone presses "back" at the first screen.
		if (showWhat < 1) {
			showWhat = 1
		};
		//here I Define the Screens
		startExperiment = false;
		
			//truns on the current screen. 
		switch (showWhat) {
						case 1: //FIRST Screen
							var new_html = '<font size=20>Pause!</font> <p> Vielen Dank! Du hast dir eine kleine Pause verdient. <p>'+
							 '<p style=color:green>Wenn du soweit bist, Druecke "J" um fortzufahren.</p>'
							break;
						case 2: //New Screen
							var new_html = "Dieser Teil der Aufgabe ist dem vorherigen sehr aehnlich."+
								"<p>Im naechsten Teil kannst du dich wieder zwischen 2 Optionen entscheiden.</p>" +
								"<p style='border:3px; border-style:solid; border-color:#FF0000; padding: 1em;'>Deine Entscheidung gibst du wieder mit Hilfe der Tasten <strong>'F'</strong> und <strong>'J'</strong> auf der Tastatur ein."+
								"auf der Tastatur</p>" +
								"<p>Wenn du dich fuer die <strong>linke</strong> Option entscheidest, " +
								"druecke bitte die Taste <strong>'F'</strong> auf der Tastatur.</p>" +
								"<p>Wenn du dich fuer die <strong>rechte</strong>, Option entscheidest, druecke <strong>'J'</strong>" +
								"<div></br></br></div>" +
								"<p style=color:orange> Ist noch etwas unklar? Druecke <strong> 'F' </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> 'J' </strong> um fortzufahren.</p>"
							break;
						case 3: //new Screen
							var new_html ='Manchmal ist es schwierig zu wissen, welche Option man waehlen soll.<p>'+
								'In unserer Datenbank sind die Entscheidungen von Personen gespeichert, welche die gleiche Aufgabe vor dir bearebeitet haben.</p>' +
								'<p>Unser Algorithmus hat eine Person gefunden, deren Entscheidungen aehnlich zu deinen Entscheidungen waren.</p>' +
								"<p><strong>Diese Entscheidungen zeigen wir dir in den naechsten Durchgaengen, bevor du deine eigene Wahl triffst.</strong></p>"+
								'<div></br></br></div>' +
								'<p style=color:orange> Ist noch etwas unklar? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
							break;
						case 4: //new Screen
							var new_html = "<strong> Die Entscheidungen der anderen Person erkennst du daran, dass wir sie eingerahmt haben. </strong>" +
								"<p>Auf dem naechsten Bildschirm kannst du sehen wie es aussieht, wenn wir dir zeigen dass sich die Person fuer die <strong> linke </strong> Option entschieden hat.</p>"+
								'<p style=color:orange> Ist noch etwas unklar? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
							break;
						case 5: //new Screen
							var new_html = "<table style='width:100%'>" +
								"<tr> <th></th> <th>Welche Option wuerdest du waehlen</th> <th></th></tr>" +
								"<tr>" +
								"<td><div class ='demonstrate' id='left' ><img src=" + trial.stimulus1 + " id='jspsych-image-keyboard-response-stimulus1'height='300' width='300'></img></td>" +
								"<td><div style='font-size:60px;'> <p> + </p> </div></td>" +
								"<td><div class='unselected' id='right' ><img src=" + trial.stimulus2 + " id='jspsych-image-keyboard-response-stimulus2'height='300' width='300'></img></td>" +
								"</tr>" +
								"<tr>" +
								"<td><p class='demonstrateleft'>Druecke <strong> 'F' </strong> fuer 5 Bonuspunkte.</p></td>" +
								"<td><p><div class='center'><img src='img/Anon_Woman.jpg' height='200' width='200'</img></div></td>" + 
								"<td><p class='small'> Druecke <strong> 'J' </strong> fuer 10 Bonuspunkte. </p></td>" +
								"</tr>" +
								"</table>"+
								'<p style=color:orange> Ist noch etwas unklar? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
							break;
						case 6: //new Screen
							var new_html = '<p><strong> Ansonsten bleibt alles wie gewohnt </strong></p>' +
								"<div> </div>" +
								'<div>Ob es sich lohnt das Gluecksspiel zu waehlen, musst du bei jedem Durchgang selbst entscheiden. </div>' +
								"<div>Es kann sein, dass du <strong> einen hohen Bonus </strong> bekommst, es kann aber auch sein, dass du <strong> keinen Bonus </strong> bekommst. </div>" +
								"<p> </p>"+
								'<div></br></br></div>' +
								'<p style=color:orange> Ist noch etwas unklar? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
							break;
						case 7:
							var new_html = "Achtung, es geht sofort los!" +
								'<p style=color:orange> Fragen? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style="border:3px; border-style:solid; border-color:#FF0000; padding: 1em; color:green;"> Druecke <strong> "J" </strong> das Experiment zu starten</p>'
							startExperiment = true;
							break;
							
		} //end of switch


		/*the stimuli are organized a t
		able*/
		/********** THIS IS WHERE THE INSTRUCTIONS AND FEEDBACKS ANND SO ON ARE WORKED OUT***********
			
		********************************************************************/



		// draw for 1000 milliseconds before starting the response listener
		display_element.innerHTML = new_html;

		// store response
		var response = {
			rt: null,
			key: null // FOR TESTING PURPOUSES. FOR ACTUAL TASK MAKE THIS NULL AGAIN!
		};
		/* Here you need to define everything that happens after a response is given or the timeout is reached*/
		// function to end trial when it is time
		var end_trial = function() {

			// kill any remaining setTimeout handlers
			jsPsych.pluginAPI.clearAllTimeouts();

			// kill keyboard listeners
			if (typeof keyboardListener !== 'undefined') {
				jsPsych.pluginAPI.cancelKeyboardResponse(keyboardListener);
			}

			// gather the data to store for the trial
			var trial_data = {
				"rt": response.rt,
				"keypress": response.key,
				"which_Screen": showWhat, // this is a numerical value that shows you which the last intoscreen was.
				"next_Screen": nextScreen,
				"startExperiment": startExperiment
			};

			// move on to the next trial
			jsPsych.finishTrial(trial_data);
		};
		// function to handle responses by the subject
		var after_response = function(info) {
			// after a valid response, the stimulus will have the CSS class 'responded'
			// only record the first response
			if (response.key == null) {
				response = info;
			}
			//the plugin saves keyboard responses as numbers. check the jspsych file to better understand that 
			console.log(response.key)
			try {
				if (response.key == 70) {
					nextScreen = (showWhat - 1);
				} else if (response.key == 74) {
					nextScreen = (showWhat + 1);
				} else {
					showWhat = 1;
				}
			} catch (e) { //if there is an error the script doesnt have any data.
				console.log("FirstTrial Probably.")
			};

			if (trial.response_ends_trial) {
				end_trial();
			}
		};
		// start the response listener
		if (trial.choices != jsPsych.NO_KEYS) {
			var keyboardListener = jsPsych.pluginAPI.getKeyboardResponse({
				callback_function: after_response,
				valid_responses: trial.choices,
				rt_method: 'date',
				persist: false,
				allow_held_key: false
			});
		}

		// hide stimulus if stimulus_duration is set
		if (trial.stimulus_duration !== null) {
			jsPsych.pluginAPI.setTimeout(function() {
				display_element.querySelector('#jspsych-image-keyboard-response-stimulus1').style.visibility = 'hidden';
				display_element.querySelector('#jspsych-image-keyboard-response-stimulus2').style.visibility = 'hidden';

			}, trial.stimulus_duration);
		}

		// end trial if trial_duration is set
		if (trial.trial_duration !== null) {
			jsPsych.pluginAPI.setTimeout(function() {
				//display_element.innerHTML = "Du hast zu lange gebraucht. Bitte antworte beim naechten mal schneller!";
				end_trial();
			}, trial.trial_duration);
		}

	};

	return plugin;
})();
