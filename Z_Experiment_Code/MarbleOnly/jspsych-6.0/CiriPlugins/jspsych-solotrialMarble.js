/**
* jspsych-image-keyboard-response
* Josh de Leeuw
*
* plugin for displaying two stimuli next to one another and get visual response of the choice of the participant. 
*
* documentation: docs.jspsych.org
*
**/


jsPsych.plugins["solotrialMarble"] = (function() {

	var plugin = {};

	jsPsych.pluginAPI.registerPreload('solotrialMarble', 'stimulus', 'image');

	plugin.info = {
		name: 'solotrialMarble',
		description: 'this is a function to generate one style in my risky experiment....',
		parameters: {
			probability: {
				type: jsPsych.plugins.parameterType.STRING,
				pretty_name: 'probability',
				default: 'sad',
				description: 'ProbabilityInfo'
			},
			valueGamble:{
				type: jsPsych.plugins.parameterType.INT,
				pretty_name: 'Value of the Gamble',
				default: 5,
				description: 'How much money is associated with the Gamble?'
			},
			stimulus1: {
				type: jsPsych.plugins.parameterType.IMAGE,
				pretty_name: 'stimulus1',
				default: 'img/blue.png',
				description: 'The first to be displayed'
			},
			stimulus2: {
				type: jsPsych.plugins.parameterType.IMAGE,
				pretty_name: 'stimulus2',
				default: 'img/orange.png',
				description: 'The second to be displayed'
			},			
			stimulus4: {
				type: jsPsych.plugins.parameterType.IMAGE,
				pretty_name: 'Picture to show in Instructions',
				default: 'img/Marble/Blue_new.png',
				description: 'Thats my Dummytrial'
			},
			stimulus3: {
				type: jsPsych.plugins.parameterType.IMAGE,
				pretty_name: 'Picture to show in instructions',
				default: 'img/Marble/Red_new.png',
				description: 'The first image to be displayed'
			},			
			choices: {
				type: jsPsych.plugins.parameterType.KEYCODE,
				array: true,
				pretty_name: 'Choices',
				default: jsPsych.ALL_KEYS,
				description: 'The keys the subject is allowed to press to respond to the stimulus.'
			},
			condition:{
				type: jsPsych.plugins.parameterType.INT,
				pretty_name: 'Condition Index',
				default:null
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

			},
			lastTrial: { //this should be set true after demonstration trial.
				type: jsPsych.plugins.parameterType.OBJECT,
				pretty_name: 'lastTrial',
				default: {},
				description: 'This is an Object'
			},
			lookUpLastTrial: { //this should be set true after demonstration trial.
				type: jsPsych.plugins.parameterType.BOOL,
				pretty_name: 'lookUpLastTrial',
				default: false,
				description: 'If true, trial will end when subject makes a response.'
			}
		}
	}

	plugin.trial = function(display_element, trial) {
		var risky=false;
		var changedOrder = false;
		var gambleKey = 74;
		var probWin=trial.probability
		var valueGamble=trial.valueGamble
		var valueSure=5;
		var left_display=valueSure; //Value Points
		var right_display=valueGamble;
		var payoff;

		switch (trial.probability){
			case "img/05.png": probWin=0.5;  break;
			case "img/025.png": probWin=0.25;  break;
			case "img/075.png": probWin=0.75;  break;
			case "img/0675.png": probWin=0.675; break;
			case "img/0125.png": probWin=0.125; break;
			case "img/0375.png": probWin=0.375; break;
		}//end of switch 
		
		if (trial.condition==3){		
			trial.stimulus2=trial.probability;	//If it is condition 3, change to description.
			var red_marbles=99;
			var blue_marbles=99;
		} else {
			var red_marbles=jsPsych.data.get().filter({trial_type: 'showGrid'}).last().values()[0].NRisk;
			var blue_marbles=jsPsych.data.get().filter({trial_type: 'showGrid'}).last().values()[0].NSafe;
		}
		/*here i mix the stimuli, so they dont always appear on the same side*/
		if (!trial.lookUpLastTrial) {
			var oldOrder = [trial.stimulus1, trial.stimulus2];
			var newOrder = jsPsych.randomization.repeat(oldOrder, 1);			
		} else {
			var oldOrder = jsPsych.data.get().last(1).values()[0].old_order;
			var newOrder = jsPsych.data.get().last(1).values()[0].new_order;
		} //if set true take the entries from the last trial... meaning the presentation trial.
		
		if (newOrder.toString() != oldOrder.toString()){//Risk is left!
			changedOrder=true;
			gambleKey=70;
			left_display=valueGamble;
			right_display=valueSure;		
			var new_html =		"<table style='width:100%'>" +
			"<tr> <th></th> <th>Waehle eine Option!</th> <th></th></tr>" +
			"<tr>" +
			"<td><div class ='unselected' id='left' ><img src=" + newOrder[0]+ " id='jspsych-image-keyboard-response-stimulus1'height='300' width='300'></img></td>" +
			"<td><div style='font-size:60px;'> <p> + </p> </div></td>" +
			"<td><div class='unselected' id='right' ><img src=" + newOrder[1] + " id='jspsych-image-keyboard-response-stimulus2'height='300' width='300'></img></td>" +
			"</tr>" +
			"<tr>" +
			"<td><p class='small'>Druecke <strong> 'F' </strong> um aus diesem Glas zu ziehen. </p>" +
			"<p>Inhalt: </p> <p><img src=" + trial.stimulus4 + " id='jspsych-image-keyboard-response-stimulus1'height='20' width='20'> fuer <strong>"+left_display+"</strong> Bonuspunkte</p> " +				
			"<p class='small'> <img src=" + trial.stimulus3 + " height='20' width='20'> fuer 0 Bonuspunkte. </p></td>" +
			// also hide this. i just want everything to look symmetrical.
			"<td><p><div class='center'  style='visibility: hidden;'><img src='img/Anon_Woman.jpg' height='200' width='200'</img></div></td>" + // the advisor shall always be hidden.								
			"<td><p class='small'>Druecke <strong> 'J' </strong> um aus diesem Glas zu ziehen. " +
			"<p>Inhalt:</p>" +
			"<p><img src=" + trial.stimulus4 + " height='20' width='20'> fuer <strong>"+right_display+"</strong> Bonuspunkte oder" +
			"<p class='small' style= 'visibility: hidden;'> </strong>  <img src=" + trial.stimulus3 + " height='20' width='20'> fuer 0 Bonuspunkte. </p></td>" +
			"</tr>" +
			"</table>"
	
		}else if (newOrder.toString() == oldOrder.toString()){// Risk is right
			gambleKey=74;
			var new_html =
			"<table style='width:100%'>" +
			"<tr> <th></th> <th>Waehle eine Option!</th> <th></th></tr>" +
			"<tr>" +
			"<td><div class ='unselected' id='left' ><img src=" + newOrder[0]+ " id='jspsych-image-keyboard-response-stimulus1'height='300' width='300'></img></td>" +
			"<td><div style='font-size:60px;'> <p> + </p> </div></td>" +
			"<td><div class='unselected' id='right' ><img src=" + newOrder[1] + " id='jspsych-image-keyboard-response-stimulus2'height='300' width='300'></img></td>" +
			"</tr>" +
			"<tr>" +
			"<td><p class='small'>Druecke <strong> 'F' </strong> um aus diesem Glas zu ziehen. </p>" +
			"<p>Inhalt: </p> <p><img src=" + trial.stimulus4 + " id='jspsych-image-keyboard-response-stimulus1'height='20' width='20'> fuer <strong>"+left_display+"</strong> Bonuspunkte</p> " +				
			"<p class='small' style= 'visibility: hidden;'> <img src=" + trial.stimulus3 + " height='20' width='20'> fuer 0 Bonuspunkte. </p></td>" +
			// also hide this. i just want everything to look symmetrical.
			"<td><p><div class='center' style='visibility: hidden;'><img src='img/Anon_Woman.jpg' height='200' width='200'</img></div></td>" + // the advisor shall always be hidden.								
			"<td><p class='small'>Druecke <strong> 'J' </strong> um aus diesem Glas zu ziehen. " +
			"<p>Inhalt:</p>" +
			"<p><img src=" + trial.stimulus4 + " height='20' width='20'> fuer <strong>"+right_display+"</strong> Bonuspunkte oder" +
			"<p class='small'> </strong>  <img src=" + trial.stimulus3 + " height='20' width='20'> fuer 0 Bonuspunkte. </p></td>" +
			"</tr>" +
			"</table>"
		}

		// add prompt
		if (trial.prompt !== null) {
			new_html += trial.prompt;
		}

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
			
			//if (trial.lookUpLastTrial){
				// kill keyboard listeners
				if (typeof keyboardListener !== 'undefined') {
					jsPsych.pluginAPI.cancelKeyboardResponse(keyboardListener);
				}//determine what the participant wins. at this round.
			
				var gambleWin=Boolean(r.bernoulli(probWin,1)[0])//see if the gamble went well...
				//determine what the participant wins. at this round.
				// CAVE!!!!!!! YOU NEED TH RANDOM NUMBER GENERATOR IN ORDER FOR THIS TO WORK.
				if (gambleWin && (gambleKey==response.key)){
					payoff=valueGamble;
				}//end
				else if (!gambleWin && (gambleKey==response.key)){
					payoff=0;
				}			else if ((gambleKey!==response.key)){
					payoff=valueSure;
				}
			
				// here I add the new Payoff to the previous 
				// here I add the new Payoff to the previous payoffs.
				var cumulatedPayoff=null;
			
				if (jsPsych.data.get().select('cumulatedPayoff').max()>0){// to catch - infiniy.
					// here I add the new Payoff to the previous payoffs.
					cumulatedPayoff=payoff+jsPsych.data.get().select('cumulatedPayoff').max();//calculate the Bonus in €.
				}
				else{
					cumulatedPayoff=payoff
				}
				//		}else{// if you only demonstrate, dont add sth to the cumulated payoff
					//			cumulatedPayoff=jsPsych.data.get().select('cumulatedPayoff').max();
					//			payoff=null;
					//		}
					// gather the data to store for the trial
					var trial_data = {
						//these I need for the feedback
						"stimulus1": trial.stimulus1,
						"stimulus2": trial.stimulus2,
						"old_order": oldOrder,
						"new_order": newOrder,
						"changed_Order":changedOrder,
						//these I need for Data analysis
						"rt": response.rt,
						"key_press": response.key,
						"red_marbles":red_marbles,
						"blue_marbles":blue_marbles,
						"changed_Order":changedOrder,
						"riskyKey": gambleKey,
						"OtherChoseRisk":null,
						"ChooseRisk":Number((gambleKey==response.key)),
						"valueGamble":valueGamble,
						"valueSure":valueSure,
						"probGamble":probWin,
						"Social1Ind0":0,
						"payoff":payoff,
						"cumulatedPayoff":cumulatedPayoff
					};

					// move on to the next trial
					jsPsych.finishTrial(trial_data);
				};
				// function to handle responses by the subject
				var after_response = function(info) {
					// after a valid response, the stimulus will have the CSS class 'responded'
					// which can be used to provide visual feedback that a response was recorded
					display_element.querySelector('#jspsych-image-keyboard-response-stimulus1').className += ' responded';
					display_element.querySelector('#jspsych-image-keyboard-response-stimulus2').className += ' responded';
					// only record the first response
					if (response.key == null) {
						response = info;
					}
					//the plugin saves keyboard responses as numbers. check the jspsych file to better understand that 
					console.log(response.key)

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
