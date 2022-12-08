/**
 * jspsych-image-keyboard-response
 * Josh de Leeuw
 *
 * plugin for displaying two stimuli next to one another and get visual response of the choice of the participant. 
 *
 * documentation: docs.jspsych.org
 *
 **/


jsPsych.plugins["advicetrialMarble"] = (function() {

	var plugin = {};

	jsPsych.pluginAPI.registerPreload('advicetrialMarble', 'stimulus', 'image');

	plugin.info = {
		name: 'advicetrialMarble',
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
		var demo=jsPsych.data.get().filter({trial_type: "riskProximity_MarbleRisky"}).values()[0].CloseSub;// this now only works, if the risk proximity plugin has been executed before in the timeline. 
		var demoChoseRisk = false;
		var changedOrder = false;
		var valueGamble=trial.valueGamble
		var valueSure=10;
		var left_display=valueSure; //Value Points
		var right_display=valueGamble;
		
		switch (trial.probability){
			case "img/05.png": probWin=0.5;  break;
			case "img/025.png": probWin=0.25;  break;
			case "img/075.png": probWin=0.75;  break;
			case "img/0675.png": probWin=0.675; break;
			case "img/0125.png": probWin=0.125; break;
			case "img/0375.png": probWin=0.375; break;
			}//end of switch 
			if (trial.condition==3){		
				trail.stimulus2=trial.probability;	
			}
			
		//check the id of the current trial. 
			for (i = 0; i < AllSubsGamble.length; i++){
				if ((AllSubsGamble[i].ppn == demo) && (AllSubsGamble[i].probAv == probability) && (AllSubsGamble[i].valueGamble == trial.valueGamble)) // check which ID in the demonstrator matches the current trial ID
				{
					demoChoseRisk=Boolean((AllSubsGamble[i].choice));
				}//end check proximity...			
			}//end trialloop	
			
		//OneCloseSub[1].decision
		/*here i mix the stimuli, so they dont always appear on the same side*/
		if (!trial.lookUpLastTrial) {
			var oldOrder = [trial.stimulus1, trial.stimulus2];
			var newOrder = jsPsych.randomization.repeat(oldOrder, 1);
			var gambleKey = null;
		} else {
			var oldOrder = jsPsych.data.get().last(1).values()[0].old_order;
			var newOrder = jsPsych.data.get().last(1).values()[0].new_order;
			var gambleKey = null;
		} //if set true take the entries from the last trial... meaning the presentation trial.
//risk is on the right side.
		if (newOrder.toString() == oldOrder.toString()) { // i check if it has been shuffeld. for this i just transfrom the whole array into a string.
			/* THATS WHATS SHOWN AT THE BEGIINING OF A TRIAL*/
			gambleKey = 74;
			if (demoChoseRisk == false) { //demonstrator chooses safe as the left option if shuffle was false
				var new_html =
					"<table style='width:100%'>" +
					"<tr> <th></th> <th>Welche Option wuerdest du waehlen</th> <th></th></tr>" +
					"<tr>" +
					"<td><div class ='demonstrate' id='left' ><img src=" + newOrder[0]+ " id='jspsych-image-keyboard-response-stimulus1'height='300' width='300'></img></td>" +
					"<td><div style='font-size:60px;'> <p> + </p> </div></td>" +
					"<td><div class='unselected' id='right' ><img src=" + newOrder[1] + " id='jspsych-image-keyboard-response-stimulus2'height='300' width='300'></img></td>" +
					"</tr>" +
					"<tr>" +
					"<td><div class= demonstrateleft><p class='small'>Druecke <strong> 'F' </strong> um aus diesem Glas zu ziehen. </p>" +
					"<p>Inhalt: </p> <p><img src=" + trial.stimulus4 + " id='jspsych-image-keyboard-response-stimulus1'height='20' width='20'> fuer <strong>"+left_display+"</strong> Bonuspunkte</p> " +				
					"<p class='small'  style= 'visibility: hidden;'> <img src=" + trial.stimulus3 + " height='20' width='20'> fuer 0 Bonuspunkte. </p></div></td>" +
				// also hide this. i just want everything to look symmetrical.
					"<td><p><div class='center'><img src='img/Anon_Woman.jpg' height='200' width='200'</img></div></td>" + // the advisor shall always be hidden.								
					"<td><p class='small'>Druecke <strong> 'J' </strong> um aus diesem Glas zu ziehen. " +
					"<p>Inhalt:</p>" +
					"<p><img src=" + trial.stimulus4 + " height='20' width='20'> fuer <strong>"+right_display+"</strong> Bonuspunkte oder" +
					"<p class='small'> </strong>  <img src=" + trial.stimulus3 + " height='20' width='20'> fuer 0 Bonuspunkte. </p></td>" +
					"</tr>" +
					"</table>"
					
			} else if (demoChoseRisk == true) { //demonstrator chooses risky as the right option if shuffle was false
				var new_html =
					"<table style='width:100%'>" +
					"<tr> <th></th> <th>Welche Option wuerdest du waehlen</th> <th></th></tr>" +
					"<tr>" +
					"<td><div class ='unselected' id='left' ><img src=" + newOrder[0]+ " id='jspsych-image-keyboard-response-stimulus1'height='300' width='300'></img></td>" +
					"<td><div style='font-size:60px;'> <p> + </p> </div></td>" +
					"<td><div class='demonstrate' id='right' ><img src=" + newOrder[1] + " id='jspsych-image-keyboard-response-stimulus2'height='300' width='300'></img></td>" +
					"</tr>" +
					"<tr>" +
					"<td><p class='small'>Druecke <strong> 'F' </strong> um aus diesem Glas zu ziehen. </p>" +
					"<p>Inhalt: </p> <p><img src=" + trial.stimulus4 + " id='jspsych-image-keyboard-response-stimulus1'height='20' width='20'> fuer <strong>"+left_display+"</strong> Bonuspunkte</p> " +				
					"<p class='small'  style= 'visibility: hidden;'> <img src=" + trial.stimulus3 + " height='20' width='20'> fuer 0 Bonuspunkte. </p></td>" +
				// also hide this. i just want everything to look symmetrical.
					"<td><p><div class='center'><img src='img/Anon_Woman.jpg' height='200' width='200'</img></div></td>" + // the advisor shall always be hidden.								
					"<td><div class= demonstrateright><p class='small'>Druecke <strong> 'J' </strong> um aus diesem Glas zu ziehen. " +
					"<p>Inhalt:</p>" +
					"<p><img src=" + trial.stimulus4 + " height='20' width='20'> fuer <strong>"+right_display+"</strong> Bonuspunkte oder" +
					"<p class='small'> </strong>  <img src=" + trial.stimulus3 + " height='20' width='20'> fuer 0 Bonuspunkte. </p></div></td>" +
					"</tr>" +
					"</table>"
				}// end of stimulus if there was no change.	
		} else if (newOrder.toString() != oldOrder.toString()) {
			gambleKey = 70; //which keypress is indicative of a risky choice.
			changedOrder= true;
			left_display=valueGamble;
			right_display=valueSure;
			 //this is so that you can switch the order in the feedback trials
			if (demoChoseRisk == false) { //demostrator chooses safe as the right option if shuffle was true.
				var new_html =
					"<table style='width:100%'>" +
					"<tr> <th></th> <th>Welche Option wuerdest du waehlen</th> <th></th></tr>" +
					"<tr>" +
					"<td><div class ='unselected' id='left' ><img src=" + newOrder[0]+ " id='jspsych-image-keyboard-response-stimulus1'height='300' width='300'></img></td>" +
					"<td><div style='font-size:60px;'> <p> + </p> </div></td>" +
					"<td><div class='demonstrate' id='right' ><img src=" + newOrder[1] + " id='jspsych-image-keyboard-response-stimulus2'height='300' width='300'></img></td>" +
					"</tr>" +
					"<tr>" +
					"<td><p class='small'>Druecke <strong> 'F' </strong> um aus diesem Glas zu ziehen. </p>" +
					"<p>Inhalt: </p> <p><img src=" + trial.stimulus4 + " id='jspsych-image-keyboard-response-stimulus1'height='20' width='20'> fuer <strong>"+left_display+"</strong> Bonuspunkte</p> " +				
					"<p class='small'> <img src=" + trial.stimulus3 + " height='20' width='20'> fuer 0 Bonuspunkte. </p></td>" +
				// also hide this. i just want everything to look symmetrical.
					"<td><p><div class='center'><img src='img/Anon_Woman.jpg' height='200' width='200'</img></div></td>" + // the advisor shall always be hidden.								
					"<td><div class= demonstrateright><p class='small'>Druecke <strong> 'J' </strong> um aus diesem Glas zu ziehen. " +
					"<p>Inhalt:</p>" +
					"<p><img src=" + trial.stimulus4 + " height='20' width='20'> fuer <strong>"+right_display+"</strong> Bonuspunkte oder" +
					"<p class='small' style= 'visibility: hidden;'> </strong>  <img src=" + trial.stimulus3 + " height='20' width='20'> fuer 0 Bonuspunkte. </div></p></td>" +
					"</tr>" +
					"</table>"
			} else if (demoChoseRisk == true) { //demostrator chooses risky as the left option if shuffle was true.
				var new_html =
					"<table style='width:100%'>" +
					"<tr> <th></th> <th>Welche Option wuerdest du waehlen</th> <th></th></tr>" +
					"<tr>" +
					"<td><div class ='demonstrate' id='left' ><img src=" + newOrder[0]+ " id='jspsych-image-keyboard-response-stimulus1'height='300' width='300'></img></td>" +
					"<td><div style='font-size:60px;'> <p> + </p> </div></td>" +
					"<td><div class='unselected' id='right' ><img src=" + newOrder[1] + " id='jspsych-image-keyboard-response-stimulus2'height='300' width='300'></img></td>" +
					"</tr>" +
					"<tr>" +
					"<td><div class= demonstrateleft><p class='small'>Druecke <strong> 'F' </strong> um aus diesem Glas zu ziehen. </p>" +
					"<p>Inhalt: </p> <p><img src=" + trial.stimulus4 + " id='jspsych-image-keyboard-response-stimulus1'height='20' width='20'> fuer <strong>"+left_display+"</strong> Bonuspunkte</p> " +				
					"<p class='small'> <img src=" + trial.stimulus3 + " height='20' width='20'> fuer 0 Bonuspunkte. </p></div></td>" +
				// also hide this. i just want everything to look symmetrical.
					"<td><p><div class='center'><img src='img/Anon_Woman.jpg' height='200' width='200'</img></div></td>" + // the advisor shall always be hidden.								
					"<td><p class='small'>Druecke <strong> 'J' </strong> um aus diesem Glas zu ziehen. " +
					"<p>Inhalt:</p>" +
					"<p><img src=" + trial.stimulus4 + " height='20' width='20'> fuer <strong>"+right_display+"</strong> Bonuspunkte oder" +
					"<p class='small' style= 'visibility: hidden;'> </strong>  <img src=" + trial.stimulus3 + " height='20' width='20'> fuer 0 Bonuspunkte. </p></td>" +
					"</tr>" +
					"</table>"
			} //end of demonstratorselection. 
		} //end of the alternatives.
		// add prompt
		if (trial.prompt !== null) {
			new_html += trial.prompt;
		}
		// draw for 1000 milliseconds before starting the response listener
		display_element.innerHTML = new_html;

		// store response
		var response = {
			rt: null,
			key: null
		};
		
		
		/* Here you need to define everything that happens after a response is given or the timeout is reached*/
		// function to end trial when it is time
		var end_trial = function() {
			// kill any remaining setTimeout handlers
			jsPsych.pluginAPI.clearAllTimeouts();

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

			// gather the data to store for the trial
			var trial_data = {
				//these i need for the feedback
				"stimulus1": trial.stimulus1,
				"stimulus2": trial.stimulus2,
				"key_press": response.key,
				"old_order": oldOrder,
				"new_order": newOrder,
				"changed_Order":changedOrder,
				//these I need for data analysis.
				"rt": response.rt,
				"red_marbles":jsPsych.data.get().filter({trial_type: 'showGrid'}).last().values()[0].NRisk,
				"blue_marbles":jsPsych.data.get().filter({trial_type: 'showGrid'}).last().values()[0].NSafe,
				"key_press": response.key,
				"changed_Order":changedOrder,
				"riskyKey": gambleKey,
				"OtherChoseRisk":Number(demoChoseRisk),
				"ChooseRisk":Number((gambleKey==response.key)),
				"valueGamble":valueGamble,
				"valueSure":valueSure,
				"probGamble":probWin,
				"Social1Ind0":1,
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
			if (trial.response_ends_trial) {
				end_trial();
			}//check if next trail should start
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
