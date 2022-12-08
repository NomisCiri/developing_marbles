/**
* jspsych-image-keyboard-response
* Josh de Leeuw
*
* This function displays a grid representing marbles these marbles are drawn from a distribution
* This is another implementation of the marble drawing problem where the condition index is a timelinevariables
* of the higher hierarchy and this function here directly executes five draws from the jar.
*
**/

jsPsych.plugins["showGrid"] = (function() {

	var plugin = {};
	jsPsych.pluginAPI.registerPreload('showGrid', 'stimulus','image');
	plugin.info = {
		name: "showGrid",
		parameters: {
			safeball: {
				type: jsPsych.plugins.parameterType.IMAGE,
				pretty_name: 'safe',
				default: 'img/Marble/Blue_new.png',
				description: 'The first to be displayed'
			},
			probability: {
				type: jsPsych.plugins.parameterType.STRING,
				pretty_name: 'probability',
				default: 'sad',
				description: 'ProbabilityInfo'
			},
			riskyball: {
				type: jsPsych.plugins.parameterType.IMAGE,
				pretty_name: 'risk',
				default: 'img/Marble/Red_New.png',
				description: 'The first to be displayed'
			},
			neutralball: {
				type: jsPsych.plugins.parameterType.IMAGE,
				pretty_name: 'risk',
				default: 'img/Marble/Grey_new.png',
				description: 'The first to be displayed'
			},
			condition:{
				type: jsPsych.plugins.parameterType.INT,
				pretty_name: 'Condition Index',
				default:null
			},
			valueGamble:{
				type: jsPsych.plugins.parameterType.STRING,
				pretty_name: 'Whats the value of the gamble',
				default:1
			},
			stimulus_duration: {
				type: jsPsych.plugins.parameterType.INT,
				pretty_name: 'Stimulus duration',
				default: 2000,
				description: 'How long to hide the stimulus.'
			},
		}
	}//end infodef

	// thats a sleeper function which enables me to present the stimulus for a certain amount of time and only then continue.
	function sleep(ms) {
		return new Promise(resolve => setTimeout(resolve, ms));
	}

	// i dont like the async prefix. but i need it in order to display each Marble stimulus for x Seconds and then continue
	plugin.trial = async function(display_element, trial) {
		var probwin=0;
		switch (trial.probability){
			case "img/05.png": probWin=0.5;  break;
			case "img/025.png": probWin=0.25;  break;
			case "img/075.png": probWin=0.75;  break;
			case "img/0675.png": probWin=0.675; break;
			case "img/0125.png": probWin=0.125; break;
			case "img/0375.png": probWin=0.375; break;
			}//end of switch 
		/*DEFINITION OF HEPER VARIABLES*/
		//okay this needs some explanaition. We look up the 
		//var index=trial.trialIDs[trial.condition]//Random Order is generated one Layer above where the experiments timeline is defined. 
		//check the id of the current trial. 
	
		var index=99;
		for (i = 0; i < conditionFile.length; i++){
			if (trial.probability == conditionFile[i].data.id && trial.condition==conditionFile[i].data.condition) // check which ID in the conditionfile matches the ID in the timeline variable and the Uncertainty condition specification.
			{
				index=i;
			}//end check proximity...
		}//end trialloop
	
	
		var color1= 0; //Number of Blue Marbles
		var color2= 0; //Number of Red Marbles
		var color3= 0; //Number of Grey Marbles
		var new_html='';
		var flashNumber=5;
		var HowLong=1000;//how long will the grid be shown?
		var ITI=0; //interstimulusinterval of 500 milliseconds
		/* THIS FIXATION IS SO THAT IT HAS EXACTLY THE SAME RELATIVE PROPORTION THAN IN THE PART WHERE THE STIMULUS IS DISPLAYED*/
		var fixation = "<table style='width:100%'>" +
		"<tr> <th></th> <th style= 'visibility: hidden;'>Du musst dich entscheiden</th> <th></th></tr>" +
		"<tr>" +
		"<td><div class ='unselected' style= 'visibility: hidden;' id='left' ><img src=img/Blue_new.png id='jspsych-image-keyboard-response-stimulus1' height='300' width='300'></img></td>" + //the selected Option.
		"<td><div style='font-size:60px;'> <p> + </p> </div></td>" +
		"<td><div class='unselected' id='right' style= 'visibility: hidden;'><img src=img/Blue_new.png id='jspsych-image-keyboard-response-stimulus2' height='300' width='300'></img></td>" +
		"</tr>" +
		"<tr>" +
		"<td><p class='small' style= 'visibility: hidden;' >Druecke <strong> 'F' </strong> um aus diesem Glas zu ziehen. </p>" +
		"<p style= 'visibility: hidden;' >Inhalt: </p> <p style= 'visibility: hidden;' ><img src= id='jspsych-image-keyboard-response-stimulus1'height='20' width='20'> fuer <strong>5</strong> Bonuspunkte</p> " +				
		"<p class='small' style= 'visibility: hidden;'> <img src= height='20' width='20'> fuer 0 Bonuspunkte. </p></td>" +// also hide this. i just want everything to look symmetrical.
		"<td><p><div class='center' style= 'visibility: hidden;'><img src='img/Anon_Woman.jpg' height='200' width='200'</img></div></td>" + // the advisor shall always be hidden.				
		"<td><p class='small' style= 'visibility: hidden;' >Druecke <strong> 'J' </strong> um aus diesem Glas zu ziehen. " +
		"<p style= 'visibility: hidden;' >Inhalt:</p>" +
		"<p style= 'visibility: hidden;' ><img src= height='20' width='20'> fuer <strong>1000</strong> Bonuspunkte oder" +
		"<p class='small' style= 'visibility: hidden;' > </strong>  <img src=height='20' width='20'> fuer 0 Bonuspunkte. </p></td>" +
		"</tr>" +
		"</table>";
	
	
		/*if (trial.condition == 3)
		{
			//end_trial();
			HowLong=0;
			flashNuber=1;
		}*/
		//show grid only if the condition is not three.
			//This loop draws five samplesof the underlying marble frequency
			for (j = 0; j < flashNumber; j++){
				color1= conditionFile[index].data.sequence_marbles_color1[j];
				color2= conditionFile[index].data.sequence_marbles_color2[j];
				color3=9-(color1+color2);
				randomizer=new Array(9);
				k=0;
				//here i iterate thwough the colors and place them in the grid.	
				new_html='<div class="grid-container">';
				//this one checks blue marbles
				for (i = 0; i < color1; i++) {
					randomizer[k]=1;
					k++;
					//    new_html +='<div class="grid-item"><img src='+trial.safeball+' width="100" height="100"></img></div>';// here I add marbles to the Jar.
				}
				//Then look up how many red marbles.
				for (i = 0; i < color2; i++) {
					randomizer[k]=2;
					k++;
					//   new_html +='<div class="grid-item"><img src='+trial.riskyball+' width="100" height="100"></img></div>';// here I add marbles to the Jar.
				}
				//Then check how many neutral marbles there are to be displayed.
				for (i = 0; i < color3; i++) {
					randomizer[k]=3;
					k++;
					//   new_html +='<div class="grid-item"><img src='+trial.neutralball+' width="100" height="100"></img></div>';// here I add marbles to the Jar.
				}
				randomizer=jsPsych.randomization.repeat(randomizer, 1);// here i shuffle it.
				// now i place the balls here.
				for (k=0;k<randomizer.length;k++){
					if (randomizer[k]==1){//1 is a safeball.
						new_html +='<div class="grid-item"><img src='+trial.safeball+' width="100" height="100"></img></div>';// here I add marbles to the Jar.
					}else if (randomizer[k]==2){//2 is a riskyball
						new_html +='<div class="grid-item"><img src='+trial.riskyball+' width="100" height="100"></img></div>';// here I add marbles to the Jar.
					}else if (randomizer[k]==3){//2 is a riskyball
						new_html +='<div class="grid-item"><img src='+trial.neutralball+' width="100" height="100"></img></div>';// here I add marbles to the Jar.
					}//endif
				}//endfor		 
				new_html += '</div>';//close the container
				// draw The Grid and wait. Then draw the Fixationcross and then wait. 
	
				/*if (trial.condition == 3)
				{
					new_html = "+";						
				}*/
				display_element.innerHTML = new_html;	
				await sleep(HowLong);
				//display_element.innerHTML = fixation;
				await sleep(ITI);
			}
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
			}
			// gather the data to store for the trial
			var trial_data = {
				"NSafe":conditionFile[index].data.sequence_marbles_color1,
				"NRisk":conditionFile[index].data.sequence_marbles_color2,
				"key_press":response.key,
				"probGamble":probWin,//i look up the id of the stimulus and not of the randomized trial. this should make way more sense now.
				"valueGamble":trial.valueGamble,
				"condition":trial.condition,
				"index":index
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

