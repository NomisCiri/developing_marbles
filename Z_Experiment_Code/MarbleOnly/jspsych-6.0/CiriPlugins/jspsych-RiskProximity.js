/**
 *risk Proximity
 * Josh de Leeuw
 *
 * plugin to go through our database of risky choices and identify a demonstraotr with similar 
 * risk attitudes.
 * This script deals with wouters nested data
 *
 **/
jsPsych.plugins["riskProximity"] = (function() {

		var plugin = {};
		plugin.info = {
		name: "riskProximity",
		parameters: {		
		stimulus_duration: {
		  type: jsPsych.plugins.parameterType.INT,
		  pretty_name: 'Stimulus duration',
		  default: 2000,
		  description: 'How long to hide the stimulus.'
		},
	}
}//end infodef
//TODO: ASK YOURSELF IF THERE IS SUPPOSED TO BE SOMETHING LIKE A PROGRESS BAR TO INDICATE" SEARCH FOR ANOTHER PARTICIPANT"
// i dont like the async prefix. but i need it in order to display each Marble stimulus for x Seconds and then continue
// OKAY THI S SCRIPT WORKS NOW. NEXT STEP: find out why it skips the variabledeclaration sometimes.

plugin.trial =  function(display_element, trial) {
		//this script calculates the percentage of risky choices of all participants in the "database"
		// is then evaluates the previous solo condition choices distance towards 
	//var AllSubs = JSON.parse(AllSubs);//Here we have the demonstratorData.	

	display_element.innerHTML = "PROXIMITY IS BEEING CALCULATED";
	 	 
	var TheirDecisions=Array(AllSubs[1].trial.length)//thats a container for decisions of each subject
	var subPercent=Array(AllSubs.length)//thats a container for teh percentage of risky choice for each subject.
	var YourDecisions=Array(1)
	var YourPercent=null
	var ProximityCrit=0.20
	var Index=Array(1)
	var count=0
	var OneCloseSub=Object
 
			
		//Here you calculate the percentage of risky choice of all subjects. 
		for (i = 0; i < AllSubs.length; i++){
		for (j = 0; j < AllSubs[i].trial.length; j++){
			TheirDecisions[j]=(AllSubs[i].trial[j].decision)-1;//-1 because its saved as one and two. i want to have the percent of risky choices (which i assume to be 2)
		}//end trialloop
		 subPercent[i]=math.mean(TheirDecisions);// use math.js
		}//end Subloop
		// Get all Data with Solo choiceTag.
		jsPsych.data.get().filter({test_part: 'SoloChoice'}).values().length
		//check123
		//this one goes through all the choices the subject made in the solo condition and calculates the percentage of risky choice.		
		for (i = 0; i < jsPsych.data.get().filter({test_part: 'SoloChoice'}).values().length; i++){
			YourDecisions[i]=Number(jsPsych.data.get().filter({test_part: 'SoloChoice'}).values()[i].ChooseRisk)//Convert the boolean value to a number and then calculate the percentage
		}//end trialloop
		YourPercent=math.mean(YourDecisions);
				
		for (i = 0; i < subPercent.length; i++){
			if (subPercent[i]<(YourPercent+ProximityCrit) && subPercent[i]>(YourPercent-ProximityCrit)){
				Index[count]=i;
				count++;
			}//end check proximity...
		}//end trialloop
		OneCloseSub=AllSubs[Index[1]].trial
		


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
		"CloseSub":OneCloseSub
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

