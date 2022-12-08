/**
 * jspsych-image-keyboard-response
 * Josh de Leeuw
 *
 * plugin for displaying two stimuli next to one another and get visual response of the choice of the participant. 
 *
 * documentation: docs.jspsych.org
 *
 **/


jsPsych.plugins["soloGambleInstructions"] = (function() {

	var plugin = {};

	jsPsych.pluginAPI.registerPreload('soloGambleInstructions', 'stimulus', 'image');

	plugin.info = {
		name: 'soloGambleInstructions',
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
		var showWhat = 1;
		//here i check which key has been pressed last time
		try {
			var showWhat = jsPsych.data.get().filter({trial_type: 'soloGambleInstructions'}).last().values()[0].next_Screen;
		} catch (e) { //if there is an error the script doesnt have any data.
			console.log("FirstTrial Probably.")
			var showWhat = 1;
		}
		var nextScreen = showWhat;

		//catch if someone presses "back" at the first screen.
		if (showWhat < 1) {
			showWhat = 1
		};
		//here I Define the Screens
		startExperiment = false;

			//truns on the current screen. 
		switch (showWhat) {
			case 1: //FIRST Screen
				var new_html = "<p>Hallo Liebe(r) Teilnehmer(in)</p><p> Willkommen zu unserer Studie 'PI-RISK'.</p>" +
					"<p>Die Studie besteht aus mehreren Teilaufgaben. Bitte lese dir die Anleitungen jeweils genau durch.</p>"+
					"<p>Falls du Fragen hast, kannst du dich gern an die Versuchsleitung wenden.</p>" +
					"<div></br></br></div>"+
					"<p style=color:green>Druecke die Taste 'J' auf der Tastatur vor dir um fortzufahren.</p>"
					trial.choices=['j'];
				break;
			case 2: //SECOND Screen
				var new_html = '<font size=20>Das Gluecksspiel</font>'+
				'<div></br></div>' +
				'<p>In der folgenden Aufgabe musst du einige Entscheidungen treffen bei denen du Bonuspunkte sammeln kannst.</p>' +
					'<p>Je mehr Bonuspunkte du sammelst, desto hoeher wird dein Bonusgewinn.</p>' +
					'<p>Du kannst in dieser Sitzung bis zu <strong>12 EUR</strong> zusaetzlich verdienen.</p>' +
					'<p>Um moeglichst viel Geld zu verdienen ist es wichtig, dass du gut verstehst was jetzt deine Aufgabe ist.</p>' +
					'<p>Bevor deine erste Aufgabe beginnt, moechten wir dir deshalb auf den folgenden Seiten genau erklaeren was zu tun ist.</p>' +
					'<div></br></br></div>' +
					'<p style=color:orange> Ist noch etwas unklar? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong>, um fortzufahren.</p>'
				break;
			case 3: //New Screen
				var new_html = "<p>In beiden Aufgaben kannst du dich zwischen einem sicheren Gewinn und einem Gluecksspiel entscheiden.</p>" +
					"<p> Bei beiden Optionen kannst du Bonuspunkte gewinnen.</p>" +
					"<p>Die Wahrscheinlichkeit zu gewinnen kannst du an einem Kreisdiagramm erkennen.</p>" +
					"<p>Der <span style=color:blue> blaue </span>Bereich des Kreisdiagramms zeigt dir die <strong>Wahrscheinlichkeit an, einen Bonus zu gewinnen</strong> falls du dich fuer diese Option entscheidest.</p>" +
					"<p>Der <span style=color:red> rote </span>Bereich des Kreisdiagramms zeigt dir die <strong>Wahrscheinlichkeit an, nichts dazu zu gewinnen</strong> falls du dich fuer diese Option entscheidest.</p>" +
					"<p> Bonuspunkte, die du einmal gesammelt hast, koennen dir nicht mehr abgezogen werden.</p>" +
					"<p> Auf der naechsten Seite kannst du ein Beispiel fuer eine Entscheidungssituation sehen.</p>" +
					'<div></br></br></div>' +
					'<p style=color:orange> Ist noch etwas unklar? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
				break;
			case 4: //new Screen
				var new_html =
					"<table style='width:100%'>" +
					"<tr> <th></th> <th>Beispiel</th> <th></th></tr>" +
					"<tr>" +
					"<td><div class ='unselected' id='left' ><img src=" + trial.stimulus1 + " id='jspsych-image-keyboard-response-stimulus1' height='300' width='300'></img></td>" + //the selected Option.
					"<td><div style='font-size:60px;'> <p> + </p> </div></td>" +
					"<td><div class='unselected' id='right' ><img src=" + trial.stimulus2 + " id='jspsych-image-keyboard-response-stimulus2' height='300' width='300'></img></td>" +
					"</tr>" +
					"<tr>" +
					"<td><p class='small'> 5 Bonuspunkte.</p></td>" +
					"<td><p><div class='center' style= 'visibility: hidden;'><img src='img/Anon_Woman.jpg' height='200' width='200'</img></div></td>" +
					"<td><p class='small'> 50 Bonuspunkte. </p></td>" +
					"</tr>" +
					"</table>"+
				'<p style=color:orange> Ist noch etwas unklar? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
				break;
			case 5: //new Screen
				var new_html = "<p>Du kannst dich bei jedem Durchgang fuer eine Option entscheiden, bei welcher du auf jeden Fall einen <strong> kleinen Bonus </strong> bekommst. </p>" +
					"<p> Eine solche Option kannst du zum Beispiel unten sehen. </p>" +
					"<p><img src=" + trial.stimulus1 + " height='300' width='300'></img></p>" +
					"<p>Ein <span style=color:blue> blaues </span>  Kreisgagramm ist eine sichere Wette, wenn du dich dafuer entscheidest.</p>" +
					"<p> Den angezeigten Bonus bekommst du mit einer Wahrscheinlichkeit von <strong>100 %</strong> wenn du dich fuer diese Option entscheidest.</br></p>" +
					'<div><br></br></div>' +
					'<p style=color:orange> Ist noch etwas unklar? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
				break;
			case 6: //new Screen
				var new_html = '<p>Hinter der zweiten Option verbirgt sich ein Gluecksspiel, das oft <strong>einen hoeheren Bonus</strong> verspricht.</p>' +
					'<p>Im Gluecksspiel hat das Kreisdiagramm einen <span style=color:blue> blauen </span>  und einen <span style=color:red> roten </span> Anteil!</p>' +
					'<p>Das heisst, du kannst nicht genau wissen ob du den angezeigten Bonus am Ende des Durchgangs bekommst.</p>' +
					'<p><strong>Erinnere dich:</strong> Der <span style=color:red> rote</span> Anteil des Kreisdiagramms gibt dir die Wahrscheinlichkeit an, garnichts zu gewinnen. </strong> </p>' +
					'<p>Der <span style=color:blue> blaue</span> Anteil des Kreisdiagramms gibt dir die Wahrscheinlichkeit an, den angezeigten Bonus zu gewinnen, wenn du dich fuer das Gluecksspiel entscheidest. </strong> </p>' +
					'<div></br></br></div>' +
					'<p style=color:orange> Ist noch etwas unklar? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
				break;
			case 7: //new Screen
				var new_html = '<p> Ein Beispiel fuer ein Gluecksspiel kannst du nun unten sehen. </p>' +
					"<div> </br> </div>" +
					"<p><img src=" + trial.stimulus2 + " height='300' width='300'></img></p>" +
					'<div></br></div>' +
					'<p>Du kannst bei jedem Durchgang selbst entscheiden, ob du das Gluecksspiel spielen willst oder nicht. </p>' +
					"<p>Falls du spielst kann es sein, dass du <strong> einen hohen Bonus </strong> bekommst, es kann aber auch sein, dass du <strong> gar keinen Bonus </strong> bekommst. </p>" +
					'<div></br></br></div>' +
					'<p style=color:orange> Ist noch etwas unklar? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
				break;
			case 8: //new Screen
				var new_html = '<p> Das Gluecksspiel simuliert ein Gluecksrad, das an einer zufaelligen Stelle stoppt. </p>' +
					'<p> Wenn der Pfeil nach dem Stoppen auf den <span style=color:blue>blauen</span> Teil des Gluecksrades zeigt, bekommst du den Bonus gutgeschrieben. </p>' +
					"<div><font size=20> &dArr; </font></br> </div>" +
					"<p><img src=" + trial.stimulus3 + " height='300' width='300'></img></p>" +
					'<div></br></br></div>' +
					'<p style=color:orange> Ist noch etwas unklar? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
				break;																
			case 9: //new Screen
				var new_html =  "<p>Ueberlege jedes Mal gut wie du dich entscheidest! <strong>Es lohnt sich!</strong> </p>"+
				"<p>Am Ende werden alle von dir erzielten Bonuspunkte in Euro umgerechnet!</p>"+ 
				"<p><strong>Du kannst bis zu 12 Euro zusaetzlich verdienen!</strong></p>"+
				"<p>Du kannst in dieser Aufgabe also echtes Geld gewinnen!</p>"+
				"<p style='border:3px; border-style:solid; border-color:#FF0000; padding: 1em;'>Deine Entscheidung gibst du mit Hilfe der Tasten <strong>'F'</strong> und <strong>'J'</strong> auf der Tastatur ein."+
				"<div></br></br></div>" +
				"<p style=color:orange> Ist noch etwas unklar? Druecke <strong> 'F' </strong> um zum vorherigem Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> 'J' </strong> um fortzufahren.</p>"
				break;
			case 10: //new Screen
				if (jsPsych.data.get().last(1).values()[0].keypress == 70) { //if you come from the next screen
					//nextScreen=9;
					display_element.innerHTML = "<p><strong>Bist du dir sicher?</strong> <p>Ueberlege noch einmal welche Optionen wir dir gezeigt haben.<p>" +
						"<p> Du hast dich fuer das Kreisdiagramm mit <span style=color:red> rotem </span> Anteil entschieden.<p>"+
						"<p> Bei der anderen Option haettest du mit <strong> Sicherheit </strong> die gleiche Anzahl an Bonuspunkten bekommen.<p>"+
						"<p> Schau dir die Optionen noch einmal genau an an!"
					await sleep(15000); // let it sink in for 15 Seconds
				}//end if
				var new_html = "<div> <strong>Wir ueben jetzt eine Entscheidungssituation, damit du ein Gefuehl dafuer bekommst! </strong> <div>" +
					"<div> <strong>Bitte entscheide dich auf der naechsten Seite fuer eine der beiden Optionen</strong> <div>" +
					'<div></br></br></div>' +
					'<p style=color:orange> Ist noch etwas unklar? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
				break;
			case 11: //new Screen				
				var new_html = "<table style='width:100%'>" +
					"<tr> <th></th> <th>Welche Option wuerdest du waehlen?</th> <th></th></tr>" +
					"<tr>" +
					"<td><div class ='unselected' id='left' ><img src=" + trial.stimulus2 + " id='jspsych-image-keyboard-response-stimulus1'height='300' width='300'></img></td>" +
					"<td><div style='font-size:60px;'> <p> + </p> </div></td>" +
					"<td><div class='unselected' id='right' ><img src=" + trial.stimulus1 + " id='jspsych-image-keyboard-response-stimulus2'height='300' width='300'></img></td>" +
					"</tr>" +
					"<tr>" +
					"<td><p class='small'>Druecke <strong> 'F' </strong> fuer 5 Bonuspunkte.</p></td>" +
					"<td><p><div class='center' style= 'visibility: hidden;'><img src='img/Anon_Woman.jpg' height='200' width='200'</img></div></td>" + // the advisor shall always be hidden.
					"<td><p class='small'> Druecke <strong> 'J' </strong> fuer 5 Bonuspunkte. </p></td>" +
					"</tr>" +
					"</table>"
					//showWhat=9;// catches that it goes back to the right screen.
				break;
			case 12:
				new_html = "<p> <strong>Genau! </strong> <p>Ueberlege noch einmal welche Optionen wir dir gezeigt haben.<p>"+
				"<p> Bei einem Kreisdiagramm mit <span style=color:red> rotem </span> Anteil entscheidest du dich fuer das Gluecksspiel. Das heisst du bekommst den Bonus nur mit einer bestimmten Wahrscheinlichkeit.<p>" +
				"<p> Die sichere, <span style=color:blue> komplett blaue </span> Option bringt hier genau so viele Punkte wie das Gluecksspiel. Es ist also sinnvoll die sichere Option zu waehlen.<p>" +
				"<p style=color:orange> Fragen? Druecke <strong> 'F' </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> 'J' </strong> um fortzufahren.</p>"
				break;
			case 13: //new Screen
				var new_html = "<div> <strong>Hast du verstanden was zu tun ist? <div>" +
					"<div> Wenn du alles verstanden hast dann kann es jetzt losgehen.<div>" +
					"<div> <strong> Wenn du Fragen hast, wende dich bitte an den Versuchsleiter oder schau dir die Instruktion noch einmal in Ruhe an.</strong> <div>" +
					'<div></br></br></div>' +
					'<p style=color:orange> Fragen? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
					//showWhat=9;// catches that it goes back to the right screen.
				break;
			case 14:
				var new_html = "Achtung, es geht sofort los!" +
					'<p style="border:3px; border-style:solid; border-color:#FF0000; padding: 1em; color:green;"> Druecke <strong> "J" </strong> das Experiment zu starten</p>'
				trial.choices=['j']
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
