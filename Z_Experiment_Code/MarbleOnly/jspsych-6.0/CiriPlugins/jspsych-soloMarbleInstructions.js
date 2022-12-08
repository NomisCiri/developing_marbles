/**
* jspsych-image-keyboard-response
* Josh de Leeuw
*
* plugin for displaying two stimuli next to one another and get visual response of the choice of the participant. 
*
* documentation: docs.jspsych.org
*
**/


jsPsych.plugins["soloMarbleInstructions"] = (function() {

	var plugin = {};

	jsPsych.pluginAPI.registerPreload('soloMarbleInstructions', 'stimulus', 'image');

	plugin.info = {
		name: 'soloMarbleInstructions',
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
			stimulus4: {
				type: jsPsych.plugins.parameterType.IMAGE,
				pretty_name: 'Picture to show in instructions',
				default: 'img/Marble/Slider_Bild.png',
				description: 'The first image to be displayed'
			},
			stimulus5: {
				type: jsPsych.plugins.parameterType.IMAGE,
				pretty_name: 'Picture to show in instructions',
				default: 'img/Marble/Grey_new.png',
				description: 'The first image to be displayed'
			},
			stimulus6: {
				type: jsPsych.plugins.parameterType.IMAGE,
				pretty_name: 'Picture to show in instructions',
				default: 'img/Marble/Red_new.png',
				description: 'The first image to be displayed'
			},			
			stimulus7: {
				type: jsPsych.plugins.parameterType.IMAGE,
				pretty_name: 'Picture to show in instructions',
				default: 'img/Marble/ExampleDraw.png',
				description: 'The first image to be displayed'
			},
			stimulus8: {
				type: jsPsych.plugins.parameterType.IMAGE,
				pretty_name: 'Picture to show in instructions',
				default: 'img/05.png',
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
			var showWhat = jsPsych.data.get().filter({
				trial_type: 'soloMarbleInstructions'
			}).last().values()[0].next_Screen;
		} catch (e) { //if there is an error the script doesnt have any data.
			console.log("FirstTrial Probably.")
			var showWhat = 1;
		}
		var nextScreen = showWhat;
		
		//this here happens so i can swicht some response keys off depending on my subjects choices. 
		var howMany = 600;
		try {
			var howMany = parseInt(jsPsych.data.get().filter({
				trial_type: 'soloMarbleInstructions'
			}).last().values()[0].howMany);
		} catch (e) { //if there is an error the script doesnt have any data.
			console.log("FirstTrial Probably.")
			//trial.choices=['f','j'];
			//howMany=-99;
		}
		

		/**********************THIS I ONLY NEED FOR ****************************************/
		/**********************CHECKING IF THEY MADE A SOMEWHAT CORRECT ESTIMATE*********************/
		if (howMany<80){//here i reset the response keys for when the participants did sth stupid.
			trial.choices=['f'];
			new_html='<p> Du hast eingegeben '+howMany+' % <span style="color:blue"> blaue </span> Murmeln gesehen zu haben'+
			'<p> Du hast aber mehr blaue Murmeln gesehen.'+
			'<p style=color:orange> Druecke <strong> "F" </strong> um dir die Murmeln noch einmal anzuschauen! </p>'
		}else if (howMany>98 && howMany<200){
			trial.choices=['f'];
			new_html='<p> Du hast eingegeben '+howMany+' % <span style="color:blue"> blaue </span> Murmeln gesehen zu haben.'+
			'<p> Du hast aber auch ein paar <span style="color:red"> rote </span> Murmeln gesehen.'+
			'<p style=color:orange> Druecke <strong> "F" </strong> um dir die Murmeln noch einmal anzuschauen! </p>'
			choices=['f'];
		}else if(howMany==-99){
			trial.choices=['f'];
			new_html='<p>Du hast deine Einschaetzung nicht eingeloggt. Klicke bitte auf "Weiter" um deine Einschaetzung einzuloggen.'+
			'<p style=color:orange> Druecke <strong> "F" </strong> um dir die Murmeln noch einmal anzuschauen! </p>'
		}else if(howMany==600){
			trial.choices=['f','j'];//nothing happened.
		}else{
			trial.choices=['f','j'];
			new_html='<p> Du hast eingegeben '+howMany+' % <span style="color:blue"> blaue </span> Murmeln gesehen zu haben.'+
			'<p> Das war gut geschaetzt.'+
			'<p style=color:orange> Ist noch etwas unklar? Druecke <strong> "F" </strong> um zum vorherigem Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
		}
	
		
		var certainty = null;
		try {
			var certainty = jsPsych.data.get().filter({
				trial_type: 'soloMarbleInstructions'
			}).last().values()[0].howSure;
		} catch (e) { //if there is an error the script doesnt have any data.
			console.log("FirstTrial Probably.")
		}
		




		/***THIS FUNCTION NEEDS TO BE DEFINED IN ORDER TO PROVIDE AN INTERACTIVE SLIDER ALREADY IN THE INSTRUCTIONS**/
		show_slider = function() {
			
			var startTime = (new Date()).getTime();
			var klicked = false;
			var color1 = 50; //Number of Blue Marbles
			var color2 = 50; //Number of Red Marbles
			var rowOpen = "    <tr>\n";
			var rowClose = "    </tr>\n";
			var cellOpen = "        <td>\n";
			var cellClose = "        </td>\n";
			var tableOpen = "<table align='center' id='gridhandle'>\n";
			var tableClose = "</table>\n";
			var new_html = "<div> Am Ende eines solchen Durchgangs moechten wir gerne von dir wissen, wie viele blaue Murmeln deiner Meinung nach im Glas waren. </div>" +
			"<div> <strong> Dafuer stelle bitte das Verhaeltnis von blau zu rot mit dem Schieberegler so ein, </div>"+
			"<div> dass er das Verhaeltnis im undurchsitgigem Glas gut beschreibt.</p> </strong> </div>" +
			"<div> Klicke dann bitte auf 'Weiter'. Danach kannst du dich erst fuer eines der Glaeser entscheiden.<br></div>"+
			"<div>Was denkst du? Wie viel % der Murmeln im Glas waren blau?</div>"+
			'<div></br></br></div>';
			//new_html contains all the generated code, then is printed at the end
			new_html += tableOpen;
			//BUILD THE TABLE
			//loops generate each row and fill with cells
			for (i = 1; i <= 10; i++) {
				new_html += rowOpen;
				for (j = 1; j <= 10; j++) {
					//soo how many parts you have to fill with the blue marbles.
					if (color1 !== 0) {
						new_html += cellOpen + "<img src=" + trial.stimulus3 + " width='20' height='20'></img> \n" + cellClose;
						color1 = color1 - 1; //loop until you placed all blue marbles
					} else {
						new_html += cellOpen + "<img src=" + trial.stimulus6 + " width='20' height='20'> \n" + cellClose; //place red marbles.
					}
				}
				new_html += rowClose;
			}
			new_html += tableClose;
			//display_element.innerHTML = new_html;	
			new_html += '<div class="slidecontainer"><input type="range" min="0" max="100" value="50" class="slider" id="Marbleslider"><p ><span align="center" id="demo"></span>%</p>';
			display_element.innerHTML = new_html; //add all the HTML stuff i wrote to the display elemet 
			// add submit button
			new_html += '<button id="jspsych-html-slider-response-next" class="jspsych-btn"> Weiter </button>';
			//display elemet is like screen flip. every html markup command i saved before is now displayed and
			//you can still acess it via an ID and the query Selector
			display_element.innerHTML = new_html;
			//creates a slider object
			var slider = display_element.querySelector('#Marbleslider'); // with query selector you get 
			var output = display_element.querySelector('#demo');
			var grid = display_element.querySelector('#gridhandle');
			output.innerHTML = slider.value;
			grid.innerHTML

			/*************************************EVERYTHING THAT HAPPENS IF THE SLIDER IS MOVED IS DISPLAYED HERE!
			//WHAT DO YOU DO IF THE SILDER IS MOVED?????????*/

			slider.oninput = function() {
				output.innerHTML = this.value;
				color1 = parseInt(this.value); //set one color to the current slider value its returned as string but i need an integer. thats why i convert it with parseint. 
				color2 = 100 - color1; //get the other color
				//draw the same table as above and the replace it.
				var rowOpen = "    <tr>\n";
				var rowClose = "    </tr>\n";
				var cellOpen = "        <td>\n";
				var cellClose = "        </td>\n";
				var tableOpen = "<table id='" + gridhandle + "'>\n";
				var tableClose = "</table>\n";
				var new_html = "";
				//new_html contains all the generated code, then is printed at the end
				new_html += tableOpen;
				//loops generate each row and fill with cells
				for (i = 1; i <= 10; i++) {
					new_html += rowOpen;
					for (j = 1; j <= 10; j++) {
						//ffill marbles.
						if (color1 !== 0) {
							new_html += cellOpen + "<img src=" + trial.stimulus3 + " width='20' height='20'></img> \n" + cellClose;
							color1 = color1 - 1;
						} else {
							new_html += cellOpen + "<img src=" + trial.stimulus6 + " width='20' height='20'> \n" + cellClose;
						}
					}
					new_html += rowClose;
				}
				new_html += tableClose;
				grid.innerHTML = new_html;
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
				if (trial.response_ends_trial) {} else {
					display_element.querySelector('#jspsych-html-slider-response-next').disabled = true;
				}
				/*WORK IN FEEDBACK*/

				new_html='<p> Du hast eingegeben '+response.response+' % blaue Murmeln gesehen zu haben.'+
				'<p style=color:orange> Ist noch etwas unklar? Druecke <strong> "F" </strong> um zum vorherigem Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
				
					
				display_element.innerHTML = new_html;
				howMany = response.response;
			});

		} //end of the responsive function. 



			
			
		howSure=function(){
			var startTime = (new Date()).getTime();
			//display_element.innerHTML = tableContainer;	
			var tableContainer = "<p> Ab und zu moechten wir gerne wissen wie sicher du dir bei dieser Einschaetzung bist."+
			"<p> Dafuer gib bitte in dieser Prozentskala (von 0 bis 100) an wie sicher du bist, dass sich wirklich deine vorher eingegebene Anzahl von <span style='color:blue'>blauen</span> Murmeln im Glas befindet.</p>"+
			"<p> Stelle bitte jetzt den untenstehenden Schieberegler so ein, dass er deine Ueberzeugung gut beschreibt.<p> Klicke dann auf 'Weiter'</p>"+
			"<strong> Wie sicher bist du dir bei deiner Einschaetzung?</p> <p>"+
			"<div></br></br></div>";
			tableContainer += '<div class="slidecontainer"><input type="range" min="0" max="100" value="50" class="slider" id="Marbleslider"><p ><span align="center" id="demo"></span>%</p>';
			//display_element.innerHTML = tableContainer; //add all the HTML stuff i wrote to the display elemet 
			// add submit button
			tableContainer += '<button id="jspsych-html-slider-response-next" class="jspsych-btn"> Weiter </button>';
		
			//display elemet is like screen flip. every html markup command i saved before is now displayed and
			//you can still acess it via an ID and the query Selector
			display_element.innerHTML = tableContainer;	
			//creates a slider object
			var slider = display_element.querySelector('#Marbleslider'); // with query selector you get 
			var output = display_element.querySelector('#demo');
			//var grid = display_element.querySelector('#gridhandle');
			output.innerHTML = slider.value;
		
			/*************************************EVERYTHING THAT HAPPENS IF THE SLIDER IS MOVED IS DISPLAYED HERE!
			//WHAT DO YOU DO IF THE SILDER IS MOVED?????????*/
			slider.oninput = function() {
				output.innerHTML = this.value;
				
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
				}/*WORK IN FEEDBACK*/
				if (response.response>70){
					new_html='<p> Du hast eingegeben '+response.response+' % sicher zu sein'+
					'<p>Insgesamt hast du 5*9 also 45 Murmeln und keinen Platzhalter gesehen. '+
					'<p>Das sind einige Murmeln. Du kannst also davon ausgehen einen guten Eindruck aus dem Glas bekommen zu haben. </p>'+
					'<p style=color:orange> Ist noch etwas unklar? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'

				}else if (response.response<70){
					new_html='<p> Du hast eingegeben '+response.response+' % sicher zu sein.'+
					'<p>Insgesamt hast du 5*9 also 45 Murmeln und keinen Platzhalter gesehen. '+
					'<p>Das sind einige Murmeln. Du kannst also davon ausgehen einen guten Eindruck aus dem Glas bekommen zu haben. </p>'+
					'<p>Du koenntest in diesem Fall auch etwas zuversichtlicher sein. </p>'+
					'<p style=color:orange> Ist noch etwas unklar? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
				}
					
				display_element.innerHTML = new_html;
				return response.response;

			});
		}//end the request 



		/*
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
		*/
		//catch if someone presses "back" at the first screen.
		if (showWhat < 1) {
			showWhat = 1
		};
		//here I Define the Screens
		startExperiment = false;

		//truns on the current screen. 
		switch (showWhat) {
		case 1: //FIRST Screen
			howMany=600;//arbitrary high value.

			var new_html = '<font size=20>Das Murmelglas</font>' +
			'<div></br></div>' +
			'<p>In der folgenden Aufgabe musst du einige Entscheidungen treffen bei denen du Bonuspunkte sammeln kannst.</p>' +
			'<p>Wieder gilt: Je mehr Bonusunkte du sammelst, desto hoeher wird dein Bonusgewinn.</p>' +
			'<p>Du kannst in dieser Sitzung bis zu <strong>8 EUR</strong> zusaetzlich verdienen.</p>' +
			'<p>Jeder Bonuspunkt ist 0.5 Cent wert!<p>'+
			'<p>Um moeglichst viel Geld zu verdienen ist es wichtig, dass du gut verstehst was jetzt deine Aufgabe ist.</p>' +
			'<p>Bevor die Aufgabe beginnt, moechten wir dir deshalb auf den folgenden Seiten genau erklaeren was zu tun ist.</p>' +
			'<div></br></br></div>' +
			'<p style=color:orange> Ist noch etwas unklar? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
			display_element.innerHTML = new_html;
			break;

		case 2: //New Screen
			howMany=600;//arbitrary high value.

			var new_html = "<table style='width:100%'>" +
			"<tr> <th></th> <th>In dieser Aufgabe musst du dich bei jedem Durchgang entscheiden, <br> aus welchem der zwei Glaeser du eine Murmel ziehen moechtest.</p> </th> <th></th></tr>" +
			"<tr>" +
			"<td><div class ='unselected' id='left' ><img src=" + trial.stimulus1 + " id='jspsych-image-keyboard-response-stimulus1'height='300' width='300'></img></td>" +
			"<td><div style='font-size:60px;'> <p> + </p> </div></td>" +
			"<td><div class='unselected' id='right' ><img src=" + trial.stimulus2 + " id='jspsych-image-keyboard-response-stimulus2'height='300' width='300'></img></td>" +
			"</tr>" +
			"<tr>" +
			"<td><p class='small'></p></td>" +
			"<td><p><div class='center' style= 'visibility: hidden;'><img src='img/Anon_Woman.jpg' height='200' width='200'</img></div></td>" + // the advisor shall always be hidden.
			"<td><p class='small'> </p></td>" +
			"</tr>" +
			"</table>" +
			"<div></br></br></div>" +
			'<p style=color:orange> Ist noch etwas unklar? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
			display_element.innerHTML = new_html;
			break;

		case 3: //new Screen
			howMany=600;//arbitrary high value.

			var new_html =
			'<div>Jede der Optionen steht fuer ein Murmelglas aus dem am Ende des Durchgangs <strong> zufaellig </strong> eine Murmel gezogen wird.</div>' +
			'<div><strong>In jedem Glas befinden sich mehr als 1000, also sehr viele Murmeln.</strong> </div>' +
			'<div>Wenn eine <span style=color:blue> blaue </span> Murmel gezogen wird, bekommst du die Bonuspunkte gutgeschrieben, die unter dem Murmelglas angezeigt wurden.</div>' +
			'<div>Wenn eine <span style=color:red> rote </span> Murmel gezogen wird, bekommst du <strong> keinen </strong> Bonus.</div>' +
			'<div></br></br></div>' +
			'<p style=color:orange> Ist noch etwas unklar? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
			display_element.innerHTML = new_html;
			break;

		case 4: //new Screen
			howMany=600;//arbitrary high value.

			var new_html = "<div>Du kannst dich bei jedem Durchgang fuer ein Murmelglas entscheiden, in welchem nur <span style=color:blue> blaue </span> Murmeln sind. </div>" +
			"<div> Ein solches Glas kannst du zum Beispiel unten sehen. </div>" +
			"<div></br></br></div>" +
			"<div><img src=" + trial.stimulus1 + " height='300' width='300'></img></div>" +
			"<div>Ein Glas voller <span style=color:blue> blauer </span>  Murmeln ist eine sichere Wette, wenn du dich dafuer entscheidest.</div>" +
			"<div> Den angezeigten Bonus bekommst du mit einer Wahrscheinlichkeit von <strong>100 %</strong> wenn du dich in dem Durchgang fuer dieses Glas entschieden hast.</br></div>" +
			'<div></br></br></div>' +
			'<p style=color:orange> Ist noch etwas unklar? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
			display_element.innerHTML = new_html;
			break;
		case 5: //new Screen
			howMany=600;//arbitrary high value.

			var new_html = '<div>Hinter der zweiten Option verbirgt sich ein hoeherer Gewinn.</div>' +
			'<div>In diesem Glas koennen sich aber <span style=color:blue> blaue </span>  und <span style=color:red> rote </span> Murmeln befinden!</div>' +
			'<div>Das heisst, wenn du dich fuer dieses Glas entscheidest bekommst du den Bonus nicht mit 100 prozentiger Wahrscheinlichkeit.</div>' +
			'<div></br></div>' +
			'<p style=color:orange> Ist noch etwas unklar? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
			display_element.innerHTML = new_html;
			break;
	
		case 6: //new Screen
			howMany=600;//arbitrary high value.
				
			var new_html = '<p> Ein Beispiel fuer ein Gluecksspiel kannst du nun unten sehen. </p>' +
			"<div> </br> </div>" +
			"<p><img src=" + trial.stimulus8 + " height='300' width='300'></img></p>" +
			'<div></br></div>' +
			'<p>Hier befinden sich zur Haelfte blaue und zur anderen Haelfte rote murmeln im Glas. </p>' +
			'<p>Du kannst bei jedem Durchgang selbst entscheiden, ob du das Gluecksspiel spielen willst oder nicht. </p>' +
			"<p>Falls du spielst kann es sein, dass du <strong> einen hohen Bonus </strong> bekommst, es kann aber auch sein, dass du <strong> gar keinen Bonus </strong> bekommst. </p>" +
			'<div></br></br></div>' +
			'<p style=color:orange> Ist noch etwas unklar? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
			display_element.innerHTML = new_html;
			
			break;
		case 7: //new Screen
			howMany=600;//arbitrary high value.
				
			var new_html = '<p> Bei dem Gluecksspiel wird zufaellig eine Murmel aus dem Glas gezogen. </p>' +
			'<p> Wenn eine <span style=color:red> rote </span>Murmel gezogen wird, bekommst du <strong>keinen Gewin</strong>n</p>' +
			'<p> Wenn eine <span style=color:blue> blaue </span> Murmel gezogen wird, bekommst du die <strong> angezeigten Bonuspunkte gutgeschrieben.</strong> </p>'+
			"<p><img src=" + trial.stimulus8 + " height='300' width='300'></img></p>" +
			'<div></br></br></div>' +
			'<p style=color:orange> Ist noch etwas unklar? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
			display_element.innerHTML = new_html;
			
			break;																
		case 8: //new Screen
			howMany=600;//arbitrary high value.
				
			var new_html =  "<p>Ueberlege jedes Mal gut wie du dich entscheidest! <strong>Es lohnt sich!</strong> </p>"+
			"<p>Am Ende werden alle von dir erzielten Bonuspunkte in Euro umgerechnet!</p>"+ 
			"<p><strong>Du kannst bis zu 12 Euro zusaetzlich verdienen!</strong></p>"+
			"<p>Du kannst in dieser Aufgabe also echtes Geld gewinnen!</p>"+
			"<p style='border:3px; border-style:solid; border-color:#FF0000; padding: 1em;'>Deine Entscheidung gibst du mit Hilfe der Tasten <strong>'F'</strong> und <strong>'J'</strong> auf der Tastatur ein."+
			"<div></br></br></div>" +
			"<p style=color:orange> Ist noch etwas unklar? Druecke <strong> 'F' </strong> um zum vorherigem Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> 'J' </strong> um fortzufahren.</p>"
			display_element.innerHTML = new_html;
			
			break;
		case 9: //new Screen
			howMany=600;//arbitrary high value.
				
			if (jsPsych.data.get().last(1).values()[0].keypress == 70) { //if you come from the next screen
				//nextScreen=9;
				display_element.innerHTML = "<p><strong>Bist du dir sicher?</strong> <p>Ueberlege noch einmal welche Optionen wir dir gezeigt haben.<p>" +
				"<p> Du hast dich fuer das Glas mit <span style=color:red> roten </span> Murmeln darin entschieden.<p>"+
				"<p> Bei der anderen Option haettest du mit <strong> Sicherheit </strong> die gleiche Anzahl an Bonuspunkten bekommen.<p>"+
				"<p> Schau dir die Optionen noch einmal genau an an!"
				await sleep(15000); // let it sink in for 15 Seconds
			}//end if
			var new_html = "<div> <strong>Wir ueben jetzt eine Entscheidungssituation, damit du ein Gefuehl dafuer bekommst! </strong> <div>" +
			"<div> <strong>Bitte entscheide dich auf der naechsten Seite fuer eine der beiden Optionen</strong> <div>" +
			'<div></br></br></div>' +
			'<p style=color:orange> Ist noch etwas unklar? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
			display_element.innerHTML = new_html;
			
			break;
		case 10: //new Screen		
			howMany=600;//arbitrary high value.
						
			var new_html = "<table style='width:100%'>" +
			"<tr> <th></th> <th>Waehle eine Option!</th> <th></th></tr>" +
			"<tr>" +
			"<td><div class ='unselected' id='left' ><img src=" + trial.stimulus8 + " id='jspsych-image-keyboard-response-stimulus1'height='300' width='300'></img></td>" +
			"<td><div style='font-size:60px;'> <p> + </p> </div></td>" +
			"<td><div class='unselected' id='right' ><img src=" + trial.stimulus1 + " id='jspsych-image-keyboard-response-stimulus2'height='300' width='300'></img></td>" +
			"</tr>" +
			"<tr>" +
			"<td><p class='small'>Druecke <strong> 'F' </strong> fuer 5 Bonuspunkte.</p></td>" +
			"<td><p><div class='center' style= 'visibility: hidden;'><img src='img/Anon_Woman.jpg' height='200' width='200'</img></div></td>" + // the advisor shall always be hidden.
			"<td><p class='small'> Druecke <strong> 'J' </strong> fuer 5 Bonuspunkte. </p></td>" +
			"</tr>" +
			"</table>"
			display_element.innerHTML = new_html;
			
			//showWhat=9;// catches that it goes back to the right screen.
			break;
		case 11:
			howMany=600;//arbitrary high value.
				
			new_html = "<p> <strong>Genau! </strong> <p>Ueberlege noch einmal welche Optionen wir dir gezeigt haben.<p>"+
			"<p> Bei einem Glas, das <span style=color:red> rote </span> Murmeln enthaelt, entscheidest du dich fuer das Gluecksspiel. Das heisst du bekommst den Bonus nur mit einer bestimmten Wahrscheinlichkeit.<p>" +
			"<p> Die sichere, <span style=color:blue> komplett blaue </span> Option bringt hier genau so viele Punkte wie das Gluecksspiel. Es ist also sinnvoll die sichere Option zu waehlen.<p>" +
			"<p style=color:orange> Fragen? Druecke <strong> 'F' </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> 'J' </strong> um fortzufahren.</p>"
			display_element.innerHTML = new_html;
			
			break;
		case 12: //new Screen
				
			new_html='<div>In manchen Durchgaengen gibt es noch ein Problem! </div>'+
			'<div>Manchmal ist das zweite Glas ist <strong> undurchsichtig! </strong> </div>' +
			'<div></br></div>' +
			'<div>Das heisst, dass du das Verhaeltnis von <span style=color:blue> blauen </span> und <span style=color:red> roten </span> Murmeln zu Beginn eines Durchganges nicht kennst.</strong> </div>' +
			'<div></br></br></div>' +
			"<p style=color:orange> Fragen? Druecke <strong> 'F' </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> 'J' </strong> um fortzufahren.</p>"
			display_element.innerHTML = new_html;
			break;
		case 13: //new Screen
			howMany=600;//arbitrary high value.

			var new_html = '<div> Das <strong> undurchsichtige </strong> Glas kannst du jetzt unten sehen. </div>' +
			"<div><strong> Vor jedem Durchgang in dem du eine Entscheidung auf Basis eines undurchsichtigen Glases triffst, siehst du kurz dieses Bild. </strong> </div>" +
			"<div><img src=" + trial.stimulus2 + " height='300' width='300'></img></div>" +
			'<div></br></div>' +
			'<div>Ob es sich lohnt, das undurchsichtige Glas zu waehlen, musst du bei jedem Durchgang selbst entscheiden. </div>' +
			"<div>Es kann sein, dass du <strong> einen hohen Bonus </strong> bekommst, es kann aber auch sein, dass du <strong> gar keinen Bonus </strong> bekommst. </div>" +
			'<div>Den Bonus bekommst du nur, wenn eine <span style=color:blue> blaue </span> Murmel gezogen wird.</strong> </div>' +
			'<div></br></br></div>' +
			'<p style=color:orange> Ist noch etwas unklar? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
			display_element.innerHTML = new_html;
			break;

		case 14: //new Screen
			howMany=600;//arbitrary high value.

			var new_html = "<div> Bevor du deine Entscheidung bei einem undurchsichtigem Glas triffst kannst du etwas darueber lernen, wie wahrscheinlich es ist, </div>" +
			"<div>dass eine <span style=color:blue> blaue </span> oder eine <span style=color:red> rote </span> Murmel gezogen wird.</div>" +
			"<div> Das heisst: Du kannst etwas darueber lernen wie wahrscheinlich es ist, dass du den Bonus gewinnst wenn du dich fuer das undurchsichtige Glas entscheidest. </div>" +
			'<div></br></br></div>' +
			'<p style=color:orange> Ist noch etwas unklar? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
			display_element.innerHTML = new_html;

			break;
		case 15: //new Screen
			howMany=600;//arbitrary high value.

			var new_html = "<div> Damit du etwas ueber die Wahrscheinlichkeit lernen kannst, ziehen wir <strong>5</strong> mal hintereinander einige Murmeln aus dem Glas. </div>" +
			"<div>Dann zeigen wir dir alle Murmeln, die wir gezogen haben und du kannst sehen,</div>"+
			"<div>wie viele davon <span style=color:blue> blau </span> und wie viele <span style=color:red> rot </span> waren.</div>" +
			"<div>Dadurch kannst du einschaetzen wie viele  <span style=color:blue> blaue </span>  und wie viele  <span style=color:red> rote </span> Murmeln im Glas sind.</div>" +
			'<div><strong> Wichtig! Du siehst nicht alle Murmeln aus dem Glas. In einem Glas befinden sich in Wirklichkeit mehr als 1000, also sehr viele Murmeln! </strong></div>' +
			'<div></br></br></div>' +
			'<p style=color:orange> Ist noch etwas unklar? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
			display_element.innerHTML = new_html;
				
			break;
		case 16: //new Screen
			howMany=600;//arbitrary high value.

			var new_html = '<div> Eine blaue Murmel sieht so aus: </div>' +
			"<div> </div>" +
			"<div><img src=" + trial.stimulus3 + " height='300' width='300'></img></div>" +
			'<div></br></div>' +
			'<div>Wenn diese Murmel am Ende gezogen wird, bekommst du den Bonus <strong>gutgeschrieben.</strong></div>' +
			'<div></br></br></div>' +
			'<p style=color:orange> Ist noch etwas unklar? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
			display_element.innerHTML = new_html;

			break;
		case 17: //new Screen
			howMany=600;//arbitrary high value.

			var new_html = '<div> Eine rote Murmel sieht so aus: </div>' +
			"<div> </div>" +
			"<div><img src=" + trial.stimulus6 + " height='300' width='300'></img></div>" +
			'<div></br></div>' +
			'<div>Wenn diese Murmel am Ende gezogen wird, bekommst du den Bonus <strong> nicht gutgeschrieben.</strong></div>' +
			'<div></br></br></div>' +
			'<p style=color:orange> Ist noch etwas unklar? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
			display_element.innerHTML = new_html;

			break;
		case 18: //new Screen
			howMany=600;//arbitrary high value.

			var new_html = '<div> Es kann auch vorkommen, dass wir dir einen grauen Kreis zeigen: </div>' +
			"<div> </div>" +
			"<div><img src=" + trial.stimulus5 + " height='300' width='300'></img></div>" +
			'<div></br></div>' +
			'<div>Graue Murmeln befinden sich aber <strong>nicht</strong> im Glas.</div>' +
			'<div></br></div>' +
			'<div>Eine grauer Kreis ist nur ein Platzhalter. Wir ziehen maximal neun Murmeln aus dem Glas. <strong>Manchmal aber auch weniger!</strong></div>' +
			'<div>Wenn wir weniger als neun Murmeln gezogen haben, siehst du den grauen Kreis als Platzhalter. </div>' +
			'<div></br></div>' +
			'<div>Hinter einem grauen Kreis kann sich <strong>entweder</strong> eine <span style=color:blue> blaue </span>  <strong>oder</strong> eine <span style=color:red> rote </span> Murmel verbergen. </strong></div>' +
			'<div></br></br></div>' +
			'<p style=color:orange> Ist noch etwas unklar? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
			display_element.innerHTML = new_html;

			break;
		case 19: //new Screen
			howMany=600;//arbitrary high value.

			var new_html = '<div> Im unteren Bild zeigen wir dir 3 <span style=color:blue>blaue</span> Murmeln, 2 <span style=color:red>rote</span> Murmeln und 4 graue Platzhalter  </div>' +
			'<div></br></br></div>' +
			"<div><img src=" + trial.stimulus7 + "></img></div>" +
			'<div></br></br></div>' +
			'<p style=color:orange> Ist noch etwas unklar? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
			display_element.innerHTML = new_html;

			break;
		case 20: //new Screen
			howMany=600;//arbitrary high value.

			var new_html = "<div>Je mehr <span style=color:blue> blaue </span> Murmeln wir dir zeigen, desto wahrscheinlicher ist es, dass eine <span style=color:blue> blaue </span> Murmel gezogen wird. </div>" +
			"<div>Mit anderen Worten: Je mehr <span style=color:blue> blaue </span> Murmeln wir dir zeigen desto wahrscheinlicher ist es,</div>" +
			"<div> <strong>dass du den angezeigten Bonus bekommst</strong>, wenn du dich fuer das <strong> undurchsichtige </strong> Glas entschieden hast. </div>" +
			"<div> Wir sagen dir nicht nach jedem Durchgang wie viele Bonuspunkte du bereits gesammelt hast.</div>"+
			"<div> Ab und zu bekommst du jedoch Feedback ueber deinen Punktestand.</div>"+
			'<div></br></br></div>' +
			'<p style=color:orange> Ist noch etwas unklar? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
			display_element.innerHTML = new_html;

			break;
		case 21: //new Screen
			howMany=600;//arbitrary high value.
			var new_html = "<div> <strong>Wir ziehen jetzt direkt hintereinander <strong>5 mal 9</strong> Murmeln fuer dich, damit du ein Gefuehl dafuer bekommst! </strong> <div>" +
			"<p>Zwischen den einzelnen Zuegen siehst du kurz ein <strong>+</strong> in der Mitte des Bildschirms.</p> <p> Das dient dazu, dass du die 5 Zuege auseinander halten kannst.</p>"+
			"Das <strong>+</strong> dient hier nur der Uebung und wird dir im eigentlichen Experiment nicht mehr gezeigt."+
			'<div></br></br></div>' +
			'<p style=color:orange> Ist noch etwas unklar? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
			display_element.innerHTML = new_html;

			break;

		case 22: //new Screen
			randomizer=new Array(9);
			randomizer=[1,1,1,1,1,1,1,1,2]			
			for (i = 0; i < 5; i++) {
				randomizer=jsPsych.randomization.repeat(randomizer, 1);// here i shuffle it.
				new_html = '<div class="grid-container">'; //make a container full of blue marbles.
				for (k = 0; k < randomizer.length; k++) { 
					if (randomizer[k]==1){
						new_html += '<div class="grid-item"><img src=' + trial.stimulus3 + ' width="100" height="100"></img></div>'; // here I add marbles to the Jar.
					}else if (randomizer[k]==2){
						new_html += '<div class="grid-item"><img src=' + trial.stimulus6 + ' width="100" height="100"></img></div>'
					}
				}//end placing the marbles at random places.
				display_element.innerHTML = '<div style="font-size:60px;">+</div>';
				await sleep(500);
				display_element.innerHTML = new_html;
				await sleep(1000);
			} //show the blue marbles five times. 
			new_html = '<div style="font-size:60px;">+</div> <div></br></br></div> <p style=color:orange>Moechtest du die Murmeln noch einmal sehen? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>';
			display_element.innerHTML = new_html;

			break;
		case 23: //new Screen
			//trial.choices=['f']//if you come from the following screen. jump even furtehr to the previous screen.
			if (jsPsych.data.get().last(1).values()[0].keypress == 70) { //if you come from the next screen
				//nextScreen=9;
				trial.choices=['j']
				display_element.innerHTML = "<p>Ueberlege noch einmal, welche Murmeln wir dir gezeigt haben.<p> Es wurden <strong>viele, aber nicht nur <span style=color:blue> blaue </span></strong> Murmeln gezogen." +
				"<p> Das heisst die Warscheinlichkeit einen hohen Bonus zu bekommen wenn du das undurchsitige Glas waehlst ist <strong>hoch aber nicht 100% </strong>. <p>Schau es dir bitte noch einmal an.";
				await sleep(15000); // let it sink in for 15 Seconds
				display_element.innerHTML = "<p>Ueberlege noch einmal welche Murmeln wir dir gezeigt haben.<p> Es wurden <strong>viele aber nicht nur <span style=color:blue> blaue </span></strong> Murmeln gezogen." +
				"<p> Das heisst die Warscheinlichkeit einen hohen Bonus zu bekommen wenn du das undurchsichtige Glas waehlst ist <strong>hoch aber nicht 100% </strong>. <p>Schau es dir bitte noch einmal an.<p style=color:green> Druecke <strong> 'J' </strong> um fortzufahren.</p>";
				howMany=600;//arbitrary high value.
				showWhat=21;	
			}else{
				trial.choices=['f','j']
				howMany=-99;
				show_slider();
				console.log(howMany);
			}
			break;
		case 24:
		display_element.innerHTML=new_html;// this has been defined above when we checked for the keys.
		break;
		case 25: //new Screen
		howMany=600;//arbitrary high value.
		if (jsPsych.data.get().last(1).values()[0].keypress == 70) { 
		trial.choices=['j']//if you come from the following screen. jump even furtehr to the previous screen.
		display_element.innerHTML = "<p><strong>Bist du dir Sicher?</strong> <p>Ueberlege noch einmal welche Murmeln wir dir gezeigt haben.<p> Es wurden <strong>viele<spanstyle=color:blue> blaue </span></strong> Murmeln gezogen." +
			"<p> Das heisst die Warscheinlichkeit 1000 Bonuspunkte zu bekommen wenn du das undurchsitige Glas waehlst ist <strong>hoch</strong>.";
				await sleep(15000); // let it sink in for 15 Seconds
			display_element.innerHTML = "<p><strong>Bist du dir Sicher?</strong> <p>Ueberlege noch einmal welche Murmeln wir dir gezeigt haben.<p> Es wurden <strong>viele<spanstyle=color:blue> blaue </span></strong> Murmeln gezogen." +
			"<p> Das heisst die Warscheinlichkeit 1000 Bonuspunkte zu bekommen wenn du das undurchsitige Glas waehlst ist <strong>hoch</strong>. <p>Schau es dir bitte noch einmal an.<p style=color:green> Druecke <strong> 'J' </strong> um fortzufahren.</p>";	
			showWhat=21;	
		}else{
		//trial.choices=jsPsych.NO_KEYS
		howSure();
	}
		break;
		case 26: //new Screen
			howMany=600;//arbitrary high value.

			var new_html = "<table style='width:100%'>" +
			"<tr> <th></th> <th>Waehle eine Option!</th> <th></th></tr>" +
			"<tr>" +
			"<td><div class ='unselected' id='left' ><img src=" + trial.stimulus1 + " id='jspsych-image-keyboard-response-stimulus1'height='300' width='300'></img></td>" +
			"<td><div style='font-size:60px;'> <p> + </p> </div></td>" +
			"<td><div class='unselected' id='right' ><img src=" + trial.stimulus2 + " id='jspsych-image-keyboard-response-stimulus2'height='300' width='300'></img></td>" +
			"</tr>" +
			"<tr>" +
			"<td><p class='small'>Druecke <strong> 'F' </strong> um aus diesem Glas zu ziehen. </p>" +
			"<p>Inhalt: </p> <p><img src=" + trial.stimulus3 + " id='jspsych-image-keyboard-response-stimulus1'height='20' width='20'> fuer <strong>5</strong> Bonuspunkte</p> " 
			+				
			"<p class='small' style= 'visibility: hidden;'> <img src=" + trial.stimulus6 + " height='20' width='20'> fuer 0 Bonuspunkte. </p></td>" +// also hide this. i just want everything to look symmetrical.
			"<td><p><div class='center' style= 'visibility: hidden;'><img src='img/Anon_Woman.jpg' height='200' width='200'</img></div></td>" + // the advisor shall always be hidden.				
			"<td><p class='small'>Druecke <strong> 'J' </strong> um aus diesem Glas zu ziehen. " +
			"<p>Inhalt:</p>" +
			"<p><img src=" + trial.stimulus3 + " height='20' width='20'> fuer <strong>1000</strong> Bonuspunkte oder" +
			"<p class='small'> </strong>  <img src=" + trial.stimulus6 + " height='20' width='20'> fuer 0 Bonuspunkte. </p></td>" +
			"</tr>" +
			"</table>"
			//showWhat=9;// catches that it goes back to the right screen.
			display_element.innerHTML = new_html;

			break;
		case 27:
			howMany=600;//arbitrary high value.
			if (jsPsych.data.get().last(1).values()[0].keypress == 70) { //catch and show the first page of the instruction.
				var new_html = '<font size=20>Das Murmelglas</font>' +
				'<div></br></div>' +
				'<p>In der folgenden Aufgabe musst du einige Entscheidungen treffen bei denen du Bonuspunkte sammeln kannst.</p>' +
				'<p>Wieder gilt: Je mehr Bonusunkte du sammelst, desto hoeher wird dein Bonusgewinn.</p>' +
				'<p>Du kannst in dieser Sitzung bis zu <strong>8 EUR</strong> zusaetzlich verdienen.</p>' +
				'<p>Um moeglichst viel Geld zu verdienen ist es wichtig, dass du gut verstehst was jetzt deine Aufgabe ist.</p>' +
				'<p>Bevor die Aufgabe beginnt, moechten wir dir deshalb auf den folgenden Seiten genau erklaeren was zu tun ist.</p>' +
				'<div></br></br></div>' +
				'<p style=color:orange> Ist noch etwas unklar? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
				display_element.innerHTML = new_html;
				showWhat=2;
			}else{
			new_html = '<p> <strong>Genau!</strong> <p>Ueberlege noch einmal welche Murmeln wir dir gezeigt haben.<p> Es wurden <strong>viele<span style=color:blue> blaue </span></strong> Murmeln gezogen.' +
				'<p>Das heisst die Warscheinlichkeit 1000 Punkte zu bekommen ist sehr hoch. <p style=color:orange> Fragen? Druecke <strong> "F" </strong> um zum vorherigen Bildschirm zu gelangen! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
			}
			display_element.innerHTML = new_html;

			break;
		case 28: //new Screen
			howMany=600;//arbitrary high value.

			var new_html = "<div> <strong>Hast du verstanden was zu tun ist? <div>" +
			"<div> Wenn du alles verstanden hast dann kann es jetzt losgehen.<div>" +
			"<div> <strong> Wenn du Fragen hast wende dich bitte an den Versuchsleiter und schau dir die Instruktion noch einmal in Ruhe an.</strong> <div>" +
			'<div></br></br></div>' +
			'<p style=color:orange> Fragen? Druecke <strong> "F" </strong> um <strong> dir die Instruktion noch einmal anzuchauen </strong>! </p> <p style=color:green> Druecke <strong> "J" </strong> um fortzufahren.</p>'
			display_element.innerHTML = new_html;

			//showWhat=9;// catches that it goes back to the right screen.
			break;
		case 29:
			howMany=600;//arbitrary high value.

			var new_html = "Achtung, es geht sofort mit einer Uebung los!" +
			'<p style="border:3px; border-style:solid; border-color:#FF0000; padding: 1em; color:green;"> Druecke <strong> "J" </strong> das Experiment zu starten</p>'
			display_element.innerHTML = new_html;

			startExperiment = true;
			break;
		} //end of switch


		/*the stimuli are organized a t
		able*/
		/********** THIS IS WHERE THE INSTRUCTIONS AND FEEDBACKS ANND SO ON ARE WORKED OUT***********
			
		********************************************************************/



		// draw for 1000 milliseconds before starting the response listener


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
				"howMany":howMany,
				"howSure":certainty,
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
