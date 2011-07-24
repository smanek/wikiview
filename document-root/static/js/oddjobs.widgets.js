// oddjobs.widgets.js
// a component of onShore Development DOM & Javascript Objects
// A set of functions which operate on DOM elements, adding
// event handlers and properties and so on, to make control-like objects
// providing additional functionality

oddjobs = window.oddjobs || {};	// Don't clobber already-loaded Oddjobs components

// Nota Bene!
//
// These are destructive functions! Unfortunately, domNode = newDomNode
// doesn't work without using replaceChild or (in IE) replaceNode, which
// I'd rather avoid, so these functions modify the node passed to it.

oddjobs.widgets = {

	// Takes an existing element, which must be an input field, and adds
	// key event handlers to restrict it to numeric values, and optionally
	// to values within a certain range. Also adds handlers to allow up- and
	// down-arrows to increment and decrement, with optional zero-padding
	
	makeNumericTextBox : function ( elem, options ) {
	
		if (!elem.tagName == 'INPUT') { throw new Error ('oddjobs.widgets.makeNumericTextBox: element is not a text field'); }
		
		// Options and defaults
		options = {
			upperLimit : options.upperLimit || Infinity, 		// maximum allowable value
			lowerLimit : options.lowerLimit || 0,					// minimum allowable value
			wrapAround : options.wrapAround || false,				// should arrow keys go to opposite limit when hitting limit?
			padTo      : options.padTo      || null				// 0-padding width
		};		
		
		// Private methods

		// Check whether keycodes are numeric
		
		function isRestrictedKey (code) {
			if ((code < 35 || code > 40) &&
		       (code < 48 || code > 57) && 
				 (code < 96 || code > 105) && code != 13 && code != 9 && code != 8 && code != 46) {
				return true;
			}
		}				
		
		// Event handlers
		
		elem.onkeydown = function (ev) {
			ev = ev || window.event;
			var number = (+elem.value);
			if (isRestrictedKey(ev.keyCode)) {
				return false;
			} else if (ev.keyCode == 40 && !isNaN(number) ) {	// down arrow							
				elem.value = oddjobs.util.padString ( number == options.lowerLimit ? (options.wrapAround ? options.upperLimit : elem.value) 
																										 : number - 1, '0', options.padTo);
				// This prevents the handler from being called twice per keystroke in Safari; I suspect changing the value of the element is triggering the keystroke events
				// (it shouldn't), but am not certain of this.																										 
				return false;																						 
			} else if (ev.keyCode == 38 && !isNaN(number) ) {	// up arrow							
				elem.value = oddjobs.util.padString ( number == options.upperLimit ? (options.wrapAround ? options.lowerLimit : elem.value)
				                                                                   : number + 1, '0', options.padTo);
				return false;
			} 			
		};

		elem.onkeyup = function (ev) {
			var number = (+elem.value);
			if (number < options.lowerLimit || number > options.upperLimit || isNaN(number)) {
				elem.value = (elem.previousValue || oddjobs.util.padString (0, '0', options.padTo));
			}
		};
		
		return elem;
	},
	
	// Takes an existing element and adds appropriate mouse event handlers
	// to make it draggable. Can either drag the original element, or create
	// a 'ghost' element
	
	makeDraggable : function ( elem, options ) {
	
		if (!elem) { throw new Error ('oddjobs.widgets.makeDraggable: Element is not a DOM element'); }
			
		// Options and defaults
		
		options = {
			isGhosted  : options.isGhosted  || false, 			// drag the original, or create a ghost?
			ondrag     : options.ondrag     || function () {},	// triggered when onmousemove occurs while dragging
			ondrop     : options.ondrop     || function () {}	// triggered when dragging is complete
		};				
		
		// Event handlers
		
		elem.onmousedown = function (ev) {						
			ev = ev || window.event;		
			var mouse = oddjobs.dom.mouseCoords (ev),
			    xOffset = oddjobs.dom.findPosX(elem),
			    yOffset = oddjobs.dom.findPosY(elem),
				 elemToDrag;

			if (options.isGhosted) {
				elemToDrag = elem.cloneNode (true);																											
			} else {
				elemToDrag = elem;
			}

			elemToDrag.ondrag = options.ondrag;
			elemToDrag.ondrop = options.ondrop;	
			elemToDrag.style.position = 'absolute';
			elemToDrag.style.left = xOffset + 'px';
			elemToDrag.style.top = yOffset + 'px';								
			elemToDrag.isDragging = true;
			elemToDrag.mouseOffsetX = mouse.x - xOffset;
			elemToDrag.mouseOffsetY = mouse.y - yOffset;																	
			
			document.onmousemove = function (ev) {																						
				if (elemToDrag.isDragging) {
					if (!elemToDrag.parentNode) {	// instantiate
						document.body.appendChild (elemToDrag);
					}
					ev = ev || window.event;
					var mousePos = oddjobs.dom.mouseCoords (ev);
					elemToDrag.style.left = (mousePos.x - elemToDrag.mouseOffsetX) + 'px';
					elemToDrag.style.top = (mousePos.y - elemToDrag.mouseOffsetY) + 'px';
					elemToDrag.ondrag(ev);
					elemToDrag.wasDragged = true;
					return false;
				}
			};
			
			document.onmouseup = function (ev) {
				if (elemToDrag.isDragging) {
					elemToDrag.isDragging = false;
					if (elemToDrag.wasDragged) {
						elemToDrag.wasDragged = false;
						elemToDrag.ondrop(ev);
					}
					
					document.onmousemove = undefined;
					document.onmouseup = undefined;
				}				
			};
			return false;			
		};
	},

	// Takes an existing form and a callback, and converts the form onsubmit handler
	// to make an Ajax request rather than submitting the form manually. Optionally
	// convert a single link to a form submission link. Additional options are 
	// passed to the Ajax call.
		
	makeAjaxForm : function (elem, callBack, options) {

		if (!elem.tagName == 'FORM') { throw new Error ('oddjobs.widgets.makeAjaxForm: Element is not a form element'); }
			
		// Options and defaults
		options.convertToLink = options.convertToLink || false; 			// convert submit button to link for aesthetic purposes?

		// Event handlers
		
		elem.onsubmit = function () {		
			oddjobs.ajax.submit (callBack, elem, options);
			return false;
		};
		
		if (options.convertToLink) {			
			oddjobs.func.foreach (elem.elements,
				function (field) {
					if (field.type == 'submit') {
						var link = document.createElement('a');
						if (field.title) {
							link.title = field.title;													// preserve mouseover text
						}
						// preserve text, stripping leading and ending whitespace, which is not collapsed in IE
						if (field.value) {
							link.appendChild (document.createTextNode (field.value.replace(/^\s*/,'').replace(/\s*$/,'')));
						}
						link.href = "";
						link.onclick = function () { elem.onsubmit(); return false; };	// And make it submit the form
						field.parentNode.replaceChild (link, field);
					}
				}
			);
		}
	}
};
