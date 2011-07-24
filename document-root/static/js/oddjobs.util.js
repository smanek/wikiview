// oddjobs.util.js
// a component of onShore Development DOM & Javascript Objects
// Generic utility functions

oddjobs = window.oddjobs || {};	// Don't clobber already-loaded Oddjobs components

oddjobs.util = {

	padString : function (str, paddingChar, len) {
		if (str.toString().length < len) {
			return oddjobs.util.padString (paddingChar + str.toString(), paddingChar, len);
		} else {
			return str;
		}
	}
	
}