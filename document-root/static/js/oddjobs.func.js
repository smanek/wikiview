// oddjobs.func.js
// a component of onShore Development DOM & Javascript Objects
// a re-implementation of various list-processing functions to support a more functional style, as seen in prototype.js

oddjobs = window.oddjobs || {};	// Don't clobber already-loaded Oddjobs components

oddjobs.func = {

	// Not functional at all; returns nothing. Simply passes each value in arr to lambda.

	foreach : function (arr, lambda) {
		for (var i = 0; i < arr.length; i++) {
			lambda (arr[i]);
		}
	},

	// Returns an array of values obtained by passing each value in arr to lambda

	map : function (lambda, arr) {
		var result = [];
		for (var i = 0; i < arr.length; i++) {
			result.push (lambda (arr[i]));
		}
		return result;
	},
	
	// Identical to map except that the resulting array omits values for which lambda returns false, so result.length <= arr.length
	
	filter : function (lambda, arr) {
		var result = [];
		for (var i = 0; i < arr.length; i++) {
			var res = lambda (arr[i]);
			if (res) {
				result.push (res);
			}
		}
		return result;	
	}
};

