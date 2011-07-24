// oddjobs.ajax.js
// a component of onShore Development DOM & Javascript Objects
// Simple AJAX components, largely inspired by "Mini AJAX" http://www.bigbold.com/snippets/posts/show/2025

oddjobs = window.oddjobs || {};	// Don't clobber already-loaded Oddjobs components

oddjobs.ajax = {

	// Make an AJAX request. The response object is passed to the callBack, which is only invoked when a loaded response with a 200 status code
	// is returned.

	call : function (callBack, uri, options) {		
		// Options and defaults
		options = {
			method : options.method || "POST",				// POST or GET
			queryString : options.queryString || '',		// may be specified separately from URI for convenience, and must be for POST requests
			// What to do on error (either network or failed errorCondition)
			onError : options.onError || function (res) { alert ("There was a problem retrieving the XML data:\n" + res.statusText); },
			// If evaluates to true, onError is called and callBack is not called
			errorCondition : options.errorCondition || function () { return false; }
		};
			
		// somewhat stolen from Apple's developer documentation
		var req = false;
		
		// branch for native XMLHttpRequest object
		if(window.XMLHttpRequest) {
			try {
				req = new XMLHttpRequest();
			} catch(e) {
				req = false;
			}

		// branch for IE/Windows ActiveX version
		} else if(window.ActiveXObject) {
			try {
				req = new ActiveXObject("Msxml2.XMLHTTP");
			} catch(e) {
				try {
					req = new ActiveXObject("Microsoft.XMLHTTP");
				} catch(e) {
					req = false;
				}
			}
		}
		if (req) {
			if (options.method == "GET") {
				uri = uri + ((options.queryString) ? ('?' + options.queryString) : '');
				options.queryString = '';
			}
			req.onreadystatechange = function () {
				// only if req shows "loaded"
				if (req.readyState == 4) {
					if (req.status) {
						if (req.status == 200 && !options.errorCondition (req)) {
							// We want to save all options, etc. on the response objects so that we can use the reload() function to call it again			
							// We can't directly set req.oddjobs.params, because in IE that is an ActiveX native code object and it doesn't work.
							// So, this nasty hack:
							var ersatzReq = { // Methods
													getAllResponseHeaders : function () { return req.getAllResponseHeaders(); },
													getResponseHeader     : function (header) { return req.getResponseHeader(header); },
													readyState            : req.readyState,
													responseText          : req.responseText,
													responseXML           : req.responseXML,
													status                : req.status,
													statusText            : req.statusText,
													oddjobs               : { params : { callBack: callBack, uri : uri, options : options } }
												};											
							callBack (ersatzReq);
						} else {					
							options.onError (req);
						}
					}
				}
			};			
			req.open(options.method, uri, true);
			if (options.method == "POST") {
				req.setRequestHeader ('Content-type','application/x-www-form-urlencoded');
			}			
			req.send(options.queryString);
		}
	},
	
	// A special case of oddjobs.ajax.call -- make an XMLHttpRequest and update an element's innerHTML in place with the contents of the responseText
	
	update : function (elem, uri, options) {		
		oddjobs.ajax.call (function (req) {
			elem.innerHTML = req.responseText;
		}, uri, options);
	},
	
	// Construct a query string from a form.
	// THIS IS VERY INCOMPLETE -- I am only adding types of form elements as I need them.
	
	serialize : function (frm) {
		var querystring = oddjobs.func.filter (function (x) {			
			if (x.value && (x.type == 'text' || x.type == 'textarea' || x.type == 'hidden' || x.type == 'password' || x.type == 'select-one')) {				
				return x.name + '=' + encodeURIComponent (x.value);
			} else if (x.value && x.type == 'radio' && x.checked) {
				return x.name + '=' + encodeURIComponent (x.value);
			}
		}, frm.elements);
		if (querystring) {
			return querystring.join('&');
		} else {
			return '';
		}
	},
	
	// A special case of oddjobs.ajax.call, which takes a form rather than a query string and generates the query string from the form elements
	
	submit : function (callBack, form, options) {
		options.queryString = oddjobs.ajax.serialize (form);
		options.method = form.method.toUpperCase();
		oddjobs.ajax.call (callBack, form.action, options);		
	},
	
	// Make a new XMLHttpRequest identical to that for which we are receiving the results object
	
	reload : function (res) {
		oddjobs.ajax.call (res.oddjobs.params.callBack, res.oddjobs.params.uri, res.oddjobs.params.options);
	}
};
