// oddjobs.dom.js
// a component of onShore Development DOM & Javascript Objects
// various useful functions to get information from the DOM in a cross-browser fashion.

oddjobs = window.oddjobs || {};	// Don't clobber already-loaded Oddjobs components

oddjobs.dom = {

	// Taken wholesale from http://www.quirksmode.org/dom/getstyles.html
	
	getStyle : function (elem, styleProp) {
		var y;
		if (elem.currentStyle) {
			y = elem.currentStyle[styleProp];
		} else if (window.getComputedStyle) {
			y = document.defaultView.getComputedStyle(elem,null).getPropertyValue(styleProp);
		}
		return y;
	},
	
	
	getElementText : function (elem) {
		return elem.nodeValue || elem.firstChild.nodeValue;
	},
	
	// The following three methods add, remove, or test for the presence of a CSS CSSClass on an object.
	// Each takes an element object and the name of a CSS class as a string
	
	addCSSClass : function (elem, CSSClass) {
		if (!(elem.className.match('\\b' + CSSClass + '\\b'))) {
			elem.className += ' ' + CSSClass;
		}
	},

	removeCSSClass : function (elem, CSSClass) {
		elem.className = elem.className.replace(CSSClass,'');
	},

	hasCSSClass : function (elem, CSSClass) {
		if (elem.className.match('\\b' + CSSClass + '\\b')) {
			return true;
		}
	},
	
	// copied from quirksmode.org, for hard-core industrial-strength cross-browser compatibility
	findPosX : function (obj)  {
		var curleft = 0;
		if (obj.offsetParent) {
        while (1) {
          curleft += obj.offsetLeft;
          if (!obj.offsetParent) { break; }
          obj = obj.offsetParent;
        }
		} else if(obj.x) {
        curleft += obj.x;
		}
		return curleft;
	},

	findPosY : function (obj) {
		var curtop = 0;
		if (obj.offsetParent) {
			while (obj.offsetParent) {
				curtop += obj.offsetTop;
				obj = obj.offsetParent;
			}
		} else if (obj.y) {
			curtop += obj.y;
		}
		return curtop;
	},
	
	// coords = { x, y }
	
	containsCoords : function (elem, coords) {	
		if (! (coords.x && coords.y)) { throw new Error ('oddjobs.dom.containsCoords: Coordinates not defined.'); }
			
		var offsetX = oddjobs.dom.findPosX(elem);
		var offsetY = oddjobs.dom.findPosY(elem);
		
		if ( (coords.x >= offsetX && coords.x <= (offsetX + elem.offsetWidth)) &&
			  (coords.y >= offsetY && coords.y <= (offsetY + elem.offsetHeight)) ) {
			return true;
		}
	},
				
	// http://www.webreference.com/programming/javascript/mk

	mouseCoords : function (ev){
		if(ev.pageX || ev.pageY){
			return {x:ev.pageX, y:ev.pageY};
		}
		return {
			x:ev.clientX + document.body.scrollLeft - document.body.clientLeft,
			y:ev.clientY + document.body.scrollTop  - document.body.clientTop
		};
	}
	
};