function started() {
  document.getElementById("submitButton").onclick = querySubmit;

  $(function(){
      setAutoComplete("query-name", "results", "/autocomplete?str=");
  });

}

function querySubmit() {
  document.getElementById("result").innerHTML = "<img src=\"/static/images/loading.gif\" alt=\"loadingImage\"/><br/>Generating Results (may take up to 30 seconds)";
  oddjobs.ajax.call(
    function (req) {
      document.getElementById("result").innerHTML = req.responseText;
    },
    "/generate-dot",
    {method : "GET", queryString : "query-name=" + document.getElementById("query-name").value});
}

function getCompletions() {
    oddjobs.ajax.call(
      function (req) {
	var cmp = req.responseText;
	cmp = eval("(" + cmp + ")");
	document.getElementById("result").innerHTML = cmp;
      },
	"/autocomplete",
	{method : "GET", queryString : "str=" + document.getElementById("query-name").value});
}