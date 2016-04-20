var w = 500;
var h = 300;
var padding = 20;

//Create SVG element
			var svg = d3.select("div#Tplot")
						.append("svg")
						.attr("width", w)
						.attr("height", h);

var line1 = svg.append("line").attr("class","line_test")
				.attr("y1", 1000)
				.attr("y2", 100)
				.attr("x1", 300)
				.attr("x2", 300).attr("stroke","red").attr( "stroke-width", "2" );