d3.json("obp/v1.0/metrics/demo-bar", function(data) {
    var bar_chart = d3.select("#content").append("div").attr("class", "chart");

    bar_chart.selectAll("div")
            .data(data.stats)
        .enter().append("div")
            .style("width", function(d) { return d.amount * 200 + "px"; })
            .html(function(d) { return "<span class='count'>" + d.amount + "</span>" + d.url;});
    $(".chart").before("<h2>Bar Graph</h2><h3>Most used API calls<h3>");
  });


d3.json("obp/v1.0/metrics/demo-line",function(data) {

  // helper function
  function getDate(d) {
      return new Date(d.date);
  }

  // get max and min dates - this assumes data is sorted
  var minDate = getDate(data.stats[0]),
      maxDate = getDate(data.stats[data.stats.length-1]);

  var w = 450,
  h = 450,
  p = 30,
  margin = 20,
  y = d3.scale.linear().domain([20, 0]).range([0 + margin, h - margin]),
  x = d3.time.scale().domain([minDate, maxDate]).range([0 + margin, w - margin]);

  var vis = d3.select("#content")
  .data([data.stats])
  .append("svg:svg")
  .attr("width", w + p * 2)
  .attr("height", h + p * 2)
  .append("svg:g")
  .attr("transform", "translate(" + p + "," + p + ")");

  var rules = vis.selectAll("g.rule")
  .data(x.ticks(3))
  .enter().append("svg:g")
  .attr("class", "rule");

  rules.append("svg:line")
  .attr("x1", x)
  .attr("x2", x)
  .attr("y1", 0)
  .attr("y2", h - 1);

  rules.append("svg:line")
  .attr("class", function(d) { return d ? null : "axis"; })
  .attr("y1", y)
  .attr("y2", y)
  .attr("x1", 0)
  .attr("x2", w + 1);

  rules.append("svg:text")
  .attr("x", x)
  .attr("y", h + 3)
  .attr("dy", ".71em")
  .attr("text-anchor", "middle")
  .text(x.tickFormat(3));

  rules.append("svg:text")
  .attr("y", y)
  .attr("x", -3)
  .attr("dy", ".35em")
  .attr("text-anchor", "end")
  .text(y.tickFormat(3));

  vis.append("svg:path")
  .attr("class", "line")
  .attr("d", d3.svg.line()
      .x(function(d) { return x(getDate(d)); })
      .y(function(d) { return y(d.amount); })
  );

  vis.selectAll("circle.line")
  .data(data.stats)
  .enter().append("svg:circle")
  .attr("class", "line")
  .attr("cx", function(d) { return x(getDate(d)); })
  .attr("cy", function(d) { return y(d.amount); })
  .attr("r", 3.5);

  var yAxisLeft = d3.svg.axis().scale(y).ticks(5).orient("left");
              // Add the y-axis to the left
              vis.append("svg:g")
                    .attr("class", "y axis")
                    .attr("transform", "translate(0,0)")
                    .call(yAxisLeft);
  $("svg").before("<h2>Line Graph</h2><h3>Daily API Request</h3><p class='axis'>Request</p>");
  })
