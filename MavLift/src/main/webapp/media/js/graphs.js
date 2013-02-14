d3.json("obp/v1.0/metrics/demo-bar", function(data) {
    var content = d3.select("#content");
    var bar_chart = content.append("div").attr("class", "bar-chart");

    // Include headings ffor the Graph
    content.insert("h2", ".bar-chart").text("Bar Graph");
    content.insert("h3", ".bar-chart").text("Most used API calls");

    // Bind only the nested data to the dataset
    var datasets = data.stats;
    var data_length = datasets.length;

    //Width and height
    var w = 1000;
    var h = 500;
    var barPadding = 2;
    var margin = 20;

    //Create SVG element
    var svg = content
                .append("svg")
                .attr("width", w)
                .attr("height", h)
                .attr("class", "svg-bar-chart");

    var xScale = d3.scale.linear().domain([0, d3.max(data.stats).amount]).range([0, w]);

    svg.selectAll("rect")
            .data(datasets)
        .enter().append("rect")
            .attr("y", function(d, i) {
                return i * (h / data_length);
            })
            .attr("x", function(d) {
                return margin;
            })
            .attr("height", h / data_length - barPadding)
            .attr("width", function(d) {
                return xScale(d.amount);
            })
            .attr("fill", function(d) {
                return "rgb(243, 249, " + (d.amount * 10 + 200) + ")";
            });

        svg.selectAll("text")
                .data(datasets)
           .enter().append("text")
               .text(function(d) {
                    return d.amount;
               })
               .attr("y", function(d, i) {
                    return i * (h / data_length) + (h / data_length - barPadding) / 1.5;
               })
               .attr("x", function(d) {
                    return margin * 2;
               });
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
  .attr("class", "line-chart")
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

    var content = d3.select("#content");
    content.insert("h2", ".line-chart").text("Line Graph");
    content.insert("h3", ".line-chart").text("Daily API Request");
    content.insert("p", ".line-chart").attr("class", "axis").text("Request");
});
