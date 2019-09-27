// Copyright 2019, VMware.
//
// Expected to be defined by earlier scripts:
//
//  filenames: array of strings; the names of the files

// These are the stats we expect to chart. The Gauges field of the
// data points in the loaded files should have four gauges for each of
// these: "..._avg_us", "..._p50_us", "..._min_us", and "..._max_us".
var times = ["parse", "time_update", "time_response", "execute", "serialize",
             "evmrun", "evmcreate", "evmwrite"];
var client_times = ["bft_time"];

// set the dimensions and margins of the graph
var margin = {top: 10, right: 45, bottom: 30, left: 60},
    width = 460 - margin.left - margin.right,
    height = 400 - margin.top - margin.bottom;

// append the svg object to the body of the page
var viz = d3.select("#my_dataviz")

// Prepare the table
var svg = {};
for (var i in times) {
    var row = viz.append("tr");
    row.append("td")
        .text(times[i]);
    svg[times[i]] = {};
    for (var j in filenames) {
        svg[times[i]][filenames[j]] = row.append("td").append("svg")
            .attr("width", width + margin.left + margin.right)
            .attr("height", height + margin.top + margin.bottom)
            .append("g")
            .attr("transform",
                  "translate(" + margin.left + "," + margin.top + ")");
    }
}

// This is where all the data will end up. It would be purer to pass
// this as a parameter to the analyze/render/etc. functions, but it's
// super nice to have this global available in the JS console for
// interactive debugging.
var parsed = {};
var parsed_clients = {};

// Load all data before rendering anything, so that we can scale
// graphs equally in rows, and align them all on time.
var q = d3.queue();

for (var i in filenames) {
    q.defer(d3.json, filenames[i]);
}

q.await(function(error, ...all_data) {
    if (error) {
        console.error("Error: "+error);
    } else {
        // All data is available - parse it, analyze it, render it.
        parse_data(all_data);
        render_data(analyze_data());
    }
});

// Does two things:
//  1. Parses the Time field into a JS Date object (and moves it to the date field)
//  2. Puts the data in the global parsed map.
function parse_data(all_data) {
    for (var i in all_data) {
        viz.select("#header").append("th").text(filenames[i]);
        parsed[filenames[i]] = all_data[i]
            .filter(function(l) { return l.Name == "concord_commands_handler" })
            .map(function(l) {
                return { date : d3.timeParse("%Y-%m-%dT%H:%M:%S.%L")(l.Time),
                         Gauges: l.Gauges }
            });
        parsed_clients[filenames[i]] = all_data[i]
            .reduce(
                function(acc, l) {
                    if (l.Name.startsWith("client_")) {
                        if (!(l.Name in acc)) {
                            acc[l.Name] = [];
                        }
                        acc[l.Name].push({ date : d3.timeParse("%Y-%m-%dT%H:%M:%S.%L")(l.Time),
                                           Gauges: l.Gauges });
                    }
                    return acc;
                },
                {});
    }
}

// Find ranges of the data:
//  1. X axis is time, so find min/max times across all samples
//  2. Scale Y for each sample type, so that the graph for that type
//  has the same scale on every host.
// Returns a map:
//  x: scale function for x values
//  y[name]: scale function for y values of named time
function analyze_data() {
    var dates = [];

    var maxes = {};
    for (var i in times) {
        // the zero is here to ensure our graphs always start at zero
        maxes[times[i]] = {left: [0], right: [0]};
    }
    for (var i in client_times) {
        maxes[client_times[i]] = {left: [0], right: [0]};
    }

    for (var i in filenames) {
        // assume first and last date are the earliest and latest
        dates.push(parsed[filenames[i]][0].date);
        dates.push(parsed[filenames[i]][parsed[filenames[i]].length-1].date);

        for (var j in parsed_clients[filenames[i]]) {
            dates.push(parsed_clients[filenames[i]][j][0].date);
            dates.push(parsed_clients[filenames[i]][j][parsed_clients[filenames[i]][j].length-1].date);
        }

        // This map is not needed - we could just reference the last
        // element of the maxes array, but it makes the code a little
        // cleaner.
        var graph_maxes = {};
        for (var j in times) {
            graph_maxes[times[j]] = {left: 0, right: 0};
        }
        for (var j in client_times) {
            graph_maxes[client_times[j]] = {left: 0, right: 0};
        }

        graph_maxes = parsed[filenames[i]].reduce(
            function(gm, d) {
                for (var j in times) {
                    gm[times[j]].left = Math.max(d.Gauges[times[j]+"_max_us"],
                                                 gm[times[j]].left);
                    gm[times[j]].right = Math.max(d.Gauges[times[j]+"_count"],
                                                  gm[times[j]].right);
                }
                return gm;
            },
            graph_maxes);

        for (var j in parsed_clients[filenames[i]]) {
            graph_maxes = parsed_clients[filenames[i]][j].reduce(
                function(gm, d) {
                    for (var k in client_times) {
                        gm[client_times[k]].left = Math.max(d.Gauges[client_times[k]+"_max_us"],
                                                            gm[client_times[k]].left);
                        gm[client_times[k]].right = Math.max(d.Gauges[client_times[k]+"_count"],
                                                             gm[client_times[k]].right);
                    }
                    return gm;
                },
                graph_maxes);
        }

        // We could just keep the one max across all hosts, but this
        // allows us to decide below whether we actually want to use
        // the max, or something between the maxes.
        for (var j in times) {
            maxes[times[j]].left.push(graph_maxes[times[j]].left);
            maxes[times[j]].right.push(graph_maxes[times[j]].right);
        }
        for (var j in client_times) {
            maxes[client_times[j]].left.push(graph_maxes[client_times[j]].left);
            maxes[client_times[j]].right.push(graph_maxes[client_times[j]].right);
        }
    }
    var x = d3.scaleTime()
        .domain(d3.extent(dates))
        .range([ 0, width ]);

    var y = {};
    for (var i in times) {
        y[times[i]] = { left: d3.scaleLinear()
                        // Clip large maxes on one host, instead of shrinking graphs
                        // to unusability on other hosts.
                        .domain([0, d3.mean(maxes[times[i]].left)])
                        .range([ height, 0 ]),
                        right: d3.scaleLinear()
                        // The right hand scale is counts, which
                        // should be the same on all hosts, so no need
                        // to worry about one blowing the others out.
                        .domain([0, d3.max(maxes[times[i]].right)])
                        .range([ height, 0 ])
                      }
    }
    for (var i in client_times) {
        y[client_times[i]] = { left: d3.scaleLinear()
                        // Clip large maxes on one host, instead of shrinking graphs
                        // to unusability on other hosts.
                        .domain([0, d3.mean(maxes[client_times[i]].left)])
                        .range([ height, 0 ]),
                        right: d3.scaleLinear()
                        // The right hand scale is counts, which
                        // should be the same on all hosts, so no need
                        // to worry about one blowing the others out.
                        .domain([0, d3.max(maxes[client_times[i]].right)])
                        .range([ height, 0 ])
                      }
    }

    return {x, y};
}

// Top-level render function. Expects return value of analyze_data as argument.
function render_data(scales) {
    for (var i in filenames) {
        render_host(parsed[filenames[i]], filenames[i], scales);
        render_host_clients(parsed_clients[filenames[i]], filenames[i], scales);
    }
}

// Render one host's replica graphs
function render_host(host_data, host, scales) {
    for (var i in times) {
        var t = times[i];
        var x = scales.x;
        var y = scales.y[times[i]];

        render_graph(host_data, svg[t][host], t, x, y);
    }
}

// Render one hosts's client graphs. This dynamically adds rows for
// client stats, based on how many clients we actually see.
function render_host_clients(host_data, host, scales) {
    var client_index = 0;
    for (var i in host_data) {
        for (var j in client_times) {
            var svg_row = j+client_index;
            if (!((svg_row) in svg)) {
                var row = viz.append("tr");
                row.append("td")
                    .text("client["+client_index+"] "+client_times[j]);
                svg[svg_row] = {};
                for (var k in filenames) {
                    svg[svg_row][filenames[k]] = row.append("td").append("svg")
                        .attr("width", width + margin.left + margin.right)
                        .attr("height", height + margin.top + margin.bottom)
                        .append("g")
                        .attr("transform",
                              "translate(" + margin.left + "," + margin.top + ")");
                }
            }

            var t = client_times[j]
            var x = scales.x;
            var y = scales.y[t];

            render_graph(host_data[i], svg[svg_row][host], t, x, y);
        }
        client_index++;
    }
}

// Render one graph.
function render_graph(host_data, svg, t, x, y) {
    // Add X axis
    svg.append("g")
        .attr("transform", "translate(0," + height + ")")
        .call(d3.axisBottom(x));

    // Add X axis label
    svg.append("text")
        .attr("transform",
              "translate(" + (width/2) + " ," +
              (height + margin.top + 20) + ")")
        .style("text-anchor", "middle")
        .text("UTC Time");

    // Add left Y axis
    svg.append("g")
        .call(d3.axisLeft(y.left));

    // Add left Y axis label
    svg.append("text")
        .attr("transform", "rotate(-90)")
        .attr("y", 0 - margin.left * 0.95)
        .attr("x", 0 - (height / 2))
        .attr("dy", "1em")
        .style("text-anchor", "middle")
        .text(t + " time (microseconds)");

    // Add right Y axis
    svg.append("g")
        .attr("transform", "translate(" + width + ",0)")
        .call(d3.axisRight(y.right));

    // Add right Y axis label
    svg.append("text")
        .attr("transform", "rotate(90)")
        .attr("y", 0 - (width + margin.right))
        .attr("x", (height / 2))
        .attr("dy", "1em")
        .style("text-anchor", "middle")
        .text(t + " count");

    // Add min/max bounds
    svg.append("path")
        .datum(host_data)
        .attr("fill", "#bbddee")
        .attr("stroke", "none")
        .attr("d", d3.area()
              .x(function(d) { return x(d.date) })
              .y0(function(d) { return y.left(d.Gauges[t+"_min_us"]) })
              .y1(function(d) { return y.left(d.Gauges[t+"_max_us"]) })
             )

    // Add average
    svg.append("path")
        .datum(host_data)
        .attr("fill", "none")
        .attr("stroke", "#006699")
        .attr("stroke-width", 1.5)
        .attr("d", d3.line()
              .x(function(d) { return x(d.date) })
              .y(function(d) { return y.left(d.Gauges[t+"_avg_us"]) })
             )

    // Add median
    svg.append("path")
        .datum(host_data)
        .attr("fill", "none")
        .attr("stroke", "#999999")
        .attr("stroke-width", 1.5)
        .attr("d", d3.line()
              .x(function(d) { return x(d.date) })
              .y(function(d) { return y.left(d.Gauges[t+"_p50_us"]) })
             )

    // Add count
    svg.append("path")
        .datum(host_data)
        .attr("fill", "none")
        .attr("stroke", "#999900")
        .attr("stroke-width", 1.5)
        .attr("d", d3.line()
              .x(function(d) { return x(d.date) })
              .y(function(d) { return y.right(d.Gauges[t+"_count"]) })
             )
}
