function startDashboard() {
    if (window.location.pathname.startsWith("/membership")) {
        getMembers();
    }
}

function getMembers() {
    d3.json("/api/athena/members", function(data) {
        d3.select("#membership").style("visibility", "visible");

        var c = d3.select("#members").selectAll("li").data(data);

        c.enter().append("li")
            .text(function(d) { return d.host + ": " + d.status; });
  });
}
