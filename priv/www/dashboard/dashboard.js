function getMembers() {
  d3.json("/api/athena/members", function(data) {
    var c = d3.select("#members").selectAll("li").data(data);

    c.enter().append("li")
      .text(function(d) { return d.host + ": " + d.status; });
  });
}
