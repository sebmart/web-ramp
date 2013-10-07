function createTable(element, tableData) {
  var table = document.createElement('table')
    , tableBody = document.createElement('tbody');

  table.className = "table";
  tableData.forEach(function(rowData) {
    var row = document.createElement('tr');

    rowData.forEach(function(cellData) {
      var cell = document.createElement('td');
      cell.contentEditable = true;
      cell.appendChild(document.createTextNode(cellData));
      row.appendChild(cell);
    });

    tableBody.appendChild(row);
  });
  table.appendChild(tableBody);
  document.getElementById(element).appendChild(table);
}

function createArray(element) {
var sln = []
$('#' + element + ' tr').each(function() {
var row = []
$('td', this).each(function() {
row.push(parseFloat($(this).text()));
});
sln.push(row);
});
return sln;
}

var networks;

var network = ""
var scenario

    function getNetwork() {
    jsonCall("network-info", function(data) {
                        scenario = data;
                        loadScenario();
    });
    }

    function dataShip() {
        return {
            network: network,
            nRestarts: getNRestarts(),
            nIters: getNIters(),
            topZoomFactor: getTopZoomFactor(),
            optimizer: getOptimizer(),
            goal: getGoal(),
            demand: getDemand(),
            splits: getSplits(),
            freeway: getFreeway()
        };
    }

    function getFreeway() {
      return JSON.stringify(createArray("freeway"));
    }

    function getDemand() {
    return JSON.stringify(createArray("demand-content"));
    }

    function getSplits() {
    return JSON.stringify(createArray("turnratios"));
    }

    function loadDemand() {
    $(".edit-grid").empty();
    createTable("turnratios", scenario.simParams.bc.splitRatios);
    createTable("demand-content", scenario.simParams.bc.demands);
    }

    function getOptimizer() {
        return $('#optimizer option:selected').attr('value');
    }

    function getGoal() {
        return $('#goal option:selected').attr('value');
    }

    function jsonCall(name, cb) {
        $.getJSON(name, dataShip(), cb);
    }

    function setupListeners() {
        $("#networks").change(function (a, b) {
            network = $('option:selected', a.target).val()
            getNetwork();
        });
        $("button#simulate-btn").click(simulate);
        $("button#optimize-btn").click(optimize);
        $("button#compare-btn").click(compare);
    }

    function drawGraph(divName, density, colorFn) {
        var width = 300;
        var height = 250;
        var stage = new Kinetic.Stage({
            container: divName,
            width: width,
            height: height
        });

        var layer = new Kinetic.Layer();
        var rows = density[0].length;
        var cols = density.length;

        var cellWidth = width / cols;
        var cellHeight = height / rows;

        $.each(density, function (colIndex, col) {
            var x = colIndex / cols * width;
            $.each(col, function (rowIndex, row) {
                var y = (1 - rowIndex / rows) * height;
                layer.add(new Kinetic.Rect({
                    x: x,
                    y: y,
                    width: cellWidth,
                    height: -cellHeight,
                    fill: colorFn(row)
                }));
            });
        });
        stage.add(layer);
    }

    function drawColorGraph(divName, density) {
    drawGraph(divName, density, colorGraphFill);
    }

    function drawDiffGraph(divName, diff) {
    drawGraph(divName, diff, diffGraphFill);
    }

    function getNIters() {
        return parseInt($("#nIterations").val());
    }

    function getNRestarts() {
        return parseInt($("#nRestarts").val());
    }

    function getTopZoomFactor() {
        return parseInt($("#topZoomFactor").val());
    }

    function optimize() {
        jsonCall("optimize", function (sim) {
            simulation = sim;
            loadSimulation();
        });
    }

    function compare() {
        jsonCall("compare", function(diff) {
                $(".sim").empty();
                drawDiffGraph("density", diff.density);
                drawDiffGraph("queue-graph", diff.queue);

        });
    }

    function simulate() {
        jsonCall("simulate", function (sim) {
            simulation = sim;
            loadSimulation();
        });
    }

    function loadSimulation() {
        $(".sim").empty();
        drawColorGraph("density", simulation.colorGraph.density);
        drawColorGraph("queue-graph", simulation.colorGraph.queue);
        $("#ttt").text(simulation.costSummary.ttt.toFixed(2));
        $("#ml").text(simulation.costSummary.tMainline.toFixed(2));
        $("#queue").text(simulation.costSummary.tQueue.toFixed(2));
    }

    function colorGraphFill(value) {
        if (value < 0) return "black";
        return "hsla(" + Math.round((1 - value) * 100) + ",100%,50%,1)";
    }

    function diffGraphFill(value) {
        var hue = 240;
        if (value >= 0) {
            hue = 0;
        }
        var lum = parseInt(50 + (1 - Math.abs(value)) * 50);
        return "hsla(" + hue + ",100%," + lum + "%,1)";
    }

    function populateNetworkNames() {
        jsonCall("networks", function (nets) {
            networks = nets;
            network = networks[0];
            getNetwork();
            $.each(networks, function (i, network) {
                $("#networks").append($("<option>").attr("value", network).text(network));
            });
        });
    }

    function populateOptimizers() {
        jsonCall("optimizers", function (optimizers) {
            $.each(optimizers, function (i, optimizer) {
                $("#optimizer").append($("<option>").attr("value", optimizer).text(optimizer));
            });
        });
    }

    function populateGoals() {
        jsonCall("goals", function (goals) {
            $.each(goals, function (i, goal) {
                $("#goal").append($("<option>").attr("value", goal).text(goal));
            });
        });
    }



    function loadScenario() {
        var div = $("#scenario");
        div.empty();
        var fw = scenario.fw;
        var fwDiv = $("<div>");
        div.append(fwDiv);
        fwDiv.attr("id", "freeway");
        fwDiv.append($("<h2>").text("Freeway"));
        var table = $('<table class="table" id="freeway">');

        fwDiv.append(table);
        var trhead = $("<tr>");
        trhead.append($("<th>").text("Cell"));
        trhead.append($("<th>").text("Length"));
        trhead.append($("<th>").text("FF vel"));
        trhead.append($("<th>").text("p Max"));
        trhead.append($("<th>").text("F max"));
        table.append($("<thead>").append(trhead));
        $.each(fw.links, function (i, link) {
            var tr = $("<tr>");
            tr.append($("<td contentEditable>").css("font-weight", "bold").text(i));
            tr.append($("<td contentEditable>").text(link.length.toFixed(2)));
            tr.append($("<td contentEditable>").text(link.fd.v.toFixed(2)));
            tr.append($("<td contentEditable>").text(link.fd.rhoMax.toFixed(2)));
            tr.append($("<td contentEditable>").text(link.fd.fMax.toFixed(2)));
            table.append(tr);
        });
        loadDemand();
    }

$(function () {

    populateNetworkNames();
    populateOptimizers();
    populateGoals();
    setupListeners();
});