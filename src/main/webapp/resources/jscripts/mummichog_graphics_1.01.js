var fcDt = {};
var colors_gbr20_arr = ["#00E700", "#00CE00", "#00B400", "#009B00", "#008100", "#006700", "#004E00", "#003400", "#001A00", "#000100", "#010000", "#1A0000", "#340000", "#4E0000", "#670000", "#810000", "#9B0000", "#B40000", "#CE0000", "#E70000"];
var initVar = true;
var defaultEdgeSize = 0.4;
var backgroundColor = "white";
var cx1;
var raw_data;
var callback_fun
var myChart;
var textColor;
var gridColor;
var light = $(parent.window.document).find("#formHidden\\:selectedTheme").val();

if (light === "light") {
    textColor = "grey";
    gridColor = "lightgrey";
} else {
    textColor = "white";
    gridColor = 'rgb(200, 200, 200,0.2)';
}
console.log(textColor)


function showMummichog() {
    $.getJSON('/MetaboAnalyst' + document.getElementById("mydir").value + '/scattermum.json',
            function (data) {
                raw_data = data
                showScatter2DMum(data, function () {
                    // This code will run after the showScatter2DMum function has completed
                    console.log("Chart has been created, now doing something else.");
                    console.log($(parent.window.document).find("#sidebar-form\\:m_report"));
                    if ($(parent.window.document).find("#sidebar-form\\:m_report")) {
                        setTimeout(function () {
                            initReportFunctions("mummichog");
                        }, 2000);
                    }
                });
            });
}

function showGSEA() {
    $.getJSON('/MetaboAnalyst' + document.getElementById("mydir").value + '/scattergsea.json',
            function (data) {
                //document.getElementById("spinner").style.display = "block";
                showScatter2DGSEA(data, function () {
                    // This code will run after the showScatter2DMum function has completed
                    console.log("Chart has been created, now doing something else.");
                    if ($(parent.window.document).find("#sidebar-form\\:m_report")) {
                        setTimeout(function () {
                            initReportFunctions("gsea");
                        }, 2000);

                    }
                });
            });
}

function showInteg() {
    $.getJSON('/MetaboAnalyst' + document.getElementById("mydir").value + '/scatterinteg.json',
            function (data) {
                //document.getElementById("spinner").style.display = "block";
                showScatter2DInteg(data, function () {
                    // This code will run after the showScatter2DMum function has completed
                    console.log("Chart has been created, now doing something else.");
                    if ($(parent.window.document).find("#sidebar-form\\:m_report")) {
                        setTimeout(function () {
                            initReportFunctions("integ");
                        }, 2000);
                    }
                });
            });
}

function rescale2Range(inputY, yMax, yMin, xMax, xMin) {
    var percent = (inputY - yMin) / (yMax - yMin);
    var outputX = percent * (xMax - xMin) + xMin;
    return outputX;
}


function exportToPng() {

    var link = document.createElement('a');
    link.download = 'scatter_plot.png';
    link.href = myChart.toBase64Image();
    link.click();
}

function closest(arr, closestTo) {
    var closest = Math.max.apply(null, arr); //Get the highest number in arr in case it match nothing.
    for (var i = 0; i < arr.length; i++) { //Loop the array
        if (arr[i] >= closestTo && arr[i] < closest)
            closest = arr[i]; //Check if it's higher than your number, but lower than your closest value
    }
    return closest; // return the value
}

function resetScatterZoom() {
    myChart.resetZoom();
}

/* 
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
var gridSVGCoords = "";
var gridSVGMappings = "";
function showScatter2DMum(dat, callback) {
    var colArr = [];
    callback_fun = callback;
    var arrlength = dat['enr'].length;
    var rainbow = generateColorGradient(["#ff0000", "#FFEF00", "#ffffff"], arrlength, 0, arrlength);
    for (var i = 0; i < arrlength; i++) {
        colArr.push('#' + rainbow.colourAt(i));
    }

    var sizeArr = generateSizeGradient(dat["pval"], 5, 20);
    var hoverSizeArr = generateSizeGradient(dat["pval"], 6, 24);

    var dataset = [];
    for (var i = 0; i < dat["enr"].length; i++) {
        var arr = {
            x: dat["enr"][i],
            y: dat["pval"][i],
            label: dat["pathnames"][i],
            backgroundColor: colArr[i]
        };

        dataset.push(arr);
    }

    var chartData = {
        datasets: [
            {
                label: "Enriched Pathways",
                data: dataset,
                backgroundColor: colArr,
                pointRadius: sizeArr,
                pointHoverRadius: hoverSizeArr
            }
        ]
    };

    // Calculate min and max for x and y axis with one extra tick padding
    const xMin = Math.min(...dat["enr"]);
    const xMax = Math.max(...dat["enr"]);
    const yMin = Math.min(...dat["pval"]);
    const yMax = Math.max(...dat["pval"]);

    const xTickStep = (xMax - xMin) * 0.1; // 10% padding step
    const yTickStep = (yMax - yMin) * 0.1; // 10% padding step

    const config = {
        type: 'scatter',
        data: chartData,
        options: {
            layout: {
                padding: 20
            },
            onClick: function (e, els) {
                var el = els[0];
                if (el.element !== undefined) {
                    myRemote([{name: 'pathname', value: el.element.$context.raw.label}]);
                }
            },

            scales: {
                y: {
                    title: {
                        display: true,
                        text: '-log10(P-value)',
                        color: textColor
                    },
                    ticks: {
                        color: textColor,
                        callback: function (value, index, values) {
                            // Hide the first and last ticks
                            if (index === 0 || index === values.length - 1) {
                                return null;
                            }
                            return value;  // Show other tick labels
                        }
                    },
                    grid: {
                        color: gridColor
                    },
                    type: 'linear',
                    grace: '5%',
                    suggestedMin: yMin - yTickStep,  // Add one extra tick below the min value
                    suggestedMax: yMax + yTickStep   // Add one extra tick above the max value
                },
                x: {
                    title: {
                        display: true,
                        text: 'Enrichment Factor',
                        color: textColor
                    },
                    ticks: {
                        color: textColor,
                        callback: function (value, index, values) {
                            // Hide the first and last ticks
                            if (index === 0 || index === values.length - 1) {
                                return null;
                            }
                            return value;  // Show other tick labels
                        }
                    },
                    grid: {
                        color: gridColor
                    },
                    suggestedMin: xMin - xTickStep,  // Add one extra tick below the min value
                    suggestedMax: xMax + xTickStep   // Add one extra tick above the max value
                }
            },

            plugins: {
                legend: {
                    display: false
                },
                tooltip: {
                    callbacks: {
                        label: function (context) {
                            var label = [context.raw.label + ": ",
                                         "Enrichment Factor:" + context.raw.x,
                                         "-Log10 P-value:" + context.raw.y];
                            return label;
                        }
                    }
                },
                zoom: {
                    zoom: {
                        wheel: {
                            enabled: false
                        },
                        pinch: {
                            enabled: false
                        },
                        mode: 'xy',
                        drag: {
                            enabled: true,
                            borderColor: 'rgb(54, 162, 235)',
                            borderWidth: 1,
                            backgroundColor: 'rgba(54, 162, 235, 0.3)'
                        }
                    }
                }
            }
        }
    };

    myChart = new Chart(
        document.getElementById('canvas1'),
        config
    );

    if (typeof callback === "function") {
        callback();
    }
}

function showScatter2DGSEA(dat, callback) {

    var up_count = 0;
    var down_count = 0;

    for (var i = 0; i < dat['enr'].length; i = i + 1) {
        if (dat['enr'][j] < 0) {
            down_count = down_count + 1;
        } else {
            up_count = up_count + 1;
        }
    }

    var arrlength = dat['enr'].length;
    var min = Math.min(...dat["enr"]);
    var max = Math.max(...dat["enr"]);
    var rainbow1 = generateColorGradient(["#458B00", "#fffee0"], down_count, min, 0);
    var rainbow2 = generateColorGradient(["#7f0000", "#fffee0"], up_count, 0, max);
    var hex_arr = [];

    var j = 0;
    for (var i = min; i <= max; i = i + (max - min) / arrlength) {
        if (dat['enr'][j] < 0) {
            hex_arr.push('#' + rainbow1.colourAt(i));
        } else {
            hex_arr.push('#' + rainbow2.colourAt(i));
        }
        j = j + 1;
    }

    var sizeArr = generateSizeGradient(dat["pval"], 5, 20);

    var data_obj = {Down: [], Up: []};
    var col_obj = {Down: [], Up: []};
    var size_obj = {Down: [], Up: []};
    var hover_size_obj = {Down: [], Up: []};

    for (var i = 0; i < dat["enr"].length; i++) {
        var arr = {
            x: dat["enr"][i],
            y: dat["pval"][i],
            size: sizeArr[i],
            label: dat["pathnames"][i],
            backgroundColor: hex_arr[i]
        };

        if (arr.x < 0) {
            data_obj["Down"].push(arr);
            col_obj["Down"].push(arr.backgroundColor);
            size_obj["Down"].push(arr.size);
            hover_size_obj["Down"].push(arr.size * 1.1);

        } else {
            data_obj["Up"].push(arr);
            col_obj["Up"].push(arr.backgroundColor);
            size_obj["Up"].push(arr.size);

            hover_size_obj["Up"].push(arr.size * 1.1);

        }
    }

    //console.log(step)

    var chartData = {
        datasets: [
            {
                label: "Downregulated",
                data: data_obj["Down"],
                backgroundColor: col_obj["Down"],
                pointRadius: size_obj["Down"],
                pointHoverRadius: size_obj["Down"]
            },
            {
                label: "Upregulated",
                data: data_obj["Up"],
                backgroundColor: col_obj["Up"],
                pointRadius: size_obj["Up"],
                pointHoverRadius: size_obj["Up"]
            },
        ]
    }

    const config = {
        type: 'scatter',
        data: chartData,
        options: {
            layout: {
                padding: 20
            },
            onClick: function (e, els) {
                var el = els[0];
                // 0:TR, 1:TL, 2:BL, 3:BR
                if (el.element !== undefined) {
                    myRemote([{name: 'pathname', value: el.element.$context.raw.label}]);
                }
            },

            scales: {
                 y: {
                    title: {
                        display: true,
                        text: '-log10(P-value)',
                        color: textColor
                    },
                    ticks: {
                        color: textColor
                    },
                    grid: {
                        color: gridColor
                    } 
                },
                x: {
                    title: {
                        display: true,
                        text: 'NES',
                        color: textColor
                    },
                    ticks: {
                        color: textColor
                    },
                    grid: {
                        color: gridColor
                    }
                }
            },
            plugins: {
                legend: {
                    display: false
                },
                tooltip: {
                    callbacks: {
                        label: function (context) {
                            var label = "";
                            //label = context.raw.label + ": " + context.raw.x + ", " + context.raw.y
                            label = [context.raw.label + ": ",
                                "NES:" + context.raw.x,
                                "-Log10 P-value:" + context.raw.y];

                            return label;
                        }
                    }
                },
                zoom: {
                    zoom: {
                        wheel: {
                            enabled: false
                        },
                        pinch: {
                            enabled: false
                        },
                        mode: 'xy',
                        drag: {
                            enabled: true,
                            borderColor: 'rgb(54, 162, 235)',
                            borderWidth: 1,
                            backgroundColor: 'rgba(54, 162, 235, 0.3)'
                        }
                    },

                }
            }
        }
    };
 
    myChart = new Chart(
            document.getElementById('canvas1'),
            config
            );
 

    if (typeof callback === "function") {
        callback();
    }
}
function showScatter2DInteg(dat, callback) {
    var colArr = [];

    var arrlength = dat['pval'].length;
    var rainbow = generateColorGradient(["#ff0000", "#FFEF00", "#ffffff"], arrlength, 0, arrlength);
    for (var i = 0; i < arrlength; i++) {
        colArr.push('#' + rainbow.colourAt(i));
    }

    var sizeArr = generateSizeGradient(dat["metap"], 5, 20);
    var hoverSizeArr = generateSizeGradient(dat["metap"], 6, 24);

    var dataset = [];
    for (var i = 0; i < dat["enr"].length; i++) {
        var arr = {
            x: dat["enr"][i],
            y: dat["pval"][i],
            label: dat["pathnames"][i],
            backgroundColor: colArr[i]
        };

        dataset.push(arr);
    }

    var chartData = {
        datasets: [
            {
                label: "Enriched Pathways",
                data: dataset,
                backgroundColor: colArr,
                pointRadius: sizeArr,
                pointHoverRadius: hoverSizeArr
            }
        ]
    };

    // Calculate min and max for x and y axes with one extra tick padding
    const xMin = Math.min(...dat["enr"]);
    const xMax = Math.max(...dat["enr"]);
    const yMin = Math.min(...dat["pval"]);
    const yMax = Math.max(...dat["pval"]);

    const xTickStep = (xMax - xMin) * 0.1;  // 10% padding step for x-axis
    const yTickStep = (yMax - yMin) * 0.1;  // 10% padding step for y-axis

    const config = {
        type: 'scatter',
        data: chartData,
        options: {
            layout: {
                padding: 20
            },
            onClick: function (e, els) {
                var el = els[0];
                if (el.element !== undefined) {
                    myRemote([{ name: 'pathname', value: el.element.$context.raw.label }]);
                }
            },

            scales: {
                y: {
                    title: {
                        display: true,
                        text: '-log10 Mummichog',
                        color: textColor
                    },
                    type: 'linear',
                    min: yMin - yTickStep,  // Extend the minimum range with extra space
                    max: yMax + yTickStep,  // Extend the maximum range with extra space
                    ticks: {
                        color: textColor,
                        callback: function (value, index, values) {
                            // Hide the first and last ticks
                            if (index === 0 || index === values.length - 1) {
                                return null;
                            }
                            return value;  // Show other tick labels
                        }
                    },
                    grid: {
                        color: gridColor
                    }
                },
                x: {
                    title: {
                        display: true,
                        text: '-log10(p) GSEA',
                        color: textColor
                    },
                    type: 'linear',
                    min: xMin - xTickStep,  // Extend the minimum range with extra space
                    max: xMax + xTickStep,  // Extend the maximum range with extra space
                    ticks: {
                        color: textColor,
                        callback: function (value, index, values) {
                            // Hide the first and last ticks
                            if (index === 0 || index === values.length - 1) {
                                return null;
                            }
                            return value;  // Show other tick labels
                        }
                    },
                    grid: {
                        color: gridColor
                    }
                }
            },
            plugins: {
                legend: {
                    display: false
                },
                tooltip: {
                    callbacks: {
                        label: function (context) {
                            var label = [context.raw.label + ": ",
                                "-log10(p) GSEA (scaled):" + context.raw.x,
                                "-log10(p) Mummichog (scaled):" + context.raw.y];
                            return label;
                        }
                    }
                },
                zoom: {
                    zoom: {
                        wheel: {
                            enabled: false
                        },
                        pinch: {
                            enabled: false
                        },
                        mode: 'xy',
                        drag: {
                            enabled: true,
                            borderColor: 'rgb(54, 162, 235)',
                            borderWidth: 1,
                            backgroundColor: 'rgba(54, 162, 235, 0.3)'
                        }
                    }
                }
            }
        }
    };

    myChart = new Chart(
        document.getElementById('canvas1'),
        config
    );

    if (typeof callback === "function") {
        callback();
    }
}




function generateColorGradient(colArr, arrlength, rangeMin, rangeMax) {
    var rainbow = new Rainbow();
    rainbow.setSpectrumByArray(colArr); //["#ff0000", "#FFEF00", "#ffffff"]
    var hex_arr = [];
    rainbow.setNumberRange(rangeMin, rangeMax);
    return rainbow;
}

function generateSizeGradient(origArr, newMin, newMax) {
    var sizeRescaled = [];
    for (var i = 0; i < origArr.length; i++) {
        var val = rescale2Range(origArr[i], Math.max(...origArr), Math.min(...origArr), newMax, newMin);
        sizeRescaled.push(val);
    }
    return sizeRescaled;

}

function setupTheme() {
    var light = $(parent.window.document).find("#formHidden\\:selectedTheme").val();

    if (light === "light") {
        textColor = "grey";
        gridColor = "lightgrey";
    } else {
        textColor = "white";
        gridColor = 'rgb(200, 200, 200,0.2)';
    }

    myChart.config.options.scales.y.ticks.color = textColor;
    myChart.config.options.scales.y.title.color = textColor;
    myChart.config.options.scales.y.grid.color = gridColor;
    myChart.config.options.scales.x.ticks.color = textColor;
    myChart.config.options.scales.x.title.color = textColor;
    myChart.config.options.scales.x.grid.color = gridColor;

    myChart.update();

}