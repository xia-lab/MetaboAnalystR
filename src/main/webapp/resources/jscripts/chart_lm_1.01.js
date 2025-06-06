/* 
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
var gridSVGCoords = "";
var gridSVGMappings = "";
var myChart;
var textColor;
var gridColor;
var light = $(parent.window.document).find("#formHidden\\:selectedTheme").val();
if (light === "true") {
    textColor = "grey";
    gridColor = "lightgrey";
} else {
    textColor = "white";
    gridColor = 'rgb(200, 200, 200,0.2)';
}
console.log(textColor)


// select elements

$(document).ready(function () { // each refresh needs to reinitiate interactivity
    initChart();
});

function initChart() {
    var res = $.ajax({
        type: "GET",
        url: '/MetaboAnalyst/faces/AjaxCall?function=getCovSvgInfo&ignoreMe=' + new Date().getTime(),
        async: false
    }).responseText;
    res = res.split("||");
    usr_dir = res[0];
    var fileNm = res[1];
    var logp_thresh = res[2];
   
    $.getJSON(usr_dir + fileNm,
            function (dat) {

                var TRNum = 0; // sig
                var TLNum = 0;
                var BLNum = 0; // never sig
                var BRNum = 0;
                var maxX = 0;
                var maxY = 0;

                var data_obj = {BL: [], BR: [], TR: [], TL: []}
                var color_obj = {BL: [], BR: [], TR: [], TL: []}
                var borderColor_obj = {BL: [], BR: [], TR: [], TL: []}
                var labels_arr = {BL: [], BR: [], TR: [], TL: []}
                for (var i = 0; i < dat["Row.names"].length; i++) {
                    var arr = {
                        x: dat["pval.no"][i],
                        y: dat["pval.adj"][i],
                        label: dat["Row.names"][i]
                    }
                   
                    if (arr.x > maxX) {
                        maxX = arr.x;
                    }

                    if (arr.y > maxY) {
                        maxY = arr.y;
                    }

                    if (arr.x > logp_thresh && arr.y > logp_thresh) { //TR blue
                        color_obj["TR"].push("rgba(0, 150, 255, 0.2)");
                        borderColor_obj["TR"].push("rgb(0, 150, 255)");
                        data_obj["TR"].push(arr);
                        TRNum++;
                    } else if (arr.x < logp_thresh && arr.y > logp_thresh) { //TL green
                        color_obj["TL"].push("rgba(34, 139, 34, 0.2)");
                        borderColor_obj["TL"].push("rgb(34, 139, 34)");
                        data_obj["TL"].push(arr);
                        TLNum++;
                    } else if (arr.x > logp_thresh && arr.y < logp_thresh) { //BR red
                        color_obj["BR"].push("rgba(199, 0, 57, 0.2)");
                        borderColor_obj["BR"].push("rgb(199, 0, 57)");
                        data_obj["BR"].push(arr);
                        BRNum++;
                    } else {
                        color_obj["BL"].push("rgba(201, 203, 207, 0.2)");
                        borderColor_obj["BL"].push("rgb(201, 203, 207)");
                        data_obj["BL"].push(arr);
                        BLNum++;
                    }

                    //labels_arr.push(dat["Row.names"][i])
                }


                var step = maxX / 10;
                if (step > 1000) {
                    step = 1000;
                } else if (step > 500) {
                    step = 500;
                } else if (step > 200) {
                    step = 200;
                } else if (step > 100) {
                    step = 100;
                } else if (step > 50) {
                    step = 50;
                } else if (step > 20) {
                    step = 20;
                } else if (step > 10) {
                    step = 10;
                } else if (step > 2) {
                    step = 5;
                } else {
                    step = 1;
                }
                //console.log(step)

                var chartData = {
                    datasets: [
                        {
                            label: "Significant (" + TRNum + ")",
                            data: data_obj["TR"],
                            backgroundColor: color_obj["TR"],
                            borderColor: borderColor_obj["TR"],
                            pointRadius: 4,
                            pointHoverRadius: 5
                        },
                        {
                            label: "Sig. when adjusted (" + TLNum + ")",
                            data: data_obj["TL"],
                            backgroundColor: color_obj["TL"],
                            borderColor: borderColor_obj["TL"]
                        },
                        {
                            label: "Non-sig. (" + BLNum + ")",
                            data: data_obj["BL"],
                            pointRadius: 2,
                            backgroundColor: color_obj["BL"],
                            borderColor: borderColor_obj["BL"]
                        },
                        {
                            label: "Non-sig. when adjusted (" + BRNum + ")",
                            data: data_obj["BR"],
                            backgroundColor: color_obj["BR"],
                            borderColor: borderColor_obj["BR"]
                        }
                    ],
                    labels: labels_arr
                }

                const config = {
                    type: 'scatter',
                    data: chartData,
                    options: {
                        onClick: function (e, els) {
                            var el = els[0];
                            // 0:TR, 1:TL, 2:BL, 3:BR
                            var dataPoint;
                            if (el.datasetIndex === 0) {
                                dataPoint = data_obj["TR"][el.index];
                            } else if (el.datasetIndex === 1) {
                                dataPoint = data_obj["TL"][el.index];
                            } else if (el.datasetIndex === 2) {
                                dataPoint = data_obj["BL"][el.index];
                            } else {
                                dataPoint = data_obj["BR"][el.index];
                            }
                            getMultiFacBoxPlot(dataPoint.label);
                        },
                        scales: {
                            y: {
                                title: {
                                    display: true,
                                    text: '-log10(raw P-value): covariate adjustment',
                                    color: textColor
                                },
                                ticks: {
                                    stepSize: step,
                                    color: textColor
                                },
                                grid: {
                                    color: gridColor
                                }
                            },
                            x: {
                                title: {
                                    display: true,
                                    text: '-log10(raw P-value): no covariate adjustment',
                                    color: textColor
                                },
                                ticks: {
                                    stepSize: step,
                                    color: textColor
                                },
                                grid: {
                                    color: gridColor
                                }
                            }
                        },
                        plugins: {
                            tooltip: {
                                callbacks: {
                                    label: function (context) {
                                        var label = "";
                                        //label = context.raw.label + ": " + context.raw.x + ", " + context.raw.y
                                        label = "Feature: " + context.raw.label
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

                                limits: {
                                    yAxes: {min: 0, max: maxY + maxY / 10},
                                    xAxes: {min: 0, max: maxX + maxX / 10}
                                }
                            }
                        }
                    }
                };

                myChart = new Chart(
                        document.getElementById('myChart'),
                        config
                        );


            });
}

function setupTheme() {

    light = $(parent.window.document).find("#formHidden\\:selectedTheme").val();
    if (light === "true") {
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
