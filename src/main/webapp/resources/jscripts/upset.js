/* 
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
var light;
var props;
var bcgcolor;
var fontcolor;
var width;
var height;
var sets;
var combinations;
var queries;
var selection = null;

function initUpsetPlot() {
    var res = $.ajax({
        type: "GET",
        url: '/MetaboAnalyst/faces/AjaxCall?function=getUpsetInfo&ignoreMe=' + new Date().getTime(),
        async: false
    }).responseText;
    var jobinfo = res.split("||");
    usr_dir = jobinfo[0];
    const reportInit = jobinfo[1];
    jsonNm = jobinfo[2] + ".json";

    $.getJSON(usr_dir + jsonNm, function (res) {
        var mode = $('#modeOpt').val()
        var result = res[0];
        var color_arr = res[1]
        var newArr = [];
        for (var i = 0; i < result.length; i++) {
            if (result[i].sets.length !== 0) {
                var obj = {}
                obj.name = result[i].name;
                obj.sets = result[i].sets;
                if (typeof obj.sets === "string") {
                    obj.sets = [obj.sets];
                }
                newArr.push(obj)
            }
        }

        const elems = newArr;

        ({sets, combinations} = UpSetJS.extractCombinations(elems, {
            "type": mode
        }));



        sets = sets.sort(compare);

        var i = sets.length
        while (i--) {

            sets[i].color = color_arr[i]
        }

        var i = combinations.length
        combinations = combinations.sort(compare2);
        while (i--) {
            var colorArr = []
            combinations[i].sets.forEach((elem) => {
                colorArr.push(elem.color)
            })
            combinations[i].color = UpSetJS.mergeColors(colorArr);
        }
        queries = [];

        if (sets.length === 2) {
            width = 600;
            height = 600;
        } else if (sets.length === 3) {
            width = 800;
            height = 600;
        } else {
            width = 1000;
            height = 750;
        }

        obj = {
            "widthRatios": [
                0.135,
                0.165
            ],
            "fontSizes": {
                "setLabel": "5px",
                "color": "blue"
            }
        }

        render();
    });

    if (reportInit === "false") {
        updateReport();
    }

    $('#reportBn').bind('click keypress', function (event) {
        updateReport();
    });
}

function onHover(set) {
    selection = set;
    render();
}

function onClick(set) {

    $("#displayed-data").remove();

    $("#selected-data").append('<span id="displayed-data" class="displayed-data"></span>');
    if (set === null) {
        queries = [];
        selection = [];
        render();
        return;
    }
    if (set.elems.length > 0) {
        var t = "<ul><lh>Total:" + set.elems.length + "</lh>";
        for (var i = 0; i < set.elems.length; i++) {
            t = t + '<li>' + set.elems[i].name + '</li>';
        }
        t = t + '</ul>';
        $("#displayed-data").append(t);
    }
    queries = [
        set
    ];
    render();

}

function render() {
    light = $(parent.window.document).find("#formHidden\\:selectedTheme").val();

    if (light === "light") {
        bcgcolor = "white";
        fontcolor = "black";
        alternatingBackgroundColor = "#d3d3d3";

    } else {
        bcgcolor = "#040d19";
        fontcolor = "white";
        alternatingBackgroundColor = "#071426";
    }
    document.getElementById("venn-demo").style.width = width + "px";
    document.getElementById("venn-demo").style.height = height + "px";
    document.getElementById("venn-demo").style.background = bcgcolor;

    props = Object.assign({
        width: width,
        height: height,
        axis: {
            color: fontcolor // Axis color
        },
        axisLabels: {
            color: fontcolor // Axis label color
        },
    }, {
        sets: sets,
        combinations: combinations,
        selection: selection,
        queries: queries,
        onHover: onHover,
        onClick: onClick
    }, {
        "widthRatios": [
            0.21,
            0.14
        ],
        "fontSizes": {
            "setLabel": "13px",
            "chartLabel": "13px",
            "color": fontcolor
        },
        "axis": {
            "color": fontcolor // Axis color
        },
        "axisLabels": {
            "color": fontcolor // Axis label color
        },
        "exportButtons": false
    });
    if (sets.length > 3) {
        props["alternatingBackgroundColor"] = alternatingBackgroundColor;
    }

    UpSetJS.render(document.getElementById("venn-demo"), props);

    document.querySelectorAll('[class^="axisTextStyle-upset-"]').forEach(el => {
        el.style.fill = fontcolor;
    });
    document.querySelectorAll('[class^="legendTextStyle-upset-"]').forEach(el => {
        el.style.fill = fontcolor;
    });
    document.querySelectorAll('[class^="barTextStyle-upset-"]').forEach(el => {
        el.style.fill = fontcolor;
    });
    document.querySelectorAll('[class^="cBarTextStyle-upset-"]').forEach(el => {
        el.style.fill = fontcolor;
    });
    document.querySelectorAll('[class^="descTextStyle-upset-"]').forEach(el => {
        el.style.fill = fontcolor;
    });
    document.querySelectorAll('[class^="setaxis-upset-"]').forEach(el => {
        el.style.fill = fontcolor;
    });
    document.querySelectorAll('[class^="cChartTextStyle-upset-"]').forEach(el => {
        el.style.fill = fontcolor;
    });
    document.querySelectorAll('[class^="sChartTextStyle-upset-"]').forEach(el => {
        el.style.fill = fontcolor;
    });
    document.querySelectorAll('[class^="sBarTextStyle-upset-"]').forEach(el => {
        el.style.fill = fontcolor;
    });
    document.querySelectorAll('[class^="setTextStyle-upset-"]').forEach(el => {
        el.style.fill = fontcolor;
    });
}

function updateReport() {
    parent.PF("statusDialog").show();
    setTimeout(function () {
        // Get the SVG element
        const svgElem = document.getElementsByTagName("svg")[0];
        if (!svgElem) {
            console.error("No SVG element found in the document.");
            parent.PF("statusDialog").hide();
            parent.PF('growlWidget').show([{severity: "error", summary: "ERROR", detail: "SVG element not found."}]);
            return;
        }

        // Serialize SVG to string
        const serializer = new window.XMLSerializer();
        const svgXmlString = serializer.serializeToString(svgElem);

        // Create Blob from SVG string
        const svgBlob = new Blob([svgXmlString], {type: "image/svg+xml;charset=utf-8"});
        const url = URL.createObjectURL(svgBlob);

        // Create image element to load the SVG
        const img = new Image();
        img.onload = function () {
            // Get the parent container and its background color
            const parentContainer = document.getElementById("venn-demo"); // Replace with the actual container ID
            const computedStyle = window.getComputedStyle(parentContainer);
            const bgColor = computedStyle.backgroundColor || "white"; // Default to white if no background color is set

            // Create canvas and set its dimensions
            const canvas = document.createElement("canvas");
            canvas.width = img.width;
            canvas.height = img.height;

            const ctx = canvas.getContext("2d");

            // Fill the canvas with the background color
            ctx.fillStyle = bgColor;
            ctx.fillRect(0, 0, canvas.width, canvas.height);

            // Draw the SVG image on top of the background
            ctx.drawImage(img, 0, 0);

            // Convert the canvas content to a PNG data URL
            const pngDataUrl = canvas.toDataURL("image/png");

            // Clean up the Blob URL
            URL.revokeObjectURL(url);

            // Handle navigation and server communication
            handleWorkflow(pngDataUrl);
        };

        img.onerror = function () {
            console.error("Failed to load SVG as an image.");
            parent.PF("statusDialog").hide();
            parent.PF('growlWidget').show([{severity: "error", summary: "ERROR", detail: "Failed to render SVG as image."}]);
        };

        img.src = url;
    }, 1000);
}

// Function to handle workflow logic
function handleWorkflow(pngDataUrl) {
    const pagesToVisit = JSON.parse(localStorage.getItem('pagesToVisit') || "[]");

    if (pagesToVisit.length > 0) {
        if (parent.PF("workflowProgressDialog")) {
            parent.PF("workflowProgressDialog").show();
        }

        pagesToVisit.shift();
        localStorage.setItem('pagesToVisit', JSON.stringify(pagesToVisit));

        setTimeout(function () {
            sendImageToServer(pngDataUrl, "upset", "png", function () {
                if (pagesToVisit.length > 0) {
                    window.parent.location.href = pagesToVisit[0];
                } else {
                    localStorage.setItem('reportEndBool', true);
                    window.parent.location.href = "/MetaboAnalyst/Secure/xialabpro/WorkflowView.xhtml";
                }
            });
        }, 3000);
    } else {
        sendImageToServer(pngDataUrl, "upset", "png");
        parent.PF("statusDialog").hide();
        parent.PF('growlWidget').show([{severity: "info", summary: "INFO", detail: "Report successfully updated!"}]);
    }
}

function exportImage(type) {
    var svg = document.getElementsByTagName("svg")[0]; //can only have one svg in the doc
    UpSetJS.exportSVG(svg, {
        title: "UpSet_plot",
        type: type
    })
}

function mergeColors(colors) {
    if (colors.length === 0) {
        return undefined;
    }
    if (colors.length === 1) {
        return colors[0];
    }
    const cc = colors.reduce(
            (acc, d) => {
        const c = lab(d || 'transparent');
        return {
            l: acc.l + c.l,
            a: acc.a + c.a,
            b: acc.b + c.b,
        };
    },
            {l: 0, a: 0, b: 0}
    );
    return lab(cc.l / colors.length, cc.a / colors.length, cc.b / colors.length).toString();
    // return null;
}

function compare(a, b) {
    if (a["name"] < b["name"]) {
        return -1;
    }
    if (a["name"] > b["name"]) {
        return 1;
    }
    return 0;
}

function compare2(a, b) {
    if (a["cardinality"] < b["cardinality"]) {
        return 1;
    }
    if (a["cardinality"] > b["cardinality"]) {
        return -1;
    }
    return 0;
}

function setupTheme() {
    render()

}

 