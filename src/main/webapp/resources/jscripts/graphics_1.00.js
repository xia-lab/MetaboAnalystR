var cx2;
var cx1;
var allshapes = ["sphere", "square", "triangle", "rhombus", "octagon", "oval", "plus", "minus", "pacman", "pacman2", "mdavid", "rect2", "pentagon", "rect3", "arc", "rectangle", "image"]
function showPCA3D() {
    $.getJSON('/MetaboAnalyst' + document.getElementById("mydir").value,
            function (raw_data) {
                var selectedColor = PF('colorOptWidget').getSelectedValue();
                var selectedShape = PF('shapeOptWidget').getSelectedValue();
                //console.log(raw_data)
                score_obj = raw_data['score'];
                names = score_obj['name'];
                coords = score_obj['xyz'];
                smps = score_obj['axis'];

                meta_names = Object.keys(score_obj['metadata'])
                var legend_order = {}
                var facA = score_obj['metadata'][selectedColor]; //color
                var facB = score_obj['metadata'][selectedShape]; //shape
                if (score_obj['metadata_type'][selectedColor] !== "cont") {
                    for (var i = 0; i < facA.length; i += 1) {
                        if (isNumeric(facA[i])) {
                            facA[i] = 'Group ' + facA[i];
                        }
                    }
                    legend_order['Color Legend'] = score_obj['metadata_list'][selectedColor];
                }

                if (score_obj['metadata_type'][meta_names[selectedShape]] !== "cont") {
                    for (var i = 0; i < facB.length; i += 1) {
                        if (isNumeric(facB[i])) {
                            facB[i] = 'Group ' + facB[i];
                        }
                    }
                    legend_order['Shape Legend'] = score_obj['metadata_list'][selectedShape];
                }
                cols = score_obj['colors'];
                // saps = score_obj['shapes'];
                //saps = allshapes.slice(0,facA.length)
                cx1 = new CanvasXpress(
                        'canvas1',
                        {
                            'z': {
                                'Color Legend': facA,
                                'Shape Legend': facB
                            },
                            'y': {
                                'vars': names,
                                'smps': smps,
                                'data': coords
                            }
                        },
                        {
                            'graphType': 'Scatter3D',
                            'xAxis': ['PC1'],
                            'yAxis': ['PC2'],
                            'zAxis': ['PC3'],
                            'colorSCheme': 'user',
                            'legendOrder': legend_order,
                            'shapeBy': 'Shape Legend',
                            'shapes': allshapes,
                            'colorBy': 'Color Legend',
                            'sizes': [10],
                            'axisTitleScaleFontFactor': 0.8,
                            'axisTickScaleFontFactor': 0.7,
                            'xAxisMinorTicks': false,
                            'xAxisTickColor': "#FFFFFF",
                            'yAxisTickColor': "#FFFFFF",
                            'zAxisTickColor': "#FFFFFF",
                            'canvasBox': false,
                            'disableToolbar': 'true',
                            'disableMenu': 'true',
                            'gradient': 'true',
                            'imageDir': '../../resources/images/'
                        }, {
                    "dblclick": function (o, e, t) {

                    }
                }
                );

                var loadings_obj = raw_data['loadings'];
                var ld_names = loadings_obj['name'];
                var ld_coords = loadings_obj['xyz'];
                var ld_smps = loadings_obj['axis'];
                var ld_cols = loadings_obj['cols'];

                cx2 = new CanvasXpress(
                        'canvas2',
                        {
                            'y': {
                                'vars': ld_names,
                                'smps': ld_smps,
                                'data': ld_coords
                            }
                        },
                        {
                            'graphType': 'Scatter3D',
                            'xAxis': ['Loadings 1'],
                            'yAxis': ['Loadings 2'],
                            'canvasBox': false,
                            'zAxis': ['Loadings 3'],
                            'sizes': [10],
                            'axisTitleScaleFontFactor': 0.8,
                            'axisTickScaleFontFactor': 0.7,
                            'xAxisMinorTicks': false,
                            'xAxisTickColor': "#FFFFFF",
                            'yAxisTickColor': "#FFFFFF",
                            'zAxisTickColor': "#FFFFFF",
                            'colorBy': 'dist',
                            'colorSCheme': 'user',
                            'colors': ld_cols,
                            'disableToolbar': 'true',
                            'disableMenu': 'true',
                            'gradient': 'true',
                            'imageDir': '../../resources/images/'
                        }, {
                    "click": function (o, e, t) {

                    },
                    "dblclick": function (o, e, t) {
                        getTimeBoxPlot(o.y.vars[0])
                    },
                    "mousemove": function (o, e, t) {

                        t.showInfoSpan(e, "<pre><table>" +
                                "<tr>" + o.y.vars[0] + "</tr><br />" +
                                "<tr><b>" + o.y.smps[0] + "</b>: " + o.y.data[0][0] + "</tr> <br />" +
                                "<tr><b>" + o.y.smps[1] + "</b>: " + o.y.data[0][1] + "</tr> <br />" +
                                "<tr><b>" + o.y.smps[2] + "</b>: " + o.y.data[0][2] + "</tr> <br />" +
                                "</table></pre>");
                    },
                    "mouseout": function (o, e, t) {
                    }
                }
                );
                updatePCAColorScheme();
            });

}

function updatePCAColorScheme() {
    //var arrElements = document.getElementById("colRadio");
    var selectedColor = PF('colorOptWidget').getSelectedValue();
    var selectedShape = PF('shapeOptWidget').getSelectedValue();

    var facA = score_obj['metadata'][selectedColor]
    var facB = score_obj['metadata'][selectedShape]
    cx1.legendOrder = {};
    if (score_obj['metadata_type'][selectedColor] === "disc") {
        for (var i = 0; i < facA.length; i += 1) {
            if (isNumeric(facA[i])) {
                facA[i] = 'Group ' + facA[i];
            }
        }
        for (var i = 0; i < score_obj['metadata_list'][selectedColor].length; i++) {
            if (isNumeric(score_obj['metadata_list'][selectedColor][i])) {
                score_obj['metadata_list'][selectedColor][i] = 'Group ' + score_obj['metadata_list'][selectedColor][i]
            }
        }
        cx1.legendOrder["Color Legend"] = score_obj['metadata_list'][selectedColor];
    }

    if (score_obj['metadata_type'][selectedShape] === "disc") {
        for (var i = 0; i < facB.length; i += 1) {
            if (isNumeric(facB[i])) {
                facB[i] = 'Group ' + facB[i];
            }
        }
        for (var i = 0; i < score_obj['metadata_list'][selectedShape].length; i++) {
            if (isNumeric(score_obj['metadata_list'][selectedShape][i])) {
                score_obj['metadata_list'][selectedShape][i] = 'Group ' + score_obj['metadata_list'][selectedShape][i]
            }
        }

        cx1.legendOrder["Shape Legend"] = score_obj['metadata_list'][selectedShape];
    }

    cx1.data = {
        'z': {
            'Color Legend': facA,
            'Shape Legend': facB
        },
        'y': {
            'vars': names,
            'smps': smps,
            'data': coords
        }
    };
    cx1.updateMetaData();
    cx1.masterReset();
    cx2.masterReset();
}


function showPCA3DScore() {
    $.getJSON('/MetaboAnalyst' + document.getElementById("mydir").value,
            function (raw_data) {

                var score_obj = raw_data['score'];
                var names = score_obj['name'];
                var coords = score_obj['xyz'];
                var smps = score_obj['axis'];
                var cols = score_obj['colors'];
                var facA = score_obj['facA'];

                cx1 = new CanvasXpress(
                        'canvas1',
                        {
                            'z': {
                                'Legend': facA
                            },
                            'y': {
                                'vars': names,
                                'smps': smps,
                                'data': coords
                            }
                        },
                        {
                            'graphType': 'Scatter3D',
                            'xAxis': ['PC1'],
                            'yAxis': ['PC2'],
                            'zAxis': ['PC3'],
                            'colorSCheme': 'user',
                            'canvasBox': false,
                            'colorBy': 'Legend',
                            'colors': cols,
                            'sizes': [12],
                            'axisTitleScaleFontFactor': 0.8,
                            'axisTickScaleFontFactor': 0.7,
                            'xAxisMinorTicks': false,
                            'xAxisTickColor': "#FFFFFF",
                            'yAxisTickColor': "#FFFFFF",
                            'zAxisTickColor': "#FFFFFF",
                            'yAxisMinorTicks': false,
                            'zAxisMinorTicks': false,
                            'axisType': 'categorical',
                            'disableToolbar': 'true',
                            'disableMenu': 'true',
                            'disableConfigurator': 'true',
                            'gradient': 'true',
                            'imageDir': '../../resources/images/'
                        }, {
                    "dblclick": function (o, e, t) {
                        current_spec_name = o.y.vars[0];
                        PF('statusDialog').show();
                        setTimeout(function () {
                            getaTICPlot(current_spec_name);
                        }, 0);
                    }
                }
                );
                uploadImage('canvas1', 'scores3D', 'png');
            });
}

function showPCA3DLoading() {
    $.getJSON('/MetaboAnalyst' + document.getElementById("mydir2").value,
            function (raw_data) {
                delete cols;
                var cols = null;
                var load_obj = raw_data['loading'];
                var names = load_obj['name'];
                var entrez = load_obj['entrez'];
                var coords = load_obj['xyz'];
                var cols = load_obj['cols'];
                //var sizes = load_obj['sizes'];
                var smps = load_obj['axis'];
                if (cx2 !== undefined) {
                    cx2.destroy();
                }

                cx2 = new CanvasXpress(
                        'canvas2',
                        {
                            'z': {
                                'name': entrez
                            },
                            'y': {
                                'vars': names,
                                'smps': smps,
                                'data': coords
                            }
                        },
                        {
                            'graphType': 'Scatter3D',
                            'xAxis': ['Dim 1'],
                            'yAxis': ['Dim 2'],
                            'zAxis': ['Dim 3'],
                            'colorSCheme': 'user',
                            'colorBy': 'dist',
                            'colors': cols,
                            //  'colors': ['rgba(0,153,255,0.8)'],
                            //   'sizeBy': sizes,
                            'sizes': [9],
                            'axisTitleScaleFontFactor': 0.8,
                            'axisTickScaleFontFactor': 0.7,
                            'canvasBox': false,
                            'disableToolbar': 'true',
                            'disableMenu': 'true',
                            'disableConfigurator': 'true',
                            'gradient': 'true',
                            'xAxisTickColor': "#FFFFFF",
                            'yAxisTickColor': "#FFFFFF",
                            'zAxisTickColor': "#FFFFFF",
                            'xAxisMinorTicks': false,
                            'yAxisMinorTicks': false,
                            'zAxisMinorTicks': false,
                            'imageDir': '../../resources/images/'
                        },
                        {
                            "click": function (o, e, t) {

                            },
                            "dblclick": function (o, e, t) {
                                //getBoxPlot(o.z.name[0]);

                                //current_entrez = o.z.entrez[0];
                                current_entrez = o.z.name[0];
                                var current_name = o.y.vars[0];
                                PF('statusDialog').show();

                                if (current_entrez !== current_name) {
                                    var current_id = current_name;
                                    setTimeout(function () {
                                    getXICPlot(current_id);
                                    }, 0);
                                } else {
                                    current_id = current_entrez;
                                    getBoxPlot(current_id);
                                }
                            },
                            "mousemove": function (o, e, t) {
                                t.showInfoSpan(e, "<pre><table>" +
                                        "<tr>" + o.y.vars[0] + "</tr><br />" +
                                        "<tr><b>" + o.y.smps[0] + "</b>: " + o.y.data[0][0] + "</tr> <br />" +
                                        "<tr><b>" + o.y.smps[1] + "</b>: " + o.y.data[0][1] + "</tr> <br />" +
                                        "<tr><b>" + o.y.smps[2] + "</b>: " + o.y.data[0][2] + "</tr> <br />" +
                                        "</table></pre>");
                            },
                            "mouseout": function (o, e, t) {
                            }
                        }
                );

                uploadImage('canvas2', 'loadings3D', 'png');
            });
}
;

function uploadImage(graphDiv, graphNm, type) {
    var canvas = document.getElementById(graphDiv);
    var dataURL = canvas.toDataURL();
    $.ajax({
        dataType: "html",
        type: "POST",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: {function: 'decodeImageData', data: dataURL, name: graphNm, format: type},
        async: false,
        cache: false,
        error: function () {
            console.log("Failed to process the request!");
        }
    });
}

function getBoxPlot(id) {
    $.ajax({
        beforeSend: function () {

        },
        dataType: "html",
        type: "GET",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: {function: 'loadingBoxPlot', id: id},
        async: false,
        success: function (result) {
            //document.getElementById('boximage').src = result;
            PF('FeatureView').show();
        }
    });
}

function getaTICPlot(fileNM) {

    $.ajax({
        beforeSend: function () {

        },
        dataType: "html",
        type: "POST",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: {function: 'loadingTICPlot', fileNM: fileNM},
        async: false,
        success: function (result) {
            PF('statusDialog').hide();
            PF('TIC').show();

        }
    });
}

function getXICPlot(id) {

    $.ajax({
        beforeSend: function () {
            PF('statusDialog').show();
        },
        dataType: "html",
        type: "POST",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: {function: 'loadingXICPlot', id: id},
        async: false,
        success: function (result) {
            PF('statusDialog').hide();
            //PF('EIC').show();
            PF('BoxPlotdialog').show();
            initSvgInteractions(result);
            updateXICs();
        }
    });
}

function getXICPlot2() {

    $.ajax({
        beforeSend: function () {
            PF('statusDialog').show();
        },
        dataType: "html",
        type: "POST",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: {function: 'loadingXICPlot', id: 'na'},
        async: false,
        success: function (result) {
            //console.log("getXICPlot2 -- loadingXICPlot" + result);
            PF('statusDialog').hide();
            //PF('EIC').show();
            PF('BoxPlotdialog').show();
            initSvgInteractions(result);
            updateXICs();
        }
    });
}

function getTimeBoxPlot(id) {
    $.ajax({
        beforeSend: function () {

        },
        dataType: "html",
        type: "POST",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: {function: 'iPCALoadingBoxPlot', id: id},
        async: false,
        success: function (result) {
            PF('FeatureView').show();
        }
    });
}

function getMultiFacBoxPlot(id) {
    $.ajax({
        beforeSend: function () {

        },
        dataType: "html",
        type: "POST",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: {function: 'multiFacBoxPlot', id: id},
        async: false,
        success: function (result) {
            PF('FeatureView').show();
        }
    });
}

function showPLSDA3DScore() {
    $.getJSON('/MetaboAnalyst' + document.getElementById("mydir").value,
            function (raw_data) {
                score_obj = raw_data['score'];
                names = score_obj['name'];
                coords = score_obj['xyz'];
                smps = score_obj['axis'];
                cols = score_obj['colors'];
                facA = score_obj['facA'];
                if (cx1 !== undefined) {
                    cx1.destroy();
                }
                cx1 = new CanvasXpress(
                        'canvas1',
                        {
                            'z': {
                                'Legend': facA
                            },
                            'y': {
                                'vars': names,
                                'smps': smps,
                                'data': coords
                            }
                        },
                        {
                            'graphType': 'Scatter3D',
                            'xAxis': ['LV1'],
                            'yAxis': ['LV2'],
                            'zAxis': ['LV3'],
                            'colorSCheme': 'user',
                            'canvasBox': false,
                            'colorBy': 'Legend',
                            'colors': cols,
                            'sizes': [10],
                            'axisTitleScaleFontFactor': 0.8,
                            'axisTickScaleFontFactor': 0.7,
                            'xAxisMinorTicks': false,
                            'xAxisTickColor': "#FFFFFF",
                            'yAxisTickColor': "#FFFFFF",
                            'zAxisTickColor': "#FFFFFF",
                            'yAxisMinorTicks': false,
                            'zAxisMinorTicks': false,
                            'axisType': 'categorical',
                            'disableToolbar': 'true',
                            'disableMenu': 'true',
                            'disableConfigurator': 'true',
                            'gradient': 'true',
                            'imageDir': '../../resources/images/'
                        }, {
                    "dblclick": function (o, e, t) {

                    }
                }
                );
            });
}

function showPLSDA3DLoading() {
    $.getJSON('/MetaboAnalyst' + document.getElementById("mydir2").value,
            function (raw_data) {
                var score_obj = raw_data['loading'];
                var names = score_obj['name'];
                var entrez = score_obj['entrez'];
                var coords = score_obj['xyz'];
                var smps = score_obj['axis'];
                var cols = score_obj['cols'];

                if (cx2 !== undefined) {
                    cx2.destroy();
                }
                cx2 = new CanvasXpress(
                        'canvas2',
                        {
                            'z': {
                                'name': entrez
                            },
                            'y': {
                                'vars': names,
                                'smps': smps,
                                'data': coords
                            }
                        },
                        {
                            'graphType': 'Scatter3D',
                            'xAxis': ['Dim 1'],
                            'yAxis': ['Dim 2'],
                            'zAxis': ['Dim 3'],
                            'colorBy': 'dist',
                            'colorSCheme': 'user',
                            'colors': cols,
                            'sizes': [10],
                            'axisTitleScaleFontFactor': 0.8,
                            'axisTickScaleFontFactor': 0.7,
                            'canvasBox': false,
                            'disableToolbar': 'true',
                            'disableMenu': 'true',
                            'disableConfigurator': 'true',
                            'gradient': 'true',
                            'xAxisTickColor': "#FFFFFF",
                            'yAxisTickColor': "#FFFFFF",
                            'zAxisTickColor': "#FFFFFF",
                            'xAxisMinorTicks': false,
                            'yAxisMinorTicks': false,
                            'zAxisMinorTicks': false,
                            'imageDir': '../../resources/images/'
                        },
                        {
                            "click": function (o, e, t) {

                            },
                            "dblclick": function (o, e, t) {
                                getBoxPlot(o.z.name[0])
                            },
                            "mousemove": function (o, e, t) {
                                t.showInfoSpan(e, "<pre><table>" +
                                        "<tr>" + o.y.vars[0] + "</tr><br />" +
                                        "<tr><b>" + o.y.smps[0] + "</b>: " + o.y.data[0][0] + "</tr> <br />" +
                                        "<tr><b>" + o.y.smps[1] + "</b>: " + o.y.data[0][1] + "</tr> <br />" +
                                        "<tr><b>" + o.y.smps[2] + "</b>: " + o.y.data[0][2] + "</tr> <br />" +
                                        "</table></pre>");
                            },
                            "mouseout": function (o, e, t) {
                            }
                        }
                );

            });
}
;

function export_image_time() {
    var exportType = PF('expMenu').getSelectedValue();
    if (exportType === "score") {
        cx1.saveSVG();
    } else {
        cx2.saveSVG();
    }
}

function save_view() {
    uploadImage('canvas1', 'scores3D', 'png');
    uploadImage('canvas2', 'loadings3D', 'png');
}

function export_image(name) {
    if (name === 'score') {
        cx1.saveSVG();
    } else {
        cx2.saveSVG();
    }
}

function updateTextInput(val) {
    //console.log("updateTextInput -->" + val);
    document.getElementById('textInput').value = val;
}

function updateloadingslider(val) {
    //console.log("updateloadingslider -->" + val);
    document.getElementById('loadingSlider').value = val;
}

function initFunctions() {
    $('#pcaDisplay').change(function () {
        var opt = $('#pcaDisplay').val();
        updatePCA(opt);
    });

    $('#updateLoading').bind('click keypress', function (event) {
        var val = $('#nodeSlider').val();
        updateLoading(val);
    });

    $('#download').change(function () {
        var type = $(this).val();
        if (type === "NA") {
            return;
        } else if (type === "pcas") {
            cx1.saveSVG();
        } else if (type === "loads") {
            cx2.saveSVG();
        } else if (type === "pcap") {
            let downloadLink = document.createElement('a');
            downloadLink.setAttribute('download', 'pca_score.png');
            let canvas = document.getElementById('canvas1');
            let img0 = canvas.toDataURL('image/png');
            var img = img0.replace(/^data:image\/[^;]/, 'data:application/octet-stream');
            downloadLink.setAttribute('href', img);
            downloadLink.click();
        } else if (type === 'loadp') {
            let downloadLink = document.createElement('a');
            downloadLink.setAttribute('download', 'pca_loading.png');
            let canvas = document.getElementById('canvas2');
            let img0 = canvas.toDataURL('image/png');
            var img = img0.replace(/^data:image\/[^;]/, 'data:application/octet-stream');
            downloadLink.setAttribute('href', img);
            downloadLink.click();
        }
    });

}

function updateLoading() {
    var nb = $('#loadingSlider').val();

    performLoad(nb, function (result) {
        if (result !== "NA") {
            updateLoad(result);
        }
    });
}

function performLoad(nb, callBack) {
    $.ajax({
        beforeSend: function () {
        },
        dataType: "html",
        type: "POST",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: "function=updateLoading" + "&number=" + nb,
        async: true,
        cache: false,
        success: function (result) {
            return callBack(result);
        },
        error: function () {
        }
    });
}

function updateLoad(result) {
    $.getJSON('/MetaboAnalyst' + result,
            function (raw_data) {
                var score_obj = raw_data['loading'];
                var names = score_obj['name'];
                var entrez = score_obj['entrez'];
                var coords = score_obj['xyz'];
                var smps = score_obj['axis'];
                var cols = score_obj['cols'];

                cx2.data.z.entrez = entrez;
                cx2.data.y.vars = names;
                cx2.data.y.smps = smps;
                cx2.data.y.data = coords;
                cx2.colors = cols;
                cx2.updateData();
            });
}

function updatePCA(opt) {
    if (opt === "all") {
        var nb = 0;
    } else if (opt === "loading") {
        var nb = $('#loadingSlider').val();
    } else {
        return;
    }

    performSpectraPCA(nb, function (result) {
        if (result !== "NA") {
            updatePCAScore(result);
        }
    });
}

function performSpectraPCA(nb, callBack) {
    $.ajax({
        beforeSend: function () {
        },
        dataType: "html",
        type: "POST",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: "function=updateSpectraPCA" + "&number=" + nb,
        async: true,
        cache: false,
        success: function (result) {
            return callBack(result);
        },
        error: function () {
            $.messager.alert('Error', 'Failed to process the request!', 'error');
            $.messager.progress('close');
        }
    });
}

function updatePCAScore(result) {
    $.getJSON('/MetaboAnalyst' + result,
            function (raw_data) {
                var score_obj = raw_data['score'];
                var names = score_obj['name'];
                var facA = score_obj['facA'];
                var facB = score_obj['facB'];
                var coords = score_obj['xyz'];
                var smps = score_obj['axis'];
                cx1.data.z["Shape Legend"] = facA;
                if (facB !== undefined) {
                    cx1.data.z["Color Legend"] = facB;
                }
                cx1.data.y.vars = names;
                cx1.data.y.smps = smps;
                cx1.data.y.data = coords;
                cx1.updateData();
                cx1.draw();
            })
}


function isNumeric(str) {
    if (typeof str != "string")
        return false // we only process strings!  
    return !isNaN(str) && // use type coercion to parse the _entirety_ of the string (`parseFloat` alone does not do this)...
            !isNaN(parseFloat(str)) // ...and ensure strings of whitespace fail
}