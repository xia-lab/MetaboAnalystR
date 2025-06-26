var view_mode = false;
var isLts = false;
var savedStateInit = "false";
var dim_mode = false;
var scaleFac = 1;
var edgeColor = '#959595';
var sigInst, settings;
var backgroundColor = "blueLight";
var edgeType = "straight";
var defaultEdgeSize = 0.8;
var cx5;
var visMode = "ppi";
var extract_count = 1;
var current_pathname;
var booleanClusterY = true;
var booleanClusterX = false;
var distType = "euclidian";
var linkType = "average";
var cls_data2 = "NA";
var order = [];
var current_GeneList = {};
var current_highlightList = [];
var netNm;
var edgeColoring = false;
var netType = "enr";
var current_comRes = [];
var highlightColor = "#0080ff";
var node_rows = [];
var initRun = true;
var moveDelay = 50;
var highlighMode = false;
var lasso_active = false;
var greyColor = '#959595';
var greyColorB = "rgba(211,211,211, 0.5)";
var prevEnrType = "siggene";
gradCol2 = "blueLight";;
gradCol1 = "blueLight";;
'use strict';

if (typeof sigma === 'undefined')
    throw 'sigma is not declared';

// Initialize packages:
sigma.utils.pkg('sigma.canvas.labels');

/**
 * This label renderer will just display the label on the right of the node.
 *
 * @param  {object}                   node     The node object.
 * @param  {CanvasRenderingContext2D} context  The canvas context.
 * @param  {configurable}             settings The settings function.
 */
sigma.canvas.labels.def = function (node, context, settings) {
    var fontSize,
            prefix = settings('prefix') || '',
            size = node[prefix + 'size'];

    if (node.labeled === -1) {
        return;
    }

    if (node.labeled !== 1) {
        if (size < settings('labelThreshold'))
            return;
    }

    if (!node.label || typeof node.label !== 'string')
        return;

    fontSize = (settings('labelSize') === 'fixed') ?
            settings('defaultLabelSize') :
            settings('labelSizeRatio') * size;

    context.font = (settings('fontStyle') ? settings('fontStyle') + ' ' : '') +
            fontSize + 'px ' + settings('font');
    context.fillStyle = (settings('labelColor') === 'node') ?
            (node.color || settings('defaultNodeColor')) :
            settings('defaultLabelColor');

    context.fillText(
            node.label,
            Math.round(node[prefix + 'x'] + size + 3),
            Math.round(node[prefix + 'y'] + fontSize / 3)
            );
};

function export_image(name) {
    if (name === 'score') {
        document.getElementById("downloadimage").src = document.getElementById("canvas1").toDataURL();
    } else {
        document.getElementById("downloadimage").src = document.getElementById("canvas2").toDataURL();
    }
    PF('exportDialog').show();
}

var sigInst;
var usr_dir;
var dataType = "";
function showSigmaNetwork() {
    if ($(parent.window.document).find("#sidebar-form\\:m_report")) {
        isLts = true;
    }

    var res = $.ajax({
        type: "GET",
        url: '/MetaboAnalyst/faces/AjaxCall?function=getjobinfo' + "&viewmode=enrnet&ignoreMe=" + new Date().getTime(),
        async: false
    }).responseText;
    var jobinfo = res.split("||");
    usr_dir = jobinfo[0];
    org = jobinfo[1];
    enrType = jobinfo[2];
    dataType = jobinfo[3];
    analType = enrType;
    naviString = "EnrichNetwork";
    initLib = jobinfo[4];
    initOkay = jobinfo[5];
    netNm = jobinfo[6];
    console.log(netNm)
    var reportInit = jobinfo[7];
    savedStateInit = jobinfo[8];
    //console.log(jobinfo);
    //netNm = "enrichNet_" + initLib + ".json";
    currFileNm = netNm;
    document.getElementById("spinner").style.display = "block";
    $("#spinner").fadeOut("slow");
    $('#network-parent').css("width", $(window).width() - 360);
    $('#network-parent').css("height", $(window).height() - 40);
    currentEnrichFile = netNm.substring(0, netNm.length - 5).replace("enrichNet", "enrichment");
    $("#gseaRow").css('display', 'disabled');

    //setEnrichOpts(org);
    sigInst = new sigma({
        renderers: [
            {
                container: document.getElementById('networkview'),
                type: 'canvas' // sigma.renderers.canvas works as well
            }
        ],
        settings: {
            defaultLabelColor: '#000',
            defaultLabelSize: 11,
            defaultLabelBGColor: '#000',
            defaultLabelHoverColor: '#000',
            labelThreshold: 12,
            defaultEdgeColor: 'default',
            doubleClickEnabled: false,
            minNodeSize: 0,
            maxNodeSize: 0,
            sideMargin: 1,
            minEdgeSize: 0,
            maxEdgeSize: 0,
            defaultNodeBorder: 1,
            nodeBorderColor: "black"

        }
    });

    var activeState = sigma.plugins.activeState(sigInst);
    var dragListener = new sigma.plugins.dragNodes(sigInst, sigInst.renderers[0], activeState);

    lasso = new sigma.plugins.lasso(sigInst, sigInst.renderers[0], {
        'strokeStyle': 'orange',
        'lineWidth': 1,
        'fillWhileDrawing': true,
        'fillStyle': 'rgba(255, 255, 177, 0.4)',
        'cursor': 'crosshair'
    });

    lasso.bind('selectedNodes', function (event) {
        // set all edges as "inactive" to avoid mixing active nodes and edges:
        activeState.dropEdges();

        // nodes within the lasso area:
        var nodes = event.data;

        // set all nodes as "inactive" if no node is selected:
        if (!nodes.length)
            activeState.dropNodes();

        // add the selected nodes to the "active" nodes:
        activeState.addNodes(nodes.map(function (n) {
            return n.id;
        }));

        //highlight the selected node
        selectedNodeIDs = [];
        for (var j = 0; j < nodes.length; j++) {
            selectedNodeIDs.push(nodes[j].id);
        }

        highlightSelectedNodes(selectedNodeIDs, "Manual Selection");

        setTimeout(function () {
            // disable the lasso tool after a selection:
            lasso.deactivate();
            //de-highlight nodes
            //unHighlightFunEnrichNodes(selectedNodeIDs);
            // refresh the display to see the active nodes:
            sigInst.refresh({skipIdexation: true});
        }, 0);
    });

    dragListener.bind('startdrag', function (event) {
        current_node = event.data.node;
        current_node.start_x = current_node.x;
        current_node.start_y = current_node.y;
        dependants = [];
        if ($('#selectOpt').val() === "neighbour") {
            var neighbors = {};
            neighbors[current_node.id] = 1;
            sigInst.graph.edges().forEach(function (e) {
                if (current_node.id === e.source || current_node.id === e.target) {
                    neighbors[e.source] = 1;
                    neighbors[e.target] = 1;
                }
            });
            sigInst.graph.nodes().forEach(function (n) {
                if (neighbors[n.id]) {
                    n.start_x = n.x;
                    n.start_y = n.y;
                    dependants.push(n);
                }
            });
        }
    });

    dragListener.bind('drag', function (event) {
        var mv_x = current_node.x - current_node.start_x;
        var mv_y = current_node.y - current_node.start_y;
        var g;
        for (var i = 0; i < dependants.length; i++) {
            g = dependants[i];
            g.x = g.start_x + mv_x;
            g.y = g.start_y + mv_y;
        }
        sigInst.refresh();
    });

    dragListener.bind('dragend', function (event) {
        if (lasso_active) {
            resetNodes(selectedNodeIDs);
        }
        sigInst.refresh();
    });


    sigInst.bind('overNode', function (event) {
        var node = event.data.node;
        var neighbors = {};
        neighbors[node.id] = 1;
        sigInst.graph.edges().forEach(function (e) {
            if (node.id === e.source || node.id === e.target) {
                neighbors[e.source] = 1;
                neighbors[e.target] = 1;
                e.color = "black";
            }
        });
        sigInst.refresh();
    });

    setupSideBar().then(() => {
        initTopMenu();

        if (isLts) {
            checkSavedState();
            addListeners();

        } else {
            setNetworkB(); // ✅ Now guaranteed to run after setupSideBar completes
            addListeners();

        }
    }).catch(err => {
        console.error("Error in setupSideBar:", err);
    });

}


function setupSideBar() {
    return new Promise((resolve, reject) => {
        console.log(usr_dir + "/" + netNm);

        if (initOkay === "true") {
            $.getJSON(usr_dir + "/" + netNm, function (data) {
                netData = data;
                console.log(netData);

                updateBackgroundColorGradient("#222222", "#222222");
                // setOrder();
                loadDgs(enrType);

                if (netData.analType === "genelist") {
                    $("#analOpt").attr('disabled', 'disabled');
                    $("#rankOpt").attr('disabled', 'disabled');
                }

                symbol2entrez = {};
                $.each(data.genelist, function (k, v) {
                    symbol2entrez[v] = k;
                });

                $('#edgeIc').bind('click keypress', function (event) {
                    if (edgeType === "straight") {
                        sigInst.graph.edges().forEach(function (e) {
                            e.molType = "curve";
                        });
                        edgeType = "curve";
                    } else {
                        sigInst.graph.edges().forEach(function (e) {
                            e.molType = "";
                        });
                        edgeType = "straight";
                    }
                    sigInst.refresh();
                });

                resolve(); // ✅ Done
            }).fail(reject);
        } else {
            $.messager.alert('Error', 'No enriched gene sets have been found using KEGG database, please try with another database', 'error');
            $.getJSON(usr_dir + "/" + netNm, function (data) {
                netData = data;
                console.log(netData);

                intersect_names = "";
                $("#dNmOpt").attr('disabled', 'disabled');
                //setOrder();
                $('#dg1').datagrid('selectAll');
                $('#setOpt').val("union");

                const rows = $('#dg1').datagrid('getSelections');
                const ids = rows.map(r => r.name);

                const comRes = [];
                $.each(data.genelist, function (k, v) {
                    comRes.push({entrez: k, symbol: v});
                });

                symbol2entrez = {};
                $.each(data.genelist, function (k, v) {
                    symbol2entrez[v] = k;
                });

                current_comRes = comRes;
                setupSideBar_Intersect(comRes);
                intersect_mode = 1;

                resolve(); // ✅ Done
            }).fail(reject);
        }
    });
}


function highlightNodes(node) {
    var set = node.label;
    var scopeVal = $('#selectOpt').val();
    var contrastCol = getContrastColor(backgroundColor.replace("#", ""));
    if (scopeVal === "single") {
        sigInst.graph.nodes().forEach(function (n) {
            if (set === n.label) {
                if (n.highlight === 1) {
                    if (contrastCol === "white") {
                        n.color = n.true_color_b;
                    } else {
                        n.color = n.true_color_w;
                    }
                    n.highlight = 0;
                } else {
                    n.size = n.size + 2;
                    n.color = highlightColor;
                    n.borderColor = null;
                    n.highlight = 1;
                }
            }
        });
    } else if (scopeVal === "neighbour") {
        var neighbors = {};
        neighbors[current_node.id] = 1;
        sigInst.graph.edges().forEach(function (e) {
            if (current_node.id === e.source || current_node.id === e.target) {
                neighbors[e.source] = 1;
                neighbors[e.target] = 1;
            }
        });
        sigInst.graph.nodes().forEach(function (n) {
            if (neighbors[n.id] || set === n.label) {
                if (n.highlight === 1) {
                    if (contrastCol === "white") {
                        n.color = n.true_color_b;
                    } else {
                        n.color = n.true_color_w;
                    }
                } else {
                    n.size = n.size + 2;
                    n.color = highlightColor;
                    n.highlight = 1;
                }
            }
        });
    }
    if (node.molType !== "gene") {
        current_highlightList = netData.hits[set];
        setupSideBar_Intersect(current_comRes);
        var genes = netData.hits[set];
        var arr = [];
        netData.bnodes.forEach(function (n) {
            if (genes.indexOf(n.label) !== -1) {
                arr.push(n);
            }
        });
        displayCurrentSelectedNodesStat(arr, set, node);
    } else {
        displayNodeInfo(node);
    }
    sigInst.refresh();

}

var colorsArray = ["#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#fb8072", "#80b1d3", "#e377c2", "#bcbd22", "#17becf", "#d9d9d9", "#bc80bd", "#ccebc5", "#ffed6f"];
function loadDgs(method) {
    setupDg2Columns(method);
    var data_grid = $('#dg2');
    data_grid.datagrid('loadData', {total: 0, rows: []}); // clear

    var mdl_rows = [];
    var idx = 0;
    var data = netData;

    $.each(data['enr'], function (k, v) {
        var row = {
            pathname: data['id'][idx]
        };

        if (method === "mum") {
            row["Hits_sig"] = data['enr'][idx]['Hits_sig'];
            row["FET"] = data['enr'][idx]['FET'];
            row["Gamma"] = data['enr'][idx]['Gamma'];
        } else if (method === "gsea") {
            row["Hits"] = data['enr'][idx]['Hits'];
            row["NES"] = data['enr'][idx]['NES'];
            row["P_val"] = data['enr'][idx]['P_val'];
        } else if (method === "integ") {
            row["Sig_Hits"] = data['enr'][idx]['Sig_Hits'];
            row["Combined_Pvals"] = data['enr'][idx]['Combined_Pvals'];
        } else {
            row["Hits"] = data['enr'][idx]['Hits'];
            row["Raw_p"] = data['enr'][idx]['Raw_p'];
            row["FDR"] = data['enr'][idx]['FDR'];
        }

        mdl_rows.push(row);
        idx++;
    });
    // Sort by primary p-value field depending on method
    if (method === "mum") {
        mdl_rows.sort((a, b) => a.Gamma - b.Gamma);
    } else if (method === "gsea") {
        mdl_rows.sort((a, b) => a.P_val - b.P_val);
    } else if (method === "integ") {
        mdl_rows.sort((a, b) => a.Combined_Pvals - b.Combined_Pvals);
    } else {
        mdl_rows.sort((a, b) => a.FDR - b.FDR);

    }

    data_grid.datagrid({
        onClickRow: function (index, row) {
            current_highlightList = netData.hits[row.pathname];
            searchNetwork(row.pathname);
            setupSideBar_Intersect(current_comRes);
        },
        onCheck: function (index, row) {
            highlightNodes(sigInst.graph.nodes(row.pathname));
        },
        onUncheck: function (index, row) {
            var node = sigInst.graph.nodes(row.pathname);
            if (node.highlight === 1) {
                highlightNodes(node);
            }
        },
        onCheckAll: function (rows) {
            var nodeIDs = rows.map(r => r.pathname);
            highlightRegularNodes(nodeIDs);
        },
        onUncheckAll: function (rows) {
            var nodeIDs = rows.map(r => r.pathname);
            resetNodes(nodeIDs);
            sigInst.refresh();
        }
    });
    console.log(mdl_rows)
    console.log($("#dg2"))
    $('#dg2').datagrid('loadData', mdl_rows);
}




function pagerFilter(data) {
    if (typeof data.length === 'number' && typeof data.splice === 'function') {    // is array
        data = {
            total: data.length,
            rows: data
        };
    }
    var dg = $('#dg');
    var opts = dg.datagrid('options');
    var pager = dg.datagrid('getPager');
    pager.pagination({
        showPageList: false,
        showRefresh: false,
        displayMsg: "",
        onSelectPage: function (pageNum, pageSize) {
            opts.pageNumber = pageNum;
            opts.pageSize = pageSize;
            pager.pagination('refresh', {
                pageNumber: pageNum,
                pageSize: pageSize
            });
            dg.datagrid('loadData', data);
        }
    });
    if (!data.originalRows) {
        data.originalRows = (data.rows);
    }
    var start = (opts.pageNumber - 1) * parseInt(opts.pageSize);
    var end = start + parseInt(opts.pageSize);
    data.rows = (data.originalRows.slice(start, end));
    return data;
}

function resetNet() {
    var c = sigInst.camera;
    sigma.misc.animation.camera(c, {
        x: 0,
        y: 0,
        ratio: 1.0
    },
            {duration: 200});
}

function downloadImage() {
    document.getElementById("downloadimage").src = export2Image();
    $("#pngdialog").dialog('open');
}



function setNetwork() {
    setTimeout(function () {
        sigInst.graph.clear();
        var data = netData;
        node_rows = data.nodes;
        var labelthresh;
        if (node_rows.length < 50) {
            labelthresh = 8
        } else {
            labelthresh = 8
        }

        sigInst.settings.labelThreshold = labelthresh;

        for (var i = 0; i < data.nodes.length; i++) {
            var nd = data.nodes[i];
            nd.hoverColor = "#333333";
            nd.orig_color = nd.color;
            nd.true_color_b = nd.color;
            nd.true_color_w = nd.colorw;
            if (nd.expanded !== null) {
                nd.expanded = false;
            } else {
                nd.expanded = true;
                expNodes(nd.label);
            }
            nd.x = nd.x - 200;
            nd.y = nd.y - 200;
            if (nd.size === null) {
                nd.size = 14;
            }
            if (nd.id !== null) {
                sigInst.graph.addNode(nd);
            }
        }

        for (var j = 0; j < data.edges.length; j++) {
            var eg = data.edges[j];
            eg.color = edgeColor;
            if (eg.width) {
                eg.size = eg.width;
            } else {
                eg.size = 0.3;
            }
            if (eg.source !== undefined) {
                sigInst.graph.addEdge(eg);
            }
        }

        var config = {
            nodeMargin: 8.0,
            scaleNodes: 1.0
        };
        sigInst.configNoverlap(config);

        sigInst.refresh();

        $("#loader").fadeOut("fast");
        $("#spinner").fadeOut("slow");
    }, 120);
}

function addRemoveLabels(type) {
    if (type === "default") {
        sigInst.graph.nodes().forEach(function (n) {
            n.labeled = 0;
        });
    } else if (type === "add") {
        sigInst.graph.nodes().forEach(function (n) {
            if (n.highlight === 1) {
                n.labeled = 1;
            }
        });
    } else if (type === "remove") {
        sigInst.graph.nodes().forEach(function (n) {
            if (n.highlight === 1) {
                n.labeled = -1;
            }
        });
    } else {
        sigInst.graph.nodes().forEach(function (n) {
            n.labeled = -1;
        });
    }
    sigInst.refresh();
}

function setNetworkB() {
    var data = netData;
    sigInst.graph.clear();
    var contrastCol = getContrastColor(backgroundColor.replace("#", ""));

    if ($("#viewOpt").val() === "bi") {
        for (var i = 0; i < data.bnodes.length; i++) {
            var nd = data.bnodes[i];
            nd.labeled = 0;
            nd.molType = data.bnodes[i].molType;
            nd.expanded = false;
            nd.x = nd.posx;
            nd.y = nd.posy;
            nd.highlight = 0;
            nd.size = nd.true_size;
            console.log(nd.molType)
            if (nd.molType === "compound") {
                nd.type = "square";
                nd.color = nd.colorw;
                nd.orig_color = nd.colorw;
                nd.true_color_b = nd.color;
                nd.true_color_w = nd.colorw;
                nd.entrez = symbol2entrez[nd.label];
                nd.borderColor = "#000"

                nd.size = 6;
            } else {
                nd.borderColor = "#000"
                nd.color = hexToRGB(nd.colorw, 0.8)
                nd.orig_color = hexToRGB(nd.colorw, 0.8)
                nd.true_color_b = hexToRGB(nd.colorb, 0.8)
                nd.true_color_w = hexToRGB(nd.colorw, 0.8)

            }
            if (nd.id !== null) {
                nd.true_size = nd.size;
                if (!sigInst.graph.nodes(nd.id)) {
                    sigInst.graph.addNode(nd);
                }
            }
        }


        for (var j = 0; j < data.bedges.length; j++) {
            var eg = data.bedges[j];
            if (!eg.id.includes("b")) {
                eg.id = "b" + eg.id;
            }
            if (contrastCol === "white") {
                eg.color = "#d3d3d3";
            } else {
                eg.color = "#808080";
            }            //eg.hidden = true;
            if (eg.id !== undefined) {
                if (!sigInst.graph.edges(eg.id)) {

                    sigInst.graph.addEdge(eg);
                }
            }

            if (eg.width) {
                eg.size = eg.width;
            } else {
                eg.size = defaultEdgeSize;
            }
        }

        sigInst.startForceAtlas2({gravity: 1, scalingRatio: 70, slowDown: 1000, barnesHutOptimize: false, startingIterations: 0, iterationsPerRender: 1});
        var runtime = 2000;
        var node_num = node_rows.length;
        if (node_num > 5000) {
            runtime = 10000;
        } else if (node_num > 1000) {
            runtime = node_num + 1000;
        }

        setTimeout(function () {
            sigInst.killForceAtlas2();
        }, runtime);
    } else {
        console.log(data.nodes);
        for (var i = 0; i < data.nodes.length; i++) {
            var nd = data.nodes[i];
            nd.x = nd.posx;
            nd.y = nd.posy;
            nd.color = nd.colorw;
            nd.orig_color = nd.colorw;
            nd.true_color_b = nd.color;
            nd.true_color_w = nd.colorw;
            nd.molType = data.nodes[i].molType;
            nd.borderColor = "#000"

            nd.expanded = false;
            nd.highlight = 0;
            nd.size = nd.true_size;
            if (nd.size === null) {
                nd.size = 14;
            }
            if (nd.id !== null) {
                if (!sigInst.graph.nodes(nd.id)) {
                    sigInst.graph.addNode(nd);
                }
            }
        }


        for (var j = 0; j < data.edges.length; j++) {
            var eg = data.edges[j];
            if (eg.source !== undefined) {
                eg.color = edgeColor;
                eg.hidden = false;
                if (eg.width) {
                    eg.size = eg.width;
                } else {
                    eg.size = defaultEdgeSize;
                }
                if (!sigInst.graph.edges(eg.id)) {

                    sigInst.graph.addEdge(eg);
                }
            }
        }
        /*
         sigInst.startForceAtlas2({gravity: 1, scalingRatio: 70, slowDown: 1000, barnesHutOptimize: false, startingIterations: 0, iterationsPerRender: 1});
         var runtime = 700;
         var node_num = node_rows.length;
         if (node_num > 200) {
         runtime = 3000;
         } else if (node_num > 100) {
         runtime = 2000;
         }
         setTimeout(function () {
         sigInst.killForceAtlas2();
         }, runtime);
         */
    }
    //check if all nodes same size
    var sizeArr = [];
    sigInst.graph.nodes().forEach(function (n) {
        sizeArr.push(n.size)
    });
    if (Math.max(sizeArr) === Math.min(sizeArr)) {
        sigInst.graph.nodes().forEach(function (n) {
            n.size = 12;
        });
    }
    //sigInst.configNoverlap(config);
    sigInst.camera.goTo({x: 0, y: 0, angle: 0, ratio: 1.0});
    sigInst.refresh();

    $("#loader").fadeOut("fast");
    $("#spinner").fadeOut("slow");
}


sigma.utils.pkg('sigma.canvas.nodes');
sigma.canvas.nodes.def = function (node, context, settings) {
    var prefix = settings('prefix') || '';

    context.fillStyle = node.color || settings('defaultNodeColor');
    context.beginPath();
    context.arc(
            node[prefix + 'x'],
            node[prefix + 'y'],
            node[prefix + 'size'],
            0,
            Math.PI * 2,
            true
            );

    context.closePath();
    context.fill();
    if (node.borderColor) {
        context.lineWidth = 2;
        context.strokeStyle = node.borderColor;
        context.stroke();
    }
};

var enrich_name = "enrichmentNet_result";

function exportResultTable() {
    if ($('#dgPw').datagrid('getRows').length === 0) {
        $.messager.alert('Error', 'No functional enrichment analysis has been performed!', 'error');
        return;
    }
    setupFileDownload(enrich_name + ".csv");
}

function setupFileDownload(result) {
    var fileLnk = $("#fileLnk");
    fileLnk.empty();
    fileLnk.append("Right click the link below, then 'Save Link As ... ' to download the file<br/><br/>");
    fileLnk.append('<strong><a href="' + usr_dir + '/' + result + '" target="_blank"><u>' + result + '</u></a></strong>');
    $.messager.progress('close');
    $("#filedialog").dialog('open');
}

function initTopMenu() {
    console.log(isLts + "====isLts");
    if (isLts) {
        initReportFunctions();
    }
    $('#extractBn').bind('click keypress', function (event) {
        extractNetwork();
        event.stopPropagation();
        return false;
    });

    $("#networkOpt").change(function () {
        var val = $(this).val();
        netType = val;
        if (val.includes("Extract")) {
            setupExtractNet(val);
        } else {
            setNetworkB();
        }

    });

    $("#viewOpt").change(function () {
        var val = $(this).val();
        netType = val;
        var netVal = $("#networkOpt").val()
        if (netVal.includes("Extract")) {
            setupExtractNet(netVal)
        } else {
            setNetworkB();
        }

    });

    $(".link").click(function () {
        //your JS here
    });

    $("#analOpt").change(function () {
        var analType = $("#analOpt").val();
        if (analType === "siggene") {
            $("#rankOpt").attr('disabled', 'disabled')
            $("#pvalInput").removeAttr("disabled");
        } else {
            $("#rankOpt").removeAttr("disabled");
            $("#pvalInput").attr('disabled', 'disabled')
        }
    });

    $("#setOpt").change(function () {
        var val = $(this).val();
        if (val !== "diff") {
            $("#dNmOpt").attr('disabled', 'disabled')
        } else {
            $("#dNmOpt").removeAttr("disabled");
        }
    });

    $('#lassoBn').bind('click keypress', function (event) {

        if (lasso_active) {
            lasso_active = false;
            lasso.deactivate();
            activeState.dropNodes(); //back to regular single node state
        } else {
            lasso_active = true;
            lasso.activate();
        }

        event.stopPropagation();
        return false;
    });


    $('#resetBn').bind('click keypress', function (event) {
        resetNetwork();
        event.stopPropagation();
        return false;
    });

    $('#autofitBn').bind('click keypress', function (event) {
        var c = sigInst.camera;
        sigma.misc.animation.camera(c, {
            x: 0,
            y: 0,
            ratio: 1.0
        },
                {duration: 200});
    });

    $('#zoomInBn').bind('click keypress', function (event) {
        var c = sigInst.camera;
        sigma.misc.animation.camera(c, {
            ratio: c.ratio / c.settings('zoomingRatio')
        }, {
            duration: 200
        });
    });
    $('#zoomOutBn').bind('click keypress', function (event) {
        var c = sigInst.camera;
        sigma.misc.animation.camera(c, {
            ratio: c.ratio * c.settings('zoomingRatio')
        }, {
            duration: 200
        });
    });
    $('#moveUpBn').bind('click keypress', function (event) {
        var c = sigInst.camera;
        sigma.misc.animation.camera(c, {
            x: c.x,
            y: c.y + moveDelay
        },
                {duration: 200});
    });
    $('#moveDownBn').bind('click keypress', function (event) {
        var c = sigInst.camera;
        sigma.misc.animation.camera(c, {
            x: c.x,
            y: c.y - moveDelay
        },
                {duration: 200});
    });
    $('#moveLeftBn').bind('click keypress', function (event) {
        var c = sigInst.camera;
        sigma.misc.animation.camera(c, {
            x: c.x + moveDelay,
            y: c.y
        },
                {duration: 200});
    });
    $('#moveRightBn').bind('click keypress', function (event) {
        var c = sigInst.camera;
        sigma.misc.animation.camera(c, {
            x: c.x - moveDelay,
            y: c.y
        },
                {duration: 200});
    });

    $('#highlightBn').bind('click keypress', function (event) {
        if (!highlighMode) {
            highlighMode = true;
        } else {
            highlighMode = false;
        }
    });

    $('#noverlapIc').bind('click keypress', function (event) {

        var node_num = node_rows.length;
        if (node_num > 5000) {
            $.messager.alert('', "Error: this network is too big to run the algorithm (max: 5000)!", 'error');
        } else {
            document.getElementById("loader").style.display = "block";
            setTimeout(function () {
                var spd = 2;
                var iters = 300;
                if (node_num > 3500) {
                    spd = 18;
                    iters = 30;
                } else if (node_num > 2000) {
                    spd = 12;
                    iters = 50;
                } else if (node_num > 500) {
                    spd = Math.floor(node_num / 500) + 2;
                    iters = 300 - 50 * Math.floor(node_num / 500);
                } else {
                    //do nothing
                }
                var config = {
                    speed: spd,
                    nodeMargin: 4.0,
                    scaleNodes: 1.3,
                    maxIterations: iters
                };
                sigInst.configNoverlap(config);
                sigInst.startNoverlap();
                $("#loader").fadeOut("fast");
            }, 120);
        }
    });

    $('#selectOpt').change(function () {
        if (lasso_active) {
            lasso_active = false;
            lasso.deactivate();
            activeState.dropNodes(); //back to regular single node state
        }
    });

    $('#shareBn').bind('click keypress', function (event) {
        currFileNm = netNm;
        sharingLink(org, analType, naviString);
    });


    $('#exportOpt').change(function () {
        var type = $('#exportOpt').val();
        if (type === 'NA')
            return;
        if (type === "png") {
            $('#pngdialog').dialog('open');
        } else if (type === "svg") {
            sigInst.graph.edges().forEach(function (e) {
                e.color = rgbaToRgbEquivalent(e.color, "rgb(255, 255, 255)");
            });

            sigInst.graph.nodes().forEach(function (n) {
                n.color = rgbaToRgbEquivalent(n.color, "rgb(255, 255, 255)");
            });
            sigInst.toSVG({download: true, filename: 'network_analyst.svg', labels: true, background: backgroundColor, width: parseInt($('#network-parent').css("width")), height: parseInt($('#network-parent').css("height"))});
        } else if (type === "tbl") {
            exportResultTable("funtb");
        } else if (type === "share") {
            currFileNm = netNm;
            sharingLink(netData.org, netData.analType, netData.naviString);
        } else {
            return;
        }
    });


    $("#custom").spectrum({
        color: "#0080ff",
        showInitial: true,
        change: function (color) {
            highlightColor = color.toHexString();
        },
        showInput: true,
        showPalette: true,
        showSelectionPalette: true, // true by default
        palette: [
            ['black', 'white'],
            ['#666666', '#0080ff']
        ]
    });

    $('#updateOpacity').bind('click keypress', function (event) {
        var val = parseFloat($('#edgeSliderOpa').val());
        var rgbaCol = hexToRGB(edgeColor, val);
        sigInst.graph.edges().forEach(function (e) {
            e.color = rgbaCol;
        });
        sigInst.refresh();
        $('#edgeopacitydlg').dialog('close');
    });


    $('#nodeLabelBn').bind('click keypress', function (event) {
        var type = $('#labelOpt').val();
        addRemoveLabels(type);
        $('#nodelabeldlg').dialog('close');
    });

    $('#nodeLabelSizeBn').bind('click keypress', function (event) {
        var val = $('#labelSliderOpa').val();
        sigInst.settings({
            defaultLabelSize: val
        });
        sigInst.refresh();
    });

    $("#customEdge").spectrum({
        color: greyColor,
        showInitial: true,
        change: function (color) {

            var col = color.toHexString();
            edgeColor = col;
            sigInst.graph.edges().forEach(function (e) {
                e.color = col;
            });
            sigInst.refresh();
            $('#edgecoldlg').dialog('close');
        },
        showInput: true,
        showPalette: true,
        showSelectionPalette: true, // true by default
        palette: [
            ['black', 'white'],
            ['#666666', '#0080ff']
        ]
    });

    $("#customLabel").spectrum({
        flat: true,
        color: backgroundColor,
        showInitial: true,
        cancelText: "", // not shown, as this is not working in flat mode
        change: function (color) {
            var labelColor = color.toHexString();
            sigInst.settings({
                defaultLabelColor: labelColor
            });

            sigInst.refresh();
            $('#nodelabeldlg').dialog('close');
        },
        showInput: true,
        showPalette: true,
        showSelectionPalette: true, // true by default
        palette: [
            ['black', 'white'],
            ['#666666', '#0080ff']
        ]
    });

    $("#customBgg").spectrum({
        color: "#222222",
        showInitial: true,
        cancelText: "", // not shown, as this is not working in flat mode        
        change: function (color) {
            var col = color.toHexString();
            var type = $("#bgColType").val();
            //var dir = $("#gradientDir").val();
            var col2 = col;
            if (type === "gradient_dark") {
                col2 = "#222222";
            } else if (type === "gradient_light") {
                col2 = "#ffffff";
            }
            updateBackgroundColorGradient(col, col2);
            $('#bgcolordlg').dialog('close');
        },
        showInput: true,
        showPalette: true,
        showSelectionPalette: true, // true by default
        palette: [
            ['black', '#905356'],
            ['#38597a', 'white']
        ]
    });

    $('#backgroundOpt').change(function () {
        var bgCol = $('#backgroundOpt').val();

        if (bgCol === "custom") {
            $('#bgcolordlg').dialog('open');
            return;
        } else if (bgCol === "blueLight") {
            updateBackgroundColorGradient("#f5f5f5", "#0066CC");
        } else if (bgCol === "purpleDark") {
            updateBackgroundColorGradient("#514F6A", "#222222");
        } else if (bgCol === "greenDark") {
            updateBackgroundColorGradient("#228B22", "#222222");
        } else {
            backgroundColor = $('#backgroundOpt').val();
            if (backgroundColor === 'NA')
                return;
            updateBackground(backgroundColor);
        }
    });

    $('#nodeStyleOpt').change(function () {
        var val = $(this).val();
        if (val === "label") {
            $('#nodelabeldlg').dialog('open');
        } else if (val === "color") {
            $('#nodecoldlg').dialog('open');
        } else if (val === "size") {
            $('#nodesizedlg').dialog('open');
        } else {
            return;
        }
    });

    $('#edgeStyleOpt').change(function () {
        var val = $(this).val();
        if (val === "opacity") {
            $('#edgeopacitydlg').dialog('open');
        } else if (val === "color") {
            $('#edgecoldlg').dialog('open');
        } else if (val === "bundle") {
            $('#bundledlg').dialog('open');
        } else if (val === "thickness") {
            $('#edgethicknessdlg').dialog('open');
        } else {
            return;
        }
    });

    $('#layoutOpt').change(function () {
        var type = $('#layoutOpt').val();
        if (type === 'na') {
            return;
        }
        if (type === 'ForceAtlas') {
            sigInst.startForceAtlas2({gravity: 100, scalingRatio: 70, slowDown: 100, barnesHutOptimize: true, startingIterations: 30, iterationsPerRender: 5});
            //adjust time for laying out, 
            var runtime = 500;
            var node_num = node_rows.length;
            if (node_num > 5000) {
                runtime = 10000;
            } else if (node_num > 1000) {
                runtime = node_num + 1000;
            }
            setTimeout(function () {
                sigInst.killForceAtlas2();
            }, runtime);
        } else if (type === 'noverlap') {

            var node_num = node_rows.length;
            if (node_num > 5000) {
                $.messager.alert('', "Error: this network is too big to run the algorithm (max: 5000)!", 'error');
            } else {
                document.getElementById("loader").style.display = "block";
                setTimeout(function () {
                    var spd = 2;
                    var iters = 300;
                    if (node_num > 3500) {
                        spd = 18;
                        iters = 30;
                    } else if (node_num > 2000) {
                        spd = 12;
                        iters = 50;
                    } else if (node_num > 500) {
                        spd = Math.floor(node_num / 500) + 2;
                        iters = 300 - 50 * Math.floor(node_num / 500);
                    } else {
                        //do nothing
                    }
                    var config = {
                        speed: spd,
                        nodeMargin: 4.0,
                        scaleNodes: 1.3,
                        maxIterations: iters
                    };
                    sigInst.configNoverlap(config);
                    sigInst.startNoverlap();
                    $("#loader").fadeOut("fast");
                }, 120);
            }
        } else {
            doLayoutUpdate(type, function (result) {
                if (result !== "NA") {
                    updateNetworkLayout(result);
                } else {
                    $.messager.alert('', "Error: failed to update the layout!", 'error');
                }
                $.messager.progress('close');
            });
        }
        //reset to default
        $('#layoutOpt').val("na");
    });

}

function doLayoutUpdate(algo, callBack) {
    var net_nm = "";

    if ($("#viewOpt").val() === "bi") {
        net_nm = "enrichNet_bipartite";
    } else {
        net_nm = "enrichNet";
    }
    $.ajax({
        beforeSend: function () {
            $.messager.progress({
                text: 'Processing .....'
            });
        },
        dataType: "html",
        type: "GET",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: "function=updateNetworkLayout" + "&layoutalgo=" + algo + "&currentNetNm=" + net_nm,
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


function updateNodeSize1() {
    var type = $('#sizeScope').val();
    var val = $('#sizeOpt1').val();
    var sizeArr = [];
    sigInst.graph.nodes().forEach(function (n) {
        sizeArr.push(n.size)
    });
    if (val === 'increase') {
        if (type === "all") {
            sigInst.graph.nodes().forEach(function (n) {
                n.size = n.size + 1;
            });
        } else if (type === "highlight") {
            sigInst.graph.nodes().forEach(function (n) {
                if (n.highlight) {
                    n.size = n.size + 1;
                }
            });
        } else {
            sigInst.graph.nodes().forEach(function (n) {
                if (n.molType === type) {
                    n.size = n.size + 1;
                }
            });
        }

    } else {

        if (type === 'all') {
            sigInst.graph.nodes().forEach(function (n) {
                if (n.size > 2) {
                    n.size = n.size - 1;
                } else {
                    n.size = n.size * 0.9;
                }
            });
        } else if (type === "highlight") {
            sigInst.graph.nodes().forEach(function (n) {
                if (n.highlight) {
                    if (n.size > 2) {
                        n.size = n.size - 1;
                    } else {
                        n.size = n.size * 0.9;
                    }
                }
            });
        } else {
            sigInst.graph.nodes().forEach(function (n) {
                if (n.molType === type) {
                    if (n.size > 2) {
                        n.size = n.size - 1;
                    } else {
                        n.size = n.size * 0.9;
                    }
                }
            });
        }
    }
    sigInst.refresh();
}
function updateBackground(backgroundColor) {
    gradCol2 = backgroundColor;
    gradCol1 = backgroundColor;
    if (backgroundColor === "custom") {
        $('#bgcolordlg').dialog('open');
        return;
        // } else if (backgroundColor === "#38597a" || backgroundColor === "#222222" || backgroundColor === "#514F6A") {
    } else if (backgroundColor === "#222222" || backgroundColor === "#514F6A") {
        $("#networkspace").css('background', '').css('background', backgroundColor);
        $("#networkview").css('background', '').css('background', backgroundColor);
        sigInst.settings({
            defaultLabelColor: '#fff',
            defaultLabelBGColor: '#fff',
            defaultEdgeColor: '#d3d3d3'

        });

        sigInst.graph.nodes().forEach(function (n) {
            if (n.expanded && n.molType !== "gene") {
                //n.borderColor = n.true_color_b
                //n.color = hexToRGB(n.true_color_b, 0.7)
            } else {
                //n.color = n.true_color_b;
            }
        });

        sigInst.graph.edges().forEach(function (e) {
            var nt = sigInst.graph.nodes(e.target)
            var ns = sigInst.graph.nodes(e.source)
            if (nt.expanded && ns.molType === "compound") {
                //e.color = nt.borderColor;
            } else {
                e.color = "#d3d3d3";
            }
        });

    } else {
        $("#networkspace").css('background', '').css('background', backgroundColor);
        $("#networkview").css('background', '').css('background', backgroundColor);
        sigInst.settings({
            defaultLabelColor: '#000',
            defaultLabelBGColor: '#000',
            defaultEdgeColor: '#222'
        });

        sigInst.graph.nodes().forEach(function (n) {
            if (n.expanded && n.molType !== "gene") {
                //n.borderColor = n.true_color_w;
                //n.color = hexToRGB(n.true_color_w, 0.7)
            } else {
                //n.color = n.true_color_w;
            }
        });
        sigInst.graph.edges().forEach(function (e) {
            var nt = sigInst.graph.nodes(e.target)
            var ns = sigInst.graph.nodes(e.source)
            if (nt.expanded && ns.molType === "compound") {
                //e.color = nt.borderColor;
            } else {
                e.color = "#808080";
            }
        });

    }
    sigInst.refresh();
}

function doSetOperation(callBack) {
    var ids = [];
    var rows = $('#dg1').datagrid('getSelections');
    var operation = $('#setOpt').val();
    var refNm = $('#dNmOpt').val();
    for (var i = 0; i < rows.length; i++) {
        ids.push(rows[i].name);
    }

    if (ids.length === 0) {
        $.messager.alert('', "Error: please first select data using the check boxes.", 'error');
        return;
    }
    if (operation === "diff") {
        if (ids.indexOf(refNm) === -1) {
            $.messager.alert('', "Error: Reference must be in your selected data.", 'error');
            return;
        }
    }
    intersect_names = ids;
    $.ajax({
        beforeSend: function () {
            $.messager.progress({
                text: 'Processing .....'
            });
        },
        dataType: "html",
        type: "GET",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: 'function=performSetOperationEnr' + "&names=" + ids.join(";") + "&operation=" + operation + "&refName=" + refNm,
        async: true,
        cache: false,
        success: function (result) {
            return callBack(result);
        },
        error: function () {
            $.messager.alert('', "Error: failed to process the request!", 'error');
            $.messager.progress('close');
        }
    });

}

function setOrder() {
    var data_grid = $('#dg1');
    //empty if there is any
    data_grid.datagrid('loadData', {
        "total": 0,
        "rows": []
    });
    data = netData['sizes'];

    var refOpts = $('#dNmOpt');
    if (data !== undefined && data[0] !== undefined) {
        for (var i in data) {
            order.push(data[i].name);
            data_grid.datagrid('appendRow', {
                name: data[i].name,
                size: data[i].size
            });
            refOpts.append('<option value="' + data[i].name + '">' + data[i].name + '</option>');
        }
        if (data.length === 1) {
            $("#setOpt").attr('disabled', 'disabled')
            current_GeneList['result'] = netData['genelist'];
        }
    } else {
        data_grid.datagrid('appendRow', {
            name: data.name,
            size: data.size
        });
        refOpts.append('<option value="' + data.name + '">' + data.name + '</option>');
        $("#setOpt").attr('disabled', 'disabled')
        current_GeneList['result'] = netData['genelist'];
    }

}

function performSetOperation() {
    var enrType = $("#analOpt").val()
    if (netData.sizes[0] === undefined || netData.sizes.length === 1 || enrType !== "siggene") {
        doSetEnrichmentTest();
    } else {
        doSetOperation(function (result) {
            if (result.substring(0, 5) !== "error") {
                $.getJSON(usr_dir + '/' + result, function (raw_data) {
                    var comRes = [];
                    $.each(raw_data, function (k, v) {
                        comRes.push({entrez: k, symbol: v});
                    });
                    current_comRes = comRes;
                    setupSideBar_Intersect(comRes);
                    intersect_mode = 1;
                    doSetEnrichmentTest();
                });
            } else {
                var msg = result.split("||")[1];
                $.messager.alert('', "Error: " + msg, 'error');
            }
            $.messager.progress('close');
        });
    }
}

function setupSideBar_Intersect(comRes) {
    var type = $('#setOpt').val();
    var difftype = $('#setOpt').val();
    var text;
    if (type === "intersect") {
        text = '<span id="header-data" class="header-data"><h4 style="line-height:1em; overflow: visible;">Intersection (' + intersect_names.length + ' datalists) </h4></span>'
    } else if (type === "union") {
        text = '<span id="header-data" class="header-data"><h4 style="line-height:1em; overflow: visible;">Union (' + intersect_names.length + ' datalists) </h4></span>'
    } else {
        text = '<span id="header-data" class="header-data"><h4 style="line-height:1em; overflow: visible;">Unique genes in ' + $('#dNmOpt').val() + '</h4></span>'
    }

    $("#header-data").html(text);
    $("#displayed-data").remove();
    $("#selected-data").append('<span id="displayed-data" class="displayed-data"></span>');

    if (comRes.length > 1000) {
        t = '<p>Genes not shown for large lists. <br /> Click on a <b>gene set</b> node to see corresponding genes.</p>'
        for (var i = 0; i < comRes.length; i++) {
            geneList = geneList + "; " + comRes[i].entrez;
        }
    } else {
        var geneList = "";
        var t = '<ul class="circos-symbols">';
        for (var i = 0; i < comRes.length; i++) {
            geneList = geneList + "; " + comRes[i].entrez;
            if (current_highlightList.indexOf(comRes[i].symbol) === -1) {
                t = t + '<li><span>' + comRes[i].symbol + '</span></li>'
            }
        }
        t = t + '</ul>';
    }
    if (geneList !== undefined) {
        current_GeneList['result'] = geneList;
    }
    $("#displayed-data").append(t);

    var qOpts = $('#queryArc');
    qOpts.empty(); //need to clear all previous options
    qOpts.append('<option value="result">Current Result</option>');
}


function doSetEnrichmentTest() {
    testSetEnrichment(function (result, enrTypePrevious) {
        if (result === 'NA.json') {
            $.messager.alert('', "Error: " + "No result was generated for the current parameters.", 'error');
        } else {
            $.getJSON(usr_dir + '/' + result, function (raw_data) {
                currentEnrichFile = result.substring(0, result.length - 5).replace("enrichNet", "enrichment");
                netNm = result;
                var data = raw_data;
                netData = raw_data;
                var data_grid = $('#dg2');
                //empty if there is any
                data_grid.datagrid('loadData', {
                    "total": 0,
                    "rows": []
                });
                //console.log(data);
                var mdl_rows = [];
                var idx = 0;
                $.each(data['id'], function (k, v) {
                    mdl_rows.push({
                        pathname: data['id'][idx],
                        //expected: data['enr'][idx]['Expected'],
                        hits: data['enr'][idx]['Hits'] + "/" + data['enr'][idx]['Total'],
                        pval: data['enr'][idx]['Pval'],
                        padj: data['enr'][idx].FDR
                    });
                    idx = idx + 1;
                });

                data_grid.datagrid('loadData', mdl_rows);

                symbol2entrez = {};
                $.each(data.genelist, function (k, v) {
                    symbol2entrez[v] = k;
                });
                setNetworkB();
                console.log(netData)
                $("#dNmOpt").attr('disabled', 'disabled')
                setOrder();
                /*&
                 $('#dg1').datagrid('selectAll');
                 $('#setOpt').val("union");
                 
                 var rows = $('#dg1').datagrid('getSelections');
                 var ids = [];
                 for (var i = 0; i < rows.length; i++) {
                 ids.push(rows[i].name);
                 }
                 intersect_names = ids;
                 */
                var comRes = [];
                $.each(data.genelist, function (k, v) {
                    comRes.push({entrez: k, symbol: v});
                });

                symbol2entrez = {}
                $.each(data.genelist, function (k, v) {
                    symbol2entrez[v] = k;
                });

                current_comRes = comRes;
                setupSideBar_Intersect(comRes);
                intersect_mode = 1;
                prevEnrType = enrTypePrevious;
            });
        }
        $.messager.progress('close');
    });
}

function testSetEnrichment(callBack) {
    var fundb = $('#enrichdb').val();
    var dataOpt = "result";
    if (dataOpt === "NA") {
        $.messager.alert('', "Enrichment analysis is on gene lists displayed in the <b>Gene List View</b>. " +
                "<b>Clicking</b> an arc or perform <b>Data Comparison</b> to obtain gene lists of interest.", 'info');
        return;
    }
    target_data = dataOpt; // data to be compared
    //now get the gene list
    var entrez_ids = current_GeneList[dataOpt];
    $('#dg2').datagrid('loadData', {
        "total": 0,
        "rows": []
    });
    var enrType = $("#analOpt").val();

    if (enrType === "siggene") {
        //console.log(prevEnrType);
        //if(prevEnrType === "singlenetwork"){
        entrez_ids = "gsea_prev";
        var pval = $("#pvalInput").val();
        //}
        $.ajax({
            beforeSend: function () {
                $.messager.progress({
                    text: 'Processing .....'
                });
            },
            dataType: "html",
            type: "POST",
            url: '/MetaboAnalyst/faces/AjaxCall',
            data: {function: 'doListEnrichmentTestView', funlib: fundb, entrezIDs: entrez_ids, pval: pval},
            async: true,
            cache: false,
            success: function (result) {
                return callBack(result, enrType);
            },
            error: function () {
                $.messager.alert("Error", "Failed to process the request!");
                $.messager.progress('close');
            }
        });
    } else if (enrType === "singlenetwork") {
        var rankOpt = $("#rankOpt").val();
        $.ajax({
            beforeSend: function () {
                $.messager.progress({
                    text: 'Processing .....'
                });
            },
            dataType: "html",
            type: "POST",
            url: '/MetaboAnalyst/faces/AjaxCall',
            data: {function: 'switchGseaNet', database: fundb, rankOpt: rankOpt},
            async: true,
            cache: false,
            success: function (result) {
                return callBack(result, enrType);
            },
            error: function () {
                $.messager.alert("Error", "Failed to process the request!");
                $.messager.progress('close');
            }
        });
    } else {
        $.ajax({
            beforeSend: function () {
                $.messager.progress({
                    text: 'Processing .....'
                });
            },
            dataType: "html",
            type: "POST",
            url: '/MetaboAnalyst/faces/AjaxCall',
            data: {function: 'calculateGsNet', type: enrType, database: fundb},
            async: true,
            cache: false,
            success: function (result) {
                return callBack(result, enrType);
            },
            error: function () {
                $.messager.alert("Error", "Failed to process the request!");
                $.messager.progress('close');
            }
        });
    }
}

function resizeNetwork() {
    //dynamic update centerpane width based on user browser
    $('#network-parent').css("width", $(window).width() - 360);
    $('#network-parent').css("height", $(window).height() - 4);

    sigInst.refresh();
}

function doGraphExport(callBack, format) {
    $.ajax({
        beforeSend: function () {
            $.messager.progress({
                text: 'Processing .....'
            });
        },
        dataType: "html",
        type: "GET",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: "function=exportNetwork" + "&format=" + format,
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


function highlightFunEnrichNodes(nodes) {
    if (nodes.length === 1) {
        searchNetwork(nodes[0]);
    } else {
        var nodeVec = [];
        highlight_mode = 1;
        //var borderCol = getBorderColor();
        //sigInst.position(0, 0, 1).draw(2, 2, 2);
        sigInst.graph.nodes().forEach(function (n) {
            if (nodes.indexOf(n.id) !== -1) {
                n.size = n.size + 2;
                //if (n.size > 10) {
                //    n.size = 10;
                //}
                n.color = highlightColor;
                n.highlight = 1;
                nodeVec.push({id: n.id, label: n.label});
            } else {
                n.color = n.orig_color
            }
        });
        sigInst.refresh();
    }
}

function searchNetwork(nodeID) {
    var hit = 0;

    var borderCol = getBorderColor();

    sigInst.graph.nodes().forEach(function (n) {
        if (n.id.toUpperCase() === nodeID.toUpperCase() || n.label.toUpperCase() === nodeID.toUpperCase()) {
            hit = 1;
            n.size = n.size + 2;
            //n.borderColor = "#FFFF00";
            n.borderColor = borderCol;
            //n.highlight = 1;
            sigma.misc.animation.camera(
                    sigInst.camera,
                    {
                        x: n[sigInst.camera.readPrefix + 'x'],
                        y: n[sigInst.camera.readPrefix + 'y'],
                        ratio: 0.35
                    },
                    {duration: 300});
        } else {
            n.borderColor = null;
        }
    });
    sigInst.refresh();
    if (!hit) {
        $.messager.alert('Error', "Node " + nodeID + " was not found in the current network!", 'error');
    }
    return;
}

function getBorderColor() {
    if (backgroundColor === "#222222" || backgroundColor === "#514F6A") {
        return("#FAFAD2");
    } else {
        return("#A0522D");
    }
}

function getContrastColor(hexcolor) {
    var r = parseInt(hexcolor.substr(0, 2), 16);
    var g = parseInt(hexcolor.substr(2, 2), 16);
    var b = parseInt(hexcolor.substr(4, 2), 16);
    var yiq = ((r * 299) + (g * 587) + (b * 114)) / 1000;
    return (yiq >= 128) ? '#222222' : 'white';
}

function exportResultTable(name) {
    if (name === "funtb") {
        if ($('#dg2').datagrid('getRows').length === 0) {
            $.messager.alert('Error', 'No functional enrichment analysis has been performed!', 'error');
            return;
        }
    }
    if (name === "comtb") {
        setupFileDownload("module_table.csv");
    } else if (name === "funtb") {
        setupFileDownload(currentEnrichFile + ".csv");
    } else {
        doGraphExport(setupFileDownload, name);
    }
}

function setupFileDownload(result) {
    var fileLnk = $("#fileLnk");
    fileLnk.empty();
    fileLnk.append("Right click the link below, then 'Save Link As ... ' to download the file<br/><br/>");
    fileLnk.append('<strong><a href="' + usr_dir + '/' + result + '" target="_blank"><u>' + result + '</u></a></strong>');
    $.messager.progress('close');
    $("#filedialog").dialog('open');
}

function formatNum(val, row) {
    console.log(val);

    console.log(row);
    if (val.toString().length > 7) {
        return parseFloat(val.toPrecision(3)).toExponential();
    } else {
        return val;
    }
}

function updateNetworkLayout(fileNm) {
    console.log(fileNm)
    console.log(usr_dir + "====usr_dir");
    $.getJSON(usr_dir + '/' + fileNm, function (data) {
        //console.log(data);
        var nd_pos = {};
        for (var i = 0; i < data.nodes.length; i++) {
            var nd = data.nodes[i];
            nd_pos[nd.id] = [nd.x, nd.y];
        }
        var my_nd;
        sigInst.graph.nodes().forEach(function (nd) {
            my_nd = nd_pos[nd.id];
            if (my_nd !== undefined) {
                nd.x = my_nd[0];
                nd.y = my_nd[1];
            }
        });
        sigInst.refresh();
        sigma.misc.animation.camera(sigInst.camera, {
            x: 0,
            y: 0,
            ratio: 1.0
        }, {duration: 200});
    });
}

function displayCurrentSelectedNodesStat(nodes, title, node = null) {
    var stats = $("#stats");
    stats.empty();
    console.log(netData.pwType);
    console.log(node);
    console.log(netData.pwType.toLowerCase().includes("kegg"))
    if (node !== null && netData.pwType.toLowerCase().includes("kegg") && ["mum", "gsea", "integ"].indexOf(enrType) === -1) {
        if (enrType !== "mum") {
            stats.append(
                    '<li><b><a href="https://www.genome.jp/kegg-bin/show_pathway?' +
                    encodeURIComponent(node.pwId) +
                    '" target="_blank" rel="noopener noreferrer">' +
                    title +
                    '</a></b></li>'
                    );
        } else {
            stats.append(
                    '<li><b><a href="https://www.genome.jp/kegg-bin/show_pathway?' +
                    encodeURIComponent(node.pwId) +
                    '" target="_blank" rel="noopener noreferrer">' +
                    title +
                    '</a></b> (significant hits in red)</li>'
                    );
        }

    } else {
        if (title !== "") {
            if (enrType === "mum") {
                stats.append('<strong>' + title + '</strong><br/> (signficant hits in red)');

            } else {
                stats.append('<lh><b>' + title + '</b></lh>');
            }
        }
    }
    var sum = 0;
    for (var i = 0; i < nodes.length; i++) {
        sum = sum + nodes[i].exp;
    }
    var avg = sum / nodes.length;
    console.log(enrType + "====" + avg)
    if (avg === 0 || isNaN(avg)) {
        for (var i = 0; i < nodes.length; i++) {
            if (enrType === "mum" || enrType === "gsea" || enrType === "integ") {
                if (enrType === "mum") {
                    if (netData.sigCmpds.indexOf(nodes[i].label) !== -1) {
                        stats.append('<li><a style="color:red" href="http://www.genome.jp/dbget-bin/www_bget?ko:' + nodes[i].id + '" target="_blank"><u>' + nodes[i].label + '</u></a></li>');
                    } else {
                        stats.append('<li><a href="http://www.genome.jp/dbget-bin/www_bget?ko:' + nodes[i].id + '" target="_blank"><u>' + nodes[i].label + '</u></a></li>');
                    }
                } else {
                    stats.append('<li><a href="http://www.genome.jp/dbget-bin/www_bget?ko:' + nodes[i].id + '" target="_blank"><u>' + nodes[i].label + '</u></a></li>');
                }
            } else {
                stats.append('<li>' + nodes[i].label + '</li>');
            }
        }
    } else {
        stats.append('<table style="width:100%">');
        for (var i = 0; i < nodes.length; i++) {
            if (nodes[i].exp === 0 || nodes[i].exp === undefined || nodes[i].exp === "NA") {
                stats.append('<tr><td>' + nodes[i].label + '</td><td>' + "" + '</td></tr>');
            } else {
                //console.log(nodes[i].exp)
                stats.append('<tr><td style="padding-right:10px">' + nodes[i].label + '</td><td>' + nodes[i].exp.toPrecision(3) + '</td></tr>');
            }
        }
        stats.append('</table> ');
    }
    console.log(stats);
}

function resetNetwork() {
    //default no edge coloring
    sigInst.camera.goTo({x: 0, y: 0, angle: 0, ratio: 1.0});
    setNetworkB();


    $("#stats").empty();
    $("#pathLnks").empty();

    /*var col2 = $('#dg2').datagrid('getColumnOption', 'color');
     col2.styler = function () {
     return 'background-color:white';
     };*/
}
function expNodes(label) {
    var contrastCol = getContrastColor(gradCol1.replace("#", ""));
    var data = netData;

    console.log(data.hits);
    console.log(label);

    /* ---------- expanded genes for this set -------------------------------- */
    var genes = data.hits[label];
    if (typeof genes === "string")
        genes = [genes];

    var set = label;
    var ndSet = null;
    var arr = [];

    /* ---------- locate centre node ---------------------------------------- */
    sigInst.graph.nodes().forEach(function (n) {
        if (n.label === set || n.id === set)
            ndSet = n;
    });

    /* ---------- gather stats for display ---------------------------------- */
    netData.bnodes.forEach(function (n) {
        if (genes.indexOf(n.label) !== -1)
            arr.push(n);
    });

    /* ====================================================================== */
    /*  EXPAND                                                                */
    /* ====================================================================== */
    if (ndSet.expanded === false) {

        displayCurrentSelectedNodesStat(arr, set, ndSet);

        /* mark centre node as expanded & dim it slightly                        */
        var n = sigInst.graph.nodes(label);
        n.expanded = true;
        //n.borderColor = n.color;
        n.color = hexToRGB(n.color, 0.8);

        /* ---------- collect new nodes --------------------------------------- */
        const newNodesArr = [];
        for (var i = 0; i < data.bnodes.length; i++) {
            var nd = data.bnodes[i];
            if (genes.indexOf(nd.label) !== -1)
                newNodesArr.push(nd);
        }

        /* sort by expression (low→high)                                        */
        newNodesArr.sort(function (a, b) {
            return parseFloat(a.exp) - parseFloat(b.exp);
        });

        /* ---------- radius calculation -------------------------------------- */
        const N = newNodesArr.length;

        var optimalDistance = 1;           // default for ≤20
        if (N > 50)
            optimalDistance = 0.15;
        else if (N > 20)
            optimalDistance = 0.30;

        var L = (N * optimalDistance) / (2 * Math.PI);

        /* minimum radius = what 10 nodes would require at this distance        */
        var minRadius = (10 * optimalDistance) / (2 * Math.PI);
        if (L < minRadius)
            L = minRadius;

        /* ---------- add / update nodes -------------------------------------- */
        for (var i = 0; i < newNodesArr.length; i++) {
            var nd = newNodesArr[i];

            nd.hidden = false;
            nd.expanded = true;

            //if (contrastCol === "white") {
            nd.color = nd.colorb;
            nd.orig_color = nd.colorb;
            //} else {
            //    nd.color = nd.colorb;
            //    nd.orig_color = nd.colorb;
            //}

            nd.true_color_b = nd.color;
            nd.true_color_w = nd.colorw;
            nd.highlight = 0;
            nd.entrez = symbol2entrez[nd.label];

            /* polar → Cartesian placement                                        */
            nd.x = ndSet.x + L * Math.cos((2 * Math.PI * i / N) - Math.PI / 2);
            nd.y = ndSet.y + L * Math.sin((2 * Math.PI * i / N) - Math.PI / 2);
            nd.borderColor = "#000"
            nd.type = "square";

            nd.size = 7.5;                      // <— larger than original 4
            nd.molType = data.bnodes[i].molType;

            if (nd.id !== null && sigInst.graph.nodes(nd.id) === undefined) {
                nd.expBool = true;
                sigInst.graph.addNode(nd);
            }
        }

        /* ---------- existing edges toggle visibility ------------------------ */
        sigInst.graph.edges().forEach(function (e) {
            if (e.source === set || e.target === set)
                e.expanded = false;
            if (genes.indexOf(e.source) !== -1 || genes.indexOf(e.target) !== -1) {
                e.expanded = true;
                e.hidden = false;
            }
        });

        /* ---------- add brand-new edges from backup list -------------------- */
        for (var j = 0; j < data.bedges.length; j++) {
            var eg = data.bedges[j];

            if (!eg.id.includes("b"))
                eg.id = "b" + eg.id;

            if (eg.source === set || eg.target === set ||
                    genes.indexOf(eg.source) !== -1 || genes.indexOf(eg.target) !== -1) {

                var exists = sigInst.graph.edges(eg.id) !== undefined;

                if (!exists) {
                    eg.hidden = false;

                    //if (eg.source === set || eg.target === set) {
                    //    eg.color = sigInst.graph.nodes(set).color;
                    //} else if (sigInst.graph.nodes(eg.target) &&
                    //        sigInst.graph.nodes(eg.target).expanded) {
                    //    eg.color = sigInst.graph.nodes(eg.target).color;
                    //} else {
                    //}
                    eg.size = 1;

                    if (sigInst.graph.nodes(eg.target)) {
                        eg.expBool = true;
                        sigInst.graph.addEdge(eg);
                    }
                }
            }
        }

        sigInst.refresh();

        /* ====================================================================== */
        /*  COLLAPSE                                                              */
        /* ====================================================================== */
    } else {

        $("#stats").empty();

        sigInst.graph.nodes().forEach(function (n) {
            if (n.label === label) {
                n.expanded = false;
                n.color = (contrastCol === "white") ? n.true_color_b : n.true_color_w;
                n.borderColor = "#000"

            }
            if (genes.indexOf(n.label) !== -1)
                sigInst.graph.dropNode(n.id);
        });

        sigInst.graph.edges().forEach(function (e) {
            if (genes.indexOf(e.source) !== -1 || genes.indexOf(e.target) !== -1) {
                e.hidden = true;
            } else if (e.expanded && (e.source === set || e.target === set)) {
                e.hidden = false;
                e.expanded = false;
            }
        });

        sigInst.refresh();
    }
}


function displayNodeInfo(node) {
    var stats = $("#stats");
    stats.empty();
    stats.append('<li><strong>Name</strong>: ' + node.label + '</li>');

    stats.append('<li><strong>Entrez</strong>: ' + '<a href="https://www.ncbi.nlm.nih.gov/gene/?term=' + node.entrez + '" target="_blank"><u>' + node.entrez + '</u></a>' + '</li>');
}

function hexToRGB(hex, alpha) {
    var r = parseInt(hex.slice(1, 3), 16),
            g = parseInt(hex.slice(3, 5), 16),
            b = parseInt(hex.slice(5, 7), 16);

    if (alpha) {
        return "rgba(" + r + ", " + g + ", " + b + ", " + alpha + ")";
    } else {
        return "rgb(" + r + ", " + g + ", " + b + ")";
    }
}


function searchNodeList() {
    var search = $('#nodeid').val();
    if (search === "") {
        resetNetwork();
        return;
    }

    var hitnode = "NA";

    sigInst.graph.nodes().forEach(function (n) {
        if (search.toUpperCase() === n.label.toUpperCase() || search.toUpperCase() === n.id.toUpperCase()) {
            hitnode = "hit";
            searchNetwork(n.id);
        }
    });

    if (hitnode === "NA") {
        $.messager.alert('', "The given node is not in the currentt graph: " + search, 'info');
        return;
    }
}

function extractNetwork() {
    var nodeIDs = [];
    sigInst.graph.nodes().forEach(function (n) {
        if (n.highlight) {
            nodeIDs.push(n.id);
        }
    });
    if (nodeIDs.length === 0) {
        $.messager.alert('Error', 'No highlighted nodes found in the network!', 'error');
        return;
    } else {
        sigInst.graph.nodes().forEach(function (n) {
            if (!n.highlight) {
                sigInst.graph.dropNode(n.id);
            } else {
                var contrastCol = getContrastColor(backgroundColor.replace("#", ""));
                if (contrastCol === "white") {
                    n.color = n.true_color_b
                } else {
                    n.color = n.true_color_w
                }
            }
        });
    }
    sigInst.refresh()
    setupNetworkOpts();
}

function setupNetworkOpts() {
    //need to set up options for functional annotation
    var nm = "EnrichNet_Extract" + extract_count;

    var json = {};
    json.nodes = [];
    json.edges = [];
    var i = 0;
    sigInst.graph.nodes().forEach(function (n) {
        n.highlight = 0;
        json.nodes[i] = n;

        i = i + 1;
    });

    var j = 0
    sigInst.graph.edges().forEach(function (e) {
        json.edges[j] = e;
        j = j + 1;
    });

    var nOpts = $('#networkOpt');
    var curr_nm = "EnrichNet_Extract" + extract_count;
    nOpts.append('<option value="' + nm + '">' + nm + '</option>');
    $('#networkOpt').val(curr_nm);
    localStorage.setItem(nm, JSON.stringify(json));
    extract_count = extract_count + 1;
}

function highlightRegularNodes(nodes) {

    var nodeVec = [];

    sigInst.graph.nodes().forEach(function (n) {
        if (nodes.indexOf(n.id) !== -1) {
            n.size = n.size + 1;
            n.color = highlightColor;
            n.highlight = 1;
        }
    });

    sigInst.refresh();
}

function resetNodes(nodeIDs) {
    var contrastCol = getContrastColor(backgroundColor.replace("#", ""));
    sigInst.graph.nodes().forEach(function (n) {
        if (nodeIDs.indexOf(n.id) !== -1) {
            if (n.highlight) {
                n.highlight = 0;
                n.borderColor = null;
                //n.size = n.true_size;
                if (contrastCol === "white") {
                    n.color = n.colorb;
                } else {
                    n.color = n.colorw;
                }
            }
        }
    });
    sigInst.refresh();
}

function setupExtractNet(val) {
    var data = JSON.parse(localStorage.getItem(val));
    sigInst.graph.clear();

    if ($("#viewOpt").val() === "enr") {
        for (var i = 0; i < data.nodes.length; i++) {
            var nd = data.nodes[i];
            if (nd.id !== null) {
                sigInst.graph.addNode(nd);
            }
        }


        for (var j = 0; j < data.edges.length; j++) {
            var eg = data.edges[j];
            if (eg.source !== undefined) {
                sigInst.graph.addEdge(eg);
            }
        }
    } else {
        var genes = []
        var sets = []
        for (var i = 0; i < data.nodes.length; i++) {
            var nd = data.nodes[i]
            if (i === 0 && nd.molType !== "gene") {
                genes = netData.hits[data.nodes[i].label];
                sets.push(nd.label);
            } else if (nd.molType !== "gene") {
                genes = genes.concat(netData.hits[data.nodes[i].label])
                sets.push(nd.label);
            }
        }
        genes = Array.from(new Set(genes));

        for (var i = 0; i < netData.bnodes.length; i++) {
            var nd = netData.bnodes[i];
            if (genes.indexOf(nd.label) !== -1 || sets.indexOf(nd.label) !== -1) {
                if (nd.molType !== "gene") {
                    nd.expanded = true;
                    nd.borderColor = nd.colorb;
                    nd.color = hexToRGB(nd.colorb, 0.1)
                    nd.size = nd.true_size;
                } else {
                    nd.hidden = false;
                    nd.expanded = true;
                    nd.color = nd.colorb;
                    nd.orig_color = nd.colorb;
                    nd.true_color_b = nd.color;
                    nd.true_color_w = nd.colorw;
                    nd.highlight = 0;
                    nd.entrez = symbol2entrez[nd.label];
                    nd.size = 4;
                    nd.molType = netData.bnodes[i].molType;
                }
                nd.x = nd.posx
                nd.y = nd.posy
                sigInst.graph.addNode(nd);
            }
        }


        for (var j = 0; j < netData.bedges.length; j++) {
            var eg = netData.bedges[j];
            if (sigInst.graph.nodes(eg.source) !== undefined && sigInst.graph.nodes(eg.target) !== undefined) {
                eg.color = edgeColor;
                sigInst.graph.addEdge(eg);
            }
        }

    }
    sigInst.refresh();
    /*
     sigInst.startForceAtlas2({gravity: 10, scalingRatio: 70, slowDown: 1000, barnesHutOptimize: false, startingIterations: 0, iterationsPerRender: 1});
     
     setTimeout(function () {
     sigInst.killForceAtlas2();
     }, 1700);
     */

}

function highlightSelectedNodes(nodes, title) {
    var nodeVec = [];
    highlight_mode = 1;
    var borderCol = getBorderColor();
    //sigInst.position(0, 0, 1).draw(2, 2, 2);

    sigInst.graph.edges().forEach(function (e) {
        e.color = greyColorB;
    });
    sigInst.graph.nodes().forEach(function (n) {
        if (nodes.indexOf(n.id) !== -1) {

            n.color = highlightColor;
            n.highlight = 1;
            nodeVec.push(n);
        } else {
            n.borderColor = null;
        }
    });
    sigInst.refresh();
    displayCurrentSelectedNodesStat(nodeVec, title);
}

function hexToRgbA(hex, alpha) {
    var c;
    if (/^#([A-Fa-f0-9]{3}){1,2}$/.test(hex)) {
        c = hex.substring(1).split('');
        if (c.length == 3) {
            c = [c[0], c[0], c[1], c[1], c[2], c[2]];
        }
        c = '0x' + c.join('');
        return 'rgba(' + [(c >> 16) & 255, (c >> 8) & 255, c & 255].join(',') + ',' + alpha + ')';
    }
    throw new Error('Bad Hex');
}


function exportingPng() {
    var scale = parseFloat($('#pngRes').val());
    document.getElementById("loader").style.display = "block";
    setTimeout(function () {
        rescaleSigInst(scale);
        var dataURL = export2Image();
        dataURL = changeDpiDataUrl(dataURL, 300);
        saveDataURI(defaultFileName('.png'), dataURL);
        rescaleSigInst(1);
    }, 100);
    $("#pngdialog").dialog('close');
    $("#loader").fadeOut("fast");
}

function updateTextInputOpa(val) {
    document.getElementById('textInputOpa').value = val;
}

function updateTextInputThickness(val) {

    document.getElementById('textInputThickness').value = val;
}

function updateTextInputLabelSize(val) {
    document.getElementById('textInputLabelSize').value = val;
}

function updateThickness() {
    var val = parseFloat($('#edgeSliderThickness').val());
    defaultEdgeSize = val;
    sigInst.settings({
        minEdgeSize: defaultEdgeSize,
        maxEdgeSize: defaultEdgeSize
    });
    sigInst.refresh();
    $('#edgethicknessdlg').dialog('close');
}

function addListeners() { //specific to list enrichment network

    var activeState = sigma.plugins.activeState(sigInst);
    var dragListener = new sigma.plugins.dragNodes(sigInst, sigInst.renderers[0], activeState);

    lasso = new sigma.plugins.lasso(sigInst, sigInst.renderers[0], {
        'strokeStyle': 'orange',
        'lineWidth': 1,
        'fillWhileDrawing': true,
        'fillStyle': 'rgba(255, 255, 177, 0.4)',
        'cursor': 'crosshair'
    });

    lasso.bind('selectedNodes', function (event) {
        // set all edges as "inactive" to avoid mixing active nodes and edges:
        activeState.dropEdges();

        // nodes within the lasso area:
        var nodes = event.data;

        // set all nodes as "inactive" if no node is selected:
        if (!nodes.length)
            activeState.dropNodes();

        // add the selected nodes to the "active" nodes:
        activeState.addNodes(nodes.map(function (n) {
            return n.id;
        }));

        //highlight the selected node
        selectedNodeIDs = [];
        for (var j = 0; j < nodes.length; j++) {
            selectedNodeIDs.push(nodes[j].id);
        }

        highlightSelectedNodes(selectedNodeIDs, "Manual Selection");

        setTimeout(function () {
            // disable the lasso tool after a selection:
            lasso.deactivate();
            //de-highlight nodes
            //unHighlightFunEnrichNodes(selectedNodeIDs);
            // refresh the display to see the active nodes:
            sigInst.refresh({skipIdexation: true});
        }, 0);
    });

    dragListener.bind('startdrag', function (event) {
        current_node = event.data.node;
        current_node.start_x = current_node.x;
        current_node.start_y = current_node.y;
        dependants = [];
        if ($('#selectOpt').val() === "neighbour") {
            var neighbors = {};
            neighbors[current_node.id] = 1;
            sigInst.graph.edges().forEach(function (e) {
                if (current_node.id === e.source || current_node.id === e.target) {
                    neighbors[e.source] = 1;
                    neighbors[e.target] = 1;
                }
            });
            sigInst.graph.nodes().forEach(function (n) {
                if (neighbors[n.id]) {
                    n.start_x = n.x;
                    n.start_y = n.y;
                    dependants.push(n);
                }
            });
        } else if ($('#selectOpt').val() === "highlight") {
            sigInst.graph.nodes().forEach(function (n) {
                if (n.highlight === 1) {
                    n.start_x = n.x;
                    n.start_y = n.y;
                    dependants.push(n);
                }
            });
        }
    });

    dragListener.bind('drag', function (event) {
        var mv_x = current_node.x - current_node.start_x;
        var mv_y = current_node.y - current_node.start_y;
        var g;
        for (var i = 0; i < dependants.length; i++) {
            g = dependants[i];
            g.x = g.start_x + mv_x;
            g.y = g.start_y + mv_y;
        }
        sigInst.refresh();
    });

    dragListener.bind('dragend', function (event) {
        if (lasso_active) {
            resetNodes(selectedNodeIDs);
        }
        sigInst.refresh();
    });


    sigInst.bind('overNode', function (event) {
        var node = event.data.node;
        var neighbors = {};
        neighbors[node.id] = 1;
        sigInst.graph.edges().forEach(function (e) {
            if (node.id === e.source || node.id === e.target) {
                neighbors[e.source] = 1;
                neighbors[e.target] = 1;
                e.color = "black";
            }
        });
        sigInst.refresh();
    });


    sigInst.bind('doubleClickNode', function (e) {
        /* sigInst.bind('clickNode', function (e) { */
        var set = e.data.node.label;
        console.log(dataType);
        console.log($("#viewOpt").val());

        //$("#networkOpt").val().includes("Extract")
        if ($("#viewOpt").val() === "enr") {
            if (highlighMode) {
                highlightNodes(e.data.node);
            } else {
                if (e.data.node.molType !== "gene") {
                    expNodes(set);
                } else {
                    highlightNodes(e.data.node);
                    displayNodeInfo(e.data.node);
                    if (dataType !== "genelist") {
                        getBoxPlot(e.data.node.entrez);
                    }
                }
            }
        } else {
            if (e.data.node.molType !== "gene") {
                highlightNodes(e.data.node);
            } else {
                highlightNodes(e.data.node);
                displayNodeInfo(e.data.node);
                if (dataType !== "genelist") {
                    getBoxPlot(e.data.node.entrez);
                }
            }
        }
        sigInst.refresh();
    });

}


function rgbaToRgbEquivalent(rgba, backgroundRgb) {
    // Check if the color is in rgba format
    if (!rgba.startsWith("rgba")) {
        // If not, return the color as is
        return rgba;
    }

    // Parse the rgba string
    let rgbaValues = rgba.match(/\d+(\.\d+)?/g).map(Number); // Updated to support floating point numbers
    let [r, g, b, a] = rgbaValues;

    // Parse the background rgb string
    let backgroundValues = backgroundRgb.match(/\d+/g).map(Number);
    let [br, bg, bb] = backgroundValues;

    // Calculate the blend
    let rFinal = a * r + (1 - a) * br;
    let gFinal = a * g + (1 - a) * bg;
    let bFinal = a * b + (1 - a) * bb;

    // Round the values and return the rgb string
    rFinal = Math.round(rFinal);
    gFinal = Math.round(gFinal);
    bFinal = Math.round(bFinal);

    return `rgb(${rFinal}, ${gFinal}, ${bFinal})`;
}


function updateBackgroundColorGradient(col1, col2) {
    var rgbCol1 = hexToRgbA(col1, 1);
    var rgbCol2 = hexToRgbA(col2, 1);
    gradCol2 = col2;
    if (col1 === col2) {
        $("#networkspace").css('background', '').css('background', col1);
        $("#networkview").css('background', '').css('background', col1);
        gradCol1 = col1;
    } else {
        //$("#networkview").css('background', col1);
        $("#networkview").css('background', "linear-gradient(0deg," + rgbCol1 + " 0%," + rgbCol2 + " 100%)");
        //$("#networkspace").css('background', col1);
        $("#networkspace").css('background', "linear-gradient(0deg," + rgbCol1 + " 0%," + rgbCol2 + " 100%)");
        gradCol1 = col1;
    }

    var contrastCol = getContrastColor(col2.replace("#", ""));

    if (contrastCol === "white") {
        sigInst.settings({
            defaultLabelColor: '#fff',
            defaultLabelBGColor: '#fff'
        });
        sigInst.graph.nodes().forEach(function (n) {
            n.color = n.true_color_b;
        });

        sigInst.graph.edges().forEach(function (e) {
            e.color = "#d3d3d3"
        });


    } else {
        sigInst.settings({
            defaultLabelColor: '#000',
            defaultLabelBGColor: '#000'
        });

        sigInst.graph.nodes().forEach(function (n) {
            n.color = n.true_color_w;
        });

        sigInst.graph.edges().forEach(function (e) {
            e.color = "#808080"
        });


    }
    sigInst.refresh();
}


function setupDg2Columns(method) {
    const colWidth = 80;

    let columns = [];

    if (method === "mum") {
        columns = [[
                {field: 'ck', checkbox: true},
                {field: 'pathname', title: 'Name', width: 160, sortable: true},
                {field: 'Hits_sig', title: 'Hits', width: colWidth, sortable: true},
                {field: 'FET', title: 'FET', width: colWidth, sortable: true},
                {field: 'Gamma', title: 'Gamma', width: colWidth, sortable: true}
            ]];
    } else if (method === "gsea") {
        columns = [[
                {field: 'ck', checkbox: true},
                {field: 'pathname', title: 'Name', width: 160, sortable: true},
                {field: 'Hits', title: 'Hits', width: colWidth, sortable: true},
                {field: 'NES', title: 'NES', width: colWidth, sortable: true},
                {field: 'P_val', title: 'P-Value', width: colWidth, sortable: true}
            ]];
    } else if (method === "integ") {
        columns = [[
                {field: 'ck', checkbox: true},
                {field: 'pathname', title: 'Name', width: 160, sortable: true},
                {field: 'Sig_Hits', title: 'Hits', width: colWidth, sortable: true},
                {field: 'Combined_Pvals', title: 'Combined P-val.', width: colWidth, sortable: true}
            ]];
    } else {
        columns = [[
                {field: 'ck', checkbox: true},
                {field: 'pathname', title: 'Name', width: 160, sortable: true},
                {field: 'Hits', title: 'Hits', width: colWidth, sortable: true},
                {field: 'Raw_p', title: 'P-Value', width: colWidth, sortable: true},
                {field: 'FDR', title: 'FDR', width: colWidth, sortable: true},
            ]];
    }

    $('#dg2').datagrid({
        columns: columns
    });
}

