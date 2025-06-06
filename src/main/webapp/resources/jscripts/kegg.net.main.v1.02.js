/*
 * Javascript functions for visualization KEGG global metabolic network
 * 2014-9-21
 * Jeff Xia (jeff.xia@mcgill.ca)
 */

//global vars
var greyColorB = '#bfbfbf';
var greyColorW = '#888888';
var pathnameOpt = 'hide';
var cmpdnameOpt = 'show';
var genenameOpt = 'show';
var minorNodeColor = 'rgba(128,128,128,0.001)'; //hide the gene node
//var highlightColor = '#bb3300';
var highlightColor = '#FFFF00';
var backgroundColor = "#222222";
var transparentColor = 'rgba(0, 0, 0, 0)';
var up_color = "#FF0000";
var white_color = "#ffffff";
var down_color = "#008000";
var style_mode = "kegg";
var view_mode = "compound";
var node_rows = [];
var usr_dir;
var sigInst;
var settings = {
    edgeLabelSize: 'fixed',
    edgeLabelThreshold: 1
};

var hidden_mode = false;
var dim_mode = false;
var highlight_mode = false;
var mouseover_mode = false; // hide remote nodes
var dragListener;
var minEdgeSize = 0.1;
var defaultEdgeSize = 0.4;
var mediumEdgeSize = 0.3;
var thickEdgeSize = 0.5;
var focus_fun_anot_kos;
var focus_fun_anot_edges;
var current_fun_kos;
var current_fun_edges;
var start_x;
var start_y;
var dependants;
var cmpd_exp = {};
var path_nms = [];
var hit_all = [];
var hit_sig = [];
var alg = "";

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

function initKeggNet() {
    initFunctions();
    $.ajax({
        beforeSend: function () {
            $.messager.progress({
                text: 'Retrieving information .....'
            });
        },
        dataType: "html",
        type: "GET",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: 'function=getmummi',
        async: false,
        cache: false,
        success: function (result) {

            $.messager.progress('close');
            var job_info = result.split("||");

            usr_dir = job_info[0];
            var type = job_info[1];
            if (type === "[mum]") {
                alg = "mummichog";
                loadCompoundEnrichTable("mummichog_query_mummichog.json");
            } else if (type === "[gsea]") {
                alg = "gsea";
                loadGseaCompoundEnrichTable("mummichog_query_gsea.json");
            } else {
                alg = "integ";
                loadIntegCompoundEnrichTable("mummichog_query_integ.json");
            }

            setupKeggNet();
            //perform default enrichment analysis
            $("#spinner").fadeOut("slow");

            if ($(parent.window.document).find("#sidebar-form\\:m_report")) {
                setTimeout(function () {
                    checkSavedState("network_" + alg + ".json");
                }, 500);
                initReportFunctions();
            }
        },
        error: function () {
            $.messager.alert('Error', 'Failed to process the request! Try to clear Browser History and perform analysis again.', 'error');
            $.messager.progress('close');
        }
    });
}

//construct dataSet name to sig.genes map
//load combo box for dataset names
var query_stat = {};

function setupKeggNet() {
    $.messager.progress({
        text: 'Processing .....'
    });

    sigInst.graph.clear();

    //we only have one global map
    var net_path = '/MetaboAnalyst/resources/libs/network/ko01100_map_2023.json';
    $.getJSON(net_path, function (data) {
        for (var i = 0; i < data.nodes.pathways.length; i++) {
            var nd = data.nodes.pathways[i];
            nd.x = parseInt(nd.x);
            nd.y = parseInt(nd.y);
            nd.size = 0.1;
            nd.color = minorNodeColor;
            nd.type = 'label.anchor';
            //keep a copy
            nd.attr = {x: nd.x, y: nd.y, size: nd.size, color: nd.color, type: 'label.anchor'};
            sigInst.graph.addNode(nd);
        }

        for (var i = 0; i < data.nodes.compounds.length; i++) {
            var nd = data.nodes.compounds[i];
            nd.x = parseInt(nd.x);
            nd.y = parseInt(nd.y);
            if (nd.type === "circle") {
                nd.size = 1.5;
                nd.type = 'circle';
                nd.graph_type = 'compound';
            } else {
                nd.size = 0;
                nd.type = 'line';
                nd.graph_type = 'fake_compound';
            }


            //keep a copy
            nd.attr = {x: nd.x, y: nd.y, size: nd.size, color: nd.color, cid: nd.label, type: nd.graph_type};
            if (nd.hasOwnProperty('name')) {
                nd.label = nd.name; //for display, KEGG CID is now in attr cid
            }
            sigInst.graph.addNode(nd);
        }

        //need to add a fake node for scale properly
        var fake_nd = {id: 'mynode', x: 0, y: 0, size: 10, color: transparentColor,
            attr: {x: 0, y: 0, size: 10, color: transparentColor}};
        sigInst.graph.addNode(fake_nd);

        //do edges
        for (var j = 0; j < data.edges.length; j++) {
            var eg = data.edges[j];
            eg.size = 0;
            var lvl = 0;
            if (query_stat[eg.id]) {
                lvl = query_stat[eg.id];
            }
            eg.attr = {color: eg.color, expr: lvl};
            sigInst.graph.addEdge(eg);
        }

        if (style_mode !== "kegg") {
            setupStyleMode();
        }
        sigInst.refresh();

        sigma.misc.animation.camera(sigInst.camera, {
            x: 0,
            y: 0,
            ratio: 1.0
        }, {duration: 250});

        //need to update the dg2 table, if selected, need to unselect
        var rows = $('#dg2').datagrid('getSelections');
        for (var i = 0; i < rows.length; i++) {
            updateCellColor("#ffffff", "function_" + rows[i].ID);
        }

        $('#dg2').datagrid('clearSelections');
        $('#dg2').datagrid('clearChecked');
        $.messager.progress('close');
    });
}

function setupStyleMode() {
    if (style_mode === "kegg") {
        sigInst.graph.nodes().forEach(function (n) {
            if (n.id === 'mynode') {
                n.color = transparentColor;
            } else {
                n.x = n.attr.x;
                n.y = n.attr.y;
                n.color = n.attr.color;
            }
            n.hidden = false;
            n.highlight = false;
        });
        sigInst.graph.edges().forEach(function (e) {
            e.color = e.attr.color;
        });
    } else if (style_mode === "expr") {
        sigInst.graph.nodes().forEach(function (n) {
            if (n.attr.type === 'compound') {
                if (n.attr.cid in cmpd_exp) {
                    n.size = 4;
                    if (cmpd_exp[n.attr.cid] > 0) {
                        n.color = "#FF0000";
                    } else {
                        n.color = "#00FF00";
                    }
                }
            }
        });
    } else if (style_mode === "plain") { //plain mode
        var mycol = greyColorW;
        if (backgroundColor === "#222222") {
            mycol = greyColorB;
        }
        sigInst.graph.nodes().forEach(function (n) {
            if (n.id === 'mynode') {
                n.color = transparentColor;
            } else {
                if (!n.highlight) {
                    n.color = mycol;
                }
            }
        });
        sigInst.graph.edges().forEach(function (e) {
            if (!e.highlight) {
                e.color = mycol;
            }
        });
    } else {
        resetNetwork();
    }
    sigInst.refresh();
}

function setupViewMode() {
    sigInst.settings({
        minNodeSize: 0.1,
        maxNodeSize: 10
    });
    if (view_mode === "gene") {
        sigInst.graph.nodes().forEach(function (n) {
            if (n.id === 'mynode') {
                n.size = 10;
            } else {
                if (n.attr.type === 'compound') {
                    n.size = 0.1;
                } else {
                    n.size = 1.5;
                }
            }
        });
    } else { //compound mode
        sigInst.graph.nodes().forEach(function (n) {
            if (n.id === 'mynode') {
                n.size = 10;
            } else {
                if (n.attr.type === 'compound') {
                    n.size = 1.5;
                } else {
                    n.size = 0.1;
                }
            }
        });
    }
    sigInst.refresh();
}

var currentEnrichFile = "";

function loadCompoundEnrichTable(result) {
    $.getJSON(usr_dir + '/' + result, function (raw_data) {
        //currentEnrichFile = result.substring(0, result.length - 5);
        //focus_fun_anot_kos = raw_data['hits.query'];
        //focus_fun_anot_edges = raw_data['hits.edge'];
        //var fun_hit = raw_data['hit.num'];
        //var fun_pval = raw_data['fun.pval'];
        //var path_id = raw_data['path.id'];
        cmpd_exp = raw_data['cmpd.exp'];
        path_nms = raw_data['path.nms'];

        //sometimes path_nms in wrong format
        if (path_nms instanceof Object) {
            path_nms = Object.values(path_nms);
        }
        hit_all = raw_data['hits.all'];
        var hit_all_size = raw_data['hits.all.size'];
        hit_sig = raw_data['hits.sig'];
        var hit_sig_size = raw_data['hits.sig.size'];
        var fisher_p = raw_data['fisher.p'];
        var data_grid = $('#dg2');
        //empty if there is any
        data_grid.datagrid('loadData', {
            "total": 0,
            "rows": []
        });
        var mdl_rows = [];
        var idx = 0;
        $.each(path_nms, function (k, v) {
            mdl_rows.push({
                ID: idx,
                pathname: v,
                hit: hit_all_size[idx],
                sig: hit_sig_size[idx],
                pval: fisher_p[idx],
                colorOnly: "#ffffff",
                color: '<span style=\"background-color:#ffffff\" id=\"function_' + idx + '\">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp</span>'
            });
            idx = idx + 1;
        });

        data_grid.datagrid({
            onSelect: function (index, row) {
                console.log(path_nms)
                var hit_inx = path_nms.indexOf(row.pathname);
                var key = Object.keys(hit_all)[hit_inx];
                current_fun_kos = hit_all[key];
                sig_nodes = hit_sig[key];
                highlightFunEnrichNodes(current_fun_kos, sig_nodes);
                updateCellColor(highlightColor, "function_" + row.ID);
                displayPathInfo(row.pathname, current_fun_kos, sig_nodes);
                row.colorOnly = highlightColor;

            },
            onUnselect: function (index, row) {
                var hit_inx = path_nms.indexOf(row.pathname);
                var key = Object.keys(hit_all)[hit_inx];
                current_fun_kos = hit_all[key];
                unHighlightFunEnrichNodes(current_fun_kos, row.pathname);
                updateCellColor("#ffffff", "function_" + row.ID);
                row.colorOnly = "#ffffff";
                var stats = $("#stats");
                stats.empty();
            }
        }).datagrid('loadData', mdl_rows);
    });
}

function loadIntegCompoundEnrichTable(result) {
    $.getJSON(usr_dir + '/' + result, function (raw_data) {
        //currentEnrichFile = result.substring(0, result.length - 5);
        //focus_fun_anot_kos = raw_data['hits.query'];
        //focus_fun_anot_edges = raw_data['hits.edge'];
        //var fun_hit = raw_data['hit.num'];
        //var fun_pval = raw_data['fun.pval'];
        //var path_id = raw_data['path.id'];
        $('#dg2').datagrid({
            columns: [[
                    {field: 'ck', checkbox: true},
                    {field: 'pathname', title: 'Name', width: 100},
                    {field: 'hit', title: 'Hits', width: 40},
                    {field: 'mpval', title: 'M.P-val', width: 40},
                    {field: 'gpval', title: 'G.P-val', width: 40},
                    {field: 'pval', title: 'Comb.P-val', width: 60},
                    {field: 'color', title: 'Color', width: 60}
                ]]
        });
        //var nes_vals = raw_data['nes'];
        cmpd_exp = raw_data['cmpd.exp'];
        var path_nms_obj = raw_data['path.nms'];
        hit_all = raw_data['hits.all'];

        path_nms = [];
        for (var key in path_nms_obj) {
            if (path_nms_obj.hasOwnProperty(key)) {
                path_nms.push(path_nms_obj[key]);
            }
        }
        ;

        var data_grid = $('#dg2');
        //empty if there is any
        data_grid.datagrid('loadData', {
            "total": 0,
            "rows": []
        });
        var mdl_rows = [];
        var idx = 0;
        $.each(path_nms, function (k, v) {
            mdl_rows.push({
                ID: idx,
                pathname: v,
                hit: raw_data['hits.all.size'][idx],
                mpval: raw_data['mum.p'][idx],
                gpval: raw_data['gsea.p'][idx],
                pval: raw_data['comb.p'][idx],
                colorOnly: "#ffffff",

                color: '<span style=\"background-color:#ffffff\" id=\"function_' + idx + '\">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp</span>'
            });
            idx = idx + 1;
        });

        data_grid.datagrid({
            onSelect: function (index, row) {
                var hit_inx = path_nms.indexOf(row.pathname);
                current_fun_kos = hit_all[hit_inx];
                sig_nodes = hit_all[hit_inx];
                highlightFunEnrichNodes(current_fun_kos, sig_nodes);
                updateCellColor(highlightColor, "function_" + row.ID);
                displayPathInfo(row.pathname, current_fun_kos, sig_nodes);
                row.colorOnly = highlightColor;
            },
            onUnselect: function (index, row) {
                var hit_inx = path_nms.indexOf(row.pathname);
                current_fun_kos = hit_all[hit_inx];
                unHighlightFunEnrichNodes(current_fun_kos, row.pathname);
                updateCellColor("#ffffff", "function_" + row.ID);
                var stats = $("#stats");
                stats.empty();
                row.colorOnly = "#ffffff";
            }
        }).datagrid('loadData', mdl_rows);
    });
}

function loadGseaCompoundEnrichTable(result) {
    $.getJSON(usr_dir + '/' + result, function (raw_data) {
        //currentEnrichFile = result.substring(0, result.length - 5);
        //focus_fun_anot_kos = raw_data['hits.query'];
        //focus_fun_anot_edges = raw_data['hits.edge'];
        //var fun_hit = raw_data['hit.num'];
        //var fun_pval = raw_data['fun.pval'];
        //var path_id = raw_data['path.id'];
        $('#dg2').datagrid({
            columns: [[
                    {field: 'ck', checkbox: true},
                    {field: 'pathname', title: 'Name', width: 150},
                    {field: 'hit', title: 'Hits', width: 40},
                    {field: 'pval', title: 'P-value', width: 40},
                    {field: 'nes', title: 'NES', width: 60},
                    {field: 'color', title: 'Color', width: 60}
                ]]
        });
        var nes_vals = raw_data['nes'];
        cmpd_exp = raw_data['cmpd.exp'];
        path_nms = raw_data['path.nms'];
        hit_all = raw_data['hits.all'];

        //sometimes path_nms in wrong format
        if (path_nms instanceof Object) {
            path_nms = Object.values(path_nms);
        }
        var hit_all_size = raw_data['hits.all.size'];
        var fisher_p = raw_data['fisher.p'];
        var data_grid = $('#dg2');
        //empty if there is any
        data_grid.datagrid('loadData', {
            "total": 0,
            "rows": []
        });
        var mdl_rows = [];
        var idx = 0;
        $.each(path_nms, function (k, v) {
            mdl_rows.push({
                ID: idx,
                pathname: v,
                hit: hit_all_size[idx],
                pval: fisher_p[idx],
                nes: nes_vals[idx],
                colorOnly: "#ffffff",
                color: '<span style=\"background-color:#ffffff\" id=\"function_' + idx + '\">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp</span>'
            });
            idx = idx + 1;
        });

        data_grid.datagrid({
            onSelect: function (index, row) {
                var hit_inx = path_nms.indexOf(row.pathname);
                current_fun_kos = hit_all[hit_inx];
                sig_nodes = hit_all[hit_inx];
                highlightFunEnrichNodes(current_fun_kos, sig_nodes);
                updateCellColor(highlightColor, "function_" + row.ID);
                displayPathInfo2(row.pathname, current_fun_kos, sig_nodes);
                row.colorOnly = highlightColor;
            },
            onUnselect: function (index, row) {
                var hit_inx = path_nms.indexOf(row.pathname);
                current_fun_kos = hit_all[hit_inx];
                unHighlightFunEnrichNodes(current_fun_kos, row.pathname);
                updateCellColor("#ffffff", "function_" + row.ID);
                var stats = $("#stats");
                stats.empty();
                row.colorOnly = "#ffffff";
            }
        }).datagrid('loadData', mdl_rows);
    });
}

function highlightFunEnrichNodes(nodes, signodes) {
    sigInst.graph.nodes().forEach(function (n) {
        if (n.attr.type === 'compound') {
            if (nodes.indexOf(n.attr.cid) !== -1) {
                n.size = 5;
                n.borderColor = highlightColor;
                n.highlight = true;
                n.type = "circle";
                if (signodes.indexOf(n.attr.cid) !== -1) {
                    n.color = highlightColor;
                    n.size = n.size + 1;
                }
            }
        }
    });
    sigInst.refresh();
}

function getBorderColor() {
    if (backgroundColor === "#222222") {
        return("#FAFAD2");
    } else {
        return("#A0522D");
    }
}

function unHighlightFunEnrichNodes(nodes, title) {
    if (style_mode === "kegg") {
        sigInst.graph.nodes().forEach(function (n) {
            if (n.attr.type === 'compound') {
                if (nodes.indexOf(n.attr.cid) !== -1) {
                    n.size = 1.5;
                    n.color = n.attr.color;
                    n.borderColor = null;
                    n.highlight = false;
                }
            }
        });
    } else if (style_mode === "expr") {
        if (backgroundColor === "#222222") {
            sigInst.graph.nodes().forEach(function (n) {
                if (n.attr.type === 'compound') {
                    if (nodes.indexOf(n.id) !== -1) {
                        n.size = n.true_size;
                        n.color = n.expr_colorb;
                        n.highlight = 0;
                    }
                }
            });
        } else {
            sigInst.graph.nodes().forEach(function (n) {
                if (n.attr.type === 'compound') {
                    if (nodes.indexOf(n.id) !== -1) {
                        n.size = n.true_size;
                        n.color = n.expr_colorw;
                        n.highlight = 0;
                    }
                }
            });
        }
    } else { //plain mode
        sigInst.graph.nodes().forEach(function (n) {
            if (n.attr.type === 'compound') {
                if (nodes.indexOf(n.id) !== -1) {
                    n.size = n.true_size;
                    n.color = greyColorB;
                    n.highlight = 0;
                }
            }
        });
    }
    sigInst.refresh();
}

//when user resize the network, trigger this function
function resizeNetwork() {
    //dynamic update centerpane width based on user browser
    $('#network-parent').css("width", $(window).width() - 350);
    $('#network-parent').css("height", $(window).height() - 50);
    if (sigInst !== undefined) {
        sigInst.refresh();
    }
}

//initiate variables and attach functions
function initFunctions() {
    //dynamic update centerpane width based on user browser
    $('#network-parent').css("width", $(window).width() - 350);
    $('#network-parent').css("height", $(window).height() - 50);
    sigInst = new sigma({
        renderers: [
            {
                container: document.getElementById('networkview'),
                type: 'canvas' // sigma.renderers.canvas works as well
            }
        ],
        settings: {
            defaultLabelColor: '#fff',
            defaultLabelSize: 11,
            defaultLabelBGColor: '#fff',
            defaultLabelHoverColor: '#000',
            labelThreshold: 4,
            defaultEdgeLabelColor: '#fff',
            defaultEdgeLabelSize: 10,
            edgeLabelThreshold: 1.4,
            enableEdgeHovering: true,
            edgeHoverColor: 'edge',
            edgeHoverSizeRatio: 1.1,
            edgeHoverExtremities: true,
            defaultEdgeColor: 'default',
            doubleClickEnabled: false,
            minNodeSize: 0.01,
            maxNodeSize: 10,
            sideMargin: 1,
            minEdgeSize: 0.5,
            maxEdgeSize: 1,
            defaultNodeBorder: 0
        }
    });

    sigInst.bind('overNode', function (event) {
        if (hidden_mode) {
            return false;
        }
        if (mouseover_mode) {
            var node = event.data.node;
            var neighbors = {};
            neighbors[node.id] = 1;
            sigInst.graph.edges().forEach(function (e) {
                if (node.id === e.source || node.id === e.target) {
                    neighbors[e.source] = 1;
                    neighbors[e.target] = 1;
                }
            });
            sigInst.graph.nodes().forEach(function (n) {
                if (!neighbors[n.id]) {
                    n.hidden = true;
                } else {
                    n.hidden = false;
                }
            });
            sigInst.refresh();
        }
    });

    sigInst.bind('outNode', function () {
        if (hidden_mode) {
            return false;
        }
        if (mouseover_mode) {
            sigInst.graph.edges().forEach(function (e) {
                e.hidden = false;
            });
            sigInst.graph.nodes().forEach(function (n) {
                n.hidden = false;
            });
            sigInst.refresh();
        }
    });

    sigInst.bind('doubleClickNode', function (e) {

        if (e.data.node.attr.cid in cmpd_exp) {
            var res = $.ajax({
                type: "GET",
                url: '/MetaboAnalyst/faces/AjaxCall?function=getcmpdinfo' + "&cid=" + e.data.node.attr.cid,
                async: false
            }).responseText;
            $.messager.alert(e.data.node.label, res);
        } else {
            $.messager.alert(e.data.node.label, "No match detected for this compound");
        }
    });


    sigInst.bind('doubleClickEdge', function (e) {
        var myE = e.data.edge;
        var srcLbl, tgtLbl;
        sigInst.graph.nodes().forEach(function (n) {
            if (n.id === myE.source) {
                srcLbl = n.label;
            } else if (n.id === myE.target) {
                tgtLbl = n.label;
            }
        });
        $.messager.alert(myE.label, "Reaction: " + srcLbl + " => " + tgtLbl);
    });


    //add action listeners for navigation buttons
    var moveDelay = 50;
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
    $('#autofitBn').bind('click keypress', function (event) {
        var c = sigInst.camera;
        sigma.misc.animation.camera(c, {
            x: 0,
            y: 0,
            ratio: 1.0
        },
                {duration: 200});
    });
    $('#resetBn').bind('click keypress', function (event) {
        resetNetwork();
        event.stopPropagation();
        return false;
    });

    $("#custom").spectrum({
        color: "#FFFF00",
        showInitial: true,
        change: function (color) {
            highlightColor = color.toHexString();
        },
        showInput: true,
        showPalette: true,
        showSelectionPalette: true, // true by default
        palette: [
            ['black', 'white'],
            ['#FFFF00', '#bb3300']
        ]
    });

    $('#viewOpt').change(function () {
        view_mode = $('#viewOpt').val();
        setupViewMode();
    });

    $('#highlightOpt').change(function () {
        style_mode = $('#highlightOpt').val();
        setupStyleMode();
    });

    $('#pathnameOpt').change(function () {
        pathnameOpt = $('#pathnameOpt').val();
        sigInst.refresh();
    });

    $('#cmpdnameOpt').change(function () {
        cmpdnameOpt = $('#cmpdnameOpt').val();
        sigInst.refresh();
    });

    $('#exportOpt').change(function () {
        var type = $('#exportOpt').val();
        if (type === 'NA')
            return;
        if (type === "png") {
            document.getElementById("downloadimage").src = export2Image();
            $("#pngdialog").dialog('open');
        } else if (type === "svg") {
            sigInst.toSVG({download: true, filename: 'network_nolabel.svg', labels: false, background: backgroundColor, width: 1500, hieght: 1200});
        } else if (type === "svglbl") {
            sigInst.toSVG({download: true, filename: 'network_labeled.svg', labels: true, background: backgroundColor});
        } else if (type === "node") {
            exportResultTable('ndtb');
        } else if (type === "func") {
            exportResultTable('funtb');
        } else {
            doGraphExport(function (result) {
                var fileLnk = $("#fileLnk");
                fileLnk.empty();
                fileLnk.append("Right click the link below, then 'Save Link As ... ' to download the file<br/><br/>");
                fileLnk.append('<strong><a href="' + usr_dir + '/' + result + '" target="_blank"><u>' + result + '</u></a></strong>');
                $.messager.progress('close');
                $("#filedialog").dialog('open');
            }, type);
        }
    });

    $('#backgroundOpt').change(function () {
        updateBackground();
    });
}

function updateBackground() {
    backgroundColor = $('#backgroundOpt').val();
    $("#networkspace").css('background-color', '').css('background-color', backgroundColor);
    if (backgroundColor === "#222222") {
        sigInst.settings({
            defaultLabelColor: '#fff',
            defaultLabelBGColor: '#fff',
            defaultEdgeLabelColor: '#fff',
            defaultEdgeLabelBGColor: '#fff'
        });
    } else {
        sigInst.settings({
            defaultLabelColor: '#000',
            defaultLabelBGColor: '#000',
            defaultEdgeLabelColor: '#000',
            defaultEdgeLabelBGColor: '#000'
        });
    }
    sigInst.graph.nodes().forEach(function (n) {
        if (n.id.indexOf('mynode') !== -1) {
            n.color = transparentColor;
        }
    });
    sigInst.refresh();
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

function resetNodes(nodeIDs) {
    sigInst.graph.nodes().forEach(function (n) {
        if (nodeIDs.indexOf(n.id) !== -1) {
            if (n.highlight) {
                n.highlight = 0;
                n.borderColor = null;
                n.size = n.true_size;
                if (backgroundColor === "#222222") {
                    n.color = n.colorb;
                } else {
                    n.color = n.colorw;
                }
                n.hidden = false;
            }
        }
    });
    sigInst.refresh();
}

function updateHighlightColor() {
    var type = $('#visOpt').val();
    if (type === "dim") {
        hidden_mode = false;
        dim_mode = true;
        sigInst.graph.edges().forEach(function (e) {
            e.color = greyColorB;
        });
        sigInst.graph.nodes().forEach(function (n) {
            n.hidden = false;
            if (!n.highlight) {
                n.color = greyColorB;
            }
        });
    } else if (type === "hide") {
        hidden_mode = true;
        sigInst.graph.nodes().forEach(function (n) {
            if (!n.highlight) {
                n.hidden = true;
            }
        });
    } else {
        hidden_mode = false;
        dim_mode = false;
        sigInst.graph.nodes().forEach(function (n) {
            if (!n.highlight) {
                n.hidden = false;
                n.size = n.true_size;
                if (backgroundColor === "#222222") {
                    n.color = n.colorb;
                } else {
                    n.color = n.colorw;
                }
            }
        });
    }
    sigInst.refresh();
}

function searchNetwork(nodeID) {
    $.messager.progress();
    var hit = 0;
    //centering 
    //sigInst.position(0, 0, 1).draw(2, 2, 2);
    //then Loop all nodes
    sigInst.graph.nodes().forEach(function (n) {
        if (n.id === nodeID) {
            hit = 1;
            n.size = 8;
            n.borderColor = "#FFFF00";
            n.highlight = 1;
            sigma.misc.animation.camera(
                    sigInst.camera,
                    {
                        x: n[sigInst.camera.readPrefix + 'x'],
                        y: n[sigInst.camera.readPrefix + 'y'],
                        ratio: 0.3
                    },
                    {duration: 300});
        }
    });
    sigInst.refresh();
    if (hit) {
        $.messager.progress('close');
    } else {
        $.messager.alert('Error', "Node " + search + " was not found in the current network!", 'error');
        $.messager.progress('close');
    }
    return;
}

function resetNetwork() {
    sigInst.settings({
        minEdgeSize: defaultEdgeSize,
        maxEdgeSize: defaultEdgeSize
    });

    //reset to kegg view
    style_mode = "kegg";
    $('#highlightOpt').val(style_mode);

    //reset background to black
    backgroundColor = "#222222";
    $('#backgroundOpt').val(backgroundColor);
    $("#networkspace").css('background-color', '').css('background-color', backgroundColor);

    sigInst.graph.nodes().forEach(function (n) {
        n.size = n.attr.size;
        n.color = n.attr.color;
        n.x = n.attr.x;
        n.y = n.attr.y;
        n.borderColor = null;
        n.hidden = false;
        n.highlight = false;
    });

    sigInst.graph.edges().forEach(function (e) {
        e.color = e.attr.color;
        e.size = defaultEdgeSize;
        e.highlight = false;
        e.hidden = false;
    });

    sigInst.camera.goTo({x: 0, y: 0, angle: 0, ratio: 1.0});
    sigInst.refresh();

    highlight_mode = 0;
    hidden_mode = false;

    var dg2 = $('#dg2');
    var col2 = dg2.datagrid('getColumnOption', 'color');
    col2.styler = function () {
        return 'background-color:white';
    };
}

function highlightRegularNodes(nodes) {
    if (nodes.length === 1) {
        searchNetwork(nodes[0]);
    } else {
        var nodeVec = [];
        highlight_mode = 1;

        sigInst.graph.edges().forEach(function (e) {
            e.color = greyColorB;
        });
        sigInst.graph.nodes().forEach(function (n) {
            if (nodes.indexOf(n.id) !== -1) {
                n.size = n.size + 1;
                n.color = highlightColor;
                n.highlight = 1;
                nodeVec.push({id: n.id, label: n.label});
            } else {
                if (!n.highlight) {
                    n.color = greyColorB;
                }
            }
        });
        sigInst.refresh();
        displayCurrentSelectedNodes(nodeVec, "");
    }
}

function updateNodeSize() {
    var type = $('#nodeOpt').val();
    var val = $('#sizeOpt').val();
    if (val === 'increase') {
        if (type === "all") {
            sigInst.graph.nodes().forEach(function (n) {
                n.size = n.size + 2;
            });
        } else {
            sigInst.graph.nodes().forEach(function (n) {
                if (n.highlight) {
                    n.size = n.size + 2;
                }
            });
        }
    } else {
        if (type === 'all') {
            sigInst.graph.nodes().forEach(function (n) {
                if (n.size > 2) {
                    n.size = n.size - 1;
                }
            });
        } else {
            sigInst.graph.nodes().forEach(function (n) {
                if (n.highlight) {
                    if (n.size > 2) {
                        n.size = n.size - 1;
                    }
                }
            });
        }
    }
    sigInst.refresh();
}

function updateEdgeSize() {
    var type = $('#shapeOpt').val();
    var val = $('#widthOpt').val();
    var w = defaultEdgeSize;
    if (val === "thin") {
        w = minEdgeSize;
    } else if (val === "medium") {
        w = mediumEdgeSize;
    } else if (val === "thick") {
        w = thickEdgeSize;
    }
    sigInst.settings({
        minEdgeSize: w,
        maxEdgeSize: w
    });

    sigInst.graph.edges().forEach(
            function (e) {
                e.type = type;
            });
    sigInst.refresh();
}

function updateCellColor(color, id) {
    $("#" + id).css("background-color", color);
}

function setupFileDownload(result) {
    var fileLnk = $("#fileLnk");
    fileLnk.empty();
    fileLnk.append("Right click the link below, then 'Save Link As ... ' to download the file<br/><br/>");
    fileLnk.append('<strong><a href="' + usr_dir + '/' + result + '" target="_blank"><u>' + result + '</u></a></strong>');
    $.messager.progress('close');
    $("#filedialog").dialog('open');
}

function export2Image(quality = 1) {
    // Retrieving a dataUrl of the rendered graph
    var dataUrl = sigInst.renderers[0].snapshot({format: 'png', background: backgroundColor, filename: 'network-graph.png', quality: quality});
    return dataUrl;
}

//display KO members for a given pathway
function displayPathInfo(pathNm, all_nodes, sig_nodes) {
    var stats = $("#stats");
    stats.empty();

    if (pathNm !== '') {
        stats.append('<strong>' + pathNm + '</strong><br/> (signficant hits in red)');
        var koLine = '<ul>';
        var nd;
        for (var i = 0; i < all_nodes.length; i++) {
            nd = all_nodes[i];
            if (sig_nodes.indexOf(nd) !== -1) {
                koLine = koLine + '<li><a style="color:red" href="http://www.genome.jp/dbget-bin/www_bget?' + nd + '" target="_blank"><u>' + nd + '</u></a></li>';
            } else {
                koLine = koLine + '<li><a href="http://www.genome.jp/dbget-bin/www_bget?' + nd + '" target="_blank"><u>' + nd + '</u></a></li>';
            }
        }
        koLine = koLine + '</ul>';
        stats.append(koLine);
    }
}

//display KO members for a given pathway --> GSEA ONLY
function displayPathInfo2(pathNm, all_nodes, sig_nodes) {
    var stats = $("#stats");
    stats.empty();

    if (pathNm !== '') {
        stats.append('<strong>' + pathNm + '</strong><br/> (Compound hits in red)');
        var koLine = '<ul>';
        var nd;
        for (var i = 0; i < all_nodes.length; i++) {
            nd = all_nodes[i];
            if (sig_nodes.indexOf(nd) !== -1) {
                koLine = koLine + '<li><a style="color:red" href="http://www.genome.jp/dbget-bin/www_bget?' + nd + '" target="_blank"><u>' + nd + '</u></a></li>';
            } else {
                koLine = koLine + '<li><a href="http://www.genome.jp/dbget-bin/www_bget?' + nd + '" target="_blank"><u>' + nd + '</u></a></li>';
            }
        }
        koLine = koLine + '</ul>';
        stats.append(koLine);
    }
}