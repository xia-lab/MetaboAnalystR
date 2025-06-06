/*
 * Javascript functions for MicrobiomeAnalyst
 * 2014-9-21
 * Jeff Xia (jeff.xia@mcgill.ca)
 */

//global vars
var notFoundElements = [];
var min_x = 25;
var max_y = 3202;
var rowSize = 50;
var unmatched_obj = {};
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
var unmatched_nodes = [];
var unmatched_edges = [];
var settings = {
    edgeLabelSize: 'fixed',
    edgeLabelThreshold: 1
};
var hidden_mode = false;
var dim_mode = false;
var highlight_mode = false;
var mouseover_mode = false; // hide remote nodes
var dragListener;
var minEdgeSize = 0.01;
var defaultEdgeSize = 0.5;
var mediumEdgeSize = 0.3;
var thickEdgeSize = 0.5;
var focus_fun_anot_kos;
var focus_fun_anot_nodes;
var focus_fun_anot_edges;
var all_fun_anot_kos;
var all_fun_anot_nodes;
var all_fun_anot_edges;
var expr_vals;
var current_fun_kos;
var current_fun_edges;
var current_fun_nodes;
var start_x;
var start_y;
var dependants;
var currentEnrichFile = "network_enrichment_pathway_0";

function initKEGGViewer() {
    initFunctions();
    $.ajax({
        type: "GET",
        url: '/MetaboAnalyst/faces/AjaxCall?function=getusrdir&ignoreMe=' + new Date().getTime(),
        async: false,
        success: function (data) {
            usr_dir = data; // Use the response data here
            console.log(usr_dir); // Check what you received
        },
        error: function (xhr, status, error) {
            console.error("Error occurred: " + error);
        }
    });
    //console.log(usr_dir)
    loadQueryData();
    loadEnrichTable("network_enrichment_pathway_0.json");
    setupKEGGGlobalMap();
    if ($(parent.window.document).find("#sidebar-form\\:m_report")) {
        parent.PF("statusDialog").show();
        setTimeout(function () {
            sendImageToServer(export2Image(0.7), "network_MetaboNet");
            parent.PF("statusDialog").hide();
        }, 1000);
        setTimeout(function () {
            checkSavedState();
        }, 500);
        initReportFunctions();
    }
    //perform default enrichment analysis
    $("#spinner").fadeOut("slow");
}


//construct dataSet name to sig.genes map
//load combo box for dataset names
var query_stat = {};
function loadQueryData() {
//clear it first
    $.getJSON(usr_dir + '/network_query.json', function (rawdata) {
        //note, we only have one object element
        query_stat = {}; //contain counts all unique genes
        $.each(rawdata, function (k, val) {
            query_stat[k] = val;
        });
    });
}

function setupKEGGGlobalMap() {
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
            nd.type = 'label.anchor';
            nd.size = 0.1;
            nd.color = minorNodeColor;
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
                nd.type = 'circle';
                nd.graph_type = 'fake_compound';
            }
            //keep a copy
            nd.attr = {x: nd.x, y: nd.y, size: nd.size, color: nd.color, cid: nd.label, type: nd.graph_type, opacity: 1};
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
            eg.attr = {color: eg.color, expr: lvl, size: eg.size};
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

        //   $('#dg2').datagrid('clearSelections');
        //   $('#dg2').datagrid('clearChecked');
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
    } else if (style_mode === "queries") {
        var alledges = [];
        for (var pathway in all_fun_anot_edges) {
            var items = all_fun_anot_edges[pathway];
            if (typeof items !== 'undefined') {
                if (Array.isArray(items)) {
                    if (items.length !== 0) {
                        for (var i = 0; i < items.length; i++) {
                            if (alledges.indexOf(items[i]) === -1) {
                                alledges.push(items[i]);
                            }
                        }
                    }
                } else {
                    if (alledges.indexOf(items) === -1) {
                        alledges.push(items);
                    }
                }
            }
        }

        if (alledges.length !== 0) {
            highlightFunEnrichEdges(alledges, "Query list");
        }

        var allnodes = [];
        for (var pathway in all_fun_anot_nodes) {
            var items = all_fun_anot_nodes[pathway];
            if (typeof items !== 'undefined') {
                if (Array.isArray(items)) {
                    if (items.length !== 0) {
                        for (var i = 0; i < items.length; i++) {
                            if (allnodes.indexOf(items[i]) === -1) {
                                allnodes.push(items[i]);
                            }
                        }
                    }
                } else {
                    if (allnodes.indexOf(items) === -1) {
                        allnodes.push(items);
                    }
                }
            }
        }
        if (allnodes.length !== 0) {
            highlightFunEnrichNodes(allnodes, "NoDisp");
        }

    } else if (style_mode === "expr") {
        sigInst.graph.nodes().forEach(function (n) {
            if (n.attr.type === 'compound') {
                if (n.attr.cid in expr_vals) {
                    n.size = 4;
                    if (expr_vals[n.attr.cid] > 0) {
                        n.color = "#FF0000";
                    } else {
                        n.color = "#00FF00";
                    }
                }
            }
        });
        sigInst.graph.edges().forEach(function (e) {
            if (e.label in expr_vals) {
                e.color = highlightColor;
                e.highlight = true;
                e.size = 3 + Math.abs(e.attr.expr);
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

function highlightPathway(row, type = "single") {

    if (view_mode === 'gene') {
        current_fun_kos = focus_fun_anot_kos[row.pathname];
        if (type === "all") {
            highlightFunEnrichNodes(current_fun_kos, row.pathname, false);
        } else {
            highlightFunEnrichNodes(current_fun_kos, row.pathname);
        }
    } else {
        current_fun_edges = focus_fun_anot_edges[row.pathname];
        if (typeof current_fun_edges !== 'undefined') {
            if (current_fun_edges.length !== 0) {
                if (type === "all") {
                    highlightFunEnrichEdges(current_fun_edges, row.pathname, false);
                } else {
                    highlightFunEnrichEdges(current_fun_edges, row.pathname);
                }
            }
        }

        current_fun_nodes = focus_fun_anot_nodes[row.pathname];
        if (typeof current_fun_nodes !== 'undefined') {
            if (current_fun_nodes.length !== 0) {
                if (type === "all") {
                    highlightFunEnrichNodes(current_fun_nodes, row.pathname, false);
                } else {
                    highlightFunEnrichNodes(current_fun_nodes, row.pathname);
                }
            }
        }

    }
    sigInst.refresh();
    updateCellColor(highlightColor, "function_" + row.ID);
}

function unhighlightPathway(row, type = "single") {

    current_fun_edges = focus_fun_anot_edges[row.pathname];
    if (typeof current_fun_edges !== 'undefined') {
        if (current_fun_edges.length !== 0) {
            if (type === "all") {
                unHighlightFunEnrichEdges(current_fun_edges, row.pathname, false);
            } else {
                unHighlightFunEnrichEdges(current_fun_edges, row.pathname);
            }
            updateCellColor("#ffffff", "function_" + row.ID);
        }
    }
    current_fun_nodes = focus_fun_anot_nodes[row.pathname];
    if (typeof current_fun_nodes !== 'undefined') {
        if (current_fun_nodes.length !== 0) {
            if (type === "all") {
                unHighlightFunEnrichNodes2(current_fun_nodes, row.pathname, false);
            } else {
                unHighlightFunEnrichNodes2(current_fun_nodes, row.pathname);
            }
            updateCellColor("#ffffff", "function_" + row.ID);
        }
    }

    sigInst.refresh()


}

function loadEnrichTable(result) {
    $.getJSON(usr_dir + '/' + result, function (raw_data) {
        currentEnrichFile = result.substring(0, result.length - 5);
        focus_fun_anot_kos = raw_data['hits.query'];
        focus_fun_anot_edges = raw_data['hits.edge'];
        focus_fun_anot_nodes = raw_data['hits.node'];
        all_fun_anot_kos = raw_data['hits.all'];
        all_fun_anot_edges = raw_data['hits.edge.all'];
        all_fun_anot_nodes = raw_data['hits.node.all'];
        hits_name = raw_data['hits.name.all'];
        expr_vals = raw_data['expr.mat'];
        conv = raw_data['conv']
        var fun_hit = raw_data['hit.num'];
        var fun_pval = raw_data['fun.pval'];
        var path_id = raw_data['path.id'];
        var data_grid = $('#dg2');
        //empty if there is any
        data_grid.datagrid('loadData', {
            "total": 0,
            "rows": []
        });
        var mdl_rows = [];
        var idx = 0;
        $.each(focus_fun_anot_kos, function (k, v) {
            mdl_rows.push({
                ID: idx,
                pathid: path_id[idx],
                pathname: k,
                hit: fun_hit[idx],
                pval: fun_pval[idx],
                colorOnly: "#ffffff",
                color: '<span style=\"background-color:#ffffff\" id=\"function_' + path_id[idx] + '\">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp</span>'
            });
            idx = idx + 1;
        });
        data_grid.datagrid({
            onSelectAll: function (list) {
                var i;
                for (i = 0; i < list.length; i++) {
                    var row = list[i];
                    highlightPathway(row, "all");
                }
            },
            onUnselectAll: function (list) {
                var i;
                for (i = 0; i < list.length; i++) {
                    var row = list[i];
                    unhighlightPathway(row, "all");
                }
            },
            onSelect: function (index, row) {
                highlightPathway(row);
                loadHitsTable(row.pathname)
                row.colorOnly = highlightColor;

            },
            onUnselect: function (index, row) {
                unhighlightPathway(row)
                row.colorOnly = "#ffffff";

            }
        }).datagrid('loadData', mdl_rows);
    });
}

function adjust(color, amount) {
    return '#' + color.replace(/^#/, '').replace(/../g, color => ('0' + Math.min(255, Math.max(0, parseInt(color, 16) + amount)).toString(16)).substr(-2));
}

function highlightSelected(id) {


    var compoundCount = 0;
    var enzymeCount = 0;
    var foundNode;
    sigInst.graph.nodes().forEach(function (n) {
        if (n.attr.cid !== undefined) {
            if (n.attr.cid === id) {
                n.size = n.size + 2;
                n.color = highlightColor
                n.highlight = true;
                n.type = "circle";
                foundNode = n;
                compoundCount++;
            }
        }
    });
    if (compoundCount === 1) {
        sigma.misc.animation.camera(
                sigInst.camera,
                {
                    x: foundNode[sigInst.camera.readPrefix + 'x'],
                    y: foundNode[sigInst.camera.readPrefix + 'y'],
                    ratio: 0.35
                },
                {duration: 300});
    }
    sigInst.graph.edges().forEach(function (e) {

        if (e.label !== null && e.label.includes(id) && !e.highlight) {

            if (e.color === highlightColor) {
                e.color = adjust(highlightColor, 50)
            } else {
                e.color = highlightColor;
            }
            e.size = 5 + Math.abs(e.attr.expr);
            e.highlight = true;
            enzymeCount++
        } else if (!e.highlight) {
            e.size = defaultEdgeSize;
        }
    });

    if (id.startsWith("C")) { //compound
        if (compoundCount === 0) {
            $.messager.alert('Error', id + ' is not present within current KEGG network.', 'info');
            if (notFoundElements.indexOf(id) === -1) {
                notFoundElements.push(id);
            }
        }
    } else { //enzyme
        if (enzymeCount === 0) {
            $.messager.alert('Error', id + ' is not present within current KEGG network.', 'info');
            if (notFoundElements.indexOf(id) === -1) {
                notFoundElements.push(id);
            }
        } else {
            sigInst.settings({
                minEdgeSize: defaultEdgeSize,
                maxEdgeSize: 5
            });
            sigma.misc.animation.camera(
                    sigInst.camera,
                    {
                        x: 0,
                        y: 0,
                        ratio: 1
                    },
                    {duration: 300});
        }
    }

    sigInst.refresh();
}


function unhighlightSelected(id) {

    var no_highlight = true; // need to see if there is no highlighted nodes left

    if (notFoundElements.indexOf(id) !== -1) {
        return;
    }

    sigInst.graph.nodes().forEach(function (n) {
        if (n.attr.cid !== undefined) {
            if (n.attr.cid === id) {
                n.size = 1.5;
                n.color = n.attr.color;
                n.highlight = false;
            }
        }
    });
    var i = 0;
    sigInst.graph.edges().forEach(function (e) {

        if (e.label !== null && e.label.includes(id)) {
            e.size = defaultEdgeSize;
            e.highlight = false;
            e.color = e.attr.color;
        } else {
            if (e.highlight) {
                no_highlight = false;
            }
        }
    });

    if (no_highlight) {
        sigInst.settings({
            minEdgeSize: defaultEdgeSize,
            maxEdgeSize: defaultEdgeSize
        });
    }
    sigInst.refresh();
}

function loadHitsTable(pathname) {

    var w = $('#westlayoutid').layout('panel', 'south');   // west panel
    w.panel('setTitle', 'Hits (' + pathname + ')');

    var data_grid = $('#dg3');
    //empty if there is any
    data_grid.datagrid('loadData', {
        "total": 0,
        "rows": []
    });
    var mdl_rows = [];
    var idx = 0;

    if (typeof all_fun_anot_kos[pathname] === "string") {
        all_fun_anot_kos[pathname] = [all_fun_anot_kos[pathname]];
    }
    $.each(all_fun_anot_kos[pathname], function (k, v) {
        var id = all_fun_anot_kos[pathname][idx];
        var inx = conv["id"].indexOf(id);
        if (inx === -1 || conv["name"][inx] === "") {
            mdl_rows.push({
                id: id,
                idLink: '<a href="http://www.genome.jp/dbget-bin/www_bget?' + id + '" target="_blank"><u>' + id + '</u></a>',
                name: all_fun_anot_kos[pathname][idx],
                exp: expr_vals[id]
                        //View: '<a href="#" class="easyui-linkbutton" iconCls="icon-ok" onclick="highlightSelected(\'' + all_fun_anot_kos[pathname][idx] + '\')">OK</a>'
            });
        } else {
            mdl_rows.push({
                id: id,
                idLink: '<a href="http://www.genome.jp/dbget-bin/www_bget?' + id + '" target="_blank"><u>' + id + '</u></a>',
                name: conv["name"][inx],
                exp: expr_vals[id]
                        //View: '<a href="#" class="easyui-linkbutton" iconCls="icon-ok" onclick="highlightSelected(\'' + all_fun_anot_kos[pathname][idx] + '\')">OK</a>'
            });
        }

        idx = idx + 1;
    });


    data_grid.datagrid({
        onSelect: function (index, row) {
            highlightSelected(row.id);
        },
        onUnselect: function (index, row) {
            unhighlightSelected(row.id)
        }
        /*,
         onLoadSuccess: function (data) {
         data_grid.datagrid('getPanel').find('div.datagrid-header input[type=checkbox]').attr('disabled', 'disabled');
         }*/
    }).datagrid('loadData', mdl_rows);
    data_grid.datagrid('selectAll');

}

function testEnrichment() {
    doEnrichmentTests(function (result) {
        if (result.startsWith('ERROR!')) {
            var errMsg = result.substring(6);
            $.messager.alert('Error', 'Failed to process the request!' + errMsg, 'error');
        } else {
            loadEnrichTable(result);
        }
        $.messager.progress('close');
    });
}

function highlightFunEnrichNodes(nodes, title, empty = true) {
//note: ko is in label, not node id
    unmatched_nodes[title] = Array.from(nodes);
    sigInst.graph.nodes().forEach(function (n) {
        if (nodes.indexOf(n.id) !== -1) {
            n.size = n.size + 3;
            n.color = highlightColor;
            n.highlight = true;
            n.type = "circle";
        }
    });
    sigInst.refresh();
    if (title !== "NoDisp") {
        displayPathInfo(title, empty);
}
}

function unHighlightFunEnrichNodes2(nodes, title) {
//note: ko is in label, not node id
    sigInst.graph.nodes().forEach(function (n) {
        if (nodes.indexOf(n.id) !== -1) {
            n.size = 1.5;
            n.color = n.attr.color;
            n.highlight = false;
        }
    });
    sigInst.refresh();
}

function highlightFunEnrichEdges(edgeIDs, title, empty = true) {
    sigInst.settings({
        minEdgeSize: defaultEdgeSize,
        maxEdgeSize: 5
    });

    unmatched_edges[title] = Array.from(edgeIDs);
    sigInst.graph.edges().forEach(function (e) {
        if (edgeIDs.indexOf(e.id) !== -1) {
            e.color = highlightColor;
            e.highlight = true;
            e.size = 5 + Math.abs(e.attr.expr);
        } else if (!e.highlight) {
            e.size = defaultEdgeSize;
        }
    });
    sigInst.refresh();
    if (title !== "NoDisp") {
        displayPathInfo(title, empty);
}
}

function unHighlightFunEnrichEdges(edgeIDs, title, no_highlight) {
    var no_highlight = true; // need to see if there is no highlighted nodes left


    sigInst.graph.edges().forEach(function (e) {
        if (edgeIDs.indexOf(e.id) !== -1) {
            e.size = defaultEdgeSize;
            e.highlight = false;
            e.color = e.attr.color;
        } else {
            if (e.highlight) {
                no_highlight = false;
            }
        }
    });
    if (no_highlight) {
        sigInst.settings({
            minEdgeSize: defaultEdgeSize,
            maxEdgeSize: defaultEdgeSize
        });
    }
    sigInst.refresh();
    displayPathInfo('');
}

function doEnrichmentTests(callBack) {

    var fundb = $('#cbox').val();
    $.ajax({
        beforeSend: function () {
            $.messager.progress({
                text: 'Processing .....'
            });
        },
        dataType: "html",
        type: "POST",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: {function: 'enrichment_kO01100', db: fundb},
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
