/*
 * Javascript functions for networkanalyst
 * 2014-9-21
 * Jeff Xia (jeff.xia@mcgill.ca)
 */

//global vars
var greyColor = '#959595';
var greyColorB = '#959595';
var greyColorW = '#888888';
var highlightColor = '#0080ff';
var backgroundColor = "#f9f9f9";
var transparentColor = 'rgba(0, 0, 0, 0)';
var up_color = "#FF0000";
var white_color = "#ffffff";
var down_color = "#008000"; //#80d0f0
var view_mode = "topo";
var anal_type = "list"; //list, single, meta
var node_rows = [];
var sub_modules = [];
var hidden_mode = false;
var dim_mode = false;
var lasso_active = false;
var highlight_mode = false;
var mouseover_mode = false; // hide remote nodes
var dragListener;
var minEdgeSize = 0.2;
var edgeColoring = false;
var defaultEdgeSize = 0.4;
var current_fun_nodes;
var dependants;
var selectedNodeIDs = []; //for free style drag-and-drop
var usr_dir, org, vismode; //vismode
var sigInst, settings;
var curr_pname;
var initialized = false;
var cmpdnameOpt = 'show';
var defaultNodeColor = "#78B0FF";
var highlightBorderColor = "#FF6700";

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

function initNetwork(fname) {
    if (!initialized) {
        initFunctions();
    }

    usr_dir = $.ajax({
        type: "GET",
        url: '/MetaboAnalyst/faces/AjaxCall?function=getusrdir&ignoreMe=' + new Date().getTime(),
        async: false
    }).responseText;

    setupNetwork(fname);
}

function downloadpng() {
    document.getElementById("downloadimage").src = export2Image();
    $("#pngdialog").dialog('open');
}

function downloadsvg() {
    sigInst.toSVG({download: true, filename: 'network_analyst.svg', labels: false, background: backgroundColor});
}

function downloadpng() {
    document.getElementById("downloadimage").src = export2Image();
    $("#pngdialog").dialog('open');
}

function downloadsvg() {
    sigInst.toSVG({download: true, filename: 'network_analyst.svg', labels: false, background: backgroundColor});
}

function highlightSelectedSMPDBNode(n) {
//    var borderCol;
//    if (!('borderColor' in n)) {
//        borderCol = highlightBorderColor;
//    } else {
//        delete n['borderColor'];
//    }
    if (!('selected' in n)) {
        n.size = n.size + 2;
        n.highlight = 1;
        n.color = highlightColor;
        //n.borderColor = borderCol;
        n['selected'] = true;
    } else {
        n.size = n.size - 2;
        n.highlight = 1;
        n.color = defaultNodeColor;
        //n.borderColor = borderCol;
        delete n['selected'];
    }
    sigInst.refresh();
}


//initiate variables and attach functions
function initFunctions() {

    initialized = true;

    //dynamic update centerpane width based on user browser
    $('#network-parent').css("width", $(window).width() - 640);
    $('#network-parent').css("height", $(window).height() - 40);
    $('#networkspace').css("background", "#FCFCFC");

    $('#ss').slider({
        tipFormatter: function (value) {
            return value + '%';
        }
    });

    // Instanciate sigma.js and customize rendering :
    sigInst = new sigma({
        renderers: [
            {
                container: document.getElementById('networkview'),
                type: 'canvas' // sigma.renderers.canvas works as well
            }
        ],
        settings: {
            defaultLabelColor: 'black',
            defaultLabelSize: 12,
            defaultLabelBGColor: '#fff',
            defaultLabelHoverColor: '#000',
            labelThreshold: 6,
            defaultEdgeColor: 'default',
            doubleClickEnabled: false,
            minNodeSize: 1,
            maxNodeSize: 10,
            sideMargin: 1,
            minEdgeSize: defaultEdgeSize,
            maxEdgeSize: 1.4,
            defaultNodeBorder: 0
        }
    });

    sigInst.bind('doubleClickNode', function (e) {
        var n = e.data.node;
        highlightSelectedSMPDBNode(n);
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


    var config = {
        nodeMargin: 8.0,
        scaleNodes: 1.3
    };

    // Configure the algorithm
    sigInst.configNoverlap(config);

    // Initialize the activeState plugin:
    var activeState = sigma.plugins.activeState(sigInst);

    // Initialize the dragNodes plugin:
    var dragListener = new sigma.plugins.dragNodes(sigInst, sigInst.renderers[0], activeState);
}

function loadNetwork(pname) {
    curr_pname = pname;
    $.ajax({
        dataType: "html",
        type: "POST",
        url: '/MetaboAnalyst/faces/AjaxCall?function=viewNetwork' + '&name=' + pname,
        async: false,
        cache: false,
        timeout: 8000,
        success: function (result) {
            initNetwork(result);
            return;
        },
        error: function () {
            $.messager.alert('Error', 'Failed to process the request!', 'error');
            $.messager.progress('close');
        }
    });
}

//loadNetwork and NodeTable
function setupNetwork(content) {
    $.messager.progress({
        text: 'Processing .....'
    });
    sigInst.graph.clear();

    content = content.split(";");
    var fname = content[0];
    var metabo_matched = content[1].split(",");
    document.getElementById('title').innerHTML = '<a href="javascript:void(0);" onclick="window.open(\'http://www.smpdb.ca/view/' + content[2] + '\');">' + content[3] + '</a> '

    var net_path = usr_dir + '/' + fname;
    $.getJSON(net_path, function (data) {
        for (var i = 0; i < data.nodes.length; i++) {
            var nd = data.nodes[i];
            if (vizMode === "network-full") {
                nd.size = 7;
            } else {
                nd.size = 5;
            }

            if (nd.pwp_id !== null) {
                nd.x = parseInt(nd.x);
                nd.y = parseInt(nd.y);

                if (isNaN(nd.x)) {
                    nd.x = 0;
                }
                if (isNaN(nd.y)) {
                    nd.y = 0;
                }
                nd.color = "grey";
                nd.type = "square";
                nd.label = nd.name;
                //keep a copy
                nd.attr = {x: nd.x, y: nd.y, size: nd.size, color: nd.color, type: nd.type, label: nd.label};
                sigInst.graph.addNode(nd);

            } else {
                nd.x = parseInt(nd.x);
                nd.y = parseInt(nd.y);
                if (isNaN(nd.x)) {
                    nd.x = 0;
                }
                if (isNaN(nd.y)) {
                    nd.y = 0;
                }
                nd.color = defaultNodeColor;
                nd.label = nd.name;

                if (metabo_matched.indexOf(nd.hmdb_id) !== -1) {
                    nd.borderColor = highlightBorderColor;
                    nd.size = nd.size + 1;
                } else {
                    nd.size = nd.size - 1;
                }
                //keep a copy
                nd.attr = {x: nd.x, y: nd.y, size: nd.size, color: nd.color, label: nd.label};
                sigInst.graph.addNode(nd);
            }
        }

        //need to add a fake node for scale properly
        var fake_nd = {id: 'mynode', hidden: true, x: 0, y: 0, size: 10, color: transparentColor,
            attr: {x: 0, y: 0, size: 10, color: transparentColor}};
        sigInst.graph.addNode(fake_nd);

        //do edges
        for (var j = 0; j < data.edges.length; j++) {
            var eg = data.edges[j];
            eg.size = defaultEdgeSize;
            eg.id = j.toString();
            eg.color = "grey";
            var lvl = 1;
            eg.attr = {color: eg.color, expr: lvl};
            sigInst.graph.addEdge(eg);
        }

        $("#spinner").fadeOut();

        //sigInst.startForceAtlas2({gravity: 200, scalingRatio: 70, slowDown: 100, barnesHutOptimize: true, startingIterations: 30, iterationsPerRender: 20});
        sigInst.refresh();
        sigInst.startNoverlap();
        // Following code is for startForceAtlas2 effect
        //    setTimeout(function () {
        //        sigInst.killForceAtlas2();
        //    }, 2000);
        sigma.misc.animation.camera(sigInst.camera, {
            x: 0,
            y: 0,
            ratio: 1.0
        }, {duration: 250});
        $.messager.progress('close');
    });

}
