/*
 * Javascript utility functions for network manipulation 
 * Jeff Xia (jeff.xia@mcgill.ca)
 */


//when user resize the network, trigger this function
function resizeNetwork() {
    //dynamic update centerpane width based on user browser
    $('#network-parent').css("width", $(window).width() - 350);
    $('#network-parent').css("height", $(window).height() - 50);
    sigInst.refresh();
}

//initiate variables and attach functions
function initFunctions() {
    //dynamic update centerpane width based on user browser
    $('#network-parent').css("width", $(window).width() - 350);
    $('#network-parent').css("height", $(window).height() - 50);

    // Instanciate sigma.js and customize rendering :
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
            defaultNodeBorder: 0.1
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
        highlightSelectedNode(e.data.node.id);
    });

    sigInst.bind('doubleClickEdge', function (e) {
        //highlightSelectedEdge(e.data.edge.id);
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

    $('#genenameOpt').change(function () {
        genenameOpt = $('#genenameOpt').val();
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
            n.color = backgroundColor;
        }
    });
    sigInst.refresh();
}


function highlightMyNodes() {//custom signature profiling
    //need to get each line
    var ids = $('#signature').val().split('\n');
    var i;
    for (i = 0; i < ids.length; i++) {
        highlightSelectedNode(ids[i]);
    }
}

function highlightSelectedEdge(edgeID) {
    highlight_mode = 1;
    var found = false;
    sigInst.graph.edges().forEach(function (e) {
        if (e.id.indexOf(edgeID) !== -1) {
            e.size = e.size + 1;
            e.highlight = 1;
            e.color = highlightColor;
            found = true;
        }
    });
    if (!found) {
        $.messager.alert('Error',"Cannot find a node with the given ID!");
    }
    sigInst.refresh();
}

function highlightSelectedNode(nodeID) {
    highlight_mode = 1;
    var found = false;
    sigInst.graph.nodes().forEach(function (n) {
        if (n.id.indexOf(nodeID) !== -1) {
            n.size = n.size + 2;
            n.highlight = 1;
            n.color = highlightColor;
            found = true;
        }
    });
    if (!found) {
        $.messager.alert('Error',"Cannot find a node with the given ID!");
    }
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



function unHighlightFunEnrichNodes(nodes) {
    if (style_mode === "expr") {
        if (backgroundColor === "#222222") {
            sigInst.graph.nodes().forEach(function (n) {
                if (nodes.indexOf(n.id) !== -1) {
                    n.size = n.true_size;
                    n.color = n.expr_colorb;
                    n.highlight = 0;
                }
            });
        } else {
            sigInst.graph.nodes().forEach(function (n) {
                if (nodes.indexOf(n.id) !== -1) {
                    n.size = n.true_size;
                    n.color = n.expr_colorw;
                    n.highlight = 0;
                }
            });
        }
    } else { //plain mode
        sigInst.graph.nodes().forEach(function (n) {
            if (nodes.indexOf(n.id) !== -1) {
                n.size = n.true_size;
                n.color = greyColorB;
                n.highlight = 0;
            }
        });
    }
    sigInst.refresh();
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

function exportResultTable(name) {
    if (name === "comtb") {
        if (sub_modules.length === 0) {
            $.messager.alert('Error', 'No module detection has been performed on current network!', 'error');
            return;
        }
    } else if (name === "funtb") {
        if ($('#dg2').datagrid('getRows').length === 0) {
            $.messager.alert('Error', 'No functional enrichment analysis has been performed!', 'error');
            return;
        }
    }

    if (name === "ndtb") {
        setupFileDownload("node_table.csv");
    } else if (name === "comtb") {
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

//should be called from exportNetwork
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

function export2Image() {
    // Retrieving a dataUrl of the rendered graph
    var dataUrl = sigInst.renderers[0].snapshot({format: 'png', background: backgroundColor, filename: 'network-graph.png'});
    return dataUrl;
}

var dataCB = [
    {group: 'Pathway', items: [
            {value: 'Metabolism', text: 'Metabolism'},
            {value: 'Genetic Information Processing', text: 'Genetic Info Processing'},
            {value: 'Environmental Information Processing', text: 'Environmental Info Processing'},
            {value: 'Cellular Processes', text: 'Cellular Processes'},
            {value: 'Organismal Systems', text: 'Organismal Systems'},
            {value: 'Human Diseases', text: 'Human Diseases'}
        ]},
    {group: 'Module', items: [
            {value: 'Pathway module', text: 'Pathway module'},
            {value: 'Structural complex', text: 'Structural complex'},
            {value: 'Functional set', text: 'Functional set'},
            {value: 'Signature module', text: 'Signature module'}
        ]}
];
function loadFilterCB(data) {
    var dd = [];
    for (var i = 0; i < data.length; i++) {
        var group = data[i].group;
        dd.push({
            group: group,
            text: group
        });
        dd = dd.concat(data[i].items);
    }
    return dd;
}

function onLoadSuccessCB() {
    var groupItems = $(this).combobox('panel').find('div.combobox-item:has(span.combobox-group-text)');
    groupItems.removeClass('combobox-item');
}

function formatterCB(row) {
    if (row.group) {
        return '<span class="combobox-group-text" style="font-weight:bold">' + row.group + '</span>';
    } else {
        return '<span style="padding-left:10px">' + row.text + '</span>';
    }
}

//display KO members for a given pathway
function displayPathInfo(pathNm, empty=true) {
    var stats = $("#stats");
    if(empty){
        stats.empty();
    }

    if (pathNm !== '') {
        stats.append('<strong>' + pathNm +'</strong><br/>');
        var idList = [];
        if (pathNm === "Query list") {
            for (var pathway in all_fun_anot_kos) {
                var items = all_fun_anot_kos[pathway];
                if (typeof items !== 'undefined') {
                    if (Array.isArray(items)) {
                        if (items.length !== 0) {
                            for (var i = 0; i < items.length; i++) {
                                if (idList.indexOf(items[i]) === -1) {
                                    idList.push(items[i]);
                                }
                            }
                        }
                    } else {
                        if (idList.indexOf(items) === -1) {
                            idList.push(items);
                        }
                    }
                }
            }
        }else{
            var idList = focus_fun_anot_kos[pathNm];
        }
        var termLine = '<ul>';
        
        if (Array.isArray(idList)) {
            for (var i = 0; i < idList.length; i++) {
                if (idList[i][0] === 'K') {
                    termLine = termLine + '<li><a href="http://www.genome.jp/dbget-bin/www_bget?ko:';
                } else {
                    termLine = termLine + '<li><a href="http://www.genome.jp/dbget-bin/www_bget?';
                }
                termLine = termLine + idList[i] + '" target="_blank"><u>' + idList[i] + '</u></a></li>';
            }
        } else {
            if (idList[0] === 'K') {
                termLine = termLine + '<li><a href="http://www.genome.jp/dbget-bin/www_bget?ko:';
            } else {
                termLine = termLine + '<li><a href="http://www.genome.jp/dbget-bin/www_bget?';
            }
            termLine = termLine + idList + '" target="_blank"><u>' + idList + '</u></a></li>';
        }
        
        termLine = termLine + '</ul>';
        stats.append(termLine);
    }
}