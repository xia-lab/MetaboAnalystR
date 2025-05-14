/*
 * Javascript functions for network analysis
 * 2014-9-21
 * Jeff Xia (jeff.xia@mcgill.ca)
 */
var scaleFac = 1;

function highlightMyNodes() {//custom signature profiling
    //need to get each line
    var unhigh = $("#unhigh").prop('checked');
    var ids = $('#signature').val().split('\n');
    highlightNodes(ids, unhigh);
}

function highlightNodes(ids, unhigh) {//custom signature profiling
    var borderCol = getBorderColor();
    sigInst.graph.nodes().forEach(function (n) {
        if (ids.indexOf(n.id) !== -1 || ids.indexOf(n.label) !== -1) {
            n.size = n.size + 2;
            n.highlight = 1;
            n.color = highlightColor;
            n.borderColor = borderCol;
            found = true;
        } else {
            n.borderColor = null;
            if (unhigh) {
                n.size = n.true_size;
                if (backgroundColor === "#222222") {
                    n.color = n.colorb;
                } else {
                    n.color = n.colorw;
                }
                n.highlight = 0;
            }
        }
    });
    sigInst.refresh();
    sigInst.camera.goTo({x: 0, y: 0, angle: 0, ratio: 1.0});
}

function highlightSelectedNode(nodeID) {
    highlight_mode = 1;
    var borderCol = getBorderColor();
    if ($('#selectOpt').val() === "neighbour") {
        var neighbors = {};
        neighbors[nodeID] = 1;
        sigInst.graph.edges().forEach(function (e) {
            if (e.hidden === false) {
                if (nodeID === e.source || nodeID === e.target) {
                    neighbors[e.source] = 1;
                    neighbors[e.target] = 1;
                    e.color = highlightColor;
                    e.highlight = 1;
                }
            }
        });
        sigInst.graph.nodes().forEach(function (n) {
            if (neighbors[n.id]) {
                n.color = highlightColor;
                n.borderColor = borderCol;
                n.highlight = 1;
                n.size = n.size + 2;
            } else {
                n.borderColor = null;
            }
        });
    } else {
        var found = false;
        sigInst.graph.nodes().forEach(function (n) {
            if (n.id === nodeID || n.label.split("; ").indexOf(nodeID) !== -1) {
                n.size = n.size + 2;
                n.highlight = 1;
                n.color = highlightColor;
                //n.borderColor = borderCol;
                found = true;
            } else {
                n.borderColor = null;
            }
        });
        if (!found) {
            $.messager.alert('Error', "Cannot find a node with the given ID!");
        }
    }
    sigInst.refresh();
}

function updateBackground(backgroundColor) {
    if (backgroundColor !== "gradient") {
        $("#networkspace").css('background', '').css('background', backgroundColor);
    } else {
        $("#networkspace").css('background', "rgb(24,123,205)");
        $("#networkspace").css('background', "linear-gradient(0deg, rgba(24,123,205,1) 0%, rgba(255,255,255,1) 100%)");
    }
    if (backgroundColor === "#222222") {
        sigInst.settings({
            defaultLabelColor: '#fff',
            defaultLabelBGColor: '#fff'
        });
        if (view_mode === "topo") {
            sigInst.graph.nodes().forEach(function (n) {
                n.color = n.true_color_b;
            });
            if (edgeColoring) {
                sigInst.graph.edges().forEach(function (e) {
                    e.color = e.true_color_b;
                });
            }
        } else {
            sigInst.graph.nodes().forEach(function (n) {
                n.color = n.expr_colorb;
            });
            if (edgeColoring) {
                sigInst.graph.edges().forEach(function (e) {
                    e.color = e.expr_colorb;
                });
            }
        }
    } else {
        sigInst.settings({
            defaultLabelColor: '#000',
            defaultLabelBGColor: '#000'
        });

        if (view_mode === "topo") {
            sigInst.graph.nodes().forEach(function (n) {
                n.color = n.true_color_w;
            });
            if (edgeColoring) {
                sigInst.graph.edges().forEach(function (e) {
                    e.color = e.true_color_w;
                });
            }
        } else {
            sigInst.graph.nodes().forEach(function (n) {
                n.color = n.expr_colorw;
            });
            if (edgeColoring) {
                sigInst.graph.edges().forEach(function (e) {
                    e.color = e.expr_colorw;
                });
            }
        }
    }
    sigInst.refresh();
}

function displayEdgeInfo(edge) {
    var stats = $("#stats");
    stats.empty();

    if (vismode === "dspc") {
        stats.append('<li><strong>P-value</strong>: ' + edge.pval + '</li>');
        stats.append('<li><strong>Q-value</strong>: ' + edge.qval + '</li>');
        stats.append('<li><strong>Partial Coeff.</strong>: ' + edge.coeff + '</li>'); 
    } else if (vismode === "metabo_phenotypes") {
        var evidenceList = edge.evidence.split(",");
        for (var i = 0; i < evidenceList.length; i++) {
            stats.append('<li><strong>PubMed</strong>: ' + '<a href=" https://www.ncbi.nlm.nih.gov/pubmed/?term=' + evidenceList[i] + '" target="_blank"><u>' + evidenceList[i] + '</u></a>' + '</li>');
        }
    } else if (vismode === "gene_metabolites") {
        var nodes = sigInst.graph.nodes();
        var src = nodes[nodeIDsArrPosition[edge.source]].evidence;
        var target = nodes[nodeIDsArrPosition[edge.target]].evidence;

        var stichlink = "http://www.stitch-db.org/cgi/textmining.pl?all_channels_on=1&show_all_direct=off&show_all_transferred=off&targetmode=proteins&identifiers=";
        stichlink = stichlink + "-1" + target.split("CIDs")[1];
        stichlink = stichlink + "%250D9606."; //9606 represents for Human in STITCH
        stichlink = stichlink + src;
        stats.append('<li><strong>STITCH</strong>: ' + '<a href=" ' + stichlink + '" target="_blank"><u>' + src + " - " + target + '</u></a>' + '</li>');

    } else if (vismode === "metabo_metabolites") {
        var nodes = sigInst.graph.nodes();
        var src = nodes[nodeIDsArrPosition[edge.source]].evidence;
        var target = nodes[nodeIDsArrPosition[edge.target]].evidence;

        var stichlink = "http://www.stitch-db.org/cgi/textmining.pl?all_channels_on=1&show_all_direct=off&show_all_transferred=off&targetmode=proteins&identifiers=";
        stichlink = stichlink + "-1" + target.split("CIDs")[1];
        stichlink = stichlink + "%250D-1" + src.split("CIDs")[1] + "&species=9606";
        stats.append('<li><strong>STITCH</strong>: ' + '<a href=" ' + stichlink + '" target="_blank"><u>' + src + " - " + target + '</u></a>' + '</li>');
    } else if (vismode === "global") {
        var nodes = sigInst.graph.nodes();

        if (edge.source[0] === "C") { //Metabolite-Disease case
            var evidence = nodes[nodeIDsArrPosition[edge.target]].evidence;
            var evidenceList = evidence.split(",");
            for (var i = 0; i < evidenceList.length; i++) {
                stats.append('<li><strong>PubMed</strong>: ' + '<a href=" https://www.ncbi.nlm.nih.gov/pubmed/?term=' + evidenceList[i] + '" target="_blank"><u>' + evidenceList[i] + '</u></a>' + '</li>');
            }
        } else if (edge.target[0] === "C") { //Gene-Metabolite case
            var src = nodes[nodeIDsArrPosition[edge.source]].evidence;
            var target = nodes[nodeIDsArrPosition[edge.target]].evidence;

            var stichlink = "http://www.stitch-db.org/cgi/textmining.pl?all_channels_on=1&show_all_direct=off&show_all_transferred=off&targetmode=proteins&identifiers=";
            stichlink = stichlink + "-1" + target.split("CIDs")[1];
            stichlink = stichlink + "%250D9606."; //9606 represents for Human in STITCH
            stichlink = stichlink + src;
            stats.append('<li><strong>STITCH</strong>: ' + '<a href=" ' + stichlink + '" target="_blank"><u>' + src + " - " + target + '</u></a>' + '</li>');
        } else { // Gene-Disease case
            var src = nodes[nodeIDsArrPosition[edge.source]];
            var target = edge.target;
            var genesymbol = src.label;
            var link = '<a href=" https://omim.org/entry/' + target + '?search=' + target + '%20' + genesymbol;
            link = link + '&highlight=' + target + '%20prkcsh';
            stats.append('<li><strong>OMIM</strong>: ' + link + '" target="_blank"><u>' + genesymbol + ',' + target + '</u></a>' + '</li>');
        }
    }
}

function displayNodeInfo(node) {
    var stats = $("#stats");
    stats.empty();
    if (vismode === "gene_metabolites") {
        if (node.type === "circle") {
            stats.append('<li><strong>Symbol</strong>: ' + node.label + '</li>');
            stats.append('<li><strong>Name</strong>: ' + node.genename + '</li>');
            stats.append('<li><strong>Genbank</strong>: ' + '<a href="http://www.ncbi.nlm.nih.gov/gene/' + node.id + '" target="_blank"><u>' + node.id + '</u></a>' + '</li>');
        } else {
            stats.append('<li><strong>Name</strong>: ' + node.label + '</li>');
            stats.append('<li><strong>HMDB ID</strong>: ' + '<a href=" http://www.hmdb.ca/metabolites/' + node.hmdb + '" target="_blank"><u>' + node.hmdb + '</u></a>' + '</li>');
            stats.append('<li><strong>KEGG COMPOUND</strong>: ' + '<a href=" http://www.genome.jp/dbget-bin/www_bget?cpd:' + node.id + '" target="_blank"><u>' + node.id + '</u></a>' + '</li>');
        }
    } else if (vismode === "global") {
        if (node.type === "circle") {
            stats.append('<li><strong>Symbol</strong>: ' + node.label + '</li>');
            stats.append('<li><strong>Name</strong>: ' + node.genename + '</li>');
            stats.append('<li><strong>Genbank</strong>: ' + '<a href="http://www.ncbi.nlm.nih.gov/gene/' + node.id + '" target="_blank"><u>' + node.id + '</u></a>' + '</li>');
        } else if (node.type === "diamond") {
            stats.append('<li><strong>Name</strong>: ' + node.label + '</li>');
            stats.append('<li><strong>HMDB ID</strong>: ' + '<a href=" http://www.hmdb.ca/metabolites/' + node.hmdb + '" target="_blank"><u>' + node.hmdb + '</u></a>' + '</li>');
            stats.append('<li><strong>KEGG COMPOUND</strong>: ' + '<a href=" http://www.genome.jp/dbget-bin/www_bget?cpd:' + node.id + '" target="_blank"><u>' + node.id + '</u></a>' + '</li>');
        } else {
            stats.append('<li><strong>Name</strong>: ' + node.label + '</li>');
            stats.append('<li><strong>OMIM</strong>: ' + '<a href=" https://www.omim.org/entry/' + node.id + '" target="_blank"><u>' + node.id + '</u></a>' + '</li>');
        }
    } else if (vismode === "metabo_phenotypes") {
        stats.append('<li><strong>Name</strong>: ' + node.label + '</li>');
        if (node.type === "circle") {
            stats.append('<li><strong>HMDB ID</strong>: ' + '<a href=" http://www.hmdb.ca/metabolites/' + node.hmdb + '" target="_blank"><u>' + node.hmdb + '</u></a>' + '</li>');
            stats.append('<li><strong>KEGG COMPOUND</strong>: ' + '<a href=" http://www.genome.jp/dbget-bin/www_bget?cpd:' + node.id + '" target="_blank"><u>' + node.id + '</u></a>' + '</li>');
        } else {
            stats.append('<li><strong>OMIM</strong>: ' + '<a href=" https://www.omim.org/entry/' + node.id + '" target="_blank"><u>' + node.id + '</u></a>' + '</li>');
        }
    } else if (vismode === "gene_phenotypes") {
        if (node.type === "circle") {
            stats.append('<li><strong>Symbol</strong>: ' + node.label + '</li>');
            stats.append('<li><strong>Name</strong>: ' + node.genename + '</li>');
            stats.append('<li><strong>Genbank</strong>: ' + '<a href="http://www.ncbi.nlm.nih.gov/gene/' + node.id + '" target="_blank"><u>' + node.id + '</u></a>' + '</li>');
        } else {
            stats.append('<li><strong>OMIM</strong>: ' + '<a href=" https://www.omim.org/entry/' + node.id + '" target="_blank"><u>' + node.id + '</u></a>' + '</li>');
        }
    } else if (vismode === "metabo_metabolites") {
        stats.append('<li><strong>Name</strong>: ' + node.label + '</li>');
        stats.append('<li><strong>HMDB ID</strong>: ' + '<a href=" http://www.hmdb.ca/metabolites/' + node.hmdb + '" target="_blank"><u>' + node.hmdb + '</u></a>' + '</li>');
        stats.append('<li><strong>KEGG COMPOUND</strong>: ' + '<a href=" http://www.genome.jp/dbget-bin/www_bget?cpd:' + node.id + '" target="_blank"><u>' + node.id + '</u></a>' + '</li>');
    }
}

function displayCurrentSelectedNodes(nodes, title) {
    var stats = $("#stats");
    stats.empty();
    if (title !== "") {
        stats.append('<lh><b>' + title + '</b></lh>');
    }
    for (var i = 0; i < nodes.length; i++) {
        // stats.append('<li><a href="http://www.genome.jp/dbget-bin/www_bget?ko:' + nodes[i].id + '" target="_blank"><u>' + nodes[i].label + '</u></a></li>');
        stats.append('<li>' + nodes[i].label + '</li>');
    }
}

//network layout updates will remove all node properties, need to re-do record
function updateNetworkLayout(fileNm) {
    $.getJSON(usr_dir + '/' + fileNm, function (data) {
        var nd_pos = {};
        for (var i = 0; i < data.nodes.length; i++) {
            var nd = data.nodes[i];
            nd_pos[nd.id] = [nd.x, nd.y];
        }
        var my_nd;
        sigInst.graph.nodes().forEach(function (nd) {
            my_nd = nd_pos[nd.id];
            nd.x = my_nd[0];
            nd.y = my_nd[1];
        });
        sigInst.refresh();
        sigma.misc.animation.camera(sigInst.camera, {
            x: 0,
            y: 0,
            ratio: 1.0
        }, {duration: 200});
    });
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

function reloadNodeTable() {
    var dg = $('#dg');
    //clear data and selection
    dg.datagrid('loadData', {
        "total": 0,
        "rows": []
    });
    dg.datagrid('clearSelections');
    dg.datagrid('clearChecked');
    dg.datagrid('loadData', node_rows);
    resetNetwork();
}

function resetNodes(nodeIDs) {
    sigInst.graph.nodes().forEach(function (n) {
        if (nodeIDs.indexOf(n.id) !== -1) {
            if (n.highlight) {
                n.highlight = 0;
                n.borderColor = null;
                n.size = n.true_size;
                if (view_mode == "topo") {
                    if (backgroundColor === "#222222") {
                        n.color = n.colorb;
                    } else {
                        n.color = n.colorw;
                    }
                } else { //expression mode
                    if (backgroundColor === "#222222") {
                        n.color = n.expr_colorb;
                    } else {
                        n.color = n.expr_colorw;
                    }
                }
                n.hidden = false;
            }
        }
    });
    sigInst.refresh();
}

function updateMouseoverEffect() {
    var type = $('#mouseOpt').val();
    if (type === 'enable') {
        mouseover_mode = true;
    } else if (type === 'disable') {
        mouseover_mode = false;
    } else {
        //nothing
    }
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

//used for manual mode
function highlightQueries(nodeVec) {
    var community = [];
    highlight_mode = 1;
    //sigInst.position(0, 0, 1).draw(2, 2, 2);

    sigInst.graph.edges().forEach(function (e) {
        e.color = greyColorB;
    });
    sigInst.graph.nodes().forEach(function (n) {
        if (nodeVec.indexOf(n.id) !== -1) {
            n.color = highlightColor;
            n.highlight = 1;
            community.push({id: n.id, label: n.label});
        } else {
            if (!n.highlight) {
                n.color = greyColorB;
            }
        }
    });

    sigInst.refresh();
    highlight_mode = 1;
    displayCurrentSelectedNodes(community, "");
}

function searchNodeTable() {
    var search = $('#nodeid').val();
    if (search === "") {
        resetNetwork();
        return;
    }
    var hitrow = "NA";
    var current_row;
    for (var i = 0; i < node_rows.length; i++) {
        current_row = node_rows[i];
        if (current_row.ID === search | current_row.Label === search) {
            hitrow = current_row;
            //node_rows.splice(i, 1);

            //note, page size is 30
            var page_num = Math.ceil(i / 30);
            var row_num = i % 30;
            $('#dg').datagrid('gotoPage', page_num);
            $('#dg').datagrid('loadData', node_rows);
            $('#dg').datagrid('selectRow', row_num);
            break;
        }
    }
    if (hitrow === "NA") {
        $.messager.alert('', "Could not find the given node: " + search, 'info');
        return;
    }
}

function updateNodeTable(nodeIDs) {
    var current_rows = [];
    for (var i = 0; i < node_rows.length; i++) {
        if (nodeIDs.indexOf(node_rows[i].ID) !== -1) {
            current_rows.push(node_rows[i]);
        }
    }

    var data_grid = $('#dg');
    //empty it first if there is any
    data_grid.datagrid('loadData', {
        "total": 0,
        "rows": []
    });
    data_grid.datagrid('loadData', current_rows);
}

function sortNodeTable(colID, order) {
    if (order === 'desc') {
        if (colID === "Betweenness") {
            node_rows.sort(function (a, b) {
                return b.Betweenness - a.Betweenness;
            });
        } else if (colID === "Degree") {
            node_rows.sort(function (a, b) {
                return b.Degree - a.Degree;
            });
        } else if (colID === "Label") {
            node_rows.sort(function (a, b) {
                if (a.Label < b.Label)
                    return 1;
                if (a.Label > b.Label)
                    return -1;
                return 0;
            });
        } else if (colID === "Status") {
            node_rows.sort(function (a, b) {
                if (a.Status === "-" & b.Status !== "-")
                    return 1;
                if (a.Status !== "-" & b.Status === "-")
                    return -1;
                if (a.Status < b.Status)
                    return 1;
                if (a.Status > b.Status)
                    return -1;
                return 0;
            });
        } else {
            node_rows.sort(function (a, b) {
                if (a.ID < b.ID)
                    return 1;
                if (a.ID > b.ID)
                    return -1;
                return 0;
            });
        }
    } else {
        if (colID === "Betweenness") {
            node_rows.sort(function (a, b) {
                return a.Betweenness - b.Betweenness;
            });
        } else if (colID === "Degree") {
            node_rows.sort(function (a, b) {
                return a.Degree - b.Degree;
            });
        } else if (colID === "Label") {
            node_rows.sort(function (a, b) {
                if (a.Label < b.Label)
                    return -1;
                if (a.Label > b.Label)
                    return 1;
                return 0;
            });
        } else if (colID === "Status") {
            node_rows.sort(function (a, b) {
                if (a.Status === "-" & b.Status !== "-")
                    return -1;
                if (a.Status !== "-" & b.Status === "-")
                    return 1;
                if (a.Status < b.Status)
                    return -1;
                if (a.Status > b.Status)
                    return 1;
                return 0;
            });
        } else {
            node_rows.sort(function (a, b) {
                if (a.ID < b.ID)
                    return -1;
                if (a.ID > b.ID)
                    return 1;
                return 0;
            });
        }
    }

    var data_grid = $('#dg');
    //empty it first if there is any
    data_grid.datagrid('loadData', {
        "total": 0,
        "rows": []
    });
    data_grid.datagrid('loadData', node_rows);
}

function getBorderColor() {
    if (backgroundColor === "#222222") {
        return("#FAFAD2");
    } else {
        return("#A0522D");
    }
}
function searchNetwork(nodeID) {
    //$.messager.progress();
    var hit = 0;
    //centering
    //sigInst.position(0, 0, 1).draw(2, 2, 2);
    //then Loop all nodes
    var borderCol = getBorderColor();
    sigInst.graph.nodes().forEach(function (n) {
        if (n.id === nodeID) {
            hit = 1;
            n.size = n.size + 2;
            //n.borderColor = "#FFFF00";
            n.borderColor = borderCol;
            n.highlight = 1;
            sigma.misc.animation.camera(
                    sigInst.camera,
                    {
                        x: n[sigInst.camera.readPrefix + 'x'],
                        y: n[sigInst.camera.readPrefix + 'y'],
                        ratio: 0.35
                    },
                    {duration: 300});
            displayNodeInfo(n);
        } else {
            n.borderColor = null;
        }
    });
    sigInst.refresh();
    if (!hit) {
        $.messager.alert('Error', "Node " + nodeID + " was not found in the current network!", 'error');
    }
    //$.messager.progress('close');
    return;
}

function resetNetwork() {
    //default no edge coloring
    $('#eColOpt').val("off");
    sigInst.settings({
        minEdgeSize: 0,
        maxEdgeSize: 0
    });

    sigInst.camera.goTo({x: 0, y: 0, angle: 0, ratio: 1.0});
    if (view_mode === "topo") {
        if (backgroundColor === "#222222") {
            sigInst.graph.nodes().forEach(function (n) {
                n.size = n.true_size;
                n.color = n.true_color_b;
                n.borderColor = null;
                n.hidden = false;
                n.highlight = false;
            });
            sigInst.graph.edges().forEach(function (e) {
                e.color = greyColorB;
                // e.size = 1;
                e.highlight = false;
                e.hidden = false;
            });
        } else {
            sigInst.graph.nodes().forEach(function (n) {
                n.size = n.true_size;
                n.color = n.true_color_w;
                n.borderColor = null;
                n.hidden = false;
                n.highlight = false;
            });
            sigInst.graph.edges().forEach(function (e) {
                e.color = greyColorB;
                //e.size = 1;
                e.highlight = false;
                e.hidden = false;
            });
        }
    } else if (view_mode === "expr") {
        if (backgroundColor === "#222222") {
            sigInst.graph.nodes().forEach(function (n) {
                n.size = n.true_size;
                n.color = n.expr_colorb;
                n.borderColor = null;
                n.hidden = false;
                n.highlight = false;
            });
            sigInst.graph.edges().forEach(function (e) {
                e.color = greyColorB;
                //e.size = 1;
                e.highlight = false;
                e.hidden = false;
            });
        } else {
            sigInst.graph.nodes().forEach(function (n) {
                n.size = n.true_size;
                n.color = n.expr_colorw;
                n.borderColor = null;
                n.hidden = false;
                n.highlight = false;
            });
            sigInst.graph.edges().forEach(function (e) {
                e.color = greyColorB;
                //e.size = 1;
                e.highlight = false;
                e.hidden = false;
            });
        }
    } else { //plain mode
        sigInst.graph.nodes().forEach(function (n) {
            n.size = n.true_size;
            n.color = greyColorB;
            n.borderColor = null;
            n.hidden = false;
            n.highlight = false;
        });
        sigInst.graph.edges().forEach(function (e) {
            e.color = greyColorB;
            //e.size = 1;
            e.highlight = false;
            e.hidden = false;
        });
    }
    sigInst.refresh();
    highlight_mode = 0;
    hidden_mode = false;
    $("#stats").empty();
    $("#pathLnks").empty();

    var dg2 = $('#dg2');
    dg2.datagrid('loadData', {
        "total": 0,
        "rows": []
    });
    var col2 = dg2.datagrid('getColumnOption', 'color');
    col2.styler = function () {
        return 'background-color:white';
    };
}

function highlightSelectedNodes(nodes, title) {
    if (nodes.length === 1) {
        searchNetwork(nodes[0]);
    } else {
        var nodeVec = [];
        highlight_mode = 1;
        var borderCol = getBorderColor();
        //sigInst.position(0, 0, 1).draw(2, 2, 2);
        if (dim_mode) {
            sigInst.graph.edges().forEach(function (e) {
                e.color = greyColorB;
            });
            sigInst.graph.nodes().forEach(function (n) {
                if (nodes.indexOf(n.id) !== -1) {
                    n.size = n.size + 2;
                    if (n.size > 10) {
                        n.size = 10;
                    }
                    n.borderColor = borderCol;
                    n.highlight = 1;
                    nodeVec.push({id: n.id, label: n.label});
                } else {
                    if (!n.highlight) {
                        n.color = greyColorB;
                    }
                    n.borderColor = null;
                }
            });
        } else {
            sigInst.graph.nodes().forEach(function (n) {
                if (nodes.indexOf(n.id) !== -1) {
                    n.size = n.size + 2;
                    if (n.size > 10) {
                        n.size = 10;
                    }
                    n.borderColor = borderCol;
                    n.highlight = 1;
                    nodeVec.push({id: n.id, label: n.label});
                } else {
                    n.borderColor = null;
                }
            });
        }
        sigInst.refresh();
        displayCurrentSelectedNodes(nodeVec, title);
    }
}

function highlightFunEnrichNodes(nodes, title) {
    if (nodes.length === 1) {
        searchNetwork(nodes[0]);
    } else {
        var nodeVec = [];
        highlight_mode = 1;
        //var borderCol = getBorderColor();
        //sigInst.position(0, 0, 1).draw(2, 2, 2);
        if (dim_mode) {
            sigInst.graph.edges().forEach(function (e) {
                e.color = greyColorB;
            });
            if (view_mode === "topo") {
                sigInst.graph.nodes().forEach(function (n) {
                    if (nodes.indexOf(n.id) !== -1) {
                        n.size = n.size + 2;
                        if (n.size > 10) {
                            n.size = 10;
                        }
                        n.color = highlightColor;
                        n.highlight = 1;
                        nodeVec.push({id: n.id, label: n.label});
                    } else {
                        if (!n.highlight) {
                            n.color = greyColorB;
                        }
                    }
                });
            } else { //expression, use border to highlight
                sigInst.graph.nodes().forEach(function (n) {
                    if (nodes.indexOf(n.id) !== -1) {
                        n.size = n.size + 2;
                        if (n.size > 10) {
                            n.size = 10;
                        }
                        n.borderColor = highlightColor;
                        n.highlight = 1;
                        nodeVec.push({id: n.id, label: n.label});
                    } else {
                        if (!n.highlight) {
                            n.borderColor = null;
                        }
                    }
                });
            }
        } else {
            if (view_mode === "topo") {
                sigInst.graph.nodes().forEach(function (n) {
                    if (nodes.indexOf(n.id) !== -1) {
                        n.size = n.size + 2;
                        if (n.size > 10) {
                            n.size = 10;
                        }
                        n.color = highlightColor;
                        n.highlight = 1;
                        nodeVec.push({id: n.id, label: n.label});
                    }
                });
            } else {
                sigInst.graph.nodes().forEach(function (n) {
                    if (nodes.indexOf(n.id) !== -1) {
                        n.size = n.size + 2;
                        if (n.size > 10) {
                            n.size = 10;
                        }
                        n.borderColor = highlightColor;
                        n.highlight = 1;
                        nodeVec.push({id: n.id, label: n.label});
                    } else {
                        if (!n.highlight) {
                            n.borderColor = null;
                        }
                    }
                });
            }
        }
        sigInst.refresh();
        displayCurrentSelectedNodes(nodeVec, title);
    }
}

function unHighlightFunEnrichNodes(nodes) {

    if (view_mode === "topo") {
        if (backgroundColor === "#222222") {
            sigInst.graph.nodes().forEach(function (n) {
                if (nodes.indexOf(n.id) !== -1) {
                    n.borderColor = null;
                    n.size = n.true_size;
                    n.color = n.true_color_b;
                    n.highlight = 0;
                }
            });
        } else {
            sigInst.graph.nodes().forEach(function (n) {
                if (nodes.indexOf(n.id) !== -1) {
                    n.borderColor = null;
                    n.size = n.true_size;
                    n.color = n.true_color_w;
                    n.highlight = 0;
                }
            });
        }
    } else if (view_mode === "expr") {
        if (backgroundColor === "#222222") {
            sigInst.graph.nodes().forEach(function (n) {
                if (nodes.indexOf(n.id) !== -1) {
                    n.borderColor = null;
                    n.size = n.true_size;
                    n.color = n.expr_colorb;
                    n.highlight = 0;
                }
            });
        } else {
            sigInst.graph.nodes().forEach(function (n) {
                if (nodes.indexOf(n.id) !== -1) {
                    n.borderColor = null;
                    n.size = n.true_size;
                    n.color = n.expr_colorw;
                    n.highlight = 0;
                }
            });
        }
    } else { //plain mode
        sigInst.graph.nodes().forEach(function (n) {
            if (nodes.indexOf(n.id) !== -1) {
                n.borderColor = null;
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
    }
    doNetworkExtract(nodeIDs, function (result) {
        if (result !== "NA") {
            setupNetwork(result);
            var netNm = result.substring(0, result.length - 5);
            updateNetworkOpts(netNm);
        } else {
            $.messager.alert('Error', 'Could not find a module with at least 3 nodes. Make sure the highlighted nodes are connected!', 'error');
        }
        $.messager.progress('close');
    });
}

function doNetworkExtract(nodeIDs, callBack) {
    $.messager.confirm('Extract Confirmation',
            "Are you sure to perform module extraction? Note, all created networks are availalbe in the <b>Network</b> menu on top. ",
            function (r) {
                if (r) {
                    $.ajax({
                        beforeSend: function () {
                            $.messager.progress({
                                text: 'Processing .....'
                            });
                        },
                        dataType: "html",
                        type: "POST",
                        url: '/MetaboAnalyst/faces/AjaxCall',
                        data: {function: 'extractModule', nodeIDs: nodeIDs.join(";")},
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
            });
}

//when user resize the network, trigger this function
function resizeNetwork() {
    //dynamic update centerpane width based on user browser
    $('#network-parent').css("width", $(window).width() - 634);
    $('#network-parent').css("height", $(window).height() - 4);

    sigInst.refresh();
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

function updateEdgeView() {
    var type = $('#shapeOpt').val();
    var eCol = $('#eColOpt').val();
    if (vismode !== "dspc") {
        var val = $('#widthOpt').val();
        if (val === 'increase') {
            defaultEdgeSize = defaultEdgeSize + 0.2;
        } else {
            defaultEdgeSize = defaultEdgeSize - 0.1;
        }
        if (defaultEdgeSize < 0.1) {
            defaultEdgeSize = 0.1;
        }

        sigInst.settings({
            minEdgeSize: defaultEdgeSize,
            maxEdgeSize: defaultEdgeSize
        });
    }
    if (eCol === "on") {
        sigInst.settings({
            minEdgeSize: 0,
            maxEdgeSize: 0
        });

        edgeColoring = true;
        var display_mode = 0;
        if (view_mode === "topo") {
            if (backgroundColor === "#222222") {
                display_mode = 0;
            } else {
                display_mode = 1;
            }
        } else {
            if (backgroundColor === "#222222") {
                display_mode = 2;
            } else {
                display_mode = 3;
            }
        }
        sigInst.graph.edges().forEach(
                function (e) {
                    //e.size = parseFloat(e.esize_pval);
                    //e.size = Math.random();
                    e.type = type;
                    switch (display_mode) {
                        case 0:
                            e.color = e.true_color_b;
                            break;
                        case 1:
                            e.color = e.true_color_w;
                            break;
                        case 2:
                            e.color = e.expr_colorb;
                            break;
                        default:
                            e.color = e.expr_colorb;
                    }
                });
    } else {
        edgeColoring = false;
        var edgeCol = greyColorW;
        if (backgroundColor === "#222222") {
            edgeCol = greyColorB;
        }
        sigInst.graph.edges().forEach(
                function (e) {
                    e.type = type;
                    e.color = edgeCol;
                });
    }
    sigInst.refresh();
}

function doLayoutUpdate(algo, callBack) {
    $.ajax({
        beforeSend: function () {
            $.messager.progress({
                text: 'Processing .....'
            });
        },
        dataType: "html",
        type: "GET",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: "function=updateNetworkLayout" + "&layoutalgo=" + algo,
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

function doNetworkPrep(netNm, callBack) {
    $.ajax({
        beforeSend: function () {
            $.messager.progress({
                text: 'Processing .....'
            });
        },
        dataType: "html",
        type: "GET",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: "function=prepareNetwork" + "&netName=" + netNm,
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

function computeShortestPath() {
    var pathLnks = $("#pathLnks");
    pathLnks.empty();
    doShortestPathCompute(function (res) {
        nds_paths = res.split("||");
        var id_sbl_path = nds_paths[0].split(";"); //get id path and symbol path
        var id_path = id_sbl_path[0].split("->");
        if (id_path.length === 1) {
            $.messager.progress('close');
            //alert(res);
            return;
        }

        for (var i = 0; i < nds_paths.length; i++) {
            pathLnks.append('<li><a href="#" onclick="highlightShortestPath(' + i + ');return false;"><u>' + nds_paths[i].split(";")[1] + '</u></a></li>');
        }

        if (nds_paths.length > 0) {
            highlightShortestPath(0);
        }
        $.messager.progress('close');
    });
}

function highlightShortestPath(inx) {
    var nodeVec = [];
    highlight_mode = 1;
    var id_sbl_path = nds_paths[inx].split(";");
    var ids = id_sbl_path[0].split("->");
    var nds = id_sbl_path[1].split("->");
    //update to allow edge size change
    sigInst.settings({
        maxEdgeSize: 1.2
    });
    //sigInst.refresh();

    sigInst.graph.edges().forEach(function (e) {
        if (ids.indexOf(e.target) !== -1 & ids.indexOf(e.source) !== -1) {
            e.size = 1.2;
            e.color = highlightColor;
            e.highlight = 1;
        } else {
            if (!e.highlight) {
                e.color = greyColorB;
                e.size = defaultEdgeSize;
            }
        }
    });
    sigInst.graph.nodes().forEach(function (n) {
        if (nds.indexOf(n.label) !== -1) {
            n.color = highlightColor;
            n.highlight = 1;
            n.size = n.size + 3;
            nodeVec.push({id: n.id, label: n.label});
        } else {
            if (!n.highlight) {
                n.size = n.true_size;
                n.color = greyColorB;
            }
        }
    });
    sigInst.refresh();
    displayCurrentSelectedNodes(nodeVec, "");
}

function doShortestPathCompute(callBack) {
    var src = $('#source').val();
    var target = $('#target').val();
    //note, convert them to a valid node ID
    var srcID = "NA";
    var targetID = "NA";
    var ndLbls = ["NA"];
    sigInst.graph.nodes().forEach(function (n) {
        ndLbls = n.label.split("; ");
        if (ndLbls.indexOf(src) !== -1 || n.id === src) {
            srcID = n.id;
        } else if (ndLbls.indexOf(target) !== -1 || n.id === target) {
            targetID = n.id;
        }
    });
    if (srcID === "NA") {
        $.messager.alert('Error', 'This source node is not in the network!', 'error');
        $.messager.progress('close');
        return;
    }
    if (targetID === "NA") {
        $.messager.alert('Error', 'This target node is not in the network!', 'error');
        $.messager.progress('close');
        return;
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
        data: 'function=getShortestPaths' + "&source=" + srcID + "&target=" + targetID,
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

function updateCellColor(color, id) {
    $("#" + id).css("background-color", color);
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

var currentEnrichFile = "";
var focus_fun_anot;
function testEnrichment() {
    doEnrichmentTests(loadEnrichmentResult);
}

function loadEnrichmentResult(result){
        if (result.startsWith('ERROR!')) {
            var errMsg = result.substring(6);
            $.messager.alert('Error', 'Failed to process the request!' + errMsg, 'error');
        } else {
            $.getJSON(usr_dir + '/' + result, function (raw_data) {
                currentEnrichFile = result.substring(0, result.length - 5);
                focus_fun_anot = raw_data['fun.anot'];

                if (typeof focus_fun_anot === 'undefined') {
                    focus_fun_anot = raw_data['hits.query'];
                }

                var fun_hit = raw_data['hit.num'];
                var fun_pval = raw_data['fun.pval'];

                var data_grid = $('#dg2');
                //empty if there is any
                data_grid.datagrid('loadData', {
                    "total": 0,
                    "rows": []
                });
                var mdl_rows = [];
                var idx = 0;
                $.each(focus_fun_anot, function (k, v) {
                    mdl_rows.push({
                        pathname: k,
                        hit: fun_hit[idx],
                        pval: fun_pval[idx],
                        color: '<span id=\"function_' + idx + '\">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp</span>'
                    });
                    idx = idx + 1;
                });

                data_grid.datagrid({
                    onSelect: function (index, row) {
                        var nodeIDs = focus_fun_anot[row.pathname];
                        current_fun_nodes = nodeIDs;
                        highlightFunEnrichNodes(nodeIDs, row.pathname);
                        updateCellColor(highlightColor, "function_" + index);
                    },
                    onUnselect: function (index, row) {
                        var nodeIDs = focus_fun_anot[row.pathname];
                        current_fun_nodes = null;
                        unHighlightFunEnrichNodes(nodeIDs);
                        updateCellColor(greyColor, "function_" + index);
                    }
                }).datagrid('loadData', mdl_rows);
            });
        }
        $.messager.progress('close');
    
}

function doEnrichmentTests(callBack) {

    if(vismode === undefined){
        vismode="";
    }

    var node_ids = "";
    var query = $('#queryView').val();
    var count = 0;
    if (query === "all") {
        sigInst.graph.nodes().forEach(function (n) {
            count++;
            node_ids = node_ids + '; ' + n.id;
        });
    } else if (query === "up") {
        sigInst.graph.nodes().forEach(function (n) {
            if (n.expr > 0) {
                count++;
                node_ids = node_ids + '; ' + n.id;
            }
        });
    } else if (query === "down") {
        sigInst.graph.nodes().forEach(function (n) {
            if (n.expr < 0) {
                count++;
                node_ids = node_ids + '; ' + n.id;
            }
        });
    } else { //highlighted
        var count = 0;
        sigInst.graph.nodes().forEach(function (n) {
            if (n.highlight) {
                count++;
                node_ids = node_ids + '; ' + n.id;
            }
        });
    }
    if (count === 0) {
        $.messager.alert('', 'No such nodes were found!', 'info');
    } else {
        //remove leading "; "
        node_ids = node_ids.substring(2, node_ids.length);
        var fundb = $('#enrichdb').val();
        $.ajax({
            beforeSend: function () {
                $.messager.progress({
                    text: 'Processing .....'
                });
            },
            dataType: "html",
            type: "POST",
            url: '/MetaboAnalyst/faces/AjaxCall',
            data: {function: 'networkEnrichment', IDs: node_ids, funDB: fundb, vismode: vismode},
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
}

function export2Image(quality=1) {
    // Retrieving a dataUrl of the rendered graph
    var dataUrl = sigInst.renderers[0].snapshot({format: 'png', gradCol1:  gradCol1, gradCol2:  gradCol2, filename: 'network-graph.png',quality:quality});
    return dataUrl;
}

function performSetOperation() {
    var ids = [];
    var rows = $('#dg').datagrid('getSelections');
    for (var i = 0; i < rows.length; i++) {
        ids.push(rows[i].ID);
    }
    if (ids.length === 0) {
        $.messager.alert('', "Error: please first select nodes using the check boxes.", 'error');
        return;
    }

    var neighbors = {};
    sigInst.graph.edges().forEach(function (e) {
        if (ids.indexOf(e.source) !== -1) { //node is the source
            if (e.target in neighbors) {
                neighbors[e.target] = neighbors[e.target] + 1;
            } else {
                neighbors[e.target] = 1;
            }
        } else if (ids.indexOf(e.target) !== -1) { // node is the target
            if (e.source in neighbors) {
                neighbors[e.source] = neighbors[e.source] + 1;
            } else {
                neighbors[e.source] = 1;
            }
        }
    });

    var operation = $('#setOpt').val();
    var nds = [];
    if (operation === "union") {
        for (var key in neighbors) {
            nds.push(key);
        }
    } else {
        var len = ids.length;
        for (var key in neighbors) {
            if (neighbors[key] === len) {
                nds.push(key);
            }
        }
    }
    if (nds.length === 0) {
        $.messager.alert('', "Error: no nodes meet the requirement.", 'error');
        return;
    }

    //now add the query nodes
    nds = nds.concat(ids);

    var edgeCol = greyColorW;
    if (backgroundColor === "#222222") {
        edgeCol = greyColorB;
    }
    sigInst.settings({
        maxEdgeSize: 0.8
    });

    sigInst.graph.edges().forEach(function (e) {
        if (nds.indexOf(e.source) !== -1 && nds.indexOf(e.target) !== -1) {
            e.color = highlightColor;
            e.size = 0.8;
            e.highlight = 1;
        } else {
            e.color = edgeCol;
            e.size = defaultEdgeSize;
            e.highlight = false;
        }
    });
    highlightNodes(nds, 1);
}


function performSetOperation_orig() {
    doSetOperation(function (result) {
        if (result.substring(0, 5) !== "error") {
            var edgeCol = greyColorW;
            if (backgroundColor === "#222222") {
                edgeCol = greyColorB;
            }
            sigInst.settings({
                maxEdgeSize: 0.8
            });
            var nds = result.split("||");
            sigInst.graph.edges().forEach(function (e) {
                if (nds.indexOf(e.source) !== -1 && nds.indexOf(e.target) !== -1) {
                    e.color = highlightColor;
                    e.size = 0.8;
                    e.highlight = 1;
                } else {
                    e.color = edgeCol;
                    e.size = defaultEdgeSize;
                    e.highlight = false;
                }
            });
            highlightNodes(nds, 1);
        } else {
            var msg = result.split("||")[1];
            $.messager.alert('', "Error: " + msg, 'error');
        }
        $.messager.progress('close');
    });
}

function doSetOperation(callBack) {
    var ids = [];
    var rows = $('#dg').datagrid('getSelections');
    var operation = $('#setOpt').val();
    for (var i = 0; i < rows.length; i++) {
        ids.push(rows[i].ID);
    }

    if (ids.length === 0) {
        $.messager.alert('', "Error: please first select mir IDs using the check boxes.", 'error');
        return;
    }

    //intersect_names = ids;
    $.ajax({
        beforeSend: function () {
            $.messager.progress({
                text: 'Processing .....'
            });
        },
        dataType: "html",
        type: "GET",
        url: '/MetaboAnalyst/faces/AjaxCall',
        data: 'function=performSetOperation' + "&IDs=" + ids.join(";") + "&operation=" + operation,
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

function deleteNodes() {
    doNodeFilter(function (result) {
        if (result.substring(0, 5) === "error") {
            var msg = result.split("||")[1];
            $.messager.alert('Error', msg, 'error');
        } else {
            $.getJSON(usr_dir + '/' + result, function (data) {
                orphans = data.deletes.split("||");
                for (var i = 0; i < orphans.length; i++) {
                    sigInst.graph.dropNode(orphans[i]);
                }
                sigInst.refresh();
                sigma.misc.animation.camera(sigInst.camera, {
                    x: 0,
                    y: 0,
                    ratio: 1.0
                }, {duration: 200});

                var node_table = [];
                for (var i = 0; i < data.nodes.length; i++) {
                    var nd = data.nodes[i];
                    node_table.push({
                        ID: nd.id,
                        Label: nd.label,
                        Degree: nd.degree,
                        Betweenness: nd.between,
                        Status: nd.expr
                    });
                }
                node_rows = node_table;
                node_rows.sort(function (a, b) {
                    return b.Degree - a.Degree;
                });
                var dg = $('#dg');
                dg.datagrid('clearSelections');
                dg.datagrid('clearChecked');
                dg.datagrid('loadData', node_rows);
                $.messager.progress('close');
                $.messager.alert('Success', "You may need to re-perform layout to achieve better view", 'OK');
            });

        }
    });
}

function doNodeFilter(callBack) {
    var ids = [];
    var rows = $('#dg').datagrid('getSelections');

    for (var i = 0; i < rows.length; i++) {
        ids.push(rows[i].ID);
    }
    if (ids.length === 0) {
        $.messager.alert('Information', 'Please first select nodes using the check boxes.', 'info');
        return;
    } else {
        $.messager.confirm('Delete Confirmation',
                "Are you sure to delete these nodes? Note the resulting isolated nodes will also be removed.",
                function (r) {
                    if (r) {
                        //delete from R igraph
                        $.ajax({
                            beforeSend: function () {
                                $.messager.progress({
                                    text: 'Processing .....'
                                });
                            },
                            dataType: "html",
                            type: "GET",
                            url: '/MetaboAnalyst/faces/AjaxCall',
                            data: 'function=performNodesFilter' + "&nodes=" + ids.join(";"),
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
                });
    }
}

function updateBackground(backgroundColor) {
    gradCol2 = backgroundColor
    gradCol1 = backgroundColor
    if (backgroundColor === "#222222") {
        $("#networkspace").css('background', '').css('background', backgroundColor);
        $("#networkview").css('background', '').css('background', backgroundColor);
        sigInst.settings({
            defaultLabelColor: '#fff',
            defaultLabelBGColor: '#fff'
        });
        if (view_mode === "topo") {
            sigInst.graph.nodes().forEach(function (n) {
                n.color = n.true_color_b;
            });
            if (edgeColoring) {
                sigInst.graph.edges().forEach(function (e) {
                    e.color = e.true_color_b;
                });
            }
        } else {
            sigInst.graph.nodes().forEach(function (n) {
                n.color = n.expr_colorb;
            });
            if (edgeColoring) {
                sigInst.graph.edges().forEach(function (e) {
                    e.color = e.expr_colorb;
                });
            }
        }
    } else if (backgroundColor === "#ffffff") {
        $("#networkspace").css('background', '').css('background', backgroundColor);
        $("#networkview").css('background', '').css('background', backgroundColor);
        sigInst.settings({
            defaultLabelColor: '#000',
            defaultLabelBGColor: '#000'
        });

        if (view_mode === "topo") {
            sigInst.graph.nodes().forEach(function (n) {
                n.color = n.true_color_w;
            });
            if (edgeColoring) {
                sigInst.graph.edges().forEach(function (e) {
                    e.color = e.true_color_w;
                });
            }
        } else {
            sigInst.graph.nodes().forEach(function (n) {
                n.color = n.expr_colorw;
            });
            if (edgeColoring) {
                sigInst.graph.edges().forEach(function (e) {
                    e.color = e.expr_colorw;
                });
            }
        }
    }
    sigInst.refresh();
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

function getContrastColor(hexcolor) {
    var r = parseInt(hexcolor.substr(0, 2), 16);
    var g = parseInt(hexcolor.substr(2, 2), 16);
    var b = parseInt(hexcolor.substr(4, 2), 16);
    var yiq = ((r * 299) + (g * 587) + (b * 114)) / 1000;
    return (yiq >= 128) ? '#222222' : 'white';
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

function saveDataURI(name, dataURI) {
    const blob = dataURIToBlob(dataURI);

    // force download
    const link = document.createElement('a');
    link.download = name;
    link.href = window.URL.createObjectURL(blob);
    link.onclick = () => {
        window.setTimeout(() => {
            window.URL.revokeObjectURL(blob);
            link.removeAttribute('href');
        }, 500);

    };
    link.click();
}


function rescaleSigInst(x) { //shared across different sigma.js based network viewer
    var scale = 1;
    var d = document.getElementById('network-parent');
    var c = document.getElementById('networkview');
    if (x === 1) {
        d.style.width = "100%";
        d.style.height = "100%";
        c.style.width = "100%";
        c.style.height = "100%";
        scale = 1;
    } else {
        d.style.width = (d.clientWidth * x) + "px";
        d.style.height = (d.clientHeight * x) + "px";
        c.style.width = (d.clientWidth * x) + "px";
        c.style.height = (d.clientHeight * x) + "px";
        scale = x * x;
    }

    var nodes = sigInst.graph.nodes();
    sigInst.graph.nodes().forEach(function (n) {
        if (scaleFac === 1) {
            n.size = n.size * (scale)
        } else {
            n.size = n.size * (1 / scaleFac)
        }
    });

    var edges = sigInst.graph.edges();
    var labelCol = sigInst.settings("defaultLabelColor")
    var labelBGCol = sigInst.settings("defaultLabelBGColor")
    var labelHoverCol = sigInst.settings("defaultLabelHoverColor")
    sigInst.kill();


    sigInst = new sigma({
        renderers: [
            {
                container: document.getElementById('networkview'),
                type: 'canvas' // sigma.renderers.canvas works as well
            }
        ],
        settings: {
            defaultLabelColor: labelCol,
            defaultLabelSize: 11 * scale,
            defaultLabelBGColor: labelBGCol,
            defaultLabelHoverColor: labelHoverCol,
            labelThreshold: 6 * scale,
            defaultEdgeColor: 'default',
            doubleClickEnabled: false,
            minNodeSize: 0,
            maxNodeSize: 0,
            sideMargin: 1,
            minEdgeSize: defaultEdgeSize * scale,
            maxEdgeSize: defaultEdgeSize * scale,
            defaultNodeBorder: 0,
            minArrowSize: 5
        }
    });
    addListeners();//specific to different networkviewer (i.e list enrichment, ppi network viewer) found in specific js file (list_enrich_net.js, network_analyst.js)
    sigInst.graph.read({
        nodes: nodes,
        edges: edges
    });
    sigInst.refresh();
    scaleFac = scale;
}

function addListeners() {

    // Initialize the activeState plugin:
    var activeState = sigma.plugins.activeState(sigInst);

    // Initialize the dragNodes plugin:
    dragListener = new sigma.plugins.dragNodes(sigInst, sigInst.renderers[0], activeState);

    dragListener.bind('startdrag', function (event) {
        current_node = event.data.node;
        current_node.start_x = current_node.x;
        current_node.start_y = current_node.y;
        dependants = [];
        if ($('#selectOpt').val() === "function") {
            if (!current_fun_nodes) {
                $.messager.alert('', "Click a row in <b>Function Explorer</b> to select an enriched function.", 'info');
            } else {
                sigInst.graph.nodes().forEach(function (n) {
                    if (current_fun_nodes.indexOf(n.id) !== -1) {
                        n.start_x = n.x;
                        n.start_y = n.y;
                        dependants.push(n);
                    }
                });
            }
        } else if ($('#selectOpt').val() === "neighbour") {
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
                if (n.highlight) {
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

    // Initialize the lasso plugin:
    var lasso = new sigma.plugins.lasso(sigInst, sigInst.renderers[0], {
        'strokeStyle': 'orange',
        'lineWidth': 1,
        'fillWhileDrawing': true,
        'fillStyle': 'rgba(255, 255, 177, 0.4)',
        'cursor': 'crosshair'
    });

    // Listen for selectedNodes event from the lasso instance:
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

    sigInst.bind('overNode', function (event) {
        if (hidden_mode) {
            return false;
        }
        if (mouseover_mode) {
            //alert("mouse over mode on");
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

    sigInst.bind('clickNode', function (event) {
        var node = event.data.node;
        displayNodeInfo(node);
    });

    sigInst.bind('doubleClickNode', function (e) {
        if (view_mode === "onemode") {

        } else {
            highlightSelectedNode(e.data.node.id);
        }
    });

    sigInst.bind('rightClickNode', function (e) {
        searchNode(e.data.node.id);
    });


    $('#selectBn').bind('click keypress', function (event) {
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
    $('#selectOpt').change(function () {
        if (lasso_active) {
            lasso_active = false;
            lasso.deactivate();
            activeState.dropNodes(); //back to regular single node state
        }
    });
}

function defaultFileName(ext) {
    const str = `${new Date().toLocaleDateString()} at ${new Date().toLocaleTimeString()}${ext}`;
    return str.replace(/\//g, '-').replace(/:/g, '.');
}

function dataURIToBlob(dataURI) {
    const binStr = window.atob(dataURI.split(',')[1]);
    const len = binStr.length;
    const arr = new Uint8Array(len);
    for (let i = 0; i < len; i++) {
        arr[i] = binStr.charCodeAt(i);
    }
    return new window.Blob([arr]);
}