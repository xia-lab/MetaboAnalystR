/* 
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/JSP_Servlet/JavaScript.js to edit this template
 */
var savedState = {};

function initReportFunctions() {

    $('#reportBn').bind('click keypress', function (event) {
        handleSaveEvent(false);
    });

    var element = $(parent.window.document).find("#sidebar-form\\:m_report");
    element.off();
    element.bind('click keypress', function (event) {
        handleSaveEvent(true);
    });

}


function handleSaveEvent(sendJsonFlag) {
    sendImageToServer(export2Image(0.8), "network_enr");

    var nodesArr = [];
    sigInst.graph.nodes().forEach(function (node) {
        let newNode = {id: node.id,
            size: limitDigits(node.size, 2),
            x: limitDigits(node.x, 2),
            y: limitDigits(node.y, 2),
            type: node.type,
            color: node.color};
        if (node.expanded) {
            newNode.expanded = true;
        }
        if ("borderColor" in node) {
            newNode.borderColor = node.borderColor;
        }
        nodesArr.push(newNode);
    });

    var expEdges = [];
    sigInst.graph.edges().forEach(function (edge) {
        if ("expBool" in edge && edge.expBool) {
            expEdges.push(edge.id);
        }
    });
    savedState.sigInstGraphNodes = nodesArr;
    savedState.sigInstGraphEdges = expEdges;
    savedState.edgeColor = edgeColor;
    savedState.edgeSize = defaultEdgeSize;
    savedState.edgeOpa = parseFloat($('#edgeSliderOpa').val());
    savedState.currentEnrichFile = currentEnrichFile;
    savedState.backgroundColor = [gradCol1, gradCol2];
    savedState.backgroundOpt = $('#backgroundOpt').val();
    savedState.netNm = netNm;
    savedState.enrichdb = $("#enrichdb").val();
    savedState.viewOpt = $("#viewOpt").val();
    let dataStr = JSON.stringify(savedState);
    //console.log(JSON.stringify(savedState).length);
    sendJsonToServer(dataStr, "enrichNet", sendJsonFlag);

}
function checkPagesToVisit() {
    if (localStorage.getItem('pagesToVisit')) {
        parent.PF("workflowProgressDialog").show();
        var pagesToVisit = JSON.parse(localStorage.getItem('pagesToVisit'));
        pagesToVisit.shift();
        localStorage.setItem('pagesToVisit', JSON.stringify(pagesToVisit));
        setTimeout(function () {

            sendImageToServer(export2Image(0.8), "network", function () {
                if (pagesToVisit.length > 0) {
                    window.parent.location.href = pagesToVisit[0];

                } else {
                    localStorage.setItem('reportEndBool', true);
                    localStorage.removeItem('pagesToVisit');

                    window.parent.location.href = "/MetaboAnalyst/Secure/xialabpro/DashboardView.xhtml";
                }
            });
        }, 3000);
        return true;
    } else {
        sendImageToServer(export2Image(0.8), "network");

        return false;
    }
}
function checkSavedState() {
    //console.log("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA===" + savedStateInit)

    if (savedStateInit === "false") {
        setTimeout(function () {
            setNetworkB();
            checkPagesToVisit();

        }, 200)
        return;
    }
    const fileNm = "report_" + currFileNm;
    fetch(usr_dir + "/" + fileNm)
            .then(response => {
                if (response.ok) {

                    console.log('File exists. Reload previous state');
                    $.getJSON(usr_dir + "/" + fileNm, function (data) {
                        savedState = data;
                        //$("#enrichdb").val(savedState.enrichdb);
                        sigInst.graph.clear();
                        //netNm = savedState.netNm;
                        //console.log(netNm);
                        $.getJSON(usr_dir + "/" + netNm, function (data) {
                            netData = data;
                            loadDgs(enrType);
                            initNodeProperties();
                        });

                        if (savedState.backgroundOpt) {
                            $('#backgroundOpt').val(savedState.backgroundOpt);
                        }
                        //console.log(savedState);
                        netData.backgroundColor[0] = savedState.backgroundColor[0];
                        netData.backgroundColor[1] = savedState.backgroundColor[1];
                        if ("edgeColor" in savedState) {
                            edgeColor = savedState.edgeColor;
                        }
                        //console.log(edgeColor)
                        updateBackgroundColorGradient(netData.backgroundColor[0], netData.backgroundColor[1]);
                        setTimeout(function () {
                            sigInst.graph.clear();

                            var newNodesArr = [];
                            savedState.sigInstGraphNodes.forEach(function (node) {
                                newNodesArr.push(node.id);
                            });

                            netData.bnodes.forEach(function (node) {
                                var inx = newNodesArr.indexOf(node.id);
                                if (inx !== -1) {
                                    const newNode = savedState.sigInstGraphNodes[inx];
                                    node.size = newNode.size;
                                    node.x = newNode.x;
                                    node.y = newNode.y;
                                    node.type = newNode.type;
                                    node.color = newNode.color;
                                    if ("expanded" in newNode) {
                                        node.expanded = newNode.expanded;
                                        node.borderColor = newNode.borderColor;
                                    }
                                    sigInst.graph.addNode(node);
                                }
                            });


                            var rgbaCol = hexToRGB(edgeColor, savedState.edgeOpa);
                            //console.log(rgbaCol)
                            sigInst.graph.edges().forEach(function (edge) {
                                edge.color = rgbaCol;
                            });
                            if (savedState.viewOpt === "bi") {
                                netData.bedges.forEach(function (edge) {
                                    edge.color = rgbaCol;
                                    sigInst.graph.addEdge(edge);
                                });
                            } else {
                                netData.edges.forEach(function (eg) {
                                    eg.color = rgbaCol;
                                    if (eg.source && eg.target) {
                                        sigInst.graph.addEdge(eg);
                                    }
                                });
                                netData.bedges.forEach(function (edge) {
                                    if (savedState.sigInstGraphEdges.indexOf(edge.id) !== -1) {
                                        if (sigInst.graph.nodes(edge.target).borderColor) {
                                            //edge.color = hexToRGB(sigInst.graph.nodes(edge.target).borderColor, savedState.edgeOpa);
                                            //edge.color = sigInst.graph.nodes(edge.target).borderColor;
                                        }
                                        sigInst.graph.addEdge(edge);
                                    }
                                });
                            }

                            defaultEdgeSize = savedState.edgeSize;
                            sigInst.settings({
                                minEdgeSize: defaultEdgeSize,
                                maxEdgeSize: defaultEdgeSize
                            });

                            //$("#viewOpt").val(savedState.viewOpt);

                            sigInst.refresh();
                        }, 1000)
                        //console.log(savedState)
                    });


                } else {
                    setTimeout(function () {
                        setNetworkB();
                        let res = checkPagesToVisit();
                        if (!res) {
                            parent.PF("statusDialog").show();

                            setTimeout(function () {
                                handleSaveEvent(false)
                                parent.PF("statusDialog").hide();
                            }, 2000);
                            console.log('File does not exist. Init normally');
                        }
                    }, 200)

                }

            }
            )
            .catch(error => {
                console.error('There was a problem with the fetch operation:', error.message);
                setTimeout(function () {
                    setNetworkB();
                    loadDgs(enrType);
                }, 200)
                let res = checkPagesToVisit();
                if (!res) {
                    parent.PF("statusDialog").show();

                    setTimeout(function () {
                        handleSaveEvent(false)
                        parent.PF("statusDialog").hide();
                    }, 2000);
                    console.log('File does not exist. Init normally');
                }
            });
    savedStateInit = "true";
}


function limitDigits(number, maxDigits) {
    const factor = Math.pow(10, maxDigits);
    return Math.floor(number * factor) / factor;
}


function initNodeProperties() {
    var data = netData;
    sigInst.graph.clear();
    for (var i = 0; i < data.bnodes.length; i++) {
        var nd = data.bnodes[i];
        nd.labeled = 0;
        nd.molType = data.bnodes[i].molType;
        nd.expanded = false;
        nd.x = nd.posx;
        nd.y = nd.posy;
        nd.highlight = 0;
        nd.size = nd.true_size;
        if (nd.molType === "gene") {
            nd.color = nd.colorb;
            nd.orig_color = nd.colorb;
            nd.true_color_b = nd.color;
            nd.true_color_w = nd.colorw;
            nd.entrez = symbol2entrez[nd.label];

            nd.size = 4;
        } else {
            nd.borderColor = nd.color;
            nd.color = hexToRGB(nd.colorb, 0.4);
            nd.orig_color = hexToRGB(nd.colorb, 0.4);
            nd.true_color_b = hexToRGB(nd.colorb, 0.4);
            nd.true_color_w = hexToRGB(nd.colorw, 0.4);

        }
    }


    for (var j = 0; j < data.bedges.length; j++) {
        var eg = data.bedges[j];
        if (!eg.id.includes("b")) {
            eg.id = "b" + eg.id;
        }
        eg.color = "rgba(211,211,211, 0.5)";
    }

    for (var i = 0; i < data.nodes.length; i++) {
        var nd = data.nodes[i];
        nd.x = nd.posx;
        nd.y = nd.posy;
        nd.color = nd.colorb;
        nd.orig_color = nd.colorb;
        nd.true_color_b = nd.color;
        nd.true_color_w = nd.colorw;
        nd.molType = data.nodes[i].molType;
        nd.expanded = false;
        nd.highlight = 0;
        nd.size = nd.true_size;
        if (nd.size === null) {
            nd.size = 14;
        }

    }


    for (var j = 0; j < data.edges.length; j++) {
        var eg = data.edges[j];
        if (eg.source !== undefined) {
            eg.color = edgeColor;
            eg.hidden = false;
            eg.size = defaultEdgeSize;
        }
    }

    //check if all nodes same size
    var sizeArr = [];
    data.nodes.forEach(function (n) {
        sizeArr.push(n.size);
    });
    if (Math.max(sizeArr) === Math.min(sizeArr)) {
        data.nodes.forEach(function (n) {
            n.size = 12;
        });
    }

    var sizeArr = [];
    data.bnodes.forEach(function (n) {
        sizeArr.push(n.size);
    });
    if (Math.max(sizeArr) === Math.min(sizeArr)) {
        data.bnodes.forEach(function (n) {
            n.size = 12;
        });
    }

}
