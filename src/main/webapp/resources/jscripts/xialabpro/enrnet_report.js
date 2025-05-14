/* 
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/JSP_Servlet/JavaScript.js to edit this template
 */
var savedStateInit = false;
var savedState = {};
function initReportFunctions() {
    $('#reportBn').bind('click keypress', function (event) {
        handleSaveEvent(false);
    });

    var element = $(parent.window.document).find("#sidebar-form\\:m_report");
    element.bind('click keypress', function (event) {
        handleSaveEvent(true);
    });

}


function checkSavedState(reportJsonName) {
    if (savedStateInit) {
        parent.PF("statusDialog").show();
        setTimeout(function () {
            handleSaveEvent(false);
            parent.PF("statusDialog").hide();
        }, 500)
        return;
    }
    fetch('/MetaboAnalyst' + document.getElementById("mydir").value + "/" + reportJsonName)
            .then(response => {
                if (response.ok) {
                    console.log('File exists. Reload previous state');
                    $.getJSON('/MetaboAnalyst' + document.getElementById("mydir").value + "/" + reportJsonName, function (data) {
                        //console.log(data)
                        savedState = data;
                        sigInst.graph.clear();
                        var newNodesArr = [];
                        savedState.sigInstGraphNodes.forEach(function (node) {
                            newNodesArr.push(node.id);
                        });

                        netData.nodes.forEach(function (node) {
                            var inx = newNodesArr.indexOf(node.id);
                            if (inx !== -1) {
                                const newNode = savedState.sigInstGraphNodes[inx];
                                node.x = newNode.x;
                                node.y = newNode.y;
                                sigInst.graph.addNode(node);
                            }
                        });

                        netData.edges.forEach(function (edge) {
                            sigInst.graph.addEdge(edge);
                        });
                        sigInst.refresh();
                        if (localStorage.getItem('pagesToVisit')) {

                            if (PF("workflowProgressDialog")) {
                                PF("workflowProgressDialog").show();
                            }
                            var pagesToVisit = JSON.parse(localStorage.getItem('pagesToVisit'));
                            pagesToVisit.shift();
                            localStorage.setItem('pagesToVisit', JSON.stringify(pagesToVisit));
                            setTimeout(function () {
                                sendImageToServer(export2Image(0.8), "enrichment_network", "png", function () {
                                    if (pagesToVisit.length > 0) {
                                        window.parent.location.href = pagesToVisit[0];
                                    } else {
                                        window.parent.location.href = "/MetaboAnalyst/Secure/xialabpro/WorkflowView.xhtml?tabWidgetId=acVar&activeTab=1";
                                    }
                                });
                            }, 3000);
                        }
                    });
                } else {
                    parent.PF("statusDialog").show();
                    setTimeout(function () {
                        handleSaveEvent(false);
                        parent.PF("statusDialog").hide();
                    }, 500)
                }
            })
            .catch(error => {
                // Handle 404 errors here
                if (error && error.response && error.response.status === 404) {
                    console.log('File not found:', error.message);
                } else {
                    // Handle other network errors here if needed
                    console.log('There was a problem with the fetch operation:', error.message);
                }
            });
    savedStateInit = true;
}

function handleSaveEvent(sendJsonFlag) {
    var vismode = "enrichment_network";
    if (localStorage.getItem('pagesToVisit')) {
        if (PF("workflowProgressDialog")) {
            PF("workflowProgressDialog").show();
        }
        var pagesToVisit = JSON.parse(localStorage.getItem('pagesToVisit'));
        pagesToVisit.shift();
        localStorage.setItem('pagesToVisit', JSON.stringify(pagesToVisit));
        setTimeout(function () {

            sendImageToServer(export2Image(0.8), vismode, "png", function () {
                if (pagesToVisit.length > 0) {
                    window.parent.location.href = pagesToVisit[0];
                } else {
                    localStorage.setItem('reportEndBool', true);
                    window.parent.location.href = "/MetaboAnalyst/Secure/xialabpro/WorkflowView.xhtml?tabWidgetId=acVar&activeTab=1";
                }
            });
        }, 3000);
    } else {
        sendImageToServer(export2Image(), vismode, "png");
    }
    var nodesArr = [];
    sigInst.graph.nodes().forEach(function (node) {
        let newNode = {id: node.id,
            x: limitDigits(node.x, 2),
            y: limitDigits(node.y, 2)
        };

        if ("borderColor" in node) {
            newNode.borderColor = node.borderColor;
        }
        nodesArr.push(newNode);
    });
    savedState.sigInstGraphNodes = nodesArr;
    //savedState.sigInstGraphEdges = sigInst.graph.edges();
    let dataStr = JSON.stringify(savedState);
    sendJsonToServer(dataStr, vismode, sendJsonFlag);
}


function limitDigits(number, maxDigits) {
    const factor = Math.pow(10, maxDigits);
    return Math.floor(number * factor) / factor;
}

function export2Image() {
    // Retrieving a dataUrl of the rendered graph
    var dataUrl = sigInst.renderers[0].snapshot({format: 'png', background: "#ffffff", filename: 'network-graph.png'});
    return dataUrl;
}