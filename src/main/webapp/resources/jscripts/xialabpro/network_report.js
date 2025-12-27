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

/*
    var element = $(parent.window.document).find("#sidebar-form\\:m_report");
    element.unbind('click keypress').bind('click keypress', function (event) {
        handleSaveEvent(true);
    });
     * */

}


function checkSavedState(reportJsonName) {
    if (savedStateInit) {
        return;
    }
    fetch(usr_dir + "/" + reportJsonName)
            .then(response => {
                if (response.ok) {
                    console.log('File exists. Reload previous state');
                    $.getJSON(usr_dir + "/" + reportJsonName, function (data) {
                        savedState = data;

                        //console.log(savedState)
                        updateBackgroundColorGradient(savedState.gradCol1, savedState.gradCol2);
                        sigInst.graph.clear();
                        var newNodesArr = [];
                        savedState.sigInstGraphNodes.forEach(function (node) {
                            newNodesArr.push(node.id);
                        });

                        netData.nodes.forEach(function (node) {
                            var inx = newNodesArr.indexOf(node.id);
                            if (inx !== -1) {
                                const newNode = savedState.sigInstGraphNodes[inx];
                                node.size = newNode.size;
                                node.x = newNode.x;
                                node.y = newNode.y;
                                node.type = newNode.type;
                                node.color = newNode.color;
                                sigInst.graph.addNode(node);
                            }
                        });

                        netData.edges.forEach(function (edge) {
                            sigInst.graph.addEdge(edge);
                        });
                        sigInst.refresh();

                        if (savedState.currentEnrichFile !== "") {
                            $('#queryView').val(savedState.queryView);
                            $('#enrichdb').val(savedState.enrichdb);
                            loadEnrichmentResult(savedState.currentEnrichFile + ".json");
                        }
                    });
                } else {
                    console.log('File does not exist.');
                    parent.PF("statusDialog").show();
                    setTimeout(function () {
                        handleSaveEvent(false);
                        parent.PF("statusDialog").hide();
                    }, 1500);
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
    if (vismode === undefined) {
        vismode = "network";
    }
    if (localStorage.getItem('pagesToVisit')) {
        if (parent.PF("workflowProgressDialog")) {
            parent.PF("workflowProgressDialog").show();
        }
        var pagesToVisit = JSON.parse(localStorage.getItem('pagesToVisit'));
        pagesToVisit.shift();
        localStorage.setItem('pagesToVisit', JSON.stringify(pagesToVisit));
        console.log(pagesToVisit);
        setTimeout(function () {

            sendImageToServer(export2Image(0.8), vismode, "png", function () {
                if (pagesToVisit.length > 0) {
                    window.parent.location.href = pagesToVisit[0];
                } else {
                    localStorage.setItem('reportEndBool', true);
                    window.parent.location.href = "/MetaboAnalyst/Secure/xialabpro/WorkflowView.xhtml";
                }
            });
        }, 3000);
    } else {
        sendImageToServer(export2Image(0.8), vismode);
    }
    var nodesArr = [];
    sigInst.graph.nodes().forEach(function (node) {
        let newNode = {id: node.id,
            size: limitDigits(node.size, 2),
            x: limitDigits(node.x, 2),
            y: limitDigits(node.y, 2),
            type: node.type,
            color: node.color};

        if ("borderColor" in node) {
            newNode.borderColor = node.borderColor;
        }
        nodesArr.push(newNode);
    });
    savedState.sigInstGraphNodes = nodesArr;
    //savedState.sigInstGraphEdges = sigInst.graph.edges();
    savedState.gradCol1 = gradCol1;
    savedState.gradCol2 = gradCol2;
    savedState.currentEnrichFile = currentEnrichFile;
    savedState.queryView = $('#queryView').val();
    savedState.enrichdb = $('#enrichdb').val();
    let dataStr = JSON.stringify(savedState);
    console.log(vismode);
    sendJsonToServer(dataStr, vismode, sendJsonFlag);
}


function limitDigits(number, maxDigits) {
    const factor = Math.pow(10, maxDigits);
    return Math.floor(number * factor) / factor;
}
