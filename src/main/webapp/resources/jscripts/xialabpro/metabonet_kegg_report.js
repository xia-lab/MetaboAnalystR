/* 
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/JSP_Servlet/JavaScript.js to edit this template
 */
var savedState = {};
var savedStateInit = false;

function initReportFunctions() {
    function handleSaveEvent(sendJsonFlag) {
        sendImageToServer(export2Image(0.7), "network_MetaboNet");
        savedState.backgroundColor = backgroundColor;
        savedState.highlightOpt = $("#highlightOpt").val();
        savedState.pathnameOpt = $("#pathnameOpt").val();
        savedState.cmpdnameOpt = $("#cmpdnameOpt").val();
        savedState.genenameOpt = $("#genenameOpt").val();

        savedState.selectedRows = $('#dg2').datagrid('getSelections');
        var highlightedNodesId = [];
        var highlightedNodes = [];

        sigInst.graph.nodes().forEach(function (n) {
            if (n.highlight) {
                highlightedNodesId.push(n.id);
                highlightedNodes.push(n);
            }
        });

        var highlightedEdgesId = [];
        var highlightedEdges = [];

        sigInst.graph.edges().forEach(function (e) {
            if (e.highlight) {
                highlightedEdgesId.push(e.id);
                highlightedEdges.push(e);
            }
        });

        savedState.highlightedEdgesId = highlightedEdgesId;
        savedState.highlightedEdges = highlightedEdges;
        savedState.highlightedNodesId = highlightedNodesId;
        savedState.highlightedNodes = highlightedNodes;
        let dataStr = JSON.stringify(savedState);
        //console.log(dataStr.length + "==jsonlength")
        sendJsonToServer(dataStr, "network_MetaboNet", sendJsonFlag);
    }

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


function checkSavedState() {
    if (savedStateInit) {
        return;
    }
    const fileNm = "report_mummichog_query.json";
    fetch(usr_dir + "/" + fileNm)
            .then(response => {
                if (response.ok) {
                    //console.log('File exists. Reload previous state');
                    $.getJSON(usr_dir + "/" + fileNm, function (data) {
                        savedState = data;
                        //console.log(data)

                        backgroundColor = savedState.backgroundColor;
                        pathnameOpt = savedState.pathnameOpt;
                        cmpdnameOpt = savedState.cmpdnameOpt;
                        genenameOpt = savedState.genenameOpt;
                        style_mode = savedState.highlightOpt;

                        $('#backgroundOpt').val(backgroundColor);
                        $('#pathnameOpt').val(pathnameOpt);
                        $("#cmpdnameOpt").val(cmpdnameOpt);
                        $("#genenameOpt").val(genenameOpt);
                        $('#highlightOpt').val(style_mode);

                        if (style_mode !== "kegg") {
                            setupStyleMode();
                        }

                        updateBackground(backgroundColor);

                        if (savedState.selectedRows.length > 0) {
                            for (var i = 0; i < savedState.selectedRows.length; i++) {
                                var row = savedState.selectedRows[i];
                                highlightColor = row.colorOnly;
                                //console.log(row.ID)
                                $('#dg2').datagrid('selectRow', row.ID);
                            }
                        }

                        $("#custom").spectrum({
                            color: highlightColor,
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

                        if (savedState.highlightedNodesId.length > 0) {
                            sigInst.graph.nodes().forEach(function (n) {
                                var inx = savedState.highlightedNodesId.indexOf(n.id);
                                if (inx === -1 && n.highlight) {
                                    n.size = 2;
                                    n.color = n.attr.color;
                                    n.highlight = false;
                                }
                            });
                        }
                        if (savedState.highlightedEdgesId.length > 0) {
                            sigInst.graph.edges().forEach(function (e) {
                                var inx = savedState.highlightedEdgesId.indexOf(e.id);
                                if (inx === -1 && e.highlight) {
                                    e.size = defaultEdgeSize;
                                    e.highlight = false;
                                    e.color = e.attr.color;
                                }
                            });
                        }
                        sigInst.refresh();
                    });

                    if (localStorage.getItem('pagesToVisit')) {
                        workflowSave();
                    }
                } else {
                    console.log('File does not exist. Init normally');
                    setTimeout(function () {
                        if (localStorage.getItem('pagesToVisit')) {
                            workflowSave();
                        } else {
                        sendImageToServer(export2Image(0.7), "network_MetaboNet");
                        }
                    }, 100)
                }
            })
            .catch(error => {
                console.error('There was a problem with the fetch operation:', error.message);
            });
    savedStateInit = true;
}

function workflowSave() {

    if (parent.PF("workflowProgressDialog")) {
        parent.PF("workflowProgressDialog").show();
    }
    var pagesToVisit = JSON.parse(localStorage.getItem('pagesToVisit'));
    pagesToVisit.shift();
    localStorage.setItem('pagesToVisit', JSON.stringify(pagesToVisit));
    setTimeout(function () {

        sendImageToServer(export2Image(0.8), "network_MetaboNet", "png", function () {
            if (pagesToVisit.length > 0) {
                window.parent.location.href = pagesToVisit[0];
            } else {
                localStorage.setItem('reportEndBool', true);
                window.parent.location.href = "/MetaboAnalyst/Secure/xialabpro/WorkflowView.xhtml?tabWidgetId=acVar&activeTab=1";
            }
        });
    }, 3000);

}