/* 
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/JSP_Servlet/JavaScript.js to edit this template
 */
var savedState = {};
var savedStateInit = false;

function initReportFunctions() {
    //console.log("initreport");

    $('#reportBn').bind('click keypress', function (event) {
        handleSaveEvent(false);
    });

    var element = $(parent.window.document).find("#sidebar-form\\:m_report");
    element.bind('click keypress', function (event) {
        //console.log("saveevent");
        handleSaveEvent(true);
    });

}

function handleSaveEvent(sendJsonFlag) {
    
    if (localStorage.getItem('pagesToVisit')) {
        if (parent.PF("workflowProgressDialog")) {
            parent.PF("workflowProgressDialog").show();
        }
        var pagesToVisit = JSON.parse(localStorage.getItem('pagesToVisit'));
        pagesToVisit.shift();
        localStorage.setItem('pagesToVisit', JSON.stringify(pagesToVisit));
        setTimeout(function () {

            sendImageToServer(export2Image(0.8), "network_" + alg,"png", function () {
                if (pagesToVisit.length > 0) {
                    window.parent.location.href = pagesToVisit[0];
                } else {
                    localStorage.setItem('reportEndBool', true);
                    window.parent.location.href = "/MetaboAnalyst/Secure/xialabpro/WorkflowView.xhtml?tabWidgetId=acVar&activeTab=1";
                }
            });
        }, 3000);
    } else {
        sendImageToServer(export2Image(0.8), "network_" + alg);
    }
    savedState.backgroundColor = backgroundColor;
    savedState.pathnameOpt = $("#pathnameOpt").val();
    savedState.cmpdnameOpt = $("#cmpdnameOpt").val();
    savedState.selectedRows = $('#dg2').datagrid('getSelections');
    let dataStr = JSON.stringify(savedState);
    sendJsonToServer(dataStr, "network_" + alg, sendJsonFlag);
}

function checkSavedState(fileName) {
    if (savedStateInit) {
        return;
    }
    const fileNm = "report_" + fileName;
    fetch(usr_dir + "/" + fileNm)
            .then(response => {
                if (response.ok) {
                    console.log('File exists. Reload previous state');
                    $.getJSON(usr_dir + "/" + fileNm, function (data) {
                        savedState = data;

                        $('#backgroundOpt').val(savedState.backgroundColor);
                        updateBackground(backgroundColor);

                        $('#pathnameOpt').val(savedState.pathnameOpt);
                        $("#cmpdnameOpt").val(savedState.cmpdnameOpt);
                        pathnameOpt = savedState.pathnameOpt;
                        cmpdnameOpt = savedState.cmpdnameOpt;

                        if (savedState.selectedRows.length > 0) {
                            for (var i = 0; i < savedState.selectedRows.length; i++) {
                                var row = savedState.selectedRows[i];
                                highlightColor = row.colorOnly;
                                $('#dg2').datagrid('selectRow', row.ID);
                            }
                        }

                        sigInst.refresh();
                    });


                } else {
                    console.log('File does not exist. Init normally');
                    parent.PF("statusDialog").show();
                    setTimeout(function () {
                        handleSaveEvent(false);
                        parent.PF("statusDialog").hide();
                    }, 1500)
                }
            })
            .catch(error => {
                console.error('There was a problem with the fetch operation:', error.message);
            });
    savedStateInit = true;
}