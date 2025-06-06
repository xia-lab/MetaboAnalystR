/* 
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/JSP_Servlet/JavaScript.js to edit this template
 */
savedState = {};

function initReportFunctions(type) {

    //sendImageToServer(myChart.toBase64Image(), "scatter_" + type, "png");
    sendJsonToServer(JSON.stringify(savedState), type, false);

    function handleSaveEvent(reportFlag) {
            //sendImageToServer(myChart.toBase64Image(), "scatter_" + type, "png");
            sendJsonToServer(JSON.stringify(savedState), type, reportFlag);
    }

    $('#reportBn').bind('click keypress', function (event) {
        handleSaveEvent(false);
    });

    var element = $(document).find("#sidebar-form\\:m_report");
    element.bind('click keypress', function (event) {
        handleSaveEvent(true);
    });
}
