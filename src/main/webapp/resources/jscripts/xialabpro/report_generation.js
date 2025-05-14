/* 
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/JSP_Servlet/JavaScript.js to edit this template
 */
$(document).ready(function () { // each refresh needs to reinitiate interactivity
    var element = $(document).find("#sidebar-form\\:m_report");
    element.bind('click keypress', function (event) {
        generateReportFromJS();
    });
});


