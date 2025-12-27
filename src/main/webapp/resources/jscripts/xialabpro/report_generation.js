/* 
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/JSP_Servlet/JavaScript.js to edit this template
*/
$(document).ready(function () { // each refresh needs to reinitiate interactivity
    console.log("report_generation.js loaded");
    var element = $(document).find("#sidebar-form\\:m_report");
    console.log("Found element:", element.length);

    element.bind('click keypress', function (event) {
        console.log("Click/keypress event triggered");
        console.log("typeof window.generateReportFromJS:", typeof window.generateReportFromJS);
        console.log("typeof generateReportFromJS:", typeof generateReportFromJS);

        // Check if the function exists in window scope
        if (typeof window.generateReportFromJS === 'function') {
            console.log("Calling window.generateReportFromJS()");
            window.generateReportFromJS();
        } else if (typeof generateReportFromJS === 'function') {
            console.log("Calling generateReportFromJS()");
            generateReportFromJS();
        } else {
            // Try multiple fallback approaches
            console.log("generateReportFromJS not found, trying fallbacks");

            // Try to find the remote command script element
            var remoteCommandScript = $("script[id*='generateReportFromJS']");
            console.log("Found remote command script:", remoteCommandScript.length);

            if (remoteCommandScript.length > 0) {
                var commandId = remoteCommandScript.attr('id');
                console.log("Remote command ID:", commandId);
                PrimeFaces.ab({s: commandId, f: "sidebar-form"});
            } else {
                // Try with just the name
                console.log("Trying PrimeFaces.ab with component name");
                PrimeFaces.ab({
                    source: "sidebar-form:generateReportFromJS",
                    process: "sidebar-form",
                    update: "@none"
                });
            }
        }
    });
});

 
